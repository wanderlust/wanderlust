;;; elmo-imap4.el -- IMAP4 Interface for ELMO.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;; 
(require 'elmo-vars)
(require 'elmo-util)
(require 'elmo-msgdb)
(require 'elmo-date)
(require 'elmo-cache)
(require 'elmo-net)
(require 'utf7)

;;; Code:
(condition-case nil
    (progn
      (require 'sasl))
  (error))
;; silence byte compiler.
(eval-when-compile
  (require 'cl)
  (condition-case nil
      (progn
	(require 'starttls)
	(require 'sasl))
    (error))
  (defun-maybe sasl-cram-md5 (username passphrase challenge))
  (defun-maybe sasl-digest-md5-digest-response 
    (digest-challenge username passwd serv-type host &optional realm))
  (defun-maybe starttls-negotiate (a))  
  (defun-maybe elmo-generic-list-folder-unread (spec mark-alist unread-marks))
  (defsubst-maybe utf7-decode-string (string &optional imap) string))

(defvar elmo-imap4-use-lock t
  "USE IMAP4 with locking process.")
;;
;; internal variables
;;
(defvar elmo-imap4-seq-prefix "elmo-imap4")
(defvar elmo-imap4-seqno 0)
(defvar elmo-imap4-use-uid t
  "Use UID as message number.")

(defvar elmo-imap4-authenticator-alist
  '((login	elmo-imap4-auth-login)
    (cram-md5	elmo-imap4-auth-cram-md5)
    (digest-md5 elmo-imap4-auth-digest-md5))
  "Definition of authenticators.")

(eval-and-compile
  (luna-define-class elmo-imap4-session (elmo-network-session)
		     (capability current-mailbox))
  (luna-define-internal-accessors 'elmo-imap4-session))

(defconst elmo-imap4-quoted-specials-list '(?\\ ?\"))

(defconst elmo-imap4-non-atom-char-regex
  (eval-when-compile
    (concat "[^" "]!#$&'+,./0-9:;<=>?@A-Z[^_`a-z|}~-" "]")))

(defconst elmo-imap4-non-text-char-regex
  (eval-when-compile
    (concat "[^"
	    "]\x01-\x09\x0b\x0c\x0e-\x1f\x7f !\"#$%&'()*+,./0-9:;<=>?@A-Z[\\^_`a-z{|}~-"
	    "]")))

(defconst elmo-imap4-literal-threshold 1024
 "Limitation of characters that can be used in a quoted string.")

;; buffer local variable
(defvar elmo-imap4-read-point 0)

(defvar elmo-imap4-extra-namespace-alist
  '(("^{.*/nntp}.*$" . ".")) ; Default is for UW's remote nntp mailbox...
  "Extra namespace alist. A list of cons cell like: (REGEXP . DELIMITER) ")

;; buffer local variable
(defvar elmo-imap4-server-capability nil)
(defvar elmo-imap4-server-namespace nil)

(defvar elmo-imap4-lock nil)

;; For debugging. 
(defvar elmo-imap4-debug nil
  "Non-nil forces IMAP4 folder as debug mode. 
Debug information is inserted in the buffer \"*IMAP4 DEBUG*\"")

(defsubst elmo-imap4-debug (message &rest args)
  (if elmo-imap4-debug
      (with-current-buffer (get-buffer-create "*IMAP4 DEBUG*")
	(goto-char (point-max))
	(insert (apply 'format message args) "\n"))))

(defun elmo-imap4-atom-p (string)
  "Return t if STRING is an atom defined in rfc2060."
  (if (string= string "")
      nil
    (save-match-data
      (not (string-match elmo-imap4-non-atom-char-regex string)))))

(defun elmo-imap4-quotable-p (string)
  "Return t if STRING can be formatted as a quoted defined in rfc2060."
  (save-match-data
    (not (string-match elmo-imap4-non-text-char-regex string))))

(defun elmo-imap4-nil (string)
  "Return a list represents the special atom \"NIL\" defined in rfc2060, \
if STRING is nil.
Otherwise return nil."
  (if (eq string nil)
      (list 'atom "NIL")))

(defun elmo-imap4-atom (string)
  "Return a list represents STRING as an atom defined in rfc2060.
Return nil if STRING is not an atom.  See `elmo-imap4-atom-p'."
  (if (elmo-imap4-atom-p string)
      (list 'atom string)))

(defun elmo-imap4-quoted (string)
  "Return a list represents STRING as a quoted defined in rfc2060.
Return nil if STRING can not be formatted as a quoted.  See `elmo-imap4-quotable-p'."
  (if (elmo-imap4-quotable-p string)
      (list 'quoted string)))

(defun elmo-imap4-literal-subr (string-or-buffer length)
  "Internal function for `elmo-imap4-literal' and `elmo-imap4-buffer-literal'.
Return a list represents STRING-OR-BUFFER as a literal defined in rfc2060.
STRING-OR-BUFFER must be an encoded string or a single-byte string or a single-byte buffer.
LENGTH must be the number of octets for STRING-OR-BUFFER."
  (list 'literal string-or-buffer length))

(defun elmo-imap4-literal (string)
  "Return a list represents STRING as a literal defined in rfc2060.
STRING must be an encoded or a single-byte string."
  (elmo-imap4-literal-subr string (length string)))

(defun elmo-imap4-buffer-literal (buffer)
  "Return a list represents BUFFER as a literal defined in rfc2060.
BUFFER must be a single-byte buffer."
  (elmo-imap4-literal-subr buffer (with-current-buffer buffer
				    (buffer-size))))

(defun elmo-imap4-string-subr (string length)
  "Internal function for `elmo-imap4-string' and `elmo-imap4-buffer-string'.
Return a list represents STRING as a string defined in rfc2060.
STRING must be an encoded or a single-byte string.
LENGTH must be the number of octets for STRING."
  (or (elmo-imap4-quoted string)
      (elmo-imap4-literal-subr string length)))

(defun elmo-imap4-string (string)
  "Return a list represents STRING as a string defined in rfc2060.
STRING must be an encoded or a single-byte string."
  (let ((length (length string)))
    (if (< elmo-imap4-literal-threshold length)
	(elmo-imap4-literal-subr string length)
      (elmo-imap4-string-subr string length))))

(defun elmo-imap4-buffer-string (buffer)
  "Return a list represents BUFFER as a string defined in rfc2060.
BUFFER must be a single-byte buffer."
  (let ((length (with-current-buffer buffer
		  (buffer-size))))
    (if (< elmo-imap4-literal-threshold length)
	(elmo-imap4-literal-subr buffer length)
      (elmo-imap4-string-subr (with-current-buffer buffer
				(buffer-string))
			      length))))

(defun elmo-imap4-astring-subr (string length)
  "Internal function for `elmo-imap4-astring' and `elmo-imap4-buffer-astring'.
Return a list represents STRING as an astring defined in rfc2060.
STRING must be an encoded or a single-byte string.
LENGTH must be the number of octets for STRING."
  (or (elmo-imap4-atom string)
      (elmo-imap4-string-subr string length)))

(defun elmo-imap4-astring (string)
  "Return a list represents STRING as an astring defined in rfc2060.
STRING must be an encoded or a single-byte string."
  (let ((length (length string)))
    (if (< elmo-imap4-literal-threshold length)
	(elmo-imap4-literal-subr string length)
      (elmo-imap4-astring-subr string length))))

(defun elmo-imap4-buffer-astring (buffer)
  "Return a list represents BUFFER as an astring defined in rfc2060.
BUFFER must be a single-byte buffer."
  (let ((length (with-current-buffer buffer
		  (buffer-size))))
    (if (< elmo-imap4-literal-threshold length)
	(elmo-imap4-literal-subr buffer length)
      (elmo-imap4-astring-subr (with-current-buffer buffer
				 (buffer-string))
			       length))))

(defun elmo-imap4-nstring (string)
  "Return a list represents STRING as a nstring defined in rfc2060.
STRING must be an encoded or a single-byte string."
   (or (elmo-imap4-nil string)
       (elmo-imap4-string string)))

(defun elmo-imap4-buffer-nstring (buffer)
  "Return a list represents BUFFER as a nstring defined in rfc2060.
BUFFER must be a single-byte buffer."
   (or (elmo-imap4-nil buffer)
       (elmo-imap4-buffer-string buffer)))

(defalias 'elmo-imap4-mailbox 'elmo-imap4-astring)
(defalias 'elmo-imap4-field-body 'elmo-imap4-astring)
(defalias 'elmo-imap4-userid 'elmo-imap4-astring)
(defalias 'elmo-imap4-password 'elmo-imap4-astring)

(defun elmo-imap4-format-quoted (string)
  "Return STRING in a form of the quoted-string defined in rfc2060."
  (concat "\""
	  (std11-wrap-as-quoted-pairs string elmo-imap4-quoted-specials-list)
	  "\""))

(defun elmo-imap4-process-folder-list (string)
  (with-temp-buffer
    (let ((case-fold-search t)
	  mailbox-list val)
      (elmo-set-buffer-multibyte nil)
      (insert string)
      (goto-char (point-min))
      ;; XXX This doesn't consider literal name response.
      (while (re-search-forward 
	      "\\* LIST (\\([^)]*\\)) \"[^\"]*\" \\([^\n]*\\)$" nil t)
	(unless (string-match "noselect"
			      (elmo-match-buffer 1))
	  (setq val (elmo-match-buffer 2))
	  (if (string-match "^\"\\(.*\\)\"$" val)
	      (setq val (match-string 1 val)))	    
	  (setq mailbox-list 
		(nconc mailbox-list 
			(list val)))))
      mailbox-list)))

(defun elmo-imap4-list-folders (spec &optional hierarchy)
  (save-excursion
    (let* ((root (elmo-imap4-spec-mailbox spec))
	   (process (elmo-imap4-get-process spec))
	   (delim (or
		 (cdr
		  (elmo-string-matched-assoc root
					     (save-excursion
					       (set-buffer
						(process-buffer process))
					       elmo-imap4-server-namespace)))
		 "/"))
	   response result append-serv type)
      ;; Append delimiter
      (if (and root
	       (not (string= root ""))
	       (not (string-match (concat "\\(.*\\)" 
					  (regexp-quote delim)
					  "\\'")
				  root)))
	  (setq root (concat root delim)))
      (elmo-imap4-send-command process
			       (list "list " (elmo-imap4-mailbox root) " *"))
      (setq response (elmo-imap4-read-response process))
      (setq result (elmo-imap4-process-folder-list response))
      (unless (string= (elmo-imap4-spec-username spec)
		       elmo-default-imap4-user)
	(setq append-serv (concat ":" (elmo-imap4-spec-username spec))))
      (unless (string= (elmo-imap4-spec-hostname spec) 
		       elmo-default-imap4-server)
	(setq append-serv (concat append-serv "@" (elmo-imap4-spec-hostname
						   spec))))
      (unless (eq (elmo-imap4-spec-port spec)
		  elmo-default-imap4-port)
	(setq append-serv (concat append-serv ":" 
				  (int-to-string 
				   (elmo-imap4-spec-port spec)))))
      (setq type (elmo-imap4-spec-stream-type spec))
      (unless (eq (elmo-network-stream-type-symbol type)
		  elmo-default-imap4-stream-type)
	(if type
	    (setq append-serv (concat append-serv
				      (elmo-network-stream-type-spec-string
				       type)))))
      (mapcar (lambda (fld)
		(concat "%" (elmo-imap4-decode-folder-string fld)
			(and append-serv 
			     (eval append-serv))))
	      result))))

(defun elmo-imap4-get-process (spec)
  (elmo-network-session-process-internal
   (elmo-imap4-get-session spec)))

(defun elmo-imap4-folder-exists-p (spec)
  (let ((process (elmo-imap4-get-process spec)))
    (elmo-imap4-send-command process
			     (list "status "
				   (elmo-imap4-mailbox
				    (elmo-imap4-spec-mailbox spec))
				   " (messages)"))
    (elmo-imap4-read-response process)))

(defun elmo-imap4-folder-creatable-p (spec)
  t)

(defun elmo-imap4-create-folder-maybe (spec dummy)
  "Create folder if necessary."
  (if (not (elmo-imap4-folder-exists-p spec))
      (elmo-imap4-create-folder spec)))

(defun elmo-imap4-create-folder (spec)
  (let ((process (elmo-imap4-get-process spec))
	(folder (elmo-imap4-spec-mailbox spec)))
    (when folder
;;;     For UW imapd 4.6, this workaround is needed to create #mh mailbox.
;;;      (if (string-match "^\\(#mh/\\).*[^/]$" folder)
;;;	  (setq folder (concat folder "/"))) ;; make directory
      (elmo-imap4-send-command process
			       (list "create " (elmo-imap4-mailbox folder)))
      (if (null (elmo-imap4-read-response process))
	  (error "Create folder %s failed" folder)
	t))))

(defun elmo-imap4-delete-folder (spec)
  (let ((process (elmo-imap4-get-process spec))
	msgs)
    (when (elmo-imap4-spec-mailbox spec)
      (when (setq msgs (elmo-imap4-list-folder spec))
	(elmo-imap4-delete-msgs spec msgs))
      (elmo-imap4-send-command process "close")
      (elmo-imap4-read-response process)
      (elmo-imap4-send-command process
			       (list "delete "
				     (elmo-imap4-mailbox
				      (elmo-imap4-spec-mailbox spec))))
      (if (null (elmo-imap4-read-response process))
	  (error "Delete folder %s failed" (elmo-imap4-spec-mailbox spec))
	t))))

(defun elmo-imap4-rename-folder (old-spec new-spec)
  (let ((process (elmo-imap4-get-process old-spec)))
    (when (elmo-imap4-spec-mailbox old-spec)
      (elmo-imap4-send-command process "close")
      (elmo-imap4-read-response process)
      (elmo-imap4-send-command process
			       (list "rename "
				     (elmo-imap4-mailbox
				      (elmo-imap4-spec-mailbox old-spec))
				     " "
				     (elmo-imap4-mailbox
				      (elmo-imap4-spec-mailbox new-spec))
				     ))
      (if (null (elmo-imap4-read-response process))
	  (error "Rename folder from %s to %s failed"
		 (elmo-imap4-spec-mailbox old-spec)
		 (elmo-imap4-spec-mailbox new-spec))
	t))))

(defun elmo-imap4-max-of-folder (spec)
  (save-excursion
    (let* ((process (elmo-imap4-get-process spec))
	   response)
      (elmo-imap4-send-command process
			       (list "status "
				     (elmo-imap4-mailbox
				      (elmo-imap4-spec-mailbox spec))
				     " (uidnext messages)"))
      (setq response (elmo-imap4-read-response process))
      (when (and response (string-match 
			   "\\* STATUS [^(]* \\(([^)]*)\\)" response))
	(setq response (read (downcase (elmo-match-string 1 response))))
	(cons (- (cadr (memq 'uidnext response)) 1)
	      (cadr (memq 'messages response)))))))

(defun elmo-imap4-get-session (spec)
  (elmo-network-get-session
   'elmo-imap4-session
   "IMAP4"
   (elmo-imap4-spec-hostname spec)
   (elmo-imap4-spec-port spec)
   (elmo-imap4-spec-username spec)
   (elmo-imap4-spec-auth spec)
   (elmo-imap4-spec-stream-type spec)))

(defun elmo-imap4-process-filter (process output)
  (save-match-data 
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output)
      (forward-line -1)
      (beginning-of-line)
      (if (looking-at (concat 
		       "\\(^" 
		       elmo-imap4-seq-prefix 
		       (int-to-string elmo-imap4-seqno)
		       "\\|^\\* OK\\|^\\* BYE\\'\\|^\\+\\)[^\n]*\n\\'"))
	  (progn
	    (setq elmo-imap4-lock nil)	; unlock process buffer.
	    (elmo-imap4-debug "unlock(%d) %s" elmo-imap4-seqno output))
	(elmo-imap4-debug "continue(%d) %s" elmo-imap4-seqno output))
      (goto-char (point-max)))))

(defun elmo-imap4-read-response (process &optional not-command)
  "Read response from PROCESS"
  (with-current-buffer (process-buffer process)
    (let ((case-fold-search nil)
	  (response-string nil)
	  (response-continue t)
	  (return-value nil)
	  match-end)
      (while response-continue
	(goto-char elmo-imap4-read-point)
	(while (not (search-forward "\r\n" nil t))
	  (accept-process-output process)
	  (goto-char elmo-imap4-read-point))
	
	(setq match-end (point))
	(setq response-string
	      (buffer-substring elmo-imap4-read-point (- match-end 2)))
	(goto-char elmo-imap4-read-point)
	(if (looking-at (format "%s[0-9]+ OK.*$\\|\\+.*$" 
				elmo-imap4-seq-prefix))
	    (progn (setq response-continue nil)
		   (setq elmo-imap4-read-point match-end)
		   (setq return-value 
			 (if return-value 
			     (concat return-value "\n" response-string)
			   response-string)))
	  (if (looking-at (format "\\(. BYE.*\\|%s[0-9]+ \\(NO\\|BAD\\).*\\)$" 
				  elmo-imap4-seq-prefix))
	      (progn (setq response-continue nil)
		     (setq elmo-imap4-read-point match-end)
		     (elmo-imap4-debug "error response: %s" response-string)
		     (setq return-value nil))
	    (setq elmo-imap4-read-point match-end)
	    (if not-command
		(setq response-continue nil))
	    (setq return-value 
		  (if return-value 
		      (concat return-value "\n" response-string)
		    response-string)))
	  (setq elmo-imap4-read-point match-end)))
      return-value)))

(defun elmo-imap4-read-contents (process)
  "Read OK response"
  (with-current-buffer (process-buffer process)
    (let ((case-fold-search nil)
	  (response-string nil)
	  match-end)
      (goto-char elmo-imap4-read-point)
      (while (not (re-search-forward 
		   (format "%s[0-9]+ \\(NO\\|BAD\\|OK\\).*$" 
			   elmo-imap4-seq-prefix)
		   nil t))
	(accept-process-output process)
	(goto-char elmo-imap4-read-point))
      (beginning-of-line)
      (setq match-end (point))
      (setq response-string (buffer-substring 
			     elmo-imap4-read-point match-end))
      (if (eq (length response-string) 0)
	  nil
	response-string))))

(defun elmo-imap4-read-bytes (buffer process bytes)
  (with-current-buffer buffer
    (let ((case-fold-search nil)
	  start gc-message return-value)
      (setq start elmo-imap4-read-point) ; starting point
      (while (< (point-max) (+ start bytes))
	(accept-process-output process))
      (setq return-value (buffer-substring
			  start (+ start bytes)))
      (setq return-value (elmo-delete-cr return-value))
      (setq elmo-imap4-read-point (+ start bytes))
      return-value)))

(defun elmo-imap4-read-body (buffer process bytes outbuf)
  (let (start gc-message ret-val)
    (with-current-buffer buffer
      (setq start elmo-imap4-read-point)
      (while (< (point-max) (+ start bytes))
	(accept-process-output process))      
      (with-current-buffer outbuf
	(erase-buffer)
	(insert-buffer-substring buffer start (+ start bytes))
	(setq ret-val (elmo-delete-cr-get-content-type)))
      (setq elmo-imap4-read-point (+ start bytes))
      ret-val)))

(defun elmo-imap4-send-string (process string)
  "Send STRING to server."
  (with-current-buffer (process-buffer process)
    (erase-buffer)
    (goto-char (point-min))
    (setq elmo-imap4-read-point (point))
    (process-send-string process string)
    (process-send-string process "\r\n")))
  
(defun elmo-imap4-commit (spec)
  (if (elmo-imap4-plugged-p spec)
      (let ((session (elmo-imap4-get-session spec)))
	(if elmo-imap4-use-select-to-update-status
	    (elmo-imap4-select-mailbox session
				       (elmo-imap4-spec-mailbox spec)
				       'force)
	  (elmo-imap4-select-mailbox session
				     (elmo-imap4-spec-mailbox spec))
	  (elmo-imap4-check session)))))

(defun elmo-imap4-check (session)
  (let ((process (elmo-network-session-process-internal session)))
    (elmo-imap4-send-command process "check")
    (elmo-imap4-read-response process)))

(defun elmo-imap4-select-mailbox (session mailbox &optional force)
  (when (or force
	    (not (string=
		  (elmo-imap4-session-current-mailbox-internal session)
		  mailbox)))
    (let ((process (elmo-network-session-process-internal session))
	  response)
      (unwind-protect
	  (progn
	    (elmo-imap4-send-command process
				     (list
				      "select "
				      (elmo-imap4-mailbox mailbox)))
	    (setq response (elmo-imap4-read-response process)))
	(if response
	    (elmo-imap4-session-set-current-mailbox-internal
	     session mailbox)
	  (elmo-imap4-session-set-current-mailbox-internal session nil)
	  (error "Select mailbox %s failed" mailbox))))))

(defun elmo-imap4-check-validity (spec validity-file)
  "get uidvalidity value from server and compare it with validity-file."
  (let* ((process (elmo-imap4-get-process spec))
	 response)
    (save-excursion
      (elmo-imap4-send-command process
			       (list "status "
				     (elmo-imap4-mailbox
				      (elmo-imap4-spec-mailbox spec))
				     " (uidvalidity)"))
      (setq response (elmo-imap4-read-response process))
      (if (string-match "UIDVALIDITY \\([0-9]+\\)" response)
	  (string= (elmo-get-file-string validity-file)
		   (elmo-match-string 1 response))
	nil))))

(defun elmo-imap4-sync-validity  (spec validity-file)
  "get uidvalidity value from server and save it to validity-file."
  (let* ((process (elmo-imap4-get-process spec))
	 response)
    (save-excursion
      (elmo-imap4-send-command process
			       (list "status "
				     (elmo-imap4-mailbox
				      (elmo-imap4-spec-mailbox spec))
				     " (uidvalidity)"))
      (setq response (elmo-imap4-read-response process))
      (if (string-match "UIDVALIDITY \\([0-9]+\\)" response)
	  (progn
	    (elmo-save-string
	     (elmo-match-string 1 response)
	     validity-file)
	    t)
	nil))))

(defun elmo-imap4-list (spec str)
  (save-excursion
    (let* ((session (elmo-imap4-get-session spec))
	   (process (elmo-network-session-process-internal session))
	   response ret-val beg end)
      (elmo-imap4-commit spec)
      (elmo-imap4-send-command process
			       (format (if elmo-imap4-use-uid 
					   "uid search %s"
					 "search %s") str))
      (setq response (elmo-imap4-read-response process))
      (if (and response (string-match "\\* SEARCH" response))
	  (progn
	    (setq response (substring response (match-end 0)))
	    (if (string-match "\n" response)
		(progn
		  (setq end (match-end 0))
		  (setq ret-val (read (concat "(" (substring 
						   response 
						   0 end) ")"))))
	      (error "SEARCH failed"))))
      ret-val)))

(defun elmo-imap4-list-folder (spec)
  (let ((killed (and elmo-use-killed-list
		     (elmo-msgdb-killed-list-load
		      (elmo-msgdb-expand-path nil spec))))
	numbers)
    (setq numbers (elmo-imap4-list spec "all"))
    (if killed
	(delq nil
	      (mapcar (lambda (number)
			(unless (memq number killed) number))
		      numbers))
      numbers)))

(defun elmo-imap4-list-folder-unread (spec mark-alist unread-marks)
  (if (elmo-imap4-use-flag-p spec)
      (elmo-imap4-list spec "unseen")
    (elmo-generic-list-folder-unread spec mark-alist unread-marks)))

(defun elmo-imap4-list-folder-important (spec overview)
  (and (elmo-imap4-use-flag-p spec)
       (elmo-imap4-list spec "flagged")))

(defun elmo-imap4-search-internal (process filter)
  (let ((search-key (elmo-filter-key filter))
	word response)
    (cond
     ((or (string= "since" search-key)
	  (string= "before" search-key))
      (setq search-key (concat "sent" search-key))
      (elmo-imap4-send-command process
			       (format 
				(if elmo-imap4-use-uid
				    "uid search %s %s" 
				  " search %s %s")
				search-key
				(elmo-date-get-description
				 (elmo-date-get-datevec 
				  (elmo-filter-value filter))))))
     (t
      (setq word (encode-mime-charset-string (elmo-filter-value filter) 
					     elmo-search-mime-charset))
      (elmo-imap4-send-command process
			       (list
				(if elmo-imap4-use-uid
				    "uid search CHARSET "
				  "search CHARSET ")
				(elmo-imap4-astring 
				 (symbol-name elmo-search-mime-charset))
				(if (eq (elmo-filter-type filter) 'unmatch)
				    " not " " ")
				(format "%s "
					(elmo-filter-key filter))
				(elmo-imap4-astring word)))))
    (if (null (setq response (elmo-imap4-read-response process)))
	(error "Search failed for %s" (elmo-filter-key filter)))
    (if (string-match "^\\* SEARCH\\([^\n]*\\)$" response)
	(read (concat "(" (elmo-match-string 1 response) ")"))
      (error "SEARCH failed"))))

(defun elmo-imap4-search (spec condition &optional from-msgs)
  (save-excursion
    (let* ((session (elmo-imap4-get-session spec))
	   (process (elmo-network-session-process-internal session))
	   response ret-val len word)
      (elmo-imap4-select-mailbox session 
				 (elmo-imap4-spec-mailbox spec))
      (while condition
	(setq response (elmo-imap4-search-internal process
						   (car condition)))
	(setq ret-val (nconc ret-val response))
	(setq condition (cdr condition)))
      (if from-msgs
	  (elmo-list-filter 
	   from-msgs
	   (elmo-uniq-list (sort ret-val '<)))
	(elmo-uniq-list (sort ret-val '<))))))

(defsubst elmo-imap4-value (value)
  (if (eq value 'NIL) nil
    value))

(defmacro elmo-imap4-nth (pos list)
  (` (let ((value (nth (, pos) (, list))))
       (if (eq 'NIL value)
	   nil
	 value))))
  
(defun elmo-imap4-use-flag-p (spec)
  (not (string-match elmo-imap4-disuse-server-flag-mailbox-regexp
		     (elmo-imap4-spec-mailbox spec))))

(defsubst elmo-imap4-make-address (name mbox host)
  (cond (name
	 (concat name " <" mbox "@" host ">"))
	(t
	 (concat mbox "@" host))))

(static-cond 
 ((fboundp 'float)
  ;; Emacs can parse dot symbol.
  (defvar elmo-imap4-rfc822-size "RFC822\.SIZE")
  (defvar elmo-imap4-header-fields "HEADER\.FIELDS")
  (defmacro elmo-imap4-replace-dot-symbols ()) ;; noop
  )
 (t 
  ;; Cannot parse dot symbol, replace it.
  (defvar elmo-imap4-rfc822-size "RFC822_SIZE")
  (defvar elmo-imap4-header-fields "HEADER_FIELDS")
  (defmacro elmo-imap4-replace-dot-symbols ()
    (goto-char (point-min))
    (while (re-search-forward "RFC822\\.SIZE" nil t)
      (replace-match elmo-imap4-rfc822-size))
    (goto-char (point-min))
    (while (re-search-forward "HEADER\\.FIELDS" nil t)
      (replace-match elmo-imap4-header-fields))
    (goto-char (point-min)))))

(defsubst elmo-imap4-make-attributes-object (string)
  (save-match-data
    (elmo-set-work-buf
     (elmo-set-buffer-multibyte nil)
     (insert string)
     (goto-char (point-min))
     (let ((case-fold-search t))
       (goto-char (point-min))
       (while (re-search-forward "{\\([0-9]+\\)}\r\n" nil t)
	 (let (str)
	   (goto-char (+ (point) 
			 (string-to-int (elmo-match-buffer 1))))
	   (setq str (save-match-data 
		       (elmo-replace-in-string 
			(buffer-substring (match-end 0) (point))
			"\r" "")))
	   (delete-region (match-beginning 0) (point))
	   (insert (prin1-to-string str))))
       (goto-char (point-min))
       (elmo-imap4-replace-dot-symbols)
       (read (current-buffer))))))


(defun elmo-imap4-parse-overview-string (string)
  (if (null string)
      (error "Getting overview failed"))
  (with-temp-buffer
    (let (ret-val beg attr number)
      (elmo-set-buffer-multibyte nil)
      (insert string)
      (goto-char (point-min))
      (setq beg (point))
      (if (re-search-forward "^\\* \\([0-9]+\\) FETCH"
			     nil t)
	  (progn
	    (setq beg (point))
	    (unless elmo-imap4-use-uid
	      (setq number (string-to-int (elmo-match-buffer 1))))
	    (while (re-search-forward 
		    "^\\* \\([0-9]+\\) FETCH"
		    nil t)
	      (setq attr (elmo-imap4-make-attributes-object
			  (buffer-substring beg (match-beginning 0))))
	      (setq beg (point))
	      (unless elmo-imap4-use-uid
		(setq attr (nconc (list 'UID number) attr))
		(setq number (string-to-int (elmo-match-buffer 1))))
	      (setq ret-val (cons attr ret-val)))
	    ;; process last one...
	    (setq attr (elmo-imap4-make-attributes-object
			(buffer-substring beg (point-max))))
	    (unless elmo-imap4-use-uid
	      (setq attr (nconc (list 'UID number) attr)))
	    (setq ret-val (cons attr ret-val))))
      (nreverse ret-val))))

(defun elmo-imap4-create-msgdb-from-overview-string (str
						     folder
						     new-mark
						     already-mark
						     seen-mark
						     important-mark
						     seen-list
						     &optional numlist)
  (let ((case-fold-search t)
	(size-sym (intern elmo-imap4-rfc822-size))
	overview attr-list attr pair section
	number important message-id from-list from-string
	to-string cc-string
	number-alist mark-alist
	reference subject date-string size flags gmark seen
	index extras extra-fields sym value)
    (setq attr-list (elmo-imap4-parse-overview-string str))
    (while attr-list
      (setq attr (car attr-list))
      ;; Remove section data. (origin octed is not considered.(OK?))
      (setq section (cadr (memq 'BODY attr)))
      (if (vectorp section)
	  (delq section attr))
      ;; number
      (setq number (cadr (memq 'UID attr)))
      (when (or (null numlist)
		(memq number numlist))
	(while attr
	  (setq sym (car attr))
	  (setq value (cadr attr))
	  (setq attr (cdr (cdr attr)))
	  (cond
	   ((eq sym 'UID))
	   ;; noop
	   ((eq sym 'FLAGS)
	    (setq flags value))
	   ((eq sym size-sym)
	    (setq size value))
	   ((eq sym 'BODY)
	    (setq extra-fields (elmo-collect-field-from-string value t)))
	   ((eq sym 'ENVELOPE)
	    ;; According to rfc2060, 
	    ;; 0 date, 1 subject, 2 from, 3 sender,
	    ;; 4 reply-to, 5 to, 6 cc, 7 bcc, 8 in-reply-to, 9 message-id.
	    (setq date-string (elmo-imap4-nth 0 value))
	    (setq subject (elmo-mime-string (or (elmo-imap4-nth 1 value)
						elmo-no-subject)))
	    (setq from-list (car (elmo-imap4-nth 2 value)))
	    (setq from-string (or
			       (and (or (elmo-imap4-nth 0 from-list)
					(elmo-imap4-nth 2 from-list)
					(elmo-imap4-nth 3 from-list))
				    (elmo-delete-char 
				     ?\"
				     (elmo-imap4-make-address
				      (elmo-imap4-nth 0 from-list)
				      (elmo-imap4-nth 2 from-list)
				      (elmo-imap4-nth 3 from-list))
				     'uni))
			       elmo-no-from))
	    (setq to-string (mapconcat
			     (lambda (to)
			       (elmo-imap4-make-address
				(elmo-imap4-nth 0 to)
				(elmo-imap4-nth 2 to)
				(elmo-imap4-nth 3 to)))
			     (elmo-imap4-nth 5 value) ","))
	    (setq cc-string (mapconcat
			     (lambda (cc)
			       (elmo-imap4-make-address
				(elmo-imap4-nth 0 cc)
				(elmo-imap4-nth 2 cc)
				(elmo-imap4-nth 3 cc)))
			     (elmo-imap4-nth 6 value) ","))	
	    (setq reference (elmo-msgdb-get-last-message-id
			     (elmo-imap4-nth 8 value)))
	    (setq message-id (elmo-imap4-nth 9 value)))))
	(when (setq pair (assoc "references" extra-fields))
	  (setq extra-fields (delq pair extra-fields)))
	(unless reference
	  (setq reference (elmo-msgdb-get-last-message-id (cdr pair))))
	(setq overview
	      (elmo-msgdb-append-element
	       overview
	       (cons message-id
		     (vector number
			     reference
			     (elmo-mime-string from-string)
			     (elmo-mime-string subject)
			     date-string
			     to-string
			     cc-string
			     size
			     extra-fields))))
	(if (memq 'Flagged flags)
	    (elmo-msgdb-global-mark-set message-id important-mark))
	(setq number-alist
	      (elmo-msgdb-number-add number-alist number message-id))
	(setq seen (member message-id seen-list))
	(if (setq gmark (or (elmo-msgdb-global-mark-get message-id)
			    (if (elmo-cache-exists-p message-id) ;; XXX
				(if (or (memq 'Seen flags) seen)
				    nil
				  already-mark)
			      (if (or (memq 'Seen flags) seen)
				  (if elmo-imap4-use-cache
				      seen-mark)
				new-mark))))
	    (setq mark-alist (elmo-msgdb-mark-append
			      mark-alist
			      number
			      ;; managing mark with message-id is evil.
			      gmark))))
      (setq attr-list (cdr attr-list)))
    (list overview number-alist mark-alist)))

(defun elmo-imap4-add-to-cont-list (cont-list msg)
  (let ((elist cont-list)
	(ret-val cont-list)
	entity found)
    (while (and elist (not found))
      (setq entity (car elist))
      (cond 
       ((and (consp entity)
	     (eq (+ 1 (cdr entity)) msg))
	(setcdr entity msg)
	(setq found t))
       ((and (integerp entity)
	     (eq (+ 1 entity) msg))
	(setcar elist (cons entity msg))
	(setq found t))
       ((or (and (integerp entity) (eq entity msg))
	    (and (consp entity) 
		 (<= (car entity) msg)
		 (<= msg (cdr entity)))) ; included
	(setq found t))); noop
      (setq elist (cdr elist)))
    (if (not found)
	(setq ret-val (append cont-list (list msg))))
    ret-val))

(defun elmo-imap4-make-number-set-list (msg-list &optional chop-length)
  "Make RFC2060's message set specifier from MSG-LIST.
Returns a list of (NUMBER . SET-STRING).
SET-STRING is the message set specifier described in RFC2060.
NUMBER is contained message number in SET-STRING.
Every SET-STRING does not contain number of messages longer than CHOP-LENGTH.
If CHOP-LENGTH is not specified, message set is not chopped."
  (let (count cont-list set-list)
    (setq msg-list (sort msg-list '<))
    (while msg-list
      (setq cont-list nil)
      (setq count 0)
      (unless chop-length
	(setq chop-length (length msg-list)))
      (while (and (not (null msg-list))
		  (< count chop-length))
	(setq cont-list 
	      (elmo-imap4-add-to-cont-list 
	       cont-list (car msg-list)))
	(incf count)
	(setq msg-list (cdr msg-list)))
      (setq set-list 
	    (cons
	     (cons
	      count
	      (mapconcat
	       (lambda (x)
		 (cond ((consp x)
			(format "%s:%s" (car x) (cdr x)))
		       ((integerp x)
			(int-to-string x))))
	       cont-list
	       ","))
	     set-list)))
    (nreverse set-list)))

;;
;; set mark
;; read-mark -> "\\Seen"
;; important -> "\\Flagged"
;; 
;; (delete -> \\Deleted)
(defun elmo-imap4-mark-set-on-msgs (spec msgs mark &optional unmark no-expunge)
  "SET flag of MSGS as MARK.
If optional argument UNMARK is non-nil, unmark."
  (save-excursion
    (let* ((session (elmo-imap4-get-session spec))
	   (process (elmo-network-session-process-internal session))
	   (msg-list (copy-sequence msgs))
	   set-list ent)
      (elmo-imap4-select-mailbox session 
				 (elmo-imap4-spec-mailbox spec))
      (setq set-list (elmo-imap4-make-number-set-list msg-list))
      (when set-list
	(elmo-imap4-send-command process
				 (format 
				  (if elmo-imap4-use-uid 
				      "uid store %s %sflags.silent (%s)"
				    "store %s %sflags.silent (%s)")
				  (cdr (car set-list))
				  (if unmark "-" "+")
				  mark))
	(unless (elmo-imap4-read-response process)
	  (error "Store %s flag failed" mark))
	(unless no-expunge
	  (elmo-imap4-send-command process "expunge")
	  (unless (elmo-imap4-read-response process)
	    (error "Expunge failed"))))
      t)))

(defun elmo-imap4-mark-as-important (spec msgs)
  (and (elmo-imap4-use-flag-p spec)
       (elmo-imap4-mark-set-on-msgs spec msgs "\\Flagged" nil 'no-expunge)))

(defun elmo-imap4-mark-as-read (spec msgs)
  (and (elmo-imap4-use-flag-p spec)
       (elmo-imap4-mark-set-on-msgs spec msgs "\\Seen" nil 'no-expunge)))

(defun elmo-imap4-unmark-important (spec msgs)
  (and (elmo-imap4-use-flag-p spec)
       (elmo-imap4-mark-set-on-msgs spec msgs "\\Flagged" 'unmark 
				    'no-expunge)))

(defun elmo-imap4-mark-as-unread (spec msgs)
  (and (elmo-imap4-use-flag-p spec)
       (elmo-imap4-mark-set-on-msgs spec msgs "\\Seen" 'unmark 'no-expunge)))

(defun elmo-imap4-delete-msgs (spec msgs)
  (elmo-imap4-mark-set-on-msgs spec msgs "\\Deleted"))

(defun elmo-imap4-delete-msgs-no-expunge (spec msgs)
  (elmo-imap4-mark-set-on-msgs spec msgs "\\Deleted" nil 'no-expunge))

(defun elmo-imap4-msgdb-create-as-numlist (spec numlist new-mark already-mark 
						seen-mark important-mark 
						seen-list)
  "Create msgdb for SPEC for NUMLIST."
  (elmo-imap4-msgdb-create spec numlist new-mark already-mark
			   seen-mark important-mark seen-list t))

(defun elmo-imap4-msgdb-create (spec numlist new-mark already-mark seen-mark 
				     important-mark seen-list &optional as-num)
  "Create msgdb for SPEC."
  (when numlist
    (save-excursion
      (let* ((session (elmo-imap4-get-session spec))
	     (process    (elmo-network-session-process-internal session))
	     (filter     (and as-num numlist))
	     (case-fold-search t)
	     (extra-fields (if elmo-msgdb-extra-fields
			       (concat " " (mapconcat
					    'identity
					    elmo-msgdb-extra-fields " "))
			     ""))
	     rfc2060 count ret-val set-list ov-str length)
	(setq rfc2060 (with-current-buffer (process-buffer process)
			(if (memq 'imap4rev1
				  (elmo-imap4-session-capability-internal
				   session))
			    t
			  (if (memq 'imap4
				    (elmo-imap4-session-capability-internal
				     session))
			      nil
			    (error "No IMAP4 capability!!")))))
	(setq count 0)
	(setq length (length numlist))
	(setq set-list (elmo-imap4-make-number-set-list 
			numlist
			elmo-imap4-overview-fetch-chop-length))
	(message "Getting overview...")
	(elmo-imap4-select-mailbox session 
				   (elmo-imap4-spec-mailbox spec))
	(while set-list
	  (elmo-imap4-send-command 
	   process
	   ;; get overview entity from IMAP4
	   (format 
	    (if rfc2060
		(concat
		 (if elmo-imap4-use-uid "uid " "")
		 "fetch %s (envelope body.peek[header.fields (references"
		 extra-fields
		 ")] rfc822.size flags)")
	      (concat
	       (if elmo-imap4-use-uid "uid " "")
	       "fetch %s (envelope rfc822.size flags)"))
	    (cdr (car set-list))))
	  ;; process string while waiting for response
	  (with-current-buffer (process-buffer process)
	    (if ov-str
		(setq ret-val
		      (elmo-msgdb-append 
		       ret-val
		       (elmo-imap4-create-msgdb-from-overview-string 
			ov-str
			(elmo-imap4-spec-mailbox spec)
			new-mark already-mark seen-mark important-mark
			seen-list filter)))))
	  (setq count (+ count (car (car set-list))))
	  (setq ov-str (elmo-imap4-read-contents process))
	  (when (> length elmo-display-progress-threshold)
	    (elmo-display-progress
	     'elmo-imap4-msgdb-create "Getting overview..." 
	     (/ (* count 100) length)))
	  (setq set-list (cdr set-list)))
	;; process last one.
	(with-current-buffer (process-buffer process)
	  (if ov-str
	      (setq ret-val
		    (elmo-msgdb-append 
		     ret-val
		     (elmo-imap4-create-msgdb-from-overview-string 
		      ov-str 
		      (elmo-imap4-spec-mailbox spec)
		      new-mark already-mark seen-mark important-mark
		      seen-list filter)))))
	(message "Getting overview...done.")
	ret-val))))

(defun elmo-imap4-parse-response (string)
  (if (string-match "^\\*\\(.*\\)$" string)
      (read (concat "(" (elmo-match-string 1 string) ")"))))

(defun elmo-imap4-parse-capability (string)
  (if (string-match "^\\*\\(.*\\)$" string)
      (read (concat "(" (downcase (elmo-match-string 1 string)) ")"))))

(defun elmo-imap4-parse-namespace (obj)
  (let ((ns (cdr obj))
	(i 0)
	prefix delim
	cur namespace-alist)
    ;; 0: personal, 1: other, 2: shared
    (while (< i 3)
      (setq cur (elmo-imap4-nth i ns))
      (incf i)
      (while cur
	(setq prefix (elmo-imap4-nth 0 (car cur)))
	(setq delim    (elmo-imap4-nth 1 (car cur)))
	(if (and prefix delim
		 (string-match (concat "\\(.*\\)" 
				       (regexp-quote delim)
				       "\\'")
			       prefix))
	    (setq prefix (substring prefix (match-beginning 1)(match-end 1))))
	(setq namespace-alist (nconc namespace-alist
				     (list (cons
					    (concat "^" (regexp-quote prefix)
						    ".*$")
					    delim))))
	(setq cur (cdr cur))))
    (append
     elmo-imap4-extra-namespace-alist
     (sort namespace-alist
	   (function
	    (lambda (x y)
	      (> (length (car x))
		 (length (car y)))))))))

;; Current buffer is process buffer.
(defun elmo-imap4-auth-login (session)
  (elmo-imap4-send-command
   (elmo-network-session-process-internal session)
   "authenticate login" 'no-lock)
  (or (elmo-imap4-read-response
       (elmo-network-session-process-internal session)
       t)
      (signal 'elmo-authenticate-error
	      '(elmo-imap4-auth-login)))
  (elmo-imap4-send-string
   (elmo-network-session-process-internal session)
   (elmo-base64-encode-string
    (elmo-network-session-user-internal session)))
  (or (elmo-imap4-read-response
       (elmo-network-session-process-internal session)
       t)
	(signal 'elmo-authenticate-error
		'(elmo-imap4-auth-login)))
  (elmo-imap4-send-string
   (elmo-network-session-process-internal session)
   (elmo-base64-encode-string
    (elmo-get-passwd (elmo-network-session-password-key session))))
  (or (elmo-imap4-read-response 
       (elmo-network-session-process-internal session))
      (signal 'elmo-authenticate-error
	      '(elmo-imap4-auth-login))))

(defun elmo-imap4-auth-cram-md5 (session)
  (let ((process (elmo-network-session-process-internal session)) response)
    (elmo-imap4-send-command 
     process
     "authenticate cram-md5" 'no-lock)
    (or (setq response (elmo-imap4-read-response process t))
	(signal 'elmo-authenticate-error
		'(elmo-imap4-auth-cram-md5)))      
    (setq response (cadr (split-string response " ")))
    (elmo-imap4-send-string
     process
     (elmo-base64-encode-string
      (sasl-cram-md5 (elmo-network-session-user-internal session)
		     (elmo-get-passwd
		      (elmo-network-session-password-key session))
		     (elmo-base64-decode-string response))))
    (or (elmo-imap4-read-response process)
	(signal 'elmo-authenticate-error
		'(elmo-imap4-auth-cram-md5)))))

(defun elmo-imap4-auth-digest-md5 (session)
  (let ((process (elmo-network-session-process-internal session))
	response)
      (elmo-imap4-send-command 
       process "authenticate digest-md5" 'no-lock)
      (setq response (elmo-imap4-read-response process t))
      (or response
	(signal 'elmo-authenticate-error
		'(elmo-imap4-auth-digest-md5)))
      (setq response (cadr (split-string response " ")))
      (elmo-imap4-send-string
       process
       (elmo-base64-encode-string
	(sasl-digest-md5-digest-response
	 (elmo-base64-decode-string response)
	 (elmo-network-session-user-internal session)
	 (elmo-get-passwd (elmo-network-session-password-key session))
	 "imap"
	 (elmo-network-session-password-key session))
	'no-line-break))
      (or (elmo-imap4-read-response process t)
	  (signal 'elmo-authenticate-error
		  '(elmo-imap4-auth-digest-md5)))
      (elmo-imap4-send-string process "")
      (or (elmo-imap4-read-response process)
	  (signal 'elmo-authenticate-error
		  '(elmo-imap4-auth-digest-md5)))))

(defun elmo-imap4-login (session)
  (elmo-imap4-send-command
   (elmo-network-session-process-internal session)
   (list "login " (elmo-imap4-userid 
		   (elmo-network-session-user-internal session))
	 " "
	 (elmo-imap4-password
	  (elmo-get-passwd (elmo-network-session-password-key session))))
   nil 'no-log)
  (or (elmo-imap4-read-response
       (elmo-network-session-process-internal session))
      (signal 'elmo-authenticate-error
	      '(elmo-imap4-auth-digest-md5))))

(luna-define-method elmo-network-initialize-session ((session 
						      elmo-imap4-session))
  (let ((process (elmo-network-session-process-internal session))
	response greeting capability mechanism)
    (with-current-buffer (process-buffer process)
      (elmo-set-buffer-multibyte nil)
      (buffer-disable-undo (current-buffer))
      (make-variable-buffer-local 'elmo-imap4-lock)
      (make-local-variable 'elmo-imap4-read-point)
      (setq elmo-imap4-read-point (point-min))
      (set-process-filter process 'elmo-imap4-process-filter)
      ;; greeting
      (elmo-network-session-set-greeting-internal
       session
       (elmo-imap4-read-response process t))
      (unless (elmo-network-session-greeting-internal session)
	(signal 'elmo-open-error
		'(elmo-network-initialize-session)))
      (elmo-imap4-send-command process "capability")
      (elmo-imap4-session-set-capability-internal
       session
       (elmo-imap4-parse-capability
	(elmo-imap4-read-response process)))
      (when (eq (elmo-network-stream-type-symbol
		 (elmo-network-session-stream-type-internal session))
		'starttls)
	(or (memq 'starttls capability)
	    (signal 'elmo-open-error
		    '(elmo-network-initialize-session)))
	(elmo-imap4-send-command process "starttls")
	(setq response
	      (elmo-imap4-read-response process))
	(if (string-match 
	     (concat "^\\(" elmo-imap4-seq-prefix 
		     (int-to-string elmo-imap4-seqno)
		     "\\|\\*\\) OK")
	     response)
	    (starttls-negotiate process))))))

(luna-define-method elmo-network-authenticate-session ((session 
							elmo-imap4-session))
  (unless (string-match "^\\* PREAUTH"
			(elmo-network-session-greeting-internal session))
    (unless (or (not (elmo-network-session-auth-internal session))
		(and (memq (intern
			    (format "auth=%s"
				    (elmo-network-session-auth-internal
				     session)))
			   (elmo-imap4-session-capability-internal session))
		     (assq
		      (elmo-network-session-auth-internal session)
		      elmo-imap4-authenticator-alist)))
      (if (or elmo-imap4-force-login
	      (y-or-n-p
	       (format
		"There's no %s capability in server. continue?"
		(elmo-network-session-auth-internal session))))
	  (elmo-network-session-set-auth-internal session nil)
	(signal 'elmo-open-error
		'(elmo-network-initialize-session))))
    (let ((authenticator
	   (if (elmo-network-session-auth-internal session)
	       (nth 1 (assq
		       (elmo-network-session-auth-internal session)
		       elmo-imap4-authenticator-alist))
	     'elmo-imap4-login)))
      (funcall authenticator session))))

(luna-define-method elmo-network-setup-session ((session 
						 elmo-imap4-session))
  (let ((process (elmo-network-session-process-internal session)))
    (with-current-buffer (process-buffer process)
      ;; get namespace of server if possible.
      (when (memq 'namespace (elmo-imap4-session-capability-internal session))
	(elmo-imap4-send-command process "namespace")
	(setq elmo-imap4-server-namespace
	      (elmo-imap4-parse-namespace
	       (elmo-imap4-parse-response
		(elmo-imap4-read-response process))))))))

(defun elmo-imap4-get-seqno ()
  (setq elmo-imap4-seqno (+ 1 elmo-imap4-seqno)))

(defun elmo-imap4-setup-send-buffer (string)
  (let ((tmp-buf (get-buffer-create " *elmo-imap4-setup-send-buffer*")))
    (save-excursion
      (save-match-data
	(set-buffer tmp-buf)
	(erase-buffer)
	(elmo-set-buffer-multibyte nil)
	(insert string)
	(goto-char (point-min))
	(if (eq (re-search-forward "^$" nil t) 
		(point-max))
	    (insert "\n"))
	(goto-char (point-min))
	(while (search-forward "\n" nil t)
	  (replace-match "\r\n"))))
    tmp-buf))

(defun elmo-imap4-send-command (process command &optional no-lock no-log)
  "Send COMMAND to the PROCESS."
  (with-current-buffer (process-buffer process)
    (when (and elmo-imap4-use-lock
	       elmo-imap4-lock)
      (elmo-imap4-debug "send: (%d) is still locking." elmo-imap4-seqno)
      (error "IMAP4 process is locked; Please try later (or plug again)"))
    (erase-buffer)
    (goto-char (point-min))
    (setq elmo-imap4-read-point (point))
    (unless no-lock
      (setq elmo-imap4-lock t))
    (let* ((command-args (if (listp command)
			     command
			   (list command)))
	   (seqno (elmo-imap4-get-seqno))
	   (cmdstr (concat elmo-imap4-seq-prefix
			   (number-to-string seqno) " "))
	   token kind formatter)
      (while (setq token (car command-args))
	(cond ((stringp token)   ; formatted
	       (setq cmdstr (concat cmdstr token)))
	      ((listp token)     ; unformatted
	       (setq kind (car token))
	       (cond ((eq kind 'atom)
		      (setq cmdstr (concat cmdstr (nth 1 token))))
		     ((eq kind 'quoted)
		      (setq cmdstr (concat cmdstr
					   (elmo-imap4-format-quoted (nth 1 token)))))
		     ((eq kind 'literal)
		      (setq cmdstr (concat cmdstr (format "{%d}" (nth 2 token))))
		      (unless no-lock
			(if no-log
			    (elmo-imap4-debug "lock(%d): (No-logging command)." seqno)
			  (elmo-imap4-debug "lock(%d): %s" seqno cmdstr)))
		      (process-send-string process cmdstr)
		      (process-send-string process "\r\n")
		      (setq cmdstr nil)
		      (if (null (elmo-imap4-read-response process t))
			  (error "No response from server"))
		      (cond ((stringp (nth 1 token))
			     (setq cmdstr (nth 1 token)))
			    ((bufferp (nth 1 token))
			     (with-current-buffer (nth 1 token)
			       (process-send-region process
						    (point-min)
						    (+ (point-min) (nth 2 token)))))
			    (t
			     (error "Wrong argument for literal"))))
		     (t
		      (error "Unknown token kind %s" kind))))
	      (t
	       (error "Invalid argument")))
	(setq command-args (cdr command-args)))
      (unless no-lock
	(if no-log
	    (elmo-imap4-debug "lock(%d): (No-logging command)." seqno)
	  (elmo-imap4-debug "lock(%d): %s" seqno cmdstr)))
      (if cmdstr
	  (process-send-string process cmdstr))
      (process-send-string process "\r\n"))
      ))

(defun elmo-imap4-read-part (folder msg part)
  (let* ((spec (elmo-folder-get-spec folder))
	 (session (elmo-imap4-get-session spec))
	 (process (elmo-network-session-process-internal session))
	 response ret-val bytes)
    (elmo-imap4-select-mailbox session 
			       (elmo-imap4-spec-mailbox spec))
    (elmo-imap4-send-command process
			     (format 
			      (if elmo-imap4-use-uid 
				  "uid fetch %s body.peek[%s]"
				"fetch %s body.peek[%s]")
			      msg part))
    (if (null (setq response (elmo-imap4-read-response 
			      process t)))
	(error "Fetch failed"))
    (save-match-data
      (while (string-match "^\\* OK" response)
	(if (null (setq response (elmo-imap4-read-response 
				  process t)))
	    (error "Fetch failed"))))
    (save-match-data
      (if (string-match ".*{\\([0-9]+\\)}" response)
	  (setq bytes
		(string-to-int
		 (elmo-match-string 1 response)))
	(error "Fetch failed")))
    (if (null (setq response (elmo-imap4-read-bytes 
			      (process-buffer process) process bytes)))
	(error "Fetch message failed"))
    (setq ret-val response)
    (elmo-imap4-read-response process) ;; ignore remaining..
    ret-val))

(defun elmo-imap4-prefetch-msg (spec msg outbuf)
  (elmo-imap4-read-msg spec msg outbuf 'unseen))

(defun elmo-imap4-read-msg (spec msg outbuf 
				 &optional leave-seen-flag-untouched)
  (let* ((session (elmo-imap4-get-session spec))
	 (process (elmo-network-session-process-internal session))
	 response ret-val bytes)
    (as-binary-process
     (elmo-imap4-select-mailbox session 
				(elmo-imap4-spec-mailbox spec))
     (elmo-imap4-send-command process
			      (format 
			       (if elmo-imap4-use-uid 
				   "uid fetch %s body%s[]" 
				 "fetch %s body%s[]")
			       msg
			       (if leave-seen-flag-untouched
				   ".peek" "")))
     (if (null (setq response (elmo-imap4-read-response 
			       process t)))
	 (error "Fetch failed"))
     (save-match-data
       (while (string-match "^\\* OK" response)
	 (if (null (setq response (elmo-imap4-read-response 
				   process t)))
	     (error "Fetch failed"))))
     (save-match-data
       (if (string-match ".*{\\([0-9]+\\)}" response)
	   (setq bytes
		 (string-to-int
		  (elmo-match-string 1 response)))
	 (error "Fetch failed")))
     (setq ret-val (elmo-imap4-read-body 
		    (process-buffer process) 
		    process bytes outbuf))
     (elmo-imap4-read-response process)) ;; ignore remaining..
    ret-val))

(defun elmo-imap4-setup-send-buffer-from-file (file)
  (let ((tmp-buf (get-buffer-create 
		  " *elmo-imap4-setup-send-buffer-from-file*")))
    (save-excursion
      (save-match-data
	(set-buffer tmp-buf)
	(erase-buffer)
	(as-binary-input-file
	 (insert-file-contents file))
	(goto-char (point-min))
	(if (eq (re-search-forward "^$" nil t) 
		(point-max))
	    (insert "\n"))	
	(goto-char (point-min))
	(while (search-forward "\n" nil t)
	  (replace-match "\r\n"))))
    tmp-buf))

(defun elmo-imap4-delete-msgids (spec msgids)
  "If actual message-id is matched, then delete it."
  (let ((message-ids msgids)
	(i 0)
	(num (length msgids)))
    (while message-ids
      (setq i (+ 1 i))
      (message "Deleting message...%d/%d" i num)
      (elmo-imap4-delete-msg-by-id spec (car message-ids))
      (setq message-ids (cdr message-ids)))
    (let* ((session (elmo-imap4-get-session spec))
	   (process (elmo-network-session-process-internal session)))
      (elmo-imap4-send-command process "expunge")
      (if (null (elmo-imap4-read-response process))
	  (error "Expunge failed")))))

(defun elmo-imap4-delete-msg-by-id (spec msgid)
  (let* ((session (elmo-imap4-get-session spec))
	 (process (elmo-network-session-process-internal session))
	 response msgs)
    (elmo-imap4-select-mailbox session 
			       (elmo-imap4-spec-mailbox spec))
    (elmo-imap4-send-command process
			     (list
			      (if elmo-imap4-use-uid 
				  "uid search header message-id "
				"search header message-id ")
			      (elmo-imap4-field-body msgid)))
    (setq response (elmo-imap4-read-response process))
    (if (and response 
	     (string-match "^\\* SEARCH\\([^\n]*\\)$" response))
	(setq msgs (read (concat "(" (elmo-match-string 1 response) ")")))
      (error "SEARCH failed"))
    (elmo-imap4-delete-msgs-no-expunge spec msgs)))

(defun elmo-imap4-append-msg-by-id (spec msgid)
  (let* ((session (elmo-imap4-get-session spec))
	 (process (elmo-network-session-process-internal session))
	 send-buf)
    (elmo-imap4-select-mailbox session 
			       (elmo-imap4-spec-mailbox spec))
    (setq send-buf (elmo-imap4-setup-send-buffer-from-file 
		    (elmo-cache-get-path msgid)))
    (elmo-imap4-send-command
     process
     (list
      "append "
      (elmo-imap4-mailbox (elmo-imap4-spec-mailbox spec))
      " (\\Seen) "
      (elmo-imap4-buffer-literal send-buf)))
    (kill-buffer send-buf)
    (if (null (elmo-imap4-read-response process))
	(error "Append failed")))
  t)

(defun elmo-imap4-append-msg (spec string &optional msg no-see)
  (let* ((session (elmo-imap4-get-session spec))
	 (process (elmo-network-session-process-internal session))
	 send-buf)
    (elmo-imap4-select-mailbox session 
			       (elmo-imap4-spec-mailbox spec))
    (setq send-buf (elmo-imap4-setup-send-buffer string))
    (elmo-imap4-send-command
     process
     (list
      "append "
      (elmo-imap4-mailbox (elmo-imap4-spec-mailbox spec))
      (if no-see " " " (\\Seen) ")
      (elmo-imap4-buffer-literal send-buf)))
    (kill-buffer send-buf)
    ;;(current-buffer)
    (if (null (elmo-imap4-read-response process))
	(error "Append failed")))
  t)

(defun elmo-imap4-copy-msgs (dst-spec
			     msgs src-spec &optional expunge-it same-number)
  "Equivalence of hostname, username is assumed."
  (let* ((src-folder (elmo-imap4-spec-mailbox src-spec))
	 (dst-folder (elmo-imap4-spec-mailbox dst-spec))
	 (session (elmo-imap4-get-session src-spec))
	 (process (elmo-network-session-process-internal session))
	 (mlist msgs))
    (elmo-imap4-select-mailbox session 
			       (elmo-imap4-spec-mailbox src-spec))
    (while mlist
      (elmo-imap4-send-command process
			       (list
				(format
				 (if elmo-imap4-use-uid 
				     "uid copy %s " 
				   "copy %s ")
				 (car mlist))
				(elmo-imap4-mailbox dst-folder)))
      (if (null (elmo-imap4-read-response process))
	  (error "Copy failed")
	(setq mlist (cdr mlist))))
    (when expunge-it
      (elmo-imap4-send-command process "expunge")
      (if (null (elmo-imap4-read-response process))
	  (error "Expunge failed")))
    t))

(defun elmo-imap4-server-diff (spec)
  "get server status"
  (let* ((session (elmo-imap4-get-session spec))
	 (process (elmo-network-session-process-internal session))
	 response)
    ;; commit.
    (elmo-imap4-commit spec)
    (elmo-imap4-send-command process
			     (list 
			      "status "
			      (elmo-imap4-mailbox
			       (elmo-imap4-spec-mailbox spec))
			      " (unseen messages)"))
    (setq response (elmo-imap4-read-response process))
    (when (string-match "\\* STATUS [^(]* \\(([^)]*)\\)" response)
      (setq response (read (downcase (elmo-match-string 1 response))))
      (cons (cadr (memq 'unseen response))
	    (cadr (memq 'messages response))))))

(defun elmo-imap4-use-cache-p (spec number)
  elmo-imap4-use-cache)

(defun elmo-imap4-local-file-p (spec number)
  nil)

(defun elmo-imap4-port-label (spec)
  (concat "imap4"
	  (if (elmo-imap4-spec-stream-type spec)
	      (concat "!" (symbol-name
			   (elmo-network-stream-type-symbol
			    (elmo-imap4-spec-stream-type spec)))))))
	      

(defsubst elmo-imap4-portinfo (spec)
  (list (elmo-imap4-spec-hostname spec) (elmo-imap4-spec-port spec)))

(defun elmo-imap4-plugged-p (spec)
  (apply 'elmo-plugged-p
	 (append (elmo-imap4-portinfo spec)
		 (list nil (quote (elmo-imap4-port-label spec))))))

(defun elmo-imap4-set-plugged (spec plugged add)
  (apply 'elmo-set-plugged plugged
	 (append (elmo-imap4-portinfo spec)
		 (list nil nil (quote (elmo-imap4-port-label spec)) add))))

(defalias 'elmo-imap4-sync-number-alist 'elmo-generic-sync-number-alist)

(provide 'elmo-imap4)

;;; elmo-imap4.el ends here
