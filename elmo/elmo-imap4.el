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
(defvar elmo-imap4-connection-cache nil
  "Cache of imap connection.")
(defvar elmo-imap4-use-uid t
  "Use UID as message number.")

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

(defun elmo-imap4-flush-connection ()
  (interactive)
  (let ((cache elmo-imap4-connection-cache)
	buffer process)
    (while cache
      (setq buffer (car (cdr (car cache))))
      (if buffer (kill-buffer buffer))
      (setq process (car (cdr (cdr (car cache)))))
      (if process (delete-process process))
      (setq cache (cdr cache)))
    (setq elmo-imap4-connection-cache nil)))

(defsubst elmo-imap4-get-process (spec)
  (elmo-imap4-connection-get-process (elmo-imap4-get-connection spec)))

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
		(append mailbox-list
			(list val)))))
      mailbox-list)))

(defun elmo-imap4-list-folders (spec &optional hierarchy)
  (save-excursion
    (let* ((root (elmo-imap4-spec-folder spec))
	   (process (elmo-imap4-get-process spec))
	   (delim (or
		 (cdr
		  (elmo-string-matched-assoc root
					     (save-excursion
					       (set-buffer
						(process-buffer process))
					       elmo-imap4-server-namespace)))
		 "/"))
	   response result append-serv ssl)
      ;; Append delimiter
      (if (and root
	       (not (string= root ""))
	       (not (string-match (concat "\\(.*\\)"
					  (regexp-quote delim)
					  "\\'")
				  root)))
	  (setq root (concat root delim)))
      (elmo-imap4-send-command (process-buffer process)
			       process
			       (format "list \"%s\" *" root))
      (setq response (elmo-imap4-read-response (process-buffer process)
					       process))
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
      (unless (eq (setq ssl (elmo-imap4-spec-ssl spec))
		  elmo-default-imap4-ssl)
	(if ssl
	    (setq append-serv (concat append-serv "!")))
	(if (eq ssl 'starttls)
	    (setq append-serv (concat append-serv "!"))))
      (mapcar '(lambda (fld)
		 (concat "%" (elmo-imap4-decode-folder-string fld)
			 (and append-serv
			      (eval append-serv))))
	      result))))

(defun elmo-imap4-folder-exists-p (spec)
  (let ((process (elmo-imap4-get-process spec)))
    (elmo-imap4-send-command (process-buffer process)
			     process
			     (format "status \"%s\" (messages)"
				     (elmo-imap4-spec-folder spec)))
    (elmo-imap4-read-response (process-buffer process) process)))

(defun elmo-imap4-folder-creatable-p (spec)
  t)

(defun elmo-imap4-create-folder-maybe (spec dummy)
  "Create folder if necessary."
  (if (not (elmo-imap4-folder-exists-p spec))
      (elmo-imap4-create-folder spec)))

(defun elmo-imap4-create-folder (spec)
  (let ((process (elmo-imap4-get-process spec))
	(folder (elmo-imap4-spec-folder spec)))
    (when folder
;;     For UW imapd 4.6, this workaround is needed to create #mh mailbox.
;      (if (string-match "^\\(#mh/\\).*[^/]$" folder)
;	  (setq folder (concat folder "/"))) ;; make directory
      (elmo-imap4-send-command (process-buffer process)
			       process
			       (format "create %s" folder))
      (if (null (elmo-imap4-read-response (process-buffer process)
					  process))
	  (error "Create folder %s failed" folder)
	t))))

(defun elmo-imap4-delete-folder (spec)
  (let ((process (elmo-imap4-get-process spec))
	msgs)
    (when (elmo-imap4-spec-folder spec)
      (when (setq msgs (elmo-imap4-list-folder spec))
	(elmo-imap4-delete-msgs spec msgs))
      (elmo-imap4-send-command (process-buffer process) process "close")
      (elmo-imap4-read-response (process-buffer process) process)
      (elmo-imap4-send-command (process-buffer process)
			       process
			       (format "delete %s"
				       (elmo-imap4-spec-folder spec)))
      (if (null (elmo-imap4-read-response (process-buffer process)
					  process))
	  (error "Delete folder %s failed" (elmo-imap4-spec-folder spec))
	t))))

(defun elmo-imap4-rename-folder (old-spec new-spec)
  (let ((process (elmo-imap4-get-process old-spec)))
    (when (elmo-imap4-spec-folder old-spec)
      (elmo-imap4-send-command (process-buffer process) process "close")
      (elmo-imap4-read-response (process-buffer process) process)
      (elmo-imap4-send-command (process-buffer process)
			       process
			       (format "rename %s %s"
				       (elmo-imap4-spec-folder old-spec)
				       (elmo-imap4-spec-folder new-spec)))
      (if (null (elmo-imap4-read-response (process-buffer process) process))
	  (error "Rename folder from %s to %s failed"
		 (elmo-imap4-spec-folder old-spec)
		 (elmo-imap4-spec-folder new-spec))
	t))))

(defun elmo-imap4-max-of-folder (spec)
  (save-excursion
    (let* ((process (elmo-imap4-get-process spec))
	   response)
      (elmo-imap4-send-command (process-buffer process)
			       process
			       (format "status \"%s\" (uidnext messages)"
				       (elmo-imap4-spec-folder spec)))
      (setq response (elmo-imap4-read-response (process-buffer process)
					       process))
      (when (and response (string-match
			   "\\* STATUS [^(]* \\(([^)]*)\\)" response))
	(setq response (read (downcase (elmo-match-string 1 response))))
	(cons (- (cadr (memq 'uidnext response)) 1)
	      (cadr (memq 'messages response)))))))

(defun elmo-imap4-get-connection (spec)
  (let* ((user   (elmo-imap4-spec-username spec))
	 (server (elmo-imap4-spec-hostname spec))
	 (port   (elmo-imap4-spec-port spec))
	 (auth   (elmo-imap4-spec-auth spec))
	 (ssl    (elmo-imap4-spec-ssl spec))
	 (user-at-host (format "%s@%s" user server))
	 ret-val result buffer process proc-stat
	 user-at-host-on-port)
    (if (not (elmo-plugged-p server port))
	(error "Unplugged"))
    (setq user-at-host-on-port
	  (concat user-at-host ":" (int-to-string port)
		  (if (eq ssl 'starttls) "!!" (if ssl "!"))))
    (setq ret-val (assoc user-at-host-on-port
			 elmo-imap4-connection-cache))
    (if (and ret-val
	     (or (eq (setq proc-stat
			   (process-status (cadr (cdr ret-val))))
		     'closed)
		 (eq proc-stat 'exit)))
	;; connection is closed...
	(progn
	  (kill-buffer (car (cdr ret-val)))
	  (setq elmo-imap4-connection-cache
		(delete ret-val elmo-imap4-connection-cache))
	  (setq ret-val nil)))
    (if ret-val
	(progn
	  (setq ret-val (cdr ret-val)) ;; connection cache exists.
	  ret-val)
      (setq result
	    (elmo-imap4-open-connection server user auth port
					(elmo-get-passwd user-at-host)
					ssl))
      (if (null result)
	  (error "Connection failed"))
      (elmo-imap4-debug "Connected to %s" user-at-host-on-port)
      (setq buffer (car result))
      (setq process (cdr result))
      (when (and process (null buffer))
	(elmo-remove-passwd user-at-host)
	(delete-process process)
	(error "Login failed"))
      (setq elmo-imap4-connection-cache
	    (append elmo-imap4-connection-cache
		    (list
		     (cons user-at-host-on-port
			   (setq ret-val (list buffer process
					       ""; current-folder..
					       ))))))
      ret-val)))

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
	    (setq elmo-imap4-lock nil) ; unlock process buffer.
	    (elmo-imap4-debug "unlock(%d) %s" elmo-imap4-seqno output))
	(elmo-imap4-debug "continue(%d) %s" elmo-imap4-seqno output))
      (goto-char (point-max)))))

(defun elmo-imap4-read-response (buffer process &optional not-command)
  (save-excursion
    (set-buffer buffer)
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

(defun elmo-imap4-read-contents (buffer process)
  "Read OK response"
  (save-excursion
    (set-buffer buffer)
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
  (save-excursion
    (set-buffer buffer)
    (let ((case-fold-search nil)
	  (return-value nil)
	  start gc-message)
      (setq start elmo-imap4-read-point);; starting point
      (while (< (point-max) (+ start bytes))
	(accept-process-output process))
      (setq return-value (buffer-substring
			  start (+ start bytes)))
      (setq return-value (elmo-delete-cr return-value))
      (setq elmo-imap4-read-point bytes)
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
  
(defun elmo-imap4-noop (connection)
  (let ((buffer (car connection))
	(process (cadr connection)))
    (save-excursion
      (elmo-imap4-send-command buffer
			       process "noop")
      (elmo-imap4-read-response buffer process))))

(defun elmo-imap4-commit (spec)
  (save-excursion
    (let ((connection (elmo-imap4-get-connection spec))
	  response ret-val beg end)
      (and (not (null (elmo-imap4-spec-folder spec)))
	   (if (not (string= (elmo-imap4-connection-get-cwf connection)
			     (elmo-imap4-spec-folder spec)))
	       (if (null (setq response
			       (elmo-imap4-select-folder
				(elmo-imap4-spec-folder spec)
				connection)))
		   (error "Select folder failed"))
	     (if elmo-imap4-use-select-to-update-status
		 (elmo-imap4-select-folder
		  (elmo-imap4-spec-folder spec)
		  connection)
	       (elmo-imap4-check connection)))))))

(defun elmo-imap4-check (connection)
  (let ((process (elmo-imap4-connection-get-process connection)))
    (save-excursion
      (elmo-imap4-send-command (process-buffer process)
			       process "check")
      (elmo-imap4-read-response (process-buffer process) process))))

(defun elmo-imap4-select-folder (folder connection)
  (let ((process (elmo-imap4-connection-get-process connection))
	response)
    (save-excursion
      (unwind-protect
	  (progn
	    (elmo-imap4-send-command (process-buffer process)
				     process (format "select \"%s\""
						     folder))
	    (setq response (elmo-imap4-read-response
			    (process-buffer process) process)))
	(if (null response)
	    (progn
	      (setcar (cddr connection) nil)
	      (error "Select folder failed"))
	  (setcar (cddr connection) folder))))
    response))

(defun elmo-imap4-check-validity (spec validity-file)
  "get uidvalidity value from server and compare it with validity-file."
  (let* ((process (elmo-imap4-get-process spec))
	 response)
    (save-excursion
      (elmo-imap4-send-command (process-buffer process)
			       process
			       (format "status \"%s\" (uidvalidity)"
				       (elmo-imap4-spec-folder spec)))
      (setq response (elmo-imap4-read-response
		      (process-buffer process) process))
      (if (string-match "UIDVALIDITY \\([0-9]+\\)" response)
	  (string= (elmo-get-file-string validity-file)
		   (elmo-match-string 1 response))
	nil))))

(defun elmo-imap4-sync-validity  (spec validity-file)
  "get uidvalidity value from server and save it to validity-file."
  (let* ((process (elmo-imap4-get-process spec))
	 response)
    (save-excursion
      (elmo-imap4-send-command (process-buffer process)
			       process
			       (format "status \"%s\" (uidvalidity)"
				       (elmo-imap4-spec-folder spec)))
      (setq response (elmo-imap4-read-response
		      (process-buffer process) process))
      (if (string-match "UIDVALIDITY \\([0-9]+\\)" response)
	  (progn
	    (elmo-save-string
	     (elmo-match-string 1 response)
	     validity-file)
	    t)
	nil))))

(defsubst elmo-imap4-list (spec str)
  (save-excursion
    (let* ((connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection))
	   response ret-val beg end)
      (and (elmo-imap4-spec-folder spec)
	   (if (not (string= (elmo-imap4-connection-get-cwf connection)
			     (elmo-imap4-spec-folder spec)))
	       (if (null (setq response
			       (elmo-imap4-select-folder
				(elmo-imap4-spec-folder spec)
				connection)))
		   (error "Select folder failed"))
	     ;; for status update.
	     (if elmo-imap4-use-select-to-update-status
		 (elmo-imap4-select-folder (elmo-imap4-spec-folder spec)
					   connection)
	       (unless (elmo-imap4-check connection)
		 ;; Check failed...not selected??
		 (elmo-imap4-select-folder (elmo-imap4-spec-folder spec)
					   connection)))))
      (elmo-imap4-send-command (process-buffer process)
			       process
			       (format (if elmo-imap4-use-uid
					   "uid search %s"
					 "search %s") str))
      (setq response (elmo-imap4-read-response (process-buffer process)
					       process))
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
  (elmo-imap4-list spec "all"))

(defun elmo-imap4-list-folder-unread (spec mark-alist unread-marks)
  (if (elmo-imap4-use-flag-p spec)
      (elmo-imap4-list spec "unseen")
    (elmo-generic-list-folder-unread spec mark-alist unread-marks)))

(defun elmo-imap4-list-folder-important (spec overview)
  (and (elmo-imap4-use-flag-p spec)
       (elmo-imap4-list spec "flagged")))

(defun elmo-imap4-search-internal (process buffer filter)
  (let ((search-key (elmo-filter-key filter))
	word response)
    (cond
     ((or (string= "since" search-key)
	  (string= "before" search-key))
      (setq search-key (concat "sent" search-key))
      (elmo-imap4-send-command buffer process
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
      (elmo-imap4-send-command buffer process
			       (format
				(if elmo-imap4-use-uid
				    "uid search CHARSET %s%s %s {%d}"
				  " search CHARSET %s%s %s {%d}")
				(symbol-name elmo-search-mime-charset)
				(if (eq (elmo-filter-type filter) 'unmatch)
				    " not" "")
				(elmo-filter-key filter)
				(length word)))
      (if (null (elmo-imap4-read-response buffer process t))
	  (error "Searching failed because of server capability??"))
      (elmo-imap4-send-string buffer process word)))
    (if (null (setq response (elmo-imap4-read-response buffer process)))
	(error "Search failed for %s" (elmo-filter-key filter)))
    (if (string-match "^\\* SEARCH\\([^\n]*\\)$" response)
	(read (concat "(" (elmo-match-string 1 response) ")"))
      (error "SEARCH failed"))))

(defun elmo-imap4-search (spec condition &optional from-msgs)
  (save-excursion
    (let* ((connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection))
	   response ret-val len word)
      (if (and (elmo-imap4-spec-folder spec)
	       (not (string= (elmo-imap4-connection-get-cwf connection)
			     (elmo-imap4-spec-folder spec)))
	       (null (elmo-imap4-select-folder
		      (elmo-imap4-spec-folder spec) connection)))
	  (error "Select folder failed"))
      (while condition
	(setq response (elmo-imap4-search-internal process
						   (process-buffer process)
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
		     (elmo-imap4-spec-folder spec))))

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
      (if (re-search-forward "^\* \\([0-9]+\\) FETCH"
			     nil t)
	  (progn
	    (setq beg (point))
	    (unless elmo-imap4-use-uid
	      (setq number (string-to-int (elmo-match-buffer 1))))
	    (while (re-search-forward
		    "^\* \\([0-9]+\\) FETCH"
		    nil t)
	      (setq attr (elmo-imap4-make-attributes-object
			  (buffer-substring beg (match-beginning 0))))
	      (setq beg (point))
	      (unless elmo-imap4-use-uid
		(setq attr(nconc (list 'UID number) attr))
		(setq number (string-to-int (elmo-match-buffer 1))))
	      (setq ret-val (cons attr ret-val)))
	    ;; process last one...
	    (setq attr (elmo-imap4-make-attributes-object
			(buffer-substring beg (point-max))))
	    (unless elmo-imap4-use-uid
	      (setq attr(nconc (list 'UID number) attr)))
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
			     '(lambda (to)
				(elmo-imap4-make-address
				 (elmo-imap4-nth 0 to)
				 (elmo-imap4-nth 2 to)
				 (elmo-imap4-nth 3 to)))
			     (elmo-imap4-nth 5 value) ","))
	    (setq cc-string (mapconcat
			     '(lambda (cc)
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
    (let* ((connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection))
	   (msg-list (copy-sequence msgs))
	   set-list ent)
      (if (and (elmo-imap4-spec-folder spec)
	       (not (string= (elmo-imap4-connection-get-cwf connection)
			     (elmo-imap4-spec-folder spec)))
	       (null (elmo-imap4-select-folder
		      (elmo-imap4-spec-folder spec) connection)))
	  (error "Select folder failed"))
      (setq set-list (elmo-imap4-make-number-set-list msg-list))
      (when set-list
	(elmo-imap4-send-command (process-buffer process)
				 process
				 (format
				  (if elmo-imap4-use-uid
				      "uid store %s %sflags.silent (%s)"
				    "store %s %sflags.silent (%s)")
				  (cdr (car set-list))
				  (if unmark "-" "+")
				  mark))
	(unless (elmo-imap4-read-response (process-buffer process) process)
	  (error "Store %s flag failed" mark))
	(unless no-expunge
	  (elmo-imap4-send-command
	   (process-buffer process) process "expunge")
	  (unless (elmo-imap4-read-response (process-buffer process) process)
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
      (let* ((connection (elmo-imap4-get-connection spec))
	     (process    (elmo-imap4-connection-get-process connection))
	     (filter     (and as-num numlist))
	     (case-fold-search t)
	     (extra-fields (if elmo-msgdb-extra-fields
			       (concat " " (mapconcat
					    'identity
					    elmo-msgdb-extra-fields " "))
			     ""))
	     rfc2060 count ret-val set-list ov-str length)
	(setq rfc2060 (with-current-buffer (process-buffer process)
			(if (memq 'imap4rev1 elmo-imap4-server-capability)
			    t
			  (if (memq 'imap4 elmo-imap4-server-capability)
			      nil
			    (error "No IMAP4 capability!!")))))
	(setq count 0)
	(setq length (length numlist))
	(setq set-list (elmo-imap4-make-number-set-list
			numlist
			elmo-imap4-overview-fetch-chop-length))
	(message "Getting overview...")
	(if (and (elmo-imap4-spec-folder spec)
		 (not (string= (elmo-imap4-connection-get-cwf connection)
			       (elmo-imap4-spec-folder spec)))
		 (null (elmo-imap4-select-folder
			(elmo-imap4-spec-folder spec) connection)))
	    (error "Select imap folder %s failed"
		   (elmo-imap4-spec-folder spec)))
	(while set-list
	  (elmo-imap4-send-command
	   (process-buffer process)
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
			(elmo-imap4-spec-folder spec)
			new-mark already-mark seen-mark important-mark
			seen-list filter)))))
	  (setq count (+ count (car (car set-list))))
	  (setq ov-str (elmo-imap4-read-contents (process-buffer process)
						 process))
	  (elmo-display-progress
	   'elmo-imap4-msgdb-create "Getting overview..."
	   (/ (* count 100) length))
	  (setq set-list (cdr set-list)))
	;; process last one.
	(with-current-buffer (process-buffer process)
	  (if ov-str
	      (setq ret-val
		    (elmo-msgdb-append
		     ret-val
		     (elmo-imap4-create-msgdb-from-overview-string
		      ov-str
		      (elmo-imap4-spec-folder spec)
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
	   '(lambda (x y)
	      (> (length (car x))
		 (length (car y))))))))

(defun elmo-imap4-open-connection (imap4-server user auth port passphrase ssl)
  "Open Imap connection and returns
the list of (process session-buffer current-working-folder).
Return nil if connection failed."
  (let ((process nil)
	(host imap4-server)
	process-buffer ret-val response capability)
    (catch 'done
      (as-binary-process
       (setq process-buffer
	     (get-buffer-create (format " *IMAP session to %s:%d" host port)))
       (save-excursion
	 (set-buffer process-buffer)
	 (elmo-set-buffer-multibyte nil)
	 (make-variable-buffer-local 'elmo-imap4-server-capability)
	 (make-variable-buffer-local 'elmo-imap4-lock)
	 (erase-buffer))
       (setq process
	     (elmo-open-network-stream "IMAP" process-buffer host port ssl))
       (and (null process) (throw 'done nil))
       (set-process-filter process 'elmo-imap4-process-filter)
       ;; flush connections when exiting...
       (save-excursion
	 (set-buffer process-buffer)
	 (make-local-variable 'elmo-imap4-read-point)
	 (setq elmo-imap4-read-point (point-min))
	 (if (null (setq response
			 (elmo-imap4-read-response process-buffer process t)))
	     (throw 'done nil)
	   (when (string-match "^\\* PREAUTH" response)
	     (setq ret-val (cons process-buffer process))
	     (throw 'done nil)))
	 (elmo-imap4-send-command process-buffer process "capability")
	 (setq elmo-imap4-server-capability
	       (elmo-imap4-parse-capability
		(elmo-imap4-read-response process-buffer process)))
	 (setq capability elmo-imap4-server-capability)
	 (if (eq ssl 'starttls)
	     (if (and (memq 'starttls capability)
		      (progn
			(elmo-imap4-send-command process-buffer process "starttls")
			(setq response
			      (elmo-imap4-read-response process-buffer process)))
		      
		      (string-match
		       (concat "^\\(" elmo-imap4-seq-prefix
			       (int-to-string elmo-imap4-seqno)
			       "\\|\\*\\) OK")
		       response))
		 (starttls-negotiate process)
	       (error "STARTTLS aborted")))
	 (if (or (and (string= "auth" auth)
		      (not (memq 'auth=login capability)))
		 (and (string= "cram-md5" auth)
		      (not (memq 'auth=cram-md5 capability)))
		 (and (string= "digest-md5" auth)
		      (not (memq 'auth=digest-md5 capability))))
	     (if (or elmo-imap4-force-login
		     (y-or-n-p
		      (format
		       "There's no %s capability in server. continue?" auth)))
		 (setq auth "login")
	       (error "Login aborted")))
	 (cond
	  ((string= "auth" auth)
	   (elmo-imap4-send-command
	    process-buffer process "authenticate login" 'no-lock)
	   ;; Base64
	   (when (null (elmo-imap4-read-response process-buffer process t))
	     (setq ret-val (cons nil process))
	     (throw 'done nil))
	   (elmo-imap4-send-string
	    process-buffer process (elmo-base64-encode-string user))
	   (when (null (elmo-imap4-read-response process-buffer process t))
	     (setq ret-val (cons nil process))
	     (throw 'done nil))
	   (elmo-imap4-send-string
	    process-buffer process (elmo-base64-encode-string passphrase))
	   (when (null (elmo-imap4-read-response process-buffer process))
	     (setq ret-val (cons nil process))
	     (throw 'done nil))
	   (setq ret-val (cons process-buffer process)))
	  ((string= "cram-md5" auth)
	   (elmo-imap4-send-command
	    process-buffer process "authenticate cram-md5" 'no-lock)
	   (when (null (setq response
			     (elmo-imap4-read-response
			      process-buffer process t)))
	     (setq ret-val (cons nil process))
	     (throw 'done nil))
	   (setq response (cadr (split-string response " ")))
	   (elmo-imap4-send-string
	    process-buffer process
	    (elmo-base64-encode-string
	     (sasl-cram-md5 user passphrase
			    (elmo-base64-decode-string response))))
	   (when (null (elmo-imap4-read-response process-buffer process))
	     (setq ret-val (cons nil process))
	     (throw 'done nil))
	   (setq ret-val (cons process-buffer process)))
	  ((string= "digest-md5" auth)
	   (elmo-imap4-send-command
	    process-buffer process "authenticate digest-md5" 'no-lock)
	   (when (null (setq response
			     (elmo-imap4-read-response
			      process-buffer process t)))
	     (setq ret-val (cons nil process))
	     (throw 'done nil))
	   (setq response (cadr (split-string response " ")))
	   (elmo-imap4-send-string
	    process-buffer process
	    (elmo-base64-encode-string
	     (sasl-digest-md5-digest-response
	      (elmo-base64-decode-string response)
	      user passphrase "imap" host)
	     'no-line-break))
	   (when (null (elmo-imap4-read-response
			process-buffer process t))
	     (setq ret-val (cons nil process))
	     (throw 'done nil))
	   (elmo-imap4-send-string process-buffer process "")
	   (when (null (elmo-imap4-read-response process-buffer process))
	     (setq ret-val (cons nil process))
	     (throw 'done nil))
	   (setq ret-val (cons process-buffer process)))
	  (t ;; not auth... try login
	   (elmo-imap4-send-command
	    process-buffer process
	    (format "login %s \"%s\"" user
		    (elmo-replace-in-string passphrase
					    "\"" "\\\\\""))
	    nil 'no-log) ;; No LOGGING.
	   (if (null (elmo-imap4-read-response process-buffer process))
	       (setq ret-val (cons nil process))
	     (setq ret-val (cons process-buffer process)))))
	 ;; get namespace of server if possible.
	 (when (memq 'namespace elmo-imap4-server-capability)
	   (elmo-imap4-send-command process-buffer process "namespace")
	   (setq elmo-imap4-server-namespace
		 (elmo-imap4-parse-namespace
		  (elmo-imap4-parse-response
		   (elmo-imap4-read-response process-buffer process))))))))
    ret-val))
	    
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

(defun elmo-imap4-send-command (buffer process command &optional no-lock
				       no-log)
  "Send COMMAND string to server with sequence number."
  (save-excursion
    (set-buffer buffer)
    (when (and elmo-imap4-use-lock
	       elmo-imap4-lock)
      (elmo-imap4-debug "send: (%d) is still locking." elmo-imap4-seqno)
      (error "IMAP4 process is locked; Please try later (or plug again)"))
    (erase-buffer)
    (goto-char (point-min))
    (setq elmo-imap4-read-point (point))
    (unless no-lock
      ;; for debug.
      (if no-log
	  (elmo-imap4-debug "lock(%d): (No-logging command)." (+ elmo-imap4-seqno 1))
	(elmo-imap4-debug "lock(%d): %s" (+ elmo-imap4-seqno 1) command))
      (setq elmo-imap4-lock t))
    (process-send-string process (concat (format "%s%d "
						 elmo-imap4-seq-prefix
						 (elmo-imap4-get-seqno))
					 command))
    (process-send-string process "\r\n")))

(defun elmo-imap4-send-string (buffer process string)
  "Send STRING to server."
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (goto-char (point-min))
    (setq elmo-imap4-read-point (point))
    (process-send-string process string)
    (process-send-string process "\r\n")))

(defun elmo-imap4-read-part (folder msg part)
  (save-excursion
    (let* ((spec (elmo-folder-get-spec folder))
	   (connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection))
	   response ret-val bytes)
      (when (elmo-imap4-spec-folder spec)
	(when (not (string= (elmo-imap4-connection-get-cwf connection)
			    (elmo-imap4-spec-folder spec)))
	  (if (null (setq response
			  (elmo-imap4-select-folder
			   (elmo-imap4-spec-folder spec) connection)))
	      (error "Select folder failed")))
	(elmo-imap4-send-command (process-buffer process)
				 process
				 (format
				  (if elmo-imap4-use-uid
				      "uid fetch %s body.peek[%s]"
				    "fetch %s body.peek[%s]")
				  msg part))
	(if (null (setq response (elmo-imap4-read-response
				  (process-buffer process)
				  process t)))
	    (error "Fetch failed"))
	(save-match-data
	  (while (string-match "^\\* OK" response)
	    (if (null (setq response (elmo-imap4-read-response
				      (process-buffer process)
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
	(elmo-imap4-read-response (process-buffer process)
				  process)) ;; ignore remaining..
      ret-val)))

(defun elmo-imap4-prefetch-msg (spec msg outbuf)
  (elmo-imap4-read-msg spec msg outbuf 'unseen))

(defun elmo-imap4-read-msg (spec msg outbuf
				 &optional leave-seen-flag-untouched)
  (save-excursion
    (let* ((connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection))
	   response ret-val bytes)
      (as-binary-process
       (when (elmo-imap4-spec-folder spec)
	 (when (not (string= (elmo-imap4-connection-get-cwf connection)
			     (elmo-imap4-spec-folder spec)))
	   (if (null (setq response
			   (elmo-imap4-select-folder
			    (elmo-imap4-spec-folder spec)
			    connection)))
	       (error "Select folder failed")))
	 (elmo-imap4-send-command (process-buffer process)
				  process
				  (format
				   (if elmo-imap4-use-uid
				       "uid fetch %s body%s[]"
				     "fetch %s body%s[]")
				   msg
				   (if leave-seen-flag-untouched
				       ".peek" "")))
	 (if (null (setq response (elmo-imap4-read-response
				   (process-buffer process)
				   process t)))
	     (error "Fetch failed"))
	 (save-match-data
	   (while (string-match "^\\* OK" response)
	     (if (null (setq response (elmo-imap4-read-response
				       (process-buffer process)
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
	 (elmo-imap4-read-response (process-buffer process)
				   process)) ;; ignore remaining..
       )
      ret-val)))

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
    (let* ((connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection)))
      (elmo-imap4-send-command (process-buffer process)
			       process "expunge")
      (if (null (elmo-imap4-read-response (process-buffer process)
					  process))
	  (error "Expunge failed")))))

(defun elmo-imap4-delete-msg-by-id (spec msgid)
  (save-excursion
    (let* ((connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection))
	   ;;(size (length string))
	   response msgs)
      (if (and (elmo-imap4-spec-folder spec)
	       (not (string= (elmo-imap4-connection-get-cwf connection)
			     (elmo-imap4-spec-folder spec)))
	       (null (elmo-imap4-select-folder
		      (elmo-imap4-spec-folder spec)
		      connection)))
	  (error "Select folder failed"))
      (save-excursion
	(elmo-imap4-send-command (process-buffer process)
				 process
				 (format
				  (if elmo-imap4-use-uid
				      "uid search header message-id \"%s\""
				    "search header message-id \"%s\"")
				  msgid))
	(setq response (elmo-imap4-read-response
			(process-buffer process) process))
	(if (and response
		 (string-match "^\\* SEARCH\\([^\n]*\\)$" response))
	    (setq msgs (read (concat "(" (elmo-match-string 1 response) ")")))
	  (error "SEARCH failed"))
	(elmo-imap4-delete-msgs-no-expunge spec msgs)))))

(defun elmo-imap4-append-msg-by-id (spec msgid)
  (save-excursion
    (let* ((connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection))
	   send-buf)
      (if (and (elmo-imap4-spec-folder spec)
	       (not (string= (elmo-imap4-connection-get-cwf connection)
			     (elmo-imap4-spec-folder spec)))
	       (null (elmo-imap4-select-folder
		      (elmo-imap4-spec-folder spec) connection)))
	  (error "Select folder failed"))
      (save-excursion
	(setq send-buf (elmo-imap4-setup-send-buffer-from-file
			(elmo-cache-get-path msgid)))
	(set-buffer send-buf)
	(elmo-imap4-send-command (process-buffer process)
				 process
				 (format "append %s (\\Seen) {%d}"
					 (elmo-imap4-spec-folder spec)
					 (buffer-size)))
	(process-send-string process (buffer-string))
	(process-send-string process "\r\n") ; finished appending.
	)
      (kill-buffer send-buf)
      (if (null (elmo-imap4-read-response (process-buffer process)
					  process))
	  (error "Append failed")))
    t))

(defun elmo-imap4-append-msg (spec string &optional msg no-see)
  (save-excursion
    (let* ((connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection))
	   send-buf)
      (if (and (elmo-imap4-spec-folder spec)
	       (not (string= (elmo-imap4-connection-get-cwf connection)
			     (elmo-imap4-spec-folder spec)))
	       (null (elmo-imap4-select-folder (elmo-imap4-spec-folder spec)
					       connection)))
	  (error "Select folder failed"))
      (save-excursion
	(setq send-buf (elmo-imap4-setup-send-buffer string))
	(set-buffer send-buf)
	(elmo-imap4-send-command (process-buffer process)
				 process
				 (format "append %s %s{%d}"
					 (elmo-imap4-spec-folder spec)
					 (if no-see "" "(\\Seen) ")
					 (buffer-size)))
	(if (null (elmo-imap4-read-response (process-buffer process)
					    process))
	    (error "Cannot append messages to this folder"))
	(process-send-string process (buffer-string))
	(process-send-string process "\r\n") ; finished appending.
	)
      (kill-buffer send-buf)
      (current-buffer)
      (if (null (elmo-imap4-read-response (process-buffer process)
					  process))
	  (error "Append failed")))
    t))

(defun elmo-imap4-copy-msgs (dst-spec msgs src-spec &optional expunge-it same-number)
  "Equivalence of hostname, username is assumed."
  (save-excursion
    (let* ((src-folder (elmo-imap4-spec-folder src-spec))
	   (dst-folder (elmo-imap4-spec-folder dst-spec))
	   (connection (elmo-imap4-get-connection src-spec))
	   (process (elmo-imap4-connection-get-process connection))
	   (mlist msgs))
      (if (and src-folder
	       (not (string= (elmo-imap4-connection-get-cwf connection)
			     src-folder))
	       (null (elmo-imap4-select-folder
		      src-folder connection)))
	  (error "Select folder failed"))
      (while mlist
	(elmo-imap4-send-command (process-buffer process)
				 process
				 (format
				  (if elmo-imap4-use-uid
				      "uid copy %s %s"
				    "copy %s %s")
				  (car mlist) dst-folder))
	(if (null (elmo-imap4-read-response (process-buffer process)
					    process))
	    (error "Copy failed")
	  (setq mlist (cdr mlist))))
      (when expunge-it
	(elmo-imap4-send-command (process-buffer process)
				 process "expunge")
	(if (null (elmo-imap4-read-response (process-buffer process)
					    process))
	    (error "Expunge failed")))
      t)))

(defun elmo-imap4-server-diff (spec)
  "get server status"
  (save-excursion
    (let* ((connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection))
	   response)
      ;; commit when same folder.
      (if (string= (elmo-imap4-connection-get-cwf connection)
		   (elmo-imap4-spec-folder spec))
	  (elmo-imap4-commit spec))
      (elmo-imap4-send-command (process-buffer process)
			       process
			       (format
				"status \"%s\" (unseen messages)"
				(elmo-imap4-spec-folder spec)))
      (setq response (elmo-imap4-read-response
		      (process-buffer process) process))
      (when (string-match "\\* STATUS [^(]* \\(([^)]*)\\)" response)
	(setq response (read (downcase (elmo-match-string 1 response))))
	(cons (cadr (memq 'unseen response))
	      (cadr (memq 'messages response)))))))

(defun elmo-imap4-use-cache-p (spec number)
  elmo-imap4-use-cache)

(defun elmo-imap4-local-file-p (spec number)
  nil)

(defun elmo-imap4-port-label (spec)
  (concat "imap4"
	  (if (nth 6 spec) "!ssl" "")))

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
