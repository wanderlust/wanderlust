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
;; Origin of IMAP parser part is imap.el, included in Gnus.
;;
;;    Copyright (C) 1998, 1999, 2000
;;    Free Software Foundation, Inc.
;;    Author: Simon Josefsson <jas@pdc.kth.se>
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
;;; internal variables
;;
(defvar elmo-imap4-seq-prefix "elmo-imap4")
(defvar elmo-imap4-seqno 0)
(defvar elmo-imap4-use-uid t
  "Use UID as message number.")

(defvar elmo-imap4-current-response nil)
(defvar elmo-imap4-status nil)
(defvar elmo-imap4-reached-tag "elmo-imap40")

;;; buffer local variables

(defvar elmo-imap4-extra-namespace-alist
  '(("^{.*/nntp}.*$" . ".")) ; Default is for UW's remote nntp mailbox...
  "Extra namespace alist. A list of cons cell like: (REGEXP . DELIMITER) ")
(defvar elmo-imap4-default-hierarchy-delimiter "/")

(defvar elmo-imap4-server-capability nil)
(defvar elmo-imap4-server-namespace nil)

(defvar elmo-imap4-parsing nil) ; indicates parsing.

(defvar elmo-imap4-fetch-callback nil)
(defvar elmo-imap4-fetch-callback-data nil)

;;; progress...(no use?)
(defvar elmo-imap4-count-progress nil)
(defvar elmo-imap4-count-progress-message nil)
(defvar elmo-imap4-progress-count nil)

;;; XXX Temporal implementation
(defvar elmo-imap4-current-msgdb nil)

(defvar elmo-imap4-local-variables '(elmo-imap4-status
				     elmo-imap4-current-response
				     elmo-imap4-seqno
				     elmo-imap4-parsing
				     elmo-imap4-reached-tag
				     elmo-imap4-count-progress
				     elmo-imap4-count-progress-message
				     elmo-imap4-progress-count
				     elmo-imap4-fetch-callback
				     elmo-imap4-fetch-callback-data
				     elmo-imap4-current-msgdb))

(defvar elmo-imap4-authenticator-alist
  '((login	elmo-imap4-auth-login)
    (cram-md5	elmo-imap4-auth-cram-md5)
    (digest-md5 elmo-imap4-auth-digest-md5)
    (plain      elmo-imap4-login))
  "Definition of authenticators.")

;;;;

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

;; For debugging.
(defvar elmo-imap4-debug nil
  "Non-nil forces IMAP4 folder as debug mode.
Debug information is inserted in the buffer \"*IMAP4 DEBUG*\"")

(defvar elmo-imap4-debug-inhibit-logging nil)

;;; 

(eval-and-compile
  (luna-define-class elmo-imap4-session (elmo-network-session)
		     (capability current-mailbox read-only))
  (luna-define-internal-accessors 'elmo-imap4-session))

;;; imap4 spec

(defsubst elmo-imap4-spec-mailbox (spec)
  (nth 1 spec))

(defsubst elmo-imap4-spec-username (spec)
  (nth 2 spec))

(defsubst elmo-imap4-spec-auth (spec)
  (nth 3 spec))

(defsubst elmo-imap4-spec-hostname (spec)
  (nth 4 spec))

(defsubst elmo-imap4-spec-port (spec)
  (nth 5 spec))

(defsubst elmo-imap4-spec-stream-type (spec)
  (nth 6 spec))


;;; Debug

(defsubst elmo-imap4-debug (message &rest args)
  (if elmo-imap4-debug
      (with-current-buffer (get-buffer-create "*IMAP4 DEBUG*")
	(goto-char (point-max))
	(if elmo-imap4-debug-inhibit-logging
	    (insert "NO LOGGING\n")
	  (insert (apply 'format message args) "\n")))))

;;; Response

(defmacro elmo-imap4-response-continue-req-p (response)
  "Returns non-nil if RESPONSE is '+' response."
  (` (assq 'continue-req (, response))))

(defmacro elmo-imap4-response-ok-p (response)
  "Returns non-nil if RESPONSE is an 'OK' response."
  (` (assq 'ok (, response))))

(defmacro elmo-imap4-response-bye-p (response)
  "Returns non-nil if RESPONSE is an 'BYE' response."
  (` (assq 'bye (, response))))

(defmacro elmo-imap4-response-value (response symbol)
  "Get value of the SYMBOL from RESPONSE."
  (` (nth 1 (assq (, symbol) (, response)))))

(defsubst elmo-imap4-response-value-all (response symbol)
  "Get all value of the SYMBOL from RESPONSE."
  (let (matched)
    (while response
      (if (eq (car (car response)) symbol)
	  (setq matched (nconc matched (nth 1 (car response)))))
      (setq response (cdr response)))
    matched))

(defmacro elmo-imap4-response-error-text (response)
  "Returns text of NO, BAD, BYE response."
  (` (nth 1 (or (elmo-imap4-response-value (, response) 'no)
		(elmo-imap4-response-value (, response) 'bad)
		(elmo-imap4-response-value (, response) 'bye)))))

(defmacro elmo-imap4-response-bodydetail-text (response)
  "Returns text of BODY[section]<partial>"
  (` (nth 3 (assq 'bodydetail (, response)))))

;;; Session commands.

; (defun elmo-imap4-send-command-wait (session command)
;   "Send COMMAND to the SESSION and wait for response.
; Returns RESPONSE (parsed lisp object) of IMAP session."
;   (elmo-imap4-read-response session
; 			    (elmo-imap4-send-command
; 			     session
; 			     command)))

(defun elmo-imap4-send-command-wait (session command)
  "Send COMMAND to the SESSION.
Returns RESPONSE (parsed lisp object) of IMAP session.
If response is not `OK', causes error with IMAP response text."
  (elmo-imap4-accept-ok session
			(elmo-imap4-send-command
			 session
			 command)))

(defun elmo-imap4-send-command (session command)
  "Send COMMAND to the SESSION.
Returns a TAG string which is assigned to the COMAND."
  (let* ((command-args (if (listp command)
			   command
			 (list command)))
	 (process (elmo-network-session-process-internal session))
	 cmdstr tag token kind)
    (with-current-buffer (process-buffer process)
      (setq tag (concat elmo-imap4-seq-prefix
			(number-to-string
			 (setq elmo-imap4-seqno (+ 1 elmo-imap4-seqno)))))
      (setq cmdstr (concat tag " "))
      ;; (erase-buffer) No need.
      (goto-char (point-min))
      (if (elmo-imap4-response-bye-p elmo-imap4-current-response)
	  (signal 'elmo-imap4-bye-error
		  (list (elmo-imap4-response-error-text
			 elmo-imap4-current-response))))
      (setq elmo-imap4-current-response nil)
      (if elmo-imap4-parsing
	  (error "IMAP process is running. Please wait (or plug again.)"))
      (setq elmo-imap4-parsing t)
      (elmo-imap4-debug "<-(%s)- %s" tag command)
      (while (setq token (car command-args))
	(cond ((stringp token)   ; formatted
	       (setq cmdstr (concat cmdstr token)))
	      ((listp token)     ; unformatted
	       (setq kind (car token))
	       (cond ((eq kind 'atom)
		      (setq cmdstr (concat cmdstr (nth 1 token))))
		     ((eq kind 'quoted)
		      (setq cmdstr (concat
				    cmdstr
				    (elmo-imap4-format-quoted (nth 1 token)))))
		     ((eq kind 'literal)
		      (setq cmdstr (concat cmdstr
					   (format "{%d}" (nth 2 token))))
		      (process-send-string process cmdstr)
		      (process-send-string process "\r\n")
		      (setq cmdstr nil)
		      (elmo-imap4-accept-continue-req session)
		      (cond ((stringp (nth 1 token))
			     (setq cmdstr (nth 1 token)))
			    ((bufferp (nth 1 token))
			     (with-current-buffer (nth 1 token)
			       (process-send-region
				process
				(point-min)
				(+ (point-min) (nth 2 token)))))
			    (t
			     (error "Wrong argument for literal"))))
		     (t
		      (error "Unknown token kind %s" kind))))
	      (t
	       (error "Invalid argument")))
	(setq command-args (cdr command-args)))
      (if cmdstr
	  (process-send-string process cmdstr))
      (process-send-string process "\r\n")
      tag)))

(defun elmo-imap4-send-string (session string)
  "Send STRING to the SESSION."
  (with-current-buffer (process-buffer
			(elmo-network-session-process-internal session))
    (setq elmo-imap4-current-response nil)
    (goto-char (point-min))
    (elmo-imap4-debug "<-- %s" string)
    (process-send-string (elmo-network-session-process-internal session)
			 string)
    (process-send-string (elmo-network-session-process-internal session)
			 "\r\n")))

(defun elmo-imap4-read-response (session tag)
  "Read parsed response from SESSION.
TAG is the tag of the command"
  (with-current-buffer (process-buffer
			(elmo-network-session-process-internal session))
    (while (not (or (string= tag elmo-imap4-reached-tag)
		    (elmo-imap4-response-bye-p elmo-imap4-current-response)))
      (when (memq (process-status
		   (elmo-network-session-process-internal session))
		  '(open run))
	(accept-process-output (elmo-network-session-process-internal session)
			       1)))
    (elmo-imap4-debug "=>%s" (prin1-to-string elmo-imap4-current-response))
    (setq elmo-imap4-parsing nil)
    elmo-imap4-current-response))

(defsubst elmo-imap4-read-untagged (process)
  (with-current-buffer (process-buffer process)
    (while (not elmo-imap4-current-response)
      (accept-process-output process 1))
    (elmo-imap4-debug "=>%s" (prin1-to-string elmo-imap4-current-response))
    elmo-imap4-current-response))

(defun elmo-imap4-read-continue-req (session)
  "Returns a text following to continue-req in SESSION.
If response is not `+' response, returns nil."
  (elmo-imap4-response-value
   (elmo-imap4-read-untagged
    (elmo-network-session-process-internal session))
   'continue-req))

(defun elmo-imap4-accept-continue-req (session)
  "Returns non-nil if `+' (continue-req) response is arrived in SESSION.
If response is not `+' response, cause an error."
  (let (response)
    (setq response
	  (elmo-imap4-read-untagged
	   (elmo-network-session-process-internal session)))
    (or (elmo-imap4-response-continue-req-p response)
	(error "IMAP error: %s"
	       (or (elmo-imap4-response-error-text response)
		   "No continut-req from server.")))))

(defun elmo-imap4-read-ok (session tag)
  "Returns non-nil if `OK' response of the command with TAG is arrived
in SESSION. If response is not `OK' response, returns nil."
  (elmo-imap4-response-ok-p
   (elmo-imap4-read-response session tag)))

(defun elmo-imap4-accept-ok (session tag)
  "Accept only `OK' response from SESSION.
If response is not `OK' response, causes error with IMAP response text."
  (let ((response (elmo-imap4-read-response session tag)))
    (if (elmo-imap4-response-ok-p response)
	response
      (if (elmo-imap4-response-bye-p response)
	  (signal 'elmo-imap4-bye-error
		  (list (elmo-imap4-response-error-text response)))
	(error "IMAP error: %s"
	       (or (elmo-imap4-response-error-text response)
		   "No `OK' response from server."))))))
;;;

(defun elmo-imap4-session-check (session)
  (elmo-imap4-send-command-wait session "check"))

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

(defsubst elmo-imap4-response-get-selectable-mailbox-list (response)
  (delq nil
	(mapcar
	 (lambda (entry)
	   (if (and (eq 'list (car entry))
		    (not (member "\\NoSelect" (nth 1 (nth 1 entry)))))
	       (car (nth 1 entry))))
	 response)))

;;; Backend methods.
(defun elmo-imap4-list-folders (spec &optional hierarchy)
  (let* ((root (elmo-imap4-spec-mailbox spec))
	 (session (elmo-imap4-get-session spec))
	 (delim (or
		 (cdr
		  (elmo-string-matched-assoc
		   root
		   (with-current-buffer (elmo-network-session-buffer session)
		     elmo-imap4-server-namespace)))
		 elmo-imap4-default-hierarchy-delimiter))
	 result append-serv type)
    ;; Append delimiter
    (if (and root
	     (not (string= root ""))
	     (not (string-match (concat "\\(.*\\)"
					(regexp-quote delim)
					"\\'")
				root)))
	(setq root (concat root delim)))
    (setq result (elmo-imap4-response-get-selectable-mailbox-list
		  (elmo-imap4-send-command-wait
		   session
		   (list "list " (elmo-imap4-mailbox root) " *"))))
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
	    result)))

(defun elmo-imap4-folder-exists-p (spec)
  (let ((session (elmo-imap4-get-session spec))
	response)
    (setq response
	  (elmo-imap4-read-response
	   session
	   (elmo-imap4-send-command
	    session
	    (list "status " (elmo-imap4-mailbox (elmo-imap4-spec-mailbox spec))
		  " (messages)"))))
    (when (elmo-imap4-response-bye-p response)
      (signal 'elmo-imap4-bye-error
	      (list (elmo-imap4-response-error-text response))))
    (elmo-imap4-response-ok-p response)))

(defun elmo-imap4-folder-creatable-p (spec)
  t)

(defun elmo-imap4-create-folder-maybe (spec dummy)
  (unless (elmo-imap4-folder-exists-p spec)
    (elmo-imap4-create-folder spec)))

(defun elmo-imap4-create-folder (spec)
  (elmo-imap4-send-command-wait
   (elmo-imap4-get-session spec)
   (list "create " (elmo-imap4-mailbox
		    (elmo-imap4-spec-mailbox spec)))))

(defun elmo-imap4-delete-folder (spec)
  (let ((session (elmo-imap4-get-session spec))
	msgs)
    (when (elmo-imap4-spec-mailbox spec)
      (when (setq msgs (elmo-imap4-list-folder spec))
	(elmo-imap4-delete-msgs spec msgs))
      ;; (elmo-imap4-send-command-wait session "close")
      (elmo-imap4-send-command-wait
       session
       (list "delete "
	     (elmo-imap4-mailbox (elmo-imap4-spec-mailbox spec)))))))

(defun elmo-imap4-rename-folder (old-spec new-spec)
  ;;(elmo-imap4-send-command-wait session "close")
  (elmo-imap4-send-command-wait
   (elmo-imap4-get-session old-spec)
   (list "rename "
	 (elmo-imap4-mailbox
	  (elmo-imap4-spec-mailbox old-spec))
	 " "
	 (elmo-imap4-mailbox
	  (elmo-imap4-spec-mailbox new-spec)))))

(defun elmo-imap4-max-of-folder (spec)
  (let ((status (elmo-imap4-response-value
		 (elmo-imap4-send-command-wait
		  (elmo-imap4-get-session spec)
		  (list "status "
			(elmo-imap4-mailbox
			 (elmo-imap4-spec-mailbox spec))
			" (uidnext messages)"))
		 'status)))
    (cons
     (- (elmo-imap4-response-value status 'uidnext) 1)
     (elmo-imap4-response-value status 'messages))))

;      (when (and response (string-match
;			   "\\* STATUS [^(]* \\(([^)]*)\\)" response))
;	(setq response (read (downcase (elmo-match-string 1 response))))
;	(cons (- (cadr (memq 'uidnext response)) 1)
;	      (cadr (memq 'messages response)))))))

(defun elmo-imap4-get-session (spec &optional if-exists)
  (elmo-network-get-session
   'elmo-imap4-session
   "IMAP"
   (elmo-imap4-spec-hostname spec)
   (elmo-imap4-spec-port spec)
   (elmo-imap4-spec-username spec)
   (elmo-imap4-spec-auth spec)
   (elmo-imap4-spec-stream-type spec)
   if-exists))

(defun elmo-imap4-commit (spec)
  (if (elmo-imap4-plugged-p spec)
      (let ((session (elmo-imap4-get-session spec 'if-exists)))
	(when session
	  (if (string=
	       (elmo-imap4-session-current-mailbox-internal session)
	       (elmo-imap4-spec-mailbox spec))
	      (if elmo-imap4-use-select-to-update-status
		  (elmo-imap4-session-select-mailbox
		   session
		   (elmo-imap4-spec-mailbox spec)
		   'force)	      
		(elmo-imap4-session-check session)))))))
  
(defun elmo-imap4-session-select-mailbox (session mailbox &optional force)
  (when (or force
	    (not (string=
		  (elmo-imap4-session-current-mailbox-internal session)
		  mailbox)))
    (let (response)
      (unwind-protect
	  (setq response
		(elmo-imap4-read-response
		 session
		 (elmo-imap4-send-command
		  session
		  (list
		   "select "
		   (elmo-imap4-mailbox mailbox)))))
	(if (elmo-imap4-response-ok-p response)
	    (progn
	      (elmo-imap4-session-set-current-mailbox-internal session mailbox)
	      (elmo-imap4-session-set-read-only-internal
	       session
	       (nth 1 (assq 'read-only (assq 'ok response)))))
	  (elmo-imap4-session-set-current-mailbox-internal session nil)
	  (error (or
		  (elmo-imap4-response-error-text response)
		  (format "Select %s failed" mailbox))))))))

(defun elmo-imap4-check-validity (spec validity-file)
  ;; Not used.
;  (elmo-imap4-send-command-wait
;   (elmo-imap4-get-session spec)
;   (list "status "
;	 (elmo-imap4-mailbox
;	  (elmo-imap4-spec-mailbox spec))
;	 " (uidvalidity)")))
  )

(defun elmo-imap4-sync-validity  (spec validity-file)
  ;; Not used.
  )

(defun elmo-imap4-list (spec flag)
  (let ((session (elmo-imap4-get-session spec)))
    (elmo-imap4-session-select-mailbox session
				       (elmo-imap4-spec-mailbox spec))
    (elmo-imap4-response-value
     (elmo-imap4-send-command-wait
      session
      (format (if elmo-imap4-use-uid "uid search %s"
		"search %s") flag))
     'search)))

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

(defmacro elmo-imap4-detect-search-charset (string)
  (` (with-temp-buffer
       (insert (, string))
       (detect-mime-charset-region (point-min) (point-max)))))

(defun elmo-imap4-search-internal (session filter)
  (let ((search-key (elmo-filter-key filter))
	charset)
    (cond
     ((or (string= "since" search-key)
	  (string= "before" search-key))
      (setq search-key (concat "sent" search-key))
      (elmo-imap4-response-value
       (elmo-imap4-send-command-wait session
				     (format
				      (if elmo-imap4-use-uid
					  "uid search %s %s"
					" search %s %s")
				      search-key
				      (elmo-date-get-description
				       (elmo-date-get-datevec
					(elmo-filter-value filter)))))
       'search))
     (t
      (setq charset (elmo-imap4-detect-search-charset
		     (elmo-filter-value filter)))
      (elmo-imap4-response-value
       (elmo-imap4-send-command-wait session
				     (list
				      (if elmo-imap4-use-uid
					  "uid search CHARSET "
					"search CHARSET ")
				      (elmo-imap4-astring
				       (symbol-name charset))
				      (if (eq (elmo-filter-type filter)
					      'unmatch)
					  " not " " ")
				      (format "%s "
					      (elmo-filter-key filter))
				      (elmo-imap4-astring
				       (encode-mime-charset-string
					(elmo-filter-value filter) charset))))
       'search)))))

(defun elmo-imap4-search (spec condition &optional from-msgs)
  (save-excursion
    (let* ((session (elmo-imap4-get-session spec))
	   response matched)
      (elmo-imap4-session-select-mailbox
       session
       (elmo-imap4-spec-mailbox spec))
      (while condition
	(setq response (elmo-imap4-search-internal session
						   (car condition)))
	(setq matched (nconc matched response))
	(setq condition (cdr condition)))
      (if from-msgs
	  (elmo-list-filter
	   from-msgs
	   (elmo-uniq-list (sort matched '<)))
	(elmo-uniq-list (sort matched '<))))))

(defun elmo-imap4-use-flag-p (spec)
  (not (string-match elmo-imap4-disuse-server-flag-mailbox-regexp
		     (elmo-imap4-spec-mailbox spec))))

(static-cond
 ((fboundp 'float)
  ;; Emacs can parse dot symbol.
  (defvar elmo-imap4-rfc822-size "RFC822\.SIZE")
  (defvar elmo-imap4-rfc822-text "RFC822\.TEXT")
  (defvar elmo-imap4-rfc822-header "RFC822\.HEADER")
  (defvar elmo-imap4-rfc822-size "RFC822\.SIZE")
  (defvar elmo-imap4-header-fields "HEADER\.FIELDS")
  (defmacro elmo-imap4-replace-dot-symbols ()) ;; noop
  (defalias 'elmo-imap4-fetch-read 'read)
  (defalias 'elmo-imap4-read 'read)
  )
 (t
  ;;; For Nemacs.
  ;; Cannot parse dot symbol.
  (defvar elmo-imap4-rfc822-size "RFC822_SIZE")
  (defvar elmo-imap4-header-fields "HEADER_FIELDS")
  (defvar elmo-imap4-rfc822-size "RFC822_SIZE")
  (defvar elmo-imap4-rfc822-text "RFC822_TEXT")
  (defvar elmo-imap4-rfc822-header "RFC822_HEADER")
  (defvar elmo-imap4-header-fields "HEADER_FIELDS")
  (defun elmo-imap4-fetch-read (buffer)
    (with-current-buffer buffer
      (let ((beg (point))
	    token)
	(when (re-search-forward "[[ ]" nil t)
	  (goto-char (match-beginning 0))
	  (setq token (buffer-substring beg (point)))
	  (cond ((string= token "RFC822.SIZE")
		 (intern elmo-imap4-rfc822-size))
		((string= token "RFC822.HEADER")
		 (intern elmo-imap4-rfc822-header))
		((string= token "RFC822.TEXT")
		 (intern elmo-imap4-rfc822-text))
		((string= token "HEADER\.FIELDS")
		 (intern elmo-imap4-header-fields))
		(t (goto-char beg)
		   (elmo-imap4-read (current-buffer))))))))
  ;; Nemacs's `read' is different.
  (defun elmo-imap4-read (obj)
    (prog1 (read obj)
      (if (bufferp obj)
	  (or (bobp) (forward-char -1)))))))

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
  (let ((session (elmo-imap4-get-session spec))
	set-list)
    (elmo-imap4-session-select-mailbox session
				       (elmo-imap4-spec-mailbox spec))
    (setq set-list (elmo-imap4-make-number-set-list msgs))
    (when set-list
      (with-current-buffer (elmo-network-session-buffer session)
	(setq elmo-imap4-fetch-callback nil)
	(setq elmo-imap4-fetch-callback-data nil))
      (elmo-imap4-send-command-wait
       session
       (format
	(if elmo-imap4-use-uid
	    "uid store %s %sflags.silent (%s)"
	  "store %s %sflags.silent (%s)")
	(cdr (car set-list))
	(if unmark "-" "+")
	mark))
      (unless no-expunge
	(elmo-imap4-send-command-wait session "expunge")))
    t))

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

;; Current buffer is process buffer.
(defun elmo-imap4-fetch-callback (element app-data)
  (funcall elmo-imap4-fetch-callback
	   (with-temp-buffer
	     (insert (or (elmo-imap4-response-bodydetail-text element)
			 ""))
	     ;; Delete CR.
	     (goto-char (point-min))
	     (while (search-forward "\r\n" nil t)
	       (replace-match "\n"))
	     (elmo-msgdb-create-overview-from-buffer
	      (elmo-imap4-response-value element 'uid)
	      (elmo-imap4-response-value element 'rfc822size)))
	   (elmo-imap4-response-value element 'flags)
	   app-data))

;;
;; app-data:
;; 0: new-mark 1: already-mark 2: seen-mark 3: important-mark
;; 4: seen-list 5: as-number
(defun elmo-imap4-fetch-callback-1 (entity flags app-data)
  "A msgdb entity callback function."
  (let ((seen (member (car entity) (nth 4 app-data)))
	mark)
    (if (member "\\Flagged" flags)
	(elmo-msgdb-global-mark-set (car entity) (nth 3 app-data)))
    (setq mark (or (elmo-msgdb-global-mark-get (car entity))
		   (if (elmo-cache-exists-p (car entity)) ;; XXX
		       (if (or (member "\\Seen" flags) seen)
			   nil
			 (nth 1 app-data))
		     (if (or (member "\\Seen" flags) seen)
			 (if elmo-imap4-use-cache
			     (nth 2 app-data))
		       (nth 0 app-data)))))
    (setq elmo-imap4-current-msgdb
	  (elmo-msgdb-append
	   elmo-imap4-current-msgdb
	   (list (list entity)
		 (list (cons (elmo-msgdb-overview-entity-get-number entity)
			     (car entity)))
		 (if mark
		     (list
		      (list (elmo-msgdb-overview-entity-get-number entity)
			    mark))))))))

(defun elmo-imap4-msgdb-create (spec numlist &rest args)
  "Create msgdb for SPEC."
  (when numlist
    (let ((session (elmo-imap4-get-session spec))
	  (headers
	   (append
	    '("Subject" "From" "To" "Cc" "Date"
	      "Message-Id" "References" "In-Reply-To")
	    elmo-msgdb-extra-fields))
	  (total 0)
	  (length (length numlist))
	  rfc2060 set-list)
      (setq rfc2060 (memq 'imap4rev1
			  (elmo-imap4-session-capability-internal
			   session)))
      (message "Getting overview...")
      (elmo-imap4-session-select-mailbox session
					 (elmo-imap4-spec-mailbox spec))
      (setq set-list (elmo-imap4-make-number-set-list
		      numlist
		      elmo-imap4-overview-fetch-chop-length))
      ;; Setup callback.
      (with-current-buffer (elmo-network-session-buffer session)
	(setq elmo-imap4-current-msgdb nil
	      elmo-imap4-fetch-callback 'elmo-imap4-fetch-callback-1
	      elmo-imap4-fetch-callback-data args)
	(while set-list
	  (elmo-imap4-send-command-wait
	   session
	   ;; get overview entity from IMAP4
	   (format "%sfetch %s (%s rfc822.size flags)"
		   (if elmo-imap4-use-uid "uid " "")
		   (cdr (car set-list))
		   (if rfc2060
		       (format "body.peek[header.fields %s]" headers)
		     (format "%s" headers))))
	  (when (> length elmo-display-progress-threshold)
	    (setq total (+ total (car (car set-list))))
	    (elmo-display-progress
	     'elmo-imap4-msgdb-create "Getting overview..."
	     (/ (* total 100) length)))
	  (setq set-list (cdr set-list)))
	(message "Getting overview...done.")
	elmo-imap4-current-msgdb))))

(defun elmo-imap4-parse-capability (string)
  (if (string-match "^\\*\\(.*\\)$" string)
      (elmo-imap4-read
       (concat "(" (downcase (elmo-match-string 1 string)) ")"))))

;; Current buffer is process buffer.
(defun elmo-imap4-auth-login (session)
  (let ((tag (elmo-imap4-send-command session "authenticate login"))
	(elmo-imap4-debug-inhibit-logging t))
    (or (elmo-imap4-read-continue-req session)
	(signal 'elmo-authenticate-error '(elmo-imap4-auth-login)))
    (elmo-imap4-send-string session
			    (elmo-base64-encode-string
			     (elmo-network-session-user-internal session)))
    (or (elmo-imap4-read-continue-req session)
	(signal 'elmo-authenticate-error '(elmo-imap4-auth-login)))
    (elmo-imap4-send-string session
			    (elmo-base64-encode-string
			     (elmo-get-passwd
			      (elmo-network-session-password-key session))))
    (or (elmo-imap4-read-ok session tag)
	(signal 'elmo-authenticate-error '(elmo-imap4-auth-login)))
    (setq elmo-imap4-status 'auth)))

(defun elmo-imap4-auth-cram-md5 (session)
  (let ((tag (elmo-imap4-send-command session "authenticate cram-md5"))
	(elmo-imap4-debug-inhibit-logging t)
	response)
    (or (setq response (elmo-imap4-read-continue-req session))
	(signal 'elmo-authenticate-error
		'(elmo-imap4-auth-cram-md5)))
    (elmo-imap4-send-string
     session
     (elmo-base64-encode-string
      (sasl-cram-md5 (elmo-network-session-user-internal session)
		     (elmo-get-passwd
		      (elmo-network-session-password-key session))
		     (elmo-base64-decode-string response))))
    (or (elmo-imap4-read-ok session tag)
	(signal 'elmo-authenticate-error '(elmo-imap4-auth-cram-md5)))))

(defun elmo-imap4-auth-digest-md5 (session)
  (let ((tag (elmo-imap4-send-command session "authenticate digest-md5"))
	(elmo-imap4-debug-inhibit-logging t)
	response)
    (or (setq response (elmo-imap4-read-continue-req session))
	(signal 'elmo-authenticate-error '(elmo-imap4-auth-digest-md5)))
    (elmo-imap4-send-string
     session
     (elmo-base64-encode-string
      (sasl-digest-md5-digest-response
       (elmo-base64-decode-string response)
       (elmo-network-session-user-internal session)
       (elmo-get-passwd (elmo-network-session-password-key session))
       "imap"
       (elmo-network-session-password-key session))
      'no-line-break))
    (or (setq response (elmo-imap4-read-continue-req session))
	(signal 'elmo-authenticate-error '(elmo-imap4-auth-digest-md5)))
    (elmo-imap4-send-string session "")
    (or (elmo-imap4-read-ok session tag)
	(signal 'elmo-authenticate-error '(elmo-imap4-auth-digest-md5)))))

(defun elmo-imap4-login (session)
  (let ((elmo-imap4-debug-inhibit-logging t))
    (or
     (elmo-imap4-read-ok
      session
      (elmo-imap4-send-command
       session
       (list "login "
	     (elmo-imap4-userid (elmo-network-session-user-internal session))
	     " "
	     (elmo-imap4-password
	      (elmo-get-passwd (elmo-network-session-password-key session))))))
     (signal 'elmo-authenticate-error '(login)))))
  
(luna-define-method
  elmo-network-initialize-session-buffer :after ((session
						  elmo-imap4-session) buffer)
  (with-current-buffer buffer
    (mapcar 'make-variable-buffer-local elmo-imap4-local-variables)
    (setq elmo-imap4-seqno 0)
    (setq elmo-imap4-status 'initial)))

(luna-define-method elmo-network-initialize-session ((session
						      elmo-imap4-session))
  (let ((process (elmo-network-session-process-internal session))
	capability)
    (with-current-buffer (process-buffer process)
      (set-process-filter process 'elmo-imap4-arrival-filter)
      (set-process-sentinel process 'elmo-imap4-sentinel)
      (while (and (memq (process-status process) '(open run))
		  (eq elmo-imap4-status 'initial))
	;;(message "Waiting for server response...")
	(accept-process-output process 1))
      ;(message "")
      (unless (memq elmo-imap4-status '(nonauth auth))
	(signal 'elmo-open-error
		(list 'elmo-network-initialize-session)))
      (elmo-imap4-session-set-capability-internal
       session
       (elmo-imap4-response-value
	(elmo-imap4-send-command-wait session "capability")
	'capability))
      (when (eq (elmo-network-stream-type-symbol
		 (elmo-network-session-stream-type-internal session))
		'starttls)
	(or (memq 'starttls capability)
	    (signal 'elmo-open-error
		    '(elmo-network-initialize-session)))
	(elmo-imap4-send-command-wait session "starttls")
	(starttls-negotiate process)))))

(luna-define-method elmo-network-authenticate-session ((session
							elmo-imap4-session))
 (with-current-buffer (process-buffer
		       (elmo-network-session-process-internal session))
   (unless (eq elmo-imap4-status 'auth)
     (unless (or (not (elmo-network-session-auth-internal session))
		 (eq (elmo-network-session-auth-internal session) 'plain)
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
       (funcall authenticator session)))))

(luna-define-method elmo-network-setup-session ((session
						 elmo-imap4-session))
  (with-current-buffer (elmo-network-session-buffer session)
    (when (memq 'namespace (elmo-imap4-session-capability-internal session))
      (setq elmo-imap4-server-namespace
	    (elmo-imap4-response-value
	     (elmo-imap4-send-command-wait session "namespace")
	     'namespace)))))

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

(defun elmo-imap4-read-part (folder msg part)
  (let* ((spec (elmo-folder-get-spec folder))
	 (session (elmo-imap4-get-session spec)))
    (elmo-imap4-session-select-mailbox session
				       (elmo-imap4-spec-mailbox spec))
    (elmo-delete-cr
     (elmo-imap4-response-bodydetail-text
      (elmo-imap4-response-value-all
       (elmo-imap4-send-command-wait session
				     (format
				      (if elmo-imap4-use-uid
					  "uid fetch %s body[%s]"
					"fetch %s body[%s]")
				      msg part))
       'fetch)))))

(defun elmo-imap4-prefetch-msg (spec msg outbuf)
  (elmo-imap4-read-msg spec msg outbuf 'unseen))

(defun elmo-imap4-read-msg (spec msg outbuf
				 &optional leave-seen-flag-untouched)
  (let ((session (elmo-imap4-get-session spec))
	response)
    (elmo-imap4-session-select-mailbox session
				       (elmo-imap4-spec-mailbox spec))
    (with-current-buffer (elmo-network-session-buffer session)
      (setq elmo-imap4-fetch-callback nil)
      (setq elmo-imap4-fetch-callback-data nil))
    (setq response
	  (elmo-imap4-send-command-wait session
					(format
					 (if elmo-imap4-use-uid
					     "uid fetch %s rfc822%s"
					   "fetch %s rfc822%s")
					 msg
					 (if leave-seen-flag-untouched
					     ".peek" ""))))
    (and (setq response (elmo-imap4-response-value
			 (elmo-imap4-response-value-all
			  response 'fetch )
			 'rfc822))
	 (with-current-buffer outbuf
	   (erase-buffer)
	   (insert response)
	   (elmo-delete-cr-get-content-type)))))

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
    (elmo-imap4-send-command-wait (elmo-imap4-get-session spec) "expunge")))

(defun elmo-imap4-delete-msg-by-id (spec msgid)
  (let ((session (elmo-imap4-get-session spec)))
    (elmo-imap4-session-select-mailbox session
				       (elmo-imap4-spec-mailbox spec))
    (elmo-imap4-delete-msgs-no-expunge
     spec
     (elmo-imap4-response-value
      (elmo-imap4-send-command-wait session
				    (list
				     (if elmo-imap4-use-uid
					 "uid search header message-id "
				       "search header message-id ")
				     (elmo-imap4-field-body msgid)))
      'search))))

(defun elmo-imap4-append-msg-by-id (spec msgid)
  (let ((session (elmo-imap4-get-session spec))
	send-buf)
    (elmo-imap4-session-select-mailbox session
				       (elmo-imap4-spec-mailbox spec))
    (setq send-buf (elmo-imap4-setup-send-buffer-from-file
		    (elmo-cache-get-path msgid)))
    (unwind-protect
	(elmo-imap4-send-command-wait
	 session
	 (list
	  "append "
	  (elmo-imap4-mailbox (elmo-imap4-spec-mailbox spec))
	  " (\\Seen) "
	  (elmo-imap4-buffer-literal send-buf)))
      (kill-buffer send-buf)))
  t)

(defun elmo-imap4-append-msg (spec string &optional msg no-see)
  (let ((session (elmo-imap4-get-session spec))
	send-buf)
    (elmo-imap4-session-select-mailbox session
				       (elmo-imap4-spec-mailbox spec))
    (setq send-buf (elmo-imap4-setup-send-buffer string))
    (unwind-protect
	(elmo-imap4-send-command-wait
	 session
	 (list
	  "append "
	  (elmo-imap4-mailbox (elmo-imap4-spec-mailbox spec))
	  (if no-see " " " (\\Seen) ")
	  (elmo-imap4-buffer-literal send-buf)))
      (kill-buffer send-buf)))
  t)

(defun elmo-imap4-copy-msgs (dst-spec
			     msgs src-spec &optional expunge-it same-number)
  "Equivalence of hostname, username is assumed."
  (let ((session (elmo-imap4-get-session src-spec)))
    (elmo-imap4-session-select-mailbox session
				       (elmo-imap4-spec-mailbox src-spec))
    (while msgs
      (elmo-imap4-send-command-wait session
				    (list
				     (format
				      (if elmo-imap4-use-uid
					  "uid copy %s "
					"copy %s ")
				      (car msgs))
				     (elmo-imap4-mailbox
				      (elmo-imap4-spec-mailbox dst-spec))))
      (setq msgs (cdr msgs)))
    (when expunge-it
      (elmo-imap4-send-command-wait session "expunge"))
    t))

(defun elmo-imap4-server-diff (spec)
  "Get server status"
  (let (response)
    ;; commit.
;    (elmo-imap4-commit spec)
    (setq response
	  (elmo-imap4-send-command-wait (elmo-imap4-get-session spec)
					(list
					 "status "
					 (elmo-imap4-mailbox
					  (elmo-imap4-spec-mailbox spec))
					 " (unseen messages)")))
    (setq response (elmo-imap4-response-value response 'status))
    (cons (elmo-imap4-response-value response 'unseen)
	  (elmo-imap4-response-value response 'messages))))

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

;;; IMAP parser.

(defvar elmo-imap4-server-eol "\r\n"
  "The EOL string sent from the server.")

(defvar elmo-imap4-client-eol "\r\n"
  "The EOL string we send to the server.")

(defvar elmo-imap4-status nil)
(defvar elmo-imap4-reached-tag nil)

(defun elmo-imap4-find-next-line ()
  "Return point at end of current line, taking into account literals.
Return nil if no complete line has arrived."
  (when (re-search-forward (concat elmo-imap4-server-eol "\\|{\\([0-9]+\\)}"
				   elmo-imap4-server-eol)
			   nil t)
    (if (match-string 1)
	(if (< (point-max) (+ (point) (string-to-number (match-string 1))))
	    nil
	  (goto-char (+ (point) (string-to-number (match-string 1))))
	  (elmo-imap4-find-next-line))
      (point))))

(defun elmo-imap4-sentinel (process string)
  (delete-process process))

(defun elmo-imap4-arrival-filter (proc string)
  "IMAP process filter."
  (with-current-buffer (process-buffer proc)
    (elmo-imap4-debug "-> %s" string)
    (goto-char (point-max))
    (insert string)
    (let (end)
      (goto-char (point-min))
      (while (setq end (elmo-imap4-find-next-line))
	(save-restriction
	  (narrow-to-region (point-min) end)
	  (delete-backward-char (length elmo-imap4-server-eol))
	  (goto-char (point-min))
	  (unwind-protect
	      (cond ((eq elmo-imap4-status 'initial)
		     (setq elmo-imap4-current-response
			   (list
			    (list 'greeting (elmo-imap4-parse-greeting)))))
		    ((or (eq elmo-imap4-status 'auth)
			 (eq elmo-imap4-status 'nonauth)
			 (eq elmo-imap4-status 'selected)
			 (eq elmo-imap4-status 'examine))
		     (setq elmo-imap4-current-response
			   (cons
			    (elmo-imap4-parse-response)
			    elmo-imap4-current-response)))
		    (t
		     (message "Unknown state %s in arrival filter"
			      elmo-imap4-status))))
	  (delete-region (point-min) (point-max)))))))

;; IMAP parser.

(defsubst elmo-imap4-forward ()
  (or (eobp) (forward-char 1)))

(defsubst elmo-imap4-parse-number ()
  (when (looking-at "[0-9]+")
    (prog1
	(string-to-number (match-string 0))
      (goto-char (match-end 0)))))

(defsubst elmo-imap4-parse-literal ()
  (when (looking-at "{\\([0-9]+\\)}\r\n")
    (let ((pos (match-end 0))
	  (len (string-to-number (match-string 1))))
      (if (< (point-max) (+ pos len))
	  nil
	(goto-char (+ pos len))
	(buffer-substring pos (+ pos len))))))
	;(list ' pos (+ pos len))))))

(defsubst elmo-imap4-parse-string ()
  (cond ((eq (char-after) ?\")
	 (forward-char 1)
	 (let ((p (point)) (name ""))
	   (skip-chars-forward "^\"\\\\")
	   (setq name (buffer-substring p (point)))
	   (while (eq (char-after) ?\\)
	     (setq p (1+ (point)))
	     (forward-char 2)
	     (skip-chars-forward "^\"\\\\")
	     (setq name (concat name (buffer-substring p (point)))))
	   (forward-char 1)
	   name))
	((eq (char-after) ?{)
	 (elmo-imap4-parse-literal))))

(defsubst elmo-imap4-parse-nil ()
  (if (looking-at "NIL")
      (goto-char (match-end 0))))

(defsubst elmo-imap4-parse-nstring ()
  (or (elmo-imap4-parse-string)
      (and (elmo-imap4-parse-nil)
	   nil)))

(defsubst elmo-imap4-parse-astring ()
  (or (elmo-imap4-parse-string)
      (buffer-substring (point)
			(if (re-search-forward "[(){ \r\n%*\"\\]" nil t)
			    (goto-char (1- (match-end 0)))
			  (end-of-line)
			  (point)))))

(defsubst elmo-imap4-parse-address ()
  (let (address)
    (when (eq (char-after) ?\()
      (elmo-imap4-forward)
      (setq address (vector (prog1 (elmo-imap4-parse-nstring)
			      (elmo-imap4-forward))
			    (prog1 (elmo-imap4-parse-nstring)
			      (elmo-imap4-forward))
			    (prog1 (elmo-imap4-parse-nstring)
			      (elmo-imap4-forward))
			    (elmo-imap4-parse-nstring)))
      (when (eq (char-after) ?\))
	(elmo-imap4-forward)
	address))))

(defsubst elmo-imap4-parse-address-list ()
  (if (eq (char-after) ?\()
      (let (address addresses)
	(elmo-imap4-forward)
	(while (and (not (eq (char-after) ?\)))
		    ;; next line for MS Exchange bug
		    (progn (and (eq (char-after) ? ) (elmo-imap4-forward)) t)
		    (setq address (elmo-imap4-parse-address)))
	  (setq addresses (cons address addresses)))
	(when (eq (char-after) ?\))
	  (elmo-imap4-forward)
	  (nreverse addresses)))
    (assert (elmo-imap4-parse-nil))))

(defsubst elmo-imap4-parse-mailbox ()
  (let ((mailbox (elmo-imap4-parse-astring)))
    (if (string-equal "INBOX" (upcase mailbox))
	"INBOX"
      mailbox)))

(defun elmo-imap4-parse-greeting ()
  "Parse a IMAP greeting."
  (cond ((looking-at "\\* OK ")
	 (setq elmo-imap4-status 'nonauth))
	((looking-at "\\* PREAUTH ")
	 (setq elmo-imap4-status 'auth))
	((looking-at "\\* BYE ")
	 (setq elmo-imap4-status 'closed))))

(defun elmo-imap4-parse-response ()
  "Parse a IMAP command response."
  (let (token)
    (case (setq token (elmo-imap4-read (current-buffer)))
      (+ (progn
	   (skip-chars-forward " ")
	   (list 'continue-req (buffer-substring (point) (point-max)))))
      (* (case (prog1 (setq token (elmo-imap4-read (current-buffer)))
		 (elmo-imap4-forward))
	   (OK         (elmo-imap4-parse-resp-text-code))
	   (NO         (elmo-imap4-parse-resp-text-code))
	   (BAD        (elmo-imap4-parse-resp-text-code))
	   (BYE        (elmo-imap4-parse-bye))
	   (FLAGS      (list 'flags
			     (elmo-imap4-parse-flag-list)))
	   (LIST       (list 'list (elmo-imap4-parse-data-list)))
	   (LSUB       (list 'lsub (elmo-imap4-parse-data-list)))
	   (SEARCH     (list
			'search
			(elmo-imap4-read (concat "("
				      (buffer-substring (point) (point-max))
				      ")"))))
	   (STATUS     (elmo-imap4-parse-status))
	   ;; Added
	   (NAMESPACE  (elmo-imap4-parse-namespace))
	   (CAPABILITY (list 'capability
			     (elmo-imap4-read
			      (concat "(" (downcase (buffer-substring
						     (point) (point-max)))
				      ")"))))
	   (ACL        (elmo-imap4-parse-acl))
	   (t       (case (prog1 (elmo-imap4-read (current-buffer))
			    (elmo-imap4-forward))
		      (EXISTS  (list 'exists token))
		      (RECENT  (list 'recent token))
		      (EXPUNGE (list 'expunge token))
		      (FETCH   (elmo-imap4-parse-fetch token))
		      (t       (list 'garbage (buffer-string)))))))
      (t (if (not (string-match elmo-imap4-seq-prefix (symbol-name token)))
	     (list 'garbage (buffer-string))
	   (case (prog1 (elmo-imap4-read (current-buffer))
		   (elmo-imap4-forward))
	     (OK  (progn
		    (setq elmo-imap4-parsing nil)
		    (elmo-imap4-debug "*%s* OK arrived" token)
		    (setq elmo-imap4-reached-tag token)
		    (list 'ok (elmo-imap4-parse-resp-text-code))))
	     (NO  (progn
		    (setq elmo-imap4-parsing nil)
		    (elmo-imap4-debug "*%s* NO arrived" token)
		    (setq elmo-imap4-reached-tag token)
		    (let (code text)
		      (when (eq (char-after) ?\[)
			(setq code (buffer-substring (point)
						     (search-forward "]")))
			(elmo-imap4-forward))
		      (setq text (buffer-substring (point) (point-max)))
		      (list 'no (list code text)))))
	     (BAD (progn
		    (setq elmo-imap4-parsing nil)
		    (elmo-imap4-debug "*%s* BAD arrived" token)
		    (setq elmo-imap4-reached-tag token)
		    (let (code text)
		      (when (eq (char-after) ?\[)
			(setq code (buffer-substring (point)
						     (search-forward "]")))
			(elmo-imap4-forward))
		      (setq text (buffer-substring (point) (point-max)))
		      (list 'bad (list code text)))))
	     (t   (list 'garbage (buffer-string)))))))))
		    
(defun elmo-imap4-parse-bye ()
  (let (code text)
    (when (eq (char-after) ?\[)
      (setq code (buffer-substring (point)
				   (search-forward "]")))
      (elmo-imap4-forward))
    (setq text (buffer-substring (point) (point-max)))
    (list 'bye (list code text))))

(defun elmo-imap4-parse-text ()
  (goto-char (point-min))
  (when (search-forward "[" nil t)
    (search-forward "]")
    (elmo-imap4-forward))
  (list 'text (buffer-substring (point) (point-max))))

(defun elmo-imap4-parse-resp-text-code ()
  (when (eq (char-after) ?\[)
    (elmo-imap4-forward)
    (cond ((search-forward "PERMANENTFLAGS " nil t)
	   (list 'permanentflags (elmo-imap4-parse-flag-list)))
	  ((search-forward "UIDNEXT " nil t)
	   (list 'uidnext (elmo-imap4-read (current-buffer))))
	  ((search-forward "UNSEEN " nil t)
	   (list 'unseen (elmo-imap4-read (current-buffer))))
	  ((looking-at "UIDVALIDITY \\([0-9]+\\)")
	   (list 'uidvalidity (match-string 1)))
	  ((search-forward "READ-ONLY" nil t)
	   (list 'read-only t))
	  ((search-forward "READ-WRITE" nil t)
	   (list 'read-write t))
	  ((search-forward "NEWNAME " nil t)
	   (let (oldname newname)
	     (setq oldname (elmo-imap4-parse-string))
	     (elmo-imap4-forward)
	     (setq newname (elmo-imap4-parse-string))
	     (list 'newname newname oldname)))
	  ((search-forward "TRYCREATE" nil t)
	   (list 'trycreate t))
	  ((looking-at "APPENDUID \\([0-9]+\\) \\([0-9]+\\)")
	   (list 'appenduid
		 (list (match-string 1)
		       (string-to-number (match-string 2)))))
	  ((looking-at "COPYUID \\([0-9]+\\) \\([0-9,:]+\\) \\([0-9,:]+\\)")
	   (list 'copyuid (list (match-string 1)
				(match-string 2)
				(match-string 3))))
	  ((search-forward "ALERT] " nil t)
	   (message "IMAP server information: %s"
		    (buffer-substring (point) (point-max))))
	  (t (list 'unknown)))))

(defun elmo-imap4-parse-data-list ()
  (let (flags delimiter mailbox)
    (setq flags (elmo-imap4-parse-flag-list))
    (when (looking-at " NIL\\| \"\\\\?\\(.\\)\"")
      (setq delimiter (match-string 1))
      (goto-char (1+ (match-end 0)))
      (when (setq mailbox (elmo-imap4-parse-mailbox))
	(list mailbox flags delimiter)))))

(defsubst elmo-imap4-parse-header-list ()
  (when (eq (char-after) ?\()
    (let (strlist)
      (while (not (eq (char-after) ?\)))
	(elmo-imap4-forward)
	(push (elmo-imap4-parse-astring) strlist))
      (elmo-imap4-forward)
      (nreverse strlist))))

(defsubst elmo-imap4-parse-fetch-body-section ()
  (let ((section
	 (buffer-substring (point)
			   (1-
			    (progn (re-search-forward "[] ]" nil t)
				   (point))))))
    (if (eq (char-before) ? )
	(prog1
	    (mapconcat 'identity
		       (cons section (elmo-imap4-parse-header-list)) " ")
	  (search-forward "]" nil t))
      section)))

(defun elmo-imap4-parse-fetch (response)
  (when (eq (char-after) ?\()
    (let (element list)
      (while (not (eq (char-after) ?\)))
	(elmo-imap4-forward)
	(let ((token (elmo-imap4-fetch-read (current-buffer))))
	  (elmo-imap4-forward)
	  (setq element
		(cond ((eq token 'UID)
		       (list 'uid (condition-case nil
				      (elmo-imap4-read (current-buffer))
				    (error nil))))
		      ((eq token 'FLAGS)
		       (list 'flags (elmo-imap4-parse-flag-list)))
		      ((eq token 'ENVELOPE)
		       (list 'envelope (elmo-imap4-parse-envelope)))
		      ((eq token 'INTERNALDATE)
		       (list 'internaldate (elmo-imap4-parse-string)))
		      ((eq token 'RFC822)
		       (list 'rfc822 (elmo-imap4-parse-nstring)))
		      ((eq token (intern elmo-imap4-rfc822-header))
		       (list 'rfc822header (elmo-imap4-parse-nstring)))
		      ((eq token (intern elmo-imap4-rfc822-text))
		       (list 'rfc822text (elmo-imap4-parse-nstring)))
		      ((eq token (intern elmo-imap4-rfc822-size))
		       (list 'rfc822size (elmo-imap4-read (current-buffer))))
		      ((eq token 'BODY)
		       (if (eq (char-before) ?\[)
			   (list
			    'bodydetail
			    (upcase (elmo-imap4-parse-fetch-body-section))
			    (and
			     (eq (char-after) ?<)
			     (buffer-substring (1+ (point))
					       (progn
						 (search-forward ">" nil t)
						 (point))))
			    (progn (elmo-imap4-forward)
				   (elmo-imap4-parse-nstring)))
			 (list 'body (elmo-imap4-parse-body))))
		      ((eq token 'BODYSTRUCTURE)
		       (list 'bodystructure (elmo-imap4-parse-body)))))
	  (setq list (cons element list))))
      (and elmo-imap4-fetch-callback
	   (elmo-imap4-fetch-callback
	    list
	    elmo-imap4-fetch-callback-data))
      (list 'fetch list))))

(defun elmo-imap4-parse-status ()
  (let ((mailbox (elmo-imap4-parse-mailbox))
	status)
    (when (and mailbox (search-forward "(" nil t))
      (while (not (eq (char-after) ?\)))
	(setq status
	      (cons
	       (let ((token (elmo-imap4-read (current-buffer))))
		 (cond ((eq token 'MESSAGES)
			(list 'messages (elmo-imap4-read (current-buffer))))
		       ((eq token 'RECENT)
			(list 'recent (elmo-imap4-read (current-buffer))))
		       ((eq token 'UIDNEXT)
			(list 'uidnext (elmo-imap4-read (current-buffer))))
		       ((eq token 'UIDVALIDITY)
			(and (looking-at " \\([0-9]+\\)")
			     (prog1 (list 'uidvalidity (match-string 1))
			       (goto-char (match-end 1)))))
		       ((eq token 'UNSEEN)
			(list 'unseen (elmo-imap4-read (current-buffer))))
		       (t
			(message
			 "Unknown status data %s in mailbox %s ignored"
			 token mailbox))))
	       status))))
    (list 'status status)))


(defmacro elmo-imap4-value (value)
  (` (if (eq (, value) 'NIL) nil
       (, value))))

(defmacro elmo-imap4-nth (pos list)
  (` (let ((value (nth (, pos) (, list))))
       (elmo-imap4-value value))))

(defun elmo-imap4-parse-namespace ()
  (list 'namespace
	(nconc
	 (copy-sequence elmo-imap4-extra-namespace-alist)
	 (elmo-imap4-parse-namespace-subr
	  (elmo-imap4-read (concat "(" (buffer-substring
			     (point) (point-max))
			")"))))))

(defun elmo-imap4-parse-namespace-subr (ns)
  (let (prefix delim namespace-alist default-delim)
    ;; 0: personal, 1: other, 2: shared
    (dotimes (i 3)
      (setq namespace-alist
	    (nconc namespace-alist
		   (delq nil
			 (mapcar
			  (lambda (namespace)
			    (setq prefix (elmo-imap4-nth 0 namespace)
				  delim (elmo-imap4-nth 1 namespace))
			    (if (and prefix delim
				     (string-match
				      (concat (regexp-quote delim) "\\'")
				      prefix))
				(setq prefix (substring prefix 0
							(match-beginning 0))))
			    (if (eq (length prefix) 0)
				(progn (setq default-delim delim) nil)
			      (cons
			       (concat "^"
				       (if (string= (downcase prefix) "inbox")
					   "[Ii][Nn][Bb][Oo][Xx]"
					 (regexp-quote prefix))
				       ".*$")
			       delim)))
			  (elmo-imap4-nth i ns))))))
    (if default-delim
	(setq namespace-alist
	      (nconc namespace-alist
		     (list (cons "^.*$" default-delim)))))
    namespace-alist))

(defun elmo-imap4-parse-acl ()
  (let ((mailbox (elmo-imap4-parse-mailbox))
	identifier rights acl)
    (while (eq (char-after) ?\ )
      (elmo-imap4-forward)
      (setq identifier (elmo-imap4-parse-astring))
      (elmo-imap4-forward)
      (setq rights (elmo-imap4-parse-astring))
      (setq acl (append acl (list (cons identifier rights)))))
    (list 'acl acl mailbox)))

(defun elmo-imap4-parse-flag-list ()
  (let ((str (buffer-substring (point) (progn (search-forward ")" nil t)
					      (point))))
	pos)
    (while (setq pos (string-match "\\\\" str (and pos (+ 2 pos))))
      (setq str (replace-match "\\\\" nil t str)))
    (mapcar 'symbol-name (elmo-imap4-read str))))

(defun elmo-imap4-parse-envelope ()
  (when (eq (char-after) ?\()
    (elmo-imap4-forward)
    (vector (prog1 (elmo-imap4-parse-nstring);; date
	      (elmo-imap4-forward))
	    (prog1 (elmo-imap4-parse-nstring);; subject
	      (elmo-imap4-forward))
	    (prog1 (elmo-imap4-parse-address-list);; from
	      (elmo-imap4-forward))
	    (prog1 (elmo-imap4-parse-address-list);; sender
	      (elmo-imap4-forward))
	    (prog1 (elmo-imap4-parse-address-list);; reply-to
	      (elmo-imap4-forward))
	    (prog1 (elmo-imap4-parse-address-list);; to
	      (elmo-imap4-forward))
	    (prog1 (elmo-imap4-parse-address-list);; cc
	      (elmo-imap4-forward))
	    (prog1 (elmo-imap4-parse-address-list);; bcc
	      (elmo-imap4-forward))
	    (prog1 (elmo-imap4-parse-nstring);; in-reply-to
	      (elmo-imap4-forward))
	    (prog1 (elmo-imap4-parse-nstring);; message-id
	      (elmo-imap4-forward)))))

(defsubst elmo-imap4-parse-string-list ()
  (cond ((eq (char-after) ?\();; body-fld-param
	 (let (strlist str)
	   (elmo-imap4-forward)
	   (while (setq str (elmo-imap4-parse-string))
	     (push str strlist)
	     (elmo-imap4-forward))
	   (nreverse strlist)))
	((elmo-imap4-parse-nil)
	 nil)))

(defun elmo-imap4-parse-body-extension ()
  (if (eq (char-after) ?\()
      (let (b-e)
	(elmo-imap4-forward)
	(push (elmo-imap4-parse-body-extension) b-e)
	(while (eq (char-after) ?\ )
	  (elmo-imap4-forward)
	  (push (elmo-imap4-parse-body-extension) b-e))
	(assert (eq (char-after) ?\)))
	(elmo-imap4-forward)
	(nreverse b-e))
    (or (elmo-imap4-parse-number)
	(elmo-imap4-parse-nstring))))

(defsubst elmo-imap4-parse-body-ext ()
  (let (ext)
    (when (eq (char-after) ?\ );; body-fld-dsp
      (elmo-imap4-forward)
      (let (dsp)
	(if (eq (char-after) ?\()
	    (progn
	      (elmo-imap4-forward)
	      (push (elmo-imap4-parse-string) dsp)
	      (elmo-imap4-forward)
	      (push (elmo-imap4-parse-string-list) dsp)
	      (elmo-imap4-forward))
	  (assert (elmo-imap4-parse-nil)))
	(push (nreverse dsp) ext))
      (when (eq (char-after) ?\ );; body-fld-lang
	(elmo-imap4-forward)
	(if (eq (char-after) ?\()
	    (push (elmo-imap4-parse-string-list) ext)
	  (push (elmo-imap4-parse-nstring) ext))
	(while (eq (char-after) ?\ );; body-extension
	  (elmo-imap4-forward)
	  (setq ext (append (elmo-imap4-parse-body-extension) ext)))))
    ext))

(defun elmo-imap4-parse-body ()
  (let (body)
    (when (eq (char-after) ?\()
      (elmo-imap4-forward)
      (if (eq (char-after) ?\()
	  (let (subbody)
	    (while (and (eq (char-after) ?\()
			(setq subbody (elmo-imap4-parse-body)))
	      (push subbody body))
	    (elmo-imap4-forward)
	    (push (elmo-imap4-parse-string) body);; media-subtype
	    (when (eq (char-after) ?\ );; body-ext-mpart:
	      (elmo-imap4-forward)
	      (if (eq (char-after) ?\();; body-fld-param
		  (push (elmo-imap4-parse-string-list) body)
		(push (and (elmo-imap4-parse-nil) nil) body))
	      (setq body
		    (append (elmo-imap4-parse-body-ext) body)));; body-ext-...
	    (assert (eq (char-after) ?\)))
	    (elmo-imap4-forward)
	    (nreverse body))

	(push (elmo-imap4-parse-string) body);; media-type
	(elmo-imap4-forward)
	(push (elmo-imap4-parse-string) body);; media-subtype
	(elmo-imap4-forward)
	;; next line for Sun SIMS bug
	(and (eq (char-after) ? ) (elmo-imap4-forward))
	(if (eq (char-after) ?\();; body-fld-param
	    (push (elmo-imap4-parse-string-list) body)
	  (push (and (elmo-imap4-parse-nil) nil) body))
	(elmo-imap4-forward)
	(push (elmo-imap4-parse-nstring) body);; body-fld-id
	(elmo-imap4-forward)
	(push (elmo-imap4-parse-nstring) body);; body-fld-desc
	(elmo-imap4-forward)
	(push (elmo-imap4-parse-string) body);; body-fld-enc
	(elmo-imap4-forward)
	(push (elmo-imap4-parse-number) body);; body-fld-octets

	;; ok, we're done parsing the required parts, what comes now is one
	;; of three things:
	;;
	;; envelope       (then we're parsing body-type-msg)
	;; body-fld-lines (then we're parsing body-type-text)
	;; body-ext-1part (then we're parsing body-type-basic)
	;;
	;; the problem is that the two first are in turn optionally followed
	;; by the third.  So we parse the first two here (if there are any)...

	(when (eq (char-after) ?\ )
	  (elmo-imap4-forward)
	  (let (lines)
	    (cond ((eq (char-after) ?\();; body-type-msg:
		   (push (elmo-imap4-parse-envelope) body);; envelope
		   (elmo-imap4-forward)
		   (push (elmo-imap4-parse-body) body);; body
		   (elmo-imap4-forward)
		   (push (elmo-imap4-parse-number) body));; body-fld-lines
		  ((setq lines (elmo-imap4-parse-number));; body-type-text:
		   (push lines body));; body-fld-lines
		  (t
		   (backward-char)))));; no match...

	;; ...and then parse the third one here...

	(when (eq (char-after) ?\ );; body-ext-1part:
	  (elmo-imap4-forward)
	  (push (elmo-imap4-parse-nstring) body);; body-fld-md5
	  (setq body
		(append (elmo-imap4-parse-body-ext) body)));; body-ext-1part..
    
	(assert (eq (char-after) ?\)))
	(elmo-imap4-forward)
	(nreverse body)))))

(provide 'elmo-imap4)

;;; elmo-imap4.el ends here
