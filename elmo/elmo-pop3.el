;;; elmo-pop3.el -- POP3 Interface for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1999,2000      Kenichi OKADA <okada@opaopa.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Kenichi OKADA <okada@opaopa.org>
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

;;; Code:
;; 

(require 'elmo-msgdb)
(require 'elmo-net)

(eval-when-compile
  (require 'elmo-util))

(eval-and-compile
  (autoload 'md5 "md5"))

;; POP3
(defcustom elmo-pop3-default-user (or (getenv "USER")
				      (getenv "LOGNAME")
				      (user-login-name))
  "*Default username for POP3."
  :type 'string
  :group 'elmo)

(defcustom elmo-pop3-default-server  "localhost"
  "*Default POP3 server."
  :type 'string
  :group 'elmo)

(defcustom elmo-pop3-default-authenticate-type 'user
  "*Default Authentication type for POP3."
  :type 'symbol
  :group 'elmo)

(defcustom elmo-pop3-default-port 110
  "*Default POP3 port."
  :type 'integer
  :group 'elmo)

(defcustom elmo-pop3-default-stream-type nil
  "*Default stream type for POP3.
Any symbol value of `elmo-network-stream-type-alist' or
`elmo-pop3-stream-type-alist'."
  :type 'symbol
  :group 'elmo)

(defcustom elmo-pop3-default-use-uidl t
  "If non-nil, use UIDL on POP3."
  :type 'boolean
  :group 'elmo)

(defvar elmo-pop3-stream-type-alist nil
  "*Stream bindings for POP3.
This is taken precedence over `elmo-network-stream-type-alist'.")

(defvar elmo-pop3-use-uidl-internal t
  "(Internal switch for using UIDL on POP3).")

(defvar elmo-pop3-use-cache t
  "Use cache in pop3 folder.")

(defvar elmo-pop3-send-command-synchronously nil
  "If non-nil, commands are send synchronously.
If server doesn't accept asynchronous commands, this variable should be
set as non-nil.")

(defvar elmo-pop3-exists-exactly t)
(defvar sasl-mechanism-alist)

(defvar elmo-pop3-total-size nil)

;; For debugging.
(defvar elmo-pop3-debug nil
  "Non-nil forces POP3 folder as debug mode.
Debug information is inserted in the buffer \"*POP3 DEBUG*\"")

(defvar elmo-pop3-debug-inhibit-logging nil)

;;; Debug
(defsubst elmo-pop3-debug (message &rest args)
  (if elmo-pop3-debug
      (with-current-buffer (get-buffer-create "*POP3 DEBUG*")
	(goto-char (point-max))
	(if elmo-pop3-debug-inhibit-logging
	    (insert "NO LOGGING\n")
	  (insert (apply 'format message args) "\n")))))

;;; ELMO POP3 folder
(eval-and-compile
  (luna-define-class elmo-pop3-folder (elmo-net-folder)
		     (use-uidl location-alist))
  (luna-define-internal-accessors 'elmo-pop3-folder))

(luna-define-method elmo-folder-initialize :around ((folder
						     elmo-pop3-folder)
						    name)
  (let ((elmo-network-stream-type-alist
	 (if elmo-pop3-stream-type-alist
	     (append elmo-pop3-stream-type-alist
		     elmo-network-stream-type-alist)
	   elmo-network-stream-type-alist)))
    (setq name (luna-call-next-method))
    ;; Setup slots for elmo-net-folder
    (when (string-match "^\\([^:/!]*\\)\\(/[^/:@!]+\\)?\\(:[^/:@!]+\\)?" name)
      (elmo-net-folder-set-user-internal folder
					 (if (match-beginning 1)
					     (elmo-match-string 1 name)))
      (if (eq (length (elmo-net-folder-user-internal folder)) 0)
	  (elmo-net-folder-set-user-internal folder
					     elmo-pop3-default-user))
      (elmo-net-folder-set-auth-internal
       folder
       (if (match-beginning 2)
	   (intern (elmo-match-substring 2 name 1))
	 elmo-pop3-default-authenticate-type))
      (elmo-pop3-folder-set-use-uidl-internal
       folder
       (if (match-beginning 3)
	   (string= (elmo-match-substring 3 name 1) "uidl")
	 elmo-pop3-default-use-uidl)))
    (unless (elmo-net-folder-server-internal folder)
      (elmo-net-folder-set-server-internal folder 
					   elmo-pop3-default-server))
    (unless (elmo-net-folder-port-internal folder)
      (elmo-net-folder-set-port-internal folder
					 elmo-pop3-default-port))
    (unless (elmo-net-folder-stream-type-internal folder)
      (elmo-net-folder-set-stream-type-internal
       folder
       elmo-pop3-default-stream-type))
    folder))

;;; POP3 session
(luna-define-class elmo-pop3-session (elmo-network-session) ())

;; buffer-local
(defvar elmo-pop3-read-point nil)
(defvar elmo-pop3-number-uidl-hash nil) ; number -> uidl
(defvar elmo-pop3-uidl-number-hash nil) ; uidl -> number
(defvar elmo-pop3-size-hash nil) ; number -> size
(defvar elmo-pop3-uidl-done nil)
(defvar elmo-pop3-list-done nil)
(defvar elmo-pop3-lock nil)

(defvar elmo-pop3-local-variables '(elmo-pop3-read-point
				    elmo-pop3-uidl-number-hash
				    elmo-pop3-number-uidl-hash
				    elmo-pop3-uidl-done
				    elmo-pop3-size-hash
				    elmo-pop3-list-done
				    elmo-pop3-lock))

(luna-define-method elmo-network-close-session ((session elmo-pop3-session))
  (when (elmo-network-session-process-internal session)
    (when (memq (process-status
		 (elmo-network-session-process-internal session))
		'(open run))
      (let ((buffer (process-buffer 
		     (elmo-network-session-process-internal session))))
	(elmo-pop3-send-command (elmo-network-session-process-internal session)
				"quit")
	;; process is dead.
	(or (elmo-pop3-read-response
	     (elmo-network-session-process-internal session)
	     t buffer)
	    (error "POP error: QUIT failed"))))
    (kill-buffer (process-buffer
		  (elmo-network-session-process-internal session)))
    (delete-process (elmo-network-session-process-internal session))))

(defun elmo-pop3-get-session (folder &optional if-exists)
  (let ((elmo-pop3-use-uidl-internal (if elmo-inhibit-number-mapping
					 nil
				       (elmo-pop3-folder-use-uidl-internal
					folder))))
    (elmo-network-get-session 'elmo-pop3-session "POP3" folder if-exists)))

(defun elmo-pop3-send-command (process command &optional no-erase)
  (with-current-buffer (process-buffer process)
    (unless no-erase
      (erase-buffer))
    (goto-char (point-min))
    (setq elmo-pop3-read-point (point))
    (elmo-pop3-lock)
    (elmo-pop3-debug "SEND: %s\n" command)
    (process-send-string process command)
    (process-send-string process "\r\n")))

(defun elmo-pop3-read-response (process &optional not-command keep-lock)
  ;; buffer is in case for process is dead.
  (with-current-buffer (process-buffer process)
    (let ((case-fold-search nil)
	  (response-string nil)
	  (response-continue t)
	  (return-value nil)
	  match-end)
      (while response-continue
	(goto-char elmo-pop3-read-point)
	(while (not (re-search-forward "\r?\n" nil t))
	  (accept-process-output process)
	  (goto-char elmo-pop3-read-point))
	(setq match-end (point))
	(setq response-string
	      (buffer-substring elmo-pop3-read-point (- match-end 2)))
	(goto-char elmo-pop3-read-point)
	(if (looking-at "\\+.*$")
	    (progn
	      (setq response-continue nil)
	      (setq elmo-pop3-read-point match-end)
	      (setq return-value
		    (if return-value
			(concat return-value "\n" response-string)
		      response-string)))
	  (if (looking-at "\\-.*$")
	      (progn
		(setq response-continue nil)
		(setq elmo-pop3-read-point match-end)
		(setq return-value nil))
	    (setq elmo-pop3-read-point match-end)
	    (if not-command
 		(setq response-continue nil))
	    (setq return-value
		  (if return-value
		      (concat return-value "\n" response-string)
		    response-string)))
	  (setq elmo-pop3-read-point match-end)))
      (unless keep-lock	(elmo-pop3-unlock))
      return-value)))

(defun elmo-pop3-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)
    (elmo-pop3-debug "RECEIVED: %s\n" output)
    (if (and elmo-pop3-total-size
	     (> elmo-pop3-total-size 
		(min elmo-display-retrieval-progress-threshold 100)))
	(elmo-display-progress
	 'elmo-display-retrieval-progress
	 (format "Retrieving (%d/%d bytes)..."
		 (buffer-size)
		 elmo-pop3-total-size)
	 (/ (buffer-size) (/ elmo-pop3-total-size 100))))))

(defun elmo-pop3-auth-user (session)
  (let ((process (elmo-network-session-process-internal session)))
    ;; try USER/PASS
    (elmo-pop3-send-command
     process
     (format "user %s" (elmo-network-session-user-internal session)))
    (or (elmo-pop3-read-response process t)
	(signal 'elmo-authenticate-error
		'(elmo-pop-auth-user)))
    (elmo-pop3-send-command  process
			     (format
			      "pass %s"
			      (elmo-get-passwd
			       (elmo-network-session-password-key session))))
    (or (elmo-pop3-read-response process t)
	(signal 'elmo-authenticate-error
		'(elmo-pop-auth-user)))))

(defun elmo-pop3-auth-apop (session)
  (if (string-match "^\+OK .*\\(<[^\>]+>\\)"
		    (elmo-network-session-greeting-internal session))
      ;; good, APOP ready server
      (progn
	(elmo-pop3-send-command
	 (elmo-network-session-process-internal session)
	 (format "apop %s %s"
		 (elmo-network-session-user-internal session)
		 (md5
		  (concat (match-string
			   1
			   (elmo-network-session-greeting-internal session))
			  (elmo-get-passwd
			   (elmo-network-session-password-key session))))))
	(or (elmo-pop3-read-response
	     (elmo-network-session-process-internal session)
	     t)
	    (signal 'elmo-authenticate-error
		    '(elmo-pop3-auth-apop))))
    (signal 'elmo-open-error '(elmo-pop3-auth-apop))))
    
(luna-define-method elmo-network-initialize-session-buffer :after
  ((session elmo-pop3-session) buffer)
  (with-current-buffer buffer
    (mapcar 'make-variable-buffer-local elmo-pop3-local-variables)))

(luna-define-method elmo-network-initialize-session ((session
						      elmo-pop3-session))
  (let ((process (elmo-network-session-process-internal session))
	response mechanism)
    (with-current-buffer (process-buffer process)
      (elmo-pop3-lock)
      (set-process-filter process 'elmo-pop3-process-filter)
      (setq elmo-pop3-read-point (point-min))
      ;; Skip garbage output from process before greeting.
      (while (and (memq (process-status process) '(open run))
		  (goto-char (point-max))
		  (forward-line -1)
		  (not (looking-at "+OK")))
	(accept-process-output process 1))
      (setq elmo-pop3-read-point (point))
      (or (elmo-network-session-set-greeting-internal
	   session
	   (elmo-pop3-read-response process t))
	  (signal 'elmo-open-error
		  '(elmo-network-intialize-session)))
      (when (eq (elmo-network-stream-type-symbol
		 (elmo-network-session-stream-type-internal session))
		'starttls)
	(elmo-pop3-send-command process "stls")
	(if (string-match "^\+OK"
			  (elmo-pop3-read-response process))
	    (starttls-negotiate process)
	  (signal 'elmo-open-error
		  '(elmo-pop3-starttls-error)))))))

(luna-define-method elmo-network-authenticate-session ((session
							elmo-pop3-session))
  (with-current-buffer (process-buffer 
			(elmo-network-session-process-internal session))
    (let* ((process (elmo-network-session-process-internal session))
	   (elmo-pop3-debug-inhibit-logging t)
	   (auth (elmo-network-session-auth-internal session))
	   (auth (mapcar '(lambda (mechanism) (upcase (symbol-name mechanism)))
			 (if (listp auth) auth (list auth))))
	   sasl-mechanisms
	   client name step response mechanism
	   sasl-read-passphrase)
      (or (and (string= "USER" (car auth))
	       (elmo-pop3-auth-user session))
	  (and (string= "APOP" (car auth))
	       (elmo-pop3-auth-apop session))
	  (progn
	    (require 'sasl)
	    (setq sasl-mechanisms (mapcar 'car sasl-mechanism-alist))
	    (setq mechanism (sasl-find-mechanism auth))
	    (unless mechanism
	      (signal 'elmo-authenticate-error '(elmo-pop3-auth-no-mechanisms)))
	    (setq client
		  (sasl-make-client
		   mechanism
		   (elmo-network-session-user-internal session)
		   "pop"
		   (elmo-network-session-server-internal session)))
;;;	    (if elmo-pop3-auth-user-realm
;;;		(sasl-client-set-property client 'realm elmo-pop3-auth-user-realm))
	    (setq name (sasl-mechanism-name mechanism))
	    (elmo-network-session-set-auth-internal session
						    (intern (downcase name)))
	    (setq sasl-read-passphrase
		  (function
		   (lambda (prompt)
		     (elmo-get-passwd
		      (elmo-network-session-password-key session)))))
	    (setq step (sasl-next-step client nil))
	    (elmo-pop3-send-command
	     process
	     (concat "AUTH " name
		     (and (sasl-step-data step)
			  (concat
			   " "
			   (elmo-base64-encode-string
			    (sasl-step-data step) 'no-line-break))))) ;)
	    (catch 'done
	      (while t
		(unless (setq response (elmo-pop3-read-response process t))
		  ;; response is NO or BAD.
		  (signal 'elmo-authenticate-error
			  (list (intern
				 (concat "elmo-pop3-auth-"
					 (downcase name))))))
		(if (string-match "^\+OK" response)
		    (if (sasl-next-step client step)
			;; Bogus server?
			(signal 'elmo-authenticate-error
				(list (intern
				       (concat "elmo-pop3-auth-"
					       (downcase name)))))
		      ;; The authentication process is finished.
		      (throw 'done nil)))
		(sasl-step-set-data
		 step
		 (elmo-base64-decode-string 
		  (cadr (split-string response " "))))
		(setq step (sasl-next-step client step))
		(elmo-pop3-send-command
		 process
		 (if (sasl-step-data step)
		     (elmo-base64-encode-string (sasl-step-data step)
						'no-line-break)
		   "")))))))))

(luna-define-method elmo-network-setup-session ((session
						 elmo-pop3-session))
  (let ((process (elmo-network-session-process-internal session))
	count response)
    (with-current-buffer (process-buffer process)
      (setq elmo-pop3-size-hash (elmo-make-hash 31))
      ;; To get obarray of uidl and size
      (elmo-pop3-send-command process "list")
      (if (null (elmo-pop3-read-response process nil 'keep-lock))
	  (error "POP LIST command failed"))
      (if (null (setq response
		      (elmo-pop3-read-contents
		       (current-buffer) process)))
	  (error "POP LIST command failed"))
      ;; POP server always returns a sequence of serial numbers.
      (setq count (elmo-pop3-parse-list-response response))
      ;; UIDL
      (when elmo-pop3-use-uidl-internal
	(setq elmo-pop3-uidl-number-hash (elmo-make-hash (* count 2)))
	(setq elmo-pop3-number-uidl-hash (elmo-make-hash (* count 2)))
	;; UIDL
	(elmo-pop3-send-command process "uidl")
	(unless (elmo-pop3-read-response process nil 'keep-lock)
	  (error "POP UIDL failed"))
	(unless (setq response (elmo-pop3-read-contents
				(current-buffer) process))
	  (error "POP UIDL failed"))
	(elmo-pop3-parse-uidl-response response)))))

(defun elmo-pop3-read-contents (buffer process)
  (with-current-buffer buffer
    (let ((case-fold-search nil)
	  match-end)
      (goto-char elmo-pop3-read-point)
      (while (not (re-search-forward "^\\.\r\n" nil t))
	(accept-process-output process)
	(goto-char elmo-pop3-read-point))
      (setq match-end (point))
      (elmo-pop3-unlock)      
      (elmo-delete-cr
       (buffer-substring elmo-pop3-read-point
			 (- match-end 3))))))

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-pop3-folder))
  (convert-standard-filename
   (expand-file-name
    (elmo-safe-filename (elmo-net-folder-user-internal folder))
    (expand-file-name (elmo-net-folder-server-internal folder)
		      (expand-file-name
		       "pop"
		       elmo-msgdb-dir)))))

(luna-define-method elmo-folder-exists-p ((folder elmo-pop3-folder))
  (if (and elmo-pop3-exists-exactly
	   (elmo-folder-plugged-p folder))
      (save-excursion
	(let (elmo-auto-change-plugged  ; don't change plug status.
	      (elmo-inhibit-number-mapping t) ; No need to use uidl.
	      session)
	  (prog1
	      (setq session (elmo-pop3-get-session folder))
	    (if session
		(elmo-network-close-session session)))))
    t))

(defun elmo-pop3-parse-uidl-response (string)
  (let ((buffer (current-buffer))
	number list size)
    (with-temp-buffer
      (let (number uid list)
	(insert string)
	(goto-char (point-min))
	(while (re-search-forward "^\\([0-9]+\\)[\t ]\\([^ \n]+\\)$" nil t)
	  (setq number  (elmo-match-buffer 1))
	  (setq uid (elmo-match-buffer 2))
	  (with-current-buffer buffer
	    (elmo-set-hash-val uid number elmo-pop3-uidl-number-hash)
	    (elmo-set-hash-val (concat "#" number) uid
			       elmo-pop3-number-uidl-hash))
	  (setq list (cons uid list)))
	(with-current-buffer buffer (setq elmo-pop3-uidl-done t))
	(nreverse list)))))

(defun elmo-pop3-parse-list-response (string)
  (let ((buffer (current-buffer))
	(count 0)
	alist)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9]+\\)[\t ]\\([0-9]+\\)$" nil t)
	(setq alist
	      (cons
	       (cons (elmo-match-buffer 1)
		     (elmo-match-buffer 2))
	       alist))
	(setq count (1+ count)))
      (with-current-buffer buffer
	(setq elmo-pop3-size-hash (elmo-make-hash (* (length alist) 2)))
	(while alist
	  (elmo-set-hash-val (concat "#" (car (car alist)))
			     (cdr (car alist))
			     elmo-pop3-size-hash)
	  (setq alist (cdr alist)))
	(setq elmo-pop3-list-done t))
      count)))

(defun elmo-pop3-list-location (folder)
  (with-current-buffer (process-buffer
			(elmo-network-session-process-internal
			 (elmo-pop3-get-session folder)))
    (let (list)
      (if elmo-pop3-uidl-done
	  (progn
	    (mapatoms
	     (lambda (atom)
	       (setq list (cons (symbol-name atom) list)))
	     elmo-pop3-uidl-number-hash)
	    (nreverse list))
	(error "POP3: Error in UIDL")))))

(defun elmo-pop3-list-folder-by-location (folder locations)
  (let* ((location-alist (elmo-pop3-folder-location-alist-internal folder))
	 (locations-in-db (mapcar 'cdr location-alist))
	 result new-locs new-alist deleted-locs i)
    (setq new-locs
	  (elmo-delete-if (function
			   (lambda (x) (member x locations-in-db)))
			  locations))
    (setq deleted-locs
	  (elmo-delete-if (function
			   (lambda (x) (member x locations)))
			  locations-in-db))
    (setq i (or (elmo-max-of-list (mapcar 'car location-alist)) 0))
    (mapcar
     (function
      (lambda (x)
	(setq location-alist
	      (delq (rassoc x location-alist) location-alist))))
     deleted-locs)
    (while new-locs
      (setq i (1+ i))
      (setq new-alist (cons (cons i (car new-locs)) new-alist))
      (setq new-locs (cdr new-locs)))
    (setq result (nconc location-alist new-alist))
    (setq result (sort result (lambda (x y) (< (car x)(car y)))))
    (elmo-pop3-folder-set-location-alist-internal folder result)
    (mapcar 'car result)))

(defun elmo-pop3-list-by-uidl-subr (folder &optional nonsort)
  (let ((flist (elmo-pop3-list-folder-by-location
		folder
		(elmo-pop3-list-location folder))))
    (if nonsort
	(cons (elmo-max-of-list flist) (length flist))
      (sort flist '<))))

(defun elmo-pop3-list-by-list (folder)
  (with-current-buffer (process-buffer
			(elmo-network-session-process-internal
			 (elmo-pop3-get-session folder)))
    (let (list)
      (if elmo-pop3-list-done
	  (progn
	    (mapatoms (lambda (atom)
			(setq list (cons (string-to-int
					  (substring (symbol-name atom) 1))
					 list)))
		      elmo-pop3-size-hash)
	    (sort list '<))
	(error "POP3: Error in list")))))

(defsubst elmo-pop3-folder-list-messages (folder)
  (if (and (not elmo-inhibit-number-mapping)
	   (elmo-pop3-folder-use-uidl-internal folder))
      (elmo-pop3-list-by-uidl-subr folder)
    (elmo-pop3-list-by-list folder)))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-pop3-folder) &optional nohide)
  (elmo-pop3-folder-list-messages folder))

(luna-define-method elmo-folder-status ((folder elmo-pop3-folder))
  (elmo-folder-check folder)
  (if (elmo-pop3-folder-use-uidl-internal folder)
      (elmo-pop3-list-by-uidl-subr folder 'nonsort)
    (let* ((process
	    (elmo-network-session-process-internal
	     (elmo-pop3-get-session folder)))
	   (total 0)
	   response)
      (with-current-buffer (process-buffer process)
	(elmo-pop3-send-command process "STAT")
	(setq response (elmo-pop3-read-response process))
	;; response: "^\+OK 2 7570$"
	(if (not (string-match "^\+OK[ \t]*\\([0-9]*\\)" response))
	    (error "POP STAT command failed")
	  (setq total
		(string-to-int
		 (substring response (match-beginning 1)(match-end 1 ))))
	  (cons total total))))))

(defvar elmo-pop3-header-fetch-chop-length 200)

(defsubst elmo-pop3-next-result-arrived-p ()
  (cond
   ((eq (following-char) ?+)
    (if (re-search-forward "\n\\.\r?\n" nil t)
 	t
      nil))
   ((looking-at "-")
    (if (search-forward "\n" nil t)
 	t
      nil))
   (t
    nil)))

(defun elmo-pop3-lock ()
  "Lock pop3 process."
  (setq elmo-pop3-lock t))

(defun elmo-pop3-unlock ()
  "Unlock pop3 process."
  (setq elmo-pop3-lock nil))

(defun elmo-pop3-locked-p (process)
  "Return t if pop3 PROCESS is locked."
  (with-current-buffer (process-buffer process)
    (if elmo-pop3-lock
	(progn
	  (elmo-pop3-debug "POP3 is LOCKED!")
	  t)
      nil)))
     
(defun elmo-pop3-retrieve-headers (buffer tobuffer process articles)
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (let ((number (length articles))
	  (count 0)
	  (received 0)
	  (last-point (point-min)))
      ;; Send HEAD commands.
      (while articles
	(elmo-pop3-send-command process (format
					 "top %s 0" (car articles))
				'no-erase)
;;;	(accept-process-output process 1)
	(setq articles (cdr articles))
	(setq count (1+ count))
	;; Every 200 requests we have to read the stream in
	;; order to avoid deadlocks.
	(when (or elmo-pop3-send-command-synchronously
		  (null articles)	;All requests have been sent.
		  (zerop (% count elmo-pop3-header-fetch-chop-length)))
	  (unless elmo-pop3-send-command-synchronously
	    (accept-process-output process 1))
	  (discard-input)
	  (while (progn
		   (set-buffer buffer)
		   (goto-char last-point)
		   ;; Count replies.
		   (while (elmo-pop3-next-result-arrived-p)
		     (setq last-point (point))
		     (setq received (1+ received)))
		   (< received count))
	    (when (> number elmo-display-progress-threshold)
	      (if (or (zerop (% received 5)) (= received number))
		  (elmo-display-progress
		   'elmo-pop3-retrieve-headers "Getting headers..."
		   (/ (* received 100) number))))
	    (accept-process-output process 1)
;;;	    (accept-process-output process)
	    (discard-input))))
      ;; Remove all "\r"'s.
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
	(replace-match "\n"))
      (copy-to-buffer tobuffer (point-min) (point-max)))))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-pop3-folder)
					      numlist new-mark
					      already-mark seen-mark
					      important-mark seen-list)
  (let ((process (elmo-network-session-process-internal
		  (elmo-pop3-get-session folder))))
    (with-current-buffer (process-buffer process)
      (elmo-pop3-sort-msgdb-by-original-number
       folder
       (elmo-pop3-msgdb-create-by-header
	process
	numlist
	new-mark already-mark
	seen-mark seen-list
	(if (elmo-pop3-folder-use-uidl-internal folder)
	    (elmo-pop3-folder-location-alist-internal folder)))))))

(defun elmo-pop3-sort-overview-by-original-number (overview loc-alist)
  (if loc-alist
      (sort overview
	    (lambda (ent1 ent2)
	      (< (elmo-pop3-uidl-to-number
		  (cdr (assq (elmo-msgdb-overview-entity-get-number ent1)
			     loc-alist)))
		 (elmo-pop3-uidl-to-number
		  (cdr (assq (elmo-msgdb-overview-entity-get-number ent2)
			     loc-alist))))))
    overview))

(defun elmo-pop3-sort-msgdb-by-original-number (folder msgdb)
  (message "Sorting...")
  (let ((overview (elmo-msgdb-get-overview msgdb)))
    (current-buffer)
    (setq overview (elmo-pop3-sort-overview-by-original-number
		    overview
		    (elmo-pop3-folder-location-alist-internal folder)))
    (message "Sorting...done")
    (list overview (nth 1 msgdb)(nth 2 msgdb)(nth 3 msgdb))))

(defun elmo-pop3-uidl-to-number (uidl)
  (string-to-number (elmo-get-hash-val uidl
				       elmo-pop3-uidl-number-hash)))

(defun elmo-pop3-number-to-uidl (number)
  (elmo-get-hash-val (format "#%d" number)
		     elmo-pop3-number-uidl-hash))

(defun elmo-pop3-number-to-size (number)
  (elmo-get-hash-val (format "#%d" number)
		     elmo-pop3-size-hash))

(defun elmo-pop3-msgdb-create-by-header (process numlist
						 new-mark already-mark
						 seen-mark
						 seen-list
						 loc-alist)
  (let ((tmp-buffer (get-buffer-create " *ELMO Overview TMP*")))
    (with-current-buffer (process-buffer process)
      (if loc-alist ; use uidl.
	  (setq numlist
		(delq
		 nil
		 (mapcar
		  (lambda (number)
		    (elmo-pop3-uidl-to-number (cdr (assq number loc-alist))))
		  numlist))))
      (elmo-pop3-retrieve-headers (process-buffer process)
				  tmp-buffer process numlist)
      (prog1
	  (elmo-pop3-msgdb-create-message
	   tmp-buffer
	   process
	   (length numlist)
	   numlist
	   new-mark already-mark seen-mark seen-list loc-alist)
	(kill-buffer tmp-buffer)))))

(defun elmo-pop3-msgdb-create-message (buffer
				       process
				       num
				       numlist new-mark already-mark
				       seen-mark
				       seen-list
				       loc-alist)
  (save-excursion
    (let (beg overview number-alist mark-alist
	      entity i number message-id gmark seen size)
      (set-buffer buffer)
      (elmo-set-buffer-multibyte default-enable-multibyte-characters)
      (goto-char (point-min))
      (setq i 0)
      (message "Creating msgdb...")
      (while (not (eobp))
	(setq beg (save-excursion (forward-line 1) (point)))
	(elmo-pop3-next-result-arrived-p)
	(save-excursion
	  (forward-line -1)
	  (save-restriction
	    (narrow-to-region beg (point))
	    (setq entity
		  (elmo-msgdb-create-overview-from-buffer
		   (car numlist)))
	    (setq numlist (cdr numlist))
	    (when entity
	      (setq overview
		    (elmo-msgdb-append-element
		     overview entity))
	      (with-current-buffer (process-buffer process)
		(elmo-msgdb-overview-entity-set-size
		 entity
		 (string-to-number
		  (elmo-pop3-number-to-size
		   (elmo-msgdb-overview-entity-get-number entity))))
		(if (setq number
			  (car
			   (rassoc
			    (elmo-pop3-number-to-uidl
			     (elmo-msgdb-overview-entity-get-number entity))
			    loc-alist)))
		    (elmo-msgdb-overview-entity-set-number entity number)))
	      (setq number-alist
		    (elmo-msgdb-number-add
		     number-alist
		     (elmo-msgdb-overview-entity-get-number entity)
		     (car entity)))
	      (setq message-id (car entity))
	      (setq seen (member message-id seen-list))
	      (if (setq gmark (or (elmo-msgdb-global-mark-get message-id)
				  (if (elmo-file-cache-status
				       (elmo-file-cache-get message-id))
				      (if seen
					  nil
					already-mark)
				    (if seen
					(if elmo-pop3-use-cache
					    seen-mark)
				      new-mark))))
		  (setq mark-alist
			(elmo-msgdb-mark-append
			 mark-alist
			 (elmo-msgdb-overview-entity-get-number entity)
			 gmark))))))
	(when (> num elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (if (or (zerop (% i 5)) (= i num))
	      (elmo-display-progress
	       'elmo-pop3-msgdb-create-message "Creating msgdb..."
	       (/ (* i 100) num)))))
      (list overview number-alist mark-alist))))

(defun elmo-pop3-read-body (process outbuf)
  (with-current-buffer (process-buffer process)
    (let ((start elmo-pop3-read-point)
	  end)
      (goto-char start)
      (while (not (re-search-forward "^\\.\r?\n" nil t))
	(accept-process-output process)
	(goto-char start))
      (setq end (point))
      (elmo-pop3-unlock)
      (with-current-buffer outbuf
	(erase-buffer)
	(insert-buffer-substring (process-buffer process) start (- end 3))))))

(luna-define-method elmo-folder-open-internal ((folder elmo-pop3-folder))
  (if (and (not elmo-inhibit-number-mapping)
	   (elmo-pop3-folder-use-uidl-internal folder))
      (elmo-pop3-folder-set-location-alist-internal
       folder (elmo-msgdb-location-load (elmo-folder-msgdb-path folder)))))

(luna-define-method elmo-folder-commit :after ((folder elmo-pop3-folder))
  (when (elmo-folder-persistent-p folder)
    (elmo-msgdb-location-save (elmo-folder-msgdb-path folder)
			      (elmo-pop3-folder-location-alist-internal
			       folder))))

(luna-define-method elmo-folder-close-internal ((folder elmo-pop3-folder))
  (elmo-folder-check folder))

(luna-define-method elmo-message-fetch-plugged ((folder elmo-pop3-folder)
						number strategy
						&optional section
						outbuf unseen)
  (let* ((loc-alist (elmo-pop3-folder-location-alist-internal folder))
	 (process (elmo-network-session-process-internal
		   (elmo-pop3-get-session folder)))
	size  response errmsg msg)
    (with-current-buffer (process-buffer process)
      (if loc-alist
	  (setq number (elmo-pop3-uidl-to-number
			(cdr (assq number loc-alist)))))
      (setq size (string-to-number
		  (elmo-pop3-number-to-size number)))
      (when number
	(elmo-pop3-send-command process
				(format "retr %s" number))
	(unless elmo-inhibit-display-retrieval-progress
	  (setq elmo-pop3-total-size size)
	  (elmo-display-progress
	   'elmo-pop3-display-retrieval-progress
	   (format "Retrieving (0/%d bytes)..." elmo-pop3-total-size)
	   0))
	(unwind-protect
	    (progn
	      (when (null (setq response (elmo-pop3-read-response
					  process t 'keep-lock)))
		(error "Fetching message failed"))
	      (setq response (elmo-pop3-read-body process outbuf)))
	  (setq elmo-pop3-total-size nil))
	(unless elmo-inhibit-display-retrieval-progress
	  (elmo-display-progress
	   'elmo-display-retrieval-progress "" 100)  ; remove progress bar.
	  (message "Retrieving...done."))
	(set-buffer outbuf)
	(goto-char (point-min))
	(while (re-search-forward "^\\." nil t)
	  (replace-match "")
	  (forward-line))
	response))))

(defun elmo-pop3-delete-msg (process number loc-alist)
  (with-current-buffer (process-buffer process)
    (let (response errmsg msg)
      (if loc-alist
	  (setq number (elmo-pop3-uidl-to-number
			(cdr (assq number loc-alist)))))
      (if number
	  (progn
	    (elmo-pop3-send-command process
				    (format "dele %s" number))
	    (when (null (setq response (elmo-pop3-read-response
					process t)))
	      (error "Deleting message failed")))
	(error "Deleting message failed")))))

(luna-define-method elmo-folder-delete-messages ((folder elmo-pop3-folder)
						      msgs)
  (let ((loc-alist (elmo-pop3-folder-location-alist-internal folder))
	(process (elmo-network-session-process-internal
		  (elmo-pop3-get-session folder))))
    (mapcar '(lambda (msg) (elmo-pop3-delete-msg
			    process msg loc-alist))
	    msgs)))

(luna-define-method elmo-message-use-cache-p ((folder elmo-pop3-folder) number)
  elmo-pop3-use-cache)

(luna-define-method elmo-folder-persistent-p ((folder elmo-pop3-folder))
  (and (elmo-folder-persistent-internal folder)
       (elmo-pop3-folder-use-uidl-internal folder)))

(luna-define-method elmo-folder-check ((folder elmo-pop3-folder))
  (if (elmo-folder-plugged-p folder)
      (let ((session (elmo-pop3-get-session folder 'if-exists)))
	(when (and session
		   (not (elmo-pop3-locked-p
			 (elmo-network-session-process-internal session))))
	  (elmo-pop3-folder-set-location-alist-internal folder nil)
	  (elmo-network-close-session session)))))

(require 'product)
(product-provide (provide 'elmo-pop3) (require 'elmo-version))

;;; elmo-pop3.el ends here
