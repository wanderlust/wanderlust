;;; elmo-pop3.el -- POP3 Interface for ELMO.

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

;;; Code:
;; 

(require 'elmo-msgdb)
(eval-when-compile
  (require 'elmo-util)
  (condition-case nil
      (progn
	(require 'starttls)
	(require 'sasl))
    (error))
  (defun-maybe md5 (a))
  (defun-maybe sasl-digest-md5-digest-response 
    (digest-challenge username passwd serv-type host &optional realm))  
  (defun-maybe sasl-scram-md5-client-msg-1
    (authenticate-id &optional authorize-id))
  (defun-maybe sasl-scram-md5-client-msg-2
    (server-msg-1 client-msg-1 salted-pass))
  (defun-maybe sasl-scram-md5-make-salted-pass
    (server-msg-1 passphrase))
  (defun-maybe sasl-scram-md5-authenticate-server
    (server-msg-1 server-msg-2 client-msg-1 salted-pass))
  (defun-maybe starttls-negotiate (a)))
(condition-case nil
    (progn
      (require 'sasl))
  (error))

(defvar elmo-pop3-use-uidl t
  "*If non-nil, use UIDL.")

(defvar elmo-pop3-exists-exactly t)
(defvar elmo-pop3-read-point nil)
(defvar elmo-pop3-connection-cache nil
  "Cache of pop3 connection.")

;; buffer-local
(defvar elmo-pop3-number-uidl-hash nil) ; number -> uidl
(defvar elmo-pop3-uidl-number-hash nil) ; uidl -> number
(defvar elmo-pop3-size-hash nil) ; number -> size
(defvar elmo-pop3-uidl-done nil)
(defvar elmo-pop3-list-done nil)

(defmacro elmo-pop3-connection-get-process (connection)
  (` (nth 1 (, connection))))

(defmacro elmo-pop3-connection-get-buffer (connection)
  (` (nth 0 (, connection))))

(defun elmo-pop3-close-connection (connection &optional process buffer)
  (and (or connection process)
       (save-excursion
	 (let ((buffer  (or buffer
			    (elmo-pop3-connection-get-buffer connection)))
	       (process (or process 
			    (elmo-pop3-connection-get-process connection))))
	   (elmo-pop3-send-command buffer process "quit")
	   (when (null (elmo-pop3-read-response buffer process t))
	     (error "POP error: QUIT failed"))
	   (if buffer (kill-buffer buffer))
	   (if process (delete-process process))))))

(defun elmo-pop3-flush-connection ()
  (interactive)
  (let ((cache elmo-pop3-connection-cache)
	buffer process proc-stat)
    (while cache
      (setq buffer (car (cdr (car cache))))
      (setq process (car (cdr (cdr (car cache)))))
      (if (and process
	       (not (or (eq (setq proc-stat 
				  (process-status process)) 
			    'closed)
			(eq proc-stat 'exit))))
	  (condition-case ()
	      (elmo-pop3-close-connection nil process buffer)
	    (error)))
      (setq cache (cdr cache)))
    (setq elmo-pop3-connection-cache nil)))

(defun elmo-pop3-get-connection (spec &optional if-exists)
  "Return opened POP3 connection for SPEC."
  (let* ((user   (elmo-pop3-spec-username spec))
	 (server (elmo-pop3-spec-hostname spec))
	 (port   (elmo-pop3-spec-port spec))
	 (auth   (elmo-pop3-spec-auth spec))
	 (ssl    (elmo-pop3-spec-ssl spec))
	 (user-at-host (format "%s@%s" user server))
	 entry connection result buffer process proc-stat response
	 user-at-host-on-port)
    (if (not (elmo-plugged-p server port))
	(error "Unplugged"))
    (setq user-at-host-on-port
	  (concat user-at-host ":" (int-to-string port)
		  (if (eq ssl 'starttls) "!!" (if ssl "!"))))
    (setq entry (assoc user-at-host-on-port elmo-pop3-connection-cache))
    (if (and entry
	     (memq (setq proc-stat
			 (process-status (cadr (cdr entry))))
		   '(closed exit)))
	;; connection is closed...
	(let ((buffer (car (cdr entry))))
	  (if buffer (kill-buffer buffer))
	  (setq elmo-pop3-connection-cache
		(delete entry elmo-pop3-connection-cache))
	  (setq entry nil)))
    (if entry
	(cdr entry)
      (unless if-exists
	(setq result
	      (elmo-pop3-open-connection
	       server user port auth
	       (elmo-get-passwd user-at-host) ssl))
	(if (null result)
	    (error "Connection failed"))
	(setq buffer (car result))
	(setq process (cdr result))
	(when (and process (null buffer))
	  (elmo-remove-passwd user-at-host)
	  (delete-process process)
	  (error "Login failed"))
	;; add a new entry to the top of the cache.
	(setq elmo-pop3-connection-cache
	      (cons
	       (cons user-at-host-on-port
		     (setq connection (list buffer process)))
	       elmo-pop3-connection-cache))
	;; initialization of list
	(with-current-buffer buffer
	  (make-variable-buffer-local 'elmo-pop3-uidl-number-hash)
	  (make-variable-buffer-local 'elmo-pop3-number-uidl-hash)
	  (make-variable-buffer-local 'elmo-pop3-uidl-done)
	  (make-variable-buffer-local 'elmo-pop3-size-hash)
	  (make-variable-buffer-local 'elmo-pop3-list-done)
	  (setq elmo-pop3-size-hash (make-vector 31 0))
	  ;; To get obarray of uidl and size
	  ;; List
	  (elmo-pop3-send-command buffer process "list")
	  (if (null (elmo-pop3-read-response buffer process))
	      (error "POP List folder failed"))
	  (if (null (setq response
			  (elmo-pop3-read-contents buffer process)))
	      (error "POP List folder failed"))
	  ;; POP server always returns a sequence of serial numbers.
	  (elmo-pop3-parse-list-response response)
	  ;; UIDL
	  (when elmo-pop3-use-uidl
	    (setq elmo-pop3-uidl-number-hash (make-vector 31 0))
	    (setq elmo-pop3-number-uidl-hash (make-vector 31 0))
	    ;; UIDL
	    (elmo-pop3-send-command buffer process "uidl")
	    (unless (elmo-pop3-read-response buffer process)
	      (error "UIDL failed."))
	    (unless (setq response (elmo-pop3-read-contents buffer process))
	      (error "UIDL failed."))
	    (elmo-pop3-parse-uidl-response response)
	    elmo-pop3-uidl-done))
	connection))))

(defun elmo-pop3-send-command (buffer process command &optional no-erase)
  (with-current-buffer buffer
    (unless no-erase
      (erase-buffer))
    (goto-char (point-min))
    (setq elmo-pop3-read-point (point))
    (process-send-string process command)
    (process-send-string process "\r\n")))

(defun elmo-pop3-read-response (buffer process &optional not-command)
  (save-excursion
    (set-buffer buffer)
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
      return-value)))

(defun elmo-pop3-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)))

(defun elmo-pop3-open-connection (server user port auth passphrase ssl)
  "Open POP3 connection to SERVER on PORT for USER.
Return a cons cell of (session-buffer . process).
Return nil if connection failed."
  (let ((process nil)
	(host server)
	process-buffer ret-val response capability)
    (catch 'done
      (as-binary-process
       (setq process-buffer
	     (get-buffer-create (format " *POP session to %s:%d" host port)))
       (save-excursion
	 (set-buffer process-buffer)
	 (elmo-set-buffer-multibyte nil)       	 
	 (erase-buffer))
       (setq process
	     (elmo-open-network-stream "POP" process-buffer host port ssl))
       (and (null process) (throw 'done nil))
       (set-process-filter process 'elmo-pop3-process-filter)
       ;; flush connections when exiting...
       (save-excursion
	 (set-buffer process-buffer)
	 (make-local-variable 'elmo-pop3-read-point)
	 (setq elmo-pop3-read-point (point-min))
	 (when (null (setq response
			   (elmo-pop3-read-response process-buffer process t)))
	   (setq ret-val (cons nil process))
	   (throw 'done nil))
	 (when (eq ssl 'starttls)
	   (elmo-pop3-send-command process-buffer process "stls")
	   (string-match "^\+OK" 
			 (elmo-pop3-read-response 
			  process-buffer process))
	   (starttls-negotiate process))
	 (cond ((string= auth "apop")
		;; try only APOP
		(if (string-match "^\+OK .*\\(<[^\>]+>\\)" response)
		    ;; good, APOP ready server
		    (progn
		      (require 'md5)
		      (elmo-pop3-send-command  
		       process-buffer process 
		       (format "apop %s %s" 
			       user
			       (md5 
				(concat (match-string 1 response)
					    passphrase)))))
		  ;; otherwise, fail (only APOP authentication)
		  (setq ret-val (cons nil process))
		  (throw 'done nil)))
	       ((string= auth "cram-md5")
		(elmo-pop3-send-command  
		 process-buffer process "auth cram-md5")
		(when (null (setq response
				  (elmo-pop3-read-response
				   process-buffer process t)))
		  (setq ret-val (cons nil process))
		  (throw 'done nil))
		(elmo-pop3-send-command
		 process-buffer process
		 (elmo-base64-encode-string
		  (sasl-cram-md5 user passphrase 
				 (elmo-base64-decode-string
				  (cadr (split-string response " ")))))))
	       ((string= auth "digest-md5")
		(elmo-pop3-send-command  
		 process-buffer process "auth digest-md5")
		(when (null (setq response
				  (elmo-pop3-read-response
				   process-buffer process t)))
		  (setq ret-val (cons nil process))
		  (throw 'done nil))
		(elmo-pop3-send-command
		 process-buffer process
		 (elmo-base64-encode-string
		  (sasl-digest-md5-digest-response
		   (elmo-base64-decode-string
		    (cadr (split-string response " ")))
		   user passphrase "pop" host)
		  'no-line-break))
		(when (null (setq response
				  (elmo-pop3-read-response
				   process-buffer process t)))
		  (setq ret-val (cons nil process))
		  (throw 'done nil))
		(elmo-pop3-send-command process-buffer process ""))
	       ((string= auth "scram-md5")
		(let (server-msg-1 server-msg-2 client-msg-1 client-msg-2
				   salted-pass)
		  (elmo-pop3-send-command
		   process-buffer process
		   (format "auth scram-md5 %s"
			   (elmo-base64-encode-string
			    (setq client-msg-1
				  (sasl-scram-md5-client-msg-1 user)))))
		  (when (null (setq response
				    (elmo-pop3-read-response
				     process-buffer process t)))
		    (setq ret-val (cons nil process))
		    (throw 'done nil))
		  (setq server-msg-1
			(elmo-base64-decode-string
			 (cadr (split-string response " "))))
		  (elmo-pop3-send-command
		   process-buffer process
		   (elmo-base64-encode-string
		    (sasl-scram-md5-client-msg-2
		     server-msg-1
		     client-msg-1
		     (setq salted-pass
			   (sasl-scram-md5-make-salted-pass 
			    server-msg-1 passphrase)))))
		  (when (null (setq response
				    (elmo-pop3-read-response
				     process-buffer process t)))
		    (setq ret-val (cons nil process))
		    (throw 'done nil))
		  (setq server-msg-2
			(elmo-base64-decode-string
			 (cadr (split-string response " "))))
		  (if (null (sasl-scram-md5-authenticate-server
			     server-msg-1
			     server-msg-2
			     client-msg-1
			     salted-pass))
		      (throw 'done nil))
		  (elmo-pop3-send-command
		   process-buffer process "")))
	       (t
		;; try USER/PASS
		(elmo-pop3-send-command  process-buffer process 
					 (format "user %s" user))
		(when (null (elmo-pop3-read-response process-buffer process t))
		  (setq ret-val (cons nil process))
		  (throw 'done nil))
		(elmo-pop3-send-command  process-buffer process 
					 (format "pass %s" passphrase))))
	 ;; read PASS or APOP response
	 (when (null (elmo-pop3-read-response process-buffer process t))
	   (setq ret-val (cons nil process))
	   (throw 'done nil))
	 (setq ret-val (cons process-buffer process)))))
    ret-val))

(defun elmo-pop3-read-contents (buffer process)
  (save-excursion
    (set-buffer buffer)
    (let ((case-fold-search nil)
	  match-end)
      (goto-char elmo-pop3-read-point)
      (while (not (re-search-forward "^\\.\r\n" nil t))
	(accept-process-output process)
	(goto-char elmo-pop3-read-point))
      (setq match-end (point))
      (elmo-delete-cr
       (buffer-substring elmo-pop3-read-point 
			 (- match-end 3))))))

;; dummy functions
(defun elmo-pop3-list-folders (spec &optional hierarchy) nil)
(defun elmo-pop3-append-msg (spec string) nil nil)
(defun elmo-pop3-folder-creatable-p (spec) nil)
(defun elmo-pop3-create-folder (spec) nil)

(defun elmo-pop3-folder-exists-p (spec)
  (if (and elmo-pop3-exists-exactly
	   (elmo-pop3-plugged-p spec))
      (save-excursion
	(let (elmo-auto-change-plugged) ;;don't change plug status.
	  (condition-case nil
	      (prog1
		  (elmo-pop3-get-connection spec)
		(elmo-pop3-close-connection
		 (elmo-pop3-get-connection spec 'if-exists)))
	    (error nil))))
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
	number list size)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9]+\\)[\t ]\\([0-9]+\\)$" nil t)
	(setq list
	      (cons
	       (string-to-int (setq number (elmo-match-buffer 1)))
	       list))
	(setq size (elmo-match-buffer 2))
	(with-current-buffer buffer
	  (elmo-set-hash-val (concat "#" number)
			     size
			     elmo-pop3-size-hash)))
      (with-current-buffer buffer (setq elmo-pop3-list-done t))
      (nreverse list))))

(defun elmo-pop3-list-location (spec)
  (with-current-buffer (elmo-pop3-connection-get-buffer
			(elmo-pop3-get-connection spec))
    (let (list)
      (if elmo-pop3-uidl-done
	  (progn
	    (mapatoms
	     (lambda (atom)
	       (setq list (cons (symbol-name atom) list)))
	     elmo-pop3-uidl-number-hash)
	    (nreverse list))
	(error "POP3: Error in UIDL")))))

(defun elmo-pop3-list-by-uidl-subr (spec &optional nonsort)
  (let ((flist (elmo-list-folder-by-location
		spec
		(elmo-pop3-list-location spec))))
    (if nonsort
	(cons (elmo-max-of-list flist) (length flist))
      (sort flist '<))))

(defun elmo-pop3-list-by-list (spec)
  (with-current-buffer (elmo-pop3-connection-get-buffer
			(elmo-pop3-get-connection spec))
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

(defun elmo-pop3-list-folder (spec)
  (let ((killed (and elmo-use-killed-list
		     (elmo-msgdb-killed-list-load
		      (elmo-msgdb-expand-path nil spec))))
	numbers)
    (elmo-pop3-commit spec)
    (setq numbers (if elmo-pop3-use-uidl
		      (progn
			(elmo-pop3-list-by-uidl-subr spec))
		    (elmo-pop3-list-by-list spec)))
    (if killed
	(delq nil
	      (mapcar (lambda (number)
			(unless (memq number killed) number))
		      numbers))
      numbers)))

(defun elmo-pop3-max-of-folder (spec)
  (elmo-pop3-commit spec)
  (if elmo-pop3-use-uidl
      (elmo-pop3-list-by-uidl-subr spec 'nonsort)
    (let* ((connection (elmo-pop3-get-connection spec))
	   (buffer  (nth 0 connection))
	   (process (nth 1 connection))
	   (total 0)
	   response)
      (with-current-buffer buffer
	(elmo-pop3-send-command buffer process "STAT")
	(setq response (elmo-pop3-read-response buffer process))
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
	(elmo-pop3-send-command buffer process (format
						"top %s 0" (car articles))
				'no-erase)
	;; (accept-process-output process 1)
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
	    ;(accept-process-output process)
	    (discard-input))))
      ;; Remove all "\r"'s.
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
	(replace-match "\n"))
      (copy-to-buffer tobuffer (point-min) (point-max)))))

(defalias 'elmo-pop3-msgdb-create 'elmo-pop3-msgdb-create-as-numlist)

(defun elmo-pop3-msgdb-create-as-numlist (spec numlist new-mark
					       already-mark seen-mark
					       important-mark seen-list
					       &optional msgdb)
  (when numlist
    (let* ((connection (elmo-pop3-get-connection spec))
	   (buffer (elmo-pop3-connection-get-buffer connection))
	   (process (elmo-pop3-connection-get-process connection))
	   loc-alist)
      (if elmo-pop3-use-uidl
	  (setq loc-alist (if msgdb (elmo-msgdb-get-location msgdb)
			    (elmo-msgdb-location-load
			     (elmo-msgdb-expand-path nil spec)))))
      (elmo-pop3-msgdb-create-by-header buffer process numlist
					new-mark already-mark 
					seen-mark seen-list
					loc-alist))))

(defun elmo-pop3-uidl-to-number (uidl)
  (string-to-number (elmo-get-hash-val uidl
				       elmo-pop3-uidl-number-hash)))

(defun elmo-pop3-number-to-uidl (number)
  (elmo-get-hash-val (format "#%d" number)
		     elmo-pop3-number-uidl-hash))

(defun elmo-pop3-number-to-size (number)
  (elmo-get-hash-val (format "#%d" number)
		     elmo-pop3-size-hash))

(defun elmo-pop3-msgdb-create-by-header (buffer process numlist
						new-mark already-mark 
						seen-mark
						seen-list
						loc-alist)
  (let ((tmp-buffer (get-buffer-create " *ELMO Overview TMP*")))
    (with-current-buffer buffer
      (if loc-alist ; use uidl.
	  (setq numlist
		(delq 
		 nil
		 (mapcar 
		  (lambda (number)
		    (elmo-pop3-uidl-to-number (cdr (assq number loc-alist))))
		  numlist))))
      (elmo-pop3-retrieve-headers buffer tmp-buffer process numlist)
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
				  (if (elmo-cache-exists-p 
				       message-id) ; XXX
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
      (list overview number-alist mark-alist loc-alist))))

(defun elmo-pop3-read-body (buffer process outbuf)
  (with-current-buffer buffer
    (let ((start elmo-pop3-read-point)
	  end)
      (goto-char start)
      (while (not (re-search-forward "^\\.\r?\n" nil t))
	(accept-process-output process)
	(goto-char start))
      (setq end (point))
      (with-current-buffer outbuf
	(erase-buffer)
	(insert-buffer-substring buffer start (- end 3))
	(elmo-delete-cr-get-content-type)))))

(defun elmo-pop3-read-msg (spec number outbuf &optional msgdb)
  (let* ((loc-alist (if elmo-pop3-use-uidl
			(if msgdb
			    (elmo-msgdb-get-location msgdb)
			  (elmo-msgdb-location-load
			   (elmo-msgdb-expand-path nil spec)))))
	 (connection (elmo-pop3-get-connection spec))
	 (buffer  (elmo-pop3-connection-get-buffer connection))
	 (process (elmo-pop3-connection-get-process connection))
	 response errmsg msg)
    (with-current-buffer buffer
      (if loc-alist
	  (setq number (elmo-pop3-uidl-to-number
			(cdr (assq number loc-alist)))))
      (when number
	(elmo-pop3-send-command buffer process 
				(format "retr %s" number))
	(when (null (setq response (elmo-pop3-read-response
				    buffer process t)))
	  (error "Fetching message failed"))
	(setq response (elmo-pop3-read-body buffer process outbuf))
	(set-buffer outbuf)
	(goto-char (point-min))
	(while (re-search-forward "^\\." nil t)
	  (replace-match "")
	  (forward-line))
	response))))

(defun elmo-pop3-delete-msg (buffer process number loc-alist)
  (with-current-buffer buffer
    (let (response errmsg msg)
      (if loc-alist
	  (setq number (elmo-pop3-uidl-to-number
			(cdr (assq number loc-alist)))))
      (if number
	  (progn
	    (elmo-pop3-send-command buffer process 
				    (format "dele %s" number))
	    (when (null (setq response (elmo-pop3-read-response
					buffer process t)))
	      (error "Deleting message failed")))
	(error "Deleting message failed")))))
	

(defun elmo-pop3-delete-msgs (spec msgs &optional msgdb)
  (let* ((loc-alist (if elmo-pop3-use-uidl
			(if msgdb
			    (elmo-msgdb-get-location msgdb)
			  (elmo-msgdb-location-load
			   (elmo-msgdb-expand-path nil spec)))))
	 (connection (elmo-pop3-get-connection spec))
	 (buffer  (elmo-pop3-connection-get-buffer connection))
	 (process (elmo-pop3-connection-get-process connection)))
    (mapcar '(lambda (msg) (elmo-pop3-delete-msg 
			    buffer process msg loc-alist))
	    msgs)))

(defun elmo-pop3-search (spec condition &optional numlist)
  (error "Searching in pop3 folder is not implemented yet"))

(defun elmo-pop3-use-cache-p (spec number)
  elmo-pop3-use-cache)

(defun elmo-pop3-local-file-p (spec number)
  nil)

(defun elmo-pop3-port-label (spec)
  (concat "pop3"
	  (if (elmo-pop3-spec-ssl spec) "!ssl" "")))

(defsubst elmo-pop3-portinfo (spec)
  (list (elmo-pop3-spec-hostname spec) 
	(elmo-pop3-spec-port spec)))

(defun elmo-pop3-plugged-p (spec)
  (apply 'elmo-plugged-p
	 (append (elmo-pop3-portinfo spec)
		 (list nil (quote (elmo-pop3-port-label spec))))))

(defun elmo-pop3-set-plugged (spec plugged add)
  (apply 'elmo-set-plugged plugged
	 (append (elmo-pop3-portinfo spec)
		 (list nil nil (quote (elmo-pop3-port-label spec)) add))))

(defalias 'elmo-pop3-sync-number-alist 
  'elmo-generic-sync-number-alist)
(defalias 'elmo-pop3-list-folder-unread 
  'elmo-generic-list-folder-unread)
(defalias 'elmo-pop3-list-folder-important
  'elmo-generic-list-folder-important)

(defun elmo-pop3-commit (spec)
  (if (elmo-pop3-plugged-p spec)
      (elmo-pop3-close-connection
       (elmo-pop3-get-connection spec 'if-exists))))

(provide 'elmo-pop3)

;;; elmo-pop3.el ends here
