;;; elmo-pop3.el -- POP3 Interface for ELMO.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news
;; Time-stamp: <00/04/28 10:28:08 teranisi>

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

(defvar elmo-pop3-exists-exactly t)
(defvar elmo-pop3-read-point nil)
(defvar elmo-pop3-connection-cache nil
  "Cache of pop3 connection.")

(defun elmo-pop3-close-connection (connection &optional process buffer)
  (save-excursion
    (let* ((buffer  (or buffer (nth 0 connection)))
	   (process (or process (nth 1 connection))))
      (elmo-pop3-send-command buffer process "quit")
      (when (null (elmo-pop3-read-response buffer process t))
	(error "POP error: QUIT failed")))))

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
      (if buffer (kill-buffer buffer))
      ;;(setq process (car (cdr (cdr (car cache)))))
      (if process (delete-process process))
      (setq cache (cdr cache)))
    (setq elmo-pop3-connection-cache nil)))

(defun elmo-pop3-get-connection (spec)
  (let* ((user   (elmo-pop3-spec-username spec))
	 (server (elmo-pop3-spec-hostname spec))
	 (port   (elmo-pop3-spec-port spec))
	 (auth   (elmo-pop3-spec-auth spec))
	 (ssl    (elmo-pop3-spec-ssl spec))
	 (user-at-host (format "%s@%s" user server))
	 ret-val result buffer process errmsg proc-stat
	 user-at-host-on-port)
    (if (not (elmo-plugged-p server port))
	(error "Unplugged"))
    (setq user-at-host-on-port 
	  (concat user-at-host ":" (int-to-string port)
		  (if (eq ssl 'starttls) "!!" (if ssl "!"))))
    (setq ret-val (assoc user-at-host-on-port elmo-pop3-connection-cache))
    (if (and ret-val 
	     (or (eq (setq proc-stat 
			   (process-status (cadr (cdr ret-val)))) 
		     'closed)
		 (eq proc-stat 'exit)))
	;; connection is closed...
	(progn
	  (kill-buffer (car (cdr ret-val)))
	  (setq elmo-pop3-connection-cache 
		(delete ret-val elmo-pop3-connection-cache))
	  (setq ret-val nil)
	  ))
    (if ret-val
	(cdr ret-val)
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
	(error "Login failed")
	)
      (setq elmo-pop3-connection-cache 
	    (append elmo-pop3-connection-cache 
		    (list 
		     (cons user-at-host-on-port
			   (setq ret-val (list buffer process))))))
      ret-val)))

(defun elmo-pop3-send-command (buffer process command)
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (goto-char (point-min))
    (setq elmo-pop3-read-point (point))
    (process-send-string process command)
    (process-send-string process "\r\n")))

(defun elmo-pop3-send-command-no-erase (buffer process command)
  (save-excursion
    (set-buffer buffer)
    ;(erase-buffer)
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
		      response-string
		      )))
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
		   process-buffer process "") ))
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
		(elmo-pop3-flush-connection))
	    (error nil))))
    t))

(defun elmo-pop3-parse-list-response (string)
  (save-excursion
    (let ((tmp-buffer (get-buffer-create " *ELMO PARSE TMP*"))
	  ret-val)
      (set-buffer tmp-buffer)
      (let ((case-fold-search t))
	(erase-buffer)
	(insert string)
	(goto-char (point-min))
	(while (re-search-forward "^\\([0-9]*\\)[\t ].*$" nil t)
	  (setq ret-val
		(cons
		 (string-to-int
		  (elmo-match-buffer 1))
		 ret-val)))
	(kill-buffer tmp-buffer)
	(nreverse ret-val)))))

(defun elmo-pop3-list-folder (spec)
  (save-excursion
    (elmo-pop3-flush-connection)
    (let* ((connection (elmo-pop3-get-connection spec))
	   (buffer  (nth 0 connection))
	   (process (nth 1 connection))
	   response errmsg ret-val)
      (elmo-pop3-send-command buffer process "list")
      (if (null (elmo-pop3-read-response buffer process))
	  (error "POP List folder failed"))
      (if (null (setq response (elmo-pop3-read-contents buffer process)))
	  (error "POP List folder failed"))
      ;; POP server always returns a sequence of serial numbers.
      (elmo-pop3-parse-list-response response))))

(defun elmo-pop3-max-of-folder (spec)
  (save-excursion
    (elmo-pop3-flush-connection)
    (let* ((connection (elmo-pop3-get-connection spec))
	   (buffer  (nth 0 connection))
	   (process (nth 1 connection))
	   (total 0)
	   response)
      (elmo-pop3-send-command buffer process "STAT")
      (setq response (elmo-pop3-read-response buffer process))
      ;; response: "^\+OK 2 7570$"
      (if (not (string-match "^\+OK[ \t]*\\([0-9]*\\)" response))
	  (error "POP STAT command failed")
	(setq total
	      (string-to-int
	       (substring response (match-beginning 1)(match-end 1 ))))
	(cons total total)))))

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
	(elmo-pop3-send-command-no-erase
	 buffer
	 process
	 (format "top %s 0" (car articles))
	 )
	; (accept-process-output process 1)
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
	    (discard-input)
	    )))
      ;; Remove all "\r"'s.
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
	(replace-match "\n"))
      (copy-to-buffer tobuffer (point-min) (point-max))
      ;(elmo-pop3-close-connection nil process buffer) ; close connection
      )))

(defalias 'elmo-pop3-msgdb-create 'elmo-pop3-msgdb-create-as-numlist)
(defun elmo-pop3-msgdb-create-as-numlist (spec numlist new-mark
					       already-mark seen-mark
					       important-mark seen-list)
  (when numlist
    (let* ((connection (elmo-pop3-get-connection spec))
	   (buffer (nth 0 connection))
	   (process (nth 1 connection))
	   response errmsg ret-val)
      (elmo-pop3-msgdb-create-by-header buffer process numlist
					new-mark already-mark 
					seen-mark seen-list))))

(defun elmo-pop3-msgdb-create-by-header (buffer process numlist
						new-mark already-mark 
						seen-mark
						seen-list)
  (let ((tmp-buffer (get-buffer-create " *ELMO Overview TMP*"))
	ret-val)
    (elmo-pop3-retrieve-headers
     buffer tmp-buffer process numlist)
    (setq ret-val
	  (elmo-pop3-msgdb-create-message
	   tmp-buffer 
	   (length numlist)
	   numlist
	   new-mark already-mark seen-mark seen-list))
    (kill-buffer tmp-buffer)
    ret-val))

(defun elmo-pop3-msgdb-create-message (buffer 
				       num numlist new-mark already-mark 
				       seen-mark
				       seen-list)
  (save-excursion
    (let (beg
	  overview number-alist mark-alist
	  entity i number message-id gmark seen)
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
	      (setq number-alist
		    (elmo-msgdb-number-add number-alist
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
			 gmark)))
	      )))
	(when (> num elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (if (or (zerop (% i 5)) (= i num))
	      (elmo-display-progress
	       'elmo-pop3-msgdb-create-message "Creating msgdb..."
	       (/ (* i 100) num)))))
      (list overview number-alist mark-alist))))

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

(defun elmo-pop3-read-msg (spec number outbuf)
  (save-excursion
    (let* ((connection (elmo-pop3-get-connection spec))
	   (buffer  (car connection))
	   (process (cadr connection))
	   (cwf     (caddr connection))	 
	   response errmsg msg)
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
      response)))

(defun elmo-pop3-delete-msg (buffer process number)
  (let (response errmsg msg)
    (elmo-pop3-send-command buffer process 
			    (format "dele %s" number))
    (when (null (setq response (elmo-pop3-read-response
				buffer process t)))
      (error "Deleting message failed"))))

(defun elmo-pop3-delete-msgs (spec msgs)
  (save-excursion
    (let* ((connection (elmo-pop3-get-connection spec))
	   (buffer  (car connection))
	   (process (cadr connection)))
      (mapcar '(lambda (msg) (elmo-pop3-delete-msg 
			      buffer process msg))
	      msgs))))

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
(defalias 'elmo-pop3-commit 'elmo-generic-commit)

(provide 'elmo-pop3)

;;; elmo-pop3.el ends here
