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
  (defun-maybe sasl-cram-md5 (username passphrase challenge))
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

(defvar elmo-pop3-authenticator-alist
  '((user        elmo-pop3-auth-user)
    (apop        elmo-pop3-auth-apop)
    (cram-md5    elmo-pop3-auth-cram-md5)
    (scram-md5   elmo-pop3-auth-scram-md5)
    (digest-md5  elmo-pop3-auth-digest-md5))
  "Definition of authenticators.")

(eval-and-compile
  (luna-define-class elmo-pop3-session (elmo-network-session) ()))

;; buffer-local
(defvar elmo-pop3-read-point nil)
(defvar elmo-pop3-number-uidl-hash nil) ; number -> uidl
(defvar elmo-pop3-uidl-number-hash nil) ; uidl -> number
(defvar elmo-pop3-size-hash nil) ; number -> size
(defvar elmo-pop3-uidl-done nil)
(defvar elmo-pop3-list-done nil)

(defvar elmo-pop3-local-variables '(elmo-pop3-read-point
				    elmo-pop3-uidl-number-hash
				    elmo-pop3-number-uidl-hash
				    elmo-pop3-uidl-done
				    elmo-pop3-size-hash
				    elmo-pop3-list-done))

(luna-define-method elmo-network-close-session ((session elmo-pop3-session))
  (when (elmo-network-session-process-internal session)
    (when (memq (process-status
		 (elmo-network-session-process-internal session))
		'(open run))
      (elmo-pop3-send-command (elmo-network-session-process-internal session)
			      "quit")
      (or (elmo-pop3-read-response
	   (elmo-network-session-process-internal session) t)
	  (error "POP error: QUIT failed")))
    (kill-buffer (process-buffer
		  (elmo-network-session-process-internal session)))
    (delete-process (elmo-network-session-process-internal session))))

(defun elmo-pop3-get-session (spec &optional if-exists)
  (elmo-network-get-session
   'elmo-pop3-session
   "POP3"
   (elmo-pop3-spec-hostname spec)
   (elmo-pop3-spec-port spec)
   (elmo-pop3-spec-username spec)
   (elmo-pop3-spec-auth spec)
   (elmo-pop3-spec-stream-type spec)
   if-exists))

(defun elmo-pop3-send-command (process command &optional no-erase)
  (with-current-buffer (process-buffer process)
    (unless no-erase
      (erase-buffer))
    (goto-char (point-min))
    (setq elmo-pop3-read-point (point))
    (process-send-string process command)
    (process-send-string process "\r\n")))

(defun elmo-pop3-read-response (process &optional not-command)
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
      return-value)))

(defun elmo-pop3-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)))

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
	(require 'md5)
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
    (signal 'elmo-open-error '(elmo-pop-auth-user))))
    
(defun elmo-pop3-auth-cram-md5 (session)
  (let ((process (elmo-network-session-process-internal session))
	response)
    (elmo-pop3-send-command  process "auth cram-md5")
    (or (setq response
	      (elmo-pop3-read-response process t))
	(signal 'elmo-open-error '(elmo-pop-auth-cram-md5)))
    (elmo-pop3-send-command
     process
     (elmo-base64-encode-string
      (sasl-cram-md5 (elmo-network-session-user-internal session)
		     (elmo-get-passwd
		      (elmo-network-session-password-key session))
		     (elmo-base64-decode-string
		      (cadr (split-string response " "))))))
    (or (elmo-pop3-read-response process t)
	(signal 'elmo-authenticate-error
		'(elmo-pop-auth-cram-md5)))))

(defun elmo-pop3-auth-scram-md5 (session)
  (let ((process (elmo-network-session-process-internal session))
	server-msg-1 server-msg-2 client-msg-1 client-msg-2
	salted-pass response)
    (elmo-pop3-send-command
     process
     (format "auth scram-md5 %s"
	     (elmo-base64-encode-string
	      (setq client-msg-1
		    (sasl-scram-md5-client-msg-1
		     (elmo-network-session-user-internal session))))))
    (or (elmo-pop3-read-response process t)
	(signal 'elmo-open-error '(elmo-pop-auth-scram-md5)))
    (setq server-msg-1
	  (elmo-base64-decode-string (cadr (split-string response " "))))
    (elmo-pop3-send-command
     process
     (elmo-base64-encode-string
      (sasl-scram-md5-client-msg-2
       server-msg-1
       client-msg-1
       (setq salted-pass
	     (sasl-scram-md5-make-salted-pass
	      server-msg-1
	      (elmo-get-passwd
	       (elmo-network-session-password-key session)))))))
    (or (setq response (elmo-pop3-read-response process t))
	(signal 'elmo-authenticate-error
		'(elmo-pop-auth-scram-md5)))
    (setq server-msg-2 (elmo-base64-decode-string
			(cadr (split-string response " "))))
    (or (sasl-scram-md5-authenticate-server server-msg-1
					    server-msg-2
					    client-msg-1
					    salted-pass)
	(signal 'elmo-authenticate-error
		'(elmo-pop-auth-scram-md5)))
    (elmo-pop3-send-command process "")
    (or (setq response (elmo-pop3-read-response process t))
	(signal 'elmo-authenticate-error
		'(elmo-pop-auth-scram-md5)))))

(defun elmo-pop3-auth-digest-md5 (session)
  (let ((process (elmo-network-session-process-internal session))
	response)
    (elmo-pop3-send-command process "auth digest-md5")
    (or (setq response
	      (elmo-pop3-read-response process t))
	(signal 'elmo-open-error
		'(elmo-pop-auth-digest-md5)))
    (elmo-pop3-send-command
     process
     (elmo-base64-encode-string
      (sasl-digest-md5-digest-response
       (elmo-base64-decode-string
	(cadr (split-string response " ")))
       (elmo-network-session-user-internal session)
       (elmo-get-passwd
	(elmo-network-session-password-key session))
       "pop"
       (elmo-network-session-host-internal session))
      'no-line-break))
    (or (elmo-pop3-read-response process t)
	(signal 'elmo-authenticate-error
		'(elmo-pop-auth-digest-md5)))
    (elmo-pop3-send-command process "")
    (or (elmo-pop3-read-response process t)
	(signal 'elmo-open-error
		'(elmo-pop-auth-digest-md5)))))

(luna-define-method elmo-network-initialize-session-buffer :after
  ((session elmo-pop3-session) buffer)
  (with-current-buffer buffer
    (mapcar 'make-variable-buffer-local elmo-pop3-local-variables)))

(luna-define-method elmo-network-initialize-session ((session
						      elmo-pop3-session))
  (let ((process (elmo-network-session-process-internal session))
	response mechanism)
    (with-current-buffer (process-buffer process)
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
  (let (authenticator)
    ;; defaults to 'user.
    (unless (elmo-network-session-auth-internal session)
      (elmo-network-session-set-auth-internal session 'user))
    (setq authenticator
	  (nth 1 (assq (elmo-network-session-auth-internal session)
		       elmo-pop3-authenticator-alist)))
    (unless authenticator (error "There's no authenticator for %s"
				 (elmo-network-session-auth-internal session)))
    (funcall authenticator session)))

(luna-define-method elmo-network-setup-session ((session
						 elmo-pop3-session))
  (let ((process (elmo-network-session-process-internal session))
	count response)
    (with-current-buffer (process-buffer process)
      (setq elmo-pop3-size-hash (elmo-make-hash 31))
      ;; To get obarray of uidl and size
      (elmo-pop3-send-command process "list")
      (if (null (elmo-pop3-read-response process))
	  (error "POP LIST command failed"))
      (if (null (setq response
		      (elmo-pop3-read-contents
		       (current-buffer) process)))
	  (error "POP LIST command failed"))
      ;; POP server always returns a sequence of serial numbers.
      (setq count (elmo-pop3-parse-list-response response))
      ;; UIDL
      (when elmo-pop3-use-uidl
	(setq elmo-pop3-uidl-number-hash (elmo-make-hash (* count 2)))
	(setq elmo-pop3-number-uidl-hash (elmo-make-hash (* count 2)))
	;; UIDL
	(elmo-pop3-send-command process "uidl")
	(unless (elmo-pop3-read-response process)
	  (error "POP UIDL failed"))
	(unless (setq response (elmo-pop3-read-contents
				(current-buffer) process))
	  (error "POP UIDL failed"))
	(elmo-pop3-parse-uidl-response response)))))

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
	(let (elmo-auto-change-plugged ; don't change plug status.
	      elmo-pop3-use-uidl       ; No need to use uidl.
	      session)
	  (prog1
	      (setq session (elmo-pop3-get-session spec))
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

(defun elmo-pop3-list-location (spec)
  (with-current-buffer (process-buffer
			(elmo-network-session-process-internal
			 (elmo-pop3-get-session spec)))
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
  (with-current-buffer (process-buffer
			(elmo-network-session-process-internal
			 (elmo-pop3-get-session spec)))
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
		      (elmo-msgdb-expand-path spec))))
	numbers)
    (elmo-pop3-commit spec)
    (setq numbers (if elmo-pop3-use-uidl
		      (progn
			(elmo-pop3-list-by-uidl-subr spec))
		    (elmo-pop3-list-by-list spec)))
    (elmo-living-messages numbers killed)))

(defun elmo-pop3-max-of-folder (spec)
  (elmo-pop3-commit spec)
  (if elmo-pop3-use-uidl
      (elmo-pop3-list-by-uidl-subr spec 'nonsort)
    (let* ((process
	    (elmo-network-session-process-internal
	     (elmo-pop3-get-session spec)))
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

(defalias 'elmo-pop3-msgdb-create 'elmo-pop3-msgdb-create-as-numlist)

(defun elmo-pop3-msgdb-create-as-numlist (spec numlist new-mark
					       already-mark seen-mark
					       important-mark seen-list
					       &optional msgdb)
  (when numlist
    (let ((process (elmo-network-session-process-internal
		    (elmo-pop3-get-session spec)))
	  loc-alist)
      (if elmo-pop3-use-uidl
	  (setq loc-alist (if msgdb (elmo-msgdb-get-location msgdb)
			    (elmo-msgdb-location-load
			     (elmo-msgdb-expand-path spec)))))
      (elmo-pop3-msgdb-create-by-header process numlist
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

(defun elmo-pop3-read-body (process outbuf)
  (with-current-buffer (process-buffer process)
    (let ((start elmo-pop3-read-point)
	  end)
      (goto-char start)
      (while (not (re-search-forward "^\\.\r?\n" nil t))
	(accept-process-output process)
	(goto-char start))
      (setq end (point))
      (with-current-buffer outbuf
	(erase-buffer)
	(insert-buffer-substring (process-buffer process) start (- end 3))
	(elmo-delete-cr-get-content-type)))))

(defun elmo-pop3-read-msg (spec number outbuf &optional msgdb)
  (let* ((loc-alist (if elmo-pop3-use-uidl
			(if msgdb
			    (elmo-msgdb-get-location msgdb)
			  (elmo-msgdb-location-load
			   (elmo-msgdb-expand-path spec)))))
	 (process (elmo-network-session-process-internal
		   (elmo-pop3-get-session spec)))
	 response errmsg msg)
    (with-current-buffer (process-buffer process)
      (if loc-alist
	  (setq number (elmo-pop3-uidl-to-number
			(cdr (assq number loc-alist)))))
      (when number
	(elmo-pop3-send-command process
				(format "retr %s" number))
	(when (null (setq response (elmo-pop3-read-response
				    process t)))
	  (error "Fetching message failed"))
	(setq response (elmo-pop3-read-body process outbuf))
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

(defun elmo-pop3-delete-msgs (spec msgs &optional msgdb)
  (let ((loc-alist (if elmo-pop3-use-uidl
		       (if msgdb
			   (elmo-msgdb-get-location msgdb)
			 (elmo-msgdb-location-load
			  (elmo-msgdb-expand-path spec)))))
	(process (elmo-network-session-process-internal
		  (elmo-pop3-get-session spec))))
    (mapcar '(lambda (msg) (elmo-pop3-delete-msg
			    process msg loc-alist))
	    msgs)))

(defun elmo-pop3-search (spec condition &optional numlist)
  (error "Searching in pop3 folder is not implemented yet"))

(defun elmo-pop3-use-cache-p (spec number)
  elmo-pop3-use-cache)

(defun elmo-pop3-local-file-p (spec number)
  nil)

(defun elmo-pop3-port-label (spec)
  (concat "pop3"
	  (if (elmo-pop3-spec-stream-type spec)
	      (concat "!" (symbol-name
			   (elmo-network-stream-type-symbol
			    (elmo-pop3-spec-stream-type spec)))))))

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
(defalias 'elmo-pop3-folder-diff 'elmo-generic-folder-diff)

(defun elmo-pop3-commit (spec)
  (if (elmo-pop3-plugged-p spec)
      (let ((session (elmo-pop3-get-session spec 'if-exists)))
	(and session
	     (elmo-network-close-session session)))))
       

(require 'product)
(product-provide (provide 'elmo-pop3) (require 'elmo-version))

;;; elmo-pop3.el ends here
