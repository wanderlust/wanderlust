;;; elmo-nntp.el -- NNTP Interface for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Copyright (C) 1999,2000      Kenichi OKADA <okada@opaopa.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA <muse@ba2.so-net.ne.jp>
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

(require 'elmo-vars)
(require 'elmo-util)
(require 'elmo-date)
(require 'elmo-msgdb)
(require 'elmo-cache)
(require 'elmo)
(require 'elmo-net)

;;; ELMO NNTP folder
(eval-and-compile
  (luna-define-class elmo-nntp-folder (elmo-net-folder)
		     (group))
  (luna-define-internal-accessors 'elmo-nntp-folder))

(luna-define-method elmo-folder-initialize :around ((folder
						     elmo-nntp-folder)
						    name)
  (let ((elmo-network-stream-type-alist
	 (if elmo-nntp-stream-type-alist
	     (setq elmo-network-stream-type-alist
		   (append elmo-nntp-stream-type-alist
			   elmo-network-stream-type-alist))
	   elmo-network-stream-type-alist)))
    (setq name (luna-call-next-method))
    (when (string-match
	   "^\\([^:@!]*\\)\\(:[^/!]+\\)?\\(/[^/:@!]+\\)?"
	   name)
      (elmo-nntp-folder-set-group-internal
       folder
       (if (match-beginning 1)
	   (elmo-match-string 1 name)))
      ;; Setup slots for elmo-net-folder
      (elmo-net-folder-set-user-internal folder
					 (if (match-beginning 2)
					     (elmo-match-substring 2 folder 1)
					   elmo-default-nntp-user))
      (unless (elmo-net-folder-server-internal folder)
	(elmo-net-folder-set-server-internal folder 
					     elmo-default-nntp-server))
      (unless (elmo-net-folder-port-internal folder)
	(elmo-net-folder-set-port-internal folder
					   elmo-default-nntp-port))
      (unless (elmo-net-folder-stream-type-internal folder)
	(elmo-net-folder-set-stream-type-internal
	 folder
	 elmo-default-nntp-stream-type))
      folder)))

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-nntp-folder))
  (convert-standard-filename
   (expand-file-name
    (elmo-nntp-folder-group-internal folder)
    (expand-file-name (or (elmo-net-folder-server-internal folder) "nowhere")
		      (expand-file-name "nntp"
					elmo-msgdb-dir)))))

;;; NNTP Session
(eval-and-compile
  (luna-define-class elmo-nntp-session (elmo-network-session)
		     (current-group))
  (luna-define-internal-accessors 'elmo-nntp-session))

;;
;; internal variables
;;

(defvar elmo-nntp-connection-cache nil
  "Cache of NNTP connection.")
;; buffer local variable

(defvar elmo-nntp-list-folders-use-cache 600
  "*Time to cache of list folders, as the number of seconds.
Don't cache if nil.")

(defvar elmo-nntp-list-folders-cache nil)
(defvar elmo-nntp-groups-hashtb nil)
(defvar elmo-nntp-groups-async nil)
(defvar elmo-nntp-header-fetch-chop-length 200)

(defvar elmo-nntp-read-point 0)

(defvar elmo-nntp-send-mode-reader t)

(defvar elmo-nntp-opened-hook nil)

(defvar elmo-nntp-get-folders-securely nil)

(defvar elmo-nntp-default-use-xover t)

(defvar elmo-nntp-default-use-listgroup t)

(defvar elmo-nntp-default-use-list-active t)

(defvar elmo-nntp-default-use-xhdr t)

(defvar elmo-nntp-server-command-alist nil)


(defconst elmo-nntp-server-command-index '((xover . 0)
					   (listgroup . 1)
					   (list-active . 2)))

(defmacro elmo-nntp-get-server-command (session)
  (` (assoc (cons (elmo-network-session-server-internal (, session))
		  (elmo-network-session-port-internal (, session)))
	    elmo-nntp-server-command-alist)))

(defmacro elmo-nntp-set-server-command (session com value)
  (` (let (entry)
       (unless (setq entry (cdr (elmo-nntp-get-server-command
				 (, session))))
	 (setq elmo-nntp-server-command-alist
	       (nconc elmo-nntp-server-command-alist
		      (list (cons
			     (cons
			      (elmo-network-session-server-internal (, session))
			      (elmo-network-session-port-internal (, session)))
			     (setq entry
				   (vector
				    elmo-nntp-default-use-xover
				    elmo-nntp-default-use-listgroup
				    elmo-nntp-default-use-list-active
				    elmo-nntp-default-use-xhdr)))))))
       (aset entry
	     (cdr (assq (, com) elmo-nntp-server-command-index))
	     (, value)))))

(defmacro elmo-nntp-xover-p (session)
  (` (let ((entry (elmo-nntp-get-server-command (, session))))
       (if entry
	   (aref (cdr entry)
		 (cdr (assq 'xover elmo-nntp-server-command-index)))
	 elmo-nntp-default-use-xover))))

(defmacro elmo-nntp-set-xover (session value)
  (` (elmo-nntp-set-server-command (, session) 'xover (, value))))

(defmacro elmo-nntp-listgroup-p (session)
  (` (let ((entry (elmo-nntp-get-server-command (, session))))
       (if entry
	   (aref (cdr entry)
		 (cdr (assq 'listgroup elmo-nntp-server-command-index)))
	 elmo-nntp-default-use-listgroup))))

(defmacro elmo-nntp-set-listgroup (session value)
  (` (elmo-nntp-set-server-command (, session) 'listgroup (, value))))

(defmacro elmo-nntp-list-active-p (session)
  (` (let ((entry (elmo-nntp-get-server-command (, session))))
       (if entry
	   (aref (cdr entry)
		 (cdr (assq 'list-active elmo-nntp-server-command-index)))
	 elmo-nntp-default-use-list-active))))

(defmacro elmo-nntp-set-list-active (session value)
  (` (elmo-nntp-set-server-command (, session) 'list-active (, value))))

(defmacro elmo-nntp-xhdr-p (session)
  (` (let ((entry (elmo-nntp-get-server-command (, session))))
       (if entry
	   (aref (cdr entry)
		 (cdr (assq 'xhdr elmo-nntp-server-command-index)))
	 elmo-nntp-default-use-xhdr))))

(defmacro elmo-nntp-set-xhdr (session value)
  (` (elmo-nntp-set-server-command (, session) 'xhdr (, value))))

(defsubst elmo-nntp-max-number-precedes-list-active-p ()
  elmo-nntp-max-number-precedes-list-active)

(defsubst elmo-nntp-folder-postfix (user server port type)
  (concat
   (and user (concat ":" user))
   (if (and server
	    (null (string= server elmo-default-nntp-server)))
       (concat "@" server))
   (if (and port
	    (null (eq port elmo-default-nntp-port)))
       (concat ":" (if (numberp port)
		       (int-to-string port) port)))
   (unless (eq (elmo-network-stream-type-symbol type)
	       elmo-default-nntp-stream-type)
     (elmo-network-stream-type-spec-string type))))

(defun elmo-nntp-get-session (folder &optional if-exists)
  (elmo-network-get-session
   'elmo-nntp-session
   "NNTP"
   folder
   if-exists))

(luna-define-method elmo-network-initialize-session ((session
						      elmo-nntp-session))
  (let ((process (elmo-network-session-process-internal session)))
    (set-process-filter (elmo-network-session-process-internal session)
			'elmo-nntp-process-filter)
    (with-current-buffer (elmo-network-session-buffer session)
      (setq elmo-nntp-read-point (point-min))
      ;; Skip garbage output from process before greeting.
      (while (and (memq (process-status process) '(open run))
                  (goto-char (point-max))
                  (forward-line -1)
                  (not (looking-at "20[01]")))
        (accept-process-output process 1))
      (setq elmo-nntp-read-point (point))
      (or (elmo-nntp-read-response session t)
	  (error "Cannot open network"))
      (when (eq (elmo-network-stream-type-symbol
		 (elmo-network-session-stream-type-internal session))
		'starttls)
	(elmo-nntp-send-command session "starttls")
	(or (elmo-nntp-read-response session)
	    (error "Cannot open starttls session"))
	(starttls-negotiate process)))))

(luna-define-method elmo-network-authenticate-session ((session
							elmo-nntp-session))
  (with-current-buffer (elmo-network-session-buffer session)
    (when (elmo-network-session-user-internal session)
      (elmo-nntp-send-command session
			      (format "authinfo user %s"
				      (elmo-network-session-user-internal
				       session)))
      (or (elmo-nntp-read-response session)
	  (signal 'elmo-authenticate-error '(authinfo)))
      (elmo-nntp-send-command
       session
       (format "authinfo pass %s"
	       (elmo-get-passwd (elmo-network-session-password-key session))))
      (or (elmo-nntp-read-response session)
	  (signal 'elmo-authenticate-error '(authinfo))))))

(luna-define-method elmo-network-setup-session ((session
						 elmo-nntp-session))
  (if elmo-nntp-send-mode-reader
      (elmo-nntp-send-mode-reader session))
  (run-hooks 'elmo-nntp-opened-hook))

(defun elmo-nntp-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)))

(defun elmo-nntp-send-mode-reader (session)
  (elmo-nntp-send-command session "mode reader")
  (if (null (elmo-nntp-read-response session t))
      (error "Mode reader failed")))
  
(defun elmo-nntp-send-command (session command &optional noerase)
  (with-current-buffer (elmo-network-session-buffer session)
    (unless noerase
      (erase-buffer)
      (goto-char (point-min)))
    (setq elmo-nntp-read-point (point))
    (process-send-string (elmo-network-session-process-internal
			  session) command)
    (process-send-string (elmo-network-session-process-internal
			  session) "\r\n")))

(defun elmo-nntp-read-response (session &optional not-command)
  (with-current-buffer (elmo-network-session-buffer session)
    (let ((process (elmo-network-session-process-internal session))
	  (case-fold-search nil)
	  (response-string nil)
	  (response-continue t)
	  response match-end)
      (while response-continue
	(goto-char elmo-nntp-read-point)
	(while (not (search-forward "\r\n" nil t))
	  (accept-process-output process)
	  (goto-char elmo-nntp-read-point))
	(setq match-end (point))
	(setq response-string
	      (buffer-substring elmo-nntp-read-point (- match-end 2)))
	(goto-char elmo-nntp-read-point)
	(if (looking-at "[23][0-9]+ .*$")
	    (progn (setq response-continue nil)
		   (setq elmo-nntp-read-point match-end)
		   (setq response
			 (if response
			     (concat response "\n" response-string)
			   response-string)))
	  (if (looking-at "[^23][0-9]+ .*$")
	      (progn (setq response-continue nil)
		     (setq elmo-nntp-read-point match-end)
		     (setq response nil))
	    (setq elmo-nntp-read-point match-end)
	    (if not-command
		(setq response-continue nil))
	    (setq response
		  (if response
		      (concat response "\n" response-string)
		    response-string)))
	  (setq elmo-nntp-read-point match-end)))
      response)))

(defun elmo-nntp-read-raw-response (session)
  (with-current-buffer (elmo-network-session-buffer session)
    (goto-char elmo-nntp-read-point)
    (while (not (search-forward "\r\n" nil t))
      (accept-process-output (elmo-network-session-process-internal
			      session))
      (goto-char elmo-nntp-read-point))
    (buffer-substring elmo-nntp-read-point (- (point) 2))))

(defun elmo-nntp-read-contents (session)
  (with-current-buffer (elmo-network-session-buffer session)
    (goto-char elmo-nntp-read-point)
    (while (not (re-search-forward "^\\.\r\n" nil t))
      (accept-process-output (elmo-network-session-process-internal
			      session))
      (goto-char elmo-nntp-read-point))
    (elmo-delete-cr
     (buffer-substring elmo-nntp-read-point
		       (- (point) 3)))))

(defun elmo-nntp-read-body (session outbuf)
  (with-current-buffer (elmo-network-session-buffer session)
    (goto-char elmo-nntp-read-point)
    (while (not (re-search-forward "^\\.\r\n" nil t))
      (accept-process-output (elmo-network-session-process-internal session))
      (goto-char elmo-nntp-read-point))
    (let ((start elmo-nntp-read-point)
	  (end  (point)))
      (with-current-buffer outbuf
	(erase-buffer)
	(insert-buffer-substring (elmo-network-session-buffer session)
				 start (- end 3))))))

(defun elmo-nntp-select-group (session group &optional force)
  (let (response)
    (when (or force
	      (not (string= (elmo-nntp-session-current-group-internal session)
			    group)))
      (unwind-protect
	  (progn
	    (elmo-nntp-send-command session (format "group %s" group))
	    (setq response (elmo-nntp-read-response session)))
	(elmo-nntp-session-set-current-group-internal session
						      (and response group))
	response))))

(defun elmo-nntp-list-folders-get-cache (folder buf)
  (when (and elmo-nntp-list-folders-use-cache
	     elmo-nntp-list-folders-cache
	     (string-match (concat "^"
				   (regexp-quote
				    (or
				     (nth 1 elmo-nntp-list-folders-cache)
				     "")))
			   (or folder "")))
    (let* ((cache-time (car elmo-nntp-list-folders-cache)))
      (unless (elmo-time-expire cache-time
				elmo-nntp-list-folders-use-cache)
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  (insert (nth 2 elmo-nntp-list-folders-cache))
	  (goto-char (point-min))
	  (or (string= folder "")
	      (and folder
		   (keep-lines (concat "^" (regexp-quote folder) "\\."))))
	  t
	  )))))

(defsubst elmo-nntp-catchup-msgdb (msgdb max-number)
  (let (msgdb-max number-alist)
    (setq number-alist (elmo-msgdb-get-number-alist msgdb))
    (setq msgdb-max (car (nth (max (- (length number-alist) 1) 0)
			      number-alist)))
    (if (or (not msgdb-max)
	    (and msgdb-max max-number
		 (< msgdb-max max-number)))
	(elmo-msgdb-set-number-alist
	 msgdb
	 (nconc number-alist (list (cons max-number nil)))))))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-nntp-folder)
						 &optional one-level)
  (elmo-nntp-folder-list-subfolders folder one-level))

(defun elmo-nntp-folder-list-subfolders (folder one-level)
  (let ((session (elmo-nntp-get-session folder))
	response ret-val top-ng append-serv use-list-active start)
    (with-temp-buffer
      (if (and (elmo-nntp-folder-group-internal folder)
	       (elmo-nntp-select-group 
		session
		(elmo-nntp-folder-group-internal folder)))
	  ;; add top newsgroups
	  (setq ret-val (list (elmo-nntp-folder-group-internal folder))))
      (unless (setq response (elmo-nntp-list-folders-get-cache
			      (elmo-nntp-folder-group-internal folder)
			      (current-buffer)))
	(when (setq use-list-active (elmo-nntp-list-active-p session))
	  (elmo-nntp-send-command
	   session
	   (concat "list"
		   (if (and (elmo-nntp-folder-group-internal folder)
			    (null (string= (elmo-nntp-folder-group-internal
					    folder) "")))
		       (concat " active"
			       (format " %s.*"
				       (elmo-nntp-folder-group-internal folder)
				       "")))))
	  (if (elmo-nntp-read-response session t)
	      (if (null (setq response (elmo-nntp-read-contents session)))
		  (error "NNTP List folders failed")
		(when elmo-nntp-list-folders-use-cache
		  (setq elmo-nntp-list-folders-cache
			(list (current-time)
			      (elmo-nntp-folder-group-internal folder)
			      response)))
		(erase-buffer)
		(insert response))
	    (elmo-nntp-set-list-active session nil)
	    (setq use-list-active nil)))
	(when (null use-list-active)
	  (elmo-nntp-send-command session "list")
	  (if (null (and (elmo-nntp-read-response session t)
			 (setq response (elmo-nntp-read-contents session))))
	      (error "NNTP List folders failed"))
	  (when elmo-nntp-list-folders-use-cache
	    (setq elmo-nntp-list-folders-cache
		  (list (current-time) nil response)))
	  (erase-buffer)
	  (setq start nil)
	  (while (string-match (concat "^"
				       (regexp-quote
					(or 
					 (elmo-nntp-folder-group-internal
					  folder)
					 "")) ".*$")
			       response start)
	    (insert (match-string 0 response) "\n")
	    (setq start (match-end 0)))))
      (goto-char (point-min))
      (let ((len (count-lines (point-min) (point-max)))
	    (i 0) regexp)
	(if one-level
	    (progn
	      (setq regexp
		    (format "^\\(%s[^. ]+\\)\\([. ]\\).*\n"
			    (if (and 
				 (elmo-nntp-folder-group-internal folder)
				 (null (string=
					(elmo-nntp-folder-group-internal
					 folder) "")))
				(concat (elmo-nntp-folder-group-internal
					 folder)
					"\\.") "")))
	      (while (looking-at regexp)
		(setq top-ng (elmo-match-buffer 1))
		(if (string= (elmo-match-buffer 2) " ")
		    (if (not (or (member top-ng ret-val)
				 (assoc top-ng ret-val)))
			(setq ret-val (nconc ret-val (list top-ng))))
		  (if (member top-ng ret-val)
		      (setq ret-val (delete top-ng ret-val)))
		  (if (not (assoc top-ng ret-val))
		      (setq ret-val (nconc ret-val (list (list top-ng))))))
		(when (> len elmo-display-progress-threshold)
		  (setq i (1+ i))
		  (if (or (zerop (% i 10)) (= i len))
		      (elmo-display-progress
		       'elmo-nntp-list-folders "Parsing active..."
		       (/ (* i 100) len))))
		(forward-line 1)))
	  (while (re-search-forward "\\([^ ]+\\) .*\n" nil t)
	    (setq ret-val (nconc ret-val
				 (list (elmo-match-buffer 1))))
	    (when (> len elmo-display-progress-threshold)
	      (setq i (1+ i))
	      (if (or (zerop (% i 10)) (= i len))
		  (elmo-display-progress
		   'elmo-nntp-list-folders "Parsing active..."
		   (/ (* i 100) len))))))
	(when (> len elmo-display-progress-threshold)
	  (elmo-display-progress
	   'elmo-nntp-list-folders "Parsing active..." 100))))
    (unless (string= (elmo-net-folder-server-internal folder)
		     elmo-default-nntp-server)
      (setq append-serv (concat "@" (elmo-net-folder-server-internal
				     folder))))
    (unless (eq (elmo-net-folder-port-internal folder) elmo-default-nntp-port)
      (setq append-serv (concat append-serv
				":" (int-to-string
				     (elmo-net-folder-port-internal folder)))))
    (unless (eq (elmo-network-stream-type-symbol
		 (elmo-net-folder-stream-type-internal folder))
		elmo-default-nntp-stream-type)
      (setq append-serv
	    (concat append-serv
		    (elmo-network-stream-type-spec-string
		     (elmo-net-folder-stream-type-internal folder)))))
    (mapcar '(lambda (fld)
	       (if (consp fld)
		   (list (concat "-" (car fld)
				 (and (elmo-net-folder-user-internal folder)
				      (concat
				       ":"
				       (elmo-net-folder-user-internal folder)))
				 (and append-serv
				      (concat append-serv))))
		 (concat "-" fld
			 (and (elmo-net-folder-user-internal folder)
			      (concat ":" (elmo-net-folder-user-internal
					   folder)))
			 (and append-serv
			      (concat append-serv)))))
	    ret-val)))

(defun elmo-nntp-make-msglist (beg-str end-str)
  (elmo-set-work-buf
   (let ((beg-num (string-to-int beg-str))
	 (end-num (string-to-int end-str))
	 i)
     (setq i beg-num)
     (insert "(")
     (while (<= i end-num)
       (insert (format "%s " i))
       (setq i (1+ i)))
     (insert ")")
     (goto-char (point-min))
     (read (current-buffer)))))

(luna-define-method elmo-folder-list-messages-internal ((folder
							 elmo-nntp-folder))
  (let ((session (elmo-nntp-get-session folder))
	(group   (elmo-nntp-folder-group-internal folder))
	response numbers use-listgroup)
    (save-excursion
      (when (setq use-listgroup (elmo-nntp-listgroup-p session))
	(elmo-nntp-send-command session
				(format "listgroup %s" group))
	(if (not (elmo-nntp-read-response session t))
	    (progn
	      (elmo-nntp-set-listgroup session nil)
	      (setq use-listgroup nil))
	  (if (null (setq response (elmo-nntp-read-contents session)))
	      (error "Fetching listgroup failed"))
	  (setq numbers (elmo-string-to-list response))
	  (elmo-nntp-session-set-current-group-internal session
							group)))
      (unless use-listgroup
	(elmo-nntp-send-command session (format "group %s" group))
	(if (null (setq response (elmo-nntp-read-response session)))
	    (error "Select group failed"))
	(when (and
	       (string-match
		"211 \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) [^.].+$"
		response)
	       (> (string-to-int (elmo-match-string 1 response)) 0))
	  (setq numbers (elmo-nntp-make-msglist
			 (elmo-match-string 2 response)
			 (elmo-match-string 3 response)))))
      numbers)))

(luna-define-method elmo-folder-status ((folder elmo-nntp-folder))
  (elmo-nntp-folder-status folder))

(defun elmo-nntp-folder-status (folder)
  (let ((killed-list (elmo-msgdb-killed-list-load
		      (elmo-folder-msgdb-path folder)))
	end-num entry)
    (if elmo-nntp-groups-async
	(if (setq entry
		  (elmo-get-hash-val
		   (concat (elmo-nntp-folder-group-internal folder)
			   (elmo-nntp-folder-postfix
			    (elmo-net-folder-user-internal folder)
			    (elmo-net-folder-server-internal folder)
			    (elmo-net-folder-port-internal folder)
			    (elmo-net-folder-stream-type-internal folder)))
		   elmo-nntp-groups-hashtb))
	    (progn
	      (setq end-num (nth 2 entry))
	      (when(and  killed-list
			 (elmo-number-set-member end-num killed-list))
		;; Max is killed.
		(setq end-num nil))
	      (cons end-num (car entry)))
	  (error "No such newsgroup \"%s\"" 
		 (elmo-nntp-folder-group-internal folder)))
      (let ((session (elmo-nntp-get-session folder))
	    response e-num)
	(if (null session)
	    (error "Connection failed"))
	(save-excursion
	  (elmo-nntp-send-command session
				  (format 
				   "group %s"
				   (elmo-nntp-folder-group-internal folder)))
	  (setq response (elmo-nntp-read-response session))
	  (if (and response
		   (string-match
		    "211 \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) [^.].+$"
		    response))
	      (progn
		(setq end-num (string-to-int
			       (elmo-match-string 3 response)))
		(setq e-num (string-to-int
			     (elmo-match-string 1 response)))
		(when (and killed-list
			   (elmo-number-set-member end-num killed-list))
		  ;; Max is killed.
		  (setq end-num nil))
		(cons end-num e-num))
	    (if (null response)
		(error "Selecting newsgroup \"%s\" failed"
		       (elmo-nntp-folder-group-internal folder))
	      nil)))))))

(defconst elmo-nntp-overview-index
  '(("number" . 0)
    ("subject" . 1)
    ("from" . 2)
    ("date" . 3)
    ("message-id" . 4)
    ("references" . 5)
    ("size" . 6)
    ("lines" . 7)
    ("xref" . 8)))

(defun elmo-nntp-create-msgdb-from-overview-string (str
						    new-mark
						    already-mark
						    seen-mark
						    important-mark
						    seen-list
						    &optional numlist)
  (let (ov-list gmark message-id seen
	ov-entity overview number-alist mark-alist num
	extras extra ext field field-index)
    (setq ov-list (elmo-nntp-parse-overview-string str))
    (while ov-list
      (setq ov-entity (car ov-list))
;;; INN bug??
;;;   (if (or (> (setq num (string-to-int (aref ov-entity 0)))
;;;		 99999)
;;;	      (<= num 0))
;;;	  (setq num 0))
;;;  (setq num (int-to-string num))
      (setq num (string-to-int (aref ov-entity 0)))
      (when (or (null numlist)
		(memq num numlist))
	(setq extras elmo-msgdb-extra-fields
	      extra nil)
	(while extras
	  (setq ext (downcase (car extras)))
	  (when (setq field-index (cdr (assoc ext elmo-nntp-overview-index)))
	    (setq field (aref ov-entity field-index))
	    (when (eq field-index 8) ;; xref
	      (setq field (elmo-msgdb-remove-field-string field)))
	    (setq extra (cons (cons ext field) extra)))
	  (setq extras (cdr extras)))
	(setq overview
	      (elmo-msgdb-append-element
	       overview
	       (cons (aref ov-entity 4)
		     (vector num
			     (elmo-msgdb-get-last-message-id
			      (aref ov-entity 5))
			     ;; from
			     (elmo-mime-string (elmo-delete-char
						?\"
						(or
						 (aref ov-entity 2)
						 elmo-no-from) 'uni))
			     ;; subject
			     (elmo-mime-string (or (aref ov-entity 1)
						   elmo-no-subject))
			     (aref ov-entity 3) ;date
			     nil ; to
			     nil ; cc
			     (string-to-int
			      (aref ov-entity 6)) ; size
			     extra ; extra-field-list
			     ))))
	(setq number-alist
	      (elmo-msgdb-number-add number-alist num
				     (aref ov-entity 4)))
	(setq message-id (aref ov-entity 4))
	(setq seen (member message-id seen-list))
	(if (setq gmark (or (elmo-msgdb-global-mark-get message-id)
			    (if (elmo-file-cache-status
				 (elmo-file-cache-get message-id))
				(if seen
				    nil
				  already-mark)
			      (if seen
				  (if elmo-nntp-use-cache
				      seen-mark)
				new-mark))))
	    (setq mark-alist
		  (elmo-msgdb-mark-append mark-alist
					  num gmark))))
      (setq ov-list (cdr ov-list)))
    (list overview number-alist mark-alist)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-nntp-folder)
					      numbers new-mark already-mark
					      seen-mark important-mark
					      seen-list)
  (elmo-nntp-folder-msgdb-create folder numbers new-mark already-mark
				 seen-mark important-mark
				 seen-list))

(defun elmo-nntp-folder-msgdb-create (folder numbers new-mark already-mark
					     seen-mark important-mark
					     seen-list)
  (let ((filter numbers)
	(session (elmo-nntp-get-session folder))
	beg-num end-num cur length
	ret-val ov-str use-xover dir)
    (elmo-nntp-select-group session (elmo-nntp-folder-group-internal
				     folder))
    (when (setq use-xover (elmo-nntp-xover-p session))
      (setq beg-num (car numbers)
	    cur beg-num
	    end-num (nth (1- (length numbers)) numbers)
	    length  (+ (- end-num beg-num) 1))
      (message "Getting overview...")
      (while (<= cur end-num)
	(elmo-nntp-send-command
	 session
	 (format
	  "xover %s-%s"
	  (int-to-string cur)
	  (int-to-string
	   (+ cur
	      elmo-nntp-overview-fetch-chop-length))))
	(with-current-buffer (elmo-network-session-buffer session)
	  (if ov-str
	      (setq ret-val
		    (elmo-msgdb-append
		     ret-val
		     (elmo-nntp-create-msgdb-from-overview-string
		      ov-str
		      new-mark
		      already-mark
		      seen-mark
		      important-mark
		      seen-list
		      filter
		      )))))
	(if (null (elmo-nntp-read-response session t))
	    (progn
	      (setq cur end-num);; exit while loop
	      (elmo-nntp-set-xover session nil)
	      (setq use-xover nil))
	  (if (null (setq ov-str (elmo-nntp-read-contents session)))
	      (error "Fetching overview failed")))
	(setq cur (+ elmo-nntp-overview-fetch-chop-length cur 1))
	(when (> length elmo-display-progress-threshold)
	  (elmo-display-progress
	   'elmo-nntp-msgdb-create "Getting overview..."
	   (/ (* (+ (- (min cur end-num)
		       beg-num) 1) 100) length))))
      (when (> length elmo-display-progress-threshold)
	(elmo-display-progress
	 'elmo-nntp-msgdb-create "Getting overview..." 100)))
    (if (not use-xover)
	(setq ret-val (elmo-nntp-msgdb-create-by-header
		       session numbers
		       new-mark already-mark seen-mark seen-list))
      (with-current-buffer (elmo-network-session-buffer session)
	(if ov-str
	    (setq ret-val
		  (elmo-msgdb-append
		   ret-val
		   (elmo-nntp-create-msgdb-from-overview-string
		    ov-str
		    new-mark
		    already-mark
		    seen-mark
		    important-mark
		    seen-list
		    filter))))))
    (elmo-folder-set-killed-list-internal
     folder
     (nconc
      (elmo-folder-killed-list-internal folder)
      (car (elmo-list-diff
	    numbers
	    (mapcar 'car
		    (elmo-msgdb-get-number-alist
		     ret-val))))))
    ;; If there are canceled messages, overviews are not obtained
    ;; to max-number(inn 2.3?).
    (when (and (elmo-nntp-max-number-precedes-list-active-p)
	       (elmo-nntp-list-active-p session))
      (elmo-nntp-send-command session
			      (format "list active %s"
				      (elmo-nntp-folder-group-internal
				       folder)))
      (if (null (elmo-nntp-read-response session))
	  (progn
	    (elmo-nntp-set-list-active session nil)
	    (error "NNTP list command failed")))
      (elmo-nntp-catchup-msgdb
       ret-val
       (nth 1 (read (concat "(" (elmo-nntp-read-contents
				 session) ")")))))
    ret-val))

(luna-define-method elmo-folder-update-number ((folder elmo-nntp-folder))
  (if (elmo-nntp-max-number-precedes-list-active-p)
      (let ((session (elmo-nntp-get-session folder))
	    (number-alist (elmo-msgdb-get-number-alist
			   (elmo-folder-msgdb-internal folder))))
	(if (elmo-nntp-list-active-p session)
	    (let (msgdb-max max-number)
	      ;; If there are canceled messages, overviews are not obtained
	      ;; to max-number(inn 2.3?).
	      (elmo-nntp-select-group session
				      (elmo-nntp-folder-group-internal folder))
	      (elmo-nntp-send-command session
				      (format "list active %s"
					      (elmo-nntp-folder-group-internal
					       folder)))
	      (if (null (elmo-nntp-read-response session))
		  (error "NNTP list command failed"))
	      (setq max-number
		    (nth 1 (read (concat "(" (elmo-nntp-read-contents
					      session) ")"))))
	      (setq msgdb-max
		    (car (nth (max (- (length number-alist) 1) 0)
			      number-alist)))
	      (if (or (and number-alist (not msgdb-max))
		      (and msgdb-max max-number
			   (< msgdb-max max-number)))
		  (elmo-msgdb-set-number-alist
		   (elmo-folder-msgdb-internal folder)
		   (nconc number-alist
			  (list (cons max-number nil))))))))))

(defun elmo-nntp-msgdb-create-by-header (session numbers
						 new-mark already-mark
						 seen-mark seen-list)
  (with-temp-buffer
    (elmo-nntp-retrieve-headers session (current-buffer) numbers)
    (elmo-nntp-msgdb-create-message
     (length numbers) new-mark already-mark seen-mark seen-list)))

(defun elmo-nntp-parse-xhdr-response (string)
  (let (response)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at "^\\([0-9]+\\) \\(.*\\)$")
	    (setq response (cons (cons (string-to-int (elmo-match-buffer 1))
				       (elmo-match-buffer 2))
				 response)))
	(forward-line 1)))
    (nreverse response)))

(defun elmo-nntp-parse-overview-string (string)
  (save-excursion
    (let ((tmp-buffer (get-buffer-create " *ELMO Overview TMP*"))
	  ret-list ret-val beg)
      (set-buffer tmp-buffer)
      (erase-buffer)
      (elmo-set-buffer-multibyte nil)
      (insert string)
      (goto-char (point-min))
      (setq beg (point))
      (while (not (eobp))
	(end-of-line)
	(setq ret-list (save-match-data
			 (apply 'vector (split-string
					 (buffer-substring beg (point))
					 "\t"))))
	(beginning-of-line)
	(forward-line 1)
	(setq beg (point))
	(setq ret-val (nconc ret-val (list ret-list))))
;;;   (kill-buffer tmp-buffer)
      ret-val)))

(defun elmo-nntp-get-newsgroup-by-msgid (msgid server user port type)
  "Get nntp header string."
  (save-excursion
    (let ((session (elmo-nntp-get-session
		    (list 'nntp nil user server port type))))
      (elmo-nntp-send-command session
			      (format "head %s" msgid))
      (if (elmo-nntp-read-response session)
	  (elmo-nntp-read-contents session))
      (with-current-buffer (elmo-network-session-buffer session)
	(std11-field-body "Newsgroups")))))

(luna-define-method elmo-message-fetch-plugged ((folder elmo-nntp-folder)
						number strategy
						&optional section outbuf
						unseen)
  (elmo-nntp-message-fetch folder number strategy section outbuf unseen))

(defun elmo-nntp-message-fetch (folder number strategy section outbuf unseen)
  (let ((session (elmo-nntp-get-session folder)))
    (with-current-buffer (elmo-network-session-buffer session)
      (elmo-nntp-select-group session (elmo-nntp-folder-group-internal folder))
      (elmo-nntp-send-command session (format "article %s" number))
      (if (null (elmo-nntp-read-response session t))
	  (progn
	    (with-current-buffer outbuf (erase-buffer))
	    (message "Fetching message failed")
	    nil)
	(prog1 (elmo-nntp-read-body session outbuf)
	  (with-current-buffer outbuf
	    (goto-char (point-min))
	    (while (re-search-forward "^\\." nil t)
	      (replace-match "")
	      (forward-line))))))))

(defun elmo-nntp-post (hostname content-buf)
  (let ((session (elmo-nntp-get-session
		  (luna-make-entity
		   'elmo-nntp-folder
		   :user elmo-default-nntp-user
		   :server hostname
		   :port elmo-default-nntp-port
		   :stream-type elmo-default-nntp-stream-type)))
	response has-message-id)
    (save-excursion
      (set-buffer content-buf)
      (goto-char (point-min))
      (if (search-forward mail-header-separator nil t)
	  (delete-region (match-beginning 0)(match-end 0)))
      (setq has-message-id (std11-field-body "message-id"))
      (elmo-nntp-send-command session "post")
      (if (string-match "^340" (setq response
				     (elmo-nntp-read-raw-response session)))
	  (if (string-match "recommended ID \\(<[^@]+@[^>]+>\\)" response)
	      (unless has-message-id
		(goto-char (point-min))
		(insert (concat "Message-ID: "
				(elmo-match-string 1 response)
				"\n"))))
	(error "POST failed"))
      (run-hooks 'elmo-nntp-post-pre-hook)
      (elmo-nntp-send-buffer session content-buf)
      (elmo-nntp-send-command session ".")
;;;   (elmo-nntp-read-response buffer process t)
      (if (not (string-match
		"^2" (setq response (elmo-nntp-read-raw-response
				     session))))
	  (error (concat "NNTP error: " response))))))

(defsubst elmo-nntp-send-data-line (session line)
  "Send LINE to SESSION."
  ;; Escape "." at start of a line
  (if (eq (string-to-char line) ?.)
      (process-send-string (elmo-network-session-process-internal
			    session) "."))
  (process-send-string (elmo-network-session-process-internal
			session) line)
  (process-send-string (elmo-network-session-process-internal
			session) "\r\n"))

(defun elmo-nntp-send-buffer (session databuf)
  "Send data content of DATABUF to SESSION."
  (let ((data-continue t)
	line bol)
    (with-current-buffer databuf
      (goto-char (point-min))
      (while data-continue
	(beginning-of-line)
	(setq bol (point))
	(end-of-line)
	(setq line (buffer-substring bol (point)))
	(unless (eq (forward-line 1) 0) (setq data-continue nil))
	(elmo-nntp-send-data-line session line)))))

(luna-define-method elmo-folder-delete-messages ((folder elmo-nntp-folder)
						 numbers)
  (elmo-nntp-folder-delete-messages folder numbers))

(defun elmo-nntp-folder-delete-messages (folder numbers)
  (let ((killed-list (elmo-folder-killed-list-internal folder)))
    (dolist (number numbers)
      (setq killed-list
	    (elmo-msgdb-set-as-killed killed-list number)))
    (elmo-folder-set-killed-list-internal folder killed-list))
  t)

(luna-define-method elmo-folder-exists-p ((folder elmo-nntp-folder))
  (let ((session (elmo-nntp-get-session folder)))
    (if (elmo-folder-plugged-p folder)
	(progn
	  (elmo-nntp-send-command
	   session
	   (format "group %s"
		   (elmo-nntp-folder-group-internal folder)))
	  (elmo-nntp-read-response session))
      t)))

(defun elmo-nntp-retrieve-field (spec field from-msgs)
  "Retrieve FIELD values from FROM-MSGS.
Returns a list of cons cells like (NUMBER . VALUE)"
  (let ((session (elmo-nntp-get-session spec)))
    (if (elmo-nntp-xhdr-p session)
	(progn
	  (elmo-nntp-select-group session (elmo-nntp-folder-group-internal spec))
	  (elmo-nntp-send-command session
				  (format "xhdr %s %s"
					  field
					  (if from-msgs
					      (format
					       "%d-%d"
					       (car from-msgs)
					       (nth
						(max
						 (- (length from-msgs) 1) 0)
						from-msgs))
					    "0-")))
	  (if (elmo-nntp-read-response session t)
	      (elmo-nntp-parse-xhdr-response
	       (elmo-nntp-read-contents session))
	    (elmo-nntp-set-xhdr session nil)
	    (error "NNTP XHDR command failed"))))))

(defun elmo-nntp-search-primitive (spec condition &optional from-msgs)
  (let ((search-key (elmo-filter-key condition)))
    (cond
     ((string= "last" search-key)
      (let ((numbers (or from-msgs (elmo-folder-list-messages spec))))
	(nthcdr (max (- (length numbers)
			(string-to-int (elmo-filter-value condition)))
		     0)
		numbers)))
     ((string= "first" search-key)
      (let* ((numbers (or from-msgs (elmo-folder-list-messages spec)))
	     (rest (nthcdr (string-to-int (elmo-filter-value condition) )
			   numbers)))
	(mapcar '(lambda (x) (delete x numbers)) rest)
	numbers))
     ((or (string= "since" search-key)
	  (string= "before" search-key))
      (let* ((key-date (elmo-date-get-datevec (elmo-filter-value condition)))
	     (key-datestr (elmo-date-make-sortable-string key-date))
	     (since (string= "since" search-key))
	     result)
	(if (eq (elmo-filter-type condition) 'unmatch)
	    (setq since (not since)))
	(setq result
	      (delq nil
		    (mapcar
		     (lambda (pair)
		       (if (if since
			       (string< key-datestr
					(elmo-date-make-sortable-string
					 (timezone-fix-time
					  (cdr pair)
					  (current-time-zone) nil)))
			     (not (string< key-datestr
					   (elmo-date-make-sortable-string
					    (timezone-fix-time
					     (cdr pair)
					     (current-time-zone) nil)))))
			   (car pair)))
		     (elmo-nntp-retrieve-field spec "date" from-msgs))))
	(if from-msgs
	    (elmo-list-filter from-msgs result)
	  result)))
     (t
      (let ((val (elmo-filter-value condition))
	    (negative (eq (elmo-filter-type condition) 'unmatch))
	    (case-fold-search t)
	    result)
	(setq result
	      (delq nil
		    (mapcar
		     (lambda (pair)
		       (if (string-match val
					 (eword-decode-string
					  (decode-mime-charset-string
					   (cdr pair) elmo-mime-charset)))
			   (unless negative (car pair))
			 (if negative (car pair))))
		     (elmo-nntp-retrieve-field spec search-key
					       from-msgs))))
	(if from-msgs
	    (elmo-list-filter from-msgs result)
	  result))))))

(luna-define-method elmo-folder-search ((folder elmo-nntp-folder) 
					condition &optional from-msgs)
  (let (result)
    (cond
     ((vectorp condition)
      (setq result (elmo-nntp-search-primitive
		    folder condition from-msgs)))
     ((eq (car condition) 'and)
      (setq result (elmo-folder-search folder (nth 1 condition) from-msgs)
	    result (elmo-list-filter result
				     (elmo-folder-search
				      folder (nth 2 condition)
				      from-msgs))))
     ((eq (car condition) 'or)
      (setq result (elmo-folder-search folder (nth 1 condition) from-msgs)
	    result (elmo-uniq-list
		    (nconc result
			   (elmo-folder-search folder (nth 2 condition)
					       from-msgs)))
	    result (sort result '<))))))

(defun elmo-nntp-get-folders-info-prepare (folder session-keys)
  (condition-case ()
      (let ((session (elmo-nntp-get-session folder))
	    key count)
	(with-current-buffer (elmo-network-session-buffer session)
	  (unless (setq key (assoc session session-keys))
	    (erase-buffer)
	    (setq key (cons session
			    (vector 0
				    (elmo-net-folder-server-internal folder)
				    (elmo-net-folder-user-internal folder)
				    (elmo-net-folder-port-internal folder)
				    (elmo-net-folder-stream-type-internal
				     folder))))
	    (setq session-keys (nconc session-keys (list key))))
	  (elmo-nntp-send-command session
				  (format "group %s"
					  (elmo-nntp-folder-group-internal
					   folder))
				  'noerase)
	  (if elmo-nntp-get-folders-securely
	      (accept-process-output
	       (elmo-network-session-process-internal session)
	       1))
	  (setq count (aref (cdr key) 0))
	  (aset (cdr key) 0 (1+ count))))
    (error
     (when elmo-auto-change-plugged
       (sit-for 1))
     nil))
  session-keys)

(defun elmo-nntp-get-folders-info (session-keys)
  (let ((sessions session-keys)
	(cur (get-buffer-create " *ELMO NNTP Temp*")))
    (while sessions
      (let* ((session (caar sessions))
	     (key     (cdar sessions))
	     (count   (aref key 0))
	     (server  (aref key 1))
	     (user    (aref key 2))
	     (port    (aref key 3))
	     (type    (aref key 4))
	     (hashtb (or elmo-nntp-groups-hashtb
			 (setq elmo-nntp-groups-hashtb
			       (elmo-make-hash count)))))
	(save-excursion
	  (elmo-nntp-groups-read-response session cur count)
	  (set-buffer cur)
	  (goto-char (point-min))
	  (let ((case-replace nil)
		(postfix (elmo-nntp-folder-postfix user server port type)))
	    (if (not (string= postfix ""))
		(save-excursion
		  (replace-regexp "^\\(211 [0-9]+ [0-9]+ [0-9]+ [^ \n]+\\).*$"
				  (concat "\\1"
					  (elmo-replace-in-string
					   postfix
					   "\\\\" "\\\\\\\\\\\\\\\\"))))))
	  (let (len min max group)
	    (while (not (eobp))
	      (condition-case ()
		  (when (= (following-char) ?2)
		    (read cur)
		    (setq len (read cur)
			  min (read cur)
			  max (read cur))
		    (set (setq group (let ((obarray hashtb)) (read cur)))
			 (list len min max)))
		(error (and group (symbolp group) (set group nil))))
	      (forward-line 1))))
	(setq sessions (cdr sessions))))
    (kill-buffer cur)))

;; original is 'nntp-retrieve-groups [Gnus]
(defun elmo-nntp-groups-read-response (session outbuf count)
  (let* ((received 0)
	 (last-point (point-min)))
    (with-current-buffer (elmo-network-session-buffer session)
      (accept-process-output
       (elmo-network-session-process-internal session) 1)
      (discard-input)
      ;; Wait for all replies.
      (message "Getting folders info...")
      (while (progn
	       (goto-char last-point)
	       ;; Count replies.
	       (while (re-search-forward "^[0-9]" nil t)
		 (setq received
		       (1+ received)))
	       (setq last-point (point))
	       (< received count))
	(accept-process-output (elmo-network-session-process-internal session)
			       1)
	(discard-input)
	(when (> count elmo-display-progress-threshold)
	  (if (or (zerop (% received 10)) (= received count))
	      (elmo-display-progress
	       'elmo-nntp-groups-read-response "Getting folders info..."
	       (/ (* received 100) count)))))
      (when (> count elmo-display-progress-threshold)
	(elmo-display-progress
	 'elmo-nntp-groups-read-response "Getting folders info..." 100))
      ;; Wait for the reply from the final command.
      (goto-char (point-max))
      (re-search-backward "^[0-9]" nil t)
      (when (looking-at "^[23]")
	(while (progn
		 (goto-char (point-max))
		 (not (re-search-backward "\r?\n" (- (point) 3) t)))
	  (accept-process-output
	   (elmo-network-session-process-internal session) 1)
	  (discard-input)))
      ;; Now all replies are received.  We remove CRs.
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "" t t))
      (copy-to-buffer outbuf (point-min) (point-max)))))

(defun elmo-nntp-make-groups-hashtb (groups &optional size)
  (let ((hashtb (or elmo-nntp-groups-hashtb
		    (setq elmo-nntp-groups-hashtb
			  (elmo-make-hash (or size (length groups)))))))
    (mapcar
     '(lambda (group)
	(or (elmo-get-hash-val group hashtb)
	    (elmo-set-hash-val group nil hashtb)))
     groups)
    hashtb))

;; from nntp.el [Gnus]

(defsubst elmo-nntp-next-result-arrived-p ()
  (cond
   ((eq (following-char) ?2)
    (if (re-search-forward "\n\\.\r?\n" nil t)
	t
      nil))
   ((looking-at "[34]")
    (if (search-forward "\n" nil t)
	t
      nil))
   (t
    nil)))

(defun elmo-nntp-retrieve-headers (session outbuf articles)
  "Retrieve the headers of ARTICLES."
  (with-current-buffer (elmo-network-session-buffer session)
    (erase-buffer)
    (let ((number (length articles))
	  (count 0)
	  (received 0)
	  (last-point (point-min))
	  article)
      ;; Send HEAD commands.
      (while (setq article (pop articles))
	(elmo-nntp-send-command session
				(format "head %s" article)
				'noerase)
	(setq count (1+ count))
	;; Every 200 requests we have to read the stream in
	;; order to avoid deadlocks.
	(when (or (null articles)	;All requests have been sent.
		  (zerop (% count elmo-nntp-header-fetch-chop-length)))
	  (accept-process-output
	   (elmo-network-session-process-internal session) 1)
	  (discard-input)
	  (while (progn
		   (goto-char last-point)
		   ;; Count replies.
		   (while (elmo-nntp-next-result-arrived-p)
		     (setq last-point (point))
		     (setq received (1+ received)))
		   (< received count))
	    (when (> number elmo-display-progress-threshold)
	      (if (or (zerop (% received 20)) (= received number))
		  (elmo-display-progress
		   'elmo-nntp-retrieve-headers "Getting headers..."
		   (/ (* received 100) number))))
	    (accept-process-output
	     (elmo-network-session-process-internal session) 1)
	    (discard-input))))
      (when (> number elmo-display-progress-threshold)
	(elmo-display-progress
	 'elmo-nntp-retrieve-headers "Getting headers..." 100))
      (message "Getting headers...done")
      ;; Remove all "\r"'s.
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
	(replace-match "\n"))
      (copy-to-buffer outbuf (point-min) (point-max)))))

;; end of from Gnus

(defun elmo-nntp-msgdb-create-message (len new-mark
					   already-mark seen-mark seen-list)
  (save-excursion
    (let (beg overview number-alist mark-alist
	      entity i num gmark seen message-id)
      (elmo-set-buffer-multibyte nil)
      (goto-char (point-min))
      (setq i 0)
      (message "Creating msgdb...")
      (while (not (eobp))
	(setq beg (save-excursion (forward-line 1) (point)))
	(setq num
	      (and (looking-at "^2[0-9]*[ ]+\\([0-9]+\\)")
		   (string-to-int
		    (elmo-match-buffer 1))))
	(elmo-nntp-next-result-arrived-p)
	(when num
	  (save-excursion
	    (forward-line -1)
	    (save-restriction
	      (narrow-to-region beg (point))
	      (setq entity
		    (elmo-msgdb-create-overview-from-buffer num))
	      (when entity
		(setq overview
		      (elmo-msgdb-append-element
		       overview entity))
		(setq number-alist
		      (elmo-msgdb-number-add
		       number-alist
		       (elmo-msgdb-overview-entity-get-number entity)
		       (car entity)))
		(setq message-id (car entity))
		(setq seen (member message-id seen-list))
		(if (setq gmark
			  (or (elmo-msgdb-global-mark-get message-id)
			      (if (elmo-file-cache-status
				   (elmo-file-cache-get message-id))
				  (if seen
				      nil
				    already-mark)
				(if seen
				    (if elmo-nntp-use-cache
					seen-mark)
				  new-mark))))
		    (setq mark-alist
			  (elmo-msgdb-mark-append
			   mark-alist
			   num gmark)))
		))))
	(when (> len elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (if (or (zerop (% i 20)) (= i len))
	      (elmo-display-progress
	       'elmo-nntp-msgdb-create-message "Creating msgdb..."
	       (/ (* i 100) len)))))
      (when (> len elmo-display-progress-threshold)
	(elmo-display-progress
	 'elmo-nntp-msgdb-create-message "Creating msgdb..." 100))
      (list overview number-alist mark-alist))))

(luna-define-method elmo-message-use-cache-p ((folder elmo-nntp-folder) number)
  elmo-nntp-use-cache)

(luna-define-method elmo-folder-append-msgdb :around
  ((folder elmo-nntp-folder) append-msgdb)
  ;; IMPLEMENT ME: Process crosspost here instead of following.
  (luna-call-next-method))

(require 'product)
(product-provide (provide 'elmo-nntp) (require 'elmo-version))

;;; elmo-nntp.el ends here
