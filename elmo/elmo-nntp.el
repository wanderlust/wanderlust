;;; elmo-nntp.el -- NNTP Interface for ELMO.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news
;; Time-stamp: <00/03/14 19:41:50 teranisi>

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
  (condition-case nil
      (progn
	(require 'starttls))
    (error))
  (require 'elmo-cache)
  (require 'elmo-util)
  (defun-maybe starttls-negotiate (a)))

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

(defvar elmo-nntp-server-command-alist nil)


(defconst elmo-nntp-server-command-index '((xover . 0)
					   (listgroup . 1)
					   (list-active . 2)))

(put 'elmo-nntp-setting 'lisp-indent-function 1)

(defmacro elmo-nntp-setting (spec &rest body)
  (` (let* ((ssl (elmo-nntp-spec-ssl (, spec)))
	    (port (elmo-nntp-spec-port (, spec)))
	    (user (elmo-nntp-spec-username (, spec)))
	    (server (elmo-nntp-spec-hostname (, spec)))
	    (folder (elmo-nntp-spec-group (, spec)))
	    (connection (elmo-nntp-get-connection server user port ssl))
	    (buffer  (car connection))
	    (process (cadr connection)))
       (,@ body))))

(defmacro elmo-nntp-get-server-command (server port)
  (` (assoc (cons (, server) (, port)) elmo-nntp-server-command-alist)))

(defmacro elmo-nntp-set-server-command (server port com value)
  (` (let (entry)
       (unless (setq entry (cdr (elmo-nntp-get-server-command
				 (, server) (, port))))
	 (setq elmo-nntp-server-command-alist
	       (nconc elmo-nntp-server-command-alist
		      (list (cons (cons (, server) (, port))
				  (setq entry
					(vector
					 elmo-nntp-default-use-xover
					 elmo-nntp-default-use-listgroup
					 elmo-nntp-default-use-list-active))
				  )))))
       (aset entry
	     (cdr (assq (, com) elmo-nntp-server-command-index))
	     (, value)))))

(defmacro elmo-nntp-xover-p (server port)
  (` (let ((entry (elmo-nntp-get-server-command (, server) (, port))))
       (if entry
	   (aref (cdr entry)
		 (cdr (assq 'xover elmo-nntp-server-command-index)))
	 elmo-nntp-default-use-xover))))

(defmacro elmo-nntp-set-xover (server port value)
  (` (elmo-nntp-set-server-command (, server) (, port) 'xover (, value))))

(defmacro elmo-nntp-listgroup-p (server port)
  (` (let ((entry (elmo-nntp-get-server-command (, server) (, port))))
       (if entry
	   (aref (cdr entry)
		 (cdr (assq 'listgroup elmo-nntp-server-command-index)))
	 elmo-nntp-default-use-listgroup))))

(defmacro elmo-nntp-set-listgroup (server port value)
  (` (elmo-nntp-set-server-command (, server) (, port) 'listgroup (, value))))

(defmacro elmo-nntp-list-active-p (server port)
  (` (let ((entry (elmo-nntp-get-server-command (, server) (, port))))
       (if entry
	   (aref (cdr entry)
		 (cdr (assq 'list-active elmo-nntp-server-command-index)))
	 elmo-nntp-default-use-list-active))))

(defmacro elmo-nntp-set-list-active (server port value)
  (` (elmo-nntp-set-server-command (, server) (, port) 'list-active (, value))))

(defsubst elmo-nntp-max-number-precedes-list-active-p ()
  elmo-nntp-max-number-precedes-list-active)

(defsubst elmo-nntp-folder-postfix (user server port ssl)
  (concat
   (and user (concat ":" user))
   (if (and server
	    (null (string= server elmo-default-nntp-server)))
       (concat "@" server))
   (if (and port
	    (null (eq port elmo-default-nntp-port)))
       (concat ":" (if (numberp port)
		       (int-to-string port) port)))
   (unless (eq ssl elmo-default-nntp-ssl)
     (if (eq ssl 'starttls)
	 "!!"
       (if ssl "!")))))

(defun elmo-nntp-flush-connection ()
  (interactive)
  (let ((cache elmo-nntp-connection-cache)
	buffer process)
    (while cache
      (setq buffer (car (cdr (car cache))))
      (if buffer (kill-buffer buffer))
      (setq process (car (cdr (cdr (car cache)))))
      (if process (delete-process process))
      (setq cache (cdr cache)))
    (setq elmo-nntp-connection-cache nil)))

(defun elmo-nntp-get-connection (server user port ssl)
  (let* ((user-at-host (format "%s@%s" user server))
	 (user-at-host-on-port (concat 
				user-at-host ":" (int-to-string port)
				(if (eq ssl 'starttls) "!!" (if ssl "!"))))
	 ret-val result buffer process errmsg proc-stat)
    (if (not (elmo-plugged-p server port))
	(error "Unplugged"))
    (setq ret-val (assoc user-at-host-on-port elmo-nntp-connection-cache))
    (if (and ret-val 
	     (or (eq  (setq proc-stat 
			    (process-status (cadr (cdr ret-val))))
		      'closed)
		 (eq proc-stat 'exit)))
	;; connection is closed...
	(progn
	  (kill-buffer (car (cdr ret-val)))
	  (setq elmo-nntp-connection-cache 
		(delete ret-val elmo-nntp-connection-cache))
	  (setq ret-val nil)))
    (if ret-val
	(cdr ret-val)
      (setq result (elmo-nntp-open-connection server user port ssl))
      (if (null result)
	  (progn
	    (if process (delete-process process))
	    (if buffer (kill-buffer buffer))
	    (error "Connection failed"))
	(setq buffer (car result))
	(setq process (cdr result))
	(setq elmo-nntp-connection-cache
	      (nconc elmo-nntp-connection-cache
		     (list
		      (cons user-at-host-on-port
			    (setq ret-val (list buffer process nil))))))
	ret-val))))

(defun elmo-nntp-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)))

(defun elmo-nntp-read-response (buffer process &optional not-command)
  (save-excursion
    (set-buffer buffer)
    (let ((case-fold-search nil)
	  (response-string nil)
	  (response-continue t)
	  (return-value nil)
	  match-end)
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
		   (setq return-value 
			 (if return-value 
			     (concat return-value "\n" response-string)
			   response-string)))
	  (if (looking-at "[^23][0-9]+ .*$")
	      (progn (setq response-continue nil)
		     (setq elmo-nntp-read-point match-end)
		     (setq return-value nil))
	    (setq elmo-nntp-read-point match-end)
	    (if not-command
		(setq response-continue nil))
	    (setq return-value 
		  (if return-value 
		      (concat return-value "\n" response-string)
		    response-string)))
	  (setq elmo-nntp-read-point match-end)))
      return-value)))

(defun elmo-nntp-read-raw-response (buffer process)
  (save-excursion
    (set-buffer buffer)
    (let ((case-fold-search nil))
      (goto-char elmo-nntp-read-point)
      (while (not (search-forward "\r\n" nil t))
	(accept-process-output process)
	(goto-char elmo-nntp-read-point))
      (buffer-substring elmo-nntp-read-point (- (point) 2)))))

(defun elmo-nntp-read-contents (buffer process)
  (save-excursion
    (set-buffer buffer)
    (let ((case-fold-search nil)
	  match-end)
      (goto-char elmo-nntp-read-point)
      (while (not (re-search-forward "^\\.\r\n" nil t))
	(accept-process-output process)
	(goto-char elmo-nntp-read-point))
      (setq match-end (point))
      (elmo-delete-cr
       (buffer-substring elmo-nntp-read-point 
			 (- match-end 3))))))

(defun elmo-nntp-read-body (buffer process outbuf)
  (with-current-buffer buffer
    (let ((start elmo-nntp-read-point)
	  end)
      (goto-char start)
      (while (not (re-search-forward "^\\.\r\n" nil t))
	(accept-process-output process)
	(goto-char start))
      (setq end (point))
      (with-current-buffer outbuf
	(erase-buffer)
	(insert-buffer-substring buffer start (- end 3))
	(elmo-delete-cr-get-content-type)))))

(defun elmo-nntp-goto-folder (server folder user port ssl)
  (let* ((connection (elmo-nntp-get-connection server user port ssl))
	 (buffer  (car connection))
	 (process (cadr connection))
	 (cwf     (caddr connection)))
    (save-excursion
      (condition-case ()
	  (if (not (string= cwf folder))
	      (progn
		(elmo-nntp-send-command buffer 
					process 
					(format "group %s" folder))
		(if (elmo-nntp-read-response buffer process)
		    (setcar (cddr connection) folder)))
	    t)
	(error
	 nil)))))

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
	  (and folder
	       (keep-lines (concat "^" (regexp-quote folder) "\\.")))
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

(defun elmo-nntp-list-folders (spec &optional hierarchy)
  (elmo-nntp-setting spec
   (let* ((cwf     (caddr connection))	 
	  (tmp-buffer (get-buffer-create " *ELMO NNTP list folders TMP*"))
	  response ret-val top-ng append-serv use-list-active start)
    (save-excursion
      (set-buffer tmp-buffer)
      (if (and folder
	       (elmo-nntp-goto-folder server folder user port ssl))
	  (setq ret-val (list folder))) ;; add top newsgroups
      (unless (setq response (elmo-nntp-list-folders-get-cache
			      folder tmp-buffer))
	(when (setq use-list-active (elmo-nntp-list-active-p server port))
	  (elmo-nntp-send-command buffer
				  process
				  (concat "list"
					  (if (and folder
						   (null (string= folder "")))
					      (concat " active"
						      (format " %s.*" folder) ""))))
	  (if (elmo-nntp-read-response buffer process t)
	      (if (null (setq response (elmo-nntp-read-contents
					buffer process)))
		  (error "NNTP List folders failed")
		(when elmo-nntp-list-folders-use-cache
		  (setq elmo-nntp-list-folders-cache
			(list (current-time) folder response)))
		(erase-buffer)
		(insert response))
	    (elmo-nntp-set-list-active server port nil)
	    (setq use-list-active nil)))
	(when (null use-list-active)
	  (elmo-nntp-send-command buffer process "list")
	  (if (null (and (elmo-nntp-read-response buffer process t)
			 (setq response (elmo-nntp-read-contents
					 buffer process))))
	      (error "NNTP List folders failed"))
	  (when elmo-nntp-list-folders-use-cache
	    (setq elmo-nntp-list-folders-cache
		  (list (current-time) nil response)))
	  (erase-buffer)
	  (setq start nil)
	  (while (string-match (concat "^"
				       (regexp-quote
					(or folder "")) ".*$")
			       response start)
	    (insert (match-string 0 response) "\n")
	    (setq start (match-end 0)))))
      (goto-char (point-min))
      (let ((len (count-lines (point-min) (point-max)))
	    (i 0) regexp)
	(if hierarchy
	    (progn
	      (setq regexp
		    (format "^\\(%s[^. ]+\\)\\([. ]\\).*\n"
			    (if folder (concat folder "\\.") "")))
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
		(setq i (1+ i))
		(and (zerop (% i 10))
		     (elmo-display-progress
		      'elmo-nntp-list-folders "Parsing active..."
		      (/ (* i 100) len)))
		(forward-line 1)
		))
	  (while (re-search-forward "\\([^ ]+\\) .*\n" nil t)
	    (setq ret-val (nconc ret-val
				 (list (elmo-match-buffer 1))))
	    (setq i (1+ i))
	    (and (zerop (% i 10))
		 (elmo-display-progress
		  'elmo-nntp-list-folders "Parsing active..."
		  (/ (* i 100) len))))))
      (kill-buffer tmp-buffer)
      (unless (string= server elmo-default-nntp-server)
	(setq append-serv (concat "@" server)))
      (unless (eq port elmo-default-nntp-port)
	(setq append-serv (concat append-serv ":" (int-to-string port))))
      (unless (eq ssl elmo-default-nntp-ssl)
	(if ssl
	    (setq append-serv (concat append-serv "!")))
	(if (eq ssl 'starttls)
	    (setq append-serv (concat append-serv "!"))))
      (mapcar '(lambda (fld)
		 (if (consp fld)
		     (list (concat "-" (car fld)
				   (and user
					(concat ":" user))
				   (and append-serv
					(concat append-serv))))
		   (concat "-" fld
			   (and user
				(concat ":" user))
			   (and append-serv 
				(concat append-serv)))))
	      ret-val)))))

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

(defun elmo-nntp-list-folder (spec)
  (elmo-nntp-setting spec
   (let* ((server (format "%s" server)) ;; delete text property
	  response retval use-listgroup)
    (save-excursion
      (when (setq use-listgroup (elmo-nntp-listgroup-p server port))
	(elmo-nntp-send-command buffer
				process
				(format "listgroup %s" folder))
	(if (not (elmo-nntp-read-response buffer process t))
	    (progn
	      (elmo-nntp-set-listgroup server port nil)
	      (setq use-listgroup nil))
	  (if (null (setq response (elmo-nntp-read-contents buffer process)))
	      (error "Fetching listgroup failed"))
	  (setq retval (elmo-string-to-list response))))
      (if use-listgroup
	  retval
	(elmo-nntp-send-command buffer 
				process 
				(format "group %s" folder))
	(if (null (setq response (elmo-nntp-read-response buffer process)))
	    (error "Select folder failed"))
	(setcar (cddr connection) folder)
	(if (and
	     (string-match "211 \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) [^.].+$" 
			   response)
	     (> (string-to-int (elmo-match-string 1 response)) 0))
	    (elmo-nntp-make-msglist
	     (elmo-match-string 2 response)
	     (elmo-match-string 3 response))
	  nil))))))

(defun elmo-nntp-max-of-folder (spec)
  (let* ((port (elmo-nntp-spec-port spec))
	 (user (elmo-nntp-spec-username spec))
	 (server (elmo-nntp-spec-hostname spec))
	 (ssl (elmo-nntp-spec-ssl spec))
	 (folder (elmo-nntp-spec-group spec)))
    (if elmo-nntp-groups-async
	(let* ((fld (concat folder
			    (elmo-nntp-folder-postfix user server port ssl)))
	       (entry (elmo-get-hash-val fld elmo-nntp-groups-hashtb)))
	  (if entry
	      (cons (nth 2 entry)
		    (car entry))
	    (error "No such newsgroup \"%s\"" fld)))
      (let* ((connection (elmo-nntp-get-connection server user port ssl))
	     (buffer  (car connection))
	     (process (cadr connection))
	     response e-num end-num)
	(if (not connection)
	    (error "Connection failed"))
	(save-excursion
	  (elmo-nntp-send-command buffer 
				  process 
				  (format "group %s" folder))
	  (setq response (elmo-nntp-read-response buffer process))
	  (if (and response 
		   (string-match 
		    "211 \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) [^.].+$" 
		    response))
	      (progn
		(setq end-num (string-to-int
			       (elmo-match-string 3 response)))
		(setq e-num (string-to-int
			     (elmo-match-string 1 response)))
		(cons end-num e-num))
	    (if (null response)
		(error "Selecting newsgroup \"%s\" failed" folder)
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
						    folder
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
      ;; INN bug??
;      (if (or (> (setq num (string-to-int (aref ov-entity 0)))
;		 99999)
;	      (<= num 0))
;	  (setq num 0))
;     (setq num (int-to-string num))
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
			    (if (elmo-cache-exists-p message-id);; XXX
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

(defun elmo-nntp-msgdb-create-as-numlist (spec numlist new-mark already-mark
					       seen-mark important-mark
					       seen-list)
  "Create msgdb for SPEC for NUMLIST."
  (elmo-nntp-msgdb-create spec numlist new-mark already-mark
			  seen-mark important-mark seen-list
			  t))

(defun elmo-nntp-msgdb-create (spec numlist new-mark already-mark
				    seen-mark important-mark 
				    seen-list &optional as-num)
  (when numlist
    (save-excursion
     (elmo-nntp-setting spec
      (let* ((cwf     (caddr connection))
	     (filter  (and as-num numlist))
	     beg-num end-num cur length
	     ret-val ov-str use-xover)
	(if (and folder
		 (not (string= cwf folder))
		 (null (elmo-nntp-goto-folder server folder user port ssl)))
	    (error "group %s not found" folder))
	(when (setq use-xover (elmo-nntp-xover-p server port))
	  (setq beg-num (car numlist)
		cur beg-num
		end-num (nth (1- (length numlist)) numlist)
		length  (+ (- end-num beg-num) 1))
	  (message "Getting overview...")
	  (while (<= cur end-num)
	    (elmo-nntp-send-command buffer process 
				    (format 
				     "xover %s-%s" 
				     (int-to-string cur)
				     (int-to-string 
				      (+ cur 
					 elmo-nntp-overview-fetch-chop-length))))
	    (with-current-buffer buffer
	      (if ov-str
		  (setq ret-val 
			(elmo-msgdb-append
			 ret-val
			 (elmo-nntp-create-msgdb-from-overview-string 
			  ov-str
			  folder
			  new-mark
			  already-mark
			  seen-mark
			  important-mark
			  seen-list
			  filter
			  )))))
	    (if (null (elmo-nntp-read-response buffer process t))
		(progn
		  (setq cur end-num);; exit while loop
		  (elmo-nntp-set-xover server port nil)
		  (setq use-xover nil))
	      (if (null (setq ov-str (elmo-nntp-read-contents buffer process)))
		  (error "Fetching overview failed")))
	    (setq cur (+ elmo-nntp-overview-fetch-chop-length cur 1))
	    (elmo-display-progress
	     'elmo-nntp-msgdb-create "Getting overview..." 
	     (/ (* (+ (- (min cur
			      end-num)
			 beg-num) 1) 100) length))))
	(if (not use-xover)
	    (setq ret-val (elmo-nntp-msgdb-create-by-header
			   folder buffer process numlist
			   new-mark already-mark seen-mark seen-list))
	  (with-current-buffer buffer
	    (if ov-str
		(setq ret-val 
		      (elmo-msgdb-append
		       ret-val
		       (elmo-nntp-create-msgdb-from-overview-string 
			ov-str
			folder
			new-mark
			already-mark
			seen-mark
			important-mark
			seen-list
			filter)))))
	  (message "Getting overview...done."))
	;; If there are canceled messages, overviews are not obtained
	;; to max-number(inn 2.3?).
	(when (and (elmo-nntp-max-number-precedes-list-active-p)
		   (elmo-nntp-list-active-p server port))
	  (elmo-nntp-send-command buffer process 
				  (format "list active %s" folder))
	  (if (null (elmo-nntp-read-response buffer process))
	      (progn
		(elmo-nntp-set-list-active server port nil)
		(error "NNTP list command failed")))
	  (elmo-nntp-catchup-msgdb 
	   ret-val 
	   (nth 1 (read (concat "(" (elmo-nntp-read-contents 
				     buffer process) ")")))))
	ret-val)))))

(defun elmo-nntp-sync-number-alist (spec number-alist)
  (if (elmo-nntp-max-number-precedes-list-active-p)
      (elmo-nntp-setting spec
	(if (elmo-nntp-list-active-p server port)
	    (let* ((cwf (caddr connection))
		   msgdb-max max-number)
	      ;; If there are canceled messages, overviews are not obtained
	      ;; to max-number(inn 2.3?).
	      (if (and folder
		       (not (string= cwf folder))
		       (null (elmo-nntp-goto-folder
			      server folder user port ssl)))
		  (error "group %s not found" folder))
	      (elmo-nntp-send-command buffer process
				      (format "list active %s" folder))
	      (if (null (elmo-nntp-read-response buffer process))
		  (error "NNTP list command failed"))
	      (setq max-number
		    (nth 1 (read (concat "(" (elmo-nntp-read-contents
					      buffer process) ")"))))
	      (setq msgdb-max
		    (car (nth (max (- (length number-alist) 1) 0)
			      number-alist)))
	      (if (or (and number-alist (not msgdb-max))
		      (and msgdb-max max-number
			   (< msgdb-max max-number)))
		  (nconc number-alist
			 (list (cons max-number nil)))
		number-alist))
	  number-alist))))

(defun elmo-nntp-msgdb-create-by-header (folder buffer process numlist
						new-mark already-mark 
						seen-mark seen-list)
  (let ((tmp-buffer (get-buffer-create " *ELMO Overview TMP*"))
	ret-val)
    (elmo-nntp-retrieve-headers
     buffer tmp-buffer process numlist)
    (setq ret-val
	  (elmo-nntp-msgdb-create-message
	   tmp-buffer (length numlist) folder new-mark already-mark 
	   seen-mark seen-list))
    (kill-buffer tmp-buffer)
    ret-val))

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
;      (kill-buffer tmp-buffer)
      ret-val)))

(defun elmo-nntp-get-overview (server beg end folder user port ssl)
  (save-excursion
    (let* ((connection (elmo-nntp-get-connection server user port ssl))
	   (buffer  (car connection))
	   (process (cadr connection))
;	   (cwf     (caddr connection))	 
	   response errmsg ov-str)  
      (catch 'done
	(if folder
	    (if (null (elmo-nntp-goto-folder server folder user port ssl))
		(progn
		  (setq errmsg (format "group %s not found." folder))
		  (throw 'done nil))))
	(elmo-nntp-send-command buffer process 
				(format "xover %s-%s" beg end))
	(if (null (setq response (elmo-nntp-read-response
				  buffer process t)))
	    (progn
	      (setq errmsg "Getting overview failed.")
	      (throw 'done nil)))
	(if (null (setq response (elmo-nntp-read-contents
				  buffer process)))
	    (progn
	      ;(setq errmsg "Fetching header failed")
	      (throw 'done nil)))
	(setq ov-str response)
	)
      (if errmsg
	  (progn 
	    (message errmsg)
	    nil)
	ov-str))))


(defun elmo-nntp-get-message (server user number folder outbuf port ssl)
  "Get nntp message on FOLDER at SERVER. 
Returns message string."
  (save-excursion
    (let* ((connection (elmo-nntp-get-connection server user port ssl))
	   (buffer  (car connection))
	   (process (cadr connection))
	   (cwf     (caddr connection))	 
	   response errmsg)
      (catch 'done
	(if (and folder
		 (not (string= cwf folder)))
	    (if (null (elmo-nntp-goto-folder server folder user port ssl))
		(progn
		  (setq errmsg (format "group %s not found." folder))
		  (throw 'done nil))))
	(elmo-nntp-send-command buffer process 
				(format "article %s" number))
	(if (null (setq response (elmo-nntp-read-response
				  buffer process t)))
	    (progn
	      (setq errmsg "Fetching message failed")
	      (set-buffer outbuf)
	      (erase-buffer)
	      (insert "\n\n")
	      (throw 'done nil)))
	(setq response (elmo-nntp-read-body buffer process outbuf))
	(set-buffer outbuf)
	(goto-char (point-min))
	(while (re-search-forward "^\\." nil t)
	  (replace-match "")
	  (forward-line))
	)
      (if errmsg
	  (progn 
	    (message errmsg)
	    nil))
      response)))

(defun elmo-nntp-get-newsgroup-by-msgid (msgid server user port ssl)
  "Get nntp header string."
  (save-excursion
    (let* ((connection (elmo-nntp-get-connection server user port ssl))
	   (buffer  (car connection))
	   (process (cadr connection)))
      (elmo-nntp-send-command buffer process 
			      (format "head %s" msgid))
      (if (elmo-nntp-read-response buffer process)
	  (elmo-nntp-read-contents buffer process))
      (set-buffer buffer)
      (std11-field-body "Newsgroups"))))

(defun elmo-nntp-open-connection (server user portnum ssl)
  "Open NNTP connection and returns 
the list of (process session-buffer current-working-folder).
Return nil if connection failed."
  (let ((process nil)
	(host server)
	(port (or portnum
		  elmo-default-nntp-port))
	(user-at-host (format "%s@%s" user server))
	process-buffer)
    (as-binary-process
     (catch 'done
       (setq process-buffer
	     (get-buffer-create (format " *NNTP session to %s:%d" host port)))
       (save-excursion
	 (set-buffer process-buffer)
	 (elmo-set-buffer-multibyte nil)
	 (erase-buffer))
       (setq process
	     (elmo-open-network-stream "NNTP" process-buffer host port ssl))
       (and (null process) (throw 'done nil))
       (set-process-filter process 'elmo-nntp-process-filter)
       ;; flush connections when exiting...?
       ;; (add-hook 'kill-emacs-hook 'elmo-nntp-flush-connection)
       (save-excursion
	 (set-buffer process-buffer)
	 (elmo-set-buffer-multibyte nil)
	 (make-local-variable 'elmo-nntp-read-point)
	 (setq elmo-nntp-read-point (point-min))
	 (if (null (elmo-nntp-read-response process-buffer process t))
	     (throw 'done nil))
	 (if elmo-nntp-send-mode-reader
	     (elmo-nntp-send-mode-reader process-buffer process))
 	 ;; starttls
 	 (if (eq ssl 'starttls)
 	     (if (progn
		   (elmo-nntp-send-command process-buffer process "starttls")
		   (elmo-nntp-read-response process-buffer process))
 		 (starttls-negotiate process)
 	       (error "STARTTLS aborted")))
 	 (if user
 	     (progn
 	       (elmo-nntp-send-command process-buffer process
 				       (format "authinfo user %s" user))
 	       (if (null (elmo-nntp-read-response process-buffer process))
 		   (error "Authinfo failed"))
 	       (elmo-nntp-send-command process-buffer process
 				       (format "authinfo pass %s"
 					       (elmo-get-passwd user-at-host)))
 	       (if (null (elmo-nntp-read-response process-buffer process))
 		   (progn
 		     (elmo-remove-passwd user-at-host)
 		     (error "Authinfo failed")))))
	 (run-hooks 'elmo-nntp-opened-hook)) ; XXX
       (cons process-buffer process)))))

(defun elmo-nntp-send-mode-reader (buffer process)
  (elmo-nntp-send-command buffer
			  process
			  "mode reader")
  (if (null (elmo-nntp-read-response buffer process t))
      (error "mode reader failed")))
  
(defun elmo-nntp-send-command (buffer process command &optional noerase)
  "Send COMMAND string to server with sequence number."
  (save-excursion
    (set-buffer buffer)
    (when (not noerase)
      (erase-buffer)
      (goto-char (point-min)))
    (setq elmo-nntp-read-point (point))
    (process-send-string process command)
    (process-send-string process "\r\n")))

(defun elmo-nntp-read-msg (spec msg outbuf)
  (elmo-nntp-get-message (elmo-nntp-spec-hostname spec)
			 (elmo-nntp-spec-username spec)
			 msg 
			 (elmo-nntp-spec-group spec)
			 outbuf 
			 (elmo-nntp-spec-port spec)
			 (elmo-nntp-spec-ssl spec)))

;(defun elmo-msgdb-nntp-overview-create-range (spec beg end mark)
;    (elmo-nntp-overview-create-range hostname beg end mark folder)))

;(defun elmo-msgdb-nntp-max-of-folder (spec)
;    (elmo-nntp-max-of-folder hostname folder)))

(defun elmo-nntp-append-msg (spec string &optional msg no-see))

(defun elmo-nntp-post (hostname content-buf)
  (let* (;(folder (nth 1 spec))
	 (connection 
	  (elmo-nntp-get-connection 
	   hostname 
	   elmo-default-nntp-user
	   elmo-default-nntp-port elmo-default-nntp-ssl))
	 (buffer (car connection))
	 (process (cadr connection))
	 response has-message-id
	 )
    (save-excursion
      (set-buffer content-buf)
      (goto-char (point-min))
      (if (search-forward mail-header-separator nil t)
	  (delete-region (match-beginning 0)(match-end 0)))
      (setq has-message-id (std11-field-body "message-id"))
      (elmo-nntp-send-command buffer process "post")
      (if (string-match "^340" (setq response 
				     (elmo-nntp-read-raw-response 
				      buffer process)))
	  (if (string-match "recommended ID \\(<[^@]+@[^>]+>\\)" response)
	      (unless has-message-id
		(goto-char (point-min))
		(insert (concat "Message-ID: "
				(elmo-match-string 1 response)
				"\n"))))
	(error "POST failed"))
      (current-buffer)
      (run-hooks 'elmo-nntp-post-pre-hook)
      (set-buffer buffer)
      (elmo-nntp-send-data process content-buf)
      (elmo-nntp-send-command buffer process ".")
      ;(elmo-nntp-read-response buffer process t)
      (if (not (string-match 
		"^2" (setq response (elmo-nntp-read-raw-response
				     buffer process))))
	  (error (concat "NNTP error: " response))))))

(defun elmo-nntp-send-data-line (process data)
  (goto-char (point-max))

  ;; Escape "." at start of a line
  (if (eq (string-to-char data) ?.)
      (process-send-string process "."))
  (process-send-string process data)
  (process-send-string process "\r\n"))

(defun elmo-nntp-send-data (process buffer)
  (let
      ((data-continue t)
       (sending-data nil)
       this-line
       this-line-end)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min)))

    (while data-continue
      (save-excursion
	(set-buffer buffer)
	(beginning-of-line)
	(setq this-line (point))
	(end-of-line)
	(setq this-line-end (point))
	(setq sending-data nil)
	(setq sending-data (buffer-substring this-line this-line-end))
	(if (/= (forward-line 1) 0)
	    (setq data-continue nil)))

      (elmo-nntp-send-data-line process sending-data))))


(defun elmo-nntp-delete-msgs (spec msgs)
  "MSGS on FOLDER at SERVER pretended as Deleted. Returns nil if failed."
  (let* ((dir (elmo-msgdb-expand-path nil spec))
;	 (msgs (mapcar 'string-to-int msgs))
	 (killed-list (elmo-msgdb-killed-list-load dir)))
    (mapcar '(lambda (msg)
	       (setq killed-list
		     (elmo-msgdb-set-as-killed killed-list msg)))
	    msgs)
    (elmo-msgdb-killed-list-save dir killed-list)
    t))

(defun elmo-nntp-check-validity (spec validity-file)
  t)
(defun elmo-nntp-sync-validity (spec validity-file)
  t)

(defun elmo-nntp-folder-exists-p (spec)
  (if (elmo-nntp-plugged-p spec)
      (elmo-nntp-setting spec
	(elmo-nntp-send-command buffer
				process
				(format "group %s" folder))
	(elmo-nntp-read-response buffer process))
    t))

(defun elmo-nntp-folder-creatable-p (spec)
  nil)

(defun elmo-nntp-create-folder (spec)
  nil) ; noop

(defun elmo-nntp-search (spec condition &optional from-msgs)
  (error "Search by %s for %s is not implemented yet." condition (car spec))
  nil)

(defun elmo-nntp-get-folders-info-prepare (spec connection-keys)
  (condition-case ()
      (elmo-nntp-setting spec
	(let (key count)
	  (save-excursion
	    (set-buffer buffer)
	    (unless (setq key (assoc (cons buffer process) connection-keys))
	      (erase-buffer)
	      (setq key (cons (cons buffer process)
			      (vector 0 server user port ssl)))
	      (setq connection-keys (nconc connection-keys (list key))))
	    (elmo-nntp-send-command buffer 
				    process 
				    (format "group %s" folder)
				    t ;; don't erase-buffer
				    )
	    (if elmo-nntp-get-folders-securely
		(accept-process-output process 1))
	    (setq count (aref (cdr key) 0))
	    (aset (cdr key) 0 (1+ count)))))
    (error
     (when elmo-auto-change-plugged
       (sit-for 1))
     nil))
  connection-keys)

(defun elmo-nntp-get-folders-info (connection-keys)
  (let ((connections connection-keys)
	(cur (get-buffer-create " *ELMO NNTP Temp*")))
    (while connections
      (let* ((connect (caar connections))
	     (key     (cdar connections))
	     (buffer  (car connect))
	     (process (cdr connect))
	     (count   (aref key 0))
	     (server  (aref key 1))
	     (user    (aref key 2))
	     (port    (aref key 3))
	     (ssl     (aref key 4))
	     (hashtb (or elmo-nntp-groups-hashtb
			 (setq elmo-nntp-groups-hashtb
			       (elmo-make-hash count)))))
	(save-excursion
	  (elmo-nntp-groups-read-response buffer cur process count)
	  (set-buffer cur)
	  (goto-char (point-min))
	  (let ((case-replace nil)
		(postfix (elmo-nntp-folder-postfix user server port ssl)))
	    (if (not (string= postfix ""))
		(save-excursion
		  (replace-regexp "^\\(211 [0-9]+ [0-9]+ [0-9]+ [^ \n]+\\).*$"
				  (concat "\\1" postfix)))))
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
	(setq connections (cdr connections))))
    (kill-buffer cur)))

;; original is 'nntp-retrieve-groups [Gnus]
(defun elmo-nntp-groups-read-response (buffer tobuffer process count)
  (let* ((received 0)
	 (last-point (point-min)))
    (save-excursion
      (set-buffer buffer)
      (accept-process-output process 1)
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
	(accept-process-output process 1)
	(discard-input)
	(and (zerop (% received 10))
	     (elmo-display-progress
	      'elmo-nntp-groups-read-response "Getting folders info..."
	      (/ (* received 100) count)))
	)
      ;; Wait for the reply from the final command.
      (goto-char (point-max))
      (re-search-backward "^[0-9]" nil t)
      (when (looking-at "^[23]")
	(while (progn
		 (goto-char (point-max))
		 (not (re-search-backward "\r?\n" (- (point) 3) t)))
	  (accept-process-output process 1)
	  (discard-input)))
      ;; Now all replies are received.  We remove CRs.
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "" t t))
      (copy-to-buffer tobuffer (point-min) (point-max)))))

(defun elmo-nntp-make-groups-hashtb (folders &optional size)
  (let ((hashtb (or elmo-nntp-groups-hashtb
		    (setq elmo-nntp-groups-hashtb
			  (elmo-make-hash (or size (length folders)))))))
    (mapcar
     '(lambda (fld)
	(or (elmo-get-hash-val fld hashtb)
	    (elmo-set-hash-val fld nil hashtb)))
     folders)
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

(defun elmo-nntp-retrieve-headers (buffer tobuffer process articles)
  "Retrieve the headers of ARTICLES."
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (let ((number (length articles))
	  (count 0)
	  (received 0)
	  (last-point (point-min))
	  article)
      ;; Send HEAD commands.
      (while (setq article (pop articles))
	(elmo-nntp-send-command
	 buffer
	 process
	 (format "head %s" article)
	 t ;; not erase-buffer
	 )
	(setq count (1+ count))
	;; Every 200 requests we have to read the stream in
	;; order to avoid deadlocks.
	(when (or (null articles)	;All requests have been sent.
		  (zerop (% count elmo-nntp-header-fetch-chop-length)))
	  (accept-process-output process 1)
	  (discard-input)
	  (while (progn
		   (set-buffer buffer)
		   (goto-char last-point)
		   ;; Count replies.
		   (while (elmo-nntp-next-result-arrived-p)
		     (setq last-point (point))
		     (setq received (1+ received)))
		   (< received count))
	    (and (zerop (% received 20))
		 (elmo-display-progress
		  'elmo-nntp-retrieve-headers "Getting headers..."
		  (/ (* received 100) number)))
	    (accept-process-output process 1)
	    (discard-input)
	    )))
      (message "Getting headers...done")
      ;; Remove all "\r"'s.
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
	(replace-match "\n"))
      (copy-to-buffer tobuffer (point-min) (point-max)))))

;; end of from Gnus

(defun elmo-nntp-msgdb-create-message (buffer len folder new-mark 
					      already-mark seen-mark seen-list)
  (save-excursion
    (let (beg
	  overview number-alist mark-alist
	  entity i num gmark seen message-id)
      (set-buffer buffer)
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
		      (elmo-msgdb-number-add number-alist
					     (elmo-msgdb-overview-entity-get-number entity)
					     (car entity)))
		(setq message-id (car entity))
		(setq seen (member message-id seen-list))
		(if (setq gmark 
			  (or (elmo-msgdb-global-mark-get message-id)
			      (if (elmo-cache-exists-p message-id);; XXX
				  (if seen
				      nil
				    already-mark)
				(if seen
				    seen-mark
				  new-mark))))
		    (setq mark-alist
			  (elmo-msgdb-mark-append 
			   mark-alist 
			   num gmark)))
		))))
	(setq i (1+ i))
	(and (zerop (% i 20))
	     (elmo-display-progress
	      'elmo-nntp-msgdb-create-message "Creating msgdb..."
	      (/ (* i 100) len)))
	)
      (message "Creating msgdb...done.")
      (list overview number-alist mark-alist))))

(defun elmo-nntp-use-cache-p (spec number)
  elmo-nntp-use-cache)

(defun elmo-nntp-local-file-p (spec number)
  nil)

(defun elmo-nntp-port-label (spec)
  (concat "nntp"
	  (if (elmo-nntp-spec-ssl spec) "!ssl" "")))

(defsubst elmo-nntp-portinfo (spec)
  (list (elmo-nntp-spec-hostname spec) 
	(elmo-nntp-spec-port spec)))

(defun elmo-nntp-plugged-p (spec)
  (apply 'elmo-plugged-p
	 (append (elmo-nntp-portinfo spec)
		 (list nil (quote (elmo-nntp-port-label spec))))))

(defun elmo-nntp-set-plugged (spec plugged add)
  (apply 'elmo-set-plugged plugged
	 (append (elmo-nntp-portinfo spec)
		 (list nil nil (quote (elmo-nntp-port-label spec)) add))))

(defalias 'elmo-nntp-list-folder-unread 
  'elmo-generic-list-folder-unread)
(defalias 'elmo-nntp-list-folder-important
  'elmo-generic-list-folder-important)
(defalias 'elmo-nntp-commit 'elmo-generic-commit)

(provide 'elmo-nntp)

;;; elmo-nntp.el ends here
