;;; elmo2.el -- ELMO main file (I don't remember why this is 2).

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

(require 'elmo-vars)
(require 'elmo-msgdb)
(require 'elmo-cache)
(require 'elmo-util)
(require 'elmo-dop)
(require 'product)
(product-provide (provide 'elmo2) (require 'elmo-version))

(eval-when-compile
  (require 'elmo-localdir)
  (require 'elmo-imap4)
  (require 'elmo-nntp)
  (require 'elmo-pop3)
  (require 'elmo-pipe)
;  (require 'elmo-multi)
  (require 'elmo-filter)
  (require 'elmo-archive)
  ;(require 'elmo-cache2)
  )

(if (or (featurep 'dbm)
	(featurep 'gnudbm)
	(featurep 'berkdb)
	(featurep 'berkeley-db))
    (require 'elmo-database))

(elmo-define-error 'elmo-error "Error" 'error)
(elmo-define-error 'elmo-open-error "Cannot open" 'elmo-error)
(elmo-define-error 'elmo-authenticate-error "Login failed" 'elmo-open-error)
(elmo-define-error 'elmo-imap4-bye-error "IMAP4 BYE response" 'elmo-open-error)

(defun elmo-quit ()
  (interactive)
  (if (featurep 'elmo-net)
      (elmo-network-clear-session-cache))
  (if (get-buffer elmo-work-buf-name)
      (kill-buffer elmo-work-buf-name)))

(defun elmo-cleanup-variables ()
  (setq elmo-folder-info-hashtb nil
	elmo-nntp-groups-hashtb nil
	elmo-nntp-list-folders-cache nil
	))

;;  (cons of max . estimated message number) elmo-max-of-folder (folder)
(defun elmo-max-of-folder (folder)
  (if (elmo-folder-plugged-p folder)
      (elmo-call-func folder "max-of-folder")
    (elmo-dop-max-of-folder folder)))

;;  list elmo-list-folder (folder)
(defun elmo-list-folder (folder)
  (if (elmo-folder-plugged-p folder)
      (elmo-call-func folder "list-folder")
    (elmo-dop-list-folder folder)))

;;  list elmo-list-folders (folder)
(defun elmo-list-folders (folder &optional hierarchy)
  (elmo-call-func folder "list-folders" hierarchy))

;; bool elmo-folder-exists-p (folder)
(defun elmo-folder-exists-p (folder)
  (if (elmo-folder-plugged-p folder)
      (elmo-call-func folder "folder-exists-p")
    (elmo-dop-folder-exists-p folder)))

;; bool elmo-folder-creatable-p (folder)
(defun elmo-folder-creatable-p (folder)
  (elmo-call-func folder "folder-creatable-p"))

;; bool elmo-create-folder (folder)
;; create folder
(defun elmo-create-folder (folder)
  (if (elmo-folder-plugged-p folder)
      (elmo-call-func folder "create-folder")
    (elmo-dop-create-folder folder)))

(defun elmo-delete-folder (folder)
  (let ((type (elmo-folder-get-type folder)))
    (if (or (not (memq type '(localdir localnews archive imap4 maildir)))
	    (if (elmo-folder-plugged-p folder)
		(elmo-call-func folder "delete-folder")
	      (elmo-dop-delete-folder folder)))
	;; If folder doesn't support delete folder, delete msgdb path only.
	(elmo-msgdb-delete-path folder))))

(defun elmo-rename-folder (old-folder new-folder)
  (let ((old-type (elmo-folder-get-type old-folder))
	(new-type (elmo-folder-get-type new-folder)))
    (if (not (eq old-type new-type))
	(error "not same folder type")
      (unless (and (memq old-type '(localdir localnews archive imap4))
		   (elmo-folder-identical-system-p old-folder new-folder))
	(error "rename folder not supported"))
      (if (elmo-folder-plugged-p old-folder)
	  (and
	   (if (or (file-exists-p (elmo-msgdb-expand-path new-folder))
		   (elmo-folder-exists-p new-folder))
	       (error "already exists folder: %s" new-folder)
	     t)
	   (elmo-call-func old-folder "rename-folder"
			   (elmo-folder-get-spec new-folder))
	   (elmo-msgdb-rename-path old-folder new-folder))
	(elmo-dop-rename-folder old-folder new-folder)))))

(defun elmo-read-msg-no-cache (folder msg outbuf)
  "Read messsage specified by FOLDER and MSG(number) into OUTBUF
without cacheing."
  (elmo-call-func folder "read-msg" msg outbuf))

(defun elmo-force-cache-msg (folder number msgid &optional loc-alist)
  "Force cache message."
  (let* ((cache-file (elmo-cache-get-path msgid))
	 dir)
    (when cache-file
      (setq dir (directory-file-name (file-name-directory cache-file)))
      (if (not (file-exists-p dir))
	  (elmo-make-directory dir))
      (if (elmo-local-file-p folder number)
	  (elmo-copy-file (elmo-get-msg-filename folder number loc-alist)
			  cache-file)
	(with-temp-buffer
	  (elmo-call-func folder "read-msg" number (current-buffer))
	  (as-binary-output-file
	   (write-region (point-min) (point-max) cache-file nil 'no-msg)))))))

(defun elmo-prefetch-msg (folder msg outbuf msgdb)
  "Read message into outbuf with cacheing."
  (save-excursion
    (let* ((number-alist (elmo-msgdb-get-number-alist
			  (or msgdb (elmo-msgdb-load folder))))
	   (dir (elmo-msgdb-expand-path folder))
	   (message-id (cdr (assq msg number-alist)))
	   type
	   cache-status
	   ret-val part-num real-fld-num)
      (set-buffer outbuf)
      (if (elmo-cache-exists-p message-id)
	  t
	;; cache doesn't exist.
	(setq real-fld-num (elmo-get-real-folder-number
			    folder msg))
	(setq type (elmo-folder-get-type (car real-fld-num)))
	(cond ((eq type 'imap4)
	       (setq ret-val (elmo-imap4-prefetch-msg
			      (elmo-folder-get-spec (car real-fld-num))
			      (cdr real-fld-num)
			      outbuf)))
	      ((elmo-folder-local-p (car real-fld-num)))
	      (t (setq ret-val (elmo-call-func (car real-fld-num)
					       "read-msg"
					       (cdr real-fld-num) outbuf))))
	(if ret-val
	    (elmo-cache-save message-id
			     (elmo-string-partial-p ret-val)
			     folder msg))
	(and ret-val t)))))

(defun elmo-prefetch-msgs (folder msgs)
  "prefetch messages for queueing."
  (let* ((msgdb (elmo-msgdb-load folder))
         (number-alist (elmo-msgdb-get-number-alist msgdb))
	 (len (length msgs))
	 (count 0)
         msgid msg)
    (while msgs
      (setq msg (car msgs))
      (setq msgid (cdr (assq msg number-alist)))
      (message "%s:Prefetching... %d/%d message(s)"
	       folder
	       (setq count (+ 1 count)) len)
      (elmo-force-cache-msg folder msg msgid)
      (setq msgs (cdr msgs)))))

;;  elmo-read-msg (folder msg outbuf msgdb)
;;; read message
(defun elmo-read-msg (folder msg outbuf msgdb &optional force-reload)
  "Read message into outbuf."
  (let ((inhibit-read-only t))
    ;;Only use elmo-read-msg-with-cache, because if folder is network and
    ;;elmo-use-cache-p is nil, cannot read important msg. (by muse)
    ;;(if (not (elmo-use-cache-p folder msg))
    ;;  (elmo-read-msg-no-cache folder msg outbuf)
    (elmo-read-msg-with-cache folder msg outbuf msgdb force-reload)))

(defun elmo-read-msg-with-cache (folder msg outbuf msgdb
					&optional force-reload)
  "Read message into outbuf with cacheing."
  (let* ((number-alist (elmo-msgdb-get-number-alist
			(or msgdb (elmo-msgdb-load folder))))
	 (dir (elmo-msgdb-expand-path folder))
	 (message-id (cdr (assq msg number-alist)))
	 (type (elmo-folder-number-get-type folder msg))
	 cache-status
	 ret-val part-num real-fld-num)
    (set-buffer outbuf)
    (if (and (not force-reload)
	     (not (elmo-local-file-p folder msg)))
	(setq ret-val (elmo-cache-read message-id folder msg)))
    (if ret-val
	t
      ;; cache doesn't exist.
      (setq real-fld-num (elmo-get-real-folder-number
			  folder msg))
      (if (setq ret-val (elmo-call-func (car real-fld-num)
					"read-msg"
					(cdr real-fld-num) outbuf))
	  (if (and message-id
		   (not (elmo-local-file-p folder msg))
		   (elmo-use-cache-p folder msg))
	      (elmo-cache-save message-id
			       (elmo-string-partial-p ret-val)
			       folder msg)))
      (and ret-val t))))

(defun elmo-copy-msgs (src-folder msgs dst-folder &optional msgdb same-number)
  (let* ((src-spec (elmo-folder-get-spec src-folder))
	 (loc-alist (if msgdb
			(elmo-msgdb-get-location msgdb)
		      (elmo-msgdb-location-load
		       (elmo-msgdb-expand-path src-spec)))))
    (if (eq (car src-spec) 'archive)
	(elmo-archive-copy-msgs-froms
	 (elmo-folder-get-spec dst-folder)
	 msgs src-spec loc-alist same-number)
      (elmo-call-func dst-folder "copy-msgs"
		      msgs src-spec loc-alist same-number))))

(defun elmo-move-msgs (src-folder msgs dst-folder
				  &optional msgdb all done
				  no-delete-info
				  no-delete
				  same-number
				  unread-marks)
  (save-excursion
    (let* ((db (or msgdb (elmo-msgdb-load src-folder)))
	   (number-alist (elmo-msgdb-get-number-alist db))
	   (mark-alist   (elmo-msgdb-get-mark-alist db))
	   (messages msgs)
	   (len (length msgs))
	   (all-msg-num (or all len))
	   (done-msg-num (or done 0))
	   (progress-message (if no-delete
				 "Copying messages..."
			       "Moving messages..."))
	   (tmp-buf (get-buffer-create " *elmo-move-msg*"))
	   ;elmo-no-cache-flag
	   ret-val real-fld-num done-copy dir pair
	   mes-string message-id src-cache i unseen seen-list)
      (setq i done-msg-num)
      (set-buffer tmp-buf)
      (when (and (not (eq dst-folder 'null))
		 (elmo-folder-direct-copy-p src-folder dst-folder))
	(message (concat (if no-delete "Copying" "Moving")
			 " %d message(s)...") (length messages))
	(unless (elmo-copy-msgs src-folder
				messages
				dst-folder
				db
				same-number)
	  (error "Copy message to %s failed" dst-folder))
	(setq done-copy t))
      (while messages
	(setq real-fld-num (elmo-get-real-folder-number src-folder
							(car messages)))
	(setq message-id (cdr (setq pair (assq (car messages) number-alist))))
	;; seen-list.
	(if (and (not (eq dst-folder 'null))
		 (not (and unread-marks
			   (setq unseen
				 (member
				  (cadr (assq (car messages) mark-alist))
				  unread-marks)))))
	    (setq seen-list (cons message-id seen-list)))
	(unless (or (eq dst-folder 'null) done-copy)
	  (if (and (elmo-folder-plugged-p src-folder)
		   (elmo-folder-plugged-p dst-folder)
		   (elmo-folder-identical-system-p (car real-fld-num)
						   dst-folder))
	      ;; online and identical system...so copy 'em!
	      (unless
		  (elmo-copy-msgs (car real-fld-num)
				  (list (cdr real-fld-num))
				  dst-folder
				  db
				  same-number)
		(error "Copy message to %s failed" dst-folder))
	    ;; use cache if exists.
	    ;; if there's other message with same message-id,
	    ;; don't use cache.
	    (elmo-read-msg src-folder (car messages)
			   tmp-buf msgdb
			   (and (elmo-folder-plugged-p src-folder)
				(and pair
				     (or
				      (rassoc
				       message-id
				       (cdr (memq pair number-alist)))
				      (not (eq pair
					       (rassoc message-id
						       number-alist)))))))
	    (unless (eq (buffer-size) 0)
	      (unless (elmo-append-msg dst-folder (buffer-string) message-id
				       (if same-number (car messages))
				       ;; null means all unread.
				       (or (null unread-marks)
					   unseen))
		(error "move: append message to %s failed" dst-folder)))))
	;; delete src cache if it is partial.
	(elmo-cache-delete-partial message-id src-folder (car messages))
	(setq ret-val (nconc ret-val (list (car messages))))
	(when (> all-msg-num elmo-display-progress-threshold)
	  (setq i (+ i 1))
	  (elmo-display-progress
	   'elmo-move-msgs progress-message
	   (/ (* i 100) all-msg-num)))
	(setq messages (cdr messages)))
      ;; Save seen-list.
      (unless (eq dst-folder 'null)
	(setq dir (elmo-msgdb-expand-path dst-folder))
	(elmo-msgdb-seen-save dir
			      (append (elmo-msgdb-seen-load dir) seen-list)))
      (kill-buffer tmp-buf)
      (if (and (not no-delete) ret-val)
	  (progn
	    (if (not no-delete-info)
		(message "Cleaning up src folder..."))
	    (if (and (elmo-delete-msgs src-folder ret-val db)
		     (elmo-msgdb-delete-msgs src-folder ret-val db t))
		(setq ret-val t)
	      (message "move: delete messages from %s failed." src-folder)
	      (setq ret-val nil)
	      )
	    (if (and ret-val
		     (not no-delete-info))
		(message "Cleaning up src folder...done")
	      )
	    ret-val)
	(if no-delete
	    (progn
	      (message "Copying messages...done")
	      t)
	  (if (eq len 0)
	      (message "No message was moved.")
	    (message "Moving messages failed.")
	    nil ; failure
	    ))))))

;;  boolean elmo-delete-msgs (folder msgs)
(defun elmo-delete-msgs (folder msgs &optional msgdb)
  ;; remove from real folder.
  (if (elmo-folder-plugged-p folder)
      (elmo-call-func folder "delete-msgs" msgs)
    (elmo-dop-delete-msgs folder msgs msgdb)))

(defun elmo-search (folder condition &optional from-msgs)
  (if (elmo-folder-plugged-p folder)
      (elmo-call-func folder "search" condition from-msgs)
    (elmo-cache-search-all folder condition from-msgs)))

(defun elmo-msgdb-search (folder condition msgdb)
  "Search messages which satisfy CONDITION from FOLDER with MSGDB."
  (let* ((condition (car (elmo-parse-search-condition condition)))
	 (overview (elmo-msgdb-get-overview msgdb))
	 (number-alist (elmo-msgdb-get-number-alist msgdb))
	 (number-list (mapcar 'car number-alist))
	 (length (length overview))
	 (i 0)
	 result)
    (if (elmo-condition-find-key condition "body")
	(elmo-search folder condition number-list)
      (while overview
	(if (elmo-msgdb-search-internal condition (car overview)
					number-list)
	    (setq result
		  (cons
		   (elmo-msgdb-overview-entity-get-number (car overview))
		   result)))
	(setq i (1+ i))
	(elmo-display-progress
	 'elmo-msgdb-search "Searching..." (/ (* i 100) length))
	(setq overview (cdr overview)))
      (nreverse result))))

(defun elmo-msgdb-create (folder numlist new-mark already-mark
				 seen-mark important-mark seen-list)
  (if (elmo-folder-plugged-p folder)
      (elmo-call-func folder "msgdb-create" numlist new-mark already-mark
		      seen-mark important-mark seen-list)
    (elmo-dop-msgdb-create folder numlist new-mark already-mark
			   seen-mark important-mark seen-list)))

(defun elmo-make-folder-numbers-list (folder msgs)
  (let ((msg-list msgs)
	pair fld-list
	ret-val)
    (while msg-list
      (when (> (car msg-list) 0)
	(setq pair (elmo-get-real-folder-number folder (car msg-list)))
	(if (setq fld-list (assoc (car pair) ret-val))
	    (setcdr fld-list (cons (cdr pair) (cdr fld-list)))
	  (setq ret-val (cons (cons (car pair) (list (cdr pair))) ret-val))))
      (setq msg-list (cdr msg-list)))
    ret-val))

(defun elmo-call-func-on-markable-msgs (folder func-name msgs msgdb)
  "Returns t if marked."
  (save-match-data
    (let ((folder-numbers (elmo-make-folder-numbers-list folder msgs))
	  type error)
      (while folder-numbers
	(if (or (eq
		 (setq type (car
			     (elmo-folder-get-spec
			      (car (car folder-numbers)))))
		 'imap4)
		(memq type '(maildir internal)))
	    (if (elmo-folder-plugged-p folder)
		(elmo-call-func (car (car folder-numbers)) func-name
				(cdr (car folder-numbers)))
	      (if elmo-enable-disconnected-operation
		  (elmo-dop-call-func-on-msgs
		   (car (car folder-numbers)) ; real folder
		   func-name
		   (cdr (car folder-numbers)) ; real number
		   msgdb)
		(setq error t))))
	(setq folder-numbers (cdr folder-numbers)))
      (not error))))

(defun elmo-unmark-important (folder msgs msgdb)
  (elmo-call-func-on-markable-msgs folder "unmark-important" msgs msgdb))
  
(defun elmo-mark-as-important (folder msgs msgdb)
  (elmo-call-func-on-markable-msgs folder "mark-as-important" msgs msgdb))

(defun elmo-mark-as-read (folder msgs msgdb)
  (elmo-call-func-on-markable-msgs folder "mark-as-read" msgs msgdb))

(defun elmo-mark-as-unread (folder msgs msgdb)
  (elmo-call-func-on-markable-msgs folder "mark-as-unread" msgs msgdb))

(defun elmo-msgdb-create-as-numlist (folder numlist new-mark already-mark
					    seen-mark important-mark seen-list)
  (if (elmo-folder-plugged-p folder)
      (elmo-call-func folder "msgdb-create-as-numlist" numlist
		      new-mark already-mark seen-mark important-mark seen-list)
    (elmo-dop-msgdb-create-as-numlist
     folder numlist new-mark already-mark
     seen-mark important-mark seen-list)))

;;   msgdb elmo-msgdb-load        (folder)
(defun elmo-msgdb-load (folder)
  (message "Loading msgdb for %s..." folder)
  (let* ((path (elmo-msgdb-expand-path folder))
	 (overview (elmo-msgdb-overview-load path))
	 (ret-val
	  (list overview
		(elmo-msgdb-number-load path)
		(elmo-msgdb-mark-load path)
		(elmo-msgdb-location-load path)
		(elmo-msgdb-make-overview-hashtb overview)
		)))
    (message "Loading msgdb for %s...done" folder)
    (elmo-folder-set-info-max-by-numdb folder (nth 1 ret-val))
    ret-val))

;;   boolean elmo-msgdb-save (folder msgdb)
(defun elmo-msgdb-save (folder msgdb)
  (message "Saving msgdb for %s..." folder)
  (save-excursion
    (let ((path (elmo-msgdb-expand-path folder)))
      (elmo-msgdb-overview-save path (car msgdb))
      (elmo-msgdb-number-save path (cadr msgdb))
      (elmo-msgdb-mark-save path (caddr msgdb))
      (elmo-msgdb-location-save path (cadddr msgdb))
    ;(elmo-sync-validity folder);; for validity check!!
      ))
  (message "Saving msgdb for %s...done" folder)
  (elmo-folder-set-info-max-by-numdb folder (cadr msgdb)))

(defun elmo-msgdb-add-msgs-to-seen-list-subr (msgs msgdb seen-marks seen-list)
  "Add to seen list."
  (let* ((seen-mark-list (string-to-char-list seen-marks))
	 (number-alist (elmo-msgdb-get-number-alist msgdb))
	 (mark-alist   (elmo-msgdb-get-mark-alist msgdb))
	 ent)
    (while msgs
      (if (setq ent (assq (car msgs) mark-alist))
	  (if (memq (string-to-char (cadr ent)) seen-mark-list)
	      (setq seen-list
		    (cons (cdr (assq (car msgs) number-alist)) seen-list)))
	;; no mark ... seen...
	(setq seen-list
	      (cons (cdr (assq (car msgs) number-alist)) seen-list)))
      (setq msgs (cdr msgs)))
    seen-list))

(defun elmo-msgdb-add-msgs-to-seen-list (folder msgs msgdb seen-marks)
  "Add to seen list."
  (unless (eq folder 'null) ;; black hole
    (let* ((dir (elmo-msgdb-expand-path folder))
	   (seen-list (elmo-msgdb-seen-load dir)))
      (setq seen-list
	    (elmo-msgdb-add-msgs-to-seen-list-subr
	     msgs msgdb seen-marks seen-list))
      (elmo-msgdb-seen-save dir seen-list))))

;;  msgdb elmo-append-msg (folder string)
(defun elmo-append-msg (folder string &optional message-id msg no-see)
  (let ((type (elmo-folder-get-type folder))
	filename)
    (cond ((eq type 'imap4)
	   (if (elmo-folder-plugged-p folder)
	       (elmo-call-func folder "append-msg" string msg no-see)
	     (elmo-dop-append-msg folder string message-id)))
	  ((eq type 'cache)
	   (if message-id
	       (elmo-cache-append-msg
		(elmo-folder-get-spec folder)
		string message-id msg no-see)
	     (error "elmo-cache-append-msg require message-id")))
	  (t
	   (elmo-call-func folder "append-msg" string msg no-see)))))

(defun elmo-check-validity (folder)
  (elmo-call-func folder "check-validity"
		  (expand-file-name
		   elmo-msgdb-validity-filename
		   (elmo-msgdb-expand-path folder))))

(defun elmo-pack-number (folder msgdb arg)
  (let ((type (elmo-folder-get-type folder)))
    (if (memq type '(localdir localnews maildir))
	(elmo-call-func folder "pack-number" msgdb arg)
      (error "pack-number not supported"))))

(defun elmo-sync-validity (folder)
  (elmo-call-func folder "sync-validity"
		  (expand-file-name
		   elmo-msgdb-validity-filename
		   (elmo-msgdb-expand-path folder))))

(defun elmo-use-cache-p (folder number)
  (elmo-call-func folder "use-cache-p" number)
  )

(defun elmo-local-file-p (folder number)
  (elmo-call-func folder "local-file-p" number))

(defun elmo-folder-portinfo (folder)
  (condition-case nil
      (elmo-call-func folder "portinfo")
    (error)))

(defun elmo-folder-plugged-p (folder)
  (and folder
       (or (elmo-folder-local-p folder)
	   (elmo-call-func folder "plugged-p"))))

(defun elmo-folder-set-plugged (folder plugged &optional add)
  (if (elmo-folder-local-p folder)
      nil	;; nop
    (elmo-call-func folder "set-plugged" plugged add)))

(defun elmo-generic-sync-number-alist (spec number-alist)
  "Just return number-alist."
  number-alist)

(defun elmo-generic-list-folder-unread (spec number-alist mark-alist
					     unread-marks)
  (delq nil
	(mapcar
	 (function (lambda (x)
		     (if (member (cadr (assq (car x) mark-alist)) unread-marks)
			 (car x))))
	 mark-alist)))

(defun elmo-generic-list-folder-important (spec number-alist)
  nil)

(defun elmo-update-number (folder msgdb)
  (when (elmo-folder-plugged-p folder)
    (message "Synchronize number...")
    (let* ((numlist (elmo-msgdb-get-number-alist msgdb))
	   (len (length numlist))
	   new-numlist)
      (if (eq (length (setq
		       new-numlist
		       (elmo-call-func folder "sync-number-alist" numlist)))
	      len)
	  nil
	(elmo-msgdb-set-number-alist msgdb new-numlist)
	(message "Synchronize number...done")
	new-numlist))))

(defun elmo-get-msg-filename (folder number &optional loc-alist)
  "Available if elmo-local-file-p is t."
  (elmo-call-func folder "get-msg-filename" number loc-alist))

(defun elmo-strict-folder-diff (fld &optional number-alist)
  (interactive)
  (let* ((dir (elmo-msgdb-expand-path fld))
	 (nalist (or number-alist
		     (elmo-msgdb-number-load dir)))
	 (in-db (sort (mapcar 'car nalist) '<))
	 (in-folder  (elmo-list-folder fld))
	 append-list delete-list diff)
    (cons (if (equal in-folder in-db)
	      0
	    (setq diff (elmo-list-diff
			in-folder in-db
			nil
			))
	    (setq append-list (car diff))
	    (setq delete-list (cadr diff))
	    (if append-list
		(length append-list)
	      (if delete-list
		  (- 0 (length delete-list))
		0)))
	  (length in-folder))))

(defun elmo-list-folder-unread (folder number-alist mark-alist unread-marks)
  (elmo-call-func folder "list-folder-unread"
		  number-alist mark-alist unread-marks))

(defun elmo-list-folder-important (folder number-alist)
  (let (importants)
    ;; Server side importants...(append only.)
    (if (elmo-folder-plugged-p folder)
	(setq importants (elmo-call-func folder "list-folder-important"
					 number-alist)))
    (or elmo-msgdb-global-mark-alist
	(setq elmo-msgdb-global-mark-alist
	      (elmo-object-load (expand-file-name
				 elmo-msgdb-global-mark-filename
				 elmo-msgdb-dir))))
    (while number-alist
      (if (assoc (cdr (car number-alist))
		 elmo-msgdb-global-mark-alist)
	  (setq importants (cons (car (car number-alist)) importants)))
      (setq number-alist (cdr number-alist)))
    importants))

(defun elmo-generic-commit (folder)
  nil)

(defun elmo-commit (folder)
  (elmo-call-func folder "commit"))

(defun elmo-clear-killed (folder)
  (elmo-msgdb-killed-list-save (elmo-msgdb-expand-path folder) nil))

(defvar elmo-folder-diff-async-callback nil)
(defvar elmo-folder-diff-async-callback-data nil)

(defun elmo-folder-diff-async (folder)
  "Get diff of FOLDER asynchronously.
`elmo-folder-diff-async-callback' is called with arguments of
FOLDER and DIFF (cons cell of UNSEEN and MESSAGES).
Currently works on IMAP4 folder only."
  (if (eq (elmo-folder-get-type folder) 'imap4)
      ;; Only works on imap4 with server diff.
      (progn
	(setq elmo-imap4-server-diff-async-callback
	      elmo-folder-diff-async-callback)
	(setq elmo-imap4-server-diff-async-callback-data
	      elmo-folder-diff-async-callback-data)
	(elmo-imap4-server-diff-async (elmo-folder-get-spec folder)))
    (and elmo-folder-diff-async-callback
	 (funcall elmo-folder-diff-async-callback
		  folder
		  (elmo-folder-diff folder)))))

(defun elmo-folder-diff (folder &optional number-list)
  "Get diff of FOLDER.
Return value is a cons cell of NEW and MESSAGES.
If optional argumnet NUMBER-LIST is set, it is used as a 
message list in msgdb. Otherwise, number-list is load from msgdb."
  (elmo-call-func folder "folder-diff" folder number-list))

(defun elmo-generic-folder-diff (spec folder &optional number-list)
  (let ((cached-in-db-max (elmo-folder-get-info-max folder))
	(in-folder (elmo-max-of-folder folder))
	(in-db t)
	unsync messages
	in-db-max)
    (if (or number-list (not cached-in-db-max))
	(let ((number-list (or number-list
			       (mapcar 'car
				       (elmo-msgdb-number-load
					(elmo-msgdb-expand-path folder))))))
	  ;; No info-cache.
	  (setq in-db (sort number-list '<))
	  (setq in-db-max (or (nth (max 0 (1- (length in-db))) in-db)
			      0))
	  (if (not number-list)
	      (elmo-folder-set-info-hashtb folder in-db-max nil)))
      (setq in-db-max cached-in-db-max))
    (setq unsync (if (and in-db
			  (car in-folder))
		     (- (car in-folder) in-db-max)
		   (if (and in-folder
			    (null in-db))
		       (cdr in-folder)
		     (if (null (car in-folder))
			 nil))))
    (setq messages (cdr in-folder))
    (if (and unsync messages (> unsync messages))
	(setq unsync messages))
    (cons (or unsync 0) (or messages 0))))

(defsubst elmo-folder-get-info (folder &optional hashtb)
  (elmo-get-hash-val folder
		     (or hashtb elmo-folder-info-hashtb)))

(defun elmo-folder-set-info-hashtb (folder max numbers &optional new unread)
  (let ((info (elmo-folder-get-info folder)))
    (when info
      (or new     (setq new     (nth 0 info)))
      (or unread  (setq unread  (nth 1 info)))
      (or numbers (setq numbers (nth 2 info)))
      (or max     (setq max     (nth 3 info))))
    (elmo-set-hash-val folder
		       (list new unread numbers max)
		       elmo-folder-info-hashtb)))

(defun elmo-folder-set-info-max-by-numdb (folder msgdb-number)
  (let ((num-db (sort (mapcar 'car msgdb-number) '<)))
    (elmo-folder-set-info-hashtb
     folder
     (or (nth (max 0 (1- (length num-db))) num-db) 0)
     nil ;;(length num-db)
     )))

(defun elmo-folder-get-info-max (folder)
  "Get folder info from cache."
  (nth 3 (elmo-folder-get-info folder)))

(defun elmo-folder-get-info-length (folder)
  (nth 2 (elmo-folder-get-info folder)))

(defun elmo-folder-get-info-unread (folder)
  (nth 1 (elmo-folder-get-info folder)))

(defun elmo-folder-info-make-hashtb (info-alist hashtb)
  (let* ((hashtb (or hashtb
		     (elmo-make-hash (length info-alist)))))
    (mapcar
     '(lambda (x)
	(let ((info (cadr x)))
	  (and (intern-soft (car x) hashtb)
	       (elmo-set-hash-val (car x)
				  (list (nth 2 info)   ;; new
					(nth 3 info)   ;; unread
					(nth 1 info)   ;; length
					(nth 0 info))  ;; max
				  hashtb))))
     info-alist)
    (setq elmo-folder-info-hashtb hashtb)))

(defun elmo-crosspost-message-set (message-id folders &optional type)
  (if (assoc message-id elmo-crosspost-message-alist)
      (setcdr (assoc message-id elmo-crosspost-message-alist)
	      (list folders type))
    (setq elmo-crosspost-message-alist
	  (nconc elmo-crosspost-message-alist
		 (list (list message-id folders type))))))

(defun elmo-crosspost-message-delete (message-id folders)
  (let* ((id-fld (assoc message-id elmo-crosspost-message-alist))
	 (folder-list (nth 1 id-fld)))
    (when id-fld
      (if (setq folder-list (elmo-list-delete folders folder-list))
	  (setcar (cdr id-fld) folder-list)
	(setq elmo-crosspost-message-alist
	      (delete id-fld elmo-crosspost-message-alist))))))


(defun elmo-get-msgs-with-mark (mark-alist mark)
  (let (ret-val)
    (while mark-alist
      (if (string= (cadr (car mark-alist)) mark)
	  (cons (car (car mark-alist)) ret-val))
      (setq mark-alist (cdr mark-alist)))
    (nreverse ret-val)))

(defun elmo-buffer-cache-message (fld msg &optional msgdb force-reload)
  (let* ((msg-id (cdr (assq msg (elmo-msgdb-get-number-alist msgdb))))
	 (hit (elmo-buffer-cache-hit (list fld msg msg-id)))
	 (read nil))
    (if hit
	(elmo-buffer-cache-sort
	 (elmo-buffer-cache-entry-make (list fld msg msg-id) hit))
      (setq hit (elmo-buffer-cache-add (list fld msg msg-id)))
      (setq read t))
    (if (or force-reload read)
	(condition-case err
	    (save-excursion
	      (set-buffer hit)
	      (elmo-read-msg fld msg
			     (current-buffer)
			     msgdb force-reload))
	  (quit
	   (elmo-buffer-cache-delete)
	   (error "read message %s/%s is quitted" fld msg))
	  (error
	   (elmo-buffer-cache-delete)
	   (signal (car err) (cdr err))
	   nil))) ;; will not be used
    hit)) ;; retrun value

(defun elmo-read-msg-with-buffer-cache (fld msg outbuf msgdb &optional force-reload)
  (if elmo-use-buffer-cache
      (let (hit start end)
	(when (setq hit (elmo-buffer-cache-message
			 (elmo-string fld) msg
			 msgdb force-reload))
	  (erase-buffer)
	  (save-excursion
	    (set-buffer hit)
	    (setq start (point-min) end (point-max)))
	  (insert-buffer-substring hit start end)))
    (elmo-read-msg fld msg outbuf msgdb force-reload)))

(defun elmo-folder-pipe-p (folder)
  (let ((type (elmo-folder-get-type folder)))
    (cond
     ((eq type 'multi)
      (let ((flds (cdr (elmo-folder-get-spec folder))))
	(catch 'done
	  (while flds
	    (if (elmo-folder-pipe-p (car flds))
		(throw 'done t)))
	  nil)))
     ((eq type 'pipe)
      t)
     ((eq type 'filter)
      (elmo-folder-pipe-p
       (nth 2 (elmo-folder-get-spec folder))))
     (t
      nil
      ))))

(defun elmo-multi-p (folder)
  (let ((type (elmo-folder-get-type folder)))
    (cond
     ((eq type 'multi)
      t)
     ((eq type 'pipe)
      (elmo-multi-p
       (elmo-pipe-spec-dst (elmo-folder-get-spec folder))))
     ((eq type 'filter)
      (elmo-multi-p
       (nth 2 (elmo-folder-get-spec folder))))
     (t
      nil
      ))))

(defun elmo-get-real-folder-number (folder number)
  (let ((type (elmo-folder-get-type folder)))
    (cond
     ((eq type 'multi)
      (elmo-multi-get-real-folder-number folder number))
     ((eq type 'pipe)
      (elmo-get-real-folder-number
       (elmo-pipe-spec-dst (elmo-folder-get-spec folder) )
       number))
     ((eq type 'filter)
      (elmo-get-real-folder-number
       (nth 2 (elmo-folder-get-spec folder)) number))
     (t
      (cons folder number)
      ))))

(defun elmo-folder-get-primitive-spec-list (folder &optional spec-list)
  (let ((type (elmo-folder-get-type folder))
	specs)
    (cond
     ((or (eq type 'multi)
	  (eq type 'pipe))
      (let ((flds (cdr (elmo-folder-get-spec folder)))
	    spec)
	(while flds
	  (setq spec (elmo-folder-get-primitive-spec-list (car flds)))
	  (if (not (memq (car spec) specs))
	      (setq specs (append specs spec)))
	  (setq flds (cdr flds)))))
     ((eq type 'filter)
      (setq specs
	    (elmo-folder-get-primitive-spec-list
	     (nth 2 (elmo-folder-get-spec folder)))))
     (t
      (setq specs (list (elmo-folder-get-spec folder)))
      ))
    specs))

(defun elmo-folder-get-primitive-folder-list (folder)
  (let* ((type (elmo-folder-get-type folder)))
    (cond
     ((or (eq type 'multi)
	  (eq type 'pipe))
      (let ((flds (cdr (elmo-folder-get-spec folder)))
	    ret-val)
	(while flds
	  (setq ret-val (append ret-val
				(elmo-folder-get-primitive-folder-list
				 (car flds))))
	  (setq flds (cdr flds)))
	ret-val))
     ((eq type 'filter)
      (elmo-folder-get-primitive-folder-list
       (nth 2 (elmo-folder-get-spec folder))))
     (t
      (list folder)
      ))))

(defun elmo-folder-contains-multi (folder)
  (let ((cur-spec (elmo-folder-get-spec folder)))
    (catch 'done
      (while cur-spec
	(cond
	 ((eq (car cur-spec) 'filter)
	  (setq cur-spec (elmo-folder-get-spec (nth 2 cur-spec))))
	 ((eq (car cur-spec) 'pipe)
	  (setq cur-spec (elmo-folder-get-spec (elmo-pipe-spec-src cur-spec))))
	 ((eq (car cur-spec) 'multi)
	  (throw 'done nil))
	 (t (setq cur-spec nil)))))
    cur-spec))

(defun elmo-folder-contains-type (folder type)
  (let ((spec (elmo-folder-get-spec folder)))
    (cond
     ((eq (car spec) 'filter)
      (elmo-folder-contains-type (nth 2 spec) type))
     ((eq (car spec) 'pipe)
      (elmo-folder-contains-type (elmo-pipe-spec-dst spec) type))
     ((eq (car spec) 'multi)
      (let ((folders (cdr spec)))
	(catch 'done
	  (while folders
	    (if (elmo-folder-contains-type (car folders) type)
		(throw 'done t))
	    (setq folders (cdr folders))))))
     ((eq (car spec) type)
      t)
     (t nil))))

(defun elmo-folder-number-get-spec (folder number)
  (let ((type (elmo-folder-get-type folder)))
    (cond
     ((eq type 'multi)
      (elmo-multi-folder-number-get-spec folder number))
     ((eq type 'pipe)
      (elmo-folder-number-get-spec
       (elmo-pipe-spec-dst (elmo-folder-get-spec folder)) number))
     ((eq type 'filter)
      (elmo-folder-number-get-spec
       (nth 2 (elmo-folder-get-spec folder)) number))
     (t
      (elmo-folder-get-spec folder)
      ))))

(defun elmo-folder-number-get-type (folder number)
  (car (elmo-folder-number-get-spec folder number)))

(defun elmo-multi-folder-number-get-spec (folder number)
  (let* ((spec (elmo-folder-get-spec folder))
	 (flds (cdr spec))
	 (fld (nth (- (/ number elmo-multi-divide-number) 1) flds)))
    (elmo-folder-number-get-spec fld number)))

;; autoloads
(autoload 'elmo-nntp-make-groups-hashtb "elmo-nntp")
(autoload 'elmo-nntp-post "elmo-nntp")
(autoload 'elmo-localdir-max-of-folder "elmo-localdir")
(autoload 'elmo-localdir-msgdb-create-overview-entity-from-file "elmo-localdir")
(autoload 'elmo-archive-copy-msgs-froms "elmo-archive")

;;; elmo2.el ends here
