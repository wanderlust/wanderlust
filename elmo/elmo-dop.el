;;; elmo-dop.el -- Modules for Disconnected Operations on ELMO.

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
(require 'elmo-util)
(eval-when-compile
  (require 'elmo-imap4)
  (require 'elmo-localdir))

;; global variable.
(defvar elmo-dop-queue nil
  "A list of (folder-name function-to-be-called argument-list).
Automatically loaded/saved.")

(defun elmo-dop-queue-append (folder function argument)
  (let ((operation (list (format "%s" folder) function argument)))
    (elmo-dop-queue-load)
    (unless (member operation elmo-dop-queue) ;; don't append same operation
      (setq elmo-dop-queue 
	    (append elmo-dop-queue
		    (list operation)))
      (elmo-dop-queue-save))))

(defun elmo-dop-queue-flush (&optional force)
  "Flush Disconnected operations.
If optional argument FORCE is non-nil, try flushing all operation queues
even an operation concerns the unplugged folder."
  (elmo-dop-queue-load) ; load cache.
  (elmo-dop-queue-merge)
  (let ((queue elmo-dop-queue)
	(count 0)
	len)
    (while queue
      (if (or force (elmo-folder-plugged-p (caar queue)))
	  (setq count (1+ count)))
      (setq queue (cdr queue)))
    (when (> count 0)
      (if (elmo-y-or-n-p 
	   (format "%d pending operation(s) exists. Perform now?" count)
	   (not elmo-dop-flush-confirm) t)
	  (progn
	    (message "")
	    (sit-for 0)
	    (let ((queue elmo-dop-queue)
		  (performed 0)
		  (i 0)
		  (num (length elmo-dop-queue))
		  folder func failure)
	      (while queue
		;; now perform pending processes.
		(setq failure nil)
		(setq i (+ 1 i))
		(message "Flushing queue....%d/%d." i num)
		(condition-case err
		    (if (and (not force)
			     (not (elmo-folder-plugged-p (nth 0 (car queue)))))
			(setq failure t)
		      (setq folder (nth 0 (car queue))
			    func (nth 1 (car queue)))
		      (cond
		       ((string= func "prefetch-msgs")
			(elmo-prefetch-msgs
			 folder
			 (nth 2 (car queue)))) ;argunemt
		       ((string= func "append-operations")
			(elmo-dop-flush-pending-append-operations
			 folder nil t))
		       (t
			(elmo-call-func
			 folder
			 func
			 (nth 2 (car queue)) ;argunemt
			 ))))
		  (quit  (setq failure t))
		  (error (setq failure err)))
		(if failure 
		    ;; create-folder was failed.
		    (when (and (string= func "create-folder-maybe")
			       (elmo-y-or-n-p 
				(format 
				 "Create folder %s failed. Abort creating?"
				 folder)
				(not elmo-dop-flush-confirm) t))
		      (elmo-dop-save-pending-messages folder)
		      (setq elmo-dop-queue (delq (car queue) elmo-dop-queue)))
		  (setq elmo-dop-queue (delq (car queue) elmo-dop-queue))
		  (setq performed (+ 1 performed)))
		(setq queue (cdr queue)))
	      (message "%d/%d operation(s) are performed successfully."
		       performed num)
	      (sit-for 1) ; 
	      (elmo-dop-queue-save)))
	(if (elmo-y-or-n-p "Clear all pending operations?"
			   (not elmo-dop-flush-confirm) t)
	    (let ((queue elmo-dop-queue))
	      (while queue
		(if (string= (nth 1 (car queue)) "append-operations")
		    (elmo-dop-append-list-save (nth 0 (car queue)) nil))
		(setq queue (cdr queue)))
	      (setq elmo-dop-queue nil)
	      (message "All pending operations are cleared.")
	      (elmo-dop-queue-save))
	  (message "")))
      count)))

(defconst elmo-dop-merge-funcs
  '("delete-msgids"
    "prefetch-msgs"
    "unmark-important"
    "mark-as-important"
    "mark-as-read"
    "mark-as-unread"))

(defun elmo-dop-queue-merge ()
  (let ((queue elmo-dop-queue)
        new-queue match-queue que)
    (while (setq que (car queue))
      (if (and 
	   (member (cadr que) elmo-dop-merge-funcs)
	   (setq match-queue
		 (car (delete nil
			      (mapcar '(lambda (new-queue)
					 (if (and 
					      (string= (car que) (car new-queue))
					      (string= (cadr que) (cadr new-queue)))
					     new-queue))
				      new-queue)))))
	  (setcar (cddr match-queue)
		  (append (nth 2 match-queue) (nth 2 que)))
	(setq new-queue (append new-queue (list que))))
      (setq queue (cdr queue)))
    (setq elmo-dop-queue new-queue)))

(defun elmo-dop-queue-load ()
  (save-excursion
    (setq elmo-dop-queue 
	  (elmo-object-load
	   (expand-file-name elmo-queue-filename
			     elmo-msgdb-dir)))))

(defun elmo-dop-queue-save ()
  (save-excursion
    (elmo-object-save
     (expand-file-name elmo-queue-filename
		       elmo-msgdb-dir)
     elmo-dop-queue)))

(defun elmo-dop-lock-message (message-id &optional lock-list)
  (let ((locked (or lock-list
		    (elmo-object-load 
		     (expand-file-name
		      elmo-msgdb-lock-list-filename
		      elmo-msgdb-dir)))))
    (setq locked (cons message-id locked))
    (elmo-object-save
     (expand-file-name elmo-msgdb-lock-list-filename
		       elmo-msgdb-dir)
     locked)))

(defun elmo-dop-unlock-message (message-id &optional lock-list)
  (let ((locked (or lock-list
		    (elmo-object-load 
		     (expand-file-name elmo-msgdb-lock-list-filename
				       elmo-msgdb-dir)))))
    (setq locked (delete message-id locked))
    (elmo-object-save
     (expand-file-name elmo-msgdb-lock-list-filename
		       elmo-msgdb-dir)
     locked)))

(defun elmo-dop-lock-list-load ()
  (elmo-object-load 
   (expand-file-name elmo-msgdb-lock-list-filename
		     elmo-msgdb-dir)))

(defun elmo-dop-lock-list-save (lock-list)
  (elmo-object-save
   (expand-file-name elmo-msgdb-lock-list-filename
		     elmo-msgdb-dir)
   lock-list))

(defun elmo-dop-append-list-load (folder &optional resume)
  (elmo-object-load 
   (expand-file-name (if resume
			 elmo-msgdb-resume-list-filename
		       elmo-msgdb-append-list-filename)
		     (elmo-msgdb-expand-path folder))))

(defun elmo-dop-append-list-save (folder append-list &optional resume)
  (if append-list
      (elmo-object-save
       (expand-file-name (if resume
			     elmo-msgdb-resume-list-filename
			   elmo-msgdb-append-list-filename)
			 (elmo-msgdb-expand-path folder))
       append-list)
    (condition-case ()
	(delete-file (expand-file-name (if resume
					   elmo-msgdb-resume-list-filename
					 elmo-msgdb-append-list-filename)
				       (elmo-msgdb-expand-path folder)))
      (error))))

(defun elmo-dop-deleting-numbers-to-msgids (alist numbers appended)
  "returns (new-appended . deleting-msgids)."
  (let (msgid deleting-msgids)
    (while numbers
      (setq msgid (cdr (assq (car numbers) alist)))
      (if (member msgid appended)
	  (setq appended (delete msgid appended))
	(setq deleting-msgids (append deleting-msgids (list msgid))))
      (setq numbers (cdr numbers)))
    (cons appended deleting-msgids)))

(defun elmo-dop-delete-msgs (folder msgs msgdb)
  (save-match-data
    (let ((folder-numbers (elmo-make-folder-numbers-list folder msgs))
	  appended-deleting)
      (while folder-numbers
	(if (eq (elmo-folder-get-type (car (car folder-numbers))) 
		'imap4)
	    (if elmo-enable-disconnected-operation
		(progn
		  (setq appended-deleting
			(elmo-dop-deleting-numbers-to-msgids
			 (elmo-msgdb-get-number-alist msgdb)
			 msgs ; virtual number
			 (elmo-dop-append-list-load folder)))
		  (if (cdr appended-deleting)
		      (elmo-dop-queue-append 
		       (car (car folder-numbers)) ; real folder
		       "delete-msgids" ;; for secure removal.
		       (cdr appended-deleting)))
		  (elmo-dop-append-list-save folder (car appended-deleting)))
	      (error "Unplugged"))
	  ;; not imap4 folder...delete now!
	  (elmo-call-func (car (car folder-numbers)) "delete-msgs"
			  (cdr (car folder-numbers))))
	(setq folder-numbers (cdr folder-numbers))))
    t))

(defun elmo-dop-prefetch-msgs (folder msgs)
  (save-match-data
    (elmo-dop-queue-append folder "prefetch-msgs" msgs)))

(defun elmo-dop-list-folder (folder)
  (if (or (memq (elmo-folder-get-type folder)
		'(imap4 nntp pop3 filter pipe))
	  (and (elmo-multi-p folder) (not (elmo-folder-local-p folder))))
      (if elmo-enable-disconnected-operation
	  (let* ((number-alist (elmo-msgdb-number-load
				(elmo-msgdb-expand-path folder)))
		 (number-list (mapcar 'car number-alist))
		 (append-list (elmo-dop-append-list-load folder))
		 (append-num (length append-list))
		 alreadies
		 (i 0)
		 max-num)
	    (while append-list
	      (if (rassoc (car append-list) number-alist)
		  (setq alreadies (append alreadies 
					  (list (car append-list)))))
	      (setq append-list (cdr append-list)))
	    (setq append-num (- append-num (length alreadies)))
	    (setq max-num 
		  (or (nth (max (- (length number-list) 1) 0) 
			   number-list) 0))
	    (while (< i append-num)
	      (setq number-list
		    (append number-list
			    (list (+ max-num i 1))))
	      (setq i (+ 1 i)))
	    number-list)
	(error "Unplugged"))
    ;; not imap4 folder...list folder
    (elmo-call-func folder "list-folder")))

(defun elmo-dop-count-appended (folder)
  (length (elmo-dop-append-list-load folder)))

(defun elmo-dop-call-func-on-msgs (folder func-name msgs msgdb)
  (let ((append-list (elmo-dop-append-list-load folder))
	(number-alist (elmo-msgdb-get-number-alist msgdb))
	matched)
    (if (eq (elmo-folder-get-type folder) 'imap4)
	(progn
	  (while append-list
	    (if (setq matched (car (rassoc (car append-list) number-alist)))
		(setq msgs (delete matched msgs)))
	    (setq append-list (cdr append-list)))
	  (if msgs
	      (elmo-dop-queue-append folder func-name msgs)))
      ;; maildir... XXX hard coding.....
      (if (not (featurep 'elmo-maildir))
	  (require 'maildir))
      (funcall (intern (format "elmo-maildir-%s" func-name))
	       (elmo-folder-get-spec folder)
	       msgs msgdb))))

(defun elmo-dop-max-of-folder (folder)
  (if (eq (elmo-folder-get-type folder) 'imap4)
      (if elmo-enable-disconnected-operation
	  (let* ((number-alist (elmo-msgdb-number-load 
				       (elmo-msgdb-expand-path folder)))
		 (number-list (mapcar 'car number-alist))
		 (append-list (elmo-dop-append-list-load folder))
		 (append-num (length append-list))
		 alreadies
		 (i 0)
		 max-num)
	    (while append-list
	      (if (rassoc (car append-list) number-alist)
		  (setq alreadies (append alreadies 
					  (list (car append-list)))))
	      (setq append-list (cdr append-list)))
	    (setq max-num 
		  (or (nth (max (- (length number-list) 1) 0) number-list)
		      0))
	    (cons (- (+ max-num append-num) (length alreadies))
		  (- (+ (length number-list) append-num) (length alreadies))))
	(error "Unplugged"))
    ;; not imap4 folder.
    (elmo-call-func folder "max-of-folder")))

(defun elmo-dop-save-pending-messages (folder)
  (message (format "Saving queued message in %s..." elmo-lost+found-folder))
  (let* ((append-list (elmo-dop-append-list-load folder))
	 file-string)
    (while append-list
      (when (setq file-string (elmo-get-file-string  ; message string
			       (elmo-cache-get-path 
				(car append-list))))
	(elmo-append-msg elmo-lost+found-folder file-string)
	(elmo-dop-unlock-message (car append-list)))
      (setq append-list (cdr append-list))
      (elmo-dop-append-list-save folder nil)))
  (message (format "Saving queued message in %s...done." 
		   elmo-lost+found-folder)))

(defun elmo-dop-flush-pending-append-operations (folder &optional appends resume)
  (message "Appending queued messages...")
  (let* ((append-list (or appends 
			  (elmo-dop-append-list-load folder)))
	 (appendings append-list)
	 (i 0)
	 (num (length append-list))
	 failure file-string)
    (when resume
      ;; Resume msgdb changed by elmo-dop-msgdb-create.
      (let* ((resumed-list (elmo-dop-append-list-load folder t))
	     (number-alist (elmo-msgdb-number-load 
			    (elmo-msgdb-expand-path folder)))
	     (appendings append-list)
	     pair dels)
	(while appendings
	  (if (setq pair (rassoc (car appendings) number-alist))
	      (setq resumed-list (append resumed-list
					 (list (car appendings)))))
	  (setq appendings (cdr appendings)))
	(elmo-dop-append-list-save folder resumed-list t)))
    (while appendings
      (setq failure nil)
      (setq file-string (elmo-get-file-string  ; message string
			 (elmo-cache-get-path 
			  (car appendings))))
      (when file-string
	(condition-case ()
	    (elmo-append-msg folder file-string (car appendings))
	  (quit  (setq failure t))
	  (error (setq failure t)))
	(setq i (+ 1 i))
	(message (format "Appending queued messages...%d" i))
	(if failure
	    (elmo-append-msg elmo-lost+found-folder
			     file-string (car appendings))))
      (elmo-dop-unlock-message (car appendings))
      (setq appendings (cdr appendings)))
    ;; All pending append operation is flushed.
    (elmo-dop-append-list-save folder nil)
    (elmo-commit folder)
    (unless resume
      ;; delete '(folder "append-operations") in elmo-dop-queue.
      (let (elmo-dop-queue)
	(elmo-dop-queue-load)
	(setq elmo-dop-queue (delete (list folder "append-operations" nil)
				     elmo-dop-queue))
	(elmo-dop-queue-save))))
  (message "Appending queued messages...done."))

(defun elmo-dop-folder-exists-p (folder)
  (if (and elmo-enable-disconnected-operation
	   (eq (elmo-folder-get-type folder) 'imap4))
      (file-exists-p (elmo-msgdb-expand-path folder))
    (elmo-call-func folder "folder-exists-p")))

(defun elmo-dop-create-folder (folder)
  (if (eq (elmo-folder-get-type folder) 'imap4)
      (if elmo-enable-disconnected-operation
	  (elmo-dop-queue-append folder "create-folder-maybe" nil)
	(error "Unplugged"))
    (elmo-call-func folder "create-folder")))

(defun elmo-dop-delete-folder (folder)
  (error "Unplugged"))

(defun elmo-dop-rename-folder (old-folder new-folder)
  (error "Unplugged"))

(defun elmo-dop-append-msg (folder string message-id &optional msg)
  (if elmo-enable-disconnected-operation
      (if message-id
	  (progn
	    (unless (elmo-cache-exists-p message-id)
	      (elmo-set-work-buf
	       (insert string)
	       (elmo-cache-save message-id nil folder msg (current-buffer))))
	    (let ((append-list (elmo-dop-append-list-load folder))
		  (number-alist (elmo-msgdb-number-load 
				 (elmo-msgdb-expand-path folder))))
	      (when (and ; not in current folder.
		     (not (rassoc message-id number-alist))
		     (not (member message-id append-list)))
		(setq append-list
		      (append append-list (list message-id)))
		(elmo-dop-lock-message message-id)
		(elmo-dop-append-list-save folder append-list)
		(elmo-dop-queue-append folder "append-operations" nil))
	      t))
	nil)
    (error "Unplugged")))

(defalias 'elmo-dop-msgdb-create 'elmo-dop-msgdb-create-as-numlist)

(defun elmo-dop-msgdb-create-as-numlist (folder numlist new-mark already-mark
						seen-mark important-mark
						seen-list)
  (if (or (eq (elmo-folder-get-type folder) 'imap4)
	  (eq (elmo-folder-get-type folder) 'nntp))
      (if elmo-enable-disconnected-operation
	  (let* ((num-alist (elmo-msgdb-number-load 
			     (elmo-msgdb-expand-path folder)))
		 (number-list (mapcar 'car num-alist))
		 (ov (elmo-msgdb-overview-load
		      (elmo-msgdb-expand-path folder)))
		 (append-list (elmo-dop-append-list-load folder))
		 (num (length numlist))
		 (i 0)
		 overview number-alist mark-alist msgid ov-entity
		 max-num percent seen gmark)
	    (setq max-num
		  (or (nth (max (- (length number-list) 1) 0) number-list)
		      0))
	    (while numlist
	      (if (setq msgid
			(nth (+ (length append-list) 
				(- (car numlist) max-num 1 num))
			     append-list))
		  (progn
		    (setq overview
			  (elmo-msgdb-append-element
			   overview
			   (elmo-localdir-msgdb-create-overview-entity-from-file
			    (car numlist)
			    (elmo-cache-get-path msgid))))
		    (setq number-alist
			  (elmo-msgdb-number-add number-alist
						 (car numlist) msgid))
		    (setq seen (member msgid seen-list))
		    (if (setq gmark
			      (or (elmo-msgdb-global-mark-get msgid)
				  (if (elmo-cache-exists-p 
				       msgid
				       folder
				       (car number-alist))
				      (if seen
					  nil
					already-mark)
				    (if seen
					seen-mark)
				    new-mark)))
			(setq mark-alist
			      (elmo-msgdb-mark-append
			       mark-alist (car numlist) gmark))))
		
		(when (setq ov-entity (assoc 
				       (cdr (assq (car numlist) num-alist))
				       ov))
		  (setq overview
			(elmo-msgdb-append-element
			 overview ov-entity))
		  (setq number-alist
			(elmo-msgdb-number-add number-alist
					       (car numlist)
					       (car ov-entity)))
		  (setq seen (member ov-entity seen-list))
		  (if (setq gmark
			    (or (elmo-msgdb-global-mark-get (car ov-entity))
				(if (elmo-cache-exists-p
				     msgid
				     folder
				     (car ov-entity))
				    (if seen
					nil
				      already-mark)
				  (if seen
				      seen-mark)
				  new-mark)))
		      (setq mark-alist
			    (elmo-msgdb-mark-append
			     mark-alist (car numlist) gmark)))))
	      (setq i (1+ i))
	      (setq percent (/ (* i 100) num))
	      (elmo-display-progress
	       'elmo-dop-msgdb-create-as-numlist "Creating msgdb..."
	       percent)
	      (setq numlist (cdr numlist)))
	    (list overview number-alist mark-alist))
	(error "Unplugged"))
    ;; not imap4 folder...
    (elmo-call-func folder "msgdb-create" numlist new-mark already-mark
		    seen-mark important-mark seen-list)))

(provide 'elmo-dop)

;;; elmo-dop.el ends here
