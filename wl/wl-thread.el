;;; wl-thread.el -- Thread display modules for Wanderlust.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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

(require 'wl-summary)
(require 'wl-highlight)

;; buffer local variables.
;(defvar wl-thread-top-entity '(nil t nil nil)) ; top entity
(defvar wl-thread-tops nil)           ; top number list (number)
(defvar wl-thread-entities nil)
(defvar wl-thread-entity-list nil)    ; entity list
(defvar wl-thread-entity-hashtb nil)  ; obarray
(defvar wl-thread-indent-regexp nil)

(mapcar
 (function make-variable-buffer-local)
 (list 'wl-thread-entity-hashtb
       'wl-thread-entities     ; -> ".wl-thread-entity"
       'wl-thread-entity-list  ; -> ".wl-thread-entity-list"
       'wl-thread-entity-cur
       'wl-thread-indent-regexp))

;;; global flag
(defvar wl-thread-insert-force-opened nil)

;;;;;; each entity is (number opened-or-not children parent) ;;;;;;;

(defun wl-meaning-of-mark (mark)
  (if (not (elmo-folder-plugged-p wl-summary-buffer-folder-name))
      (cond
       ((string= mark wl-summary-unread-cached-mark)
	'unread)
       ((string= mark wl-summary-important-mark)
	'important))
    (cond
     ((string= mark wl-summary-new-mark)
      'new)
     ((or (string= mark wl-summary-unread-uncached-mark)
	  (string= mark wl-summary-unread-cached-mark))
      'unread)
     ((string= mark wl-summary-important-mark)
      'important))))
  
(defun wl-thread-next-mark-p (mark next)
  (cond ((not (elmo-folder-plugged-p wl-summary-buffer-folder-name))
	 (or (string= mark wl-summary-unread-cached-mark)
	     (string= mark wl-summary-important-mark)))
	((eq next 'new)
	 (string= mark wl-summary-new-mark))
	((eq next 'unread)
	 (or (string= mark wl-summary-unread-uncached-mark)
	     (string= mark wl-summary-unread-cached-mark)
	     (string= mark wl-summary-new-mark)))
	(t
	 (or (string= mark wl-summary-unread-uncached-mark)
	     (string= mark wl-summary-unread-cached-mark)
	     (string= mark wl-summary-new-mark)
	     (string= mark wl-summary-important-mark)))))

(defun wl-thread-next-failure-mark-p (mark next)
  (cond ((not (elmo-folder-plugged-p wl-summary-buffer-folder-name))
	 (string= mark wl-summary-unread-cached-mark))
	((or (eq next 'new)
	     (eq next 'unread))
	 (or (string= mark wl-summary-unread-uncached-mark)
	     (string= mark wl-summary-unread-cached-mark)
	     (string= mark wl-summary-new-mark)
	     (string= mark wl-summary-important-mark)))
	(t t)))

(defun wl-thread-resume-entity (fld)
  (let (entities top-list)
    (setq entities (wl-summary-load-file-object
		    (expand-file-name wl-thread-entity-file
				      (elmo-msgdb-expand-path fld))))
    (setq top-list
	  (wl-summary-load-file-object
	   (expand-file-name wl-thread-entity-list-file
			     (elmo-msgdb-expand-path fld))))
    (current-buffer)
    (message "Resuming thread structure...")
    ;; set obarray value.
    (setq wl-thread-entity-hashtb (elmo-make-hash (* (length entities) 2)))
    (mapcar
     '(lambda (x)
       (elmo-set-hash-val (format "#%d" (car x))
			  x
			  wl-thread-entity-hashtb))
     entities)
    ;; set buffer local variables.
    (setq wl-thread-entities entities)
    (setq wl-thread-entity-list top-list)
    (message "Resuming thread structure...done.")))

(defun wl-thread-save-entity (dir)
  (wl-thread-save-entities dir)
  (wl-thread-save-top-list dir))

(defun wl-thread-save-top-list (dir)
  (let ((top-file (expand-file-name wl-thread-entity-list-file dir))
	(entity wl-thread-entity-list)
	(tmp-buffer (get-buffer-create " *wl-thread-save-top-list*")))
    (save-excursion
      (set-buffer tmp-buffer)
      (erase-buffer)
      (when (file-writable-p top-file)
	(prin1 entity tmp-buffer)
	(princ "\n" tmp-buffer)
	(write-region (point-min) (point-max) top-file nil 'no-msg)
	(kill-buffer tmp-buffer)))))

(defun wl-thread-save-entities (dir)
  (let ((top-file (expand-file-name wl-thread-entity-file dir))
	(entities wl-thread-entities)
	(tmp-buffer (get-buffer-create " *wl-thread-save-entities*")))
    (save-excursion
      (set-buffer tmp-buffer)
      (erase-buffer)
      (when (file-writable-p top-file)
	(prin1 entities tmp-buffer)
	(princ "\n" tmp-buffer)
	(write-region (point-min) (point-max) top-file nil 'no-msg)
	(kill-buffer tmp-buffer)))))

(defsubst wl-thread-entity-get-number (entity)
  (nth 0 entity))
(defsubst wl-thread-entity-get-opened (entity)
  (nth 1 entity))
(defsubst wl-thread-entity-get-children (entity)
  (nth 2 entity))
(defsubst wl-thread-entity-get-parent (entity)
  (nth 3 entity))

(defsubst wl-thread-create-entity (num parent &optional opened)
  (list num (or opened wl-thread-insert-opened) nil parent))

(defsubst wl-thread-get-entity (num)
  (and num
       (boundp (intern (format "#%d" num) wl-thread-entity-hashtb))
       (elmo-get-hash-val (format "#%d" num) wl-thread-entity-hashtb)))

(defsubst wl-thread-entity-set-parent (entity parent)
  (setcar (cdddr entity) parent)
  entity)

(defsubst wl-thread-entity-set-children (entity children)
  (setcar (cddr entity) children))

(defsubst wl-thread-entity-insert-as-top (entity)
  (when (and entity
	     (car entity))
    (setq wl-thread-entity-list (append wl-thread-entity-list
					(list (car entity))))
    (setq wl-thread-entities (cons entity wl-thread-entities))
    (elmo-set-hash-val (format "#%d" (car entity)) entity
		       wl-thread-entity-hashtb)))

(defsubst wl-thread-entity-insert-as-children (to entity)
  (let ((children (nth 2 to)))
    (setcar (cddr to) (wl-append children
				 (list (car entity))))
    (setq wl-thread-entities (cons entity wl-thread-entities))
    (elmo-set-hash-val (format "#%d" (car entity)) entity
		       wl-thread-entity-hashtb)))

(defsubst wl-thread-entity-set-opened (entity opened)
  (setcar (cdr entity) opened))

(defsubst wl-thread-entity-get-children-num (entity)
  (let (children
	ret-val msgs-stack
	(msgs (list (car entity))))
   (while msgs
     (setq msgs (cdr msgs))
     (setq children (wl-thread-entity-get-children entity))
     (if (null children)
	 (while (and (null msgs) msgs-stack)
	   (setq msgs (wl-pop msgs-stack)))
       (setq ret-val (+ (or ret-val 0) (length children)))
       (wl-push msgs msgs-stack)
       (setq msgs children))
     (setq entity (wl-thread-get-entity (car msgs))))
   ret-val))

(defsubst wl-thread-entity-get-descendant (entity)
  (let (children
	ret-val msgs-stack
	(msgs (list (car entity))))
   (while msgs
     (setq msgs (cdr msgs))
     (setq children (wl-thread-entity-get-children entity))
     (if (null children)
	 (while (and (null msgs) msgs-stack)
	   (setq msgs (wl-pop msgs-stack)))
       (setq ret-val (append ret-val (copy-sequence children)))
       (wl-push msgs msgs-stack)
       (setq msgs children))
     (setq entity (wl-thread-get-entity (car msgs))))
   ret-val))

(defsubst wl-thread-entity-get-parent-entity (entity)
  (wl-thread-get-entity (wl-thread-entity-get-parent entity)))

(defun wl-thread-entity-get-top-entity (entity)
  (let ((cur-entity entity)
	p-num)
    (while (setq p-num (wl-thread-entity-get-parent cur-entity))
      (setq cur-entity (wl-thread-get-entity p-num)))
    cur-entity))

(defun wl-thread-entity-parent-invisible-p (entity)
  "If parent of ENTITY is invisible, the top invisible ancestor entity of
ENTITY is returned."
  (let ((cur-entity entity)
	ret-val)
    (catch 'done
      (while (setq cur-entity (wl-thread-entity-get-parent-entity
			       cur-entity))
	(if (null (wl-thread-entity-get-number cur-entity))
	    ;; top!!
	    (progn
	      ;;(setq ret-val nil)
	      (throw 'done nil))
	  (when (not (wl-thread-entity-get-opened cur-entity))
	    ;; not opened!!
	    (setq ret-val cur-entity)))))
    ;; top of closed entity in the path.
    ret-val))

(defun wl-thread-entity-get-mark (number)
  (let ((mark-alist (elmo-msgdb-get-mark-alist wl-summary-buffer-msgdb))
	mark)
    (setq mark (cadr (assq number mark-alist)))
    (if (string= mark wl-summary-read-uncached-mark)
	()
      mark)))

(defun wl-thread-meaning-alist-get-result (meaning-alist)
  (let ((malist meaning-alist)
	ret-val)
    (catch 'done
      (while malist
	(if (setq ret-val (cdr (car malist)))
	    (throw 'done ret-val))
	(setq malist (cdr malist))))))

(defun wl-thread-entity-check-prev-mark (entity prev-marks)
  "Check prev mark. Result is stored in PREV-MARK."
  (let ((msgs (list (car entity)))
	(succeed-list (car prev-marks))
	(failure-list (cdr prev-marks))
	msgs-stack children
	mark meaning success failure parents)
  (catch 'done
    (while msgs
      (if (and (not (memq (car msgs) parents))
	       (setq children (reverse (wl-thread-entity-get-children entity))))
	  (progn
	    (wl-append parents (list (car msgs)))
	    (wl-push msgs msgs-stack)
	    (setq msgs children))
	(if (setq mark (wl-thread-entity-get-mark (car entity)))
	    (if (setq meaning (wl-meaning-of-mark mark))
		(if (setq success (assq meaning succeed-list))
		    (progn
		      (setcdr success entity)
		      (throw 'done nil))
		  (setq failure (assq meaning failure-list))
		  (unless (cdr failure)
		    (setcdr (assq meaning failure-list) entity)))))
	(setq msgs (cdr msgs)))
	(unless msgs
	  (while (and (null msgs) msgs-stack)
	    (setq msgs (wl-pop msgs-stack))))
      (setq entity (wl-thread-get-entity (car msgs)))))))

(defun wl-thread-entity-check-next-mark (entity next-marks)
  "Check next mark. Result is stored in NEXT-MARK."
  (let ((msgs (list (car entity)))
	(succeed-list (car next-marks))
	(failure-list (cdr next-marks))
	msgs-stack children
	mark meaning success failure)
  (catch 'done
    (while msgs
      (if (setq mark (wl-thread-entity-get-mark (car entity)))
	  (if (setq meaning (wl-meaning-of-mark mark))
	      (if (setq success (assq meaning succeed-list))
		  (progn
		    (setcdr success entity)
		    (throw 'done nil))
		(setq failure (assq meaning failure-list))
		(unless (cdr failure)
		  (setcdr (assq meaning failure-list) entity)))))
      (setq msgs (cdr msgs))
      (setq children (wl-thread-entity-get-children entity))
      (if children
	  (progn
	    (wl-push msgs msgs-stack)
	    (setq msgs children))
	(unless msgs
	  (while (and (null msgs) msgs-stack)
	    (setq msgs (wl-pop msgs-stack)))))
      (setq entity (wl-thread-get-entity (car msgs)))))))

(defun wl-thread-entity-get-older-brothers (entity &optional parent)
  (let* ((parent (or parent
		     (wl-thread-entity-get-parent-entity entity)))
	 (brothers (wl-thread-entity-get-children parent))
	 ret-val)
    (if parent
	brothers
      (setq brothers wl-thread-entity-list))
    (catch 'done
      (while brothers
	(if (not (eq (wl-thread-entity-get-number entity)
		     (car brothers)))
	    (wl-append ret-val (list (car brothers)))
	  (throw 'done ret-val))
	(setq brothers (cdr brothers))))))

(defun wl-thread-entity-get-younger-brothers (entity &optional parent)
  (let* ((parent (or parent
		     (wl-thread-entity-get-parent-entity entity)))
	 (brothers (wl-thread-entity-get-children parent)))
    (if parent
	(cdr (memq (wl-thread-entity-get-number entity)
		   brothers))
      ;; top!!
      (cdr (memq (car entity) wl-thread-entity-list)))))

(defun wl-thread-entity-check-prev-mark-from-older-brother (entity prev-marks)
  (let* (older-brother parent)
  (catch 'done
    (while entity
      (setq older-brother
	    (reverse (wl-thread-entity-get-older-brothers entity)))
      ;; check itself
      (let ((succeed-list (car prev-marks))
 	    (failure-list (cdr prev-marks))
 	    mark meaning success failure)
 	(if (setq mark (wl-thread-entity-get-mark (car entity)))
 	    (if (setq meaning (wl-meaning-of-mark mark))
 		(if (setq success (assq meaning succeed-list))
 		    (progn
 		      (setcdr success entity)
 		      (throw 'done nil))
 		  (setq failure (assq meaning failure-list))
 		  (unless (cdr failure)
 		    (setcdr (assq meaning failure-list) entity))))))
      ;; check older brothers
      (while older-brother
	(wl-thread-entity-check-prev-mark (wl-thread-get-entity
					   (car older-brother))
					  prev-marks)
	(if (wl-thread-meaning-alist-get-result
	     (car prev-marks))
	    (throw 'done nil))
	(setq older-brother (cdr older-brother)))
      (setq entity (wl-thread-entity-get-parent-entity entity))))))

(defun wl-thread-entity-get-prev-marked-entity (entity prev-marks)
  (let ((older-brothers (reverse
			 (wl-thread-entity-get-older-brothers entity)))
	marked)
    (or (catch 'done
	  (while older-brothers
	    (wl-thread-entity-check-prev-mark
	     (wl-thread-get-entity (car older-brothers)) prev-marks)
	    (if (setq marked
		      (wl-thread-meaning-alist-get-result
		       (car prev-marks)))
		(throw 'done marked))
	    (setq older-brothers (cdr older-brothers))))
	(wl-thread-entity-check-prev-mark-from-older-brother
	 (wl-thread-entity-get-parent-entity entity) prev-marks)
	(if (setq marked
		  (wl-thread-meaning-alist-get-result
		   (car prev-marks)))
	    marked
	  (if (setq marked
		    (wl-thread-meaning-alist-get-result
		     (cdr prev-marks)))
	      marked)))))

(defun wl-thread-get-prev-unread (msg &optional hereto)
  (let ((cur-entity (wl-thread-get-entity msg))
	(prev-marks (cond ((eq wl-summary-move-order 'new)
			   (cons (list (cons 'new nil))
				 (list (cons 'unread nil)
				       (cons 'important nil))))
			  ((eq wl-summary-move-order 'unread)
			   (cons (list (cons 'unread nil)
				       (cons 'new nil))
				 (list (cons 'important nil))))
			  (t
			   (cons (list (cons 'unread nil)
				       (cons 'new nil)
				       (cons 'important nil))
				 nil))))
	mark ret-val)
    (if hereto
	(when (wl-thread-next-mark-p (setq mark
					   (wl-thread-entity-get-mark
					    (car cur-entity)))
				     (caaar prev-marks))
	  ;;(setq mark (cons cur-entity
	  ;;(wl-thread-entity-get-mark cur-entity)))
	  (setq ret-val msg)))
    (when (and (not ret-val)
	       (or (setq cur-entity
			 (wl-thread-entity-get-prev-marked-entity
			  cur-entity prev-marks))
		   (and hereto mark)))
      (if (and hereto
	       (catch 'done
		 (let ((success-list (car prev-marks)))
		   (while success-list
		     (if (cdr (car success-list))
			 (throw 'done nil))
		     (setq success-list (cdr success-list)))
		   t))
	       (wl-thread-next-failure-mark-p mark (caaar prev-marks)))
	  (setq ret-val msg)
	(when cur-entity
	  (setq ret-val (car cur-entity)))))
    ret-val))
    
(defun wl-thread-jump-to-prev-unread (&optional hereto)
  "If prev unread is a children of a closed message,
the closed parent will be opened."
  (interactive "P")
  (let ((msg (wl-thread-get-prev-unread
	      (wl-summary-message-number) hereto)))
    (when msg
      (wl-thread-entity-force-open (wl-thread-get-entity msg))
      (wl-summary-jump-to-msg msg)
      t)))

(defun wl-thread-jump-to-msg (&optional number)
  (interactive)
  (let ((num (or number
                 (string-to-int
                  (read-from-minibuffer "Jump to Message(No.): ")))))
    (wl-thread-entity-force-open (wl-thread-get-entity num))
    (wl-summary-jump-to-msg num)))

(defun wl-thread-get-next-unread (msg &optional hereto)
  (let ((cur-entity (wl-thread-get-entity msg))
	(next-marks (cond ((not (elmo-folder-plugged-p
				 wl-summary-buffer-folder-name))
			   (cons (list (cons 'unread nil))
				 (list (cons 'important nil))))
			  ((eq wl-summary-move-order 'new)
			   (cons (list (cons 'new nil))
				 (list (cons 'unread nil)
				       (cons 'important nil))))
			  ((eq wl-summary-move-order 'unread)
			   (cons (list (cons 'unread nil)
				       (cons 'new nil))
				 (list (cons 'important nil))))
			  (t
			   (cons (list (cons 'unread nil)
				       (cons 'new nil)
				       (cons 'important nil))
				 nil))))
	mark ret-val)
    (if hereto
	(when (wl-thread-next-mark-p (setq mark
					   (wl-thread-entity-get-mark
					    (car cur-entity)))
				     (caaar next-marks))
	  (setq ret-val msg)))
    (when (and (not ret-val)
	       (or (setq cur-entity
			 (wl-thread-entity-get-next-marked-entity
			  cur-entity next-marks))
		   (and hereto mark)))
      (if (and hereto
	       ;; all success-list is nil
	       (catch 'done
		 (let ((success-list (car next-marks)))
		   (while success-list
		     (if (cdr (car success-list))
		       (throw 'done nil))
		     (setq success-list (cdr success-list)))
		   t))
	       (wl-thread-next-failure-mark-p mark (caaar next-marks)))
	  (setq ret-val msg)
	(when cur-entity
	  (setq ret-val (car cur-entity)))))
    ret-val))

(defun wl-thread-jump-to-next-unread (&optional hereto)
  "If next unread is a children of a closed message,
the closed parent will be opened."
  (interactive "P")
  (let ((msg (wl-thread-get-next-unread
	      (wl-summary-message-number) hereto)))
    (when msg
      (wl-thread-entity-force-open (wl-thread-get-entity msg))
      (wl-summary-jump-to-msg msg)
      t)))

(defun wl-thread-close-all ()
  "Close all top threads."
  (interactive)
  (message "Closing all threads...")
  (let ((entities wl-thread-entity-list)
	(cur 0)
	(len (length wl-thread-entity-list)))
    (while entities
      (when (and (wl-thread-entity-get-opened (wl-thread-get-entity
					       (car entities)))
		 (wl-thread-entity-get-children (wl-thread-get-entity
						 (car entities))))
	(wl-summary-jump-to-msg (car entities))
	(wl-thread-open-close)
	(setq cur (1+ cur))
	(elmo-display-progress
	 'wl-thread-close-all "Closing all threads..."
	 (/ (* cur 100) len)))
      (setq entities (cdr entities))))
  (elmo-display-progress 'wl-thread-close-all
			 "Closing all threads..."
			 100)
  (message "Closing all threads...done.")
  (goto-char (point-max)))

(defun wl-thread-open-all ()
  "Open all threads."
  (interactive)
  (message "Opening all threads...")
  (let ((entities wl-thread-entity-list)
	(cur 0)
	(len (length wl-thread-entity-list)))
    (while entities
      (if (not (wl-thread-entity-get-opened (wl-thread-get-entity
					     (car entities))))
	  (wl-thread-entity-force-open (wl-thread-get-entity
					(car entities))))
      (setq cur (1+ cur))
      (elmo-display-progress
       'wl-thread-open-all "Opening all threads..."
       (/ (* cur 100) len))
      (setq entities (cdr entities))))
  (message "Opening all threads...done.")
  (goto-char (point-max)))

(defun wl-thread-open-all-unread ()
  (interactive)
  (let ((mark-alist (elmo-msgdb-get-mark-alist wl-summary-buffer-msgdb))
	mark)
    (while mark-alist
      (if (setq mark (nth 1 (car mark-alist)))
	  (if (or (string= mark wl-summary-unread-uncached-mark)
		  (string= mark wl-summary-unread-cached-mark)
		  (string= mark wl-summary-new-mark)
		  (string= mark wl-summary-important-mark))
	      (wl-thread-entity-force-open (wl-thread-get-entity
					    (nth 0 (car mark-alist))))))
      (setq mark-alist (cdr mark-alist)))))

;;; a subroutine for wl-thread-entity-get-next-marked-entity.
(defun wl-thread-entity-check-next-mark-from-younger-brother
  (entity next-marks)
  (let* (parent younger-brother)
    (catch 'done
      (while entity
	(setq parent (wl-thread-entity-get-parent-entity entity)
	      younger-brother
	      (wl-thread-entity-get-younger-brothers entity parent))
	;; check my brother!
	(while younger-brother
	  (wl-thread-entity-check-next-mark
	   (wl-thread-get-entity (car younger-brother))
	   next-marks)
	  (if  (wl-thread-meaning-alist-get-result
		(car next-marks))
	      (throw 'done nil))
	  (setq younger-brother (cdr younger-brother)))
	(setq entity parent)))))

(defun wl-thread-entity-get-next-marked-entity (entity next-marks)
  (let ((children (wl-thread-entity-get-children entity))
	marked)
    (or (catch 'done
	  (while children
	    (wl-thread-entity-check-next-mark
	     (wl-thread-get-entity (car children)) next-marks)
	    (if (setq marked
		      (wl-thread-meaning-alist-get-result
		       (car next-marks)))
		(throw 'done marked))
	    (setq children (cdr children))))
	;; check younger brother
	(wl-thread-entity-check-next-mark-from-younger-brother
	 entity next-marks)
	(if (setq marked
		  (wl-thread-meaning-alist-get-result
		   (car next-marks)))
	    marked
	  (if (setq marked
		    (wl-thread-meaning-alist-get-result
		     (cdr next-marks)))
	      marked)))))

(defun wl-thread-update-line-msgs (msgs)
  (wl-delete-all-overlays)
  (while msgs
    (setq msgs
	  (wl-thread-update-line-on-buffer (car msgs) nil msgs))))

(defsubst wl-thread-update-line-on-buffer-sub (entity &optional msg parent-msg)
  (let ((number-alist (elmo-msgdb-get-number-alist wl-summary-buffer-msgdb))
	(overview (elmo-msgdb-get-overview wl-summary-buffer-msgdb))
	(mark-alist (elmo-msgdb-get-mark-alist wl-summary-buffer-msgdb))
	(buffer-read-only nil)
	(inhibit-read-only t)
	;;(parent-msg parent-msg)
	overview-entity
	temp-mark
	children-num
	summary-line)
    (if (memq msg wl-summary-buffer-delete-list)
	(setq temp-mark "D"))
    (if (memq msg wl-summary-buffer-target-mark-list)
	(setq temp-mark "*"))
    (if (assq msg wl-summary-buffer-refile-list)
	(setq temp-mark "o"))
    (if (assq msg wl-summary-buffer-copy-list)
	(setq temp-mark "O"))
    (unless temp-mark
      (setq temp-mark (wl-summary-get-score-mark msg)))
    ;(setq parent-entity (wl-thread-entity-get-parent-entity entity))
    (unless parent-msg
      (setq parent-msg (wl-thread-entity-get-parent entity)))
    ;;(setq children (wl-thread-entity-get-children entity))
    (setq children-num (wl-thread-entity-get-children-num entity))
    (setq overview-entity
	  (elmo-msgdb-search-overview-entity msg
					     number-alist overview))
    ;;(wl-delete-all-overlays)
    (when overview-entity
      (setq summary-line
	    (wl-summary-overview-create-summary-line
	     msg
	     overview-entity
	     (assoc			; parent-entity
	      (cdr (assq parent-msg
			 number-alist)) overview)
	     nil
	     mark-alist
	     (if wl-thread-insert-force-opened
		 nil
	       (if (not (wl-thread-entity-get-opened entity))
		   (or children-num)))
	     temp-mark entity))
      (wl-summary-insert-line summary-line))))

(defun wl-thread-update-line-on-buffer (&optional msg parent-msg updates)
  (interactive)
  (let ((msgs (list (or msg (wl-summary-message-number))))
	entity children msgs-stack)
   (while msgs
    (setq msg (wl-pop msgs))
    (setq updates (and updates (delete msg updates)))
    (when (wl-thread-delete-line-from-buffer msg)
      (setq entity (wl-thread-get-entity msg))
      (wl-thread-update-line-on-buffer-sub entity msg parent-msg)
      ;;
      (setq children (wl-thread-entity-get-children entity))
      (if children
	  ;; update children
	  (when (wl-thread-entity-get-opened entity)
	    (wl-push msgs msgs-stack)
	    (setq parent-msg msg
		  msgs children))
	(unless msgs
	  (while (and (null msgs) msgs-stack)
	    (setq msgs (wl-pop msgs-stack)))
	  (when msgs
	    (setq parent-msg
		  (wl-thread-entity-get-number
		   (wl-thread-entity-get-parent-entity
		    (wl-thread-get-entity (car msgs))))))))))
   updates))

(defun wl-thread-delete-line-from-buffer (msg)
  "Simply delete msg line."
  (let (beg)
    (if (wl-summary-jump-to-msg msg)
	(progn
	  (setq beg (point))
	  (forward-line 1)
	  (delete-region beg (point))
	  t)
      nil)))

(defun wl-thread-cleanup-symbols (msgs)
  (let (sym)
    (while msgs
      ;; free symbol.
      (when (boundp (setq sym (intern (format "#%d" (car msgs))
				      wl-thread-entity-hashtb)))
	;; delete entity.
	(setq wl-thread-entities
	      (delq (wl-thread-get-entity (car msgs))
		    wl-thread-entities))
	(makunbound sym))
      (setq msgs (cdr msgs)))))

(defun wl-thread-delete-message (msg &optional update)
  "Delete MSG from entity and buffer."
  (save-excursion
    (let* ((entity (wl-thread-get-entity msg))
	   children children2
	   older-brothers younger-brothers ;;brothers
	   parent num)
      (when entity
	(setq parent (wl-thread-entity-get-parent-entity entity))
	(if parent
	    (progn
	      ;; has parent.
	      ;;(setq brothers (wl-thread-entity-get-children parent))
	      (setq older-brothers (wl-thread-entity-get-older-brothers
				    entity parent))
	      (setq younger-brothers (wl-thread-entity-get-younger-brothers
				      entity parent))
	      ;; 
	      (setq children (wl-thread-entity-get-children entity))
	      (mapcar '(lambda (x)
			(wl-thread-entity-set-parent
			 (wl-thread-get-entity x)
			 (wl-thread-entity-get-number parent)))
		      children)
	      (wl-thread-entity-set-children
	       parent
	       (append
		(append
		 older-brothers
		 children)
		younger-brothers)))
	  ;; top...children becomes top.
	  (mapcar '(lambda (x)
		    (wl-thread-entity-set-parent (wl-thread-get-entity x)
						 nil))
		  (setq children (wl-thread-entity-get-children entity)))
	  ;; delete myself from top list.
	  (setq older-brothers (wl-thread-entity-get-older-brothers
				entity nil))
	  (setq younger-brothers (wl-thread-entity-get-younger-brothers
				  entity nil))
	  (setq wl-thread-entity-list
		(append (append older-brothers children)
			younger-brothers))))
      
      ;; delete myself from buffer.
      (unless (wl-thread-delete-line-from-buffer msg)
	;; jump to suitable point.
	;; just upon the oldest younger-brother of my top.
	(let ((younger-bros (wl-thread-entity-get-younger-brothers
			     (wl-thread-entity-get-top-entity entity)
			     nil)))
	  (if younger-bros
	      (wl-summary-jump-to-msg (car younger-bros))
	    (goto-char (point-max)))) ; no younger brothers.
	)
      ;; insert children if thread is closed.
      (when (not (wl-thread-entity-get-opened entity))
	(setq children2 children)
	(while children2
	  (wl-thread-insert-entity 0 ; no mean now...
				   (wl-thread-get-entity
				    (car children2))
				   entity nil)
	  (setq children2 (cdr children2))))
      (if update
 	  ;; modify buffer.
 	  (progn
 	    (if parent
 		;; update parent on buffer.
 		(progn
		  (setq num (wl-thread-entity-get-number parent))
 		  (when num
 		    (wl-thread-update-line-on-buffer num)))
 	      ;; update children lines on buffer.
 	      (mapcar '(lambda (x)
			(wl-thread-update-line-on-buffer
 			 x
 			 (wl-thread-entity-get-number parent)))
 		      children)))
 	;; don't update buffer
	(if parent
 	    ;; return parent number
 	    (list (wl-thread-entity-get-number parent))
 	  children))
       ;; update the indent string
;	    (wl-summary-goto-top-of-current-thread)
;	    (setq beg (point))
;	    (wl-thread-goto-bottom-of-sub-thread)
;	    (wl-thread-update-indent-string-region beg (point)))
      )))
 
  
(defun wl-thread-insert-message (overview-entity overview mark-alist
				 msg parent-msg &optional update)
  "Insert MSG to the entity.
When optional argument UPDATE is non-nil,
Message is inserted to the summary buffer."
  (let ((parent (wl-thread-get-entity parent-msg))
	child-entity invisible-top)
;; Update the thread view...not implemented yet.
;    (when force-insert
;      (if parent
;	  (wl-thread-entity-force-open parent))
    (if parent
	;; insert as children.
	(wl-thread-entity-insert-as-children
	 parent
	 (setq child-entity (wl-thread-create-entity msg (nth 0 parent))))
      ;; insert as top message.
      (wl-thread-entity-insert-as-top
       (wl-thread-create-entity msg nil)))
    (if update
	(if (not (setq invisible-top
		       (wl-thread-entity-parent-invisible-p child-entity)))
	    ;; visible.
	    (progn
	      (wl-summary-update-thread
	       overview-entity
	       overview
	       mark-alist
	       child-entity
	       (elmo-msgdb-overview-get-entity-by-number overview parent-msg))
	      (when parent
		;; use thread structure.
		(wl-thread-entity-get-number
		 (wl-thread-entity-get-top-entity parent)))); return value;
;; 	      (setq beg (point))
;; 	      (wl-thread-goto-bottom-of-sub-thread)
;; 	      (wl-thread-update-indent-string-region beg (point)))
	  ;; currently invisible.. update closed line.
	  (wl-thread-update-children-number invisible-top)
	  nil))))

(defun wl-thread-update-indent-string-thread (top-list)
  (let (beg)
    (while top-list
      (wl-summary-jump-to-msg (car top-list))
      (setq beg (point))
      (wl-thread-goto-bottom-of-sub-thread)
      (wl-thread-update-indent-string-region beg (point))
      (setq top-list (cdr top-list)))))

(defun wl-thread-update-children-number (entity)
  "Update the children number."
  (save-excursion
    (wl-summary-jump-to-msg (wl-thread-entity-get-number entity))
    (beginning-of-line)
    (let ((text-prop (get-text-property (point) 'face))
	  from from-end beg str)
      (cond
       ((looking-at (concat "^" wl-summary-buffer-number-regexp
			    "..../..\(.*\)..:.. ["
			    wl-thread-indent-regexp
			    "]*\\[\\+\\([0-9]+\\):"))
	(delete-region (match-beginning 1)(match-end 1))
	(goto-char (match-beginning 1))
	(setq str (format "%s" (wl-thread-entity-get-children-num entity)))
	(if wl-summary-highlight
	    (put-text-property 0 (length str) 'face text-prop str))
	(insert str))
       ((looking-at (concat "^" wl-summary-buffer-number-regexp
			    "..../..\(.*\)..:.. ["
			    wl-thread-indent-regexp
			    "]*\\["))
	(goto-char (match-end 0))
	(setq beg (current-column))
	(setq from-end (save-excursion
			 (move-to-column (+ 1 beg wl-from-width))
			 (point)))
	(setq from (buffer-substring (match-end 0) from-end))
	(delete-region (match-end 0) from-end)
	(setq str (wl-set-string-width
		   (1+ wl-from-width)
		   (format
		    "+%s:%s"
		    (wl-thread-entity-get-children-num
		     entity)
		    from)))
	(if wl-summary-highlight
	    (put-text-property 0 (length str) 'face text-prop str))
	(insert str)
	(condition-case nil ; it's dangerous, so ignore error.
	    (run-hooks 'wl-thread-update-children-number-hook)
	  (error
	   (ding)
	   (message "Error in wl-thread-update-children-number-hook."))))))))

;; 
;; Thread oriented commands.
;;
(defun wl-thread-call-region-func (func &optional arg)
  (save-excursion
    (if arg
	(wl-summary-goto-top-of-current-thread)
      (beginning-of-line))
    (let ((beg (point)))
      (wl-thread-goto-bottom-of-sub-thread)
      (funcall func beg (point)))))

(defun wl-thread-prefetch (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-prefetch-region arg))

(defun wl-thread-msg-mark-as-read (msg)
  "Set mark as read for invisible MSG. Modeline is not changed."
  (let* ((msgdb wl-summary-buffer-msgdb)
	 (mark-alist (elmo-msgdb-get-mark-alist msgdb))
	 cur-mark)
    (setq cur-mark (cadr (assq msg mark-alist)))
    (cond ((or (string= cur-mark wl-summary-new-mark)
	       (string= cur-mark wl-summary-unread-uncached-mark))
	   ;; N,U -> u or " "
	   (setq mark-alist
		 (elmo-msgdb-mark-set mark-alist
				      msg
				      (if (elmo-use-cache-p
					   wl-summary-buffer-folder-name
					   msg)
					  wl-summary-read-uncached-mark)))
	   (elmo-msgdb-set-mark-alist msgdb mark-alist)
	   (wl-summary-set-mark-modified))
	  ((string= cur-mark wl-summary-unread-cached-mark)
	   ;; "!" -> " "
	   (setq mark-alist (elmo-msgdb-mark-set mark-alist msg nil))
	   (elmo-msgdb-set-mark-alist msgdb mark-alist)
	   (wl-summary-set-mark-modified)))))

(defun wl-thread-msg-mark-as-unread (msg)
  "Set mark as unread for invisible MSG. Modeline is not changed."
  (let* ((msgdb wl-summary-buffer-msgdb)
	 (mark-alist (elmo-msgdb-get-mark-alist msgdb))
	 cur-mark)
    (setq cur-mark (cadr (assq msg mark-alist)))
    (cond ((string= cur-mark wl-summary-read-uncached-mark)
	   ;; u -> U
	   (setq mark-alist
		 (elmo-msgdb-mark-set mark-alist
				      msg
				      wl-summary-unread-uncached-mark))
	   (elmo-msgdb-set-mark-alist msgdb mark-alist)
	   (wl-summary-set-mark-modified))
	  ((null cur-mark)
	   ;; " " -> "!"
	   (setq mark-alist (elmo-msgdb-mark-set mark-alist msg
				      wl-summary-unread-cached-mark))
	   (elmo-msgdb-set-mark-alist msgdb mark-alist)
	   (wl-summary-set-mark-modified)))))

(defun wl-thread-msg-mark-as-important (msg)
  "Set mark as important for invisible MSG. Modeline is not changed."
  (let* ((msgdb wl-summary-buffer-msgdb)
	 (mark-alist (elmo-msgdb-get-mark-alist msgdb))
	 cur-mark)
    (setq cur-mark (cadr (assq msg mark-alist)))
    (setq mark-alist
	  (elmo-msgdb-mark-set mark-alist
			       msg
			       (if (string= cur-mark wl-summary-important-mark)
				   nil
				 wl-summary-important-mark)))
    (elmo-msgdb-set-mark-alist msgdb mark-alist)
    (wl-summary-set-mark-modified)))

(defun wl-thread-mark-as-read (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-mark-as-read-region arg))

(defun wl-thread-mark-as-unread (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-mark-as-unread-region arg))

(defun wl-thread-mark-as-important (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-mark-as-important-region arg))

(defun wl-thread-copy (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-copy-region arg))

(defun wl-thread-refile (&optional arg)
  (interactive "P")
  (condition-case err
      (progn
	(wl-thread-call-region-func 'wl-summary-refile-region arg)
	(if arg
	    (wl-summary-goto-top-of-current-thread))
	(wl-thread-goto-bottom-of-sub-thread))
    (error
     (elmo-display-error err t)
     nil)))
	
(defun wl-thread-delete (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-delete-region arg)
  (if arg
      (wl-summary-goto-top-of-current-thread))
  (if (not wl-summary-move-direction-downward)
      (wl-summary-prev)
    (wl-thread-goto-bottom-of-sub-thread)
    (if wl-summary-buffer-disp-msg
	(wl-summary-redisplay))))

(defun wl-thread-target-mark (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-target-mark-region arg))

(defun wl-thread-unmark (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-unmark-region arg))

(defun wl-thread-exec (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-exec-region arg))

(defun wl-thread-save (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-save-region arg))

(defun wl-thread-force-open (&optional msg-num)
  "force open current folder"
  (if msg-num
      (wl-summary-jump-to-msg msg-num))
  (let ((wl-thread-insert-force-opened t))
    (wl-thread-open-close)))

(defun wl-thread-entity-force-open (entity)
  (let ((wl-thread-insert-force-opened t)
	notopen)
    (if (null (wl-thread-entity-get-parent entity))
	;; top!!
	(if (and (not (wl-thread-entity-get-opened entity))
		 (wl-thread-entity-get-children entity))
	    (wl-thread-force-open (wl-thread-entity-get-number entity)))
      (if (setq notopen (wl-thread-entity-parent-invisible-p entity))
	  (wl-thread-force-open (wl-thread-entity-get-number notopen))))))

(defun wl-thread-insert-top ()
  (let ((elist wl-thread-entity-list)
	(len (length wl-thread-entity-list))
	(cur 0))
    (wl-delete-all-overlays)
    (while elist
      (wl-thread-insert-entity
       0
       (wl-thread-get-entity (car elist))
       nil
       len)
      (setq cur (1+ cur))
      (elmo-display-progress
       'wl-thread-insert-top "Inserting thread..."
       (/ (* cur 100) len))
      (setq elist (cdr elist)))))

(defsubst wl-thread-insert-entity-sub (indent entity parent-entity all)
  (let ((number-alist (elmo-msgdb-get-number-alist wl-summary-buffer-msgdb))
	(overview (elmo-msgdb-get-overview wl-summary-buffer-msgdb))
	(mark-alist (elmo-msgdb-get-mark-alist wl-summary-buffer-msgdb))
	msg-num
	overview-entity
	temp-mark
	children-num
	summary-line
	score)
    (when (setq msg-num (wl-thread-entity-get-number entity))
      (unless all ; all...means no temp-mark.
	(cond ((memq msg-num wl-summary-buffer-delete-list)
	       (setq temp-mark "D"))
	      ((memq msg-num wl-summary-buffer-target-mark-list)
	       (setq temp-mark "*"))
	      ((assq msg-num wl-summary-buffer-refile-list)
	       (setq temp-mark "o"))
	      ((assq msg-num wl-summary-buffer-copy-list)
	       (setq temp-mark "O"))))
      (unless temp-mark
	(setq temp-mark (wl-summary-get-score-mark msg-num)))
      (setq children-num (wl-thread-entity-get-children-num entity))
      (setq overview-entity
	    (elmo-msgdb-search-overview-entity
	     (nth 0 entity) number-alist overview))
      ;;(wl-delete-all-overlays)
      (when overview-entity
	(setq summary-line
	      (wl-summary-overview-create-summary-line
	       msg-num
	       overview-entity
	       (assoc  ; parent-entity
		(cdr (assq (nth 0 parent-entity)
			   number-alist)) overview)
	       (1+ indent)
	       mark-alist
	       (if wl-thread-insert-force-opened
		   nil
		 (if (not (wl-thread-entity-get-opened entity))
		     (or children-num)))
	       temp-mark entity))
	(wl-summary-insert-line summary-line)))))

(defun wl-thread-insert-entity (indent entity parent-entity all)
  "Insert thread entity in current buffer."
  (let ((msgs (list (car entity)))
	children msgs-stack)
    (while msgs
      (wl-thread-insert-entity-sub indent entity parent-entity all)
      (setq msgs (cdr msgs))
      (setq children (nth 2 entity))
      (if children
	  ;; insert children
	  (when (or wl-thread-insert-force-opened
		    (wl-thread-entity-get-opened entity))
	    (wl-thread-entity-set-opened entity t)
	    (wl-push msgs msgs-stack)
	    (setq msgs children
		  indent (1+ indent)
		  parent-entity entity)))
      (unless msgs
	(while (and (null msgs) msgs-stack)
	  (setq msgs (wl-pop msgs-stack))
	  (setq indent (1- indent)))
	(when msgs
	  (setq entity (wl-thread-get-entity (car msgs)))
	  (setq parent-entity (wl-thread-entity-get-parent-entity entity))))
      (setq entity (wl-thread-get-entity (car msgs))))))

(defun wl-thread-descendant-p (mynumber number)
  (let ((cur (wl-thread-get-entity number))
	num)
    (catch 'done
      (while cur
	(setq cur (wl-thread-entity-get-parent-entity cur))
	(if (null (setq num (wl-thread-entity-get-number cur))) ; top!
	    (throw 'done nil))
	(if (and num
		 (eq mynumber (wl-thread-entity-get-number cur)))
	    (throw 'done t)))
      nil)))

; (defun wl-thread-goto-bottom-of-sub-thread ()
;   (interactive)
;   (let ((depth (wl-thread-get-depth-of-current-line)))
;     (forward-line 1)
;     (while (and (not (eobp))
; 		(> (wl-thread-get-depth-of-current-line)
; 		   depth))
;       (forward-line 1))
;     (beginning-of-line)))

(defun wl-thread-goto-bottom-of-sub-thread (&optional msg)
  (interactive)
  (let ((mynumber (or msg (wl-summary-message-number))))
    (forward-line 1)
    (while (wl-thread-descendant-p mynumber (wl-summary-message-number))
      (forward-line 1))
    (beginning-of-line)))

(defun wl-thread-remove-destination-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
	(let ((num (wl-summary-message-number)))
	  (if (assq num wl-summary-buffer-refile-list)
	      (wl-summary-remove-destination)))
	(forward-line 1)))))

(defun wl-thread-print-destination-region (beg end)
  (if (or wl-summary-buffer-refile-list
	  wl-summary-buffer-copy-list)
      (save-excursion
	(save-restriction
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (let ((num (wl-summary-message-number))
		  pair)
	      (if (or (setq pair (assq num wl-summary-buffer-refile-list))
		      (setq pair (assq num wl-summary-buffer-copy-list)))
		  (wl-summary-print-destination (car pair) (cdr pair))))
	    (forward-line 1))))))

(defsubst wl-thread-get-children-msgs (msg)
  (let ((msgs (list msg))
	msgs-stack children
	ret-val)
    (while msgs
      (wl-append ret-val (list (car msgs)))
      (setq children (wl-thread-entity-get-children
		      (wl-thread-get-entity (car msgs))))
      (setq msgs (cdr msgs))
      (if (null children)
	  (while (and (null msgs) msgs-stack)
	    (setq msgs (wl-pop msgs-stack)))
	(wl-push msgs msgs-stack)
	(setq msgs children)))
    ret-val))

(defun wl-thread-get-children-msgs-uncached (msg &optional uncached-marks)
  (let ((children-msgs (wl-thread-get-children-msgs msg))
	(mark-alist (elmo-msgdb-get-mark-alist wl-summary-buffer-msgdb))
	(number-alist (elmo-msgdb-get-number-alist wl-summary-buffer-msgdb))
	mark
	uncached-list)
    (while children-msgs
      (if (and (not (eq msg (car children-msgs))) ; except itself
	       (or (and uncached-marks
			(setq mark (cadr (assq (car children-msgs)
					       mark-alist)))
			(member mark uncached-marks))
		   (and (not uncached-marks)
			(null (elmo-cache-exists-p
			       (cdr (assq (car children-msgs)
					  number-alist)))))))
	  (wl-append uncached-list (list (car children-msgs))))
      (setq children-msgs (cdr children-msgs)))
    uncached-list))

(defun wl-thread-get-children-msgs-with-mark (msg mark)
  (let ((children-msgs (wl-thread-get-children-msgs msg))
	(check-func (cond ((string= mark "o")
			   'wl-summary-msg-marked-as-refiled)
			  ((string= mark "O")
			   'wl-summary-msg-marked-as-copied)
			  ((string= mark "D")
			   'wl-summary-msg-marked-as-deleted)
			  ((string= mark "*")
			   'wl-summary-msg-marked-as-target)))
	ret-val)
    (while children-msgs
      (if (funcall check-func (car children-msgs))
	  (wl-append ret-val (list (car children-msgs))))
      (setq children-msgs (cdr children-msgs)))
    ret-val))

(defun wl-thread-close (entity)
  (let (depth beg)
    (wl-thread-entity-set-opened entity nil)
    (setq depth (wl-thread-get-depth-of-current-line))
    (beginning-of-line)
    (setq beg (point))
    (wl-thread-goto-bottom-of-sub-thread)
    (wl-thread-remove-destination-region beg
					 (point))
    (forward-char -1)	;; needed for mouse-face.
    (delete-region beg (point))
    (wl-thread-insert-entity (- depth 1)
			     entity
			     (wl-thread-get-entity
			      (nth 3 entity))
			     nil)
    (delete-char 1) ; delete '\n'
    (wl-thread-print-destination-region beg (point))))

(defun wl-thread-open (entity)
  (let (depth beg)
    (beginning-of-line)
    (setq beg (point))
    (setq depth (wl-thread-get-depth-of-current-line))
    (end-of-line)
    (delete-region beg (point))
    (wl-thread-entity-set-opened entity t)
    (wl-thread-insert-entity depth ;(- depth 1)
			     entity
			     (wl-thread-get-entity
			      (nth 3 entity)) nil)
    (delete-char 1) ; delete '\n'
    (wl-thread-print-destination-region beg (point))))

(defun wl-thread-open-close (&optional force-open)
  (interactive "P")
  (when (eq wl-summary-buffer-view 'thread)
    ;(if (equal wl-thread-top-entity '(nil t nil nil))
    ;(error "There's no thread structure."))
    (save-excursion
      (let ((inhibit-read-only t)
	    (buffer-read-only nil)
	    (wl-thread-insert-force-opened
	     (or wl-thread-insert-force-opened
		 force-open))
	    msg entity beg depth parent)
	(setq msg (wl-summary-message-number))
	(setq entity (wl-thread-get-entity msg))
	(if (wl-thread-entity-get-opened entity)
	    ;; if already opened, close its child!
	  (if (wl-thread-entity-get-children entity)
	      (wl-thread-close entity)
	    ;; opened, but has no children, close its parent!
	    (when (setq parent (wl-thread-entity-get-parent entity))
	      (wl-summary-jump-to-msg parent)
	      (wl-thread-close
	       (wl-thread-get-entity (wl-summary-message-number)))))
	  ;; if closed (or it is just a thread bottom message)
	  ;; has children, open it!
	  (if (wl-thread-entity-get-children entity)
	      (wl-thread-open entity)
	    ;; closed, and has no children, close its parent!
	    (setq msg (or (wl-thread-entity-get-parent entity)
			  (wl-thread-entity-get-number entity)))
	    (when msg
	      (wl-summary-jump-to-msg msg)
	      (wl-thread-close
	       (wl-thread-get-entity (wl-summary-message-number)))))))
      (wl-summary-set-message-modified)
      (set-buffer-modified-p nil))))
  

(defun wl-thread-get-depth-of-current-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((depth 0))
      (if (re-search-forward (concat "^" wl-summary-buffer-number-regexp
				     "..../..\(.*\)..:.. ")
			     nil t)
	  (while (string-match wl-thread-indent-regexp
			       (char-to-string
				(char-after (point))))
	    (setq depth (1+ depth))
	    (forward-char)))
      (/ depth wl-thread-indent-level-internal))))

(defun wl-thread-update-indent-string-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (wl-thread-update-indent-string)
      (forward-line 1))))

(defsubst wl-thread-make-indent-string (entity)
  (let ((cur entity)
	(ret-val "")
	(space-str (wl-repeat-string wl-thread-space-str-internal
				     (- wl-thread-indent-level-internal 1)))
	parent)
    (when (wl-thread-entity-get-number
	   (setq parent (wl-thread-entity-get-parent-entity cur)))
      (if (wl-thread-entity-get-younger-brothers cur)
	  (setq ret-val wl-thread-have-younger-brother-str-internal)
	(setq ret-val wl-thread-youngest-child-str-internal))
      (setq ret-val (concat ret-val
			    (wl-repeat-string
			     wl-thread-horizontal-str-internal
			     (- wl-thread-indent-level-internal 1))))
      (setq cur parent)
      (while (wl-thread-entity-get-number
	      (wl-thread-entity-get-parent-entity cur))
	(if (wl-thread-entity-get-younger-brothers cur)
	    (setq ret-val (concat wl-thread-vertical-str-internal
				  space-str
				  ret-val))
	  (setq ret-val (concat wl-thread-space-str-internal
				space-str
				ret-val)))
	(setq cur (wl-thread-entity-get-parent-entity cur))))
    ret-val))

(defun wl-thread-update-indent-string ()
  "Update indent string of current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  thr-str)
      (when (looking-at (concat "^ *\\([0-9]+\\)"
				"..../..\(.*\)..:.. \\("
				wl-highlight-thread-indent-string-regexp
				"\\)\\["))
	(goto-char (match-beginning 2))
	(delete-region (match-beginning 2)
		       (match-end 2))
	(setq thr-str
	      (wl-thread-make-indent-string
	       (wl-thread-get-entity (string-to-int (wl-match-buffer 1)))))
	(if (and wl-summary-width
		 wl-summary-indent-length-limit
		 (< wl-summary-indent-length-limit
		    (string-width thr-str)))
	    (setq thr-str (wl-set-string-width
			   wl-summary-indent-length-limit
			   thr-str)))
	(insert thr-str)
	(if wl-summary-highlight
	    (wl-highlight-summary-current-line))))))

(provide 'wl-thread)

;;; wl-thread.el ends here
