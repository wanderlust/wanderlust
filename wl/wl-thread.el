;;; wl-thread.el -- Thread display modules for Wanderlust.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000 Masahiro MURATA  <muse@ba2.so-net.ne.jp>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA  <muse@ba2.so-net.ne.jp>
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
;;(defvar wl-thread-top-entity '(nil t nil nil)) ; top entity
(defvar wl-thread-tops nil)           ; top number list (number)
(defvar wl-thread-entities nil)
(defvar wl-thread-entity-list nil)    ; entity list
(defvar wl-thread-entity-hashtb nil)  ; obarray
(defvar wl-thread-indent-regexp nil)

(make-variable-buffer-local 'wl-thread-entity-hashtb)
(make-variable-buffer-local 'wl-thread-entities)     ; ".wl-thread-entity"
(make-variable-buffer-local 'wl-thread-entity-list)  ; ".wl-thread-entity-list"
(make-variable-buffer-local 'wl-thread-entity-cur)
(make-variable-buffer-local 'wl-thread-indent-regexp)

;;; global flag
(defvar wl-thread-insert-force-opened nil)

;;;;;; each entity is (number opened-or-not children parent) ;;;;;;;

(defun wl-meaning-of-mark (mark)
  (if (not (elmo-folder-plugged-p wl-summary-buffer-elmo-folder))
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
  (cond ((not (elmo-folder-plugged-p wl-summary-buffer-elmo-folder))
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
  (cond ((not (elmo-folder-plugged-p wl-summary-buffer-elmo-folder))
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
				      (elmo-folder-msgdb-path fld))))
    (setq top-list
	  (wl-summary-load-file-object
	   (expand-file-name wl-thread-entity-list-file
			     (elmo-folder-msgdb-path fld))))
    (current-buffer)
    (message "Resuming thread structure...")
    ;; set obarray value.
    (setq wl-thread-entity-hashtb (elmo-make-hash (* (length entities) 2)))
    ;; set buffer local variables.
    (setq wl-thread-entities entities)
    (setq wl-thread-entity-list top-list)
    (while entities
      (elmo-set-hash-val (format "#%d" (car (car entities))) (car entities)
			 wl-thread-entity-hashtb)
      (setq entities (cdr entities)))
    (message "Resuming thread structure...done")))

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
(defsubst wl-thread-entity-get-linked (entity)
  (nth 4 entity))

(defsubst wl-thread-create-entity (num parent &optional opened linked)
  (list num (or opened wl-thread-insert-opened) nil parent linked))

(defsubst wl-thread-get-entity (num)
  (and num
       (elmo-get-hash-val (format "#%d" num) wl-thread-entity-hashtb)))

(defsubst wl-thread-entity-set-parent (entity parent)
  (setcar (cdddr entity) parent)
  entity)

(defsubst wl-thread-entity-set-children (entity children)
  (setcar (cddr entity) children))

(defsubst wl-thread-entity-set-linked (entity linked)
  (if (cddddr entity)
      (setcar (cddddr entity) linked)
    (nconc entity (list linked)))
  entity)

(defsubst wl-thread-reparent-children (children parent)
  (while children
    (wl-thread-entity-set-parent
     (wl-thread-get-entity (car children)) parent)
    (wl-thread-entity-set-linked
     (wl-thread-get-entity (car children)) t)
    (setq children (cdr children))))

(defsubst wl-thread-entity-insert-as-top (entity)
  (when (and entity
	     (car entity))
    (wl-append wl-thread-entity-list (list (car entity)))
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
  (let ((mark-alist (elmo-msgdb-get-mark-alist (wl-summary-buffer-msgdb)))
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

(defun wl-thread-entity-get-nearly-older-brother (entity &optional parent)
  (let ((brothers (wl-thread-entity-get-older-brothers entity parent)))
    (when brothers
      (car (last brothers)))))

(defun wl-thread-entity-get-older-brothers (entity &optional parent)
  (let* ((parent (or parent
		     (wl-thread-entity-get-parent-entity entity)))
	 (brothers (wl-thread-entity-get-children parent))
	 ret-val)
    (if parent
	brothers
      (setq brothers wl-thread-entity-list))
    (while (and brothers
		(not (eq (wl-thread-entity-get-number entity)
			 (car brothers))))
      (wl-append ret-val (list (car brothers)))
      (setq brothers (cdr brothers)))
    ret-val))

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
  (let* (older-brother)
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
  "If prev unread is a children of a closed message.
The closed parent will be opened."
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
				 wl-summary-buffer-elmo-folder))
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
  "If next unread is a children of a closed message.
The closed parent will be opened."
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
  (save-excursion
    (let ((entities wl-thread-entity-list)
	  (cur 0)
	  (len (length wl-thread-entity-list)))
      (while entities
	(when (and (wl-thread-entity-get-opened (wl-thread-get-entity
						 (car entities)))
		   (wl-thread-entity-get-children (wl-thread-get-entity
						   (car entities))))
	  (wl-summary-jump-to-msg (car entities))
	  (wl-thread-open-close))
	(when (> len elmo-display-progress-threshold)
	  (setq cur (1+ cur))
	  (if (or (zerop (% cur 5)) (= cur len))
	      (elmo-display-progress
	       'wl-thread-close-all "Closing all threads..."
	       (/ (* cur 100) len))))
	(setq entities (cdr entities)))))
  (message "Closing all threads...done"))

(defun wl-thread-open-all ()
  "Open all threads."
  (interactive)
  (message "Opening all threads...")
  (save-excursion
    (goto-char (point-min))
    (let ((len (count-lines (point-min) (point-max)))
	  (cur 0)
	  entity)
      (while (not (eobp))
	(if (wl-thread-entity-get-opened
	     (setq entity (wl-thread-get-entity
			   (wl-summary-message-number))))
	    (forward-line 1)
	  (wl-thread-force-open)
	  (wl-thread-goto-bottom-of-sub-thread))
	(when (> len elmo-display-progress-threshold)
	  (setq cur (1+ cur))
	  (elmo-display-progress
	   'wl-thread-open-all "Opening all threads..."
	   (/ (* cur 100) len)))))
    ;; Make sure to be 100%.
    (elmo-display-progress
     'wl-thread-open-all "Opening all threads..."
     100))
  (message "Opening all threads...done"))

(defun wl-thread-open-all-unread ()
  (interactive)
  (let ((mark-alist (elmo-msgdb-get-mark-alist (wl-summary-buffer-msgdb)))
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

(defsubst wl-thread-maybe-get-children-num (msg)
  (let ((entity (wl-thread-get-entity msg)))
    (if (not (wl-thread-entity-get-opened entity))
	(wl-thread-entity-get-children-num entity))))

(defsubst wl-thread-update-line-on-buffer-sub (entity msg &optional parent-msg)
  (let* ((entity (or entity (wl-thread-get-entity msg)))
	 (parent-msg (or parent-msg (wl-thread-entity-get-parent entity)))
	 (overview (elmo-msgdb-get-overview (wl-summary-buffer-msgdb)))
	 (mark-alist (elmo-msgdb-get-mark-alist (wl-summary-buffer-msgdb)))
	 (buffer-read-only nil)
	 (inhibit-read-only t)
	 overview-entity temp-mark summary-line invisible-top dest-pair)
    (if (wl-thread-delete-line-from-buffer msg)
	(progn
	  (cond
	   ((memq msg wl-summary-buffer-delete-list)
	    (setq temp-mark "D"))
	   ((memq msg wl-summary-buffer-target-mark-list)
	    (setq temp-mark "*"))
	   ((setq dest-pair (assq msg wl-summary-buffer-refile-list))
	    (setq temp-mark "o"))
	   ((setq dest-pair (assq msg wl-summary-buffer-copy-list))
	    (setq temp-mark "O"))
	   (t (setq temp-mark (wl-summary-get-score-mark msg))))
	  (when (setq overview-entity
		      (elmo-msgdb-overview-get-entity
		       msg (wl-summary-buffer-msgdb)))
	    (setq summary-line
		  (wl-summary-overview-create-summary-line
		   msg
		   overview-entity
		   (elmo-msgdb-overview-get-entity
		    parent-msg (wl-summary-buffer-msgdb))
		   nil
		   mark-alist
		   (if wl-thread-insert-force-opened
		       nil
		     (wl-thread-maybe-get-children-num msg))
		   temp-mark entity))
	    (save-excursion
	      (wl-summary-insert-line summary-line))
	    (if dest-pair
		(wl-summary-print-destination (car dest-pair)
					      (cdr dest-pair)))))
      ;; insert thread (moving thread)
      (if (not (setq invisible-top
		     (wl-thread-entity-parent-invisible-p entity)))
	  (wl-summary-update-thread
	   (elmo-msgdb-overview-get-entity msg (wl-summary-buffer-msgdb))
	   overview
	   mark-alist
	   entity
	   (and parent-msg
		(elmo-msgdb-overview-get-entity
		 parent-msg (wl-summary-buffer-msgdb))))
	;; currently invisible.. update closed line.
	(wl-thread-update-children-number invisible-top)))))

(defun wl-thread-update-line-on-buffer (&optional msg parent-msg updates)
  (interactive)
  (let ((msgs (list (or msg (wl-summary-message-number))))
	entity children msgs-stack)
   (while msgs
    (setq msg (wl-pop msgs))
    (setq updates (and updates (delete msg updates)))
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
		  (wl-thread-get-entity (car msgs)))))))))
   updates))

(defun wl-thread-update-line-msgs (msgs &optional no-msg)
  (wl-delete-all-overlays)
  (let ((i 0)
	(updates msgs)
	len)
;;; (while msgs
;;;   (setq updates
;;;	    (append updates
;;;		    (wl-thread-get-children-msgs (car msgs))))
;;;   (setq msgs (cdr msgs)))
;;; (setq updates (elmo-uniq-list updates))
    (setq len (length updates))
    (while updates
      (wl-thread-update-line-on-buffer-sub nil (car updates))
      (setq updates (cdr updates))
      (when (and (not no-msg)
		 (> len elmo-display-progress-threshold))
	(setq i (1+ i))
	(if (or (zerop (% i 5)) (= i len))
	    (elmo-display-progress
	     'wl-thread-update-line-msgs "Updating deleted thread..."
	     (/ (* i 100) len)))))))

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
  (let (entity)
    (while msgs
      (when (setq entity (wl-thread-get-entity (car msgs)))
	;; delete entity.
	(setq wl-thread-entities (delq entity wl-thread-entities))
	;; free symbol.
	(elmo-clear-hash-val (format "#%d" (car msgs))
			     wl-thread-entity-hashtb))
      (setq msgs (cdr msgs)))))

(defun wl-thread-get-exist-children (msg)
  (let ((msgs (list msg))
	msgs-stack children
	entity ret-val)
    (while msgs
      (setq children (wl-thread-entity-get-children
		      (setq entity (wl-thread-get-entity (car msgs)))))
      (when (elmo-msgdb-overview-get-entity (car msgs) (wl-summary-buffer-msgdb))
	(wl-append ret-val (list (car msgs)))
	(setq children nil))
      (setq msgs (cdr msgs))
      (if (null children)
	  (while (and (null msgs) msgs-stack)
	    (setq msgs (wl-pop msgs-stack)))
	(wl-push msgs msgs-stack)
	(setq msgs children)))
    ret-val))

(defun wl-thread-delete-message (msg &optional deep update)
  "Delete MSG from entity and buffer."
  (save-excursion
    (let* ((entity (wl-thread-get-entity msg))
	   children older-brothers younger-brothers top-child ;;grandchildren
	   top-entity parent update-msgs beg invisible-top)
      (when entity
	(setq parent (wl-thread-entity-get-parent-entity entity))
	(if parent
	    (progn
;;; has parent.
;;;	      (setq brothers (wl-thread-entity-get-children parent))
	      (setq older-brothers (wl-thread-entity-get-older-brothers
				    entity parent))
	      (setq younger-brothers (wl-thread-entity-get-younger-brothers
				      entity parent))
	      ;;
	      (unless deep
		(setq children (wl-thread-entity-get-children entity))
		(wl-thread-reparent-children
		 children (wl-thread-entity-get-number parent))
		(setq update-msgs
		      (apply (function nconc)
			     update-msgs
			     (mapcar
			      (function
			       (lambda (message)
				 (wl-thread-get-children-msgs message t)))
			      children))))
	      (wl-thread-entity-set-children
	       parent (append older-brothers children younger-brothers))
	      ;; If chidren and younger-brothers not exists,
	      ;; update nearly older brother.
	      (when (and older-brothers
			 (not younger-brothers)
			 (not children))
		(wl-append
		 update-msgs
		 (wl-thread-get-children-msgs (car (last older-brothers))))))

	  ;; top...oldest child becomes top.
	  (unless deep
	    (setq children (wl-thread-entity-get-children entity))
	    (when children
	      (setq top-child (car children)
		    children (cdr children))
	      (setq top-entity (wl-thread-get-entity top-child))
	      (wl-thread-entity-set-parent top-entity nil)
	      (wl-thread-entity-set-linked top-entity nil)
	      (wl-append update-msgs
			 (wl-thread-get-children-msgs top-child t)))
	    (when children
	      (wl-thread-entity-set-children
	       top-entity
	       (append
		(wl-thread-entity-get-children top-entity)
		children))
	      (wl-thread-reparent-children children top-child)
	      (wl-append update-msgs children)))
	  ;; delete myself from top list.
	  (setq older-brothers (wl-thread-entity-get-older-brothers
				entity nil))
	  (setq younger-brothers (wl-thread-entity-get-younger-brothers
				  entity nil))
	  (setq wl-thread-entity-list
		(append (append older-brothers
				(and top-child (list top-child)))
			younger-brothers))))

      (if deep
	  ;; delete thread on buffer
	  (when (wl-summary-jump-to-msg msg)
	    (setq beg (point))
	    (wl-thread-goto-bottom-of-sub-thread)
	    (delete-region beg (point)))
	;; delete myself from buffer.
	(unless (wl-thread-delete-line-from-buffer msg)
	  ;; jump to suitable point.
	  ;; just upon the oldest younger-brother of my top.
	  (setq invisible-top
		(car (wl-thread-entity-parent-invisible-p entity)))
	  (if invisible-top
	      (progn
		(wl-append update-msgs (list invisible-top))
		(wl-summary-jump-to-msg invisible-top))
	    (goto-char (point-max))))

	;; insert children if thread is closed or delete top.
	(when (or top-child
		  (not (wl-thread-entity-get-opened entity)))
	  (let* (next-top insert-msgs ent e grandchildren)
	    (if top-child
		(progn
		  (setq insert-msgs (wl-thread-get-exist-children top-child))
		  (setq next-top (car insert-msgs))
		  (setq ent (wl-thread-get-entity next-top))
		  (when (and
			 (wl-thread-entity-get-opened entity) ;; open
			 (not (wl-thread-entity-get-opened ent)) ;; close
			 (setq grandchildren
			       (wl-thread-entity-get-children ent))
			 (wl-summary-jump-to-msg next-top))
		    (forward-line 1)
		    (setq insert-msgs (append (cdr insert-msgs) grandchildren)))
		  (when top-entity (wl-thread-entity-set-opened top-entity t))
		  (when ent (wl-thread-entity-set-opened ent t)))
	      (when (not invisible-top)
		(setq insert-msgs (wl-thread-get-exist-children msg))
		;; First msg always opened, because first msg maybe becomes top.
		(if (setq ent (wl-thread-get-entity (car insert-msgs)))
		    (wl-thread-entity-set-opened ent t))))
	    ;; insert children
	    (while insert-msgs
	      ;; if no exists in summary, insert entity.
	      (when (and (car insert-msgs)
			 (not (wl-summary-jump-to-msg (car insert-msgs))))
		(setq ent (wl-thread-get-entity (car insert-msgs)))
		(wl-thread-insert-entity 0 ; no mean now...
					 ent entity nil))
	      (setq insert-msgs (cdr insert-msgs))))))
      (if update
 	  ;; modify buffer.
	  (while update-msgs
	    (wl-thread-update-line-on-buffer-sub nil (pop update-msgs)))
 	;; don't update buffer
	update-msgs)))) ; return value

(defun wl-thread-insert-message (overview-entity overview mark-alist
				 msg parent-msg &optional update linked)
  "Insert MSG to the entity.
When optional argument UPDATE is non-nil,
Message is inserted to the summary buffer."
  (let ((parent (wl-thread-get-entity parent-msg))
	child-entity invisible-top)
;;; Update the thread view...not implemented yet.
;;;  (when force-insert
;;;    (if parent
;;;	  (wl-thread-entity-force-open parent))
    (if parent
	;; insert as children.
	(wl-thread-entity-insert-as-children
	 parent
	 (setq child-entity (wl-thread-create-entity msg (nth 0 parent) nil linked)))
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
	       (elmo-msgdb-overview-get-entity
		parent-msg (wl-summary-buffer-msgdb)))
	      (when parent
		;; use thread structure.
		(wl-thread-entity-get-nearly-older-brother
		 child-entity parent))) ; return value
;;; 		(wl-thread-entity-get-number
;;; 		 (wl-thread-entity-get-top-entity parent)))) ; return value;
;;; 	      (setq beg (point))
;;; 	      (wl-thread-goto-bottom-of-sub-thread)
;;; 	      (wl-thread-update-indent-string-region beg (point)))
	  ;; currently invisible.. update closed line.
	  (wl-thread-update-children-number invisible-top)
	  nil))))

(defun wl-thread-get-parent-list (msgs)
  (let* ((msgs2 msgs)
	 myself)
    (while msgs2
      (setq myself (car msgs2)
	    msgs2 (cdr msgs2))
      (while (not (eq myself (car msgs2)))
	(if (wl-thread-descendant-p myself (car msgs2))
	    (setq msgs (delq (car msgs2) msgs)))
	(setq msgs2 (or (cdr msgs2) msgs)))
      (setq msgs2 (cdr msgs2)))
    msgs))

(defun wl-thread-update-indent-string-thread (top-list)
  (let ((top-list (wl-thread-get-parent-list top-list))
	beg)
    (while top-list
      (when (car top-list)
	(wl-summary-jump-to-msg (car top-list))
	(setq beg (point))
	(wl-thread-goto-bottom-of-sub-thread)
	(wl-thread-update-indent-string-region beg (point)))
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
			    "]*[[<]\\+\\([0-9]+\\):"))
	(delete-region (match-beginning 1)(match-end 1))
	(goto-char (match-beginning 1))
	(setq str (format "%s" (wl-thread-entity-get-children-num entity)))
	(if wl-summary-highlight
	    (put-text-property 0 (length str) 'face text-prop str))
	(insert str))
       ((looking-at (concat "^" wl-summary-buffer-number-regexp
			    "..../..\(.*\)..:.. ["
			    wl-thread-indent-regexp
			    "]*[[<]"))
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

(defun wl-thread-msg-mark-as-important (msg)
  "Set mark as important for invisible MSG. Modeline is not changed."
  (let* ((msgdb (wl-summary-buffer-msgdb))
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
      (setq elist (cdr elist))
      (when (> len elmo-display-progress-threshold)
	(setq cur (1+ cur))
	(if (or (zerop (% cur 2)) (= cur len))
	    (elmo-display-progress
	     'wl-thread-insert-top "Inserting thread..."
	     (/ (* cur 100) len)))))))

(defsubst wl-thread-insert-entity-sub (indent entity parent-entity all)
  (let ((mark-alist (elmo-msgdb-get-mark-alist (wl-summary-buffer-msgdb)))
	msg-num
	overview-entity
	temp-mark
	summary-line)
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
      (setq overview-entity
	    (elmo-msgdb-overview-get-entity
	     (nth 0 entity) (wl-summary-buffer-msgdb)))
;;;   (wl-delete-all-overlays)
      (when overview-entity
	(setq summary-line
	      (wl-summary-overview-create-summary-line
	       msg-num
	       overview-entity
	       (elmo-msgdb-overview-get-entity
		(nth 0 parent-entity) (wl-summary-buffer-msgdb))
	       (1+ indent)
	       mark-alist
	       (if wl-thread-insert-force-opened
		   nil
		 (wl-thread-maybe-get-children-num msg-num))
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

;; (defun wl-thread-goto-bottom-of-sub-thread ()
;;   (interactive)
;;   (let ((depth (wl-thread-get-depth-of-current-line)))
;;     (forward-line 1)
;;     (while (and (not (eobp))
;; 		(> (wl-thread-get-depth-of-current-line)
;; 		   depth))
;;       (forward-line 1))
;;     (beginning-of-line)))

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

(defsubst wl-thread-get-children-msgs (msg &optional visible-only)
  (let ((msgs (list msg))
	msgs-stack children
	entity ret-val)
    (while msgs
      (wl-append ret-val (list (car msgs)))
      (setq children (wl-thread-entity-get-children
		      (setq entity (wl-thread-get-entity (car msgs)))))
      (if (and visible-only
	       (not (wl-thread-entity-get-opened entity)))
	  (setq children nil))
      (setq msgs (cdr msgs))
      (if (null children)
	  (while (and (null msgs) msgs-stack)
	    (setq msgs (wl-pop msgs-stack)))
	(wl-push msgs msgs-stack)
	(setq msgs children)))
    ret-val))

(defun wl-thread-get-children-msgs-uncached (msg &optional uncached-marks)
  (let ((children-msgs (wl-thread-get-children-msgs msg))
	(mark-alist (elmo-msgdb-get-mark-alist (wl-summary-buffer-msgdb)))
	(number-alist (elmo-msgdb-get-number-alist (wl-summary-buffer-msgdb)))
	mark
	uncached-list)
    (while children-msgs
      (if (and (not (eq msg (car children-msgs))) ; except itself
	       (or (and uncached-marks
			(setq mark (cadr (assq (car children-msgs)
					       mark-alist)))
			(member mark uncached-marks))
		   (and (not uncached-marks)
			(null (elmo-file-cache-exists-p
			       (elmo-message-field
				wl-summary-buffer-elmo-folder
				(car children-msgs)
				'message-id))))))
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
;;; (if (equal wl-thread-top-entity '(nil t nil nil))
;;;	(error "There's no thread structure"))
    (save-excursion
      (let ((inhibit-read-only t)
	    (buffer-read-only nil)
	    (wl-thread-insert-force-opened
	     (or wl-thread-insert-force-opened
		 force-open))
	    msg entity parent)
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
				"\\)[[<]"))
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

(defun wl-thread-set-parent (&optional parent-number)
  "Set current message's parent interactively."
  (interactive)
  (let ((number (wl-summary-message-number))
	(dst-parent (if (interactive-p)
			(read-from-minibuffer "Parent Message (No.): ")))
	entity dst-parent-entity src-parent children
	update-msgs
	buffer-read-only)
    (if (string= dst-parent "")
	(setq dst-parent nil)
      (if (interactive-p)
	  (setq dst-parent (string-to-int dst-parent))
	(setq dst-parent parent-number)))
    (if (and dst-parent
	     (memq dst-parent (wl-thread-get-children-msgs number)))
	(error "Parent is children or myself"))
    (setq entity (wl-thread-get-entity number))
    (when (and number entity)
      ;; delete thread
      (setq update-msgs (wl-thread-delete-message number 'deep))
      ;; insert as child at new parent
      (setq dst-parent-entity (wl-thread-get-entity dst-parent))
      (if dst-parent-entity
	  (progn
	    (if (setq children
		      (wl-thread-entity-get-children dst-parent-entity))
		(wl-append update-msgs
			   (wl-thread-get-children-msgs
			    (car (last children)) t)))
	    (wl-thread-entity-set-children
	     dst-parent-entity
	     (append children (list number)))
	    (wl-thread-entity-set-linked entity t))
	;; insert as top
	(wl-append wl-thread-entity-list (list number))
	(wl-thread-entity-set-linked entity nil))

      ;; update my thread
      (wl-append update-msgs (wl-thread-get-children-msgs number t))
      (setq update-msgs (elmo-uniq-list update-msgs))
      (wl-thread-entity-set-parent entity dst-parent)
      ;; update thread on buffer
      (wl-thread-update-line-msgs update-msgs t))))

(require 'product)
(product-provide (provide 'wl-thread) (require 'wl-version))

;;; wl-thread.el ends here
