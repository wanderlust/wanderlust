;;; elmo-multi.el -- Multiple Folder Interface for ELMO.

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
(require 'elmo-vars)
(require 'elmo2)

(defun elmo-multi-msgdb (msgdb base)
  (list (mapcar (function
		 (lambda (x)
		   (elmo-msgdb-overview-entity-set-number
		    x
		    (+ base
		       (elmo-msgdb-overview-entity-get-number x)))))
		(nth 0 msgdb))
	(mapcar (function
		 (lambda (x) (cons
			      (+ base (car x))
			      (cdr x))))
		(nth 1 msgdb))
	(mapcar (function
		 (lambda (x) (cons
			      (+ base (car x))
			      (cdr x)))) (nth 2 msgdb))))

(defun elmo-multi-msgdb-create-as-numlist (spec numlist new-mark already-mark
						seen-mark important-mark
						seen-list)
  (when numlist
    (let* ((flds (cdr spec))
	   overview number-alist mark-alist entity
	   one-list-list
	   cur-number
	   i percent num
	   ret-val)
      (setq one-list-list (elmo-multi-get-intlist-list numlist))
      (setq cur-number 0)
      (while (< cur-number (length flds))
	(setq ret-val
	      (elmo-msgdb-append
	       ret-val
	       (elmo-multi-msgdb
		(elmo-msgdb-create-as-numlist (nth cur-number flds)
					      (nth cur-number one-list-list)
					      new-mark already-mark
					      seen-mark important-mark
					      seen-list)
		(* elmo-multi-divide-number (1+ cur-number)))))
	(setq cur-number (1+ cur-number)))
      (elmo-msgdb-sort-by-date ret-val))))

;; returns append-msgdb
(defun elmo-multi-delete-crossposts (already-msgdb append-msgdb)
  (let* ((number-alist (elmo-msgdb-get-number-alist append-msgdb))
	 (dummy (copy-sequence (append
				number-alist
				(elmo-msgdb-get-number-alist already-msgdb))))
	 (cur number-alist)
	 to-be-deleted
	 overview mark-alist
	 same)
    (while cur
      (setq dummy (delq (car cur) dummy))
      (if (setq same (rassoc (cdr (car cur)) dummy)) ;; same message id is remained
	  (unless (= (/ (car (car cur)) elmo-multi-divide-number)
		     (/ (car same) elmo-multi-divide-number))
	    ;; base is also same...delete it!
	    (setq to-be-deleted (append to-be-deleted (list (car cur))))))
      (setq cur (cdr cur)))
    (setq overview (elmo-delete-if
		    (function
		     (lambda (x)
		       (assq
			(elmo-msgdb-overview-entity-get-number x)
			to-be-deleted)))
		    (elmo-msgdb-get-overview append-msgdb)))
    (setq mark-alist (elmo-delete-if
		      (function
		       (lambda (x)
			 (assq
			  (car x) to-be-deleted)))
		      (elmo-msgdb-get-mark-alist append-msgdb)))
    ;; keep number-alist untouched for folder diff!!
    (cons (and to-be-deleted (length to-be-deleted))
	  (list overview number-alist mark-alist))))

(defun elmo-multi-msgdb-create (spec numlist new-mark already-mark
				     seen-mark important-mark seen-list)
  (when numlist
    (let* ((flds (cdr spec))
	   overview number-alist mark-alist entity
	   one-list-list
	   cur-number
	   i percent num
	   ret-val)
      (setq one-list-list (elmo-multi-get-intlist-list numlist))
      (setq cur-number 0)
      (while (< cur-number (length flds))
	(setq ret-val
	      (elmo-msgdb-append
	       ret-val
	       (elmo-multi-msgdb
		(elmo-msgdb-create (nth cur-number flds)
				   (nth cur-number one-list-list)
				   new-mark already-mark
				   seen-mark important-mark
				   seen-list)
		(* elmo-multi-divide-number (1+ cur-number)))))
	(setq cur-number (1+ cur-number)))
      (elmo-msgdb-sort-by-date ret-val))))

(defun elmo-multi-list-folders (spec &optional hierarchy)
  ;; not implemented.
  nil)

(defun elmo-multi-append-msg (spec string)
  (error "Cannot append messages to multi folder"))

(defun elmo-multi-read-msg (spec number outbuf)
  (let* ((flds (cdr spec))
	 (folder (nth (- (/ number elmo-multi-divide-number) 1) flds))
	 (number (% number elmo-multi-divide-number)))
    (elmo-call-func folder "read-msg" number outbuf)))

(defun elmo-multi-delete-msgs (spec msgs)
  (let ((flds (cdr spec))
	one-list-list
	(cur-number 0))
    (setq one-list-list (elmo-multi-get-intlist-list msgs))
    (while (< cur-number (length flds))
      (elmo-delete-msgs (nth cur-number flds)
			(nth cur-number one-list-list))
      (setq cur-number (+ 1 cur-number)))
    t))

(defun elmo-multi-mark-alist-list (mark-alist)
  (let ((cur-number 0)
	one-alist result)
    (while mark-alist
      (setq cur-number (+ cur-number 1))
      (setq one-alist nil)
      (while (and mark-alist
		  (eq 0
		      (/ (- (car (car mark-alist))
			    (* elmo-multi-divide-number cur-number))
			 elmo-multi-divide-number)))
	(setq one-alist (nconc
			 one-alist
			 (list
			  (list (% (car (car mark-alist))
				   (* elmo-multi-divide-number cur-number))
				(cadr (car mark-alist))))))
	(setq mark-alist (cdr mark-alist)))
      (setq result (nconc result (list one-alist))))
    result))

(defun elmo-multi-list-folder-unread (spec msgdb unread-marks)
  (let* ((flds (cdr spec))
	 (cur-number 0)
	 (mark-alist (elmo-msgdb-get-mark-alist msgdb))
	 mark-alist-list
	 ret-val)
    (setq mark-alist-list (elmo-multi-mark-alist-list mark-alist))
    (while flds
      (setq cur-number (+ cur-number 1))
      (setq ret-val (append
		     ret-val
		     (mapcar
		      (function
		       (lambda (x)
			 (+
			  (* elmo-multi-divide-number cur-number) x)))
		      (elmo-list-folder-unread (car flds)
					       (car mark-alist-list)
					       unread-marks))))
      (setq mark-alist-list (cdr mark-alist-list))
      (setq flds (cdr flds)))
    ret-val))

(defun elmo-multi-list-folder-important (spec msgdb)
  (let* ((flds (cdr spec))
	 (cur-number 0)
	 ret-val)
    (while flds
      (setq cur-number (+ cur-number 1))
      (setq ret-val (append
		     ret-val
		     (mapcar
		      (function
		       (lambda (x)
			 (+
			  (* elmo-multi-divide-number cur-number) x)))
		      (elmo-list-folder-important (car flds)
						  msgdb))))
      (setq flds (cdr flds)))
    ret-val))

(defun elmo-multi-list-folder (spec)
  (let* ((flds (cdr spec))
	 (cur-number 0)
	 (killed (and elmo-use-killed-list
		      (elmo-msgdb-killed-list-load
		       (elmo-msgdb-expand-path nil spec))))
	 numbers)
    (while flds
      (setq cur-number (+ cur-number 1))
      (setq numbers (append
		     numbers
		     (mapcar
		      (function
		       (lambda (x)
			 (+
			  (* elmo-multi-divide-number cur-number) x)))
		      (elmo-list-folder (car flds)))))
      (setq flds (cdr flds)))
    (if killed
	(delq nil
	      (mapcar (lambda (number)
			(unless (memq number killed) number))
		      numbers))
      numbers)))

(defun elmo-multi-folder-exists-p (spec)
  (let* ((flds (cdr spec)))
    (catch 'exists
      (while flds
	(unless (elmo-folder-exists-p (car flds))
	  (throw 'exists nil))
	(setq flds (cdr flds)))
      t)))

(defun elmo-multi-folder-creatable-p (spec)
  (let* ((flds (cdr spec)))
    (catch 'creatable
      (while flds
	(when (and (elmo-call-func (car flds) "folder-creatable-p")
		   (not (elmo-folder-exists-p (car flds))))
	      ;; If folder already exists, don't to `creatable'.
	      ;; Because this function is called, when folder doesn't exists.
	  (throw 'creatable t))
	(setq flds (cdr flds)))
      nil)))

(defun elmo-multi-create-folder (spec)
  (let* ((flds (cdr spec)))
    (catch 'create
      (while flds
	(unless (or (elmo-folder-exists-p (car flds))
		    (elmo-create-folder (car flds)))
	  (throw 'create nil))
	(setq flds (cdr flds)))
      t)))

(defun elmo-multi-search (spec condition &optional numlist)
  (let* ((flds (cdr spec))
	 (cur-number 0)
	 numlist-list cur-numlist ; for filtered search.
	 ret-val)
    (if numlist
	(setq numlist-list
	      (elmo-multi-get-intlist-list numlist t)))
    (while flds
      (setq cur-number (+ cur-number 1))
      (when numlist
	(setq cur-numlist (car numlist-list))
	(if (null cur-numlist)
	    ;; t means filter all.
	    (setq cur-numlist t)))
      (setq ret-val (append
		     ret-val
		     (elmo-list-filter
		      cur-numlist
		      (mapcar
		       (function
			(lambda (x)
			  (+
			   (* elmo-multi-divide-number cur-number) x)))
		       (elmo-call-func
			(car flds) "search" condition)))))
      (when numlist
	(setq numlist-list (cdr numlist-list)))
      (setq flds (cdr flds)))
    ret-val))

(defun elmo-multi-use-cache-p (spec number)
  (elmo-call-func (nth (- (/ number elmo-multi-divide-number) 1)
		       (cdr spec))
		  "use-cache-p"
		  (% number elmo-multi-divide-number)))

(defun elmo-multi-local-file-p (spec number)
  (elmo-call-func (nth (- (/ number elmo-multi-divide-number) 1)
		       (cdr spec))
		  "local-file-p"
		  (% number elmo-multi-divide-number)))

(defun elmo-multi-commit (spec)
  (mapcar 'elmo-commit (cdr spec)))

(defun elmo-multi-plugged-p (spec)
  (let* ((flds (cdr spec)))
    (catch 'plugged
      (while flds
	(unless (elmo-folder-plugged-p (car flds))
	  (throw 'plugged nil))
	(setq flds (cdr flds)))
      t)))

(defun elmo-multi-set-plugged (spec plugged add)
  (let* ((flds (cdr spec)))
    (while flds
      (elmo-folder-set-plugged (car flds) plugged add)
      (setq flds (cdr flds)))))

(defun elmo-multi-get-msg-filename (spec number &optional loc-alist)
  (elmo-call-func (nth (- (/ number elmo-multi-divide-number) 1)
		       (cdr spec))
		  "get-msg-filename"
		  (% number elmo-multi-divide-number)
		  loc-alist))

(defun elmo-multi-sync-number-alist (spec number-alist)
  (let ((folder-list (cdr spec))
	(number-alist-list
	 (elmo-multi-get-number-alist-list number-alist))
	(multi-base 0)
	append-alist result-alist)
    (while folder-list
      (incf multi-base)
      (setq append-alist
	    (elmo-call-func (nth (- multi-base 1) (cdr spec)) ;; folder name
			    "sync-number-alist"
			    (nth (- multi-base 1) number-alist-list)))
      (mapcar
       (function
	(lambda (x)
	  (setcar x
		  (+ (* elmo-multi-divide-number multi-base) (car x)))))
       append-alist)
      (setq result-alist (nconc result-alist append-alist))
      (setq folder-list (cdr folder-list)))
    result-alist))

(provide 'elmo-multi)

;;; elmo-multi.el ends here
