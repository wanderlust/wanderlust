;;; elmo-multi.el -- Multiple Folder Interface for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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

(require 'elmo)
(require 'luna)

(defvar elmo-multi-divide-number 100000
  "*Multi divider number.")

;;; ELMO Multi folder
(eval-and-compile
  (luna-define-class elmo-multi-folder (elmo-folder)
		     (children divide-number))
  (luna-define-internal-accessors 'elmo-multi-folder))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-multi-folder)
					    name)
  (elmo-multi-folder-set-children-internal
   folder
   (mapcar 'elmo-make-folder (split-string name ",")))
  (elmo-multi-folder-set-divide-number-internal
   folder
   elmo-multi-divide-number)
  folder)

(luna-define-method elmo-folder-open-internal ((folder elmo-multi-folder))
  (dolist (fld (elmo-multi-folder-children-internal folder))
    (elmo-folder-open-internal fld)))

(luna-define-method elmo-folder-check ((folder elmo-multi-folder))
  (dolist (fld (elmo-multi-folder-children-internal folder))
    (elmo-folder-check fld)))

(luna-define-method elmo-folder-close-internal ((folder elmo-multi-folder))
  (dolist (fld (elmo-multi-folder-children-internal folder))
    (elmo-folder-close-internal fld)))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-multi-folder))
  (expand-file-name (elmo-replace-string-as-filename 
		     (elmo-folder-name-internal folder))
		    (expand-file-name "multi"
				      elmo-msgdb-dir)))

(luna-define-method elmo-folder-get-primitive-list ((folder elmo-multi-folder))
  (elmo-flatten
   (mapcar 
    'elmo-folder-get-primitive-list
    (elmo-multi-folder-children-internal folder))))

(luna-define-method elmo-folder-contains-type ((folder elmo-multi-folder) type)
  (let ((children (elmo-multi-folder-children-internal folder))
	match)
    (while children
      (when (elmo-folder-contains-type (car children) type)
	(setq match t)
	(setq children nil))
      (setq children (cdr children)))
    match))

(luna-define-method elmo-message-use-cache-p ((folder elmo-multi-folder)
					     number)
  (elmo-message-use-cache-p 
   (nth (- (/ number (elmo-multi-folder-divide-number-internal folder)) 1)
	(elmo-multi-folder-children-internal folder))
   (% number (elmo-multi-folder-divide-number-internal folder))))

(luna-define-method elmo-message-folder ((folder elmo-multi-folder)
					 number)
  (nth (- (/ number (elmo-multi-folder-divide-number-internal folder)) 1)
       (elmo-multi-folder-children-internal folder)))

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

(defun elmo-multi-split-numbers (folder numlist &optional as-is)
  (let ((numbers (sort numlist '<))
	(divider (elmo-multi-folder-divide-number-internal folder))
	(cur-number 0)
	one-list numbers-list)
    (while numbers
      (setq cur-number (+ cur-number 1))
      (setq one-list nil)
      (while (and numbers
		  (eq 0
		      (/ (- (car numbers)
			    (* divider cur-number))
			 divider)))
	(setq one-list (nconc
			one-list
			(list
			 (if as-is
			     (car numbers)
			   (% (car numbers)
			      (* divider cur-number))))))
	(setq numbers (cdr numbers)))
      (setq numbers-list (nconc numbers-list (list one-list))))
    numbers-list))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-multi-folder)
					      numbers new-mark already-mark
					      seen-mark important-mark
					      seen-list)
  (let* ((folders (elmo-multi-folder-children-internal folder))
	 overview number-alist mark-alist entity
	 numbers-list
	 cur-number
	 i percent num
	 msgdb)
    (setq numbers-list (elmo-multi-split-numbers folder numbers))
    (setq cur-number 0)
    (while (< cur-number (length folders))
      (if (nth cur-number numbers-list)
	  (setq msgdb
		(elmo-msgdb-append
		 msgdb
		 (elmo-multi-msgdb
		  (elmo-folder-msgdb-create (nth cur-number folders)
					    (nth cur-number numbers-list)
					    new-mark already-mark
					    seen-mark important-mark
					    seen-list)
		  (* (elmo-multi-folder-divide-number-internal folder)
		     (1+ cur-number))))))
      (setq cur-number (1+ cur-number)))
    (elmo-msgdb-sort-by-date msgdb)))

(luna-define-method elmo-folder-process-crosspost ((folder elmo-multi-folder)
						   &optional
						   number-alist)
  (let ((number-alists (elmo-multi-split-number-alist
			folder
			(elmo-msgdb-get-number-alist
			 (elmo-folder-msgdb-internal folder))))
	(cur-number 1))
    (dolist (child (elmo-multi-folder-children-internal folder))
      (elmo-folder-process-crosspost child (car number-alists))
      (setq cur-number (+ 1 cur-number)
	    number-alists (cdr number-alists)))))

(defsubst elmo-multi-folder-append-msgdb (folder append-msgdb)
  (if append-msgdb
      (let* ((number-alist (elmo-msgdb-get-number-alist append-msgdb))
	     (all-alist (copy-sequence (append
					(elmo-msgdb-get-number-alist
					 (elmo-folder-msgdb-internal folder))
					number-alist)))
	     (cur number-alist)
	     to-be-deleted
	     mark-alist same)
	(while cur
	  (setq all-alist (delq (car cur) all-alist))
	  ;; same message id exists.
	  (if (setq same (rassoc (cdr (car cur)) all-alist))
	      (unless (= (/ (car (car cur))
			    (elmo-multi-folder-divide-number-internal folder))
			 (/ (car same) 
			    (elmo-multi-folder-divide-number-internal folder)))
		;; base is also same...delete it!
		(setq to-be-deleted (append to-be-deleted (list (car cur))))))
	  (setq cur (cdr cur)))
	(setq mark-alist (elmo-delete-if
			  (function
			   (lambda (x)
			     (assq (car x) to-be-deleted)))
			  (elmo-msgdb-get-mark-alist append-msgdb)))
	(elmo-msgdb-set-mark-alist append-msgdb mark-alist)
	(elmo-folder-set-msgdb-internal folder
					(elmo-msgdb-append
					 (elmo-folder-msgdb-internal folder)
					 append-msgdb t))
	(length to-be-deleted))
    0))

(luna-define-method elmo-folder-append-msgdb ((folder elmo-multi-folder)
					      append-msgdb)
  (elmo-multi-folder-append-msgdb folder append-msgdb))

(defmacro elmo-multi-real-folder-number (folder number)
  "Returns a cons cell of real FOLDER and NUMBER."
  (` (cons (nth (- 
		 (/ (, number)
		    (elmo-multi-folder-divide-number-internal (, folder)))
		 1) (elmo-multi-folder-children-internal (, folder)))
	   (% (, number) (elmo-multi-folder-divide-number-internal
			  (, folder))))))

(defsubst elmo-multi-find-fetch-strategy (folder entity ignore-cache)
  (if entity
      (let ((pair (elmo-multi-real-folder-number
		   folder
		   (elmo-msgdb-overview-entity-get-number entity)))
	    (new-entity (elmo-msgdb-copy-overview-entity entity)))
	(setq new-entity
	      (elmo-msgdb-overview-entity-set-number new-entity (cdr pair)))
	(elmo-find-fetch-strategy (car pair) new-entity ignore-cache))
    (elmo-make-fetch-strategy 'entire)))

(luna-define-method elmo-find-fetch-strategy
  ((folder elmo-multi-folder)
   entity &optional ignore-cache)
  (elmo-multi-find-fetch-strategy folder entity ignore-cache))

(luna-define-method elmo-message-fetch ((folder elmo-multi-folder)
					number strategy
					&optional section outbuf unseen)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-fetch (car pair) (cdr pair) strategy section outbuf unseen)))

(luna-define-method elmo-folder-delete-messages ((folder elmo-multi-folder)
						 numbers)
  (let ((flds (elmo-multi-folder-children-internal folder))
	one-list-list
	(cur-number 0))
    (setq one-list-list (elmo-multi-split-numbers folder numbers))
    (while (< cur-number (length flds))
      (elmo-folder-delete-messages (nth cur-number flds)
				   (nth cur-number one-list-list))
      (setq cur-number (+ 1 cur-number)))
    t))

(luna-define-method elmo-folder-diff ((folder elmo-multi-folder)
				      &optional numbers)
  (elmo-multi-folder-diff folder numbers))

(defun elmo-multi-folder-diff (folder numbers)
  (let ((flds (elmo-multi-folder-children-internal folder))
	(numbers (mapcar 'car
			 (elmo-msgdb-number-load
			  (elmo-folder-msgdb-path folder))))
	(killed (elmo-msgdb-killed-list-load (elmo-folder-msgdb-path folder)))
	(count 0)
	(unsync 0)
	(messages 0)
	num-list
	diffs)
    (setq num-list
	  (elmo-multi-split-numbers folder
				    (elmo-uniq-list
				     (nconc
				      (elmo-number-set-to-number-list killed)
				      numbers))))
    (while flds
      (setq diffs (nconc diffs (list (elmo-folder-diff
				      (car flds)
				      (car num-list)))))
      (setq count (+ 1 count))
      (setq num-list (cdr num-list))
      (setq flds (cdr flds)))
    (while diffs
      (and (car (car diffs))
	   (setq unsync (+ unsync (car (car diffs)))))
      (setq messages  (+ messages (cdr (car diffs))))
      (setq diffs (cdr diffs)))
    (elmo-folder-set-info-hashtb folder nil messages)
    (cons unsync messages)))

(defun elmo-multi-split-number-alist (folder number-alist)
  (let ((alist (sort (copy-sequence number-alist)
		     (lambda (pair1 pair2)
		       (< (car pair1)(car pair2)))))
	(cur-number 0)
	one-alist split num)
    (while alist
      (setq cur-number (+ cur-number 1))
      (setq one-alist nil)
      (while (and alist
		  (eq 0
		      (/ (- (setq num (car (car alist)))
			    (* elmo-multi-divide-number cur-number))
			 (elmo-multi-folder-divide-number-internal folder))))
	(setq one-alist (nconc
			 one-alist
			 (list
			  (cons
			   (% num (* (elmo-multi-folder-divide-number-internal
				      folder) cur-number))
			   (cdr (car alist))))))
	(setq alist (cdr alist)))
      (setq split (nconc split (list one-alist))))
    split))

(defun elmo-multi-split-mark-alist (folder mark-alist)
  (let ((cur-number 0)
	(alist (sort (copy-sequence mark-alist)
		     (lambda (pair1 pair2)
		       (< (car pair1)(car pair2)))))
	one-alist result)
    (while alist
      (setq cur-number (+ cur-number 1))
      (setq one-alist nil)
      (while (and alist
		  (eq 0
		      (/ (- (car (car alist))
			    (* (elmo-multi-folder-divide-number-internal
				folder) cur-number))
			 (elmo-multi-folder-divide-number-internal folder))))
	(setq one-alist (nconc
			 one-alist
			 (list
			  (list (% (car (car alist))
				   (* (elmo-multi-folder-divide-number-internal
				       folder) cur-number))
				(cadr (car alist))))))
	(setq alist (cdr alist)))
      (setq result (nconc result (list one-alist))))
    result))

(luna-define-method elmo-folder-list-unreads-internal
  ((folder elmo-multi-folder) unread-marks &optional mark-alist)
  (elmo-multi-folder-list-unreads-internal folder unread-marks))

(defun elmo-multi-folder-list-unreads-internal (folder unread-marks)
  (let ((folders (elmo-multi-folder-children-internal folder))
	(mark-alists (elmo-multi-split-mark-alist
		      folder
		      (elmo-msgdb-get-mark-alist
		       (elmo-folder-msgdb-internal folder))))
	(cur-number 0)
	unreads
	all-unreads)
    (while folders
      (setq cur-number (+ cur-number 1))
      (unless (listp (setq unreads
			   (elmo-folder-list-unreads-internal
			    (car folders) unread-marks (car mark-alists))))
	(setq unreads (delq  nil
			     (mapcar
			      (lambda (x)
				(if (member (cadr x) unread-marks)
				    (car x)))
			      (car mark-alists)))))
      (setq all-unreads
	    (nconc all-unreads
		   (mapcar 
		    (lambda (x)
		      (+ x
			 (* cur-number
			    (elmo-multi-folder-divide-number-internal
			     folder))))		   
		    unreads)))
      (setq mark-alists (cdr mark-alists)
	    folders (cdr folders)))
    all-unreads))

(luna-define-method elmo-folder-list-importants-internal
  ((folder elmo-multi-folder) important-mark)
  (let ((folders (elmo-multi-folder-children-internal folder))
	(mark-alists (elmo-multi-split-mark-alist
		      folder
		      (elmo-msgdb-get-mark-alist
		       (elmo-folder-msgdb-internal folder))))
	(cur-number 0)
	importants
	all-importants)
    (while folders
      (setq cur-number (+ cur-number 1))
      (unless (listp (setq importants
			   (elmo-folder-list-importants-internal
			    (car folders) important-mark)))
	(setq importants (delq  nil
			     (mapcar
			      (lambda (x)
				(if (string= (cadr x) important-mark)
				    (car x)))
			      (car mark-alists)))))
      (setq all-importants
	    (nconc all-importants
		   (mapcar 
		    (lambda (x)
		      (+ x
			 (* cur-number
			    (elmo-multi-folder-divide-number-internal
			     folder))))		   
		    importants)))
      (setq mark-alists (cdr mark-alists)
	    folders (cdr folders)))
    all-importants))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-multi-folder) &optional nohide)
  (let* ((flds (elmo-multi-folder-children-internal folder))
	 (cur-number 0)
	 numbers)
    (while flds
      (setq cur-number (+ cur-number 1))
      (setq numbers (append
		     numbers
		     (mapcar
		      (function
		       (lambda (x)
			 (+
			  (* (elmo-multi-folder-divide-number-internal
			      folder) cur-number) x)))
		      (elmo-folder-list-messages-internal (car flds)))))
      (setq flds (cdr flds)))
    numbers))

(luna-define-method elmo-folder-exists-p ((folder elmo-multi-folder))
  (let ((flds (elmo-multi-folder-children-internal folder)))
    (catch 'exists
      (while flds
	(unless (elmo-folder-exists-p (car flds))
	  (throw 'exists nil))
	(setq flds (cdr flds)))
      t)))

(luna-define-method elmo-folder-creatable-p ((folder elmo-multi-folder))
  (let ((flds (elmo-multi-folder-children-internal folder)))
    (catch 'creatable
      (while flds
	(when (and (elmo-folder-creatable-p (car flds))
		   (not (elmo-folder-exists-p (car flds))))
	  ;; If folder already exists, don't to `creatable'.
	  ;; Because this function is called, when folder doesn't exists.
	  (throw 'creatable t))
	(setq flds (cdr flds)))
      nil)))

(luna-define-method elmo-folder-create ((folder elmo-multi-folder))
  (let ((flds (elmo-multi-folder-children-internal folder)))
    (catch 'create
      (while flds
	(unless (or (elmo-folder-exists-p (car flds))
		    (elmo-folder-create (car flds)))
	  (throw 'create nil))
	(setq flds (cdr flds)))
      t)))

(luna-define-method elmo-folder-search ((folder elmo-multi-folder)
					condition &optional numlist)
  (let* ((flds (elmo-multi-folder-children-internal folder))
	 (cur-number 0)
	 numlist-list cur-numlist ; for filtered search.
	 ret-val)
    (if numlist
	(setq numlist-list
	      (elmo-multi-split-numbers folder numlist t)))
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
			   (* (elmo-multi-folder-divide-number-internal
			       folder) cur-number) x)))
		       (elmo-folder-search
			(car flds) condition)))))
      (when numlist
	(setq numlist-list (cdr numlist-list)))
      (setq flds (cdr flds)))
    ret-val))

(luna-define-method elmo-message-use-cache-p ((folder elmo-multi-folder)
					      number)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-use-cache-p (car pair) (cdr pair))))

(luna-define-method elmo-message-file-p ((folder elmo-multi-folder) number)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-file-p (car pair) (cdr pair))))

(luna-define-method elmo-message-file-name ((folder elmo-multi-folder) number)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-file-name (car pair) (cdr pair))))
  
(luna-define-method elmo-folder-plugged-p ((folder elmo-multi-folder))
  (let ((flds (elmo-multi-folder-children-internal folder)))
    (catch 'plugged
      (while flds
	(unless (elmo-folder-plugged-p (car flds))
	  (throw 'plugged nil))
	(setq flds (cdr flds)))
      t)))

(luna-define-method elmo-folder-set-plugged ((folder elmo-multi-folder)
					     plugged add)
  (let ((flds  (elmo-multi-folder-children-internal folder)))
    (dolist (fld flds)
      (elmo-folder-set-plugged fld plugged add))))

(defun elmo-multi-folder-numbers-list-assoc (folder folder-numbers)
  (let (ent)
    (while folder-numbers
      (when (string= (elmo-folder-name-internal (car (car folder-numbers)))
		     (elmo-folder-name-internal folder))
	(setq ent (car folder-numbers)
	      folder-numbers nil))
      (setq folder-numbers (cdr folder-numbers)))
    ent))

(defun elmo-multi-make-folder-numbers-list (folder msgs)
  (let ((msg-list msgs)
	pair fld-list
	ret-val)
    (while msg-list
      (when (and (numberp (car msg-list))
		 (> (car msg-list) 0))
	(setq pair (elmo-multi-real-folder-number folder (car msg-list)))
	(if (setq fld-list (elmo-multi-folder-numbers-list-assoc
			    (car pair)
			    ret-val))
	    (setcdr fld-list (cons (cdr pair) (cdr fld-list)))
	  (setq ret-val (cons (cons (car pair) (list (cdr pair))) ret-val))))
      (setq msg-list (cdr msg-list)))
    ret-val))

(luna-define-method elmo-folder-mark-as-important ((folder elmo-multi-folder)
						   numbers)
  (dolist (folder-numbers (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-mark-as-important (car folder-numbers)
				   (cdr folder-numbers)))
  t)
  
(luna-define-method elmo-folder-unmark-important ((folder elmo-multi-folder)
						  numbers)
  (dolist (folder-numbers (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-unmark-important (car folder-numbers)
				  (cdr folder-numbers)))
  t)

(luna-define-method elmo-folder-mark-as-read ((folder elmo-multi-folder)
					      numbers)
  (dolist (folder-numbers (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-mark-as-read (car folder-numbers)
			      (cdr folder-numbers)))
  t)

(luna-define-method elmo-folder-unmark-read ((folder elmo-multi-folder)
					     numbers)
  (dolist (folder-numbers (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-unmark-read (car folder-numbers)
			     (cdr folder-numbers)))
  t)

(require 'product)
(product-provide (provide 'elmo-multi) (require 'elmo-version))

;;; elmo-multi.el ends here
