;;; elmo-multi.el --- Multiple Folder Interface for ELMO.

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

(defmacro elmo-multi-real-folder-number (folder number)
  "Returns a cons cell of real FOLDER and NUMBER."
  (` (cons (nth (- 
		 (/ (, number)
		    (elmo-multi-folder-divide-number-internal (, folder)))
		 1) (elmo-multi-folder-children-internal (, folder)))
	   (% (, number) (elmo-multi-folder-divide-number-internal
			  (, folder))))))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-multi-folder)
					    name)
  (while (> (length (car (setq name (elmo-parse-token name ",")))) 0)
    (elmo-multi-folder-set-children-internal
     folder
     (nconc (elmo-multi-folder-children-internal
	     folder)
	    (list (elmo-make-folder (car name)))))
    (setq name (cdr name))
    (when (and (> (length name) 0)
	       (eq (aref name 0) ?,))
      (setq name (substring name 1))))
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

(luna-define-method elmo-folder-close :after ((folder elmo-multi-folder))
  (dolist (fld (elmo-multi-folder-children-internal folder))
    (elmo-folder-set-msgdb-internal fld nil)))

(luna-define-method elmo-folder-synchronize ((folder elmo-multi-folder)
					     &optional ignore-msgdb
					     no-check)
  (dolist (fld (elmo-multi-folder-children-internal folder))
    (elmo-folder-synchronize fld ignore-msgdb no-check))
  0)

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-multi-folder))
  (expand-file-name (elmo-replace-string-as-filename
		     (elmo-folder-name-internal folder))
		    (expand-file-name "multi"
				      elmo-msgdb-directory)))

(luna-define-method elmo-folder-newsgroups ((folder elmo-multi-folder))
  (delq nil
	(elmo-flatten
	 (mapcar
	  'elmo-folder-newsgroups
	  (elmo-flatten
	   (mapcar
	    'elmo-folder-get-primitive-list
	    (elmo-multi-folder-children-internal folder)))))))

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

(luna-define-method elmo-message-folder ((folder elmo-multi-folder)
					 number)
  (nth (- (/ number (elmo-multi-folder-divide-number-internal folder)) 1)
       (elmo-multi-folder-children-internal folder)))

(luna-define-method elmo-message-set-cached ((folder elmo-multi-folder)
					     number cached)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-set-cached (car pair) (cdr pair) cached)))

(luna-define-method elmo-find-fetch-strategy
  ((folder elmo-multi-folder) entity &optional ignore-cache)
  (let ((pair (elmo-multi-real-folder-number
	       folder
	       (elmo-message-entity-number entity))))
    (elmo-find-fetch-strategy
     (car pair)
     (elmo-message-entity (car pair) (cdr pair)) ignore-cache)))

(luna-define-method elmo-message-entity ((folder elmo-multi-folder) key)
  (cond
   ((numberp key)
    (let* ((pair (elmo-multi-real-folder-number folder key))
	   (entity (elmo-message-entity (car pair) (cdr pair))))
      (when entity
	(elmo-message-entity-set-number (elmo-message-copy-entity entity)
					key))))
   ((stringp key)
    (let ((children (elmo-multi-folder-children-internal folder))
	  (cur-number 0)
	  match)
      (while children
	(setq cur-number (+ cur-number 1))
	(when (setq match (elmo-message-entity (car children) key))
	  (setq match (elmo-message-copy-entity match))
	  (elmo-message-entity-set-number
	   match
	   (+ (* (elmo-multi-folder-divide-number-internal folder)
		 cur-number)
	      (elmo-message-entity-number match)))
	  (setq children nil))
	(setq children (cdr children)))
      match))))

(luna-define-method elmo-message-field ((folder elmo-multi-folder)
					number field)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-field (car pair) (cdr pair) field)))

(luna-define-method elmo-message-mark ((folder elmo-multi-folder) number)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-mark (car pair) (cdr pair))))

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

(luna-define-method elmo-folder-process-crosspost ((folder elmo-multi-folder))
  (dolist (child (elmo-multi-folder-children-internal folder))
    (elmo-folder-process-crosspost child)))

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

(luna-define-method elmo-folder-detach-messages ((folder elmo-multi-folder)
						 numbers)
  (let ((flds (elmo-multi-folder-children-internal folder))
	one-list-list
	(cur-number 0))
    (setq one-list-list (elmo-multi-split-numbers folder numbers))
    (while (< cur-number (length flds))
      (elmo-folder-detach-messages (nth cur-number flds)
				   (nth cur-number one-list-list))
      (setq cur-number (+ 1 cur-number)))
    t))

(luna-define-method elmo-folder-diff ((folder elmo-multi-folder)
				      &optional numbers)
  (elmo-multi-folder-diff folder numbers))

(defun elmo-multi-folder-diff (folder numbers)
  (let ((flds (elmo-multi-folder-children-internal folder))
	(num-list (and numbers (elmo-multi-split-numbers folder numbers)))
	(unsync 0)
	(messages 0)
	diffs)
    (while flds
      (setq diffs (nconc diffs (list (elmo-folder-diff (car flds)
						       (car num-list)))))
      (setq flds (cdr flds)))
    (while diffs
      (and (car (car diffs))
	   (setq unsync (+ unsync (car (car diffs)))))
      (setq messages  (+ messages (cdr (car diffs))))
      (setq diffs (cdr diffs)))
    (elmo-folder-set-info-hashtb folder nil messages)
    (cons unsync messages)))

(luna-define-method elmo-folder-list-unreads ((folder elmo-multi-folder))
  (let ((cur-number 0)
	unreads)
    (dolist (child (elmo-multi-folder-children-internal folder))
      (setq cur-number (+ cur-number 1))
      (setq unreads
	    (nconc
	     unreads
	     (mapcar (lambda (x)
		       (+ x (* cur-number
			       (elmo-multi-folder-divide-number-internal
				folder))))
		     (elmo-folder-list-unreads child)))))
    unreads))

(luna-define-method elmo-folder-list-answereds ((folder elmo-multi-folder))
  (let ((cur-number 0)
	answereds)
    (dolist (child (elmo-multi-folder-children-internal folder))
      (setq cur-number (+ cur-number 1))
      (setq answereds
	    (nconc
	     answereds
	     (mapcar (lambda (x)
		       (+ x (* cur-number
			       (elmo-multi-folder-divide-number-internal
				folder))))
		     (elmo-folder-list-answereds child)))))
    answereds))

(luna-define-method elmo-folder-list-importants ((folder elmo-multi-folder))
  (let ((cur-number 0)
	importants)
    (dolist (child (elmo-multi-folder-children-internal folder))
      (setq cur-number (+ cur-number 1))
      (setq importants
	    (nconc
	     importants
	     (mapcar (lambda (x)
		       (+ x (* cur-number
			       (elmo-multi-folder-divide-number-internal
				folder))))
		     (elmo-folder-list-importants child)))))
    (elmo-uniq-list
     (nconc importants
	    (elmo-folder-list-messages-with-global-mark
	     folder elmo-msgdb-important-mark)))))

(luna-define-method elmo-folder-list-messages
  ((folder elmo-multi-folder) &optional visible-only in-msgdb)
  (let* ((flds (elmo-multi-folder-children-internal folder))
	 (cur-number 0)
	 list numbers)
    (while flds
      (setq cur-number (+ cur-number 1))
      (setq list (elmo-folder-list-messages (car flds) visible-only in-msgdb))
      (setq numbers
	    (nconc
	     numbers
	     (mapcar
	      (function
	       (lambda (x)
		 (+
		  (* (elmo-multi-folder-divide-number-internal
		      folder) cur-number) x)))
	      list)))
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

(luna-define-method elmo-folder-mark-as-important ((folder
						    elmo-multi-folder)
						   numbers
						   &optional
						   ignore-flags)
  (dolist (folder-numbers (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-mark-as-important (car folder-numbers)
				   (cdr folder-numbers)
				   ignore-flags)))

(luna-define-method elmo-folder-unmark-important ((folder
						   elmo-multi-folder)
						  numbers
						  &optional
						  ignore-flags)
  (dolist (folder-numbers (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-unmark-important (car folder-numbers)
				  (cdr folder-numbers)
				  ignore-flags)))

(luna-define-method elmo-folder-mark-as-read ((folder
					       elmo-multi-folder)
					      numbers
					      &optional ignore-flag)
  (dolist (folder-numbers (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-mark-as-read (car folder-numbers)
			      (cdr folder-numbers)
			      ignore-flag)))

(luna-define-method elmo-folder-unmark-read ((folder
					      elmo-multi-folder)
					     numbers
					     &optional ignore-flag)
  (dolist (folder-numbers (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-unmark-read (car folder-numbers)
			     (cdr folder-numbers)
			     ignore-flag)))

(luna-define-method elmo-folder-mark-as-answered ((folder
						   elmo-multi-folder)
						  numbers)
  (dolist (folder-numbers (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-mark-as-answered (car folder-numbers)
				  (cdr folder-numbers))))

(luna-define-method elmo-folder-unmark-answered ((folder
						  elmo-multi-folder)
						 numbers)
  (dolist (folder-numbers (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-unmark-answered (car folder-numbers)
				 (cdr folder-numbers))))

(luna-define-method elmo-folder-list-flagged ((folder elmo-multi-folder)
					      flag
					      &optional in-msgdb)
  (let ((cur-number 0)
	numbers)
    (dolist (child (elmo-multi-folder-children-internal folder))
      (setq cur-number (+ cur-number 1)
	    numbers
	    (nconc
	     numbers
	     (mapcar
	      (function
	       (lambda (x)
		 (+
		  (* (elmo-multi-folder-divide-number-internal folder)
		     cur-number) x)))
	      (elmo-folder-list-flagged child flag in-msgdb)))))
    numbers))

(luna-define-method elmo-folder-commit ((folder elmo-multi-folder))
  (dolist (child (elmo-multi-folder-children-internal folder))
    (elmo-folder-commit child)))

(luna-define-method elmo-folder-length ((folder elmo-multi-folder))
  (let ((sum 0))
    (dolist (child (elmo-multi-folder-children-internal folder))
      (setq sum (+ sum (elmo-folder-length child))))
    sum))

(luna-define-method elmo-folder-count-flags ((folder elmo-multi-folder))
  (let ((new 0)
	(unreads 0)
	(answered 0)
	flags)
    (dolist (child (elmo-multi-folder-children-internal folder))
      (setq flags (elmo-folder-count-flags child))
      (setq new (+ new (nth 0 flags)))
      (setq unreads (+ unreads (nth 1 flags)))
      (setq answered (+ answered (nth 2 flags))))
    (list new unreads answered)))

(require 'product)
(product-provide (provide 'elmo-multi) (require 'elmo-version))

;;; elmo-multi.el ends here
