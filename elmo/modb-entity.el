;;; modb-entity.el --- Message Entity Interface.

;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
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
;; Message entity handling.

;;; Code:

(eval-when-compile (require 'cl))

(require 'luna)
(require 'elmo-vars)
(require 'elmo-util)

(eval-and-compile (luna-define-class modb-entity-handler))

(defcustom modb-entity-default-handler 'modb-legacy-entity-handler
  "Default entity handler."
  :type 'symbol
  :group 'elmo)

(defcustom elmo-msgdb-prefer-in-reply-to-for-parent nil
  "*Non-nil to prefer In-Reply-To header for finding parent message on thread,
rather than References header."
  :type 'boolean
  :group 'elmo)

(defvar modb-entity-default-cache-internal nil)

(defun elmo-message-entity-handler (&optional entity)
  "Get modb entity handler instance which corresponds to the ENTITY."
  (if (and entity
	   (car-safe entity)
	   (not (eq (car entity) t))
	   (not (stringp (car entity))))
      (car entity)
    (or modb-entity-default-cache-internal
	(setq modb-entity-default-cache-internal
	      (luna-make-entity modb-entity-default-handler)))))

(luna-define-generic elmo-msgdb-make-message-entity (handler &rest args)
  "Make a message entity using HANDLER.")

(luna-define-generic elmo-msgdb-message-entity-number (handler entity)
  "Number of the ENTITY.")

(luna-define-generic elmo-msgdb-message-entity-set-number (handler
							   entity number)
  "Set number of the ENTITY.")

(luna-define-generic elmo-msgdb-message-entity-field (handler
						      entity field
						      &optional decode)
  "Retrieve field value of the message entity.
HANDLER is the message entity handler.
ENTITY is the message entity structure.
FIELD is a symbol of the field.
If optional DECODE is no-nil, the field value is decoded.")

(luna-define-generic elmo-msgdb-message-entity-set-field (handler
							  entity field value)
  "Set the field value of the message entity.
HANDLER is the message entity handler.
ENTITY is the message entity structure.
FIELD is a symbol of the field.
VALUE is the field value to set.")

(luna-define-generic elmo-msgdb-message-entity-update-fields (handler
							      entity values)
  "Update message entity by VALUES.
HANDLER is the message entity handler.
ENTITY is the message entity structure.
VALUES is an alist of field-name and field-value.")

(luna-define-generic elmo-msgdb-copy-message-entity (handler entity)
  "Copy message entity.
HANDLER is the message entity handler.
ENTITY is the message entity structure.")

(luna-define-generic elmo-msgdb-create-message-entity-from-file (handler
								 number
								 file)
  "Create message entity from file.
HANDLER is the message entity handler.
NUMBER is the number of the newly created message entity.
FILE is the message file.")

(luna-define-generic elmo-msgdb-create-message-entity-from-buffer (handler
								   number
								   &rest args)
  "Create message entity from current buffer.
HANDLER is the message entity handler.
NUMBER is the number of the newly created message entity.
Rest of the ARGS is a plist of message entity field for initial value.
Header region is supposed to be narrowed.")

;; Transitional interface.
(luna-define-generic elmo-msgdb-message-match-condition (handler
							 condition
							 entity
							 flags
							 numbers)
  "Return non-nil when the entity matches the condition.")

;; Generic implementation.
(luna-define-method elmo-msgdb-create-message-entity-from-file
  ((handler modb-entity-handler) number file)
  (let (insert-file-contents-pre-hook   ; To avoid autoconv-xmas...
	insert-file-contents-post-hook header-end
	(attrib (file-attributes file))
	ret-val size mtime)
    (with-temp-buffer
      (if (not (file-exists-p file))
	  ()
	(setq size (nth 7 attrib))
	(setq mtime (timezone-make-date-arpa-standard
		     (current-time-string (nth 5 attrib)) (current-time-zone)))
	;; insert header from file.
	(catch 'done
	  (condition-case nil
	      (elmo-msgdb-insert-file-header file)
	    (error (throw 'done nil)))
	  (goto-char (point-min))
	  (setq header-end
		(if (re-search-forward "\\(^--.*$\\)\\|\\(\n\n\\)" nil t)
		    (point)
		  (point-max)))
	  (narrow-to-region (point-min) header-end)
	  (elmo-msgdb-create-message-entity-from-buffer
	   handler number :size size :date mtime))))))

(luna-define-method elmo-msgdb-make-message-entity ((handler
						     modb-entity-handler)
						    args)
  (cons handler args))

(luna-define-method elmo-msgdb-message-entity-field ((handler
						     modb-entity-handler)
						     entity field
						     &optional decode)
  (plist-get (cdr entity) (intern (concat ":" (symbol-name field)))))

(luna-define-method elmo-msgdb-message-entity-number ((handler
						       modb-entity-handler)
						      entity)
  (plist-get (cdr entity) :number))

(luna-define-method elmo-msgdb-message-entity-update-fields
  ((handler modb-entity-handler) entity values)
  (let (updated)
    (dolist (pair values)
      (unless (equal
	       (cdr pair)
	       (elmo-msgdb-message-entity-field handler entity (car pair)))
	(elmo-msgdb-message-entity-set-field handler entity
					     (car pair) (cdr pair))
	(setq updated t)))
    updated))

;; Legacy implementation.
(eval-and-compile (luna-define-class modb-legacy-entity-handler
				     (modb-entity-handler)))

;;
;; mime decode cache
;;
(defvar elmo-msgdb-decoded-cache-hashtb nil)
(make-variable-buffer-local 'elmo-msgdb-decoded-cache-hashtb)

(defsubst elmo-msgdb-get-decoded-cache (string)
  (if elmo-use-decoded-cache
      (let ((hashtb (or elmo-msgdb-decoded-cache-hashtb
			(setq elmo-msgdb-decoded-cache-hashtb
			      (elmo-make-hash 2048))))
	    decoded)
	(or (elmo-get-hash-val string hashtb)
	    (prog1
		(setq decoded
		      (elmo-with-enable-multibyte
			(decode-mime-charset-string string elmo-mime-charset)))
	      (elmo-set-hash-val string decoded hashtb))))
    (elmo-with-enable-multibyte
      (decode-mime-charset-string string elmo-mime-charset))))

(defsubst modb-legacy-make-message-entity (args)
  "Make an message entity."
  (cons (plist-get args :message-id)
	(vector (plist-get args :number)
		(plist-get args :references)
		(plist-get args :from)
		(plist-get args :subject)
		(plist-get args :date)
		(plist-get args :to)
		(plist-get args :cc)
		(plist-get args :size)
		(plist-get args :extra))))

(luna-define-method elmo-msgdb-make-message-entity
  ((handler modb-legacy-entity-handler) args)
  (modb-legacy-make-message-entity args))

(luna-define-method elmo-msgdb-create-message-entity-from-buffer
  ((handler modb-legacy-entity-handler) number args)
  (let ((extras elmo-msgdb-extra-fields)
	(default-mime-charset default-mime-charset)
	entity message-id references from subject to cc date
	extra field-body charset size)
    (save-excursion
      (setq entity (modb-legacy-make-message-entity args))
      (set-buffer-multibyte default-enable-multibyte-characters)
      (setq message-id (elmo-msgdb-get-message-id-from-buffer))
      (and (setq charset (cdr (assoc "charset" (mime-read-Content-Type))))
	   (setq charset (intern-soft charset))
	   (setq default-mime-charset charset))
      (setq references
	    (if elmo-msgdb-prefer-in-reply-to-for-parent
		(or (elmo-msgdb-get-last-message-id
		     (elmo-field-body "in-reply-to"))
		    (elmo-msgdb-get-last-message-id
		     (elmo-field-body "references")))
	      (or (elmo-msgdb-get-last-message-id
		   (elmo-field-body "references"))
		  (elmo-msgdb-get-last-message-id
		   (elmo-field-body "in-reply-to"))))
	    from (elmo-replace-in-string
		  (elmo-mime-string (or (elmo-field-body "from")
					elmo-no-from))
		  "\t" " ")
	    subject (elmo-replace-in-string
		     (elmo-mime-string (or (elmo-field-body "subject")
					   elmo-no-subject))
		     "\t" " ")
	    date (elmo-unfold-field-body "date")
	    to   (mapconcat 'identity (elmo-multiple-field-body "to") ",")
	    cc   (mapconcat 'identity (elmo-multiple-field-body "cc") ","))
      (unless (elmo-msgdb-message-entity-field handler entity 'size)
	(if (setq size (elmo-field-body "content-length"))
	    (setq size (string-to-int size))
	  (setq size 0)))
      (while extras
	(if (setq field-body (elmo-field-body (car extras)))
	    (elmo-msgdb-message-entity-set-field
	     handler entity (intern (downcase (car extras))) field-body))
	(setq extras (cdr extras)))
      (dolist (field '(message-id number references from subject
				  date to cc size))
	(when (symbol-value field)
	  (elmo-msgdb-message-entity-set-field
	   handler entity field (symbol-value field))))
      entity)))

(luna-define-method elmo-msgdb-message-entity-number
  ((handler modb-legacy-entity-handler) entity)
  (and entity (aref (cdr entity) 0)))

(luna-define-method elmo-msgdb-message-entity-set-number
  ((handler modb-legacy-entity-handler) entity number)
  (and entity (aset (cdr entity) 0 number))
  entity)

(luna-define-method elmo-msgdb-message-entity-field
  ((handler modb-legacy-entity-handler) entity field &optional decode)
  (and entity
       (let ((field-value
	      (case field
		(to (aref (cdr entity) 5))
		(cc (aref (cdr entity) 6))
		(date (aref (cdr entity) 4))
		(subject (aref (cdr entity) 3))
		(from (aref (cdr entity) 2))
		(message-id (car entity))
		(references (aref (cdr entity) 1))
		(size (aref (cdr entity) 7))
		(t (cdr (assoc (symbol-name field) (aref (cdr entity) 8)))))))
	 (if (and decode (memq field '(from subject)))
	     (elmo-msgdb-get-decoded-cache field-value)
	   field-value))))

(luna-define-method elmo-msgdb-message-entity-set-field
  ((handler modb-legacy-entity-handler) entity field value)
  (and entity
       (case field
	 (number (aset (cdr entity) 0 value))
	 (to (aset (cdr entity) 5 value))
	 (cc (aset (cdr entity) 6 value))
	 (date (aset (cdr entity) 4 value))
	 (subject (aset (cdr entity) 3 value))
	 (from (aset (cdr entity) 2 value))
	 (message-id (setcar entity value))
	 (references (aset (cdr entity) 1 value))
	 (size (aset (cdr entity) 7 value))
	 (t
	  (let ((extras (and entity (aref (cdr entity) 8)))
		extra)
	    (if (setq extra (assoc (symbol-name field) extras))
		(setcdr extra value)
	      (aset (cdr entity) 8 (cons (cons (symbol-name field)
					       value) extras))))))))

(luna-define-method elmo-msgdb-copy-message-entity
  ((handler modb-legacy-entity-handler) entity)
  (cons (car entity)
	(copy-sequence (cdr entity))))

(luna-define-method elmo-msgdb-message-match-condition
  ((handler modb-legacy-entity-handler) condition entity flags numbers)
  (cond
   ((vectorp condition)
    (elmo-msgdb-match-condition-primitive handler condition
					  entity flags numbers))
   ((eq (car condition) 'and)
    (let ((lhs (elmo-msgdb-message-match-condition handler
						   (nth 1 condition)
						   entity flags numbers)))
      (cond
       ((elmo-filter-condition-p lhs)
	(let ((rhs (elmo-msgdb-message-match-condition
		    handler (nth 2 condition) entity flags numbers)))
	  (cond ((elmo-filter-condition-p rhs)
		 (list 'and lhs rhs))
		(rhs
		 lhs))))
       (lhs
	(elmo-msgdb-message-match-condition handler (nth 2 condition)
					    entity flags numbers)))))
   ((eq (car condition) 'or)
    (let ((lhs (elmo-msgdb-message-match-condition handler (nth 1 condition)
						   entity flags numbers)))
      (cond
       ((elmo-filter-condition-p lhs)
	(let ((rhs (elmo-msgdb-message-match-condition handler
						       (nth 2 condition)
						       entity flags numbers)))
	  (cond ((elmo-filter-condition-p rhs)
		 (list 'or lhs rhs))
		(rhs
		 t)
		(t
		 lhs))))
       (lhs
	t)
       (t
	(elmo-msgdb-message-match-condition handler
					     (nth 2 condition)
					     entity flags numbers)))))))

;;
(defun elmo-msgdb-match-condition-primitive (handler
					     condition
					     entity
					     flags
					     numbers)
  (catch 'unresolved
    (let ((key (elmo-filter-key condition))
	  (case-fold-search t)
	  result)
      (cond
       ((string= key "last")
	(setq result (<= (length (memq
				  (elmo-msgdb-message-entity-number
				   handler entity)
				  numbers))
			 (string-to-int (elmo-filter-value condition)))))
       ((string= key "first")
	(setq result (< (-
			 (length numbers)
			 (length (memq
				  (elmo-msgdb-message-entity-number
				   handler entity)
				  numbers)))
			(string-to-int (elmo-filter-value condition)))))
       ((string= key "flag")
	(setq result
	      (cond
	       ((string= (elmo-filter-value condition) "any")
		(or (memq 'important flags)
		    (memq 'answered flags)
		    (memq 'unread flags)))
	       ((string= (elmo-filter-value condition) "digest")
		(or (memq 'important flags)
		    (memq 'unread flags)))
	       ((string= (elmo-filter-value condition) "unread")
		(memq 'unread flags))
	       ((string= (elmo-filter-value condition) "important")
		(memq 'important flags))
	       ((string= (elmo-filter-value condition) "answered")
		(memq 'answered flags)))))
       ((string= key "from")
	(setq result (string-match
		      (elmo-filter-value condition)
		      (elmo-msgdb-message-entity-field
		       handler entity 'from t))))
       ((string= key "subject")
	(setq result (string-match
		      (elmo-filter-value condition)
		      (elmo-msgdb-message-entity-field
		       handler entity 'subject t))))
       ((string= key "to")
	(setq result (string-match
		      (elmo-filter-value condition)
		      (elmo-msgdb-message-entity-field
		       handler entity 'to))))
       ((string= key "cc")
	(setq result (string-match
		      (elmo-filter-value condition)
		      (elmo-msgdb-message-entity-field
		       handler entity 'cc))))
       ((or (string= key "since")
	    (string= key "before"))
	(let ((field-date (elmo-date-make-sortable-string
			   (timezone-fix-time
			    (elmo-msgdb-message-entity-field
			     handler entity 'date)
			    (current-time-zone) nil)))
	      (specified-date
	       (elmo-date-make-sortable-string
		(elmo-date-get-datevec
		 (elmo-filter-value condition)))))
	  (setq result (if (string= key "since")
			   (or (string= specified-date field-date)
			       (string< specified-date field-date))
			 (string< field-date specified-date)))))
       ((member key elmo-msgdb-extra-fields)
	(let ((extval (elmo-msgdb-message-entity-field handler
						       entity
						       (intern key))))
	  (when (stringp extval)
	    (setq result (string-match
			  (elmo-filter-value condition)
			  extval)))))
       (t
	(throw 'unresolved condition)))
      (if (eq (elmo-filter-type condition) 'unmatch)
	  (not result)
	result))))

(require 'product)
(product-provide (provide 'modb-entity) (require 'elmo-version))

;;; modb-entity.el ends here
