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

(eval-and-compile
  (luna-define-class modb-entity-handler () (mime-charset))
  (luna-define-internal-accessors 'modb-entity-handler))

(defcustom modb-entity-default-handler 'modb-legacy-entity-handler
  "Default entity handler."
  :type 'symbol
  :group 'elmo)

(defcustom modb-entity-field-extractor-alist
  '((ml-info . modb-entity-extract-mailing-list-info))
  "*An alist of field name and function to extract field body from buffer."
  :type '(repeat (cons (symbol :tag "Field Name")
		       (function :tag "Function")))
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

(luna-define-generic modb-entity-handler-list-parameters (handler)
  "Return a parameter list of HANDLER.")

(luna-define-generic elmo-msgdb-make-message-entity (handler &rest args)
  "Make a message entity using HANDLER.")

(luna-define-generic elmo-msgdb-message-entity-number (handler entity)
  "Number of the ENTITY.")

(luna-define-generic elmo-msgdb-message-entity-set-number (handler
							   entity number)
  "Set number of the ENTITY.")

(luna-define-generic elmo-msgdb-message-entity-field (handler entity field
							      &optional type)
  "Retrieve field value of the message entity.
HANDLER is the message entity handler.
ENTITY is the message entity structure.
FIELD is a symbol of the field.
If optional argument TYPE is specified, return converted value.")

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

(luna-define-generic elmo-msgdb-copy-message-entity (handler entity
							     &optional
							     make-handler)
  "Copy message entity.
HANDLER is the message entity handler.
ENTITY is the message entity structure.
If optional argument MAKE-HANDLER is specified, use it to make new entity.")

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
(luna-define-method modb-entity-handler-list-parameters
  ((handler modb-entity-handler))
  (list 'mime-charset))

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
						     &optional type)
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

(defun modb-entity-handler-equal-p (handler other)
  "Return non-nil, if OTHER hanlder is equal this HANDLER."
  (and (eq (luna-class-name handler)
	   (luna-class-name other))
       (catch 'mismatch
	 (dolist (slot (modb-entity-handler-list-parameters handler))
	   (when (not (equal (luna-slot-value handler slot)
			     (luna-slot-value other slot)))
	     (throw 'mismatch nil)))
	 t)))

(defun modb-entity-handler-dump-parameters (handler)
  "Return parameters for reconstruct HANDLER as plist."
  (apply #'nconc
	 (mapcar (lambda (slot)
		   (let ((value (luna-slot-value handler slot)))
		     (when value
		       (list (intern (concat ":" (symbol-name slot)))
			     value))))
	 (modb-entity-handler-list-parameters handler))))

;; field in/out converter
(defun modb-set-field-converter (converter type &rest specs)
  "Set convert function of TYPE into CONVERTER.
SPECS must be like `FIELD1 FUNCTION1 FIELD2 FUNCTION2 ...'.
If each field is t, function is set as default converter."
  (when specs
    (let ((alist (symbol-value converter))
	  (type (or type t)))
      (while specs
	(let ((field (pop specs))
	      (function (pop specs))
	      cell)
	  (if (setq cell (assq type alist))
	      (setcdr cell (put-alist field function (cdr cell)))
	    (setq cell  (cons type (list (cons field function)))
		  alist (cons cell alist)))
	  ;; support colon keyword (syntax sugar).
	  (unless (or (eq field t)
		      (string-match "^:" (symbol-name field)))
	    (setcdr cell (put-alist (intern (concat ":" (symbol-name field)))
				    function
				    (cdr cell))))))
      (set converter alist))))
(put 'modb-set-field-converter 'lisp-indent-function 2)

(defsubst modb-convert-field-value (converter field value &optional type)
  (and value
       (let* ((alist (cdr (assq (or type t) converter)))
	      (function (cdr (or (assq field alist)
				 (assq t alist)))))
	 (if function
	     (funcall function field value)
	   value))))

;; mime decode cache
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

(defun modb-entity-string-decoder (field value)
  (elmo-msgdb-get-decoded-cache value))

(defun modb-entity-string-encoder (field value)
  (elmo-with-enable-multibyte
    (encode-mime-charset-string value elmo-mime-charset)))

(defun modb-entity-parse-date-string (field value)
  (if (stringp value)
      (elmo-time-parse-date-string value)
    value))

(defun modb-entity-make-date-string (field value)
  (if (stringp value)
      value
    (elmo-time-make-date-string value)))

(defun modb-entity-mime-decoder (field value)
  (mime-decode-field-body value (symbol-name field) 'summary))

(defun modb-entity-mime-encoder (field value)
  (mime-encode-field-body value (symbol-name field)))

(defun modb-entity-address-list-decoder (field value)
  (if (stringp value)
      (mapcar (lambda (address)
		(mime-decode-field-body address (symbol-name field)))
	      (elmo-parse-addresses value))
    value))

(defun modb-entity-address-list-encoder (field value)
  (if (stringp value)
      value
    (mime-encode-field-body (mapconcat 'identity value ", ")
			    (symbol-name field))))

(defun modb-entity-parse-address-string (field value)
  (modb-entity-encode-string-recursive
   field
   (if (stringp value)
       (elmo-parse-addresses value)
     value)))

(defun modb-entity-make-address-string (field value)
  (let ((value (modb-entity-decode-string-recursive field value)))
    (if (stringp value)
	value
      (mapconcat 'identity value ", "))))

(defun modb-entity-decode-string-recursive (field value)
  (cond ((stringp value)
	 (elmo-msgdb-get-decoded-cache value))
	((consp value)
	 (setcar value (modb-entity-decode-string-recursive field (car value)))
	 (setcdr value (modb-entity-decode-string-recursive field (cdr value)))
	 value)
	(t
	 value)))

(defun modb-entity-encode-string-recursive (field value)
  (cond ((stringp value)
	 (elmo-with-enable-multibyte
	   (encode-mime-charset-string value elmo-mime-charset)))
	((consp value)
	 (setcar value (modb-entity-encode-string-recursive field (car value)))
	 (setcdr value (modb-entity-encode-string-recursive field (cdr value)))
	 value)
	(t
	 value)))


(defun modb-entity-create-field-indices (slots)
  (let ((index 0)
	indices)
    (while slots
      (setq indices (cons (cons (car slots) index) indices)
	    index   (1+ index)
	    slots   (cdr slots)))
    (append
     indices
     (mapcar (lambda (cell)
	       (cons (intern (concat ":" (symbol-name (car cell))))
		     (cdr cell)))
	     indices))))


;; Legacy implementation.
(eval-and-compile
  (luna-define-class modb-legacy-entity-handler (modb-entity-handler)))

(defconst modb-legacy-entity-field-slots
 '(number
   references
   from
   subject
   date
   to
   cc
   size
   extra))

(defconst modb-legacy-entity-field-indices
  (modb-entity-create-field-indices modb-legacy-entity-field-slots))

(defvar modb-legacy-entity-normalizer nil)
(modb-set-field-converter 'modb-legacy-entity-normalizer nil
  'message-id	nil
  'number	nil
  'references	nil
  'from		#'modb-entity-string-encoder
  'subject	#'modb-entity-string-encoder
  'date		#'modb-entity-make-date-string
  'to		#'modb-entity-address-list-encoder
  'cc		#'modb-entity-address-list-encoder
  'size		nil
  t		#'modb-entity-mime-encoder)

(defvar modb-legacy-entity-specializer nil)
;; default type
(modb-set-field-converter 'modb-legacy-entity-specializer nil
  'message-id	nil
  'number	nil
  'references	nil
  'from		#'modb-entity-string-decoder
  'subject	#'modb-entity-string-decoder
  'date		#'modb-entity-parse-date-string
  'to		#'modb-entity-address-list-decoder
  'cc		#'modb-entity-address-list-decoder
  'size		nil
  t		#'modb-entity-mime-decoder)
;; string type
(modb-set-field-converter 'modb-legacy-entity-specializer 'string
  'message-id	nil
  'number	nil			; not supported
  'references	nil
  'from		#'modb-entity-string-decoder
  'subject	#'modb-entity-string-decoder
  'date		nil
  'size		nil			; not supported
  t		#'modb-entity-mime-decoder)


(defmacro modb-legacy-entity-field-index (field)
  `(cdr (assq ,field modb-legacy-entity-field-indices)))

(defsubst modb-legacy-entity-set-field (entity field value &optional as-is)
  (when entity
    (let (index)
      (unless as-is
	(setq value (modb-convert-field-value
		     modb-legacy-entity-normalizer
		     field value)))
      (cond ((memq field '(message-id :message-id))
	     (setcar entity value))
	    ((setq index (modb-legacy-entity-field-index field))
	     (aset (cdr entity) index value))
	    (t
	     (setq index (modb-legacy-entity-field-index :extra))
	     (let ((extras (and entity (aref (cdr entity) index)))
		   extra)
	       (if (setq extra (assoc (symbol-name field) extras))
		   (setcdr extra value)
		 (aset (cdr entity) index (cons (cons (symbol-name field)
						      value) extras)))))))))

(defsubst modb-legacy-make-message-entity (args)
  "Make an message entity."
  (let ((entity (cons nil (make-vector 9 nil)))
	field value)
    (while args
      (setq field (pop args)
	    value (pop args))
      (when value
	(modb-legacy-entity-set-field entity field value)))
    entity))

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
	    (elmo-msgdb-get-references-from-buffer)
	    from (elmo-replace-in-string
		  (elmo-mime-string (or (elmo-field-body "from")
					elmo-no-from))
		  "\t" " ")
	    subject (elmo-replace-in-string
		     (elmo-mime-string (or (elmo-field-body "subject")
					   elmo-no-subject))
		     "\t" " ")
	    date (elmo-decoded-field-body "date")
	    to   (mapconcat 'identity (elmo-multiple-field-body "to") ",")
	    cc   (mapconcat 'identity (elmo-multiple-field-body "cc") ","))
      (unless (elmo-msgdb-message-entity-field handler entity 'size)
	(if (setq size (elmo-field-body "content-length"))
	    (setq size (string-to-int size))
	  (setq size 0)))
      (while extras
	(if (setq field-body (elmo-field-body (car extras)))
	    (modb-legacy-entity-set-field
	     entity (intern (downcase (car extras))) field-body 'as-is))
	(setq extras (cdr extras)))
      (dolist (field '(message-id number references from subject
				  date to cc size))
	(when (symbol-value field)
	  (modb-legacy-entity-set-field
	   entity field (symbol-value field) 'as-is)))
      entity)))

(luna-define-method elmo-msgdb-message-entity-number
  ((handler modb-legacy-entity-handler) entity)
  (and entity (aref (cdr entity) 0)))

(luna-define-method elmo-msgdb-message-entity-set-number
  ((handler modb-legacy-entity-handler) entity number)
  (and entity (aset (cdr entity) 0 number)))

(luna-define-method elmo-msgdb-message-entity-field
  ((handler modb-legacy-entity-handler) entity field &optional type)
  (and entity
       (let (index)
	 (modb-convert-field-value
	  modb-legacy-entity-specializer
	  field
	  (cond ((memq field '(message-id :message-id))
		 (car entity))
		((setq index (modb-legacy-entity-field-index field))
		 (aref (cdr entity) index))
		(t
		 (setq index (modb-legacy-entity-field-index :extra))
		 (cdr (assoc (symbol-name field)
			     (aref (cdr entity) index)))))
	  type))))

(luna-define-method elmo-msgdb-message-entity-set-field
  ((handler modb-legacy-entity-handler) entity field value)
  (modb-legacy-entity-set-field entity field value))

(luna-define-method elmo-msgdb-copy-message-entity
  ((handler modb-legacy-entity-handler) entity &optional make-handler)
  (if make-handler
      (let ((copy (elmo-msgdb-make-message-entity make-handler)))
	(dolist (field (append '(message-id number references from subject
					    date to cc size)
			       (mapcar (lambda (extra) (intern (car extra)))
				       (aref (cdr entity) 8))))
	  (elmo-msgdb-message-entity-set-field
	   make-handler copy field
	   (elmo-msgdb-message-entity-field handler entity field)))
	copy)
    (cons (car entity)
	  (copy-sequence (cdr entity)))))

(luna-define-method elmo-msgdb-message-match-condition
  ((handler modb-entity-handler) condition entity flags numbers)
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
		       handler entity 'from))))
       ((string= key "subject")
	(setq result (string-match
		      (elmo-filter-value condition)
		      (elmo-msgdb-message-entity-field
		       handler entity 'subject))))
       ((string= key "to")
	(setq result (string-match
		      (elmo-filter-value condition)
		      (elmo-msgdb-message-entity-field
		       handler entity 'to 'string))))
       ((string= key "cc")
	(setq result (string-match
		      (elmo-filter-value condition)
		      (elmo-msgdb-message-entity-field
		       handler entity 'cc 'string))))
       ((or (string= key "since")
	    (string= key "before"))
	(let ((field-date (elmo-msgdb-message-entity-field
			   handler entity 'date))
	      (specified-date
	       (elmo-datevec-to-time
		(elmo-date-get-datevec
		 (elmo-filter-value condition)))))
	  (setq result (if (string= key "since")
			   (not (elmo-time< field-date specified-date))
			 (elmo-time< field-date specified-date)))))
       ((member key elmo-msgdb-extra-fields)
	(let ((extval (elmo-msgdb-message-entity-field handler
						       entity
						       (intern key)
						       'string)))
	  (when (stringp extval)
	    (setq result (string-match
			  (elmo-filter-value condition)
			  extval)))))
       (t
	(throw 'unresolved condition)))
      (if (eq (elmo-filter-type condition) 'unmatch)
	  (not result)
	result))))


;; Standard implementation.
(eval-and-compile
  (luna-define-class modb-standard-entity-handler (modb-entity-handler)))

(defconst modb-standard-entity-field-slots
  '(number
    from
    subject
    date
    to
    cc
    content-type
    references
    size
    score
    extra))

(defconst modb-standard-entity-field-indices
  (modb-entity-create-field-indices modb-standard-entity-field-slots))

(defvar modb-standard-entity-normalizer nil)
(modb-set-field-converter 'modb-standard-entity-normalizer nil
  'messgae-id	nil
  'number	nil
  'date		#'modb-entity-parse-date-string
  'to		#'modb-entity-parse-address-string
  'cc		#'modb-entity-parse-address-string
  'references	nil
  'size		nil
  'score	nil
  t		#'modb-entity-encode-string-recursive)

(defvar modb-standard-entity-specializer nil)
(modb-set-field-converter 'modb-standard-entity-specializer nil
  'messgae-id	nil
  'number	nil
  'date		nil
  'references	nil
  'size		nil
  'score	nil
  t		#'modb-entity-decode-string-recursive)
(modb-set-field-converter 'modb-standard-entity-specializer 'string
  'messgae-id	nil
  'number	nil
  'date		#'modb-entity-make-date-string
  'to		#'modb-entity-make-address-string
  'cc		#'modb-entity-make-address-string
  'references	nil
  'size		nil
  'score	nil
  'ml-info	#'modb-entity-make-mailing-list-info-string
  t		#'modb-entity-decode-string-recursive)

(defmacro modb-standard-entity-field-index (field)
  `(cdr (assq ,field modb-standard-entity-field-indices)))

(defsubst modb-standard-entity-set-field (entity field value &optional as-is)
  (when entity
    (let (index)
      (unless as-is
	(let ((elmo-mime-charset
	       (or (modb-entity-handler-mime-charset-internal (car entity))
		   elmo-mime-charset)))
	  (setq value (modb-convert-field-value modb-standard-entity-normalizer
						field value))))
      (cond ((memq field '(message-id :message-id))
	     (setcar (cdr entity) value))
	    ((setq index (modb-standard-entity-field-index field))
	     (aset (cdr (cdr entity)) index value))
	    (t
	     (setq index (modb-standard-entity-field-index :extra))
	     (let ((extras (aref (cdr (cdr entity)) index))
		   cell)
	       (if (setq cell (assq field extras))
		   (setcdr cell value)
		 (aset (cdr (cdr entity))
		       index
		       (cons (cons field value) extras)))))))))

(defsubst modb-standard-make-message-entity (handler args)
  (let ((entity (cons handler
		      (cons nil
			    (make-vector
			     (length modb-standard-entity-field-slots)
			     nil))))
	field value)
    (while args
      (setq field (pop args)
	    value (pop args))
      (when value
	(modb-standard-entity-set-field entity field value)))
    entity))

(luna-define-method elmo-msgdb-make-message-entity
  ((handler modb-standard-entity-handler) args)
  (modb-standard-make-message-entity handler args))

(luna-define-method elmo-msgdb-message-entity-number
  ((handler modb-standard-entity-handler) entity)
  (and entity (aref (cdr (cdr entity)) 0)))

(luna-define-method elmo-msgdb-message-entity-set-number
  ((handler modb-standard-entity-handler) entity number)
  (and entity (aset (cdr (cdr entity)) 0 number)))

(luna-define-method elmo-msgdb-message-entity-field
  ((handler modb-standard-entity-handler) entity field &optional type)
  (and entity
       (let ((elmo-mime-charset
	      (or (modb-entity-handler-mime-charset-internal handler)
		  elmo-mime-charset))
	     index)
	 (modb-convert-field-value
	  modb-standard-entity-specializer
	  field
	  (cond ((memq field '(message-id :message-id))
		 (car (cdr entity)))
		((setq index (modb-standard-entity-field-index field))
		 (aref (cdr (cdr entity)) index))
		(t
		 (setq index (modb-standard-entity-field-index :extra))
		 (cdr (assq field (aref (cdr (cdr entity)) index)))))
	  type))))

(luna-define-method elmo-msgdb-message-entity-set-field
  ((handler modb-standard-entity-handler) entity field value)
  (modb-standard-entity-set-field entity field value))

(luna-define-method elmo-msgdb-copy-message-entity
  ((handler modb-standard-entity-handler) entity &optional make-handler)
  (if make-handler
      (let ((copy (elmo-msgdb-make-message-entity make-handler)))
	(dolist (field (nconc
			(delq 'extra
			      (copy-sequence modb-standard-entity-field-slots))
			(mapcar 'car
				(aref
				 (cdr (cdr entity))
				 (modb-standard-entity-field-index :extra)))
			'(message-id)))
	  (elmo-msgdb-message-entity-set-field
	   make-handler copy field
	   (elmo-msgdb-message-entity-field handler entity field)))
	copy)
    (cons handler
	  (cons (car (cdr entity))
		(copy-sequence (cdr (cdr entity)))))))

(luna-define-method elmo-msgdb-create-message-entity-from-buffer
  ((handler modb-standard-entity-handler) number args)
  (let ((default-mime-charset default-mime-charset)
	entity content-type charset)
    (save-excursion
      (set-buffer-multibyte default-enable-multibyte-characters)
      (and (setq content-type (elmo-decoded-field-body
			       "content-type" 'summary))
	   (setq charset (mime-content-type-parameter
			  (mime-parse-Content-Type content-type) "charset"))
	   (setq charset (intern-soft charset))
	   (mime-charset-p charset)
	   (setq default-mime-charset charset))
      (setq entity
	    (modb-standard-make-message-entity
	     handler
	     (append
	      args
	      (list
	       :number
	       number
	       :message-id
	       (elmo-msgdb-get-message-id-from-buffer)
	       :references
	       (elmo-msgdb-get-references-from-buffer)
	       :from
	       (elmo-replace-in-string
		(or (elmo-decoded-field-body "from" 'summary)
		    elmo-no-from)
		"\t" " ")
	       :subject
	       (elmo-replace-in-string
		(or (elmo-decoded-field-body "subject" 'summary)
		    elmo-no-subject)
		"\t" " ")
	       :date
	       (elmo-decoded-field-body "date" 'summary)
	       :to
	       (mapconcat
		(lambda (field-body)
		  (mime-decode-field-body field-body "to" 'summary))
		(elmo-multiple-field-body "to") ",")
	       :cc
	       (mapconcat
		(lambda (field-body)
		  (mime-decode-field-body field-body "cc" 'summary))
		(elmo-multiple-field-body "cc") ",")
	       :content-type
	       content-type
	       :size
	       (let ((size (elmo-field-body "content-length")))
		 (if size
		     (string-to-int size)
		   (or (plist-get args :size) 0)))))))
      (let (field-name field-body extractor)
	(dolist (extra (cons "newsgroups" elmo-msgdb-extra-fields))
	  (setq field-name (intern (downcase extra))
		extractor  (cdr (assq field-name
				      modb-entity-field-extractor-alist))
		field-body (if extractor
			       (funcall extractor field-name)
			     (elmo-decoded-field-body extra 'summary)))
	  (when field-body
	    (modb-standard-entity-set-field entity field-name field-body))))
      entity)))


;; mailing list info handling
(defun modb-entity-extract-ml-info-from-x-sequence ()
  (let ((sequence (elmo-decoded-field-body "x-sequence" 'summary))
	name count)
    (when sequence
      (elmo-set-list '(name count) (split-string sequence " "))
      (cons name count))))

(defun modb-entity-extract-ml-info-from-subject ()
  (let ((subject (elmo-decoded-field-body "subject" 'summary)))
    (when (and subject
	       (string-match "^\\s(\\(\\S)+\\)[ :]\\([0-9]+\\)\\s)[ \t]*"
			     subject))
      (cons (match-string 1 subject) (match-string 2 subject)))))

(defun modb-entity-extract-ml-info-from-return-path ()
  (let ((return-path (elmo-decoded-field-body "return-path" 'summary)))
    (when (and return-path
	       (string-match "^<\\([^@>]+\\)-return-\\([0-9]+\\)-"
			     return-path))
      (cons (match-string 1 return-path)
	    (match-string 2 return-path)))))

(defun modb-entity-extract-ml-info-from-delivered-to ()
  (let ((delivered-to (elmo-decoded-field-body "delivered-to" 'summary)))
    (when (and delivered-to
	       (string-match "^mailing list \\([^@]+\\)@" delivered-to))
      (cons (match-string 1 delivered-to) nil))))

(defun modb-entity-extract-ml-info-from-mailing-list ()
  (let ((mailing-list (elmo-decoded-field-body "mailing-list" 'summary)))
    ;; *-help@, *-owner@, etc.
    (when (and mailing-list
	       (string-match "\\(^\\|; \\)contact \\([^@]+\\)-[^-@]+@"
			     mailing-list))
      (cons (match-string 2 mailing-list) nil))))

(defvar modb-entity-extract-mailing-list-info-functions
  '(modb-entity-extract-ml-info-from-x-sequence
    modb-entity-extract-ml-info-from-subject
    modb-entity-extract-ml-info-from-return-path
    modb-entity-extract-ml-info-from-delivered-to
    modb-entity-extract-ml-info-from-mailing-list))

(defun modb-entity-extract-mailing-list-info (field)
  (let ((ml-name (elmo-decoded-field-body "x-ml-name" 'summary))
	(ml-count (or (elmo-decoded-field-body "x-mail-count" 'summary)
		      (elmo-decoded-field-body "x-ml-count" 'summary)))
	(functions modb-entity-extract-mailing-list-info-functions)
	result)
    (while (and functions
		(or (null ml-name) (null ml-count)))
      (when (setq result (funcall (car functions)))
	(unless ml-name
	  (setq ml-name (car result)))
	(unless ml-count
	  (setq ml-count (cdr result))))
      (setq functions (cdr functions)))
    (when (or ml-name ml-count)
      (cons (and ml-name (car (split-string ml-name " ")))
	    (and ml-count (string-to-int ml-count))))))

(defun modb-entity-make-mailing-list-info-string (field value)
  (when (car value)
    (format (if (cdr value) "(%s %05.0f)" "(%s)")
	    (elmo-msgdb-get-decoded-cache (car value))
	    (cdr value))))

(require 'product)
(product-provide (provide 'modb-entity) (require 'elmo-version))

;;; modb-entity.el ends here
