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

;;; Code:
;;

(eval-when-compile (require 'cl))

(require 'elmo-vars)
(require 'elmo-util)
(require 'mime)

;;
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
	    (progn
	      (elmo-set-hash-val
	       string
	       (setq decoded
		     (decode-mime-charset-string string elmo-mime-charset))
	       hashtb)
	      decoded)))
    (decode-mime-charset-string string elmo-mime-charset)))


;;; Message entity interface
;;
(defun elmo-msgdb-make-message-entity (&rest args)
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

(defsubst elmo-msgdb-message-entity-field (entity field &optional decode)
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

(defsubst elmo-msgdb-message-entity-set-field (entity field value)
  (and entity
       (case field
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
	    (if (setq extra (assoc field extras))
		(setcdr extra value)
	      (aset (cdr entity) 8 (cons (cons (symbol-name field)
					       value) extras))))))))

(defun elmo-msgdb-copy-overview-entity (entity)
  (cons (car entity)
	(copy-sequence (cdr entity))))

;;; obsolete interface
;;
(defsubst elmo-msgdb-overview-entity-get-id (entity)
  (and entity (car entity)))

(defsubst elmo-msgdb-overview-entity-get-number (entity)
  (and entity (aref (cdr entity) 0)))

(defsubst elmo-msgdb-overview-entity-set-number (entity number)
  (and entity (aset (cdr entity) 0 number))
  entity)

(defsubst elmo-msgdb-overview-entity-get-references (entity)
  (and entity (aref (cdr entity) 1)))

(defsubst elmo-msgdb-overview-entity-set-references (entity references)
  (and entity (aset (cdr entity) 1 references))
  entity)

(defsubst elmo-msgdb-overview-entity-get-from-no-decode (entity)
  (and entity (aref (cdr entity) 2)))

(defsubst elmo-msgdb-overview-entity-get-from (entity)
  (and entity
       (aref (cdr entity) 2)
       (elmo-msgdb-get-decoded-cache (aref (cdr entity) 2))))

(defsubst elmo-msgdb-overview-entity-set-from (entity from)
  (and entity (aset (cdr entity) 2 from))
  entity)

(defsubst elmo-msgdb-overview-entity-get-subject (entity)
  (and entity
       (aref (cdr entity) 3)
       (elmo-msgdb-get-decoded-cache (aref (cdr entity) 3))))

(defsubst elmo-msgdb-overview-entity-get-subject-no-decode (entity)
  (and entity (aref (cdr entity) 3)))

(defsubst elmo-msgdb-overview-entity-set-subject (entity subject)
  (and entity (aset (cdr entity) 3 subject))
  entity)

(defsubst elmo-msgdb-overview-entity-get-date (entity)
  (and entity (aref (cdr entity) 4)))

(defsubst elmo-msgdb-overview-entity-set-date (entity date)
  (and entity (aset (cdr entity) 4 date))
  entity)

(defsubst elmo-msgdb-overview-entity-get-to (entity)
  (and entity (aref (cdr entity) 5)))

(defsubst elmo-msgdb-overview-entity-get-cc (entity)
  (and entity (aref (cdr entity) 6)))

(defsubst elmo-msgdb-overview-entity-get-size (entity)
  (and entity (aref (cdr entity) 7)))

(defsubst elmo-msgdb-overview-entity-set-size (entity size)
  (and entity (aset (cdr entity) 7 size))
  entity)

(defsubst elmo-msgdb-overview-entity-get-extra (entity)
  (and entity (aref (cdr entity) 8)))

(defsubst elmo-msgdb-overview-entity-set-extra (entity extra)
  (and entity (aset (cdr entity) 8 extra))
  entity)

(defsubst elmo-msgdb-overview-entity-get-extra-field (entity field-name)
  (let ((field-name (downcase field-name))
	(extra (and entity (aref (cdr entity) 8))))
    (and extra
	 (cdr (assoc field-name extra)))))

(defsubst elmo-msgdb-overview-entity-set-extra-field (entity field-name value)
  (let ((field-name (downcase field-name))
	(extras (and entity (aref (cdr entity) 8)))
	extra)
    (if (setq extra (assoc field-name extras))
	(setcdr extra value)
      (elmo-msgdb-overview-entity-set-extra
       entity
       (cons (cons field-name value) extras)))))


;;;
;;
(defun elmo-msgdb-match-condition-primitive (condition entity flags numbers)
  (catch 'unresolved
    (let ((key (elmo-filter-key condition))
	  (case-fold-search t)
	  result)
      (cond
       ((string= key "last")
	(setq result (<= (length (memq
				  (elmo-msgdb-overview-entity-get-number
				   entity)
				  numbers))
			 (string-to-int (elmo-filter-value condition)))))
       ((string= key "first")
	(setq result (< (-
			 (length numbers)
			 (length (memq
				  (elmo-msgdb-overview-entity-get-number
				   entity)
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
		      (elmo-msgdb-overview-entity-get-from entity))))
       ((string= key "subject")
	(setq result (string-match
		      (elmo-filter-value condition)
		      (elmo-msgdb-overview-entity-get-subject entity))))
       ((string= key "to")
	(setq result (string-match
		      (elmo-filter-value condition)
		      (elmo-msgdb-overview-entity-get-to entity))))
       ((string= key "cc")
	(setq result (string-match
		      (elmo-filter-value condition)
		      (elmo-msgdb-overview-entity-get-cc entity))))
       ((or (string= key "since")
	    (string= key "before"))
	(let ((field-date (elmo-date-make-sortable-string
			   (timezone-fix-time
			    (elmo-msgdb-overview-entity-get-date entity)
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
	(let ((extval (elmo-msgdb-overview-entity-get-extra-field entity key)))
	  (when (stringp extval)
	    (setq result (string-match
			  (elmo-filter-value condition)
			  extval)))))
       (t
	(throw 'unresolved condition)))
      (if (eq (elmo-filter-type condition) 'unmatch)
	  (not result)
	result))))

(defun elmo-msgdb-match-condition-internal (condition entity flags numbers)
  (cond
   ((vectorp condition)
    (elmo-msgdb-match-condition-primitive condition entity flags numbers))
   ((eq (car condition) 'and)
    (let ((lhs (elmo-msgdb-match-condition-internal (nth 1 condition)
						    entity flags numbers)))
      (cond
       ((elmo-filter-condition-p lhs)
	(let ((rhs (elmo-msgdb-match-condition-internal
		    (nth 2 condition) entity flags numbers)))
	  (cond ((elmo-filter-condition-p rhs)
		 (list 'and lhs rhs))
		(rhs
		 lhs))))
       (lhs
	(elmo-msgdb-match-condition-internal (nth 2 condition)
					     entity flags numbers)))))
   ((eq (car condition) 'or)
    (let ((lhs (elmo-msgdb-match-condition-internal (nth 1 condition)
						    entity flags numbers)))
      (cond
       ((elmo-filter-condition-p lhs)
	(let ((rhs (elmo-msgdb-match-condition-internal (nth 2 condition)
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
	(elmo-msgdb-match-condition-internal (nth 2 condition)
					     entity flags numbers)))))))


(require 'product)
(product-provide (provide 'modb-entity) (require 'elmo-version))

;;; modb-entity.el ends here
