;;; modb-standard.el --- Standartd Implement of MODB.

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

(require 'elmo-util)
(require 'mime)
(require 'modb)

(defcustom modb-standard-divide-number 500
  "*Standard modb divide entity number."
  :type '(choice (const :tag "Not divide" nil)
		 number)
  :group 'elmo)

(defvar modb-standard-entity-filename "entity"
  "Message entity database.")

(defvar modb-standard-flag-filename "flag"
  "Message number <=> Flag status database.")

(defvar modb-standard-msgid-filename "msgid"
  "Message number <=> Message-Id database.")

(eval-and-compile
  (luna-define-class modb-standard (modb-generic)
		     (number-list	; sorted list of message numbers.
		      entity-map	; number, msg-id -> entity mapping.
		      flag-map		; number -> flag-list mapping
		      ))
  (luna-define-internal-accessors 'modb-standard))

;; for internal use only
(defsubst modb-standard-key (number)
  (concat "#" (number-to-string number)))

(defsubst modb-standard-entity-id (entity)
  (if (eq 'autoload (car-safe entity))
      (cddr entity)
    (elmo-msgdb-message-entity-field
     (elmo-message-entity-db entity)
     entity 'message-id)))

(defsubst modb-standard-entity-map (modb)
  (or (modb-standard-entity-map-internal modb)
      (modb-standard-set-entity-map-internal
       modb
       (elmo-make-hash (elmo-msgdb-length modb)))))

(defsubst modb-standard-flag-map (modb)
  (or (modb-standard-flag-map-internal modb)
      (modb-standard-set-flag-map-internal
       modb
       (elmo-make-hash (elmo-msgdb-length modb)))))

(defsubst modb-standard-set-message-modified (modb number)
  (if modb-standard-divide-number
      (let ((section (/ number modb-standard-divide-number))
	    (modified (modb-generic-message-modified-internal modb)))
	(unless (memq section modified)
	  (modb-generic-set-message-modified-internal
	   modb (cons section modified))))
    (modb-generic-set-message-modified-internal modb t)))

(defsubst modb-standard-set-flag-modified (modb number)
  (modb-generic-set-flag-modified-internal modb t))

(defsubst modb-standard-message-flags (modb number)
  (cdr (elmo-get-hash-val (modb-standard-key number)
			  (modb-standard-flag-map-internal modb))))

(defsubst modb-standard-match-flags (check-flags flags)
  (catch 'done
    (while check-flags
      (when (memq (car check-flags) flags)
	(throw 'done t))
      (setq check-flags (cdr check-flags)))))


;; save and load functions
(defun modb-standard-load-msgid (modb path)
  (let* ((alist (elmo-object-load
		 (expand-file-name modb-standard-msgid-filename path)))
	 (table (or (modb-standard-entity-map-internal modb)
		    (elmo-make-hash (length alist))))
	 numbers info)
    (dolist (pair alist)
      (setq info (cons 'autoload pair))
      (elmo-set-hash-val (modb-standard-key (car pair)) info table)
      (elmo-set-hash-val (cdr pair) info table)
      (setq numbers (cons (car pair) numbers)))
    (modb-standard-set-number-list-internal modb (nreverse numbers))
    (modb-standard-set-entity-map-internal modb table)))

(defun modb-standard-save-msgid (modb path)
  (let ((table (modb-standard-entity-map-internal modb))
	entity alist)
    (dolist (number (modb-standard-number-list-internal modb))
      (setq entity (elmo-get-hash-val (modb-standard-key number) table))
      (setq alist (cons (cons number (modb-standard-entity-id entity))
			alist)))
    (elmo-object-save
     (expand-file-name modb-standard-msgid-filename path)
     (nreverse alist))))

(defun modb-standard-load-flag (modb path)
  (let ((table (or (modb-standard-flag-map-internal modb)
		   (elmo-make-hash (elmo-msgdb-length modb)))))
    (dolist (info (elmo-object-load
		   (expand-file-name modb-standard-flag-filename path)))
      (elmo-set-hash-val (modb-standard-key (car info)) info table))
    (modb-standard-set-flag-map-internal modb table)))

(defun modb-standard-save-flag (modb path)
  (let (table flist info)
    (when (setq table (modb-standard-flag-map-internal modb))
      (mapatoms
       (lambda (atom)
	 (setq info (symbol-value atom))
	 (when (cdr info)
	   (setq flist (cons info flist))))
       table))
    (elmo-object-save
     (expand-file-name modb-standard-flag-filename path)
     flist)))

(defsubst modb-standard-entity-filename (section)
  (if section
      (concat modb-standard-entity-filename
	      "-"
	      (number-to-string section))
    modb-standard-entity-filename))

(defun modb-standard-load-entity (modb path &optional section)
  (let ((table (or (modb-standard-entity-map-internal modb)
		   (elmo-make-hash (elmo-msgdb-length modb)))))
    (dolist (entity (elmo-object-load
		     (expand-file-name
		      (modb-standard-entity-filename section)
		      path)))
      (elmo-set-hash-val (modb-standard-key
			  (elmo-msgdb-message-entity-number
			   (elmo-message-entity-db entity)
			   entity))
			 entity
			 table)
      (elmo-set-hash-val (elmo-msgdb-message-entity-field
			  (elmo-message-entity-db entity)
			  entity 'message-id)
			 entity
			 table))
    (modb-standard-set-entity-map-internal modb table)))

(defsubst modb-standard-save-entity-1 (modb path &optional section)
  (let ((table (modb-standard-entity-map-internal modb))
	(filename (expand-file-name
		   (modb-standard-entity-filename section) path))
	entity entities)
    (dolist (number (modb-standard-number-list-internal modb))
      (when (and (or (null section)
		     (= section (/ number modb-standard-divide-number)))
		 (setq entity (elmo-msgdb-message-entity modb number)))
	(setq entities (cons entity entities))))
    (if entities
	(elmo-object-save filename entities)
      (ignore-errors (delete-file filename)))))

(defun modb-standard-save-entity (modb path)
  (let ((sections (modb-generic-message-modified-internal modb)))
    (cond ((listp sections)
	   (dolist (section sections)
	     (modb-standard-save-entity-1 modb path section)))
	  (sections
	   (modb-standard-save-entity-1 modb path)))))

;;; Implement
;;
(luna-define-method elmo-msgdb-load ((msgdb modb-standard))
  (let ((inhibit-quit t)
	(path (elmo-msgdb-location msgdb)))
    (when (file-exists-p (expand-file-name modb-standard-flag-filename path))
      (modb-standard-load-msgid msgdb path)
      (modb-standard-load-flag msgdb path)
      (unless modb-standard-divide-number
	(modb-standard-load-entity msgdb path))
      t)))

(luna-define-method elmo-msgdb-save ((msgdb modb-standard))
  (let ((path (elmo-msgdb-location msgdb)))
    (when (elmo-msgdb-message-modified-p msgdb)
      (modb-standard-save-msgid  msgdb path)
      (modb-standard-save-entity msgdb path)
      (modb-generic-set-message-modified-internal msgdb nil))
    (when (elmo-msgdb-flag-modified-p msgdb)
      (modb-standard-save-flag msgdb path)
      (modb-generic-set-flag-modified-internal msgdb nil))))

(luna-define-method elmo-msgdb-append :around ((msgdb modb-standard)
					       msgdb-append)
  (when (> (elmo-msgdb-length msgdb-append) 0)
    (if (eq (luna-class-name msgdb-append) 'modb-standard)
	(let ((numbers (modb-standard-number-list-internal msgdb-append))
	      duplicates)
	  ;; number-list
	  (modb-standard-set-number-list-internal
	   msgdb
	   (nconc (modb-standard-number-list-internal msgdb)
		  numbers))
	  ;; entity-map
	  (let ((table (modb-standard-entity-map msgdb))
		entity msg-id)
	    (dolist (number numbers)
	      (setq entity (elmo-msgdb-message-entity msgdb-append number)
		    msg-id (modb-standard-entity-id entity))
	      (if (elmo-get-hash-val msg-id table)
		  (setq duplicates (cons number duplicates))
		(elmo-set-hash-val msg-id entity table))
	      (elmo-set-hash-val (modb-standard-key number)
				 entity
				 table)))
	  ;; flag-map
	  (let ((table (modb-standard-flag-map msgdb)))
	    (mapatoms
	     (lambda (atom)
	       (elmo-set-hash-val (symbol-name atom)
				  (symbol-value atom)
				  table))
	     (modb-standard-flag-map msgdb-append)))
	  ;; modification flags
	  (dolist (number (modb-standard-number-list-internal msgdb-append))
	    (modb-standard-set-message-modified msgdb number)
	    (modb-standard-set-flag-modified msgdb number))
	  duplicates)
      (luna-call-next-method))))

(luna-define-method elmo-msgdb-clear :after ((msgdb modb-standard))
  (modb-standard-set-number-list-internal msgdb nil)
  (modb-standard-set-entity-map-internal msgdb nil)
  (modb-standard-set-flag-map-internal msgdb nil))

(luna-define-method elmo-msgdb-length ((msgdb modb-standard))
  (length (modb-standard-number-list-internal msgdb)))

(luna-define-method elmo-msgdb-flags ((msgdb modb-standard) number)
  (modb-standard-message-flags msgdb number))

(luna-define-method elmo-msgdb-set-flag ((msgdb modb-standard)
					 number flag)
  (case flag
    (read
     (elmo-msgdb-unset-flag msgdb number 'unread))
    (uncached
     (elmo-msgdb-unset-flag msgdb number 'cached))
    (t
     (let* ((cur-flags (modb-standard-message-flags msgdb number))
	    (new-flags (copy-sequence cur-flags)))
       (and (memq 'new new-flags)
	    (setq new-flags (delq 'new new-flags)))
       (or (memq flag new-flags)
	   (setq new-flags (cons flag new-flags)))
       (when (and (eq flag 'unread)
		  (memq 'answered new-flags))
	 (setq new-flags (delq 'answered new-flags)))
       (unless (equal new-flags cur-flags)
	 (elmo-set-hash-val (modb-standard-key number)
			    (cons number new-flags)
			    (modb-standard-flag-map msgdb))
	 (modb-standard-set-flag-modified msgdb number))))))

(luna-define-method elmo-msgdb-unset-flag ((msgdb modb-standard)
					   number flag)
  (case flag
    (read
     (elmo-msgdb-set-flag msgdb number 'unread))
    (uncached
     (elmo-msgdb-set-flag msgdb number 'cached))
    (t
     (let* ((cur-flags (modb-standard-message-flags msgdb number))
	    (new-flags (copy-sequence cur-flags)))
       (and (memq 'new new-flags)
	    (setq new-flags (delq 'new new-flags)))
       (and (memq flag new-flags)
	    (setq new-flags (delq flag new-flags)))
       (when (and (eq flag 'unread)
		  (memq 'answered new-flags))
	 (setq new-flags (delq 'answered new-flags)))
       (unless (equal new-flags cur-flags)
	 (elmo-set-hash-val (modb-standard-key number)
			    (cons number new-flags)
			    (modb-standard-flag-map msgdb))
	 (modb-standard-set-flag-modified msgdb number))))))

(luna-define-method elmo-msgdb-list-messages ((msgdb modb-standard))
  (copy-sequence
   (modb-standard-number-list-internal msgdb)))

(luna-define-method elmo-msgdb-list-flagged ((msgdb modb-standard) flag)
  (let (entry matched)
    (case flag
      (read
       (dolist (number (modb-standard-number-list-internal msgdb))
	 (unless (memq 'unread (modb-standard-message-flags msgdb number))
	   (setq matched (cons number matched)))))
      (digest
       (mapatoms
	(lambda (atom)
	  (setq entry (symbol-value atom))
	  (when (modb-standard-match-flags '(unread important)
					   (cdr entry))
	    (setq matched (cons (car entry) matched))))
	(modb-standard-flag-map msgdb)))
      (any
       (mapatoms
	(lambda (atom)
	  (setq entry (symbol-value atom))
	  (when (modb-standard-match-flags '(unread important answered)
					   (cdr entry))
	    (setq matched (cons (car entry) matched))))
	(modb-standard-flag-map msgdb)))
      (t
       (mapatoms
	(lambda (atom)
	  (setq entry (symbol-value atom))
	  (when (memq flag (cdr entry))
	    (setq matched (cons (car entry) matched))))
	(modb-standard-flag-map msgdb))))
    matched))

(luna-define-method elmo-msgdb-append-entity ((msgdb modb-standard)
					      entity &optional flags)
  (let ((number (elmo-msgdb-message-entity-number
		 (elmo-message-entity-db entity) entity))
	(msg-id (elmo-msgdb-message-entity-field
		 (elmo-message-entity-db entity) entity 'message-id))
	duplicate)
    ;; number-list
    (modb-standard-set-number-list-internal
     msgdb
     (nconc (modb-standard-number-list-internal msgdb)
	    (list number)))
    ;; entity-map
    (let ((table (modb-standard-entity-map msgdb)))
      (setq duplicate (elmo-get-hash-val msg-id table))
      (elmo-set-hash-val (modb-standard-key number) entity table)
      (elmo-set-hash-val msg-id entity table))
    ;; modification flags
    (modb-standard-set-message-modified msgdb number)
    ;; flag-map
    (when flags
      (elmo-set-hash-val
       (modb-standard-key number)
       (cons number flags)
       (modb-standard-flag-map msgdb))
      (modb-standard-set-flag-modified msgdb number))
    duplicate))

(luna-define-method elmo-msgdb-delete-messages ((msgdb modb-standard)
						numbers)
  (let ((number-list (modb-standard-number-list-internal msgdb))
	(entity-map (modb-standard-entity-map-internal msgdb))
	(flag-map (modb-standard-flag-map-internal msgdb))
	key entity)
    (dolist (number numbers)
      (setq key (modb-standard-key number)
	    entity (elmo-get-hash-val key entity-map))
      ;; number-list
      (setq number-list (delq number number-list))
      ;; entity-map
      (elmo-clear-hash-val key entity-map)
      (elmo-clear-hash-val (modb-standard-entity-id entity) entity-map)
      ;; flag-map
      (elmo-clear-hash-val key flag-map)
      (modb-standard-set-message-modified msgdb number)
      (modb-standard-set-flag-modified msgdb number))
    (modb-standard-set-number-list-internal msgdb number-list)
    (modb-standard-set-entity-map-internal msgdb entity-map)
    (modb-standard-set-flag-map-internal msgdb flag-map)))

(luna-define-method elmo-msgdb-sort-entities ((msgdb modb-standard)
					      predicate &optional app-data)
  (message "Sorting...")
  (let ((numbers (modb-standard-number-list-internal msgdb)))
    (modb-standard-set-number-list-internal
     msgdb
     (sort numbers (lambda (a b)
		     (funcall predicate
			      (elmo-msgdb-message-entity msgdb a)
			      (elmo-msgdb-message-entity msgdb b)
			      app-data))))
    (message "Sorting...done")
    msgdb))

(luna-define-method elmo-msgdb-message-entity ((msgdb modb-standard) key)
  (let ((ret (elmo-get-hash-val
	      (cond ((stringp key) key)
		    ((numberp key) (modb-standard-key key)))
	      (modb-standard-entity-map-internal msgdb))))
    (if (eq 'autoload (car-safe ret))
	(when modb-standard-divide-number
	  (modb-standard-load-entity
	   msgdb
	   (elmo-msgdb-location msgdb)
	   (/ (nth 1 ret) modb-standard-divide-number))
	  (elmo-get-hash-val
	   (cond ((stringp key) key)
		 ((numberp key) (modb-standard-key key)))
	   (modb-standard-entity-map-internal msgdb)))
      ret)))

;;; Message entity handling.
(defsubst modb-standard-make-message-entity (args)
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

(luna-define-method elmo-msgdb-make-message-entity ((msgdb modb-standard)
						    args)
  (modb-standard-make-message-entity args))

(luna-define-method elmo-msgdb-create-message-entity-from-buffer
  ((msgdb modb-standard) number args)
  (let ((extras elmo-msgdb-extra-fields)
	(default-mime-charset default-mime-charset)
	entity message-id references from subject to cc date
	extra field-body charset size)
    (save-excursion
      (setq entity (modb-standard-make-message-entity args)
	    ;; For compatibility.
	    msgdb (elmo-message-entity-db entity))
      (elmo-set-buffer-multibyte default-enable-multibyte-characters)
      (setq message-id (elmo-msgdb-get-message-id-from-buffer))
      (and (setq charset (cdr (assoc "charset" (mime-read-Content-Type))))
	   (setq charset (intern-soft charset))
	   (setq default-mime-charset charset))
      (setq references
	    (or (elmo-msgdb-get-last-message-id
		 (elmo-field-body "in-reply-to"))
		(elmo-msgdb-get-last-message-id
		 (elmo-field-body "references")))
	    from (elmo-replace-in-string
		  (elmo-mime-string (or (elmo-field-body "from")
					elmo-no-from))
		  "\t" " ")
	    subject (elmo-replace-in-string
		     (elmo-mime-string (or (elmo-field-body "subject")
					   elmo-no-subject))
		     "\t" " ")
	    date (elmo-field-body "date")
	    to   (mapconcat 'identity (elmo-multiple-field-body "to") ",")
	    cc   (mapconcat 'identity (elmo-multiple-field-body "cc") ","))
      (unless (elmo-msgdb-message-entity-field msgdb entity 'size)
	(if (setq size (elmo-field-body "content-length"))
	    (setq size (string-to-int size))
	  (setq size 0)))
      (while extras
	(if (setq field-body (elmo-field-body (car extras)))
	    (elmo-msgdb-message-entity-set-field
	     msgdb entity (intern (downcase (car extras))) field-body))
	(setq extras (cdr extras)))
      (dolist (field '(number message-id references from subject
			      date to cc size))
	(when (symbol-value field)
	  (elmo-msgdb-message-entity-set-field
	   msgdb entity field (symbol-value field))))
      entity)))

;;; Message entity interface
;;
(luna-define-method elmo-msgdb-message-entity-number ((msgdb modb-standard)
						      entity)
  ;; To be implemented.
  )

(luna-define-method elmo-msgdb-message-entity-set-number ((msgdb modb-standard)
							  entity
							  number)
  ;; To be implemented.
  )

(luna-define-method elmo-msgdb-message-entity-field ((msgdb modb-standard)
						     entity field
						     &optional decode)
  ;; To be implemented.
  )

(luna-define-method elmo-msgdb-message-entity-set-field ((msgdb modb-standard)
							 entity field value)
  ;; To be implemented.
  )

(luna-define-method elmo-msgdb-copy-message-entity ((msgdb modb-standard)
						    entity)
  ;; To be implemented.
  )

(luna-define-method elmo-msgdb-match-condition-internal ((msgdb modb-standard)
							 condition
							 entity flags numbers)
  ;; To be implemented.
  )

(require 'product)
(product-provide (provide 'modb-standard) (require 'elmo-version))

;;; modb-standard.el ends here
