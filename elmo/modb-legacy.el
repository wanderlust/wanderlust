;;; modb-legacy.el --- Legacy Implement of MODB.

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
(require 'modb)

;;; legacy implement
;;

(defconst modb-legacy-new-mark "N"
  "Mark for new message.")

(defconst modb-legacy-unread-uncached-mark "U"
  "Mark for unread and uncached message.")

(defconst modb-legacy-unread-cached-mark "!"
  "Mark for unread but already cached message.")

(defconst modb-legacy-read-uncached-mark "u"
  "Mark for read but uncached message.")

(defconst modb-legacy-answered-cached-mark "&"
  "Mark for answered and cached message.")

(defconst modb-legacy-answered-uncached-mark "A"
  "Mark for answered but cached message.")

(defconst modb-legacy-important-mark "$"
  "Mark for important message.")

(eval-and-compile
  (luna-define-class modb-legacy (modb-generic)
		     (overview number-alist mark-alist index))
  (luna-define-internal-accessors 'modb-legacy))

;; for internal use only
(defsubst elmo-msgdb-get-overview (msgdb)
  (modb-legacy-overview-internal msgdb))

(defsubst elmo-msgdb-get-number-alist (msgdb)
  (modb-legacy-number-alist-internal msgdb))

(defsubst elmo-msgdb-get-mark-alist (msgdb)
  (modb-legacy-mark-alist-internal msgdb))

(defsubst elmo-msgdb-get-index (msgdb)
  (modb-legacy-index-internal msgdb))

(defsubst elmo-msgdb-get-entity-hashtb (msgdb)
  (car (modb-legacy-index-internal msgdb)))

(defsubst elmo-msgdb-get-mark-hashtb (msgdb)
  (cdr (modb-legacy-index-internal msgdb)))

(defsubst elmo-msgdb-get-path (msgdb)
  (elmo-msgdb-location msgdb))

(defsubst elmo-msgdb-set-overview (msgdb overview)
  (modb-legacy-set-overview-internal msgdb overview))

(defsubst elmo-msgdb-set-number-alist (msgdb number-alist)
  (modb-legacy-set-number-alist-internal msgdb number-alist))

(defsubst elmo-msgdb-set-mark-alist (msgdb mark-alist)
  (modb-legacy-set-mark-alist-internal msgdb mark-alist))

(defsubst elmo-msgdb-set-index (msgdb index)
  (modb-legacy-set-index-internal msgdb index))

(defsubst elmo-msgdb-set-path (msgdb path)
  (modb-generic-set-location-internal msgdb path))

;;;
;; Internal use only (obsolete interface)
;;
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
	    (progn
	      (elmo-set-hash-val
	       string
	       (setq decoded
		     (decode-mime-charset-string string elmo-mime-charset))
	       hashtb)
	      decoded)))
    (decode-mime-charset-string string elmo-mime-charset)))

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

;;; load & save
(defun elmo-msgdb-number-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-number-filename dir)))

(defun elmo-msgdb-overview-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-overview-filename dir)))

(defun elmo-msgdb-mark-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-mark-filename dir)))

(defun elmo-msgdb-number-save (dir obj)
  (elmo-object-save
   (expand-file-name elmo-msgdb-number-filename dir)
   obj))

(defun elmo-msgdb-mark-save (dir obj)
  (elmo-object-save
   (expand-file-name elmo-msgdb-mark-filename dir)
   obj))

(defsubst elmo-msgdb-overview-save (dir overview)
  (elmo-object-save
   (expand-file-name elmo-msgdb-overview-filename dir)
   overview))

;;;


(defvar modb-legacy-unread-marks-internal nil)
(defsubst modb-legacy-unread-marks ()
  "Return an unread mark list"
  (or modb-legacy-unread-marks-internal
      (setq modb-legacy-unread-marks-internal
	    (list modb-legacy-new-mark
		  modb-legacy-unread-uncached-mark
		  modb-legacy-unread-cached-mark))))

(defvar modb-legacy-answered-marks-internal nil)
(defsubst modb-legacy-answered-marks ()
  "Return an answered mark list"
  (or modb-legacy-answered-marks-internal
      (setq modb-legacy-answered-marks-internal
	    (list modb-legacy-answered-cached-mark
		  modb-legacy-answered-uncached-mark))))

(defvar modb-legacy-uncached-marks-internal nil)
(defsubst modb-legacy-uncached-marks ()
  (or modb-legacy-uncached-marks-internal
      (setq modb-legacy-uncached-marks-internal
	    (list modb-legacy-new-mark
		  modb-legacy-answered-uncached-mark
		  modb-legacy-unread-uncached-mark
		  modb-legacy-read-uncached-mark))))

(defsubst modb-legacy-mark-to-flags (mark)
  (append
   (and (string= mark modb-legacy-new-mark)
	'(new))
   (and (string= mark modb-legacy-important-mark)
	'(important))
   (and (member mark (modb-legacy-unread-marks))
	'(unread))
   (and (member mark (modb-legacy-answered-marks))
	'(answered))
   (and (not (member mark (modb-legacy-uncached-marks)))
	'(cached))))

(defsubst modb-legacy-flags-to-mark (flags)
  (cond ((memq 'new flags)
	 modb-legacy-new-mark)
	((memq 'important flags)
	 modb-legacy-important-mark)
	((memq 'answered flags)
	 (if (memq 'cached flags)
	     modb-legacy-answered-cached-mark
	   modb-legacy-answered-uncached-mark))
	((memq 'unread flags)
	 (if (memq 'cached flags)
	     modb-legacy-unread-cached-mark
	   modb-legacy-unread-uncached-mark))
	(t
	 (if (memq 'cached flags)
	     nil
	   modb-legacy-read-uncached-mark))))

(defsubst elmo-msgdb-get-mark (msgdb number)
  "Get mark string from MSGDB which corresponds to the message with NUMBER."
  (cadr (elmo-get-hash-val (format "#%d" number)
			   (elmo-msgdb-get-mark-hashtb msgdb))))

(defsubst elmo-msgdb-set-mark (msgdb number mark)
  "Set MARK of the message with NUMBER in the MSGDB.
if MARK is nil, mark is removed."
  (let ((elem (elmo-get-hash-val (format "#%d" number)
				 (elmo-msgdb-get-mark-hashtb msgdb))))
    (if elem
	(if mark
	    ;; Set mark of the elem
	    (setcar (cdr elem) mark)
	  ;; Delete elem from mark-alist
	  (elmo-msgdb-set-mark-alist
	   msgdb
	   (delq elem (elmo-msgdb-get-mark-alist msgdb)))
	  (elmo-clear-hash-val (format "#%d" number)
			       (elmo-msgdb-get-mark-hashtb msgdb)))
      (when mark
	;; Append new element.
	(elmo-msgdb-set-mark-alist
	 msgdb
	 (nconc
	  (elmo-msgdb-get-mark-alist msgdb)
	  (list (setq elem (list number mark)))))
	(elmo-set-hash-val (format "#%d" number) elem
			   (elmo-msgdb-get-mark-hashtb msgdb))))
    (modb-generic-set-flag-modified-internal msgdb t)
    ;; return value.
    t))

(defun elmo-msgdb-make-index (msgdb &optional overview mark-alist)
  "Append OVERVIEW and MARK-ALIST to the index of MSGDB.
If OVERVIEW and MARK-ALIST are nil, make index for current MSGDB.
Return a list of message numbers which have duplicated message-ids."
  (when msgdb
    (let* ((overview (or overview (elmo-msgdb-get-overview msgdb)))
	   (mark-alist (or mark-alist (elmo-msgdb-get-mark-alist msgdb)))
	   (index (elmo-msgdb-get-index msgdb))
	   (ehash (or (car index) ;; append
		      (elmo-make-hash (length overview))))
	   (mhash (or (cdr index) ;; append
		      (elmo-make-hash (length overview))))
	   duplicates)
      (while overview
	;; key is message-id
	(if (elmo-get-hash-val (caar overview) ehash) ; duplicated.
	    (setq duplicates (cons
			      (elmo-msgdb-overview-entity-get-number
			       (car overview))
			      duplicates)))
	(if (caar overview)
	    (elmo-set-hash-val (caar overview) (car overview) ehash))
	;; key is number
	(elmo-set-hash-val
	 (format "#%d"
		 (elmo-msgdb-overview-entity-get-number (car overview)))
	 (car overview) ehash)
	(setq overview (cdr overview)))
      (while mark-alist
	;; key is number
	(elmo-set-hash-val
	 (format "#%d" (car (car mark-alist)))
	 (car mark-alist) mhash)
	(setq mark-alist (cdr mark-alist)))
      (setq index (or index (cons ehash mhash)))
      (elmo-msgdb-set-index msgdb index)
      duplicates)))

(defun elmo-msgdb-clear-index (msgdb entity)
  (let ((ehash (elmo-msgdb-get-entity-hashtb msgdb))
	(mhash (elmo-msgdb-get-mark-hashtb msgdb))
	number)
    (when (and entity ehash)
      (and (setq number (elmo-msgdb-overview-entity-get-number entity))
	   (elmo-clear-hash-val (format "#%d" number) ehash))
      (and (car entity) ;; message-id
	   (elmo-clear-hash-val (car entity) ehash)))
    (when (and entity mhash)
      (and (setq number (elmo-msgdb-overview-entity-get-number entity))
	   (elmo-clear-hash-val (format "#%d" number) mhash)))))

;;; Implement
;;
(luna-define-method elmo-msgdb-load ((msgdb modb-legacy))
  (let ((inhibit-quit t)
	(path (elmo-msgdb-location msgdb)))
    (when (file-exists-p (expand-file-name elmo-msgdb-mark-filename path))
      (modb-legacy-set-overview-internal
       msgdb
       (elmo-msgdb-overview-load path))
      (modb-legacy-set-number-alist-internal
       msgdb
       (elmo-msgdb-number-load path))
      (modb-legacy-set-mark-alist-internal
       msgdb
       (elmo-msgdb-mark-load path))
      (elmo-msgdb-make-index msgdb)
      t)))

(luna-define-method elmo-msgdb-save ((msgdb modb-legacy))
  (let ((path (elmo-msgdb-location msgdb)))
    (when (elmo-msgdb-message-modified-p msgdb)
      (elmo-msgdb-overview-save
       path
       (modb-legacy-overview-internal msgdb))
      (elmo-msgdb-number-save
       path
       (modb-legacy-number-alist-internal msgdb))
      (modb-generic-set-message-modified-internal msgdb nil))
    (when (elmo-msgdb-flag-modified-p msgdb)
      (elmo-msgdb-mark-save
       path
       (modb-legacy-mark-alist-internal msgdb))
      (modb-generic-set-flag-modified-internal msgdb nil))))

(luna-define-method elmo-msgdb-append :around ((msgdb modb-legacy)
					       msgdb-append)
  (if (eq (luna-class-name msgdb-append)
	  'modb-legacy)
      (let (duplicates)
	(elmo-msgdb-set-overview
	 msgdb
	 (nconc (elmo-msgdb-get-overview msgdb)
		(elmo-msgdb-get-overview msgdb-append)))
	(elmo-msgdb-set-number-alist
	 msgdb
	 (nconc (elmo-msgdb-get-number-alist msgdb)
		(elmo-msgdb-get-number-alist msgdb-append)))
	(elmo-msgdb-set-mark-alist
	 msgdb
	 (nconc (elmo-msgdb-get-mark-alist msgdb)
		(elmo-msgdb-get-mark-alist msgdb-append)))
	(setq duplicates (elmo-msgdb-make-index
			  msgdb
			  (elmo-msgdb-get-overview msgdb-append)
			  (elmo-msgdb-get-mark-alist msgdb-append)))
	(elmo-msgdb-set-path
	 msgdb
	 (or (elmo-msgdb-get-path msgdb)
	     (elmo-msgdb-get-path msgdb-append)))
	(modb-generic-set-message-modified-internal msgdb t)
	(modb-generic-set-flag-modified-internal msgdb t)
	duplicates)
    (luna-call-next-method)))

(luna-define-method elmo-msgdb-clear :after ((msgdb modb-legacy))
  (elmo-msgdb-set-overview msgdb nil)
  (elmo-msgdb-set-number-alist msgdb nil)
  (elmo-msgdb-set-mark-alist msgdb nil)
  (elmo-msgdb-set-index msgdb nil))

(luna-define-method elmo-msgdb-length ((msgdb modb-legacy))
  (length (modb-legacy-overview-internal msgdb)))

(luna-define-method elmo-msgdb-flags ((msgdb modb-legacy) number)
  (modb-legacy-mark-to-flags (elmo-msgdb-get-mark msgdb number)))

(luna-define-method elmo-msgdb-set-flag ((msgdb modb-legacy)
					 number flag)
  (case flag
    (read
     (elmo-msgdb-unset-flag msgdb number 'unread))
    (uncached
     (elmo-msgdb-unset-flag msgdb number 'cached))
    (t
     (let* ((cur-mark (elmo-msgdb-get-mark msgdb number))
	    (flags (modb-legacy-mark-to-flags cur-mark))
	    new-mark)
       (and (memq 'new flags)
	    (setq flags (delq 'new flags)))
       (or (memq flag flags)
	   (setq flags (cons flag flags)))
       (when (and (eq flag 'unread)
		  (memq 'answered flags))
	 (setq flags (delq 'answered flags)))
       (setq new-mark (modb-legacy-flags-to-mark flags))
       (unless (string= new-mark cur-mark)
	 (elmo-msgdb-set-mark msgdb number new-mark))))))

(luna-define-method elmo-msgdb-unset-flag ((msgdb modb-legacy)
					   number flag)
  (case flag
    (read
     (elmo-msgdb-set-flag msgdb number 'unread))
    (uncached
     (elmo-msgdb-set-flag msgdb number 'cached))
    (t
     (let* ((cur-mark (elmo-msgdb-get-mark msgdb number))
	    (flags (modb-legacy-mark-to-flags cur-mark))
	    new-mark)
       (and (memq 'new flags)
	    (setq flags (delq 'new flags)))
       (and (memq flag flags)
	    (setq flags (delq flag flags)))
       (when (and (eq flag 'unread)
		  (memq 'answered flags))
	 (setq flags (delq 'answered flags)))
       (setq new-mark (modb-legacy-flags-to-mark flags))
       (unless (string= new-mark cur-mark)
	 (elmo-msgdb-set-mark msgdb number new-mark))))))

(luna-define-method elmo-msgdb-list-messages ((msgdb modb-legacy))
  (mapcar 'elmo-msgdb-overview-entity-get-number
	  (elmo-msgdb-get-overview msgdb)))

(luna-define-method elmo-msgdb-list-flagged ((msgdb modb-legacy) flag)
  (let ((case-fold-search nil)
	mark-regexp matched)
    (case flag
      (new
       (setq mark-regexp (regexp-quote modb-legacy-new-mark)))
      (unread
       (setq mark-regexp (elmo-regexp-opt (modb-legacy-unread-marks))))
      (answered
       (setq mark-regexp (elmo-regexp-opt (modb-legacy-answered-marks))))
      (important
       (setq mark-regexp (regexp-quote modb-legacy-important-mark)))
      (read
       (setq mark-regexp (elmo-regexp-opt (modb-legacy-unread-marks))))
      (digest
       (setq mark-regexp (elmo-regexp-opt
			  (append (modb-legacy-unread-marks)
				  (list modb-legacy-important-mark)))))
      (any
       (setq mark-regexp (elmo-regexp-opt
			  (append
			   (modb-legacy-unread-marks)
			   (modb-legacy-answered-marks)
			   (list modb-legacy-important-mark))))))
    (when mark-regexp
      (if (eq flag 'read)
	  (dolist (number (elmo-msgdb-list-messages msgdb))
	    (let ((mark (elmo-msgdb-get-mark msgdb number)))
	      (unless (and mark (string-match mark-regexp mark))
		(setq matched (cons number matched)))))
	(dolist (elem (elmo-msgdb-get-mark-alist msgdb))
	  (if (string-match mark-regexp (cadr elem))
	      (setq matched (cons (car elem) matched))))))
    matched))

(luna-define-method elmo-msgdb-append-entity ((msgdb modb-legacy)
					      entity &optional flags)
  (when entity
    (let ((number (elmo-msgdb-overview-entity-get-number entity))
	  (message-id (elmo-msgdb-overview-entity-get-id entity))
	  mark)
      (elmo-msgdb-set-overview
       msgdb
       (nconc (elmo-msgdb-get-overview msgdb)
	      (list entity)))
      (elmo-msgdb-set-number-alist
       msgdb
       (nconc (elmo-msgdb-get-number-alist msgdb)
	      (list (cons number message-id))))
      (modb-generic-set-message-modified-internal msgdb t)
      (when (setq mark (modb-legacy-flags-to-mark flags))
	(elmo-msgdb-set-mark-alist
	 msgdb
	 (nconc (elmo-msgdb-get-mark-alist msgdb)
		(list (list number mark))))
	(modb-generic-set-flag-modified-internal msgdb t))
      (elmo-msgdb-make-index
       msgdb
       (list entity)
       (list (list number mark))))))

(luna-define-method elmo-msgdb-delete-messages ((msgdb modb-legacy)
						numbers)
  (let* ((overview (elmo-msgdb-get-overview msgdb))
	 (number-alist (elmo-msgdb-get-number-alist msgdb))
	 (mark-alist (elmo-msgdb-get-mark-alist msgdb))
	 (index (elmo-msgdb-get-index msgdb))
	 ov-entity)
    ;; remove from current database.
    (dolist (number numbers)
      (setq overview
	    (delq
	     (setq ov-entity
		   (elmo-msgdb-message-entity msgdb number))
	     overview))
      (setq number-alist (delq (assq number number-alist) number-alist))
      (setq mark-alist (delq (assq number mark-alist) mark-alist))
      ;;
      (when index (elmo-msgdb-clear-index msgdb ov-entity)))
    (elmo-msgdb-set-overview msgdb overview)
    (elmo-msgdb-set-number-alist msgdb number-alist)
    (elmo-msgdb-set-mark-alist msgdb mark-alist)
    (elmo-msgdb-set-index msgdb index)
    (modb-generic-set-message-modified-internal msgdb t)
    (modb-generic-set-flag-modified-internal msgdb t)
    t)) ;return value

(luna-define-method elmo-msgdb-sort-entities ((msgdb modb-legacy)
					      predicate &optional app-data)
  (message "Sorting...")
  (let ((overview (elmo-msgdb-get-overview msgdb)))
    (elmo-msgdb-set-overview
     msgdb
     (sort overview (lambda (a b) (funcall predicate a b app-data))))
    (message "Sorting...done")
    msgdb))

(luna-define-method elmo-msgdb-message-entity ((msgdb modb-legacy) key)
  (elmo-get-hash-val
   (cond ((stringp key) key)
	 ((numberp key) (format "#%d" key)))
   (elmo-msgdb-get-entity-hashtb msgdb)))

;;; Message entity handling.
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

(luna-define-method elmo-msgdb-make-message-entity ((msgdb modb-legacy)
						    args)
  (modb-legacy-make-message-entity args))

(defsubst elmo-msgdb-insert-file-header (file)
  "Insert the header of the article."
  (let ((beg 0)
	insert-file-contents-pre-hook   ; To avoid autoconv-xmas...
	insert-file-contents-post-hook
	format-alist)
    (when (file-exists-p file)
      ;; Read until header separator is found.
      (while (and (eq elmo-msgdb-file-header-chop-length
		      (nth 1
			   (insert-file-contents-as-binary
			    file nil beg
			    (incf beg elmo-msgdb-file-header-chop-length))))
		  (prog1 (not (search-forward "\n\n" nil t))
		    (goto-char (point-max))))))))

(luna-define-method elmo-msgdb-create-message-entity-from-file
  ((msgdb modb-legacy) number file)
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
	   msgdb number :size size :date mtime))))))

(luna-define-method elmo-msgdb-create-message-entity-from-buffer
  ((msgdb modb-legacy) number args)
  (let ((extras elmo-msgdb-extra-fields)
	(default-mime-charset default-mime-charset)
	entity message-id references from subject to cc date
	extra field-body charset size)
    (save-excursion
      (setq entity (modb-legacy-make-message-entity args))
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
      (dolist (field '(message-id number references from subject
				  date to cc size))
	(when (symbol-value field)
	  (elmo-msgdb-message-entity-set-field
	   msgdb entity field (symbol-value field))))
      entity)))

;;; Message entity interface
;;
(luna-define-method elmo-msgdb-message-entity-number ((msgdb modb-legacy)
						      entity)
  (and entity (aref (cdr entity) 0)))

(luna-define-method elmo-msgdb-message-entity-set-number ((msgdb modb-legacy)
							  entity
							  number)
  (and entity (aset (cdr entity) 0 number))
  entity)

(luna-define-method elmo-msgdb-message-entity-field ((msgdb modb-legacy)
						     entity field
						     &optional decode)
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

(luna-define-method elmo-msgdb-message-entity-set-field ((msgdb modb-legacy)
							 entity field value)
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

(luna-define-method elmo-msgdb-copy-message-entity ((msgdb modb-legacy)
						    entity)
  (cons (car entity)
	(copy-sequence (cdr entity))))

(luna-define-method elmo-msgdb-match-condition-internal ((msgdb modb-legacy)
							 condition
							 entity flags numbers)
  (cond
   ((vectorp condition)
    (elmo-msgdb-match-condition-primitive condition entity flags numbers))
   ((eq (car condition) 'and)
    (let ((lhs (elmo-msgdb-match-condition-internal msgdb
						    (nth 1 condition)
						    entity flags numbers)))
      (cond
       ((elmo-filter-condition-p lhs)
	(let ((rhs (elmo-msgdb-match-condition-internal
		    msgdb (nth 2 condition) entity flags numbers)))
	  (cond ((elmo-filter-condition-p rhs)
		 (list 'and lhs rhs))
		(rhs
		 lhs))))
       (lhs
	(elmo-msgdb-match-condition-internal msgdb (nth 2 condition)
					     entity flags numbers)))))
   ((eq (car condition) 'or)
    (let ((lhs (elmo-msgdb-match-condition-internal msgdb (nth 1 condition)
						    entity flags numbers)))
      (cond
       ((elmo-filter-condition-p lhs)
	(let ((rhs (elmo-msgdb-match-condition-internal msgdb
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
	(elmo-msgdb-match-condition-internal msgdb
					     (nth 2 condition)
					     entity flags numbers)))))))

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

(require 'product)
(product-provide (provide 'modb-legacy) (require 'elmo-version))

;;; modb-legacy.el ends here
