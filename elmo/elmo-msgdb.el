;;; elmo-msgdb.el --- Message Database for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000           Masahiro MURATA <muse@ba2.so-net.ne.jp>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA <muse@ba2.so-net.ne.jp>
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
(require 'emu)
(require 'std11)

(defsubst elmo-msgdb-append-element (list element)
  (if list
;;;   (append list (list element))
      (nconc list (list element))
    ;; list is nil
    (list element)))

(defsubst elmo-msgdb-get-overview (msgdb)
  (car msgdb))
(defsubst elmo-msgdb-get-number-alist (msgdb)
  (cadr msgdb))
(defsubst elmo-msgdb-get-mark-alist (msgdb)
  (caddr msgdb))
;(defsubst elmo-msgdb-get-location (msgdb)
;  (cadddr msgdb))
(defsubst elmo-msgdb-get-overviewht (msgdb)
  (nth 3 msgdb))

;;
;; number <-> Message-ID handling
;;
(defsubst elmo-msgdb-number-add (alist number id)
  (let ((ret-val alist))
    (setq ret-val
	  (elmo-msgdb-append-element ret-val (cons number id)))
    ret-val))

;;;
;; parsistent mark handling
;; (for global!)

(defvar elmo-msgdb-global-mark-alist nil)

(defun elmo-msgdb-global-mark-delete (msgid)
  (let* ((path (expand-file-name
		elmo-msgdb-global-mark-filename
		elmo-msgdb-directory))
	 (malist (or elmo-msgdb-global-mark-alist
		     (setq elmo-msgdb-global-mark-alist
			   (elmo-object-load path))))
	 match)
    (when (setq match (assoc msgid malist))
      (setq elmo-msgdb-global-mark-alist
	    (delete match elmo-msgdb-global-mark-alist))
      (elmo-object-save path elmo-msgdb-global-mark-alist))))

(defun elmo-msgdb-global-mark-set (msgid mark)
  (let* ((path (expand-file-name
		elmo-msgdb-global-mark-filename
		elmo-msgdb-directory))
	 (malist (or elmo-msgdb-global-mark-alist
		     (setq elmo-msgdb-global-mark-alist
			   (elmo-object-load path))))
	 match)
    (if (setq match (assoc msgid malist))
	(setcdr match mark)
      (setq elmo-msgdb-global-mark-alist
	    (nconc elmo-msgdb-global-mark-alist
		   (list (cons msgid mark)))))
    (elmo-object-save path elmo-msgdb-global-mark-alist)))

(defun elmo-msgdb-global-mark-get (msgid)
  (cdr (assoc msgid (or elmo-msgdb-global-mark-alist
			(setq elmo-msgdb-global-mark-alist
			      (elmo-object-load
			       (expand-file-name
				elmo-msgdb-global-mark-filename
				elmo-msgdb-directory)))))))

;;;
;; persistent mark handling
;; (for each folder)
(defun elmo-msgdb-mark-set (alist id mark)
  (let ((ret-val alist)
	entity)
    (setq entity (assq id alist))
    (if entity
	(if (eq mark nil)
	    ;; delete this entity
	    (setq ret-val (delq entity alist))
	  ;; set mark
	  (setcar (cdr entity) mark))
      (if mark
	  (setq ret-val (elmo-msgdb-append-element ret-val
						   (list id mark)))))
    ret-val))

(defun elmo-msgdb-mark-append (alist id mark)
  "Append mark."
  (setq alist (elmo-msgdb-append-element alist
					 (list id mark))))

(defun elmo-msgdb-mark-alist-to-seen-list (number-alist mark-alist seen-marks)
  "Make seen-list from MARK-ALIST."
  (let ((seen-mark-list (string-to-char-list seen-marks))
	ret-val ent)
    (while number-alist
      (if (setq ent (assq (car (car number-alist)) mark-alist))
	  (if (and (cadr ent)
		   (memq (string-to-char (cadr ent)) seen-mark-list))
	      (setq ret-val (cons (cdr (car number-alist)) ret-val)))
	(setq ret-val (cons (cdr (car number-alist)) ret-val)))
      (setq number-alist (cdr number-alist)))
    ret-val))

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

;;
;; overview handling
;;

(defsubst elmo-msgdb-get-field-value (field-name beg end buffer)
  (save-excursion
    (save-restriction
      (set-buffer buffer)
      (narrow-to-region beg end)
      (elmo-field-body field-name))))

(defun elmo-multiple-field-body (name &optional boundary)
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (goto-char (point-min))
      (let ((case-fold-search t)
	    (field-body nil))
	(while (re-search-forward (concat "^" name ":[ \t]*") nil t)
	  (setq field-body
		(nconc field-body
		       (list (buffer-substring-no-properties
			      (match-end 0) (std11-field-end))))))
	field-body))))

(defun elmo-multiple-fields-body-list (field-names &optional boundary)
  "Return list of each field-bodies of FIELD-NAMES of the message header
in current buffer. If BOUNDARY is not nil, it is used as message
header separator."
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (let* ((case-fold-search t)
	     (s-rest field-names)
	     field-name field-body)
	(while (setq field-name (car s-rest))
	  (goto-char (point-min))
	  (while (re-search-forward (concat "^" field-name ":[ \t]*") nil t)
	    (setq field-body
		  (nconc field-body
			 (list (buffer-substring-no-properties
				(match-end 0) (std11-field-end))))))
	  (setq s-rest (cdr s-rest)))
	field-body))))

(defsubst elmo-msgdb-remove-field-string (string)
  (if (string-match (concat std11-field-head-regexp "[ \t]*") string)
      (substring string (match-end 0))
    string))

(defsubst elmo-msgdb-get-last-message-id (string)
  (if string
      (save-match-data
	(let (beg)
	  (elmo-set-work-buf
	   (insert string)
	   (goto-char (point-max))
	   (when (search-backward "<" nil t)
	     (setq beg (point))
	     (if (search-forward ">" nil t)
		 (elmo-replace-in-string
		  (buffer-substring beg (point)) "\n[ \t]*" ""))))))))

(defun elmo-msgdb-number-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-number-filename dir)))

(defun elmo-msgdb-overview-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-overview-filename dir)))

(defun elmo-msgdb-mark-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-mark-filename dir)))

(defsubst elmo-msgdb-seen-load (dir)
  (elmo-object-load (expand-file-name
		     elmo-msgdb-seen-filename
		     dir)))

(defun elmo-msgdb-number-save (dir obj)
  (elmo-object-save
   (expand-file-name elmo-msgdb-number-filename dir)
   obj))

(defun elmo-msgdb-mark-save (dir obj)
  (elmo-object-save
   (expand-file-name elmo-msgdb-mark-filename dir)
   obj))

(defun elmo-msgdb-change-mark (msgdb before after)
  "Set the BEFORE marks to AFTER."
  (let ((mark-alist (elmo-msgdb-get-mark-alist msgdb))
	entity)
    (while mark-alist
      (setq entity (car mark-alist))
      (when (string= (cadr entity) before)
	(setcar (cdr entity) after))
      (setq mark-alist (cdr mark-alist)))))

(defsubst elmo-msgdb-seen-save (dir obj)
  (elmo-object-save
   (expand-file-name elmo-msgdb-seen-filename dir)
   obj))

(defsubst elmo-msgdb-overview-save (dir overview)
  (elmo-object-save
   (expand-file-name elmo-msgdb-overview-filename dir)
   overview))

(defun elmo-msgdb-search-internal-primitive (condition entity number-list)
  (let ((key (elmo-filter-key condition))
	(case-fold-search t)
	result)
    (cond
     ((string= key "last")
      (setq result (<= (length (memq
				(elmo-msgdb-overview-entity-get-number entity)
				number-list))
		       (string-to-int (elmo-filter-value condition)))))
     ((string= key "first")
      (setq result (< (-
		       (length number-list)
		       (length (memq
				(elmo-msgdb-overview-entity-get-number entity)
				number-list)))
		      (string-to-int (elmo-filter-value condition)))))
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
      (let ((field-date (timezone-make-date-sortable
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
	(if (stringp extval)
	    (setq result (string-match
			  (elmo-filter-value condition)
			  extval))))))
    (if (eq (elmo-filter-type condition) 'unmatch)
	(setq result (not result)))
    result))

(defun elmo-msgdb-search-internal (condition entity number-list)
  (cond
   ((vectorp condition)
    (elmo-msgdb-search-internal-primitive condition entity number-list))
   ((eq (car condition) 'and)
    (and (elmo-msgdb-search-internal
	  (nth 1 condition) entity number-list)
	 (elmo-msgdb-search-internal
	  (nth 2 condition) entity number-list)))
   ((eq (car condition) 'or)
    (or (elmo-msgdb-search-internal
	 (nth 1 condition) entity number-list)
	(elmo-msgdb-search-internal
	 (nth 2 condition) entity number-list)))))

(defun elmo-msgdb-delete-msgs (msgdb msgs)
  "Delete MSGS from MSGDB
content of MSGDB is changed."
  (save-excursion
    (let* (;(msgdb (elmo-folder-msgdb folder))
	   (overview (car msgdb))
	   (number-alist (cadr msgdb))
	   (mark-alist (caddr msgdb))
	   (hashtb (elmo-msgdb-get-overviewht msgdb))
	   (newmsgdb (list overview number-alist mark-alist hashtb))
	   ov-entity)
      ;; remove from current database.
      (while msgs
	(setq overview
	      (delq
	       (setq ov-entity
		     (elmo-msgdb-overview-get-entity (car msgs) newmsgdb))
	       overview))
	(when (and elmo-use-overview-hashtb hashtb)
	  (elmo-msgdb-clear-overview-hashtb ov-entity hashtb))
	(setq number-alist
	      (delq (assq (car msgs) number-alist) number-alist))
	(setq mark-alist (delq (assq (car msgs) mark-alist) mark-alist))
	(setq msgs (cdr msgs)))
      ;(elmo-folder-set-message-modified-internal folder t)
      (setcar msgdb overview)
      (setcar (cdr msgdb) number-alist)
      (setcar (cddr msgdb) mark-alist)
      (setcar (nthcdr 3 msgdb) hashtb))
    t)) ;return value

(defsubst elmo-msgdb-set-overview (msgdb overview)
  (setcar msgdb overview))

(defsubst elmo-msgdb-set-number-alist (msgdb number-alist)
  (setcar (cdr msgdb) number-alist))

(defsubst elmo-msgdb-set-mark-alist (msgdb mark-alist)
  (setcar (cddr msgdb) mark-alist))

(defsubst elmo-msgdb-overview-entity-get-references (entity)
  (and entity (aref (cdr entity) 1)))

(defsubst elmo-msgdb-overview-entity-set-references (entity references)
  (and entity (aset (cdr entity) 1 references))
  entity)

;; entity -> parent-entity
(defsubst elmo-msgdb-overview-get-parent-entity (entity database)
  (setq entity (elmo-msgdb-overview-entity-get-references entity))
  ;; entity is parent-id.
  (and entity (assoc entity database)))

(defsubst elmo-msgdb-overview-entity-get-number (entity)
  (and entity (aref (cdr entity) 0)))

(defsubst elmo-msgdb-overview-entity-get-from-no-decode (entity)
  (and entity (aref (cdr entity) 2)))

(defsubst elmo-msgdb-overview-entity-get-from (entity)
  (and entity
       (aref (cdr entity) 2)
       (elmo-msgdb-get-decoded-cache (aref (cdr entity) 2))))

(defsubst elmo-msgdb-overview-entity-set-number (entity number)
  (and entity (aset (cdr entity) 0 number))
  entity)
;;;(setcar (cadr entity) number) entity)

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

(defsubst elmo-msgdb-overview-entity-get-id (entity)
  (and entity (car entity)))

(defsubst elmo-msgdb-overview-entity-get-extra-field (entity field-name)
  (let ((extra (and entity (aref (cdr entity) 8))))
    (and extra
	 (cdr (assoc field-name extra)))))

(defsubst elmo-msgdb-overview-entity-set-extra-field (entity field-name value)
  (let ((extras (and entity (aref (cdr entity) 8)))
	extra)
    (if (setq extra (assoc field-name extras))
	(setcdr extra value)
      (elmo-msgdb-overview-entity-set-extra
       entity
       (cons (cons field-name value) extras)))))

(defsubst elmo-msgdb-overview-entity-get-extra (entity)
  (and entity (aref (cdr entity) 8)))

(defsubst elmo-msgdb-overview-entity-set-extra (entity extra)
  (and entity (aset (cdr entity) 8 extra))
  entity)

(defun elmo-msgdb-overview-get-entity-by-number (database number)
  (when number
    (let ((db database)
	  entity)
      (while db
	(if (eq (elmo-msgdb-overview-entity-get-number (car db)) number)
	    (setq entity (car db)
		  db nil) ; exit loop
	  (setq db (cdr db))))
      entity)))

(defun elmo-msgdb-overview-get-entity (id msgdb)
  (when id
    (let ((ovht (elmo-msgdb-get-overviewht msgdb)))
      (if ovht ; use overview hash
	  (if (stringp id) ;; ID is message-id
	      (elmo-get-hash-val id ovht)
	    (elmo-get-hash-val (format "#%d" id) ovht))
	(let* ((overview (elmo-msgdb-get-overview msgdb))
	       (number-alist (elmo-msgdb-get-number-alist msgdb))
	       (message-id (if (stringp id)
			       id ;; ID is message-id
			     (cdr (assq id number-alist))))
	       entity)
	  (if message-id
	      (assoc message-id overview)
	    ;; ID is number. message-id is nil or no exists in number-alist.
	    (elmo-msgdb-overview-get-entity-by-number overview id)))))))

;;
;; deleted message handling
;;
(defun elmo-msgdb-killed-list-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-killed-filename dir)
   nil t))

(defun elmo-msgdb-killed-list-save (dir killed-list)
  (elmo-object-save
   (expand-file-name elmo-msgdb-killed-filename dir)
   killed-list))

(defun elmo-msgdb-killed-message-p (killed-list msg)
  (elmo-number-set-member msg killed-list))

(defun elmo-msgdb-set-as-killed (killed-list msg)
  (elmo-number-set-append killed-list msg))

(defun elmo-msgdb-append-to-killed-list (folder msgs)
  (elmo-folder-set-killed-list-internal
   folder
   (elmo-number-set-append-list
    (elmo-folder-killed-list-internal folder)
    msgs)))

(defun elmo-msgdb-killed-list-length (killed-list)
  (let ((killed killed-list)
	(ret-val 0))
    (while (car killed)
      (if (consp (car killed))
	  (setq ret-val (+ ret-val 1 (- (cdar killed) (caar killed))))
	(setq ret-val (+ ret-val 1)))
      (setq killed (cdr killed)))
    ret-val))

(defun elmo-msgdb-max-of-killed (killed-list)
  (let ((klist killed-list)
	(max 0)
	k)
    (while (car klist)
      (if (< max
	     (setq k
		   (if (consp (car klist))
		       (cdar klist)
		     (car klist))))
	  (setq max k))
      (setq klist (cdr klist)))
    max))

(defun elmo-living-messages (messages killed-list)
  (if killed-list
      (delq nil
	    (mapcar (lambda (number)
		      (unless (elmo-number-set-member number killed-list)
			number))
		    messages))
    messages))

(defun elmo-msgdb-finfo-load ()
  (elmo-object-load (expand-file-name
		     elmo-msgdb-finfo-filename
		     elmo-msgdb-directory)
		    elmo-mime-charset t))

(defun elmo-msgdb-finfo-save (finfo)
  (elmo-object-save (expand-file-name
		     elmo-msgdb-finfo-filename
		     elmo-msgdb-directory)
		    finfo elmo-mime-charset))

(defun elmo-msgdb-flist-load (fname)
  (let ((flist-file (expand-file-name
		     elmo-msgdb-flist-filename
		     (expand-file-name
		      (elmo-safe-filename fname)
		      (expand-file-name "folder" elmo-msgdb-directory)))))
    (elmo-object-load flist-file elmo-mime-charset t)))

(defun elmo-msgdb-flist-save (fname flist)
  (let ((flist-file (expand-file-name
		     elmo-msgdb-flist-filename
		     (expand-file-name
		      (elmo-safe-filename fname)
		      (expand-file-name "folder" elmo-msgdb-directory)))))
    (elmo-object-save flist-file flist elmo-mime-charset)))

(defun elmo-crosspost-alist-load ()
  (elmo-object-load (expand-file-name
		     elmo-crosspost-alist-filename
		     elmo-msgdb-directory)
		    nil t))

(defun elmo-crosspost-alist-save (alist)
  (elmo-object-save (expand-file-name
		     elmo-crosspost-alist-filename
		     elmo-msgdb-directory)
		    alist))

(defun elmo-msgdb-add-msgs-to-seen-list (msgs msgdb unread-marks seen-list)
  ;; Add to seen list.
  (let* ((number-alist (elmo-msgdb-get-number-alist msgdb))
	 (mark-alist   (elmo-msgdb-get-mark-alist msgdb))
	 ent)
    (while msgs
      (if (setq ent (assq (car msgs) mark-alist))
	  (unless (member (cadr ent) unread-marks) ;; not unread mark
	    (setq seen-list
		  (cons (cdr (assq (car msgs) number-alist)) seen-list)))
	;; no mark ... seen...
	(setq seen-list
	      (cons (cdr (assq (car msgs) number-alist)) seen-list)))
      (setq msgs (cdr msgs)))
    seen-list))

(defun elmo-msgdb-get-message-id-from-buffer ()
  (or (elmo-field-body "message-id")
      ;; no message-id, so put dummy msgid.
      (concat (timezone-make-date-sortable
	       (elmo-field-body "date"))
	      (nth 1 (eword-extract-address-components
		      (or (elmo-field-body "from") "nobody"))))))

(defsubst elmo-msgdb-create-overview-from-buffer (number &optional size time)
  "Create overview entity from current buffer.
Header region is supposed to be narrowed."
  (save-excursion
    (let ((extras elmo-msgdb-extra-fields)
	  message-id references from subject to cc date
	  extra field-body)
      (elmo-set-buffer-multibyte default-enable-multibyte-characters)
      (setq message-id (elmo-msgdb-get-message-id-from-buffer))
      (setq references
	    (or (elmo-msgdb-get-last-message-id
		 (elmo-field-body "in-reply-to"))
		(elmo-msgdb-get-last-message-id
		 (elmo-field-body "references"))))
      (setq from (elmo-mime-string (elmo-delete-char
				    ?\"
				    (or
				     (elmo-field-body "from")
				     elmo-no-from))))
      (setq subject (elmo-mime-string (or (elmo-field-body "subject")
					  elmo-no-subject)))
      (setq date (or (elmo-field-body "date") time))
      (setq to   (mapconcat 'identity (elmo-multiple-field-body "to") ","))
      (setq cc   (mapconcat 'identity (elmo-multiple-field-body "cc") ","))
      (or size
	  (if (setq size (elmo-field-body "content-length"))
	      (setq size (string-to-int size))
	    (setq size 0)));; No mean...
      (while extras
	(if (setq field-body (elmo-field-body (car extras)))
	    (setq extra (cons (cons (downcase (car extras))
				    field-body) extra)))
	(setq extras (cdr extras)))
      (cons message-id (vector number references
			       from subject date to cc
			       size extra))
      )))

(defun elmo-msgdb-copy-overview-entity (entity)
  (cons (car entity)
	(copy-sequence (cdr entity))))

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

(defsubst elmo-msgdb-create-overview-entity-from-file (number file)
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
	  (elmo-msgdb-create-overview-from-buffer number size mtime))))))

(defun elmo-msgdb-overview-sort-by-date (overview)
  (sort overview
	(function
	 (lambda (x y)
	   (condition-case nil
	       (string<
		(timezone-make-date-sortable
		 (elmo-msgdb-overview-entity-get-date x))
		(timezone-make-date-sortable
		 (elmo-msgdb-overview-entity-get-date y)))
	     (error))))))

(defun elmo-msgdb-sort-by-date (msgdb)
  (message "Sorting...")
  (let ((overview (elmo-msgdb-get-overview msgdb)))
    (setq overview (elmo-msgdb-overview-sort-by-date overview))
    (message "Sorting...done")
    (list overview (nth 1 msgdb)(nth 2 msgdb))))

(defun elmo-msgdb-clear-overview-hashtb (entity hashtb)
  (let (number)
    (when (and entity
	       elmo-use-overview-hashtb
	       hashtb)
      (and (setq number (elmo-msgdb-overview-entity-get-number entity))
	   (elmo-clear-hash-val (format "#%d" number) hashtb))
      (and (car entity) ;; message-id
	   (elmo-clear-hash-val (car entity) hashtb)))))

(defun elmo-msgdb-make-overview-hashtb (overview &optional hashtb)
  (if (and elmo-use-overview-hashtb
	   overview)
      (let ((hashtb (or hashtb ;; append
			(elmo-make-hash (length overview)))))
	(while overview
	  ;; key is message-id
	  (if (caar overview)
	      (elmo-set-hash-val (caar overview) (car overview) hashtb))
	  ;; key is number
	  (elmo-set-hash-val
	   (format "#%d" (elmo-msgdb-overview-entity-get-number (car overview)))
	   (car overview) hashtb)
	  (setq overview (cdr overview)))
	hashtb)
    nil))

(defsubst elmo-msgdb-append (msgdb msgdb-append &optional set-hash)
  (list
   (nconc (car msgdb) (car msgdb-append))
   (nconc (cadr msgdb) (cadr msgdb-append))
   (nconc (caddr msgdb) (caddr msgdb-append))
   (and set-hash
	(elmo-msgdb-make-overview-hashtb (car msgdb-append) (nth 3 msgdb)))))

(defsubst elmo-msgdb-clear (&optional msgdb)
  (if msgdb
      (list
       (setcar msgdb nil)
       (setcar (cdr msgdb) nil)
       (setcar (cddr msgdb) nil)
       (setcar (nthcdr 3 msgdb) (elmo-msgdb-make-overview-hashtb nil)))
    (list nil nil nil (elmo-msgdb-make-overview-hashtb nil))))

(defsubst elmo-folder-get-info (folder &optional hashtb)
  (elmo-get-hash-val folder
		     (or hashtb elmo-folder-info-hashtb)))

(defun elmo-folder-set-info-hashtb (folder max numbers &optional new unread)
  (let ((info (elmo-folder-get-info folder)))
    (when info
      (or new     (setq new     (nth 0 info)))
      (or unread  (setq unread  (nth 1 info)))
      (or numbers (setq numbers (nth 2 info)))
      (or max     (setq max     (nth 3 info))))
    (elmo-set-hash-val folder
		       (list new unread numbers max)
		       elmo-folder-info-hashtb)))

(defun elmo-folder-set-info-max-by-numdb (folder msgdb-number)
  (let ((num-db (sort (mapcar 'car msgdb-number) '<)))
    (elmo-folder-set-info-hashtb
     folder
     (or (nth (max 0 (1- (length num-db))) num-db) 0)
     nil ;;(length num-db)
     )))

(defun elmo-folder-get-info-max (folder)
  "Get folder info from cache."
  (nth 3 (elmo-folder-get-info folder)))

(defun elmo-folder-get-info-length (folder)
  (nth 2 (elmo-folder-get-info folder)))

(defun elmo-folder-get-info-unread (folder)
  (nth 1 (elmo-folder-get-info folder)))

(defun elmo-folder-info-make-hashtb (info-alist hashtb)
  (let* ((hashtb (or hashtb
		     (elmo-make-hash (length info-alist)))))
    (mapcar
     '(lambda (x)
	(let ((info (cadr x)))
	  (and (intern-soft (car x) hashtb)
	       (elmo-set-hash-val (car x)
				  (list (nth 2 info)   ;; new
					(nth 3 info)   ;; unread
					(nth 1 info)   ;; length
					(nth 0 info))  ;; max
				  hashtb))))
     info-alist)
    (setq elmo-folder-info-hashtb hashtb)))

(defsubst elmo-msgdb-location-load (dir)
  (elmo-object-load
   (expand-file-name
    elmo-msgdb-location-filename
    dir)))

(defsubst elmo-msgdb-location-add (alist number location)
  (let ((ret-val alist))
    (setq ret-val
	  (elmo-msgdb-append-element ret-val (cons number location)))
    ret-val))

(defsubst elmo-msgdb-location-save (dir alist)
  (elmo-object-save
   (expand-file-name
    elmo-msgdb-location-filename
    dir) alist))

(require 'product)
(product-provide (provide 'elmo-msgdb) (require 'elmo-version))

;;; elmo-msgdb.el ends here
