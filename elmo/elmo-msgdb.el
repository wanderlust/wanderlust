;;; elmo-msgdb.el -- Message Database for Elmo.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news
;; Time-stamp: <00/05/11 09:20:11 teranisi>

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
(require 'elmo-cache)

(defun elmo-msgdb-expand-path (folder &optional spec)
  (convert-standard-filename
   (let* ((spec (or spec (elmo-folder-get-spec folder)))
	  (type (car spec))
	  fld)
     (cond 
      ((eq type 'imap4)
       (setq fld (elmo-imap4-spec-mailbox spec))
       (if (string= "inbox" (downcase fld))
	   (setq fld "inbox"))
       (if (eq (string-to-char fld) ?/)
	   (setq fld (substring fld 1 (length fld))))
       (expand-file-name 
	fld
	(expand-file-name (or (elmo-imap4-spec-username spec) "nobody")
			  (expand-file-name (or 
					     (elmo-imap4-spec-hostname spec)
					     "nowhere")
					    (expand-file-name 
					     "imap"
					     elmo-msgdb-dir)))))
      ((eq type 'nntp)
       (expand-file-name 
	(elmo-nntp-spec-group spec)
	(expand-file-name (or (elmo-nntp-spec-hostname spec) "nowhere")
			  (expand-file-name "nntp"
					    elmo-msgdb-dir))))
      ((eq type 'maildir)
       (expand-file-name (elmo-safe-filename (nth 1 spec))
			 (expand-file-name "maildir"
					   elmo-msgdb-dir)))
      ((eq type 'folder)
       (expand-file-name (elmo-safe-filename (nth 1 spec))
			 (expand-file-name "folder"
					   elmo-msgdb-dir)))
      ((eq type 'multi)
       (expand-file-name (elmo-safe-filename folder)
			 (expand-file-name "multi"
					   elmo-msgdb-dir)))
      ((eq type 'filter)
       (expand-file-name
	(elmo-safe-filename folder)
	(expand-file-name "filter"
			  elmo-msgdb-dir)))
      ((eq type 'archive)
       (expand-file-name 
	(directory-file-name
	 (concat
	  (elmo-replace-in-string 
	   (elmo-replace-in-string
	    (elmo-replace-in-string 
	     (nth 1 spec)
	     "/" "_")
	    ":" "__")
	   "~" "___")
	  "/" (nth 3 spec)))
	(expand-file-name (concat (symbol-name type) "/" 
				  (symbol-name (nth 2 spec)))
			  elmo-msgdb-dir)))
      ((eq type 'pop3)
       (expand-file-name 
	(elmo-safe-filename (elmo-pop3-spec-username spec))
	(expand-file-name (elmo-pop3-spec-hostname spec)
			  (expand-file-name
			   "pop"
			   elmo-msgdb-dir))))
      ((eq type 'localnews)
       (expand-file-name
	(elmo-replace-in-string (nth 1 spec) "/" ".")
	(expand-file-name "localnews"
			  elmo-msgdb-dir)))
      ((eq type 'internal)
       (expand-file-name (elmo-safe-filename (concat (symbol-name (nth 1 spec))
						     (nth 2 spec)))
			 (expand-file-name "internal"
					   elmo-msgdb-dir)))
      ((eq type 'cache)
       (expand-file-name (elmo-safe-filename (nth 1 spec))
			 (expand-file-name "internal/cache"
					   elmo-msgdb-dir)))
      (t ; local dir or undefined type
       ;; absolute path
       (setq fld (nth 1 spec))
       (if (file-name-absolute-p fld)
	   (setq fld (elmo-safe-filename fld)))
       (expand-file-name fld
			 (expand-file-name (symbol-name type)
					   elmo-msgdb-dir)))))))

(defsubst elmo-msgdb-append-element (list element)
  (if list
      ;(append list (list element))
      (nconc list (list element))
    ;; list is nil
    (list element)))

(defsubst elmo-msgdb-get-overview (msgdb)
  (car msgdb))
(defsubst elmo-msgdb-get-number-alist (msgdb)
  (cadr msgdb))
(defsubst elmo-msgdb-get-mark-alist (msgdb)
  (caddr msgdb))
(defsubst elmo-msgdb-get-location (msgdb)
  (cadddr msgdb))
(defsubst elmo-msgdb-get-overviewht (msgdb)
  (nth 4 msgdb))

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
		elmo-msgdb-dir))
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
		elmo-msgdb-dir))
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
				elmo-msgdb-dir)))))))

;;
;; number <-> location handling
;;
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

(defun elmo-list-folder-by-location (spec locations &optional msgdb)
  (let* ((path (elmo-msgdb-expand-path nil spec))
	 (location-alist (if msgdb
			     (elmo-msgdb-get-location msgdb)
			   (elmo-msgdb-location-load path)))
	 (locations-in-db (mapcar 'cdr location-alist))
	 result new-locs new-alist deleted-locs i
	 modified)
    (setq new-locs
	  (elmo-delete-if (function
			   (lambda (x) (member x locations-in-db)))
			  locations))
    (setq deleted-locs
	  (elmo-delete-if (function
			   (lambda (x) (member x locations)))
			  locations-in-db))
    (setq modified new-locs)
    (setq i (or (elmo-max-of-list (mapcar 'car location-alist)) 0))
    (mapcar 
     (function 
      (lambda (x)
	(setq location-alist
	      (delq (rassoc x location-alist) location-alist))))
     deleted-locs)
    (while new-locs
      (setq i (1+ i))
      (setq new-alist (cons (cons i (car new-locs)) new-alist))
      (setq new-locs (cdr new-locs)))
    (setq result (nconc location-alist new-alist))
    (setq result (sort result (lambda (x y) (< (car x)(car y)))))
    (if modified (elmo-msgdb-location-save path result))
    (mapcar 'car result)))

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
  "Append mark"
  (setq alist (elmo-msgdb-append-element alist
					 (list id mark))))

(defun elmo-msgdb-mark-alist-to-seen-list (number-alist mark-alist seen-marks)
  "Make seen-list from mark-alist"
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

(defsubst elmo-msgdb-seen-save (dir obj)
  (elmo-object-save 
   (expand-file-name elmo-msgdb-seen-filename dir)
   obj))

(defsubst elmo-msgdb-overview-save (dir overview)
  (elmo-object-save 
   (expand-file-name elmo-msgdb-overview-filename dir)
   overview))

(defun elmo-msgdb-delete-msgs (folder msgs msgdb &optional reserve-cache)
  "Delete MSGS from FOLDER in MSGDB. 
content of MSGDB is changed."
  (save-excursion
    (let* ((msg-list msgs)
	   (dir (elmo-msgdb-expand-path folder))
	   (overview (or (car msgdb)
			 (elmo-msgdb-overview-load dir)))
	   (number-alist (or (cadr msgdb)
			     (elmo-msgdb-number-load dir)))
	   (mark-alist (or (caddr msgdb)
			   (elmo-msgdb-mark-load dir)))
	   (hashtb (or (elmo-msgdb-get-overviewht msgdb)
		       (elmo-msgdb-make-overview-hashtb overview)))
	   (newmsgdb (list overview number-alist mark-alist (nth 3 msgdb) hashtb))
	   ov-entity message-id)
      ;; remove from current database.
      (while msg-list
	(setq message-id (cdr (assq (car msg-list) number-alist)))
	(if (and (not reserve-cache) message-id) 
	    (elmo-cache-delete message-id
			       folder (car msg-list)))
	;; This is no good!!!!
	;(setq overview (delete (assoc message-id overview) overview))
	(setq overview 
	      (delq
	       (setq ov-entity
		     (elmo-msgdb-overview-get-entity (car msg-list) newmsgdb))
	       overview))
	(when (and elmo-use-overview-hashtb hashtb)
	  (elmo-msgdb-clear-overview-hashtb ov-entity hashtb))
	(setq number-alist
	      (delq (assq (car msg-list) number-alist) number-alist))
	(setq mark-alist (delq (assq (car msg-list) mark-alist) mark-alist))
	(setq msg-list (cdr msg-list)))
      (setcar msgdb overview)
      (setcar (cdr msgdb) number-alist)
      (setcar (cddr msgdb) mark-alist)
      (setcar (nthcdr 4 msgdb) hashtb))
    t)) ;return value

(defsubst elmo-msgdb-set-overview (msgdb overview)
  (setcar msgdb overview))

(defsubst elmo-msgdb-set-number-alist (msgdb number-alist)
  (setcar (cdr msgdb) number-alist))

(defsubst elmo-msgdb-set-mark-alist (msgdb mark-alist)
  (setcar (cddr msgdb) mark-alist))

(defsubst elmo-msgdb-overview-entity-get-references (entity)
  (and entity (aref (cdr entity) 1)))

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
  ;(setcar (cadr entity) number) entity)

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

(defsubst elmo-msgdb-overview-entity-get-to (entity)
  (and entity (aref (cdr entity) 5)))

(defsubst elmo-msgdb-overview-entity-get-cc (entity)
  (and entity (aref (cdr entity) 6)))

(defsubst elmo-msgdb-overview-entity-get-size (entity)
  (and entity (aref (cdr entity) 7)))

(defsubst elmo-msgdb-overview-entity-get-id (entity)
  (and entity (car entity)))

(defsubst elmo-msgdb-overview-entity-get-extra-field (entity field-name)
  (let ((extra (and entity (aref (cdr entity) 8))))
    (and extra
	 (cdr (assoc field-name extra)))))

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
      (if ovht ;; use overview hash
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
  (and killed-list
       (not (listp
	     (catch 'found
	       (mapcar 
		(function 
		 (lambda (entity)
		   (cond 
		    ((integerp entity)
		     (if (eq entity msg)
			 (throw 'found t)))
		    ((consp entity)
		     (if (and (<= (car entity) msg)
			      (<= msg (cdr entity)))
			 (throw 'found t)))))
		 killed-list)))))))

(defun elmo-msgdb-set-as-killed (killed-list msg)
  "if cons cell, set car-cdr messages as killed.
if integer, set number th message as killed."
  (let ((dlist killed-list)
	(ret-val killed-list)
	entity found)
    (cond
     ((integerp msg)
      (while (and dlist (not found))
	(setq entity (car dlist))
	(if (or (and (integerp entity) (eq entity msg))
		(and (consp entity) 
		     (<= (car entity) msg)
		     (<= msg (cdr entity))))
	    (setq found t))
	(setq dlist (cdr dlist))
	)
      (if (not found)
	  (setq ret-val (elmo-msgdb-append-element killed-list msg)))
      )
     ((consp msg)
      (while (and dlist (not found))
	(setq entity (car dlist))
	(if (integerp entity)
	    (cond 
	     ((and (<= (car msg) entity)(<= entity (cdr msg)))
	      (setcar dlist msg)
	      (setq found t)
	      )
	     ((= (1- (car msg)) entity)
	      (setcar dlist (cons entity (cdr msg)))
	      (setq found t)
	      )
	     ((= (1+ (cdr msg)) entity)
	      (setcar dlist (cons (car msg) entity))
	      (setq found t)
	      ))
	  ;; entity is consp
	  (cond  ; there are four patterns
	   ((and (<= (car msg) (car entity))
		 (<= (cdr entity) (cdr msg)))
	    (setcar dlist msg)
	    (setq found t))
	   ((and (< (car entity)(car msg))
		 (< (cdr msg) (cdr entity)))
	    (setq found t))
	   ((and (<= (car msg) (car entity))
		 (<= (cdr msg) (cdr entity)))
	    (setcar dlist (cons (car msg) (cdr entity)))
	    (setq found t))
	   ((and (<= (car entity) (car msg))
		 (<= (cdr entity) (cdr msg)))
	    (setcar dlist (cons (car entity) (cdr msg)))
	    (setq found t))))
	(setq dlist (cdr dlist)))
      (if (not found)
	  (setq ret-val (elmo-msgdb-append-element killed-list msg)))))
    ret-val))

(defun elmo-msgdb-finfo-load ()
  (elmo-object-load (expand-file-name 
		     elmo-msgdb-finfo-filename
		     elmo-msgdb-dir)
		    elmo-mime-charset t))

(defun elmo-msgdb-finfo-save (finfo)
  (elmo-object-save (expand-file-name
		     elmo-msgdb-finfo-filename
		     elmo-msgdb-dir)
		    finfo elmo-mime-charset))

(defun elmo-msgdb-flist-load (folder)
  (let ((flist-file (expand-file-name
		     elmo-msgdb-flist-filename
		     (elmo-msgdb-expand-path folder (list 'folder folder)))))
    (elmo-object-load flist-file nil t)))

(defun elmo-msgdb-flist-save (folder flist)
  (let ((flist-file (expand-file-name
		     elmo-msgdb-flist-filename
		     (elmo-msgdb-expand-path folder (list 'folder folder)))))
    (elmo-object-save flist-file flist)))

(defun elmo-crosspost-alist-load ()
  (elmo-object-load (expand-file-name
		     elmo-crosspost-alist-filename
		     elmo-msgdb-dir)
		    nil t))

(defun elmo-crosspost-alist-save (alist)
  (elmo-object-save (expand-file-name
		     elmo-crosspost-alist-filename
		     elmo-msgdb-dir)
		    alist))

(defsubst elmo-msgdb-create-overview-from-buffer (number &optional size time)
  "Create overview entity from current buffer. 
Header region is supposed to be narrowed."
  (save-excursion
    (let ((extras elmo-msgdb-extra-fields)
	  message-id references from subject to cc date
	  extra field-body)
      (elmo-set-buffer-multibyte default-enable-multibyte-characters)
      (setq message-id (elmo-field-body "message-id"))
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
    (message "Sorting...done.")
    (list overview (nth 1 msgdb)(nth 2 msgdb)(nth 3 msgdb)(nth 4 msgdb))))

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
  (if elmo-use-overview-hashtb
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
   (nconc (cadddr msgdb) (cadddr msgdb-append))
   (and set-hash
	(elmo-msgdb-make-overview-hashtb (car msgdb-append) (nth 4 msgdb)))))

(defsubst elmo-msgdb-clear (&optional msgdb)
  (if msgdb
      (list
       (setcar msgdb nil)
       (setcar (cdr msgdb) nil)
       (setcar (cddr msgdb) nil)
       (setcar (cdddr msgdb) nil)
       (setcar (nthcdr 4 msgdb) (elmo-msgdb-make-overview-hashtb nil)))
    (list nil nil nil nil (elmo-msgdb-make-overview-hashtb nil))))

(defun elmo-msgdb-delete-path (folder &optional spec)
  (let ((path (elmo-msgdb-expand-path folder spec)))
    (if (file-directory-p path)
	(elmo-delete-directory path t))))

(defun elmo-msgdb-rename-path (old-folder new-folder &optional old-spec new-spec)
  (let* ((old (directory-file-name (elmo-msgdb-expand-path old-folder old-spec)))
	 (new (directory-file-name (elmo-msgdb-expand-path new-folder new-spec)))
	 (new-dir (directory-file-name (file-name-directory new))))
    (if (not (file-directory-p old))
	()
      (if (file-exists-p new)
	  (error "already exists directory: %s" new)
	(if (not (file-exists-p new-dir))
	    (elmo-make-directory new-dir))
	(rename-file old new)))))

(provide 'elmo-msgdb)

;;; elmo-msgdb.el ends here
