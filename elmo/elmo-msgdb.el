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
(require 'mime)
(require 'modb)
(require 'modb-entity)

;;; MSGDB interface.
;;
;; MSGDB elmo-load-msgdb PATH

;; NUMBER elmo-msgdb-get-number MSGDB MESSAGE-ID
;; FIELD-VALUE elmo-msgdb-get-field MSGDB NUMBER FIELD
;; elmo-msgdb-sort-by-date MSGDB

;; elmo-flag-table-load
;; elmo-flag-table-set
;; elmo-flag-table-get
;; elmo-flag-table-save

;; elmo-msgdb-overview-save DIR OBJ

;;; Abolish
;; elmo-msgdb-get-parent-entity ENTITY MSGDB

;; elmo-msgdb-killed-list-load DIR
;; elmo-msgdb-killed-list-save DIR
;; elmo-msgdb-append-to-killed-list FOLDER MSG
;; elmo-msgdb-killed-list-length KILLED-LIST
;; elmo-msgdb-max-of-killed KILLED-LIST
;; elmo-msgdb-killed-message-p KILLED-LIST MSG
;; elmo-living-messages MESSAGES KILLED-LIST

;; elmo-msgdb-finfo-load
;; elmo-msgdb-finfo-save
;; elmo-msgdb-flist-load
;; elmo-msgdb-flist-save

;; elmo-crosspost-alist-load
;; elmo-crosspost-alist-save

;; elmo-msgdb-create-overview-from-buffer NUMBER SIZE TIME
;; elmo-msgdb-create-overview-entity-from-file NUMBER FILE

;; elmo-folder-get-info
;; elmo-folder-get-info-max
;; elmo-folder-get-info-length
;; elmo-folder-get-info-unread

;;; Helper functions for MSGDB
;;
(defun elmo-load-msgdb (location)
  "Load the MSGDB from PATH."
  (let ((msgdb (elmo-make-msgdb location)))
    (elmo-msgdb-load msgdb)
    msgdb))

(defun elmo-make-msgdb (&optional location type)
  "Make a MSGDB."
  (let* ((type (or type elmo-msgdb-default-type))
	 (class (intern (format "modb-%s" type))))
    (require class)
    (luna-make-entity class
		      :location location)))

(defsubst elmo-msgdb-get-number (msgdb message-id)
  "Get number of the message which corrensponds to MESSAGE-ID from MSGDB."
  (elmo-msgdb-overview-entity-get-number
   (elmo-msgdb-message-entity msgdb message-id)))

(defsubst elmo-msgdb-get-field (msgdb number field)
  "Get FIELD value of the message with NUMBER from MSGDB."
  (case field
    (message-id (elmo-msgdb-overview-entity-get-id
		 (elmo-msgdb-message-entity
		  msgdb number)))
    (subject (elmo-msgdb-overview-entity-get-subject
	      (elmo-msgdb-message-entity
	       msgdb number)))
    (size (elmo-msgdb-overview-entity-get-size
	   (elmo-msgdb-message-entity
	    msgdb number)))
    (date (elmo-msgdb-overview-entity-get-date
	   (elmo-msgdb-message-entity
	    msgdb number)))
    (to (elmo-msgdb-overview-entity-get-to
	 (elmo-msgdb-message-entity
	  msgdb number)))
    (cc (elmo-msgdb-overview-entity-get-cc
	 (elmo-msgdb-message-entity
	  msgdb number)))))

(defun elmo-msgdb-sort-by-date (msgdb)
  (elmo-msgdb-sort-entities
   msgdb
   (lambda (x y app-data)
     (condition-case nil
	 (string<
	  (timezone-make-date-sortable
	   (elmo-msgdb-overview-entity-get-date x))
	  (timezone-make-date-sortable
	   (elmo-msgdb-overview-entity-get-date y)))
       (error)))))

;;;
(defsubst elmo-msgdb-append-element (list element)
  (if list
;;;   (append list (list element))
      (nconc list (list element))
    ;; list is nil
    (list element)))

;;
;; number <-> Message-ID handling
;;
(defsubst elmo-msgdb-number-add (alist number id)
  (let ((ret-val alist))
    (setq ret-val
	  (elmo-msgdb-append-element ret-val (cons number id)))
    ret-val))

;;; flag table
;;
(defvar elmo-flag-table-filename "flag-table")
(defun elmo-flag-table-load (dir)
  "Load flag hashtable for MSGDB."
  (let ((table (elmo-make-hash))
	;; For backward compatibility
	(seen-file (expand-file-name elmo-msgdb-seen-filename dir))
	value)
    (when (file-exists-p seen-file)
      (dolist (msgid (elmo-object-load seen-file))
	(elmo-set-hash-val msgid '(read) table))
      (delete-file seen-file))
    (dolist (pair (elmo-object-load
		   (expand-file-name elmo-flag-table-filename dir)))
      (setq value (cdr pair))
      (elmo-set-hash-val (car pair)
			 (cond ((consp value)
				value)
			       ;; Following cases for backward compatibility.
			       (value
				(list value))
			       (t
				'(unread)))
			 table))
    table))

(defun elmo-flag-table-set (flag-table msg-id flags)
  (elmo-set-hash-val msg-id (or flags '(read)) flag-table))

(defun elmo-flag-table-get (flag-table msg-id)
  (let ((flags (elmo-get-hash-val msg-id flag-table)))
    (if flags
	(append
	 (and (elmo-file-cache-exists-p msg-id)
	      '(cached))
	 (elmo-list-delete '(cached read)
			   (copy-sequence flags)
			   #'delq))
      '(new unread))))

(defun elmo-flag-table-save (dir flag-table)
  (elmo-object-save
   (expand-file-name elmo-flag-table-filename dir)
   (if flag-table
       (let (list)
	 (mapatoms (lambda (atom)
		     (setq list (cons (cons (symbol-name atom)
					    (symbol-value atom))
				      list)))
		   flag-table)
	 list))))
;;;
;; persistent mark handling
;; (for each folder)

(defun elmo-msgdb-mark-append (alist id mark)
  "Append mark."
  (setq alist (elmo-msgdb-append-element alist
					 (list id mark))))

(defun elmo-msgdb-flag-table (msgdb &optional flag-table)
  ;; Make a table of msgid flag (read, answered)
  (let ((flag-table (or flag-table
			(elmo-make-hash (elmo-msgdb-length msgdb))))
	entity)
    (dolist (number (elmo-msgdb-list-messages msgdb))
      (setq entity (elmo-msgdb-message-entity msgdb number))
      (elmo-flag-table-set
       flag-table
       (elmo-msgdb-overview-entity-get-id entity)
       (elmo-msgdb-flags msgdb number)))
    flag-table))

;;
;; overview handling
;;
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

(defsubst elmo-msgdb-out-of-date-messages (msgdb)
  (dolist (number (elmo-msgdb-list-flagged msgdb 'new))
    (elmo-msgdb-unset-flag msgdb number 'new)))

(defsubst elmo-msgdb-overview-save (dir overview)
  (elmo-object-save
   (expand-file-name elmo-msgdb-overview-filename dir)
   overview))

(defun elmo-msgdb-match-condition (msgdb condition number numbers)
  "Check whether the condition of the message is satisfied or not.
MSGDB is the msgdb to search from.
CONDITION is the search condition.
NUMBER is the message number to check.
NUMBERS is the target message number list.
Return CONDITION itself if no entity exists in msgdb."
  (let ((entity (elmo-msgdb-message-entity msgdb number)))
    (if entity
	(elmo-msgdb-match-condition-internal condition
					     entity
					     (elmo-msgdb-flags msgdb number)
					     numbers)
      condition)))

;; entity -> parent-entity
(defsubst elmo-msgdb-overview-get-parent-entity (entity database)
  (setq entity (elmo-msgdb-overview-entity-get-references entity))
  ;; entity is parent-id.
  (and entity (assoc entity database)))

(defsubst elmo-msgdb-get-parent-entity (entity msgdb)
  (setq entity (elmo-msgdb-overview-entity-get-references entity))
  ;; entity is parent-id.
  (and entity (elmo-msgdb-message-entity msgdb entity)))

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

(defun elmo-msgdb-get-message-id-from-buffer ()
  (let ((msgid (elmo-field-body "message-id")))
    (if msgid
	(if (string-match "<\\(.+\\)>$" msgid)
	    msgid
	  (concat "<" msgid ">")) ; Invaild message-id.
      ;; no message-id, so put dummy msgid.
      (concat "<" (timezone-make-date-sortable
		   (elmo-field-body "date"))
	      (nth 1 (eword-extract-address-components
		      (or (elmo-field-body "from") "nobody"))) ">"))))

(defsubst elmo-msgdb-create-overview-from-buffer (number &optional size time)
  "Create overview entity from current buffer.
Header region is supposed to be narrowed."
  (save-excursion
    (let ((extras elmo-msgdb-extra-fields)
	  (default-mime-charset default-mime-charset)
	  message-id references from subject to cc date
	  extra field-body charset)
      (elmo-set-buffer-multibyte default-enable-multibyte-characters)
      (setq message-id (elmo-msgdb-get-message-id-from-buffer))
      (and (setq charset (cdr (assoc "charset" (mime-read-Content-Type))))
	   (setq charset (intern-soft charset))
	   (setq default-mime-charset charset))
      (setq references
	    (or (elmo-msgdb-get-last-message-id
		 (elmo-field-body "in-reply-to"))
		(elmo-msgdb-get-last-message-id
		 (elmo-field-body "references"))))
      (setq from (elmo-replace-in-string
		  (elmo-mime-string (or (elmo-field-body "from")
					elmo-no-from))
		  "\t" " ")
	    subject (elmo-replace-in-string
		     (elmo-mime-string (or (elmo-field-body "subject")
					   elmo-no-subject))
		     "\t" " "))
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

(defsubst elmo-folder-get-info (folder &optional hashtb)
  (elmo-get-hash-val folder
		     (or hashtb elmo-folder-info-hashtb)))

(defun elmo-folder-get-info-max (folder)
  "Get folder info from cache."
  (nth 3 (elmo-folder-get-info folder)))

(defun elmo-folder-get-info-length (folder)
  (nth 2 (elmo-folder-get-info folder)))

(defun elmo-folder-get-info-unread (folder)
  (nth 1 (elmo-folder-get-info folder)))

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
