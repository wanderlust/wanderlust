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

(defcustom elmo-msgdb-new-mark "N"
  "Mark for new message."
  :type '(string :tag "Mark")
  :group 'elmo)

(defcustom elmo-msgdb-unread-uncached-mark "U"
  "Mark for unread and uncached message."
  :type '(string :tag "Mark")
  :group 'elmo)

(defcustom elmo-msgdb-unread-cached-mark "!"
  "Mark for unread but already cached message."
  :type '(string :tag "Mark")
  :group 'elmo)

(defcustom elmo-msgdb-read-uncached-mark "u"
  "Mark for read but uncached message."
  :type '(string :tag "Mark")
  :group 'elmo)

;; Not implemented yet.
(defcustom elmo-msgdb-answered-cached-mark "&"
  "Mark for answered and cached message."
  :type '(string :tag "Mark")
  :group 'elmo)

(defcustom elmo-msgdb-answered-uncached-mark "A"
  "Mark for answered but cached message."
  :type '(string :tag "Mark")
  :group 'elmo)

(defcustom elmo-msgdb-important-mark"$"
  "Mark for important message."
  :type '(string :tag "Mark")
  :group 'elmo)

;;; MSGDB interface.
;;
;; MSGDB elmo-load-msgdb PATH
;; MARK elmo-msgdb-get-mark MSGDB NUMBER 

;; CACHED elmo-msgdb-get-cached MSGDB NUMBER
;; VOID elmo-msgdb-set-cached MSGDB NUMBER CACHED USE-CACHE
;; VOID elmo-msgdb-set-flag MSGDB FOLDER NUMBER FLAG
;; VOID elmo-msgdb-unset-flag MSGDB FOLDER NUMBER FLAG

;; LIST-OF-NUMBERS elmo-msgdb-count-marks MSGDB
;; NUMBER elmo-msgdb-get-number MSGDB MESSAGE-ID
;; FIELD-VALUE elmo-msgdb-get-field MSGDB NUMBER FIELD
;; MSGDB elmo-msgdb-append MSGDB MSGDB-APPEND
;; MSGDB elmo-msgdb-clear MSGDB
;; elmo-msgdb-delete-msgs MSGDB NUMBERS
;; elmo-msgdb-sort-by-date MSGDB

;; elmo-flag-table-load
;; elmo-flag-table-set
;; elmo-flag-table-get
;; elmo-flag-table-save

;; elmo-msgdb-append-entity-from-buffer
;; msgdb number flag-table &optional buffer

;; 

;; ENTITY elmo-msgdb-make-entity ARGS
;; VALUE elmo-msgdb-entity-field ENTITY
;; 

;; OVERVIEW elmo-msgdb-get-overview MSGDB
;; NUMBER-ALIST elmo-msgdb-get-number-alist MSGDB
;; MARK-ALIST elmo-msgdb-get-mark-alist MSGDB
;; elmo-msgdb-change-mark MSGDB BEFORE AFTER

;; (for internal use?)
;; LIST-OF-MARKS elmo-msgdb-unread-marks 
;; LIST-OF-MARKS elmo-msgdb-answered-marks 
;; LIST-OF-MARKS elmo-msgdb-uncached-marks 
;; elmo-msgdb-seen-save DIR OBJ
;; elmo-msgdb-overview-save DIR OBJ

;; elmo-msgdb-overview-entity-get-references ENTITY
;; elmo-msgdb-overview-entity-set-references ENTITY
;; elmo-msgdb-get-parent-entity ENTITY MSGDB
;; elmo-msgdb-overview-enitty-get-number ENTITY
;; elmo-msgdb-overview-enitty-get-from-no-decode ENTITY
;; elmo-msgdb-overview-enitty-get-from ENTITY
;; elmo-msgdb-overview-enitty-get-subject-no-decode ENTITY
;; elmo-msgdb-overview-enitty-get-subject ENTITY
;; elmo-msgdb-overview-enitty-get-date ENTITY
;; elmo-msgdb-overview-enitty-get-to ENTITY
;; elmo-msgdb-overview-enitty-get-cc ENTITY
;; elmo-msgdb-overview-enitty-get-size ENTITY
;; elmo-msgdb-overview-enitty-get-id ENTITY
;; elmo-msgdb-overview-enitty-get-extra-field ENTITY
;; elmo-msgdb-overview-enitty-get-extra ENTITY
;; elmo-msgdb-overview-get-entity ID MSGDB

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
;; elmo-msgdb-copy-overview-entity ENTITY
;; elmo-msgdb-create-overview-entity-from-file NUMBER FILE
;; elmo-msgdb-overview-sort-by-date OVERVIEW
;; elmo-msgdb-clear-index

;; elmo-folder-get-info
;; elmo-folder-get-info-max
;; elmo-folder-get-info-length
;; elmo-folder-get-info-unread

;; elmo-msgdb-list-flagged MSGDB FLAG
;; (MACRO) elmo-msgdb-do-each-entity 

(defun elmo-load-msgdb (path)
  "Load the MSGDB from PATH."
  (let ((inhibit-quit t))
    (elmo-make-msgdb (elmo-msgdb-overview-load path)
		     (elmo-msgdb-number-load path)
		     (elmo-msgdb-mark-load path)
		     path)))

(defun elmo-make-msgdb (&optional overview number-alist mark-alist path)
  "Make a MSGDB."
  (let ((msgdb (list overview number-alist mark-alist nil path)))
    (elmo-msgdb-make-index msgdb)
    msgdb))

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
    ;; return value.
    t))

(defun elmo-msgdb-get-cached (msgdb number)
  "Return non-nil if message is cached."
  (not (member (elmo-msgdb-get-mark msgdb number)
	       (elmo-msgdb-uncached-marks))))

(defun elmo-msgdb-set-cached (msgdb number cached use-cache)
  "Set message cache status.
If mark is changed, return non-nil."
  (let* ((cur-mark (elmo-msgdb-get-mark msgdb number))
	 (cur-flag (cond
		      ((string= cur-mark elmo-msgdb-important-mark)
		       'important)
		      ((member cur-mark (elmo-msgdb-answered-marks))
		       'answered)
		      ((not (member cur-mark (elmo-msgdb-unread-marks)))
		       'read)))
	 (cur-cached (elmo-file-cache-exists-p
		      (elmo-msgdb-get-field msgdb number 'message-id))))
    (unless (eq cached cur-cached)
      (case cur-flag
	(read
	 (elmo-msgdb-set-mark msgdb number
			      (if (and use-cache (not cached))
				  elmo-msgdb-read-uncached-mark)))
	(important nil)
	(answered
	 (elmo-msgdb-set-mark msgdb number
			      (if cached
				  elmo-msgdb-answered-cached-mark
				elmo-msgdb-answered-uncached-mark)))
	(t
	 (elmo-msgdb-set-mark msgdb number
			      (if cached
				  elmo-msgdb-unread-cached-mark
				elmo-msgdb-unread-uncached-mark)))))))

(defun elmo-msgdb-set-flag (msgdb folder number flag)
  "Set message flag.
MSGDB is the ELMO msgdb.
FOLDER is a ELMO folder structure.
NUMBER is a message number to set flag.
FLAG is a symbol which is one of the following:
`read'      ... Messages which are already read.
`important' ... Messages which are marked as important.
`answered'  ... Messages which are marked as answered."
  (let* ((cur-mark (elmo-msgdb-get-mark msgdb number))
	 (use-cache (elmo-message-use-cache-p folder number))
	 (cur-flag (cond
		    ((string= cur-mark elmo-msgdb-important-mark)
		     'important)
		    ((member cur-mark (elmo-msgdb-answered-marks))
		     'answered)
		    ((not (member cur-mark (elmo-msgdb-unread-marks)))
		     'read)))
	 (cur-cached (elmo-file-cache-exists-p
		      (elmo-msgdb-get-field msgdb number 'message-id)))
	 mark-modified)
    (case flag
      (read
       (case cur-flag
	 ((read important answered))
	 (t (elmo-msgdb-set-mark msgdb number
				 (if (and use-cache (not cur-cached))
				     elmo-msgdb-read-uncached-mark))
	    (setq mark-modified t))))
      (important
       (unless (eq cur-flag 'important)
	 (elmo-msgdb-set-mark msgdb number elmo-msgdb-important-mark)
	 (setq mark-modified t)))
      (answered
       (unless (or (eq cur-flag 'answered) (eq cur-flag 'important))
	 (elmo-msgdb-set-mark msgdb number
			      (if cur-cached
				  elmo-msgdb-answered-cached-mark
				elmo-msgdb-answered-uncached-mark)))
       (setq mark-modified t)))
    (if mark-modified (elmo-folder-set-mark-modified-internal folder t))))

(defun elmo-msgdb-unset-flag (msgdb folder number flag)
  "Unset message flag.
MSGDB is the ELMO msgdb.
FOLDER is a ELMO folder structure.
NUMBER is a message number to be set flag.
FLAG is a symbol which is one of the following:
`read'      ... Messages which are already read.
`important' ... Messages which are marked as important.
`answered'  ... Messages which are marked as answered."
  (let* ((cur-mark (elmo-msgdb-get-mark msgdb number))
	 (use-cache (elmo-message-use-cache-p folder number))
	 (cur-flag (cond
		    ((string= cur-mark elmo-msgdb-important-mark)
		     'important)
		    ((member cur-mark (elmo-msgdb-answered-marks))
		     'answered)
		    ((not (member cur-mark (elmo-msgdb-unread-marks)))
		     'read)))
	 (cur-cached (elmo-file-cache-exists-p
		      (elmo-msgdb-get-field msgdb number 'message-id)))
	 mark-modified)
    (case flag
      (read
       (when (eq cur-flag 'read)
	 (elmo-msgdb-set-mark msgdb number
			      (if cur-cached
				  elmo-msgdb-unread-cached-mark
				elmo-msgdb-unread-uncached-mark))
	 (setq mark-modified t)))
      (important
       (when (eq cur-flag 'important)
	 (elmo-msgdb-set-mark msgdb number nil)
	 (setq mark-modified t)))
      (answered
       (when (eq cur-flag 'answered)
	 (elmo-msgdb-set-mark msgdb number
			      (if (and use-cache (not cur-cached))
				  elmo-msgdb-read-uncached-mark))
	 (setq mark-modified t))))
    (if mark-modified (elmo-folder-set-mark-modified-internal folder t))))

(defvar elmo-msgdb-unread-marks-internal nil)
(defsubst elmo-msgdb-unread-marks ()
  "Return an unread mark list"
  (or elmo-msgdb-unread-marks-internal
      (setq elmo-msgdb-unread-marks-internal
	    (list elmo-msgdb-new-mark
		  elmo-msgdb-unread-uncached-mark
		  elmo-msgdb-unread-cached-mark))))

(defvar elmo-msgdb-answered-marks-internal nil)
(defsubst elmo-msgdb-answered-marks ()
  "Return an answered mark list"
  (or elmo-msgdb-answered-marks-internal
      (setq elmo-msgdb-answered-marks-internal
	    (list elmo-msgdb-answered-cached-mark
		  elmo-msgdb-answered-uncached-mark))))

(defvar elmo-msgdb-uncached-marks-internal nil)
(defsubst elmo-msgdb-uncached-marks ()
  (or elmo-msgdb-uncached-marks-internal
      (setq elmo-msgdb-uncached-marks-internal
	    (list elmo-msgdb-new-mark
		  elmo-msgdb-answered-uncached-mark
		  elmo-msgdb-unread-uncached-mark
		  elmo-msgdb-read-uncached-mark))))

(defsubst elmo-msgdb-count-marks (msgdb)
  (let ((new 0)
	(unreads 0)
	(answered 0))
    (dolist (elem (elmo-msgdb-get-mark-alist msgdb))
      (cond
       ((string= (cadr elem) elmo-msgdb-new-mark)
	(incf new))
       ((member (cadr elem) (elmo-msgdb-unread-marks))
	(incf unreads))
       ((member (cadr elem) (elmo-msgdb-answered-marks))
	(incf answered))))
    (list new unreads answered)))

(defsubst elmo-msgdb-get-number (msgdb message-id)
  "Get number of the message which corrensponds to MESSAGE-ID from MSGDB."
  (elmo-msgdb-overview-entity-get-number
   (elmo-msgdb-overview-get-entity message-id msgdb)))

(defsubst elmo-msgdb-get-field (msgdb number field)
  "Get FIELD value of the message with NUMBER from MSGDB."
  (case field
    (message-id (elmo-msgdb-overview-entity-get-id
		 (elmo-msgdb-overview-get-entity
		  number msgdb)))
    (subject (elmo-msgdb-overview-entity-get-subject
	      (elmo-msgdb-overview-get-entity
	       number msgdb)))
    (size (elmo-msgdb-overview-entity-get-size
	   (elmo-msgdb-overview-get-entity
	    number msgdb)))
    (date (elmo-msgdb-overview-entity-get-date
	   (elmo-msgdb-overview-get-entity
	    number msgdb)))
    (to (elmo-msgdb-overview-entity-get-to
	 (elmo-msgdb-overview-get-entity
	  number msgdb)))
    (cc (elmo-msgdb-overview-entity-get-cc
	 (elmo-msgdb-overview-get-entity
	  number msgdb)))))

(defsubst elmo-msgdb-append (msgdb msgdb-append)
  (list
   (nconc (car msgdb) (car msgdb-append))
   (nconc (cadr msgdb) (cadr msgdb-append))
   (nconc (caddr msgdb) (caddr msgdb-append))
   (elmo-msgdb-make-index
    msgdb
    (elmo-msgdb-get-overview msgdb-append)
    (elmo-msgdb-get-mark-alist msgdb-append))
   (nth 4 msgdb)))

(defsubst elmo-msgdb-clear (&optional msgdb)
  (if msgdb
      (list
       (setcar msgdb nil)
       (setcar (cdr msgdb) nil)
       (setcar (cddr msgdb) nil)
       (setcar (nthcdr 3 msgdb) nil)
       (setcar (nthcdr 4 msgdb) nil))
    (list nil nil nil nil nil)))

(defun elmo-msgdb-delete-msgs (msgdb msgs)
  "Delete MSGS from MSGDB
content of MSGDB is changed."
  (let* ((overview (car msgdb))
	 (number-alist (cadr msgdb))
	 (mark-alist (caddr msgdb))
	 (index (elmo-msgdb-get-index msgdb))
	 (newmsgdb (list overview number-alist mark-alist index
			 (nth 4 msgdb)))
	 ov-entity)
    ;; remove from current database.
    (while msgs
      (setq overview
	    (delq
	     (setq ov-entity
		   (elmo-msgdb-overview-get-entity (car msgs) newmsgdb))
	     overview))
      (setq number-alist (delq (assq (car msgs) number-alist) number-alist))
      (setq mark-alist (delq (assq (car msgs) mark-alist) mark-alist))
      ;;
      (when index (elmo-msgdb-clear-index msgdb ov-entity))
      (setq msgs (cdr msgs)))
    (setcar msgdb overview)
    (setcar (cdr msgdb) number-alist)
    (setcar (cddr msgdb) mark-alist)
    (setcar (nthcdr 3 msgdb) index)
    t)) ;return value

(defun elmo-msgdb-sort-by-date (msgdb)
  (message "Sorting...")
  (let ((overview (elmo-msgdb-get-overview msgdb)))
    (setq overview (elmo-msgdb-overview-sort-by-date overview))
    (message "Sorting...done")
    (list overview (nth 1 msgdb)(nth 2 msgdb))))

(defun elmo-msgdb-make-entity (&rest args)
  "Make an msgdb entity."
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

;;;
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

(defsubst elmo-msgdb-get-index (msgdb)
  (nth 3 msgdb))

(defsubst elmo-msgdb-get-entity-hashtb (msgdb)
  (car (nth 3 msgdb)))

(defsubst elmo-msgdb-get-mark-hashtb (msgdb)
  (cdr (nth 3 msgdb)))

(defsubst elmo-msgdb-get-path (msgdb)
  (nth 4 msgdb))

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
	seen-list)
    (when (file-exists-p seen-file)
      (setq seen-list (elmo-object-load seen-file))
      (delete-file seen-file))
    (dolist (msgid seen-list)
      (elmo-set-hash-val msgid 'read table))
    (dolist (pair (elmo-object-load
		   (expand-file-name elmo-flag-table-filename dir)))
      (elmo-set-hash-val (car pair) (cdr pair) table))
    table))

(defun elmo-flag-table-set (flag-table msg-id flag)
  (elmo-set-hash-val msg-id flag flag-table))

(defun elmo-flag-table-get (flag-table msg-id)
  (elmo-get-hash-val msg-id flag-table))

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

(defsubst elmo-msgdb-length (msgdb)
  (length (elmo-msgdb-get-overview msgdb)))

(defun elmo-msgdb-flag-table (msgdb &optional flag-table)
  ;; Make a table of msgid flag (read, answered)
  (let ((flag-table (or flag-table (elmo-make-hash (elmo-msgdb-length msgdb))))
	mark)
    (dolist (ov (elmo-msgdb-get-overview msgdb))
      (setq mark (elmo-msgdb-get-mark
		  msgdb
		  (elmo-msgdb-overview-entity-get-number ov)))
      (cond 
       ((null mark)
	(elmo-set-hash-val
	 (elmo-msgdb-overview-entity-get-id ov)
	 'read
	 flag-table))
       ((and mark (member mark (elmo-msgdb-answered-marks)))
	(elmo-set-hash-val
	 (elmo-msgdb-overview-entity-get-id ov)
	 'answered
	 flag-table))
       ((and mark (not (member mark
			       (elmo-msgdb-unread-marks))))
	(elmo-set-hash-val
	 (elmo-msgdb-overview-entity-get-id ov)
	 'read
	 flag-table))))
    flag-table))

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

(defsubst elmo-msgdb-mark (flag cached &optional new)
  (if new
      (case flag
	(read
	 (if cached
	     nil
	   elmo-msgdb-read-uncached-mark))
	(important
	 elmo-msgdb-important-mark)
	(answered
	 (if cached
	     elmo-msgdb-answered-cached-mark
	   elmo-msgdb-answered-uncached-mark))
	(t
	 (if cached
	     elmo-msgdb-unread-cached-mark
	   elmo-msgdb-new-mark)))
    (case flag
      (unread
       (if cached
	   elmo-msgdb-unread-cached-mark
	 elmo-msgdb-unread-uncached-mark))
      (important
       elmo-msgdb-important-mark)
      (answered
       (if cached
	   elmo-msgdb-answered-cached-mark
	 elmo-msgdb-answered-uncached-mark)))))

(defsubst elmo-msgdb-seen-save (dir obj)
  (elmo-object-save
   (expand-file-name elmo-msgdb-seen-filename dir)
   obj))

(defsubst elmo-msgdb-overview-save (dir overview)
  (elmo-object-save
   (expand-file-name elmo-msgdb-overview-filename dir)
   overview))

(defun elmo-msgdb-match-condition-primitive (condition mark entity numbers)
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
		(not (or (null mark)
			 (string= mark elmo-msgdb-read-uncached-mark))))
	       ((string= (elmo-filter-value condition) "digest")
		(not (or (null mark)
			 (string= mark elmo-msgdb-read-uncached-mark)
			 (string= mark elmo-msgdb-answered-cached-mark)
			 (string= mark elmo-msgdb-answered-uncached-mark))))
;;	  (member mark (append (elmo-msgdb-answered-marks)
;;			       (list elmo-msgdb-important-mark)
;;			       (elmo-msgdb-unread-marks))))
	       ((string= (elmo-filter-value condition) "unread")
		(member mark (elmo-msgdb-unread-marks)))
	       ((string= (elmo-filter-value condition) "important")
		(string= mark elmo-msgdb-important-mark))
	       ((string= (elmo-filter-value condition) "answered")
		(member mark (elmo-msgdb-answered-marks))))))
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

(defun elmo-msgdb-match-condition-internal (condition mark entity numbers)
  (cond
   ((vectorp condition)
    (elmo-msgdb-match-condition-primitive condition mark entity numbers))
   ((eq (car condition) 'and)
    (let ((lhs (elmo-msgdb-match-condition-internal (nth 1 condition)
						    mark entity numbers)))
      (cond
       ((elmo-filter-condition-p lhs)
	(let ((rhs (elmo-msgdb-match-condition-internal
		    (nth 2 condition) mark entity numbers)))
	  (cond ((elmo-filter-condition-p rhs)
		 (list 'and lhs rhs))
		(rhs
		 lhs))))
       (lhs
	(elmo-msgdb-match-condition-internal (nth 2 condition)
					     mark entity numbers)))))
   ((eq (car condition) 'or)
    (let ((lhs (elmo-msgdb-match-condition-internal (nth 1 condition)
						    mark entity numbers)))
      (cond
       ((elmo-filter-condition-p lhs)
	(let ((rhs (elmo-msgdb-match-condition-internal (nth 2 condition)
							mark entity numbers)))
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
					     mark entity numbers)))))))

(defun elmo-msgdb-match-condition (msgdb condition number numbers)
  "Check whether the condition of the message is satisfied or not.
MSGDB is the msgdb to search from.
CONDITION is the search condition.
NUMBER is the message number to check.
NUMBERS is the target message number list.
Return CONDITION itself if no entity exists in msgdb."
  (let ((entity (elmo-msgdb-overview-get-entity number msgdb)))
    (if entity
	(elmo-msgdb-match-condition-internal condition
					     (elmo-msgdb-get-mark msgdb number)
					     entity numbers)
      condition)))

(defsubst elmo-msgdb-set-overview (msgdb overview)
  (setcar msgdb overview))

(defsubst elmo-msgdb-set-number-alist (msgdb number-alist)
  (setcar (cdr msgdb) number-alist))

(defsubst elmo-msgdb-set-mark-alist (msgdb mark-alist)
  (setcar (cddr msgdb) mark-alist))

(defsubst elmo-msgdb-set-index (msgdb index)
  (setcar (cdddr msgdb) index))

(defsubst elmo-msgdb-set-path (msgdb path)
  (setcar (cddddr msgdb) path))

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

(defsubst elmo-msgdb-get-parent-entity (entity msgdb)
  (setq entity (elmo-msgdb-overview-entity-get-references entity))
  ;; entity is parent-id.
  (and entity (elmo-msgdb-overview-get-entity entity msgdb)))

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

(defun elmo-msgdb-overview-get-entity (id msgdb)
  (when id
    (let ((ht (elmo-msgdb-get-entity-hashtb msgdb)))
      (if ht
	  (if (stringp id) ;; ID is message-id
	      (elmo-get-hash-val id ht)
	    (elmo-get-hash-val (format "#%d" id) ht))))))

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

(defun elmo-msgdb-make-index (msgdb &optional overview mark-alist)
  "Append OVERVIEW and MARK-ALIST to the index of MSGDB.
If OVERVIEW and MARK-ALIST are nil, make index for current MSGDB.
Return the updated INDEX."
  (when msgdb
    (let* ((overview (or overview (elmo-msgdb-get-overview msgdb)))
	   (mark-alist (or mark-alist (elmo-msgdb-get-mark-alist msgdb)))
	   (index (elmo-msgdb-get-index msgdb))
	   (ehash (or (car index) ;; append
		      (elmo-make-hash (length overview))))
	   (mhash (or (cdr index) ;; append
		      (elmo-make-hash (length overview)))))
      (while overview
	;; key is message-id
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
      index)))

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

(defun elmo-msgdb-list-flagged (msgdb flag)
  (let ((case-fold-search nil)
	mark-regexp matched)
    (case flag
      (new
       (setq mark-regexp (regexp-quote elmo-msgdb-new-mark)))
      (unread
       (setq mark-regexp (elmo-regexp-opt (elmo-msgdb-unread-marks))))
      (answered
       (setq mark-regexp (elmo-regexp-opt (elmo-msgdb-unread-marks))))
      (important
       (setq mark-regexp (regexp-quote elmo-msgdb-important-mark)))
      (read
       (setq mark-regexp (elmo-regexp-opt (elmo-msgdb-unread-marks))))
      (digest
       (setq mark-regexp (elmo-regexp-opt
			  (append (elmo-msgdb-unread-marks)
				  (list elmo-msgdb-important-mark)))))
      (any
       (setq mark-regexp (elmo-regexp-opt
			  (append
			   (elmo-msgdb-unread-marks)
			   (elmo-msgdb-answered-marks)
			   (list elmo-msgdb-important-mark))))))
    (when mark-regexp
      (if (eq flag 'read)
	  (dolist (number (elmo-msgdb-get-number-alist msgdb))
	    (unless (string-match mark-regexp (elmo-msgdb-get-mark
					       msgdb number))
	      (setq matched (cons number matched))))
	(dolist (elem (elmo-msgdb-get-mark-alist msgdb))
	  (if (string-match mark-regexp (cadr elem))
	      (setq matched (cons (car elem) matched))))))
    matched))

(put 'elmo-msgdb-do-each-entity 'lisp-indent-function '1)
(def-edebug-spec elmo-msgdb-do-each-entity
  ((symbolp form &rest form) &rest form))
(defmacro elmo-msgdb-do-each-entity (spec &rest form)
  `(dolist (,(car spec) (elmo-msgdb-get-overview ,(car (cdr spec))))
     ,@form))

(require 'product)
(product-provide (provide 'elmo-msgdb) (require 'elmo-version))

;;; elmo-msgdb.el ends here
