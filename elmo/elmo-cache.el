;;; elmo-cache.el -- Cache modules for Elmo.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000 Kenichi OKADA <okada@opaopa.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Kenichi OKADA <okada@opaopa.org>
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
(require 'elmo-vars)
(require 'elmo-util)

(defsubst elmo-cache-to-msgid (filename)
  (concat "<" (elmo-recover-string-from-filename filename) ">"))

;;; File cache.

(defun elmo-file-cache-get-path (msgid &optional section)
  "Get cache path for MSGID.
If optional argument SECTION is specified, partial cache path is returned."
  (if (setq msgid (elmo-msgid-to-cache msgid))
      (expand-file-name
       (if section
	   (format "%s/%s/%s/%s/%s"
		   elmo-msgdb-dir
		   elmo-cache-dirname
		   (elmo-cache-get-path-subr msgid)
		   msgid
		   section)
	 (format "%s/%s/%s/%s"
		 elmo-msgdb-dir
		 elmo-cache-dirname
		 (elmo-cache-get-path-subr msgid)
		 msgid)))))

(defmacro elmo-file-cache-expand-path (path section)
  "Return file name for the file-cache corresponds to the section.
PATH is the file-cache path.
SECTION is the section string."
  (` (expand-file-name (or (, section) "") (, path))))

(defun elmo-file-cache-delete (path)
  "Delete a cache on PATH."
  (let (files)
    (when (file-exists-p path)
      (if (file-directory-p path)
	  (progn
	    (setq files (directory-files path t "^[^\\.]"))
	    (while files
	      (delete-file (car files))
	      (setq files (cdr files)))
	    (delete-directory path))
	(delete-file path)))))

(defun elmo-file-cache-exists-p (msgid)
  "Returns 'section or 'entire if a cache which corresponds to MSGID exists."
  (elmo-file-cache-status (elmo-file-cache-get msgid)))

(defun elmo-file-cache-save (cache-path section)
  "Save current buffer as cache on PATH."
  (let ((path (if section (expand-file-name section cache-path) cache-path))
	files dir)
    (if (and (null section)
	     (file-directory-p path))
	(progn
	  (setq files (directory-files path t "^[^\\.]"))
	  (while files
	    (delete-file (car files))
	    (setq files (cdr files)))
	  (delete-directory path))
      (if (and section
	       (not (file-directory-p cache-path)))
	  (delete-file cache-path)))
    (when path
      (setq dir (directory-file-name (file-name-directory path)))
      (if (not (file-exists-p dir))
	  (elmo-make-directory dir))
      (write-region-as-binary (point-min) (point-max)
			      path nil 'no-msg))))

(defmacro elmo-make-file-cache (path status)
  "PATH is the cache file name.
STATUS is one of 'section, 'entire or nil.
 nil means no cache exists.
'section means partial section cache exists.
'entire means entire cache exists.
If the cache is partial file-cache, TYPE is 'partial."
  (` (cons (, path) (, status))))

(defmacro elmo-file-cache-path (file-cache)
  "Returns the file path of the FILE-CACHE."
  (` (car (, file-cache))))

(defmacro elmo-file-cache-status (file-cache)
  "Returns the status of the FILE-CACHE."
  (` (cdr (, file-cache))))

(defun elmo-file-cache-get (msgid &optional section)
  "Returns the current file-cache object associated with MSGID.
MSGID is the message-id of the message.
If optional argument SECTION is specified, get partial file-cache object
associated with SECTION."
  (if msgid
      (let ((path (elmo-cache-get-path msgid)))
	(if (and path (file-exists-p path))
	    (if (file-directory-p path)
		(if section
		    (if (file-exists-p (setq path (expand-file-name
						   section path)))
			(cons path 'section))
		  ;; section is not specified but sectional.
		  (cons path 'section))
	      ;; not directory.
	      (unless section
		(cons path 'entire)))
	  ;; no cache.
	  (cons path nil)))))

;;;
(defun elmo-cache-expire ()
  (interactive)
  (let* ((completion-ignore-case t)
	 (method (completing-read (format "Expire by (%s): "
					  elmo-cache-expire-default-method)
				  '(("size" . "size")
				    ("age" . "age")))))
    (if (string= method "")
	(setq method elmo-cache-expire-default-method))
    (funcall (intern (concat "elmo-cache-expire-by-" method)))))

(defun elmo-read-float-value-from-minibuffer (prompt &optional initial)
  (let ((str (read-from-minibuffer prompt initial)))
    (cond
     ((string-match "[0-9]*\\.[0-9]+" str)
      (string-to-number str))
     ((string-match "[0-9]+" str)
      (string-to-number (concat str ".0")))
     (t (error "%s is not number" str)))))

(defun elmo-cache-expire-by-size (&optional kbytes)
  "Expire cache file by size.
If KBYTES is kilo bytes (This value must be float)."
  (interactive)
  (let ((size (or kbytes
		  (and (interactive-p)
		       (elmo-read-float-value-from-minibuffer
			"Enter cache disk size (Kbytes): "
			(number-to-string
			 (if (integerp elmo-cache-expire-default-size)
			     (float elmo-cache-expire-default-size)
			   elmo-cache-expire-default-size))))
		  (if (integerp elmo-cache-expire-default-size)
		      (float elmo-cache-expire-default-size))))
	(locked (elmo-dop-lock-list-load))
	(count 0)
	(Kbytes 1024)
	total beginning)
    (message "Checking disk usage...")
    (setq total (/ (elmo-disk-usage
		    (expand-file-name
		     elmo-cache-dirname elmo-msgdb-dir)) Kbytes))
    (setq beginning total)
    (message "Checking disk usage...done")
    (let ((cfl (elmo-cache-get-sorted-cache-file-list))
	  (deleted 0)
	  oldest
	  cur-size cur-file)
      (while (and (<= size total)
		  (setq oldest (elmo-cache-get-oldest-cache-file-entity cfl)))
	(setq cur-file (expand-file-name (car (cdr oldest)) (car oldest)))
	(setq cur-size (/ (elmo-disk-usage cur-file) Kbytes))
	(when (elmo-cache-force-delete cur-file locked)
	  (setq count (+ count 1))
	  (message "%d cache(s) are expired." count))
	(setq deleted (+ deleted cur-size))
	(setq total (- total cur-size)))
      (message "%d cache(s) are expired from disk (%d Kbytes/%d Kbytes)."
	       count deleted beginning))))

(defun elmo-cache-make-file-entity (filename path)
  (cons filename (elmo-get-last-accessed-time filename path)))

(defun elmo-cache-get-oldest-cache-file-entity (cache-file-list)
  (let ((cfl cache-file-list)
	flist firsts oldest-entity wonlist)
    (while cfl
      (setq flist (cdr (car cfl)))
      (setq firsts (append firsts (list
				   (cons (car (car cfl))
					 (car flist)))))
      (setq cfl (cdr cfl)))
;;; (prin1 firsts)
    (while firsts
      (if (and (not oldest-entity)
	       (cdr (cdr (car firsts))))
	  (setq oldest-entity (car firsts)))
      (if (and (cdr (cdr (car firsts)))
	       (cdr (cdr oldest-entity))
	       (> (cdr (cdr oldest-entity)) (cdr (cdr (car firsts)))))
	  (setq oldest-entity (car firsts)))
      (setq firsts (cdr firsts)))
    (setq wonlist (assoc (car oldest-entity) cache-file-list))
    (and wonlist
	 (setcdr wonlist (delete (car (cdr wonlist)) (cdr wonlist))))
    oldest-entity))

(defun elmo-cache-get-sorted-cache-file-list ()
  (let ((dirs (directory-files
	       (expand-file-name elmo-cache-dirname elmo-msgdb-dir)
	       t "^[^\\.]"))
	(i 0) num
	elist
	ret-val)
    (setq num (length dirs))
    (message "Collecting cache info...")
    (while dirs
      (setq elist (mapcar (lambda (x)
			    (elmo-cache-make-file-entity x (car dirs)))
			  (directory-files (car dirs) nil "^[^\\.]")))
      (setq ret-val (append ret-val
			    (list (cons
				   (car dirs)
				   (sort
				    elist
				    (lambda (x y)
				      (< (cdr x)
					 (cdr y))))))))
      (when (> num elmo-display-progress-threshold)
	(setq i (+ i 1))
	(elmo-display-progress
	 'elmo-cache-get-sorted-cache-file-list "Collecting cache info..."
	 (/ (* i 100) num)))
      (setq dirs (cdr dirs)))
    (message "Collecting cache info...done")
    ret-val))

(defun elmo-cache-expire-by-age (&optional days)
  (let ((age (or (and days (int-to-string days))
		 (and (interactive-p)
		      (read-from-minibuffer
		       (format "Enter days (%s): "
			       elmo-cache-expire-default-age)))
		 (int-to-string elmo-cache-expire-default-age)))
	(dirs (directory-files
	       (expand-file-name elmo-cache-dirname elmo-msgdb-dir)
	       t "^[^\\.]"))
	(locked (elmo-dop-lock-list-load))
	(count 0)
	curtime)
    (if (string= age "")
	(setq age elmo-cache-expire-default-age)
      (setq age (string-to-int age)))
    (setq curtime (current-time))
    (setq curtime (+ (* (nth 0 curtime)
			(float 65536)) (nth 1 curtime)))
    (while dirs
      (let ((files (directory-files (car dirs) t "^[^\\.]"))
	    (limit-age (* age 86400)))
	(while files
	  (when (> (- curtime (elmo-get-last-accessed-time (car files)))
		   limit-age)
	    (when (elmo-cache-force-delete (car files) locked)
	      (setq count (+ 1 count))
	      (message "%d cache file(s) are expired." count)))
	  (setq files (cdr files))))
      (setq dirs (cdr dirs)))))

(defun elmo-cache-search-all (folder condition from-msgs)
  (let* ((number-alist (elmo-msgdb-number-load
			(elmo-msgdb-expand-path folder)))
	 (number-list (or from-msgs (mapcar 'car number-alist)))
	 (num (length number-alist))
	 cache-file
	 ret-val
	 case-fold-search msg
	 percent i)
    (setq i 0)
    (while number-alist
      (if (and (memq (car (car number-alist)) number-list)
	       (setq cache-file (elmo-cache-exists-p (cdr (car
							   number-alist))
						     folder
						     (car (car
							   number-alist))))
	       (elmo-file-field-condition-match cache-file condition
						(car (car number-alist))
						number-list))
	  (setq ret-val (append ret-val (list (caar number-alist)))))
      (when (> num elmo-display-progress-threshold)
	(setq i (1+ i))
	(setq percent (/ (* i 100) num))
	(elmo-display-progress
	 'elmo-cache-search-all "Searching..."
	 percent))
      (setq number-alist (cdr number-alist)))
    ret-val))

(defun elmo-cache-collect-sub-directories (init dir &optional recursively)
  "Collect subdirectories under DIR."
  (let ((dirs
	 (delete (expand-file-name elmo-cache-dirname
				   elmo-msgdb-dir)
		 (directory-files dir t "^[^\\.]")))
	ret-val)
    (setq dirs (elmo-delete-if (lambda (x) (not (file-directory-p x))) dirs))
    (setq ret-val (append init dirs))
    (while (and recursively dirs)
      (setq ret-val
	    (elmo-cache-collect-sub-directories
	     ret-val
	     (car dirs) recursively))
      (setq dirs (cdr dirs)))
    ret-val))

(defun elmo-msgid-to-cache (msgid)
  (when (and msgid
	     (string-match "<\\(.+\\)>$" msgid))
    (elmo-replace-string-as-filename (elmo-match-string 1 msgid))))

(defun elmo-cache-get-path (msgid &optional folder number)
  "Get path for cache file associated with MSGID, FOLDER, and NUMBER."
  (if (setq msgid (elmo-msgid-to-cache msgid))
      (expand-file-name
       (expand-file-name
	(if folder
	    (format "%s/%s/%s@%s"
		    (elmo-cache-get-path-subr msgid)
		    msgid
		    (or number "")
		    (elmo-safe-filename folder))
	  (format "%s/%s"
		  (elmo-cache-get-path-subr msgid)
		  msgid))
	(expand-file-name elmo-cache-dirname
			  elmo-msgdb-dir)))))

(defsubst elmo-cache-get-path-subr (msgid)
  (let ((chars '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?A ?B ?C ?D ?E ?F))
	(clist (string-to-char-list msgid))
	(sum 0))
    (while clist
      (setq sum (+ sum (car clist)))
      (setq clist (cdr clist)))
    (format "%c%c"
	    (nth (% (/ sum 16) 2) chars)
	    (nth (% sum 16) chars))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;
;; cache backend by Kenichi OKADA <okada@opaopa.org>
;;

(defsubst elmo-cache-get-folder-directory (spec)
  (if (file-name-absolute-p (nth 1 spec))
      (nth 1 spec) ; already full path.
    (expand-file-name (nth 1 spec)
		      (expand-file-name elmo-cache-dirname elmo-msgdb-dir))))

(defun elmo-cache-msgdb-expand-path (spec)
  (let ((fld-name (nth 1 spec)))
    (expand-file-name fld-name
		      (expand-file-name "internal/cache"
					elmo-msgdb-dir))))

(defun elmo-cache-number-to-filename (spec number)
  (let ((number-alist
	 (elmo-cache-list-folder-subr spec nil t)))
    (elmo-msgid-to-cache
     (cdr (assq number number-alist)))))

(defsubst elmo-cache-msgdb-create-overview-entity-from-file (number file)
  (save-excursion
    (let ((tmp-buffer (get-buffer-create " *ELMO Cache Temp*"))
	  insert-file-contents-pre-hook   ; To avoid autoconv-xmas...
	  insert-file-contents-post-hook header-end
	  (attrib (file-attributes file))
	  ret-val size mtime)
      (set-buffer tmp-buffer)
      (erase-buffer)
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
	  (setq ret-val (elmo-msgdb-create-overview-from-buffer number size mtime))
	  (kill-buffer tmp-buffer))
	ret-val))))

(defun elmo-cache-msgdb-create-as-numlist (spec numlist new-mark
						   already-mark seen-mark
						   important-mark seen-list)
  (when numlist
    (let ((dir (elmo-cache-get-folder-directory spec))
	  (nalist (elmo-cache-list-folder-subr spec nil t))
	  overview number-alist mark-alist entity message-id
	  i percent len num seen gmark)
      (setq len (length numlist))
      (setq i 0)
      (message "Creating msgdb...")
      (while numlist
	(setq entity
	      (elmo-cache-msgdb-create-overview-entity-from-file
	       (car numlist)
	       (expand-file-name
		(elmo-msgid-to-cache
		 (setq message-id (cdr (assq (car numlist) nalist)))) dir)))
	(if (null entity)
	    ()
	  (setq num (elmo-msgdb-overview-entity-get-number entity))
	  (setq overview
		(elmo-msgdb-append-element
		 overview entity))
	  (setq number-alist
		(elmo-msgdb-number-add number-alist num message-id))
	  (setq seen (member message-id seen-list))
	  (if (setq gmark (or (elmo-msgdb-global-mark-get message-id)
			      (if seen
				  nil
				new-mark)))
	      (setq mark-alist
		    (elmo-msgdb-mark-append
		     mark-alist
		     num
		     gmark))))
	(when (> len elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (setq percent (/ (* i 100) len))
	  (elmo-display-progress
	   'elmo-cache-msgdb-create-as-numlist "Creating msgdb..."
	   percent))
	(setq numlist (cdr numlist)))
      (message "Creating msgdb...done")
      (list overview number-alist mark-alist))))

(defalias 'elmo-cache-msgdb-create 'elmo-cache-msgdb-create-as-numlist)

(defun elmo-cache-list-folders (spec &optional hierarchy)
  (let ((folder (concat "'cache" (nth 1 spec))))
    (elmo-cache-list-folders-subr folder hierarchy)))

(defun elmo-cache-list-folders-subr (folder &optional hierarchy)
  (let ((case-fold-search t)
	folders curdir dirent relpath abspath attr
	subprefix subfolder)
    (condition-case ()
	(progn
	  (setq curdir
		(expand-file-name
		 (nth 1 (elmo-folder-get-spec folder))
		 (expand-file-name elmo-cache-dirname elmo-msgdb-dir)))
	  (if (string-match "^[+=$!]$" folder) ; localdir, archive, localnews
	      (setq subprefix folder)
	    (setq subprefix (concat folder elmo-path-sep)))
	    ;; include parent
	    ;(setq folders (list folder)))
	  (setq dirent (directory-files curdir nil "^[01][0-9A-F]$"))
	  (catch 'done
	   (while dirent
	    (setq relpath (car dirent))
	    (setq dirent (cdr dirent))
	    (setq abspath (expand-file-name relpath curdir))
	    (and
	     (eq (nth 0 (setq attr (file-attributes abspath))) t)
	     (setq subfolder (concat subprefix relpath))
	     (setq folders (nconc folders (list subfolder))))))
	  folders)
      (file-error folders))))

(defsubst elmo-cache-list-folder-subr (spec &optional nonsort nonalist)
  (let* ((dir (elmo-cache-get-folder-directory spec))
	 (flist (mapcar 'file-name-nondirectory
			(elmo-delete-if 'file-directory-p
					(directory-files
					 dir t "^[^@]+@[^@]+$" t))))
	 (folder (concat "'cache/" (nth 1 spec)))
	 (number-alist (or (elmo-msgdb-number-load
			    (elmo-msgdb-expand-path folder))
			   (list nil)))
	 nlist)
    (setq nlist
	  (mapcar '(lambda (filename)
		     (elmo-cache-filename-to-number filename number-alist))
		  flist))
    (if nonalist
	number-alist
      (if nonsort
	  (cons (or (elmo-max-of-list nlist) 0) (length nlist))
	(sort nlist '<)))))

(defsubst elmo-cache-filename-to-number (filename number-alist)
  (let* ((msgid (elmo-cache-to-msgid filename))
	 number)
    (or (car (rassoc msgid number-alist))
	(prog1
	    (setq number (+ (or (caar (last number-alist))
				0) 1))
	  (if (car number-alist)
	      (nconc number-alist
		     (list (cons number msgid)))
	    (setcar number-alist (cons number msgid)))))))

(defun elmo-cache-append-msg (spec string message-id &optional msg no-see)
  (let ((dir (elmo-cache-get-folder-directory spec))
	(tmp-buffer (get-buffer-create " *ELMO Temp buffer*"))
	filename)
    (save-excursion
      (set-buffer tmp-buffer)
      (erase-buffer)
      (setq filename (expand-file-name (elmo-msgid-to-cache message-id) dir))
      (unwind-protect
	  (if (file-writable-p filename)
	      (progn
		(insert string)
		(as-binary-output-file
		 (write-region (point-min) (point-max) filename nil 'no-msg))
		t)
	    nil)
	(kill-buffer tmp-buffer)))))

(defun elmo-cache-delete-msg (spec number locked)
  (let* ((dir (elmo-cache-get-folder-directory spec))
	 (file (expand-file-name
		(elmo-cache-number-to-filename spec number) dir)))
    ;; return nil if failed.
    (elmo-cache-force-delete file locked)))

(defun elmo-cache-read-msg (spec number outbuf &optional set-mark)
  (save-excursion
    (let* ((dir (elmo-cache-get-folder-directory spec))
	   (file (expand-file-name
		  (elmo-cache-number-to-filename spec number) dir)))
      (set-buffer outbuf)
      (erase-buffer)
      (when (file-exists-p file)
	(as-binary-input-file (insert-file-contents file))
	(elmo-delete-cr-get-content-type)))))

(defun elmo-cache-delete-msgs (spec msgs)
  (let ((locked (elmo-dop-lock-list-load)))
    (not (memq nil
	       (mapcar '(lambda (msg) (elmo-cache-delete-msg spec msg locked))
		       msgs)))))

(defun elmo-cache-list-folder (spec)	; called by elmo-cache-search()
  (let ((killed (and elmo-use-killed-list
		     (elmo-msgdb-killed-list-load
		      (elmo-msgdb-expand-path spec))))
	numbers)
    (setq numbers (elmo-cache-list-folder-subr spec))
    (elmo-living-messages numbers killed)))

(defun elmo-cache-max-of-folder (spec)
  (elmo-cache-list-folder-subr spec t))

(defun elmo-cache-check-validity (spec validity-file)
  t)

(defun elmo-cache-sync-validity (spec validity-file)
  t)

(defun elmo-cache-folder-exists-p (spec)
  (file-directory-p (elmo-cache-get-folder-directory spec)))

(defun elmo-cache-folder-creatable-p (spec)
  nil)

(defun elmo-cache-create-folder (spec)
  nil)

(defun elmo-cache-search (spec condition &optional from-msgs)
  (let* ((number-alist (elmo-cache-list-folder-subr spec nil t))
	 (msgs (or from-msgs (mapcar 'car number-alist)))
	 (num (length msgs))
	 (i 0) case-fold-search ret-val)
    (while msgs
      (if (elmo-file-field-condition-match
	   (expand-file-name
	    (elmo-msgid-to-cache
	     (cdr (assq (car msgs) number-alist)))
	    (elmo-cache-get-folder-directory spec))
	   condition
	   (car msgs)
	   msgs)
	  (setq ret-val (cons (car msgs) ret-val)))
      (when (> num elmo-display-progress-threshold)
	(setq i (1+ i))
	(elmo-display-progress
	 'elmo-cache-search "Searching..."
	 (/ (* i 100) num)))
      (setq msgs (cdr msgs)))
    (nreverse ret-val)))

;;; (localdir, maildir, localnews) -> cache
(defun elmo-cache-copy-msgs (dst-spec msgs src-spec
				      &optional loc-alist same-number)
  (let ((dst-dir
	 (elmo-cache-get-folder-directory dst-spec))
	(next-num (1+ (car (elmo-cache-list-folder-subr dst-spec t))))
	(number-alist
	 (elmo-msgdb-number-load
	  (elmo-msgdb-expand-path src-spec))))
    (if same-number (error "Not implemented"))
    (while msgs
      (elmo-copy-file
       ;; src file
       (elmo-call-func src-spec "get-msg-filename" (car msgs) loc-alist)
       ;; dst file
       (expand-file-name
	(elmo-msgid-to-cache
	 (cdr (assq (if same-number (car msgs) next-num) number-alist)))
	dst-dir))
      (if (and (setq msgs (cdr msgs))
	       (not same-number))
	  (setq next-num (1+ next-num))))
    t))

(defun elmo-cache-use-cache-p (spec number)
  nil)

(defun elmo-cache-local-file-p (spec number)
  t)

(defun elmo-cache-get-msg-filename (spec number &optional loc-alist)
  (expand-file-name
   (elmo-cache-number-to-filename spec number)
   (elmo-cache-get-folder-directory spec)))

(defalias 'elmo-cache-sync-number-alist
  'elmo-generic-sync-number-alist)
(defalias 'elmo-cache-list-folder-unread
  'elmo-generic-list-folder-unread)
(defalias 'elmo-cache-list-folder-important
  'elmo-generic-list-folder-important)
(defalias 'elmo-cache-commit 'elmo-generic-commit)
(defalias 'elmo-cache-folder-diff 'elmo-generic-folder-diff)

(require 'product)
(product-provide (provide 'elmo-cache) (require 'elmo-version))

;;; elmo-cache.el ends here
