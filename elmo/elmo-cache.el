;;; elmo-cache.el -- Cache modules for Elmo.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright 2000 Kenichi OKADA <okada@opaopa.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;         Kenichi OKADA <okada@opaopa.org>
;; Keywords: mail, net news
;; Time-stamp: <00/03/01 09:57:55 teranisi>

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

(defun elmo-cache-delete (msgid folder number)
  "Delete cache file associated with message-id 'MSGID', FOLDER, NUMBER."
  (let ((path (elmo-cache-exists-p msgid folder number)))
    (if path (delete-file path))))

(defsubst elmo-cache-to-msgid (filename)
  (concat "<" (elmo-recover-msgid-from-filename filename) ">"))

(defun elmo-cache-force-delete (path &optional locked)
  "Delete cache file."
  ;; for safety...
  (unless (string-match elmo-cache-dirname path)
    (error "%s is not cache file!" path))
  (let (message-id)
    (if (or (elmo-msgdb-global-mark-get 
	     (setq message-id
		   (elmo-cache-to-msgid (file-name-nondirectory path))))
	    (member message-id locked))
	nil ;; Don't delete caches with mark (or locked message).
      (if (and path 
	       (file-directory-p path))
	  (progn
	    (mapcar 'delete-file (directory-files path t "^[^\\.]"))
	    (delete-directory path))
	(delete-file path))
      t)))

(defun elmo-cache-delete-partial (msgid folder number)
  "Delete cache file only if it is partial message."
  (if msgid
      (let ((path1 (elmo-cache-get-path msgid))
	    path2)
	(if (and path1 
		 (file-exists-p path1))
	    (if (and folder
		     (file-directory-p path1))
		(when (file-exists-p (setq path2 
					   (expand-file-name
					    (format "%s@%s" 
						    number
						    (elmo-safe-filename
						     folder))
					    path1)))
		  (delete-file path2)
		  (unless (directory-files path1 t "^[^\\.]")
		    (delete-directory path1))))))))

(defun elmo-cache-read (msgid &optional folder number outbuf)
  "Read cache contents to outbuf"
  (save-excursion
    (let ((path (elmo-cache-exists-p msgid folder number)))
      (when path
	(if outbuf (set-buffer outbuf))
	(erase-buffer)
	(as-binary-input-file (insert-file-contents path))
	t))))

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
    (message "Checking disk usage...done.")
    (let ((cfl (elmo-cache-get-sorted-cache-file-list))
	  (deleted 0)
	  oldest 
	  cur-size cur-file)
      (while (and (<= size total)
		  (setq oldest (elmo-cache-get-oldest-cache-file-entity cfl)))
	(setq cur-file (expand-file-name (car (cdr oldest)) (car oldest)))
	(if (file-directory-p cur-file)
	    (setq cur-size (elmo-disk-usage cur-file))
	  (setq cur-size 
		(/ (float (nth 7 (file-attributes cur-file)))
		   Kbytes)))
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
;    (prin1 firsts)
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
    (message "Collecting cache info...done.")
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

(defun elmo-cache-save (msgid partial folder number &optional inbuf)
  "If partial is non-nil, save current buffer (or INBUF) as partial cache."
  (condition-case nil
  (save-excursion
    (let* ((path (if partial
		     (elmo-cache-get-path msgid folder number)
		   (elmo-cache-get-path msgid)))
	   dir tmp-buf)
      (when path 
	(setq dir (directory-file-name (file-name-directory path)))
	(if (not (file-exists-p dir))
	    (elmo-make-directory dir))
	(if inbuf (set-buffer inbuf))
	(goto-char (point-min))
	(as-binary-output-file (write-region (point-min) (point-max)
					     path nil 'no-msg)))))
  (error)))

(defun elmo-cache-exists-p (msgid &optional folder number)
  "Returns the path if the cache exists."
  (save-match-data
    (if msgid
	(let ((path (elmo-cache-get-path msgid)))
	  (if (and path
		   (file-exists-p path))
	      (if (and folder
		       (file-directory-p path))
		  (if (file-exists-p (setq path (expand-file-name
						 (format "%s@%s" 
							 (or number "") 
							 (elmo-safe-filename
							  folder))
						 path)))
		      path
		    )
		;; not directory.
		path))))))

(defun elmo-cache-search-all (folder condition from-msgs)
  (let* ((number-alist (elmo-msgdb-number-load
			(elmo-msgdb-expand-path folder)))
	 (nalist number-alist)
	 (num (length number-alist))
	 cache-file
	 ret-val
	 case-fold-search msg
	 percent i)
    (setq i 0)    
    (while nalist
      (if (and (setq cache-file (elmo-cache-exists-p (cdr (car nalist))
						     folder 
						     (car (car nalist))))
	       (elmo-file-field-condition-match cache-file condition))
	  (setq ret-val (append ret-val (list (caar nalist)))))
      (when (> num elmo-display-progress-threshold)
	(setq i (1+ i))
	(setq percent (/ (* i 100) num))
	(elmo-display-progress
	 'elmo-cache-search-all "Searching..."
	 percent))
      (setq nalist (cdr nalist)))
    ret-val))

(defun elmo-cache-collect-sub-directories (init dir &optional recursively)
  "Collect subdirectories under 'dir'"
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
    (elmo-replace-msgid-as-filename (elmo-match-string 1 msgid))))

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
;;; buffer cache module

(defconst elmo-buffer-cache-name " *elmo cache*")

(defvar elmo-buffer-cache nil
  "Message cache. (old ... new) order alist with association
 ((\"folder\" message \"message-id\") . cache-buffer)")

(defmacro elmo-buffer-cache-buffer-get (entry)
  (` (cdr (, entry))))

(defmacro elmo-buffer-cache-folder-get (entry)
  (` (car (car (, entry)))))

(defmacro elmo-buffer-cache-message-get (entry)
  (` (cdr (car (, entry)))))

(defmacro elmo-buffer-cache-entry-make (fld-msg-id buf)
  (` (cons (, fld-msg-id) (, buf))))

(defmacro elmo-buffer-cache-hit (fld-msg-id)
  "Return value assosiated with key."
  (` (elmo-buffer-cache-buffer-get
      (assoc (, fld-msg-id) elmo-buffer-cache))))

(defun elmo-buffer-cache-sort (entry)
  (let* ((pointer (cons nil elmo-buffer-cache))
	 (top pointer))
    (while (cdr pointer)
      (if (equal (car (cdr pointer)) entry)
	  (setcdr pointer (cdr (cdr pointer)))
	(setq pointer (cdr pointer))))
    (setcdr pointer (list entry))
    (setq elmo-buffer-cache (cdr top))))

(defun elmo-buffer-cache-add (fld-msg-id)
  "Adding (fld-msg-id . buf) to the top of \"elmo-buffer-cache\".
Returning its cache buffer."
  (let ((len (length elmo-buffer-cache))
	(buf nil))
    (if (< len elmo-buffer-cache-size)
	(setq buf (get-buffer-create (format "%s%d" elmo-buffer-cache-name len)))
      (setq buf (elmo-buffer-cache-buffer-get (nth (1- len) elmo-buffer-cache)))
      (setcdr (nthcdr (- len 2) elmo-buffer-cache) nil))
    (save-excursion
      (set-buffer buf)
      (elmo-set-buffer-multibyte nil))
    (setq elmo-buffer-cache
	  (cons (elmo-buffer-cache-entry-make fld-msg-id buf)
		elmo-buffer-cache))
    buf))

(defun elmo-buffer-cache-delete ()
  "Delete the most recent cache entry."
  (let ((buf (elmo-buffer-cache-buffer-get (car elmo-buffer-cache))))
    (setq elmo-buffer-cache
	  (nconc (cdr elmo-buffer-cache)
		 (list (elmo-buffer-cache-entry-make nil buf))))))

(defun elmo-buffer-cache-clean-up ()
  "A function to flush all decoded messages in cache list."
  (interactive)
  (let ((n 0) buf)
    (while (< n elmo-buffer-cache-size)
      (setq buf (concat elmo-buffer-cache-name (int-to-string n)))
      (elmo-kill-buffer buf)
      (setq n (1+ n))))
  (setq elmo-buffer-cache nil))

;;;
;;; cache backend by Kenichi OKADA <okada@opaopa.org>
;;;

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

(if (boundp 'nemacs-version)
    (defsubst elmo-cache-insert-header (file)
      "Insert the header of the article (Does not work on nemacs)."
      (as-binary-input-file
       (insert-file-contents file)))
  (defsubst elmo-cache-insert-header (file)
    "Insert the header of the article."
    (let ((beg 0)
	  insert-file-contents-pre-hook   ; To avoid autoconv-xmas...
	  insert-file-contents-post-hook
	  format-alist)
      (when (file-exists-p file)
	;; Read until header separator is found.
	(while (and (eq elmo-localdir-header-chop-length
			(nth 1 
			     (as-binary-input-file 
			      (insert-file-contents
			       file nil beg
			       (incf beg elmo-localdir-header-chop-length)))))
		    (prog1 (not (search-forward "\n\n" nil t))
		      (goto-char (point-max)))))))))

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
	      (elmo-cache-insert-header file)
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
      (message "Creating msgdb...done.")
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
	  (if (string-match "^[+=$!]$" folder) ;; localdir, archive, localnews
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

(defun elmo-cache-list-folder (spec); called by elmo-cache-search()
  (elmo-cache-list-folder-subr spec))

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
					    condition)
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
	  (elmo-msgdb-expand-path nil src-spec))))
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

(provide 'elmo-cache)

;;; elmo-cache.el ends here
