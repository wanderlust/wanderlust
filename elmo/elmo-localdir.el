;;; elmo-localdir.el -- Localdir Interface for ELMO.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news
;; Time-stamp: <00/03/22 00:03:39 teranisi>

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

(require 'emu)
(require 'std11)

(eval-when-compile
  (require 'elmo-cache))
(require 'elmo-msgdb)

(defsubst elmo-localdir-get-folder-directory (spec)
  (if (file-name-absolute-p (nth 1 spec))
      (nth 1 spec) ; already full path.
    (expand-file-name (nth 1 spec)
		      (cond ((eq (car spec) 'localnews)
			     elmo-localnews-folder-path)
			    (t
			     elmo-localdir-folder-path)))))

(defun elmo-localdir-msgdb-expand-path (spec)
  (let ((fld-name (nth 1 spec)))
    (expand-file-name fld-name
		      (expand-file-name "localdir"
					elmo-msgdb-dir))))

(defun elmo-localdir-number-to-filename (spec dir number &optional loc-alist)
  (expand-file-name (int-to-string number) dir))

(if (boundp 'nemacs-version)
    (defsubst elmo-localdir-insert-header (file)
      "Insert the header of the article (Does not work on nemacs)."
      (as-binary-input-file
       (insert-file-contents file)))
  (defsubst elmo-localdir-insert-header (file)
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


(defsubst elmo-localdir-msgdb-create-overview-entity-from-file (number file)
  (save-excursion
    (let ((tmp-buffer (get-buffer-create " *ELMO LocalDir Temp*"))
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
	      (elmo-localdir-insert-header file)
	    (error (throw 'done nil)))
	  (goto-char (point-min))
	  (setq header-end 
		(if (re-search-forward "\\(^--.*$\\)\\|\\(\n\n\\)" nil t)
		    (point)
		  (point-max)))
	  (narrow-to-region (point-min) header-end)
	  (setq ret-val (elmo-msgdb-create-overview-from-buffer number size mtime))
	  (kill-buffer tmp-buffer))
	ret-val
	))))

(defun elmo-localdir-msgdb-create-entity (dir number)
  (elmo-localdir-msgdb-create-overview-entity-from-file
   number (expand-file-name (int-to-string number) dir)))

(defun elmo-localdir-msgdb-create-as-numlist (spec numlist new-mark 
						   already-mark seen-mark 
						   important-mark seen-list)
  (when numlist
    (let ((dir (elmo-localdir-get-folder-directory spec))
	  overview number-alist mark-alist entity message-id
	  i percent len num seen gmark)
      (setq len (length numlist))
      (setq i 0)
      (message "Creating msgdb...")
      (while numlist
	(setq entity
	      (elmo-localdir-msgdb-create-entity 
	       dir (car numlist)))
	(if (null entity)
	    ()
	  (setq num (elmo-msgdb-overview-entity-get-number entity))
	  (setq overview 
		(elmo-msgdb-append-element
		 overview entity))
	  (setq number-alist
		(elmo-msgdb-number-add number-alist
				       num
				       (elmo-msgdb-overview-entity-get-id
					entity)))
	  (setq message-id (elmo-msgdb-overview-entity-get-id entity))
	  (setq seen (member message-id seen-list))
	  (if (setq gmark (or (elmo-msgdb-global-mark-get message-id)
			      (if (elmo-cache-exists-p message-id) ; XXX
				  (if seen 
				      nil
				    already-mark)
				(if seen 
				    nil ;;seen-mark
				  new-mark))))
	      (setq mark-alist
		    (elmo-msgdb-mark-append 
		     mark-alist 
		     num
		     gmark))))
	(setq i (1+ i))
	(setq percent (/ (* i 100) len))
	(elmo-display-progress
	 'elmo-localdir-msgdb-create-as-numlist "Creating msgdb..."
	 percent)
	(setq numlist (cdr numlist)))
      (message "Creating msgdb...done.")
      (list overview number-alist mark-alist))))

(defalias 'elmo-localdir-msgdb-create 'elmo-localdir-msgdb-create-as-numlist)

(defvar elmo-localdir-list-folders-spec-string "+")
(defvar elmo-localdir-list-folders-filter-regexp "^\\(\\.\\.?\\|[0-9]+\\)$")

(defun elmo-localdir-list-folders (spec &optional hierarchy)
  (let ((folder (concat elmo-localdir-list-folders-spec-string (nth 1 spec))))
    (elmo-localdir-list-folders-subr folder hierarchy)))

(defun elmo-localdir-list-folders-subr (folder &optional hierarchy)
  (let ((case-fold-search t)
	folders curdir dirent relpath abspath attr
	subprefix subfolder)
    (condition-case ()
	(progn
	  (setq curdir
		(expand-file-name (nth 1 (elmo-folder-get-spec folder))
				  elmo-localdir-folder-path))
	  (if (string-match "^[+=$.]$" folder) ; localdir, archive, localnews
	      (setq subprefix folder)
	    (setq subprefix (concat folder elmo-path-sep))
	    ;; include parent
	    (setq folders (list folder)))
	  (setq dirent (directory-files curdir))
	  (catch 'done
	   (while dirent
	    (setq relpath (car dirent))
	    (setq dirent (cdr dirent))
	    (setq abspath (expand-file-name relpath curdir))
	    (and
	     (not (string-match 
		   elmo-localdir-list-folders-filter-regexp
		   relpath))
	     (eq (nth 0 (setq attr (file-attributes abspath))) t)
	     (if (eq hierarchy 'check)
		 (throw 'done (nconc folders t))
	       t)
	     (setq subfolder (concat subprefix relpath))
	     (setq folders (nconc folders
				  (if (and hierarchy
					   (if elmo-have-link-count
					       (< 2 (nth 1 attr))
					     (cdr
					      (elmo-localdir-list-folders-subr
					       subfolder 'check))))
				      (list (list subfolder))
				    (list subfolder))))
	     (or
	      hierarchy
	      (and elmo-have-link-count (>= 2 (nth 1 attr)))
	      (setq folders
		    (nconc folders (cdr (elmo-localdir-list-folders-subr
					 subfolder hierarchy))))))))
	  folders)
      (file-error folders))))

(defsubst elmo-localdir-list-folder-subr (spec &optional nonsort)
  (let* ((dir (elmo-localdir-get-folder-directory spec))
	 (flist (mapcar 'string-to-int
			(directory-files dir nil "^[0-9]+$" t))))
    (if nonsort
	(cons (or (elmo-max-of-list flist) 0) (length flist))
      (sort flist '<))))

(defun elmo-localdir-append-msg (spec string &optional msg no-see)
  (let ((dir (elmo-localdir-get-folder-directory spec))
	(tmp-buffer (get-buffer-create " *ELMO Temp buffer*"))
	(next-num (or msg
		      (1+ (car (elmo-localdir-list-folder-subr spec t)))))
	filename)
    (save-excursion
      (set-buffer tmp-buffer)
      (erase-buffer)
      (setq filename (expand-file-name (int-to-string
					next-num)
				       dir))
      (unwind-protect
	  (if (file-writable-p filename)
	      (progn
		(insert string)
		(as-binary-output-file
		 (write-region (point-min) (point-max) filename nil 'no-msg))
		t)
	    nil
	    )
	(kill-buffer tmp-buffer)))))

(defun elmo-localdir-delete-msg (spec number)
  (let (file
	(dir (elmo-localdir-get-folder-directory spec))
        (number (int-to-string number)))
    (setq file (expand-file-name number dir))
    (if (and (string-match "[0-9]+" number) ; for safety.
	     (file-exists-p file)
	     (file-writable-p file) 
	     (not (file-directory-p file)))
	(progn (delete-file file)
	       t))))

(defun elmo-localdir-read-msg (spec number outbuf &optional set-mark)
  (save-excursion
    (let* ((number (int-to-string number))
	   (dir (elmo-localdir-get-folder-directory spec))
	   (file (expand-file-name number dir)))
      (set-buffer outbuf)
      (erase-buffer)
      (when (file-exists-p file)
	(as-binary-input-file (insert-file-contents file))
	(elmo-delete-cr-get-content-type)))))

(defun elmo-localdir-delete-msgs (spec msgs)
  (mapcar '(lambda (msg) (elmo-localdir-delete-msg spec msg))
	  msgs))

(defun elmo-localdir-list-folder (spec); called by elmo-localdir-search()
  (elmo-localdir-list-folder-subr spec))

(defun elmo-localdir-max-of-folder (spec)
  (elmo-localdir-list-folder-subr spec t))

(defun elmo-localdir-check-validity (spec validity-file)
  (let* ((dir (elmo-localdir-get-folder-directory spec))
	 (cur-val (nth 5 (file-attributes dir)))
	 (file-val (read 
		    (or (elmo-get-file-string validity-file)
			"nil"))))
    (cond
     ((or (null cur-val) (null file-val)) nil)
     ((> (car cur-val) (car file-val)) nil)
     ((= (car cur-val) (car file-val))
      (if (> (cadr cur-val) (cadr file-val)) nil t)) ; t if same
     (t t))))

(defun elmo-localdir-sync-validity (spec validity-file)
  (save-excursion
    (let* ((dir (elmo-localdir-get-folder-directory spec))
	   (tmp-buffer (get-buffer-create " *ELMO TMP*"))
	   (number-file (expand-file-name elmo-msgdb-number-filename dir)))
      (set-buffer tmp-buffer)
      (erase-buffer)
      (prin1 (nth 5 (file-attributes dir)) tmp-buffer)
      (princ "\n" tmp-buffer)
      (if (file-writable-p validity-file)
	  (write-region (point-min) (point-max) 
			validity-file nil 'no-msg)
	(message (format "%s is not writable." number-file)))
      (kill-buffer tmp-buffer))))

(defun elmo-localdir-folder-exists-p (spec)
  (file-directory-p (elmo-localdir-get-folder-directory spec)))

(defun elmo-localdir-folder-creatable-p (spec)
  t)

(defun elmo-localdir-create-folder (spec)
  (save-excursion
    (let ((dir (elmo-localdir-get-folder-directory spec)))
      (if (file-directory-p dir)
          ()
	(if (file-exists-p dir)
	    (error "Create folder failed")
	  (elmo-make-directory dir))
	t
	))))

(defun elmo-localdir-delete-folder (spec)
  (let* ((dir (elmo-localdir-get-folder-directory spec)))
    (if (not (file-directory-p dir))
	(error "no such directory: %s" dir)
      (elmo-delete-directory dir t)
      t)))

(defun elmo-localdir-rename-folder (old-spec new-spec)
  (let* ((old (elmo-localdir-get-folder-directory old-spec))
	 (new (elmo-localdir-get-folder-directory new-spec))
	 (new-dir (directory-file-name (file-name-directory new))))
    (if (not (file-directory-p old))
	(error "no such directory: %s" old)
      (if (file-exists-p new)
	  (error "already exists directory: %s" new)
	(if (not (file-exists-p new-dir))
	    (elmo-make-directory new-dir))
	(rename-file old new)
	t))))

(defsubst elmo-localdir-field-condition-match (spec number condition)
  (elmo-file-field-condition-match 
   (expand-file-name (int-to-string number)
		     (elmo-localdir-get-folder-directory spec))
   condition))

(defun elmo-localdir-search (spec condition &optional from-msgs)
  (let* ((msgs (or from-msgs (elmo-localdir-list-folder spec)))
	 (num (length msgs))
	 (i 0) case-fold-search ret-val)
    (while msgs
      (if (elmo-localdir-field-condition-match spec (car msgs)
					       condition)
	  (setq ret-val (cons (car msgs) ret-val)))
      (setq i (1+ i))
      (elmo-display-progress
       'elmo-localdir-search "Searching..."
       (/ (* i 100) num))
      (setq msgs (cdr msgs)))
    (nreverse ret-val)))

;;; (localdir, maildir, localnews) -> localdir
(defun elmo-localdir-copy-msgs (dst-spec msgs src-spec
					 &optional loc-alist same-number)
  (let ((dst-dir
	 (elmo-localdir-get-folder-directory dst-spec))
	(next-num (1+ (car (elmo-localdir-list-folder-subr dst-spec t)))))
    (while msgs
      (elmo-copy-file
       ;; src file
       (elmo-call-func src-spec "get-msg-filename" (car msgs) loc-alist)
       ;; dst file
       (expand-file-name (int-to-string
			  (if same-number (car msgs) next-num))
			 dst-dir))
      (if (and (setq msgs (cdr msgs))
	       (not same-number))
	  (setq next-num
		(if (and (eq (car dst-spec) 'localdir)
			 (elmo-localdir-locked-p))
		    ;; MDA is running.
		    (1+ (car (elmo-localdir-list-folder-subr dst-spec t)))
		  (1+ next-num)))))
    t))

(defun elmo-localdir-pack-number (spec msgdb arg)
  (let ((dir (elmo-localdir-get-folder-directory spec))
	(onum-alist (elmo-msgdb-get-number-alist msgdb))
	(omark-alist (elmo-msgdb-get-mark-alist msgdb))
	(oov (elmo-msgdb-get-overview msgdb))
	i flist onum mark new-mark-alist total)
    (setq i 1)
    (setq flist
	  (if elmo-pack-number-check-strict
	      (elmo-call-func spec "list-folder") ;; allow localnews
	    (mapcar 'car onum-alist)))
    (setq total (length flist))
    (while flist
      (elmo-display-progress
       'elmo-localdir-pack-number "Packing..."
       (/ (* i 100) total))
      (setq onum (car flist))
      (when (not (eq onum i)) ;; why \=() is wrong..
        (elmo-bind-directory
	 dir
	 ;; xxx  nfs,hardlink
	 (rename-file (int-to-string onum) (int-to-string i) t))
        ;; update overview
        (elmo-msgdb-overview-entity-set-number
	 (elmo-msgdb-overview-get-entity-by-number
	  oov onum) i)
	;; update number-alist
	(setcar (assq onum onum-alist) i))
      ;; update mark-alist
      (when (setq mark (cadr (assq onum omark-alist)))
	(setq new-mark-alist
	      (elmo-msgdb-mark-append
	       new-mark-alist
	       i mark)))
      (setq i (1+ i))
      (setq flist (cdr flist)))
    (message "Packing...done.")
    (list (elmo-msgdb-get-overview msgdb)
	  onum-alist
	  new-mark-alist
	  (elmo-msgdb-get-location msgdb))))

(defun elmo-localdir-use-cache-p (spec number)
  nil)

(defun elmo-localdir-local-file-p (spec number)
  t)

(defun elmo-localdir-get-msg-filename (spec number &optional loc-alist)
  (expand-file-name 
   (int-to-string number)
   (elmo-localdir-get-folder-directory spec)))

(defun elmo-localdir-locked-p ()
  (if elmo-localdir-lockfile-list
      (let ((lock elmo-localdir-lockfile-list))
	(catch 'found
	  (while lock
	    (if (file-exists-p (car lock))
		(throw 'found t))
	    (setq lock (cdr lock)))))))

(defalias 'elmo-localdir-sync-number-alist 
  'elmo-generic-sync-number-alist)
(defalias 'elmo-localdir-list-folder-unread 
  'elmo-generic-list-folder-unread)
(defalias 'elmo-localdir-list-folder-important
  'elmo-generic-list-folder-important)
(defalias 'elmo-localdir-commit 'elmo-generic-commit)

(provide 'elmo-localdir)

;;; elmo-localdir.el ends here
