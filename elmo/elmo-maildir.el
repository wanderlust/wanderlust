;;; elmo-maildir.el -- Maildir interface for ELMO.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
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
(require 'elmo-localdir)

(defvar elmo-maildir-sequence-number-internal 0
  "Sequence number for the pid part of unique filename.
This variable should not be used in elsewhere.")

(defsubst elmo-maildir-get-folder-directory (spec)
  (if (file-name-absolute-p (nth 1 spec))
      (nth 1 spec) ; already full path.
    (expand-file-name (nth 1 spec)
		      elmo-maildir-folder-path)))

(defun elmo-maildir-number-to-filename (dir number loc-alist)
  (let ((location (cdr (assq number loc-alist))))
    (and location (elmo-maildir-get-filename location dir))))

(defun elmo-maildir-get-filename (location dir)
  "Get a filename that is corresponded to LOCATION in DIR."
  (expand-file-name
   (let ((file (file-name-completion (symbol-name location)
				     (expand-file-name "cur" dir))))
     (if (eq file t) location file))
   (expand-file-name "cur" dir)))

(defsubst elmo-maildir-list-location (dir &optional child-dir)
  (let* ((cur-dir (expand-file-name (or child-dir "cur") dir))
	 (cur (directory-files cur-dir
			       nil "^[^.].*$" t))
	 seen-list seen sym list)
    (setq list
	  (mapcar
	   (lambda (x)
	     (if (string-match "^\\([^:]+\\):\\([^:]+\\)$" x)
		 (progn
		   (setq seen nil)
		   (save-match-data
		     (if (string-match
			  "S"
			  (elmo-match-string 2 x))
			 (setq seen t)))
		   (setq sym (intern (elmo-match-string 1 x)))
		   (if seen
		       (setq seen-list (cons sym seen-list)))
		   sym)
	       (intern x)))
	   cur))
    (cons list seen-list)))

(defun elmo-maildir-msgdb-create-entity (dir number loc-alist)
  (elmo-localdir-msgdb-create-overview-entity-from-file
   number
   (elmo-maildir-number-to-filename dir number loc-alist)))

(defun elmo-maildir-cleanup-temporal (dir)
  ;; Delete files in the tmp dir which are not accessed
  ;; for more than 36 hours.
  (let ((cur-time (current-time))
	(count 0)
	last-accessed)
    (mapcar (function
	     (lambda (file)
	       (setq last-accessed (nth 4 (file-attributes file)))
	       (when (or (> (- (car cur-time)(car last-accessed)) 1)
			 (and (eq (- (car cur-time)(car last-accessed)) 1)
			      (> (- (cadr cur-time)(cadr last-accessed))
				 64064))) ; 36 hours.
		 (message "Maildir: %d tmp file(s) are cleared."
			  (setq count (1+ count)))
		 (delete-file file))))
	    (directory-files (expand-file-name "tmp" dir)
			     t ; full
			     "^[^.].*$" t))))

(defun elmo-maildir-update-current (spec)
  "Move all new msgs to cur in the maildir"
  (let* ((maildir (elmo-maildir-get-folder-directory spec))
	 (news (directory-files (expand-file-name "new"
						  maildir)
				nil
				"^[^.].*$" t)))
    ;; cleanup tmp directory.
    (elmo-maildir-cleanup-temporal maildir)
    ;; move new msgs to cur directory.
    (mapcar (lambda (x)
	      (rename-file
	       (expand-file-name x (expand-file-name "new" maildir))
	       (expand-file-name (concat x ":2,")
				 (expand-file-name "cur" maildir))))
	    news)))

(defun elmo-maildir-set-mark (filename mark)
  "Mark the file in the maildir. MARK is a character."
  (if (string-match "^\\([^:]+:2,\\)\\(.*\\)$" filename)
      (let ((flaglist (string-to-char-list (elmo-match-string
					    2 filename))))
	(unless (memq mark flaglist)
	  (setq flaglist (sort (cons mark flaglist) '<))
	  (rename-file filename
		       (concat (elmo-match-string 1 filename)
			       (char-list-to-string flaglist)))))))

(defun elmo-maildir-delete-mark (filename mark)
  "Mark the file in the maildir. MARK is a character."
  (if (string-match "^\\([^:]+:2,\\)\\(.*\\)$" filename)
      (let ((flaglist (string-to-char-list (elmo-match-string
					    2 filename))))
	(when (memq mark flaglist)
	  (setq flaglist (delq mark flaglist))
	  (rename-file filename
		       (concat (elmo-match-string 1 filename)
			       (if flaglist
				   (char-list-to-string flaglist))))))))

(defsubst elmo-maildir-set-mark-msgs (spec mark msgs msgdb)
  (let ((dir (elmo-maildir-get-folder-directory spec))
	(locs (if msgdb
		  (elmo-msgdb-get-location msgdb)
		(elmo-msgdb-location-load (elmo-msgdb-expand-path spec))))
	file)
    (while msgs
      (if (setq file (elmo-maildir-number-to-filename dir (car msgs) locs))
	  (elmo-maildir-set-mark file mark))
      (setq msgs (cdr msgs)))))

(defsubst elmo-maildir-delete-mark-msgs (spec mark msgs msgdb)
  (let ((dir (elmo-maildir-get-folder-directory spec))
	(locs (if msgdb
		  (elmo-msgdb-get-location msgdb)
		(elmo-msgdb-location-load (elmo-msgdb-expand-path spec))))
	file)
    (while msgs
      (if (setq file (elmo-maildir-number-to-filename dir (car msgs) locs))
	  (elmo-maildir-delete-mark file mark))
      (setq msgs (cdr msgs)))))

(defun elmo-maildir-mark-as-important (spec msgs &optional msgdb)
  (elmo-maildir-set-mark-msgs spec ?F msgs msgdb))
  
(defun elmo-maildir-unmark-important (spec msgs &optional msgdb)
  (elmo-maildir-delete-mark-msgs spec ?F msgs msgdb))

(defun elmo-maildir-mark-as-read (spec msgs &optional msgdb)
  (elmo-maildir-set-mark-msgs spec ?S msgs msgdb))

(defun elmo-maildir-mark-as-unread (spec msgs &optional msgdb)
  (elmo-maildir-delete-mark-msgs spec ?S msgs msgdb))

(defun elmo-maildir-msgdb-create (spec numlist new-mark
				       already-mark seen-mark
				       important-mark
				       seen-list
				       &optional msgdb)
  (when numlist
    (let* ((dir (elmo-maildir-get-folder-directory spec))
	   (loc-alist (if msgdb (elmo-msgdb-get-location msgdb)
			(elmo-msgdb-location-load (elmo-msgdb-expand-path
						   spec))))
	   (loc-seen (elmo-maildir-list-location dir))
	   (loc-list  (car loc-seen))
	   (seen-list (cdr loc-seen))
	   overview number-alist mark-alist entity
	   i percent num location pair)
      (setq num (length numlist))
      (setq i 0)
      (message "Creating msgdb...")
      (while numlist
	(setq entity
	      (elmo-maildir-msgdb-create-entity
	       dir (car numlist) loc-alist))
	(if (null entity)
	    ()
	  (setq overview
		(elmo-msgdb-append-element
		 overview entity))
	  (setq number-alist
		(elmo-msgdb-number-add number-alist
				       (elmo-msgdb-overview-entity-get-number
					entity)
				       (elmo-msgdb-overview-entity-get-id
					entity)))
	  (setq location (cdr (assq (car numlist) loc-alist)))
	  (unless (member location seen-list)
	    (setq mark-alist
		  (elmo-msgdb-mark-append
		   mark-alist
		   (elmo-msgdb-overview-entity-get-number
		    entity)
		   (or (elmo-msgdb-global-mark-get
			(elmo-msgdb-overview-entity-get-id
			 entity))
		       new-mark)))))
	(when (> num elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (setq percent (/ (* i 100) num))
	  (elmo-display-progress
	   'elmo-maildir-msgdb-create "Creating msgdb..."
	   percent))
	(setq numlist (cdr numlist)))
      (message "Creating msgdb...done.")
      (elmo-msgdb-sort-by-date
       (list overview number-alist mark-alist loc-alist)))))

(defalias 'elmo-maildir-msgdb-create-as-numlist 'elmo-maildir-msgdb-create)

(defun elmo-maildir-list-folders (spec &optional hierarchy)
  (let ((elmo-localdir-folder-path elmo-maildir-folder-path)
	(elmo-localdir-list-folders-spec-string ".")
	(elmo-localdir-list-folders-filter-regexp
	 "^\\(\\.\\.?\\|cur\\|tmp\\|new\\)$")
	elmo-have-link-count folders)
    (setq folders (elmo-localdir-list-folders spec hierarchy))
    (if (eq (length (nth 1 spec)) 0) ; top
	(setq folders (append
		       (list (concat elmo-localdir-list-folders-spec-string
				     (nth 1 spec)))
		       folders)))
    (elmo-delete-if
     (function (lambda (folder)
		 (not (or (listp folder) (elmo-folder-exists-p folder)))))
     folders)))

(static-cond
 ((>= emacs-major-version 19)
  (defun elmo-maildir-make-unique-string ()
    "This function generates a string that can be used as a unique
file name for maildir directories."
     (let ((cur-time (current-time)))
       (format "%.0f.%d_%d.%s"
 	      (+ (* (car cur-time)
                    (float 65536)) (cadr cur-time))
	      (emacs-pid)
	      (incf elmo-maildir-sequence-number-internal)
	      (system-name)))))
 ((eq emacs-major-version 18)
  ;; A fake function for v18
  (defun elmo-maildir-make-unique-string ()
    "This function generates a string that can be used as a unique
file name for maildir directories."
    (unless (fboundp 'float-to-string)
      (load-library "float"))
    (let ((time (current-time)))
      (format "%s%d.%d.%s"
	      (substring
	       (float-to-string
		(f+ (f* (f (car time))
			(f 65536))
		    (f (cadr time))))
	       0 5)
	      (cadr time)
	      (% (abs (random t)) 10000); dummy pid
	      (system-name))))))

(defun elmo-maildir-temporal-filename (basedir)
  (let ((filename (expand-file-name
		   (concat "tmp/" (elmo-maildir-make-unique-string))
		   basedir)))
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))
    (while (file-exists-p filename)
      ;; (sleep-for 2) ; I don't want to wait.
      (setq filename
	    (expand-file-name
	     (concat "tmp/" (elmo-maildir-make-unique-string))
	     basedir)))
    filename))

(defun elmo-maildir-append-msg (spec string &optional msg no-see)
  (let ((basedir (elmo-maildir-get-folder-directory spec))
	filename)
    (condition-case nil
	(with-temp-buffer
	  (setq filename (elmo-maildir-temporal-filename basedir))
	  (insert string)
	  (as-binary-output-file
	   (write-region (point-min) (point-max) filename nil 'no-msg))
	  ;; add link from new.
	  (elmo-add-name-to-file
	   filename
	   (expand-file-name
	    (concat "new/" (file-name-nondirectory filename))
	    basedir))
	  t)
      ;; If an error occured, return nil.
      (error))))

(defun elmo-maildir-delete-msg (spec number loc-alist)
  (let ((dir (elmo-maildir-get-folder-directory spec))
	file)
    (setq file (elmo-maildir-number-to-filename dir number loc-alist))
    (if (and (file-writable-p file)
	     (not (file-directory-p file)))
	(progn (delete-file file)
	       t))))

(defun elmo-maildir-read-msg (spec number outbuf &optional msgdb)
  (save-excursion
    (let* ((loc-alist (if msgdb (elmo-msgdb-get-location msgdb)
			(elmo-msgdb-location-load (elmo-msgdb-expand-path
						   spec))))
	   (dir (elmo-maildir-get-folder-directory spec))
	   (file (elmo-maildir-number-to-filename dir number loc-alist)))
      (set-buffer outbuf)
      (erase-buffer)
      (when (file-exists-p file)
	(as-binary-input-file (insert-file-contents file))
	(elmo-delete-cr-get-content-type)))))

(defun elmo-maildir-delete-msgs (spec msgs &optional msgdb)
  (let ((loc-alist (if msgdb (elmo-msgdb-get-location msgdb)
		     (elmo-msgdb-location-load (elmo-msgdb-expand-path
						spec)))))
    (mapcar '(lambda (msg) (elmo-maildir-delete-msg spec msg
						    loc-alist))
	    msgs)))

(defsubst elmo-maildir-list-folder-subr (spec &optional nonsort)
  (let* ((dir (elmo-maildir-get-folder-directory spec))
	 (flist (elmo-list-folder-by-location
		 spec
		 (car (elmo-maildir-list-location dir))))
	 (killed (and elmo-use-killed-list
		      (elmo-msgdb-killed-list-load
		       (elmo-msgdb-expand-path spec))))
	 (news (car (elmo-maildir-list-location dir "new")))
	 numbers)
    (if nonsort
	(cons (+ (or (elmo-max-of-list flist) 0) (length news))
	      (+ (length news)
		 (if killed
		     (- (length flist) (length killed))
		   (length flist))))
      (setq numbers (sort flist '<))
      (elmo-living-messages numbers killed))))

(defun elmo-maildir-list-folder (spec)
  (elmo-maildir-update-current spec)
  (elmo-maildir-list-folder-subr spec))

(defun elmo-maildir-max-of-folder (spec)
  (elmo-maildir-list-folder-subr spec t))

(defalias 'elmo-maildir-check-validity 'elmo-localdir-check-validity)

(defalias 'elmo-maildir-sync-validity  'elmo-localdir-sync-validity)

(defun elmo-maildir-folder-exists-p (spec)
  (let ((basedir (elmo-maildir-get-folder-directory spec)))
    (and (file-directory-p (expand-file-name "new" basedir))
	 (file-directory-p (expand-file-name "cur" basedir))
	 (file-directory-p (expand-file-name "tmp" basedir)))))

(defun elmo-maildir-folder-creatable-p (spec)
  t)

(defun elmo-maildir-create-folder (spec)
  (let ((basedir (elmo-maildir-get-folder-directory spec)))
    (condition-case nil
	(progn
	  (mapcar (function (lambda (dir)
			      (setq dir (expand-file-name dir basedir))
			      (or (file-directory-p dir)
				  (progn
				    (elmo-make-directory dir)
				    (set-file-modes dir 448)))))
		  '("." "new" "cur" "tmp"))
	  t)
      (error))))

(defun elmo-maildir-delete-folder (spec)
  (let ((basedir (elmo-maildir-get-folder-directory spec)))
    (condition-case nil
	(let ((tmp-files (directory-files
			  (expand-file-name "tmp" basedir)
			  t "[^.].*")))
	  ;; Delete files in tmp.
	  (and tmp-files (mapcar 'delete-file tmp-files))
	  (mapcar
	   (function
	    (lambda (dir)
	      (setq dir (expand-file-name dir basedir))
	      (if (not (file-directory-p dir))
		  (error nil)
		(elmo-delete-directory dir t))))
	   '("new" "cur" "tmp" "."))
	  t)
      (error nil))))

(defun elmo-maildir-search (spec condition &optional from-msgs msgdb)
  (save-excursion
    (let* ((msgs (or from-msgs (elmo-maildir-list-folder spec)))
	   (loc-alist (if msgdb (elmo-msgdb-get-location msgdb)
			(elmo-msgdb-location-load (elmo-msgdb-expand-path
						   spec))))
	   (dir (elmo-maildir-get-folder-directory spec))
	   (i 0)
	   case-fold-search ret-val
	   percent num
	   (num (length msgs))
	   number-list msg-num)
      (setq number-list msgs)
      (while msgs
	(setq msg-num (car msgs))
	(if (elmo-file-field-condition-match
	     (elmo-maildir-number-to-filename
	      dir (car msgs) loc-alist)
	     condition (car msgs) number-list)
	    (setq ret-val (append ret-val (list msg-num))))
	(setq i (1+ i))
	(setq percent (/ (* i 100) num))
	(elmo-display-progress
	 'elmo-maildir-search "Searching..."
	 percent)
	(setq msgs (cdr msgs)))
      ret-val)))

;;; (maildir) -> maildir
(defun elmo-maildir-copy-msgs (dst-spec msgs src-spec
					&optional loc-alist same-number)
  (let (srcfile)
    (while msgs
      (setq srcfile
	    (elmo-maildir-get-msg-filename src-spec (car msgs) loc-alist))
      (elmo-copy-file
       ;; src file
       srcfile
       ;; dst file
       (expand-file-name
	(file-name-nondirectory srcfile)
	(concat (elmo-maildir-get-folder-directory dst-spec) "/cur")))
      (setq msgs (cdr msgs))))
  t)

(defun elmo-maildir-use-cache-p (spec number)
  nil)

(defun elmo-maildir-local-file-p (spec number)
  t)

(defun elmo-maildir-get-msg-filename (spec number &optional loc-alist)
  (elmo-maildir-number-to-filename
   (elmo-maildir-get-folder-directory spec)
   number (or loc-alist (elmo-msgdb-location-load
			 (elmo-msgdb-expand-path
			  spec)))))

(defun elmo-maildir-pack-number (spec msgdb arg)
  (let ((old-number-alist (elmo-msgdb-get-number-alist msgdb))
	(old-overview (elmo-msgdb-get-overview msgdb))
	(old-mark-alist (elmo-msgdb-get-mark-alist msgdb))
	(old-location (elmo-msgdb-get-location msgdb))
	old-number overview number-alist mark-alist location
	mark (number 1))
    (setq overview old-overview)
    (while old-overview
      (setq old-number
	    (elmo-msgdb-overview-entity-get-number (car old-overview)))
      (elmo-msgdb-overview-entity-set-number (car old-overview) number)
      (setq number-alist
	    (cons (cons number (cdr (assq old-number old-number-alist)))
		  number-alist))
      (when (setq mark (cadr (assq old-number old-mark-alist)))
	(setq mark-alist
	      (elmo-msgdb-mark-append
	       mark-alist number mark)))
      (setq location
	    (cons (cons number (cdr (assq old-number old-location)))
		  location))
      (setq number (1+ number))
      (setq old-overview (cdr old-overview)))
    ;; XXX Should consider when folder is not persistent.
    (elmo-msgdb-location-save (elmo-msgdb-expand-path spec) location)
    (list overview
	  (nreverse number-alist)
	  (nreverse mark-alist)
	  (nreverse location)
	  (elmo-msgdb-make-overview-hashtb overview))))

(defalias 'elmo-maildir-sync-number-alist
  'elmo-generic-sync-number-alist)
(defalias 'elmo-maildir-list-folder-unread
  'elmo-generic-list-folder-unread)
(defalias 'elmo-maildir-list-folder-important
  'elmo-generic-list-folder-important)
(defalias 'elmo-maildir-commit 'elmo-generic-commit)
(defalias 'elmo-maildir-folder-diff 'elmo-generic-folder-diff)

(provide 'elmo-maildir)

;;; elmo-maildir.el ends here
