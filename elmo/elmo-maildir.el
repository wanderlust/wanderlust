;;; elmo-maildir.el --- Maildir interface for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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
(require 'elmo)
(require 'elmo-map)

;;; ELMO Maildir folder
(eval-and-compile
  (luna-define-class elmo-maildir-folder
		     (elmo-map-folder)
		     (directory unread-locations flagged-locations))
  (luna-define-internal-accessors 'elmo-maildir-folder))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-maildir-folder)
					    name)
  (if (file-name-absolute-p name)
      (elmo-maildir-folder-set-directory-internal
       folder
       (expand-file-name name))
    (elmo-maildir-folder-set-directory-internal
     folder
     (expand-file-name
      name
      elmo-maildir-folder-path)))
  folder)

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-maildir-folder))
  (expand-file-name
   (elmo-replace-string-as-filename
    (elmo-maildir-folder-directory-internal folder))
   (expand-file-name
    "maildir"
    elmo-msgdb-directory)))

(defun elmo-maildir-message-file-name (folder location)
  "Get a file name of the message from FOLDER which corresponded to
LOCATION."
  (let ((file (file-name-completion
	       location
	       (expand-file-name
		"cur"
		(elmo-maildir-folder-directory-internal folder)))))
    (if file
	(expand-file-name
	 (if (eq file t) location file)
	 (expand-file-name
	  "cur"
	  (elmo-maildir-folder-directory-internal folder))))))

(defsubst elmo-maildir-list-location (dir &optional child-dir)
  (let* ((cur-dir (expand-file-name (or child-dir "cur") dir))
	 (cur (directory-files cur-dir
			       nil "^[^.].*$" t))
	 unread-locations flagged-locations seen flagged sym
	 locations)
    (setq locations
	  (mapcar
	   (lambda (x)
	     (if (string-match "^\\([^:]+\\):\\([^:]+\\)$" x)
		 (progn
		   (setq seen nil)
		   (save-match-data
		     (cond
		      ((string-match "S" (elmo-match-string 2 x))
		       (setq seen t))
		      ((string-match "F" (elmo-match-string 2 x))
		       (setq flagged t))))
		   (setq sym (elmo-match-string 1 x))
		   (unless seen (setq unread-locations
				      (cons sym unread-locations)))
		   (if flagged (setq flagged-locations
				     (cons sym flagged-locations)))
		   sym)
	       x))
	   cur))
    (list locations unread-locations flagged-locations)))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-maildir-folder))
  (elmo-maildir-update-current folder)
  (let ((locs (elmo-maildir-list-location
	       (elmo-maildir-folder-directory-internal folder))))
    ;; 0: locations, 1: unread-locations, 2: flagged-locations
    (elmo-maildir-folder-set-unread-locations-internal folder (nth 1 locs))
    (elmo-maildir-folder-set-flagged-locations-internal folder (nth 2 locs))
    (nth 0 locs)))

(luna-define-method elmo-map-folder-list-unreads
  ((folder elmo-maildir-folder))
  (elmo-maildir-folder-unread-locations-internal folder))

(luna-define-method elmo-map-folder-list-importants
  ((folder elmo-maildir-folder))
  (elmo-maildir-folder-flagged-locations-internal folder))

(luna-define-method elmo-folder-msgdb-create 
  ((folder elmo-maildir-folder)
   numbers new-mark already-mark seen-mark important-mark seen-list)
  (let* ((unread-list (elmo-maildir-folder-unread-locations-internal folder))
	 (flagged-list (elmo-maildir-folder-flagged-locations-internal folder))
	 (len (length numbers))
	 (i 0)
	 overview number-alist mark-alist entity
	 location pair mark)
    (message "Creating msgdb...")
    (dolist
	(number numbers)
      (setq location (elmo-map-message-location folder number))
      (setq entity
	    (elmo-msgdb-create-overview-entity-from-file
	     number
	     (elmo-maildir-message-file-name folder location)))
      (when entity
	(setq overview
	      (elmo-msgdb-append-element overview entity))
	(setq number-alist
	      (elmo-msgdb-number-add number-alist
				     (elmo-msgdb-overview-entity-get-number
				      entity)
				     (elmo-msgdb-overview-entity-get-id
				      entity)))
	(cond 
	 ((member location unread-list)
	  (setq mark new-mark)) ; unread!
	 ((member location flagged-list)
	  (setq mark important-mark)))
	(if (setq mark (or (elmo-msgdb-global-mark-get
			    (elmo-msgdb-overview-entity-get-id
			     entity))
			   mark))
	    (setq mark-alist
		  (elmo-msgdb-mark-append
		   mark-alist
		   (elmo-msgdb-overview-entity-get-number
		    entity)
		   mark)))
	(when (> len elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (elmo-display-progress
	   'elmo-maildir-msgdb-create "Creating msgdb..."
	   (/ (* i 100) len)))))
    (message "Creating msgdb...done")
    (elmo-msgdb-sort-by-date
     (list overview number-alist mark-alist))))

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

(defun elmo-maildir-update-current (folder)
  "Move all new msgs to cur in the maildir."
  (let* ((maildir (elmo-maildir-folder-directory-internal folder))
	 (news (directory-files (expand-file-name "new"
						  maildir)
				nil
				"^[^.].*$" t)))
    ;; cleanup tmp directory.
    (elmo-maildir-cleanup-temporal maildir)
    ;; move new msgs to cur directory.
    (while news
      (rename-file
       (expand-file-name (car news) (expand-file-name "new" maildir))
       (expand-file-name (concat (car news) ":2,")
			 (expand-file-name "cur" maildir)))
      (setq news (cdr news)))))

(defun elmo-maildir-set-mark (filename mark)
  "Mark the FILENAME file in the maildir.  MARK is a character."
  (if (string-match "^\\([^:]+:[12],\\)\\(.*\\)$" filename)
      (let ((flaglist (string-to-char-list (elmo-match-string
					    2 filename))))
	(unless (memq mark flaglist)
	  (setq flaglist (sort (cons mark flaglist) '<))
	  (rename-file filename
		       (concat (elmo-match-string 1 filename)
			       (char-list-to-string flaglist)))))
    ;; Rescue no info file in maildir.
    (rename-file filename
		 (concat filename ":2," (char-to-string mark))))
  t)

(defun elmo-maildir-delete-mark (filename mark)
  "Mark the FILENAME file in the maildir.  MARK is a character."
  (if (string-match "^\\([^:]+:2,\\)\\(.*\\)$" filename)
      (let ((flaglist (string-to-char-list (elmo-match-string
					    2 filename))))
	(when (memq mark flaglist)
	  (setq flaglist (delq mark flaglist))
	  (rename-file filename
		       (concat (elmo-match-string 1 filename)
			       (if flaglist
				   (char-list-to-string flaglist))))))))

(defsubst elmo-maildir-set-mark-msgs (folder locs mark)
  (dolist (loc locs)
    (elmo-maildir-set-mark
     (elmo-maildir-message-file-name folder loc)
     mark))
  t)

(defsubst elmo-maildir-delete-mark-msgs (folder locs mark)
  (dolist (loc locs)
    (elmo-maildir-delete-mark
     (elmo-maildir-message-file-name folder loc)
     mark))
  t)

(luna-define-method elmo-map-folder-mark-as-important ((folder elmo-maildir-folder)
						       locs)
  (elmo-maildir-set-mark-msgs folder locs ?F))
  
(luna-define-method elmo-map-folder-unmark-important ((folder elmo-maildir-folder)
						      locs)
  (elmo-maildir-delete-mark-msgs folder locs ?F))

(luna-define-method elmo-map-folder-mark-as-read ((folder elmo-maildir-folder)
						  locs)
  (elmo-maildir-set-mark-msgs folder locs ?S))

(luna-define-method elmo-map-folder-unmark-read ((folder elmo-maildir-folder)
						 locs)
  (elmo-maildir-delete-mark-msgs folder locs ?S))

(luna-define-method elmo-folder-list-subfolders
  ((folder elmo-maildir-folder) &optional one-level)
  (let ((prefix (concat (elmo-folder-name-internal folder)
			(unless (string= (elmo-folder-prefix-internal folder)
					 (elmo-folder-name-internal folder))
			  elmo-path-sep)))
	(elmo-list-subdirectories-ignore-regexp
	 "^\\(\\.\\.?\\|cur\\|tmp\\|new\\)$")
	elmo-have-link-count)
    (append
     (list (elmo-folder-name-internal folder))
     (elmo-mapcar-list-of-list
      (function (lambda (x) (concat prefix x)))
      (elmo-list-subdirectories
       (elmo-maildir-folder-directory-internal folder)
       ""
       one-level)))))

(defvar elmo-maildir-sequence-number-internal 0)

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
;;; I don't want to wait.
;;;   (sleep-for 2)
      (setq filename
	    (expand-file-name
	     (concat "tmp/" (elmo-maildir-make-unique-string))
	     basedir)))
    filename))

(luna-define-method elmo-folder-append-buffer ((folder elmo-maildir-folder)
					       unread &optional number)
  (let ((basedir (elmo-maildir-folder-directory-internal folder))
	(src-buf (current-buffer))
	dst-buf filename)
    (condition-case nil
	(with-temp-buffer
	  (setq filename (elmo-maildir-temporal-filename basedir))
	  (setq dst-buf (current-buffer))
	  (with-current-buffer src-buf
	    (copy-to-buffer dst-buf (point-min) (point-max)))
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

(luna-define-method elmo-folder-message-file-p ((folder elmo-maildir-folder))
  t)

(luna-define-method elmo-message-file-name ((folder elmo-maildir-folder)
					    number)
  (elmo-maildir-message-file-name
   folder
   (elmo-map-message-location folder number)))

(luna-define-method elmo-folder-message-make-temp-file-p
  ((folder elmo-maildir-folder))
  t)

(luna-define-method elmo-folder-message-make-temp-files ((folder
							  elmo-maildir-folder)
							 numbers
							 &optional
							 start-number)
  (let ((temp-dir (elmo-folder-make-temporary-directory folder))
	(cur-number (if start-number 0)))
    (dolist (number numbers)
      (elmo-copy-file
       (elmo-message-file-name folder number)
       (expand-file-name
	(int-to-string (if start-number (incf cur-number) number))
	temp-dir)))
    temp-dir))

(luna-define-method elmo-folder-append-messages :around
  ((folder elmo-maildir-folder)
   src-folder numbers unread-marks &optional same-number)
  (if (elmo-folder-message-file-p src-folder)
      (let ((dir (elmo-maildir-folder-directory-internal folder))
	    (succeeds numbers)
	    filename)
	(dolist (number numbers)
	  (setq filename (elmo-maildir-temporal-filename dir))
	  (elmo-copy-file
	   (elmo-message-file-name src-folder number)
	   filename)
	  (elmo-add-name-to-file
	   filename
	   (expand-file-name
	    (concat "new/" (file-name-nondirectory filename))
	    dir))
	  (elmo-progress-notify 'elmo-folder-move-messages))
	succeeds)
    (luna-call-next-method)))

(luna-define-method elmo-map-folder-delete-messages
  ((folder elmo-maildir-folder) locations)
  (let (file)
    (dolist (location locations)
      (setq file (elmo-maildir-message-file-name folder location))
      (if (and file
	       (file-writable-p file)
	       (not (file-directory-p file)))
	  (delete-file file)))))

(luna-define-method elmo-map-message-fetch ((folder elmo-maildir-folder)
					    location strategy
					    &optional section unseen)
  (let ((file (elmo-maildir-message-file-name folder location)))
    (when (file-exists-p file)
      (insert-file-contents-as-binary file))))

(luna-define-method elmo-folder-exists-p ((folder elmo-maildir-folder))
  (let ((basedir (elmo-maildir-folder-directory-internal folder)))
    (and (file-directory-p (expand-file-name "new" basedir))
	 (file-directory-p (expand-file-name "cur" basedir))
	 (file-directory-p (expand-file-name "tmp" basedir)))))

(luna-define-method elmo-folder-diff ((folder elmo-maildir-folder)
				      &optional numbers)
  (let* ((dir (elmo-maildir-folder-directory-internal folder))
	 (new-len (length (car (elmo-maildir-list-location dir "new"))))
	 (cur-len (length (car (elmo-maildir-list-location dir "cur")))))
    (cons new-len (+ new-len cur-len))))

(luna-define-method elmo-folder-creatable-p ((folder elmo-maildir-folder))
  t)

(luna-define-method elmo-folder-writable-p ((folder elmo-maildir-folder))
  t)

(luna-define-method elmo-folder-create ((folder elmo-maildir-folder))
  (let ((basedir (elmo-maildir-folder-directory-internal folder)))
    (condition-case nil
	(progn
	  (dolist (dir '("." "new" "cur" "tmp"))
	    (setq dir (expand-file-name dir basedir))
	    (or (file-directory-p dir)
		(progn
		  (elmo-make-directory dir)
		  (set-file-modes dir 448))))
	  t)
      (error))))

(luna-define-method elmo-folder-delete ((folder elmo-maildir-folder))
  (let ((basedir (elmo-maildir-folder-directory-internal folder)))
    (condition-case nil
	(let ((tmp-files (directory-files
			  (expand-file-name "tmp" basedir)
			  t "[^.].*")))
	  ;; Delete files in tmp.
	  (dolist (file tmp-files)
	    (delete-file file))
	  (dolist (dir '("new" "cur" "tmp" "."))
	    (setq dir (expand-file-name dir basedir))
	    (if (not (file-directory-p dir))
		(error nil)
	      (elmo-delete-directory dir t)))
	  t)
      (error nil))))

(luna-define-method elmo-folder-search ((folder elmo-maildir-folder)
					condition &optional numbers)
  (save-excursion
    (let* ((msgs (or numbers (elmo-folder-list-messages folder)))
	   (i 0)
	   case-fold-search matches
	   percent num
	   (len (length msgs))
	   number-list msg-num)
      (setq number-list msgs)
      (dolist (number numbers)
	(if (elmo-file-field-condition-match
	     (elmo-message-file-name folder number)
	     condition number number-list)
	    (setq matches (cons number matches)))
	(setq i (1+ i))
	(elmo-display-progress
	 'elmo-maildir-search "Searching..."
	 (/ (* i 100) len)))
      (nreverse matches))))

(require 'product)
(product-provide (provide 'elmo-maildir) (require 'elmo-version))

;;; elmo-maildir.el ends here
