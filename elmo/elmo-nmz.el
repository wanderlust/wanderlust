;;; elmo-nmz.el --- Namazu interface for ELMO.

;; Copyright (C) 2000 Yuuichi Teranishi <teranisi@gohome.org>

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

(require 'elmo)
(require 'elmo-map)
(require 'mime-edit)

(defcustom elmo-nmz-default-index-path "~/Mail"
  "*Default index path for namazu.
If the value is a list, all elements are used as index paths for namazu."
  :type 'directory
  :group 'elmo)

(defcustom elmo-nmz-prog "namazu"
  "*Program name of namazu."
  :type 'string
  :group 'elmo)

(defcustom elmo-nmz-charset 'iso-2022-jp
  "*Charset for namazu argument."
  :type 'symbol
  :group 'elmo)

(defcustom elmo-nmz-args '("--all" "--list" "--late")
  "*Argument list for namazu to list matched files."
  :type '(repeat string)
  :group 'elmo)

(defcustom elmo-nmz-index-alias-alist nil
  "*Alist of ALIAS and INDEX-PATH."
  :type '(repeat (cons (string :tag "Alias Name")
		       (choice (directory :tag "Index Path")
			       (repeat (directory :tag "Index Path")))))
  :group 'elmo)

(defvar elmo-nmz-use-drive-letter (if (memq system-type
					    '(OS/2 emx windows-nt)) t nil)
  "*If non-nil, do a drive letter conversion (e.g. /a|/ => a:/).")

;;; "namazu search"
(eval-and-compile
  (luna-define-class elmo-nmz-folder
		     (elmo-map-folder) (pattern index-path))
  (luna-define-internal-accessors 'elmo-nmz-folder))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-nmz-folder)
					    name)
  (when (> (length name) 0)
    (with-temp-buffer
      (insert "[" name)
      (goto-char (point-min))
      (forward-sexp)
      (elmo-nmz-folder-set-pattern-internal folder
					    (buffer-substring
					     (+ 1 (point-min))
					     (- (point) 1)))
      (let ((index (buffer-substring (point) (point-max))))
	(elmo-nmz-folder-set-index-path-internal
	 folder
	 (cond ((cdr (assoc index elmo-nmz-index-alias-alist)))
	       ((eq (length index) 0)
		elmo-nmz-default-index-path)
	       (t
		index))))))
  folder)

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-nmz-folder))
  (expand-file-name
   (elmo-replace-string-as-filename
    (elmo-folder-name-internal folder))
   (expand-file-name "nmz" elmo-msgdb-directory)))

(defun elmo-nmz-msgdb-create-entity (msgdb folder number)
  "Create msgdb entity for the message in the FOLDER with NUMBER."
  (let ((location (expand-file-name (elmo-map-message-location folder number)))
	entity uid)
    (setq entity (elmo-msgdb-create-message-entity-from-file
		  (elmo-msgdb-message-entity-handler msgdb) number location))
    (unless (or (> (length (elmo-message-entity-field entity 'to)) 0)
		(> (length (elmo-message-entity-field entity 'cc)) 0)
		(not (string= (elmo-message-entity-field entity 'subject)
			      elmo-no-subject)))
      (elmo-message-entity-set-field entity 'subject location)
      (setq uid (nth 2 (file-attributes location)))
      (elmo-message-entity-set-field entity 'from
				     (concat
				      (user-full-name uid)
				      " <"(user-login-name uid) "@"
				      (system-name) ">")))
    entity))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-nmz-folder)
					      numlist flag-table)
  (let ((new-msgdb (elmo-make-msgdb))
	entity mark i percent num)
    (setq num (length numlist))
    (setq i 0)
    (message "Creating msgdb...")
    (while numlist
      (setq entity
	    (elmo-nmz-msgdb-create-entity
	     new-msgdb folder (car numlist)))
      (when entity
	(elmo-msgdb-append-entity new-msgdb entity '(new unread)))
      (when (> num elmo-display-progress-threshold)
	(setq i (1+ i))
	(setq percent (/ (* i 100) num))
	(elmo-display-progress
	 'elmo-folder-msgdb-create "Creating msgdb..."
	 percent))
      (setq numlist (cdr numlist)))
    (message "Creating msgdb...done")
    new-msgdb))

(luna-define-method elmo-folder-message-file-p ((folder elmo-nmz-folder))
  t)

(luna-define-method elmo-message-file-name ((folder elmo-nmz-folder)
					    number)
  (elmo-map-message-location folder number))

(luna-define-method elmo-folder-message-make-temp-file-p
  ((folder elmo-nmz-folder))
  t)

(luna-define-method elmo-folder-diff ((folder elmo-nmz-folder))
  (cons nil nil))

(luna-define-method elmo-folder-message-make-temp-files ((folder
							  elmo-nmz-folder)
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

(luna-define-method elmo-map-message-fetch ((folder elmo-nmz-folder)
					    location strategy
					    &optional section unseen)
  (when (file-exists-p location)
    (prog1
	(insert-file-contents-as-binary (expand-file-name location))
      (unless (or (std11-field-body "To")
		  (std11-field-body "Cc")
		  (std11-field-body "Subject"))
	(let (charset guess uid)
	  (erase-buffer)
	  (set-buffer-multibyte t)
	  (insert-file-contents (expand-file-name location))
	  (setq charset (detect-mime-charset-region (point-min)
						    (point-max)))
	  (goto-char (point-min))
	  (setq guess (mime-find-file-type location))
	  (setq uid (nth 2 (file-attributes location)))
	  (insert "From: " (concat (user-full-name uid)
				   " <"(user-login-name uid) "@"
				   (system-name) ">") "\n")
	  (insert "Subject: " location "\n")
	  (insert "Content-Type: "
		  (concat (nth 0 guess) "/" (nth 1 guess))
		  "; charset=" (upcase (symbol-name charset))
		  "\nMIME-Version: 1.0\n\n")
	  (encode-mime-charset-region (point-min) (point-max) charset)
	  (set-buffer-multibyte nil))))))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-nmz-folder))
  (let (bol locations)
    (with-temp-buffer
      (apply 'call-process elmo-nmz-prog nil t t
	     (append elmo-nmz-args
		     (list
		      (encode-mime-charset-string
		       (elmo-nmz-folder-pattern-internal folder)
		       elmo-nmz-charset))
		     (if (listp (elmo-nmz-folder-index-path-internal folder))
			 (mapcar
			  'expand-file-name
			  (elmo-nmz-folder-index-path-internal folder))
		       (list
			(expand-file-name
			 (elmo-nmz-folder-index-path-internal folder))))))
      (goto-char (point-min))
      (while (not (eobp))
	(beginning-of-line)
	;; convert url to file path.
	(when (looking-at "^file://")
	  (replace-match ""))
	(when (and elmo-nmz-use-drive-letter
		   (looking-at "^/\\([A-Za-z]\\)|/"))
	  (replace-match "\\1:/")
	  (beginning-of-line))
	(setq bol (point))
	(end-of-line)
	(setq locations (cons (buffer-substring bol (point)) locations))
	(forward-line 1))
      (nreverse locations))))

(luna-define-method elmo-folder-exists-p ((folder elmo-nmz-folder))
  (elmo-nmz-folder-pattern-internal folder))

(luna-define-method elmo-folder-have-subfolder-p ((folder elmo-nmz-folder))
  (null (elmo-nmz-folder-pattern-internal folder)))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-nmz-folder)
						 &optional one-level)
  (mapcar (lambda (name) (elmo-recover-string-from-filename name))
	  (directory-files (expand-file-name "nmz" elmo-msgdb-directory)
			   nil
			   (concat
			    "^"
			    (regexp-quote
			     (elmo-folder-prefix-internal folder))))))

(require 'product)
(product-provide (provide 'elmo-nmz) (require 'elmo-version))

;;; elmo-nmz.el ends here
