;;; elmo-flag.el --- global flag handling.

;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

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
(require 'elmo-util)
(require 'elmo-localdir)
(eval-when-compile (require 'cl))

;;; Code:
(defcustom elmo-global-flag-list '(important)
  "A list of flag symbol which is managed globally by the flag folder."
  :type '(repeat symbol)
  :group 'elmo)

(defvar elmo-global-flag-folder-alist nil
  "Internal variable to hold global-flag-folder structures.")

(eval-and-compile
  (luna-define-class elmo-flag-folder (elmo-localdir-folder)
		     (flag minfo minfo-hash))
  (luna-define-internal-accessors 'elmo-flag-folder))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-flag-folder)
					    name)
  (if (string-match "flag/\\([a-z]+\\)" name)
      (setq name (match-string 1 name))
    (setq name (symbol-name (car elmo-global-flag-list))))
  (or (cdr (assq (intern name) elmo-global-flag-folder-alist))
      (let (msgdb-path)
	(elmo-flag-folder-set-flag-internal folder (intern name))
	;; must be AFTER set flag slot.
	(setq msgdb-path (elmo-folder-msgdb-path folder))
	(unless (file-directory-p msgdb-path)
	  (elmo-make-directory msgdb-path))
	(elmo-localdir-folder-set-dir-name-internal
	 folder
	 msgdb-path)
	(elmo-localdir-folder-set-directory-internal
	 folder
	 msgdb-path)
	(if (file-exists-p (expand-file-name ".minfo" msgdb-path))
	    (elmo-flag-folder-set-minfo-internal
	     folder
	     (elmo-object-load (expand-file-name ".minfo" msgdb-path))))
	(elmo-flag-folder-set-minfo-hash-internal
	 folder
	 (elmo-make-hash (length (elmo-flag-folder-minfo-internal folder))))
	(dolist (elem (elmo-flag-folder-minfo-internal folder))
	  (elmo-set-hash-val (nth 1 elem) elem
			     (elmo-flag-folder-minfo-hash-internal folder))
	  (elmo-set-hash-val (concat "#" (number-to-string (nth 2 elem)))
			     elem
			     (elmo-flag-folder-minfo-hash-internal folder))
	  (dolist (pair (car elem))
	    (elmo-set-hash-val (concat (number-to-string (cdr pair))
				       ":" (car pair))
			       elem
			       (elmo-flag-folder-minfo-hash-internal folder))))
	(setq elmo-global-flag-folder-alist
	      (cons (cons (intern name) folder) elmo-global-flag-folder-alist))
	folder)))

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-flag-folder))
  (expand-file-name (concat "flag/"
			    (symbol-name
			     (elmo-flag-folder-flag-internal folder)))
		    elmo-msgdb-directory))

(luna-define-method elmo-folder-commit :after ((folder
						elmo-flag-folder))
  (elmo-object-save
   (expand-file-name ".minfo" (elmo-folder-msgdb-path folder))
   (elmo-flag-folder-minfo-internal folder)))

(defun elmo-flag-folder-delete-message (folder number
					       &optional keep-referrer)
  (let* ((elem (elmo-get-hash-val (concat "#" (number-to-string number))
				  (elmo-flag-folder-minfo-hash-internal
				   folder)))
	 target-folder key)
    (dolist (pair (car elem))
      (when (and (car pair) (cdr pair))
	(elmo-clear-hash-val (concat (number-to-string (cdr pair)) ":"
				     (car pair))
			     (elmo-flag-folder-minfo-hash-internal
			      folder))
	(unless keep-referrer
	  (setq target-folder (elmo-make-folder (car pair)))
	  (elmo-folder-open target-folder 'load-msgdb)
	  ;; Unset the flag of the original folder.
	  ;; (XXX Should the message-id checked?)
	  (elmo-message-unset-flag target-folder (cdr pair)
				   (elmo-flag-folder-flag-internal folder))
	  (elmo-folder-close target-folder))))
    (elmo-clear-hash-val (concat "#" (number-to-string number))
			 (elmo-flag-folder-minfo-hash-internal
			  folder))
    (elmo-clear-hash-val (nth 1 elem) (elmo-flag-folder-minfo-hash-internal
				       folder))
    (elmo-flag-folder-set-minfo-internal
     folder
     (delq elem (elmo-flag-folder-minfo-internal folder))))
  t)

(luna-define-method elmo-folder-delete-messages ((folder
						  elmo-flag-folder)
						 numbers)
  (dolist (number numbers)
    (elmo-flag-folder-delete-message folder number)
    (elmo-localdir-delete-message folder number))
  (elmo-folder-commit folder)
  t)

;; Same as localdir except that the flag is always the flag.
(luna-define-method elmo-folder-msgdb-create ((folder elmo-flag-folder)
					      numbers
					      flag-table)
  (when numbers
    (let ((dir (elmo-localdir-folder-directory-internal folder))
	  (new-msgdb (elmo-make-msgdb))
	  entity (i 0)
	  (len (length numbers)))
      (message "Creating msgdb...")
      (while numbers
	(when (setq entity (elmo-localdir-msgdb-create-entity
			    new-msgdb dir (car numbers)))
	  (elmo-msgdb-append-entity new-msgdb entity
				    (list (elmo-flag-folder-flag-internal
					   folder))))
	(when (> len elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (elmo-display-progress
	   'elmo-flag-folder-msgdb-create "Creating msgdb..."
	   (/ (* i 100) len)))
	(setq numbers (cdr numbers)))
      (message "Creating msgdb...done")
      new-msgdb)))

(luna-define-method elmo-folder-append-messages ((folder elmo-flag-folder)
						 src-folder
						 numbers
						 &optional same-number)
  (dolist (number numbers)
    (elmo-global-flag-set (elmo-flag-folder-flag-internal folder)
			  src-folder number (elmo-message-field
					     src-folder
					     number
					     'message-id)))
  numbers)

(luna-define-method elmo-folder-append-buffer ((folder elmo-flag-folder)
					       unread
					       &optional number)
  (error "Cannot append to the flag folder"))

;;; Utilities

(defmacro elmo-flag-get-folder (flag)
  "Get the flag folder structure for FLAG."
  `(when (memq ,flag elmo-global-flag-list)
     (elmo-make-folder (concat  "'flag/" (symbol-name ,flag)))))

(defun elmo-flag-folder-referrer (folder number)
  "Return a list of referrer message information.
Each element is a cons cell like following:
\(FNAME . NUMBER\)
FNAME is the name of the folder which the message is contained.
NUMBER is the number of the message."
  (when (eq (elmo-folder-type-internal folder) 'flag)
    (car (elmo-get-hash-val (concat "#" (number-to-string number))
			    (elmo-flag-folder-minfo-hash-internal
			     folder)))))

;;; Global-Flag API
(defun elmo-global-flag-initialize ()
  "Initialize flag folders.
This function is necessary to be called before using `elmo-flag-folder'."
  (unless elmo-global-flag-folder-alist
    (dolist (flag elmo-global-flag-list)
      (setq elmo-global-flag-folder-alist
	    (cons (elmo-make-folder
		   (concat "'flag/" (symbol-name flag)))
		  elmo-global-flag-folder-alist)))))

(defun elmo-global-flag-p (flag)
  "Return non-nil when FLAG is global."
  (memq flag elmo-global-flag-list))

(defun elmo-global-flags (fname number)
  "Return a list of global flags for the message.
FNAME is the name string of the folder.
NUMBER is the number of the message."
  (let ((flag-list elmo-global-flag-list)
	folder matches)
    (while flag-list
      (setq folder (elmo-flag-get-folder (car flag-list)))
      (when (elmo-get-hash-val
	     (concat (number-to-string number) ":" fname)
	     (elmo-flag-folder-minfo-hash-internal folder))
	(setq matches (cons (elmo-flag-folder-flag-internal folder)
			    matches)))
      (setq flag-list (cdr flag-list)))
    matches))

(defun elmo-folder-list-global-flag-messages (folder flag)
  "List messages which have global flag.
FOLDER is the elmo folder structure.
FLAG is the symbol of the flag."
  (when (elmo-global-flag-p flag)
    (let ((flag-folder (elmo-flag-get-folder flag))
	  result entity)
      (dolist (elem (elmo-flag-folder-minfo-internal flag-folder))
	(if (setq entity (elmo-message-entity folder (nth 1 elem)))
	    (setq result (cons (elmo-message-entity-number entity)
			       result))))
      result)))

;;;
;; minfo is a list of following cell.
;; ((((FNAME . NUMBER)...(FNAME . NUMBER)) MESSAGE-ID NUMBER-IN-FLAG-FOLDER)
;; minfo-index is the hash table of above with following indice;
(defun elmo-global-flags-set (flags folder number message-id)
  "Set global flags to the message.
FLAGS is a list of symbol of the flag.
FOLDER is the elmo folder structure.
NUMBER is the message number."
  (dolist (flag flags)
    (elmo-global-flag-set flag folder number message-id)))

(defsubst elmo-global-flag-set-internal (flag folder number message-id)
  (when message-id
    (let ((flag-folder (elmo-flag-get-folder flag))
	  cache new-file new-number elem)
      (if (setq elem (elmo-get-hash-val
		      message-id
		      (elmo-flag-folder-minfo-hash-internal
		       flag-folder)))
	  ;; Same ID already exists.
	  (when (and folder number
		     (not (member (cons (elmo-folder-name-internal folder)
					number) (car elem))))
	    (setcar elem
		    (cons (cons (elmo-folder-name-internal folder)
				number) (car elem)))
	    (setq new-number (nth 2 elem))
	    (elmo-set-hash-val (concat (number-to-string number)
				       ":" (elmo-folder-name-internal
					    folder))
			       elem
			       (elmo-flag-folder-minfo-hash-internal
				flag-folder)))
	;; Append new element.
	(setq new-file
	      (expand-file-name
	       (int-to-string
		(setq new-number (1+ (car (elmo-folder-status flag-folder)))))
	       (elmo-localdir-folder-directory-internal flag-folder)))
	(with-temp-buffer
	  (setq cache (and message-id (elmo-file-cache-get message-id)))
	  (if (and cache (eq (elmo-file-cache-status cache) 'entire))
	      (elmo-copy-file (elmo-file-cache-path cache)
			      new-file)
	    (when (and folder number)
	      (elmo-message-fetch folder number (elmo-make-fetch-strategy
						 'entire)
				  nil (current-buffer))
	      (write-region-as-binary (point-min) (point-max) new-file nil
				      'no-msg))))
	(elmo-flag-folder-set-minfo-internal
	 flag-folder
	 (cons
	  (setq elem (list
		      (when (and folder number)
			(list (cons (elmo-folder-name-internal folder)
				    number)))
		      message-id
		      new-number))
	  (elmo-flag-folder-minfo-internal flag-folder)))
	(when (and folder number)
	  (elmo-set-hash-val (concat (number-to-string number)
				     ":" (elmo-folder-name-internal
					  folder))
			     elem
			     (elmo-flag-folder-minfo-hash-internal
			      flag-folder)))
	(elmo-set-hash-val message-id elem
			   (elmo-flag-folder-minfo-hash-internal
			    flag-folder))
	(elmo-set-hash-val (concat "#" (number-to-string new-number)) elem
			   (elmo-flag-folder-minfo-hash-internal
			    flag-folder)))
      (elmo-folder-commit flag-folder)
      new-number)))

(defun elmo-global-flag-set (flag folder number message-id)
  "Set global flag to the message.
FLAG is a symbol of the flag.
FOLDER is the elmo folder structure.
NUMBER is the message number.
MESSAGE-ID is the message-id of the message."
  (when (elmo-global-flag-p flag)
    (elmo-global-flag-set-internal flag folder number message-id)))

(defun elmo-global-flag-detach (flag folder number &optional delete-if-none)
  "Detach the message from the global flag.
FOLDER is the folder structure.
NUMBERS is the message number.
If optional DELETE-IF-NONE is non-nil, delete message from flag folder when
the message is not flagged in any folder.
If DELETE-IF-NONE is a symbol `always',
delete message without flagged in other folder."
  (unless (eq (elmo-folder-type-internal folder) 'flag)
    (let ((flag-folder (elmo-flag-get-folder flag))
	  elem key)
      (when flag-folder
	(setq key (concat (number-to-string number) ":"
			  (elmo-folder-name-internal folder))
	      elem (elmo-get-hash-val
		    key
		    (elmo-flag-folder-minfo-hash-internal flag-folder)))
	(when elem
	  (setcar elem (delete (cons (elmo-folder-name-internal folder)
				     number) (car elem)))
	  (elmo-clear-hash-val key (elmo-flag-folder-minfo-hash-internal
				    flag-folder))
	  ;; Does not have any referrer, remove.
	  (when (and delete-if-none
		     (or (eq delete-if-none 'always)
			 (null (car elem))))
	    (elmo-flag-folder-delete-message flag-folder (nth 2 elem)
					     (null (car elem)))
	    (elmo-localdir-delete-message flag-folder (nth 2 elem))
	    (elmo-folder-commit flag-folder)))))))

(defun elmo-global-flag-detach-messages (folder numbers &optional
						delete-if-none)
  "Detach all messages specified from all global flags.
FOLDER is the folder structure.
NUMBERS is the message number list.
If optional DELETE-IF-NONE is non-nil, delete message from flag folder when
the message is not flagged in any folder."
  (unless (eq (elmo-folder-type-internal folder) 'flag)
    (dolist (flag elmo-global-flag-list)
      (dolist (number numbers)
	(elmo-global-flag-detach flag folder number delete-if-none)))))

;;; To migrate from global mark folder
(defvar elmo-global-mark-filename "global-mark"
  "Obsolete variable. (Just for migration)")

(defun elmo-global-mark-upgrade ()
  "Upgrade old `global-mark' structure."
  (interactive)
  (when (file-exists-p (expand-file-name
			elmo-global-mark-filename elmo-msgdb-directory))
    (message "Upgrading flag structure...")
    (elmo-global-flag-initialize)
    (when (elmo-global-flag-p 'important)
      (let ((global-marks
	     (elmo-object-load
	      (expand-file-name
	       elmo-global-mark-filename elmo-msgdb-directory)))
	    (folder (elmo-flag-get-folder 'important))
	    file-cache)
	(dolist (elem global-marks)
	  (setq file-cache (elmo-file-cache-get (car elem)))
	  (when (eq (elmo-file-cache-status file-cache) 'entire)
	    (elmo-global-flag-set 'important nil nil (car elem))))))
    (message "Upgrading flag structure...done")))

(require 'product)
(product-provide (provide 'elmo-flag) (require 'elmo-version))

;;; elmo-flag.el ends here
