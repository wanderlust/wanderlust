;;; elmo-mark.el --- Global mark folder for ELMO.

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
(require 'elmo)
(require 'elmo-map)

(defcustom elmo-mark-default-mark "$"
  "*Default global-mark for mark-folder."
  :type 'string
  :group 'elmo)

;;; ELMO mark folder
(eval-and-compile
  (luna-define-class elmo-mark-folder (elmo-map-folder) (mark))
  (luna-define-internal-accessors 'elmo-mark-folder))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-mark-folder)
					    name)
  (elmo-mark-folder-set-mark-internal
   folder
   elmo-mark-default-mark)
  folder)

(luna-define-method elmo-folder-have-subfolder-p ((folder elmo-mark-folder))
  nil)

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-mark-folder))
  (expand-file-name "mark"
		    (expand-file-name "internal"
				      elmo-msgdb-directory)))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-mark-folder))
  (elmo-mark-folder-list-message-locations folder))

(defun elmo-mark-folder-list-message-locations (folder)
  (let (result)
    (dolist (pair (or elmo-msgdb-global-mark-alist
		      (setq elmo-msgdb-global-mark-alist
			    (elmo-object-load
			     (expand-file-name
			      elmo-msgdb-global-mark-filename
			      elmo-msgdb-directory)))))
      (if (string= (elmo-mark-folder-mark-internal folder)
		   (cdr pair))
	  (setq result (cons (car pair) result))))
    (nreverse result)))

(luna-define-method elmo-folder-message-file-p ((folder elmo-mark-folder))
  t)

(luna-define-method elmo-message-file-name ((folder elmo-mark-folder)
					    number)
  (elmo-file-cache-get-path
   (elmo-map-message-location folder number)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-mark-folder)
					      numbers flag-table)
  (elmo-mark-folder-msgdb-create folder numbers))

(defun elmo-mark-folder-msgdb-create (folder numbers)
  (let ((i 0)
	(len (length numbers))
	overview number-alist mark-alist entity message-id
	num)
    (message "Creating msgdb...")
    (while numbers
      (setq entity
	    (elmo-msgdb-create-overview-entity-from-file
	     (car numbers) (elmo-message-file-name folder (car numbers))))
      (if (null entity)
	  ()
	(setq num (elmo-msgdb-overview-entity-get-number entity))
	(setq overview
	      (elmo-msgdb-append-element
	       overview entity))
	(setq message-id (elmo-msgdb-overview-entity-get-id entity))
	(setq number-alist
	      (elmo-msgdb-number-add number-alist
				     num
				     message-id))
	(setq mark-alist
	      (elmo-msgdb-mark-append
	       mark-alist
	       num (elmo-mark-folder-mark-internal folder))))
      (when (> len elmo-display-progress-threshold)
	(setq i (1+ i))
	(elmo-display-progress
	 'elmo-mark-folder-msgdb-create "Creating msgdb..."
	 (/ (* i 100) len)))
      (setq numbers (cdr numbers)))
    (message "Creating msgdb...done")
    (list overview number-alist mark-alist)))

(luna-define-method elmo-folder-append-buffer ((folder elmo-mark-folder)
					       &optional flag number)
  (let* ((msgid (elmo-field-body "message-id"))
	 (path (elmo-file-cache-get-path msgid))
	 dir)
    (when path
      (setq dir (directory-file-name (file-name-directory path)))
      (unless (file-exists-p dir)
	(elmo-make-directory dir))
      (when (file-writable-p path)
	(write-region-as-binary (point-min) (point-max)
				path nil 'no-msg)))
    (elmo-msgdb-global-mark-set msgid
				(elmo-mark-folder-mark-internal folder))))

(luna-define-method elmo-map-folder-delete-messages ((folder elmo-mark-folder)
						     locations)
  (dolist (location locations)
    (elmo-msgdb-global-mark-delete location)))

(luna-define-method elmo-message-fetch-with-cache-process
  ((folder elmo-mark-folder) number strategy &optional section unseen)
  ;; disbable cache process
  (elmo-message-fetch-internal folder number strategy section unseen))

(luna-define-method elmo-map-message-fetch ((folder elmo-mark-folder)
					    location strategy
					    &optional section unseen)
  (let ((file (elmo-file-cache-get-path location)))
    (when (file-exists-p file)
      (insert-file-contents-as-binary file))))

(luna-define-method elmo-folder-exists-p ((folder elmo-mark-folder))
  t)

(luna-define-method elmo-folder-writable-p ((folder elmo-mark-folder))
  t)

(require 'product)
(product-provide (provide 'elmo-mark) (require 'elmo-version))

;;; elmo-mark.el ends here
