;;; elmo-cache.el --- Cache modules for ELMO.

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
(require 'elmo)
(require 'elmo-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cache backend by Kenichi OKADA <okada@opaopa.org>
;;
(eval-and-compile
  (luna-define-class elmo-cache-folder (elmo-map-folder) (dir-name directory))
  (luna-define-internal-accessors 'elmo-cache-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-cache-folder)
					    name)
  (when (string-match "\\([^/]*\\)/?\\(.*\\)$" name)
    (elmo-cache-folder-set-dir-name-internal
     folder
     (elmo-match-string 2 name))
    (elmo-cache-folder-set-directory-internal
     folder
     (expand-file-name (elmo-match-string 2 name)
		       elmo-cache-directory))
    folder))

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-cache-folder))
  (expand-file-name (elmo-cache-folder-dir-name-internal folder)
		    (expand-file-name "internal/cache"
				      elmo-msgdb-directory)))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-cache-folder))
  (elmo-cache-folder-list-message-locations folder))

(defun elmo-cache-folder-list-message-locations (folder)
  (mapcar 'file-name-nondirectory
	  (elmo-delete-if
	   'file-directory-p
	   (directory-files (elmo-cache-folder-directory-internal folder)
			    t "^[^@]+@[^@]+$" t))))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-cache-folder)
						 &optional one-level)
  (delq nil (mapcar
	     (lambda (f) (if (file-directory-p f)
			     (concat (elmo-folder-prefix-internal folder)
				     "cache/"
				     (file-name-nondirectory f))))
	     (directory-files (elmo-cache-folder-directory-internal folder)
			      t "^[^.].*+"))))

(luna-define-method elmo-folder-message-file-p ((folder elmo-cache-folder))
  t)

(luna-define-method elmo-message-file-name ((folder elmo-cache-folder)
					    number)
  (expand-file-name
   (elmo-map-message-location folder number)
   (elmo-cache-folder-directory-internal folder)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-cache-folder)
					      numbers new-mark
					      already-mark seen-mark
					      important-mark
					      seen-list)
  (let ((i 0)
	(len (length numbers))
	overview number-alist mark-alist entity message-id
	num mark)
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
	(if (setq mark (or (elmo-msgdb-global-mark-get message-id)
			   (if (member message-id seen-list) nil new-mark)))
	    (setq mark-alist
		  (elmo-msgdb-mark-append
		   mark-alist
		   num mark)))
	(when (> len elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (elmo-display-progress
	   'elmo-cache-folder-msgdb-create "Creating msgdb..."
	   (/ (* i 100) len))))
      (setq numbers (cdr numbers)))
    (message "Creating msgdb...done")
    (list overview number-alist mark-alist)))

(luna-define-method elmo-folder-append-buffer ((folder elmo-cache-folder)
					       unread
					       &optional number)
  ;; dir-name is changed according to msgid.
  (unless (elmo-cache-folder-dir-name-internal folder)
    (let* ((file (elmo-file-cache-get-path (std11-field-body "message-id")))
	   (dir (directory-file-name (file-name-directory file))))
      (unless (file-exists-p dir)
	(elmo-make-directory dir))
      (when (file-writable-p file)
	(write-region-as-binary
	 (point-min) (point-max) file nil 'no-msg))))
  t)

(luna-define-method elmo-map-folder-delete-messages ((folder elmo-cache-folder)
						     locations)
  (dolist (location locations)
    (elmo-file-cache-delete
     (expand-file-name location
		       (elmo-cache-folder-directory-internal folder)))))

(luna-define-method elmo-message-fetch-with-cache-process
  ((folder elmo-cache-folder) number strategy &optional section unseen)
  ;; disbable cache process
  (elmo-message-fetch-internal folder number strategy section unseen))

(luna-define-method elmo-map-message-fetch ((folder elmo-cache-folder)
					    location strategy
					    &optional section unseen)
  (let ((file (expand-file-name
	       location
	       (elmo-cache-folder-directory-internal folder))))
    (when (file-exists-p file)
      (insert-file-contents-as-binary file))))

(luna-define-method elmo-folder-creatable-p ((folder elmo-cache-folder))
  nil)

(luna-define-method elmo-folder-writable-p ((folder elmo-cache-folder))
  t)

(luna-define-method elmo-folder-exists-p ((folder elmo-cache-folder))
  t)

(luna-define-method elmo-folder-search ((folder elmo-cache-folder)
					condition &optional from-msgs)
  (let* ((msgs (or from-msgs (elmo-folder-list-messages folder)))
	 (number-list msgs)
	 (i 0)
	 (num (length msgs))
	 file
	 matched
	 case-fold-search)
    (while msgs
      (if (and (setq file (elmo-message-file-name folder (car msgs)))
	       (file-exists-p file)
	       (elmo-file-field-condition-match file
						condition
						(car msgs)
						number-list))
	  (setq matched (nconc matched (list (car msgs)))))
      (elmo-display-progress
       'elmo-internal-folder-search "Searching..."
       (/ (* (setq i (1+ i)) 100) num))
      (setq msgs (cdr msgs)))
    matched))

(luna-define-method elmo-message-file-p ((folder elmo-cache-folder) number)
  t)

;;; To override elmo-map-folder methods.
(luna-define-method elmo-folder-list-unreads-internal
  ((folder elmo-cache-folder) unread-marks &optional mark-alist)
  t)

(luna-define-method elmo-folder-unmark-important ((folder elmo-cache-folder)
						  numbers)
  t)

(luna-define-method elmo-folder-mark-as-important ((folder elmo-cache-folder)
						   numbers)
  t)

(luna-define-method elmo-folder-unmark-read ((folder elmo-cache-folder)
					     numbers)
  t)

(luna-define-method elmo-folder-mark-as-read ((folder elmo-cache-folder)
					      numbers)
  t)

(require 'product)
(product-provide (provide 'elmo-cache) (require 'elmo-version))

;;; elmo-cache.el ends here
