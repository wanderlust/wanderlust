;;; elmo-sendlog.el --- Sendlog folder for ELMO.

;; Copyright (C) 2001 Kenichi OKADA <okada@opaopa.org>

;; Author: Kenichi OKADA <okada@opaopa.org>
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

(defvar elmo-sendlog-filename "sendlog")

;;; ELMO sendlog folder
(eval-and-compile
  (luna-define-class elmo-sendlog-folder (elmo-map-folder) (dir-name directory))
  (luna-define-internal-accessors 'elmo-sendlog-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-sendlog-folder)
					    name)
  folder)

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-sendlog-folder))
  (expand-file-name "sendlog"
		    (expand-file-name "internal"
				      elmo-msgdb-directory)))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-sendlog-folder))
  (elmo-sendlog-folder-list-message-locations folder))

(defun elmo-sendlog-folder-list-message-locations (folder)
  (let ((filename (expand-file-name elmo-sendlog-filename
				    elmo-msgdb-directory))
	result)
    (if (not (file-readable-p filename))
	nil
      (elmo-set-work-buf
       (as-binary-input-file
	(insert-file-contents filename))
       (goto-char (point-min))
       (catch 'done
	 (while t
	   (re-search-forward "id=\\([^@]+@[^@]+\\)$" (point-at-eol) t)
	   (setq result (append result (list (match-string 1))))
	   (if (eq (1+ (point-at-eol)) (point-max))
	       (throw 'done nil)
	     (beginning-of-line 2))))))
    result))

(luna-define-method elmo-folder-message-file-p ((folder elmo-sendlog-folder))
  t)

(luna-define-method elmo-message-file-name ((folder elmo-sendlog-folder)
					    number)
  (elmo-file-cache-get-path
   (elmo-map-message-location folder number)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-sendlog-folder)
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
	  (elmo-folder-set-killed-list-internal
	   folder
	   (nconc
	    (elmo-folder-killed-list-internal folder)
	    (list (car numbers))))
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
	   'elmo-sendlog-folder-msgdb-create "Creating msgdb..."
	   (/ (* i 100) len))))
      (setq numbers (cdr numbers)))
    (message "Creating msgdb...done")
    (list overview number-alist mark-alist)))

(luna-define-method elmo-message-fetch-with-cache-process
  ((folder elmo-sendlog-folder) number strategy &optional section unseen)
  ;; disbable cache process
  (elmo-message-fetch-internal folder number strategy section unseen))

(luna-define-method elmo-map-message-fetch ((folder elmo-sendlog-folder)
					    location strategy
					    &optional section unseen)
  (let ((filename (elmo-file-cache-get-path location)))
    (if (file-exists-p filename)
	(insert-file-contents-as-binary filename)
      (error "Now this message is not cached. Please s all"))))

(luna-define-method elmo-folder-exists-p ((folder elmo-sendlog-folder))
  t)

(luna-define-method elmo-folder-delete-messages ((folder elmo-sendlog-folder)
						 numbers)
  (let ((killed-list (elmo-folder-killed-list-internal folder)))
    (dolist (number numbers)
      (setq killed-list
	    (elmo-msgdb-set-as-killed killed-list number)))
    (elmo-folder-set-killed-list-internal folder killed-list))
  t)

(luna-define-method elmo-message-file-p ((folder elmo-sendlog-folder) number)
  t)

;;; To override elmo-map-folder methods.
(luna-define-method elmo-folder-list-unreads-internal
  ((folder elmo-sendlog-folder) unread-marks &optional mark-alist)
  t)

(luna-define-method elmo-folder-unmark-important ((folder elmo-sendlog-folder)
						  numbers)
  t)

(luna-define-method elmo-folder-mark-as-important ((folder elmo-sendlog-folder)
						   numbers)
  t)

(luna-define-method elmo-folder-unmark-read ((folder elmo-sendlog-folder)
					     numbers)
  t)

(luna-define-method elmo-folder-mark-as-read ((folder elmo-sendlog-folder)
					      numbers)
  t)

(require 'product)
(product-provide (provide 'elmo-sendlog) (require 'elmo-version))

;;; elmo-sendlog.el ends here
