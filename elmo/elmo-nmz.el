;;; elmo-nmz.el -- Namazu interface for ELMO.

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
(require 'elmo)
(require 'elmo-map)

(defcustom elmo-nmz-default-index-path "~/Mail"
  "*Default index path for namazu."
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

(defcustom elmo-nmz-args '("--all" "--list" "--early")
  "*Argument list for namazu to list matched files."
  :type '(repeat string)
  :group 'elmo)

;;; "namazu search"
(eval-and-compile
  (luna-define-class elmo-nmz-folder
		     (elmo-map-folder) (pattern index-path))
  (luna-define-internal-accessors 'elmo-nmz-folder))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-nmz-folder)
					    name)
  (with-temp-buffer
    (insert "[" name)
    (goto-char (point-min))
    (forward-sexp)
    (elmo-nmz-folder-set-pattern-internal folder
					  (buffer-substring
					   (+ 1 (point-min))
					   (- (point) 1)))
    (elmo-nmz-folder-set-index-path-internal folder
					     (buffer-substring (point)
							       (point-max)))
    (if (eq (length (elmo-nmz-folder-index-path-internal folder)) 0)
	(elmo-nmz-folder-set-index-path-internal folder
						 elmo-nmz-default-index-path))
    folder))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-nmz-folder))
  (expand-file-name
   (elmo-replace-string-as-filename
    (elmo-folder-name-internal folder))
   (expand-file-name "nmz" elmo-msgdb-dir)))
		     
(defun elmo-nmz-msgdb-create-entity (folder number)
  "Create msgdb entity for the message in the FOLDER with NUMBER."
  (elmo-msgdb-create-overview-entity-from-file
   number
   (elmo-map-message-location folder number)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-nmz-folder)
					      numlist new-mark
					      already-mark seen-mark
					      important-mark
					      seen-list)
  (let* (overview number-alist mark-alist entity
		  i percent num pair)
    (setq num (length numlist))
    (setq i 0)
    (message "Creating msgdb...")
    (while numlist
      (setq entity
	    (elmo-nmz-msgdb-create-entity
	     folder (car numlist)))
      (when entity
	(setq overview
	      (elmo-msgdb-append-element
	       overview entity))
	(setq number-alist
	      (elmo-msgdb-number-add number-alist
				     (elmo-msgdb-overview-entity-get-number
				      entity)
				     (elmo-msgdb-overview-entity-get-id
				      entity)))
	(setq mark-alist
	      (elmo-msgdb-mark-append
	       mark-alist
	       (elmo-msgdb-overview-entity-get-number
		entity)
	       (or (elmo-msgdb-global-mark-get
		    (elmo-msgdb-overview-entity-get-id
		     entity))
		   new-mark))))
      (when (> num elmo-display-progress-threshold)
	(setq i (1+ i))
	(setq percent (/ (* i 100) num))
	(elmo-display-progress
	 'elmo-folder-msgdb-create "Creating msgdb..."
	 percent))
      (setq numlist (cdr numlist)))
    (message "Creating msgdb...done.")
    (list overview number-alist mark-alist)))

(luna-define-method elmo-folder-message-file-p ((folder elmo-nmz-folder))
  t)

(luna-define-method elmo-message-file-name ((folder elmo-nmz-folder)
					    number)
  (elmo-map-message-location folder number))

(luna-define-method elmo-folder-message-make-temp-file-p
  ((folder elmo-nmz-folder))
  t)

(luna-define-method elmo-folder-diff ((folder elmo-nmz-folder)
				      &optional numbers)
  (cons nil nil))

(luna-define-method elmo-folder-message-make-temp-files ((folder
							  elmo-nmz-folder)
							 numbers
							 &optional
							 start-number)
  (let ((temp-dir (elmo-folder-make-temp-dir folder))
	(cur-number (if start-number 0)))
    (dolist (number numbers)
      (elmo-add-name-to-file
       (elmo-message-file-name folder number)
       (expand-file-name
	(int-to-string (if start-number (incf cur-number) number))
	temp-dir)))
    temp-dir))

(luna-define-method elmo-map-message-fetch ((folder elmo-nmz-folder)
					    location strategy
					    &optional section unseen)
  (when (file-exists-p location)
    (insert-file-contents-as-binary location)))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-nmz-folder))
  (let (bol locations)
    (with-temp-buffer
      (apply 'call-process elmo-nmz-prog nil t t
	     (append elmo-nmz-args
		     (list
		      (encode-mime-charset-string
		       (elmo-nmz-folder-pattern-internal folder)
		       elmo-nmz-charset)
		      (expand-file-name
		       (elmo-nmz-folder-index-path-internal folder)))))
      (goto-char (point-min))
      (while (not (eobp))
	(beginning-of-line)
	(setq bol (point))
	(end-of-line)
	(setq locations (cons (buffer-substring bol (point)) locations))
	(forward-line 1))
      locations)))

(luna-define-method elmo-folder-exists-p ((folder elmo-nmz-folder))
  t)

(luna-define-method elmo-folder-search ((folder elmo-nmz-folder)
					condition &optional from-msgs)
  (let* ((msgs (or from-msgs (elmo-folder-list-messages folder)))
	 (orig msgs)
	 (i 0)
	 case-fold-search matches
	 percent num
	 (num (length msgs)))
    (while msgs
      (if (elmo-file-field-condition-match
	   (elmo-map-message-location folder (car msgs))
	   condition
	   (car msgs)
 	   orig)
 	  (setq matches (cons (car msgs) matches)))
       (setq i (1+ i))
       (setq percent (/ (* i 100) num))
       (elmo-display-progress
        'elmo-nmz-search "Searching..."
        percent)
       (setq msgs (cdr msgs)))
    matches))

;;; To override elmo-map-folder methods.
(luna-define-method elmo-folder-list-unreads-internal
  ((folder elmo-nmz-folder) unread-marks &optional mark-alist)
  t)

(luna-define-method elmo-folder-list-importants-internal
  ((folder elmo-nmz-folder) important-mark)
  t)

(luna-define-method elmo-folder-unmark-important ((folder elmo-nmz-folder)
						  numbers)
  t)

(luna-define-method elmo-folder-mark-as-important ((folder elmo-nmz-folder)
						   numbers)
  t)

(luna-define-method elmo-folder-unmark-read ((folder elmo-nmz-folder) numbers)
  t)

(luna-define-method elmo-folder-mark-as-read ((folder elmo-nmz-folder) numbers)
  t)
  
(require 'product)
(product-provide (provide 'elmo-nmz) (require 'elmo-version))

;;; elmo-nmz.el ends here