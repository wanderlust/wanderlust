;;; elmo-map.el --- A ELMO folder class with message number mapping.

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
;; Folders which do not have unique message numbers but unique message names
;; should inherit this folder.

;;; Code:
;;
(require 'elmo)
(require 'elmo-msgdb)

(eval-when-compile (require 'cl))

(eval-and-compile
  ;; location-hash: location->number mapping
  ;; number-hash:   number->location mapping
  (luna-define-class elmo-map-folder (elmo-folder)
		     (location-alist number-max location-hash))
  (luna-define-internal-accessors 'elmo-map-folder))

(defun elmo-map-folder-numbers-to-locations (folder numbers)
  (let (locations pair)
    (dolist (number numbers)
      (if (setq pair (elmo-get-hash-val
		      (concat "#" (int-to-string number))
		      (elmo-map-folder-location-hash-internal folder)))
	  (setq locations (cons (cdr pair) locations))))
    (nreverse locations)))

(defun elmo-map-folder-locations-to-numbers (folder locations)
  (let (numbers pair)
    (dolist (location locations)
      (if (setq pair (elmo-get-hash-val
		      location
		      (elmo-map-folder-location-hash-internal folder)))
	  (setq numbers (cons (car pair) numbers))))
    (nreverse numbers)))

(luna-define-generic elmo-map-folder-list-message-locations (folder)
  "Return a location list of the FOLDER.")

(luna-define-generic elmo-map-folder-set-flag (folder locations flag)
  "Set FLAG to LOCATIONS.")

(luna-define-generic elmo-map-folder-unset-flag (folder locations flag)
  "Unset FLAG from LOCATIONS.")

(luna-define-generic elmo-map-message-fetch (folder location
						    strategy
						    &optional
						    section
						    unseen)
  "")

(luna-define-generic elmo-map-folder-delete-messages (folder locations)
  "")

(luna-define-method elmo-folder-status ((folder elmo-map-folder))
  (elmo-folder-open-internal folder)
  (elmo-folder-set-killed-list-internal
   folder
   (elmo-msgdb-killed-list-load (elmo-folder-msgdb-path folder)))
  (let ((numbers (mapcar
		  'car
		  (elmo-map-folder-location-alist-internal folder))))
    (setq numbers (elmo-living-messages
		   numbers
		   (elmo-folder-killed-list-internal folder)))
    (prog1
	(cons (elmo-max-of-list numbers)
	      (length numbers))
      ;; Don't close after status.
      (unless (elmo-folder-reserve-status-p folder)
	(elmo-folder-close-internal folder)))))

(defun elmo-map-message-number (folder location)
  "Return number of the message in the FOLDER with LOCATION."
  (car (elmo-get-hash-val
	location
	(elmo-map-folder-location-hash-internal folder))))

(defun elmo-map-message-location (folder number)
  "Return location of the message in the FOLDER with NUMBER."
  (cdr (elmo-get-hash-val
	(concat "#" (int-to-string number))
	(elmo-map-folder-location-hash-internal folder))))

(luna-define-method elmo-folder-pack-numbers ((folder elmo-map-folder))
  (let* ((msgdb (elmo-folder-msgdb folder))
	 (numbers
	  (sort (elmo-folder-list-messages folder nil
					   (not elmo-pack-number-check-strict))
		'<))
	 (new-msgdb (elmo-make-msgdb (elmo-folder-msgdb-path folder)))
	 (number 1)
	 total location entity)
    (setq total (length numbers))
    (elmo-with-progress-display (> total elmo-display-progress-threshold)
	(elmo-folder-pack-numbers total "Packing...")
      (dolist (old-number numbers)
	(setq entity (elmo-msgdb-message-entity msgdb old-number))
	(elmo-message-entity-set-number entity number)
	(elmo-msgdb-append-entity new-msgdb entity
				  (elmo-msgdb-flags msgdb old-number))
	(setq location
	      (cons (cons number
			  (elmo-map-message-location folder old-number))
		    location))
	(elmo-emit-signal 'message-number-changed folder old-number number)
	(setq number (1+ number))))
    (message "Packing...done")
    (elmo-map-folder-location-setup folder (nreverse location))
    (elmo-folder-set-msgdb-internal folder new-msgdb)))

(defun elmo-map-folder-location-setup (folder locations)
  (elmo-map-folder-set-location-alist-internal
   folder
   locations)
  (elmo-map-folder-set-location-hash-internal
   folder (elmo-make-hash
	   (* 2 (length locations))))
  (elmo-map-folder-set-number-max-internal folder 0)
  ;; Set number-max and hashtables.
  (dolist (location-cons locations)
    (if (< (elmo-map-folder-number-max-internal folder)
	   (car location-cons))
	(elmo-map-folder-set-number-max-internal folder (car location-cons)))
    (elmo-set-hash-val (cdr location-cons)
		       location-cons
		       (elmo-map-folder-location-hash-internal folder))
    (elmo-set-hash-val (concat "#" (int-to-string (car location-cons)))
		       location-cons
		       (elmo-map-folder-location-hash-internal folder))))

(defun elmo-map-folder-update-locations (folder locations)
  ;; A subroutine to make location-alist.
  ;; location-alist is existing location-alist.
  ;; locations is the newest locations.
  (let* ((location-alist (elmo-map-folder-location-alist-internal folder))
	 (locations-in-db (mapcar 'cdr location-alist))
	 new-locs new-alist deleted-locs pair i)
    (setq new-locs
	  (elmo-delete-if (function
			   (lambda (x) (member x locations-in-db)))
			  locations))
    (setq deleted-locs
	  (elmo-delete-if (function
			   (lambda (x) (member x locations)))
			  locations-in-db))
    (dolist (location deleted-locs)
      (setq location-alist
	    (delq (setq pair
			(elmo-get-hash-val
			 location
			 (elmo-map-folder-location-hash-internal
			  folder)))
		  location-alist))
      (when pair
	(elmo-clear-hash-val (concat "#" (int-to-string (car pair)))
			     (elmo-map-folder-location-hash-internal
			      folder))
	(elmo-clear-hash-val location
			     (elmo-map-folder-location-hash-internal
			      folder))))
    (setq i (elmo-map-folder-number-max-internal folder))
    (dolist (location new-locs)
      (setq i (1+ i))
      (elmo-map-folder-set-number-max-internal folder i)
      (setq new-alist (cons (setq pair (cons i location)) new-alist))
      (setq new-alist (nreverse new-alist))
      (elmo-set-hash-val (concat "#" (int-to-string i))
			 pair
			 (elmo-map-folder-location-hash-internal
			  folder))
      (elmo-set-hash-val location
			 pair
			 (elmo-map-folder-location-hash-internal
			  folder)))
    (setq location-alist
	  (sort (nconc location-alist new-alist)
		(lambda (x y) (< (car x) (car y)))))
    (elmo-map-folder-set-location-alist-internal folder location-alist)))

(luna-define-method elmo-folder-open-internal ((folder elmo-map-folder))
  (elmo-map-folder-location-setup
   folder
   (elmo-msgdb-location-load (elmo-folder-msgdb-path folder)))
  (if (elmo-folder-plugged-p folder)
      (elmo-map-folder-update-locations
       folder
       (elmo-map-folder-list-message-locations folder))))

(luna-define-method elmo-folder-commit :after ((folder elmo-map-folder))
  (when (elmo-folder-persistent-p folder)
    (elmo-msgdb-location-save (elmo-folder-msgdb-path folder)
			      (elmo-map-folder-location-alist-internal
			       folder))))

(luna-define-method elmo-folder-close-internal ((folder elmo-map-folder))
  (elmo-map-folder-set-location-alist-internal folder nil)
  (elmo-map-folder-set-location-hash-internal folder nil))

(luna-define-method elmo-folder-check ((folder elmo-map-folder))
  (elmo-map-folder-update-locations
   folder
   (elmo-map-folder-list-message-locations folder)))

(luna-define-method elmo-folder-next-message-number ((folder elmo-map-folder))
  (1+ (elmo-map-folder-number-max-internal folder)))

(luna-define-method elmo-folder-clear :around ((folder elmo-map-folder)
					       &optional keep-killed)
  (unless keep-killed
    (elmo-map-folder-set-number-max-internal folder 0)
    (elmo-map-folder-set-location-alist-internal folder nil)
    ;; clear hashtable.
    (elmo-map-folder-set-location-hash-internal folder (elmo-make-hash)))
  (luna-call-next-method))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-map-folder) &optional nohide)
  (mapcar 'car (elmo-map-folder-location-alist-internal folder)))

(luna-define-method elmo-folder-set-flag :before ((folder elmo-map-folder)
						  numbers
						  flag
						  &optional is-local)
  (unless is-local
    (elmo-map-folder-set-flag
     folder
     (elmo-map-folder-numbers-to-locations folder numbers)
     flag)))

(luna-define-method elmo-folder-unset-flag :before ((folder elmo-map-folder)
						    numbers
						    flag
						    &optional is-local)
  (unless is-local
    (elmo-map-folder-unset-flag
     folder
     (elmo-map-folder-numbers-to-locations folder numbers)
     flag)))

(luna-define-method elmo-message-fetch-internal ((folder elmo-map-folder)
						 number strategy
						 &optional section unread)
  (elmo-map-message-fetch
   folder
   (elmo-map-message-location folder number)
   strategy section unread))

(luna-define-method elmo-folder-list-flagged-internal ((folder elmo-map-folder)
						       flag)
  (let ((locations (elmo-map-folder-list-flagged folder flag)))
    (if (listp locations)
	(elmo-map-folder-locations-to-numbers folder locations)
      t)))

(luna-define-generic elmo-map-folder-list-flagged (folder flag)
  "Return a list of message location in the FOLDER with FLAG.
Return t if the message list is not available.")

(luna-define-method elmo-map-folder-list-flagged ((folder elmo-map-folder)
						  flag)
  t)

(luna-define-method elmo-folder-delete-messages-internal ((folder
							   elmo-map-folder)
							  numbers)
  (elmo-map-folder-delete-messages
   folder
   (elmo-map-folder-numbers-to-locations folder numbers)))

(luna-define-method elmo-folder-detach-messages :around ((folder
							  elmo-map-folder)
							 numbers)
  (when (luna-call-next-method)
    (dolist (number numbers)
      (elmo-map-folder-set-location-alist-internal
       folder
       (delq (elmo-get-hash-val
	      (concat "#" (int-to-string number))
	      (elmo-map-folder-location-hash-internal
	       folder))
	     (elmo-map-folder-location-alist-internal folder)))
      (elmo-clear-hash-val (concat "#" (int-to-string number))
			   (elmo-map-folder-location-hash-internal
			    folder)))
    t)) ; success

(require 'product)
(product-provide (provide 'elmo-map) (require 'elmo-version))

;;; elmo-map.el ends here
