;;; elmo-internal.el -- Internal Interface for ELMO.

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
(require 'elmo-localdir)

(defsubst elmo-internal-list-folder-subr (spec &optional nonsort)
  (let* ((directive (nth 1 spec))
	 (arg (nth 2 spec))
	 (flist (elmo-list-folder-by-location
		 spec
		 (elmo-internal-list-location directive arg)))
	 (killed (and elmo-use-killed-list
		      (elmo-msgdb-killed-list-load
		       (elmo-msgdb-expand-path spec))))
	 numbers)
    (if nonsort
	(cons (or (elmo-max-of-list flist) 0)
	      (if killed
		  (- (length flist)
		     (elmo-msgdb-killed-list-length killed))
		(length flist)))
      (setq numbers (sort flist '<))
      (elmo-living-messages numbers killed))))

(defun elmo-internal-list-folder (spec)
  (elmo-internal-list-folder-subr spec))

(defun elmo-internal-list-folder-by-location (spec location &optional msgdb)
  (let* ((path (elmo-msgdb-expand-path spec))
	 (location-alist
	  (if msgdb
	      (elmo-msgdb-get-location msgdb)
	    (elmo-msgdb-location-load path)))
	 (i 0)
	 result pair
	 location-max modified)
    (setq location-max
	  (or (elmo-max-of-list (mapcar 'car location-alist)) 0))
    (when location-max
      (while location
	(if (setq pair (rassoc (car location) location-alist))
	    (setq result
		  (append result
			  (list (cons (car pair) (car location)))))
	  (setq i (1+ i))
	  (setq result (append result
			       (list
				(cons (+ location-max i) (car location))))))
	(setq location (cdr location))))
    (setq result (sort result '(lambda (x y)
				 (< (car x)(car y)))))
    (if (not (equal result location-alist))
	(setq modified t))
    (if modified
	(elmo-msgdb-location-save path result))
    (mapcar 'car result)))

(defun elmo-internal-list-location (directive arg)
  (let ((mark-alist
	 (or elmo-msgdb-global-mark-alist
	     (setq elmo-msgdb-global-mark-alist
		   (elmo-object-load (expand-file-name
				      elmo-msgdb-global-mark-filename
				      elmo-msgdb-dir)))))
	result)
    (mapcar (function (lambda (x)
			(setq result (cons (car x) result))))
	    mark-alist)
    (nreverse result)))

(defun elmo-internal-msgdb-create-entity (number loc-alist)
  (elmo-localdir-msgdb-create-overview-entity-from-file
   number
   (elmo-cache-get-path (cdr (assq number loc-alist)))))

(defun elmo-internal-msgdb-create (spec numlist new-mark
				       already-mark seen-mark
				       important-mark
				       seen-list
				       &optional msgdb)
  (when numlist
    (let* ((directive (nth 1 spec))
	   (arg       (nth 2 spec))
	   (loc-alist (if msgdb (elmo-msgdb-get-location msgdb)
			(elmo-msgdb-location-load (elmo-msgdb-expand-path
						   spec))))
	   (loc-list (elmo-internal-list-location directive arg))
	   overview number-alist mark-alist entity
	   i percent num location pair)
      (setq num (length numlist))
      (setq i 0)
      (message "Creating msgdb...")
      (while numlist
	(setq entity
	      (elmo-internal-msgdb-create-entity
	       (car numlist) loc-alist))
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
	  (unless (memq location seen-list)
	    (setq mark-alist
		  (elmo-msgdb-mark-append
		   mark-alist
		   (elmo-msgdb-overview-entity-get-number
		    entity)
;;;		   (nth 0 entity)
		   (or (elmo-msgdb-global-mark-get
			(elmo-msgdb-overview-entity-get-id
			 entity))
		       (if (elmo-cache-exists-p
			    (elmo-msgdb-overview-entity-get-id
			     entity))
			   already-mark
			 new-mark))))))
	(when (> num elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (setq percent (/ (* i 100) num))
	  (elmo-display-progress
	   'elmo-internal-msgdb-create "Creating msgdb..."
	   percent))
	(setq numlist (cdr numlist)))
      (message "Creating msgdb...done")
      (list overview number-alist mark-alist loc-alist))))

(defalias 'elmo-internal-msgdb-create-as-numlist 'elmo-internal-msgdb-create)

(defun elmo-internal-list-folders (spec &optional hierarchy)
  ;; XXX hard cording.
  (unless (nth 1 spec) ; toplevel.
    (list (list "'cache") "'mark")))

(defvar elmo-internal-mark "$")

(defun elmo-internal-append-msg (spec string &optional msg no-see)
  (elmo-set-work-buf
   (insert string)
   (let* ((msgid (elmo-field-body "message-id"))
	  (path (elmo-cache-get-path msgid))
	  dir)
     (when path
       (setq dir (directory-file-name (file-name-directory path)))
       (if (not (file-exists-p dir))
	   (elmo-make-directory dir))
       (as-binary-output-file (write-region (point-min) (point-max)
					    path nil 'no-msg)))
     (elmo-msgdb-global-mark-set msgid elmo-internal-mark))))

(defun elmo-internal-delete-msgs (spec msgs &optional msgdb)
  (let ((loc-alist (if msgdb (elmo-msgdb-get-location msgdb)
		     (elmo-msgdb-location-load (elmo-msgdb-expand-path
						spec)))))
    (mapcar '(lambda (msg) (elmo-internal-delete-msg spec msg
						     loc-alist))
	    msgs)))

(defun elmo-internal-delete-msg (spec number loc-alist)
  (let ((pair (assq number loc-alist)))
    (elmo-msgdb-global-mark-delete (cdr pair))))

(defun elmo-internal-read-msg (spec number outbuf &optional msgdb)
  (save-excursion
    (let* ((loc-alist (if msgdb (elmo-msgdb-get-location msgdb)
			(elmo-msgdb-location-load (elmo-msgdb-expand-path
						   spec))))
	   (file (elmo-cache-get-path (cdr (assq number loc-alist)))))
      (set-buffer outbuf)
      (erase-buffer)
      (when (file-exists-p file)
	(as-binary-input-file (insert-file-contents file))
	(elmo-delete-cr-get-content-type)))))

(defun elmo-internal-max-of-folder (spec)
  (elmo-internal-list-folder-subr spec t))

(defun elmo-internal-check-validity (spec)
  nil)

(defun elmo-internal-sync-validity (spec)
  nil)

(defun elmo-internal-folder-exists-p (spec)
  t)

(defun elmo-internal-folder-creatable-p (spec)
  nil)

(defun elmo-internal-create-folder (spec)
  nil)

(defun elmo-internal-search (spec condition &optional from-msgs msgdb)
  (let* ((mark-alist
	 (or elmo-msgdb-global-mark-alist
	     (setq elmo-msgdb-global-mark-alist
		   (elmo-object-load (expand-file-name
				      elmo-msgdb-global-mark-filename
				      elmo-msgdb-dir)))))
	 (loc-alist (if msgdb (elmo-msgdb-get-location msgdb)
		      (elmo-msgdb-location-load (elmo-msgdb-expand-path
						 spec))))
	 (number-list (mapcar 'car loc-alist))
	 cache-file
	 ret-val
	 case-fold-search msg
	 percent i num)
    (setq num (length loc-alist))
    (setq i 0)
    (while loc-alist
      (if (and (setq cache-file (elmo-cache-exists-p (cdr (car loc-alist))))
	       (elmo-file-field-condition-match cache-file
						condition
						(car (car loc-alist))
						number-list))
	  (setq ret-val (append ret-val (list (car (car loc-alist))))))
      (setq i (1+ i))
      (setq percent (/ (* i 100) num))
      (elmo-display-progress
       'elmo-internal-search "Searching..."
       percent)
      (setq loc-alist (cdr loc-alist)))
    ret-val))

(defun elmo-internal-use-cache-p (spec number)
  nil)

(defun elmo-internal-local-file-p (spec number)
  nil ;; XXXX
  )

(defalias 'elmo-internal-sync-number-alist 'elmo-generic-sync-number-alist)
(defalias 'elmo-internal-list-folder-unread
  'elmo-generic-list-folder-unread)
(defalias 'elmo-internal-list-folder-important
  'elmo-generic-list-folder-important)
(defalias 'elmo-internal-commit 'elmo-generic-commit)
(defalias 'elmo-internal-folder-diff 'elmo-generic-folder-diff)

(require 'product)
(product-provide (provide 'elmo-internal) (require 'elmo-version))

;;; elmo-internal.el ends here
