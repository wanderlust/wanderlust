;;; elmo-shimbun.el -- Shimbun interface for ELMO.

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

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
(require 'elmo-dop)
(require 'shimbun)

(defcustom elmo-shimbun-check-interval 60
  "*Check interval for shimbun."
  :type 'integer
  :group 'elmo)

(defcustom elmo-shimbun-default-index-range 2
  "*Default value for the range of header indices."
  :type '(choice (const :tag "all" all)
		 (const :tag "last" last)
		 (integer :tag "number"))
  :group 'elmo)

(defcustom elmo-shimbun-index-range-alist nil
  "*Alist of FOLDER and RANGE.
FOLDER is the shimbun folder name.
RANGE is the range of the header indices .
See `shimbun-headers' for more detail about RANGE."
  :type '(repeat (cons (string :tag "Folder Name")
		       (choice (const :tag "all" all)
			       (const :tag "last" last)
			       (integer :tag "number"))))
  :group 'elmo)

;; Shimbun mua.
(eval-and-compile 
  (luna-define-class shimbun-elmo-mua (shimbun-mua) (folder))
  (luna-define-internal-accessors 'shimbun-elmo-mua))

(luna-define-method shimbun-mua-search-id ((mua shimbun-elmo-mua) id)
  (elmo-msgdb-overview-get-entity id 
				  (elmo-folder-msgdb
				   (shimbun-elmo-mua-folder-internal mua))))

(eval-and-compile
  (luna-define-class elmo-shimbun-folder
		     (elmo-map-folder) (shimbun headers header-hash
						group range last-check))
  (luna-define-internal-accessors 'elmo-shimbun-folder))

(defsubst elmo-shimbun-lapse-seconds (time)
  (let ((now (current-time)))
    (+ (* (- (car now) (car time)) 65536)
       (- (nth 1 now) (nth 1 time)))))

(defun elmo-shimbun-parse-time-string (string)
  "Parse the time-string STRING and return its time as Emacs style."
  (ignore-errors
    (let ((x (timezone-fix-time string nil nil)))
      (encode-time (aref x 5) (aref x 4) (aref x 3)
		   (aref x 2) (aref x 1) (aref x 0)
		   (aref x 6)))))

(defsubst elmo-shimbun-headers-check-p (folder)
  (or (null (elmo-shimbun-folder-last-check-internal folder))
      (and (elmo-shimbun-folder-last-check-internal folder)
	   (> (elmo-shimbun-lapse-seconds
	       (elmo-shimbun-folder-last-check-internal folder))
	      elmo-shimbun-check-interval))))

(defun elmo-shimbun-msgdb-to-headers (folder expire-days)
  (let (headers)
    (dolist (ov (elmo-msgdb-get-overview (elmo-folder-msgdb folder)))
      (when (and (elmo-msgdb-overview-entity-get-extra-field ov "xref")
		 (if expire-days
		     (< (elmo-shimbun-lapse-seconds
			 (elmo-shimbun-parse-time-string
			  (elmo-msgdb-overview-entity-get-date ov)))
			(* expire-days 86400 ; seconds per day
			   ))
		   t))
	(setq headers
	      (cons (shimbun-make-header
		     (elmo-msgdb-overview-entity-get-number ov)
		     (shimbun-mime-encode-string
		      (elmo-msgdb-overview-entity-get-subject ov))
		     (shimbun-mime-encode-string
		      (elmo-msgdb-overview-entity-get-from ov))
		     (elmo-msgdb-overview-entity-get-date ov)
		     (elmo-msgdb-overview-entity-get-id ov)
		     (elmo-msgdb-overview-entity-get-references ov)
		     0
		     0
		     (elmo-msgdb-overview-entity-get-extra-field ov "xref"))
		    headers))))
    (nreverse headers)))

(defun elmo-shimbun-folder-setup (folder)
  ;; Resume headers from existing msgdb.
  (elmo-shimbun-folder-set-headers-internal
   folder
   (elmo-shimbun-msgdb-to-headers folder nil))
  (elmo-shimbun-folder-set-header-hash-internal
   folder
   (elmo-make-hash
    (length (elmo-shimbun-folder-headers-internal folder))))
  (dolist (header (elmo-shimbun-folder-headers-internal folder))
    (elmo-set-hash-val
     (shimbun-header-id header) header
     (elmo-shimbun-folder-header-hash-internal folder))))

(defun elmo-shimbun-get-headers (folder)
  (shimbun-open-group
   (elmo-shimbun-folder-shimbun-internal folder)
   (elmo-shimbun-folder-group-internal folder))
  (let* ((shimbun (elmo-shimbun-folder-shimbun-internal folder))
	 (key (concat (shimbun-server-internal shimbun)
		      "." (shimbun-current-group-internal shimbun)))
	 (elmo-hash-minimum-size 0)
	 entry headers hash)
    ;; new headers.
    (setq headers
	  (delq nil
		(mapcar
		 (lambda (x)
		   (unless (elmo-msgdb-overview-get-entity 
			    (shimbun-header-id x)
			    (elmo-folder-msgdb folder))
		     x))
		 ;; This takes much time.
		 (shimbun-headers
		  (elmo-shimbun-folder-shimbun-internal folder)
		  (elmo-shimbun-folder-range-internal folder)))))
    (elmo-shimbun-folder-set-headers-internal
     folder
     (nconc (elmo-shimbun-msgdb-to-headers
	     folder (shimbun-article-expiration-days
		     (elmo-shimbun-folder-shimbun-internal folder)))
	    headers))
    (setq hash
	  (elmo-shimbun-folder-set-header-hash-internal
	   folder
	   (elmo-make-hash
	    (length (elmo-shimbun-folder-headers-internal folder)))))
    ;; Set up header hash.
    (dolist (header (elmo-shimbun-folder-headers-internal folder))
      (elmo-set-hash-val
       (shimbun-header-id header) header
       (elmo-shimbun-folder-header-hash-internal folder)))
    (elmo-shimbun-folder-set-last-check-internal folder (current-time))))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-shimbun-folder)
					    name)
  (let ((server-group (if (string-match "\\([^.]+\\)\\." name)
			  (list (elmo-match-string 1 name)
				(substring name (match-end 0)))
			(list name))))
    (when (nth 0 server-group) ; server
      (elmo-shimbun-folder-set-shimbun-internal
       folder
       (shimbun-open (nth 0 server-group)
		     (luna-make-entity 'shimbun-elmo-mua :folder folder))))
    (when (nth 1 server-group)
      (elmo-shimbun-folder-set-group-internal
       folder
       (nth 1 server-group)))
    (elmo-shimbun-folder-set-range-internal
     folder
     (or (cdr (assoc (elmo-folder-name-internal folder)
		     elmo-shimbun-index-range-alist))
	 elmo-shimbun-default-index-range))
    folder))

(luna-define-method elmo-folder-open-internal ((folder elmo-shimbun-folder))
  (when (elmo-folder-plugged-p folder)
    (when (elmo-shimbun-headers-check-p folder)
      (let ((inhibit-quit t))
	(elmo-map-folder-location-setup
	 folder 
	 (elmo-msgdb-location-load (elmo-folder-msgdb-path folder)))
	;; Resume headers from existing msgdb.
	(elmo-shimbun-folder-setup folder))
      (elmo-shimbun-get-headers folder))
    (elmo-map-folder-update-locations
     folder
     (elmo-map-folder-list-message-locations folder))))

(luna-define-method elmo-folder-reserve-status-p ((folder elmo-shimbun-folder))
  t)

(luna-define-method elmo-folder-close-internal :after ((folder
							elmo-shimbun-folder))
  (shimbun-close-group
   (elmo-shimbun-folder-shimbun-internal folder))
  (elmo-shimbun-folder-set-headers-internal
   folder nil)
  (elmo-shimbun-folder-set-header-hash-internal
   folder nil)
  (elmo-shimbun-folder-set-last-check-internal
   folder nil))

(luna-define-method elmo-folder-plugged-p ((folder elmo-shimbun-folder))
  (elmo-plugged-p
   "shimbun" 
   (shimbun-server-internal (elmo-shimbun-folder-shimbun-internal folder))
   nil nil
   (shimbun-server-internal (elmo-shimbun-folder-shimbun-internal folder))))
			    
(luna-define-method elmo-folder-set-plugged ((folder elmo-shimbun-folder)
					     plugged &optional add)
  (elmo-set-plugged plugged
		    "shimbun"
		    (shimbun-server-internal
		     (elmo-shimbun-folder-shimbun-internal folder))
		    nil nil nil
		    (shimbun-server-internal
		     (elmo-shimbun-folder-shimbun-internal folder))
		    add))

(luna-define-method elmo-net-port-info ((folder elmo-shimbun-folder))
  (list "shimbun"
	(shimbun-server-internal
	 (elmo-shimbun-folder-shimbun-internal folder))
	nil))

(luna-define-method elmo-folder-check :around ((folder elmo-shimbun-folder))
  (when (shimbun-current-group-internal 
	 (elmo-shimbun-folder-shimbun-internal folder))
    (when (and (elmo-folder-plugged-p folder)
	       (elmo-shimbun-headers-check-p folder))
      (elmo-shimbun-get-headers folder)
      (luna-call-next-method))))

(luna-define-method elmo-folder-clear :around ((folder elmo-shimbun-folder)
					       &optional keep-killed)
  (elmo-shimbun-folder-set-headers-internal folder nil)
  (elmo-shimbun-folder-set-header-hash-internal folder nil)
  (elmo-shimbun-folder-set-last-check-internal folder nil)
  (luna-call-next-method))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-shimbun-folder))
  (expand-file-name
   (concat (shimbun-server-internal
	    (elmo-shimbun-folder-shimbun-internal folder))
	   "/"
	   (elmo-shimbun-folder-group-internal folder))
   (expand-file-name "shimbun" elmo-msgdb-dir)))
		     
(defun elmo-shimbun-msgdb-create-entity (folder number)
  (let ((header (elmo-get-hash-val
		 (elmo-map-message-location folder number)
		 (elmo-shimbun-folder-header-hash-internal folder)))
	ov)
    (when header
      (with-temp-buffer
	(shimbun-header-insert
	 (elmo-shimbun-folder-shimbun-internal folder)
	 header)
	(setq ov (elmo-msgdb-create-overview-from-buffer number))
	(elmo-msgdb-overview-entity-set-extra
	 ov
	 (nconc
	  (elmo-msgdb-overview-entity-get-extra ov)
	  (list (cons "xref" (shimbun-header-xref header)))))))))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-shimbun-folder)
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
	    (elmo-shimbun-msgdb-create-entity
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
    (elmo-msgdb-sort-by-date
     (list overview number-alist mark-alist))))

(luna-define-method elmo-folder-message-file-p ((folder elmo-shimbun-folder))
  nil)

(luna-define-method elmo-map-message-fetch ((folder elmo-shimbun-folder)
					    location strategy
					    &optional section unseen)
  (shimbun-article (elmo-shimbun-folder-shimbun-internal folder)
		   (elmo-get-hash-val
		    location
		    (elmo-shimbun-folder-header-hash-internal folder))))

(luna-define-method elmo-message-encache :around ((folder
						   elmo-shimbun-folder)
						  number)
  (if (elmo-folder-plugged-p folder)
      (luna-call-next-method)
    (if elmo-enable-disconnected-operation
	(elmo-message-encache-dop folder number)
      (error "Unplugged"))))

(luna-define-method elmo-folder-list-messages-internal :around
  ((folder elmo-shimbun-folder) &optional nohide)
  (if (elmo-folder-plugged-p folder)
      (luna-call-next-method)
    t))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-shimbun-folder))
  (mapcar
   (function shimbun-header-id)
   (elmo-shimbun-folder-headers-internal folder)))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-shimbun-folder)
						 &optional one-level)
  (unless (elmo-shimbun-folder-group-internal folder)
    (mapcar
     (lambda (x)
       (concat (elmo-folder-prefix-internal folder)
	       (shimbun-server-internal
		(elmo-shimbun-folder-shimbun-internal folder))
	       "."
	       x))
     (shimbun-groups (elmo-shimbun-folder-shimbun-internal folder)))))

(luna-define-method elmo-folder-exists-p ((folder elmo-shimbun-folder))
  (if (elmo-shimbun-folder-group-internal folder)
      (progn
	(member 
	 (elmo-shimbun-folder-group-internal folder)
	 (shimbun-groups (elmo-shimbun-folder-shimbun-internal
			  folder))))
    t))

(luna-define-method elmo-folder-search ((folder elmo-shimbun-folder)
					condition &optional from-msgs)
  nil)

;;; To override elmo-map-folder methods.
(luna-define-method elmo-folder-list-unreads-internal
  ((folder elmo-shimbun-folder) unread-marks &optional mark-alist)
  t)

(luna-define-method elmo-folder-unmark-important ((folder elmo-shimbun-folder)
						  numbers)
  t)

(luna-define-method elmo-folder-mark-as-important ((folder elmo-shimbun-folder)
						   numbers)
  t)

(luna-define-method elmo-folder-unmark-read ((folder elmo-shimbun-folder)
					     numbers)
  t)

(luna-define-method elmo-folder-mark-as-read ((folder elmo-shimbun-folder)
					      numbers)
  t)

(require 'product)
(product-provide (provide 'elmo-shimbun) (require 'elmo-version))

;;; elmo-shimbun.el ends here