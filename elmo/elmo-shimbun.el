;;; elmo-shimbun.el --- Shimbun interface for ELMO.

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

(eval-when-compile
  (defun-maybe shimbun-servers-list ()))

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

(defcustom elmo-shimbun-use-cache t
  "*If non-nil, use cache for each article."
  :type 'boolean
  :group 'elmo)

(defcustom elmo-shimbun-index-range-alist nil
  "*Alist of FOLDER-REGEXP and RANGE.
FOLDER-REGEXP is the regexp for shimbun folder name.
RANGE is the range of the header indices .
See `shimbun-headers' for more detail about RANGE."
  :type '(repeat (cons (regexp :tag "Folder Regexp")
		       (choice (const :tag "all" all)
			       (const :tag "last" last)
			       (integer :tag "number"))))
  :group 'elmo)

(defcustom elmo-shimbun-update-overview-folder-list nil
  "*List of FOLDER-REGEXP.
FOLDER-REGEXP is the regexp of shimbun folder name which should be
update overview when message is fetched."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'elmo)

;; Shimbun header.
(defsubst elmo-shimbun-header-extra-field (header field-name)
  (let ((extra (and header (shimbun-header-extra header))))
    (and extra
	 (cdr (assoc field-name extra)))))

(defsubst elmo-shimbun-header-set-extra-field (header field-name value)
  (let ((extras (and header (shimbun-header-extra header)))
	extra)
    (if (setq extra (assoc field-name extras))
	(setcdr extra value)
      (shimbun-header-set-extra
       header
       (cons (cons field-name value) extras)))))

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
						entity-hash
						group range last-check))
  (luna-define-internal-accessors 'elmo-shimbun-folder))

(defun elmo-shimbun-folder-entity-hash (folder)
  (or (elmo-shimbun-folder-entity-hash-internal folder)
      (let ((overviews (elmo-msgdb-get-overview (elmo-folder-msgdb folder)))
	    hash id)
	(when overviews
	  (setq hash (elmo-make-hash (length overviews)))
	  (dolist (entity overviews)
	    (elmo-set-hash-val (elmo-msgdb-overview-entity-get-id entity)
			       entity hash)
	    (when (setq id (elmo-msgdb-overview-entity-get-extra-field
			    entity "x-original-id"))
	      (elmo-set-hash-val id entity hash)))
	  (elmo-shimbun-folder-set-entity-hash-internal folder hash)))))

(defsubst elmo-shimbun-folder-shimbun-header (folder location)
  (let ((hash (elmo-shimbun-folder-header-hash-internal folder)))
    (or (and hash (elmo-get-hash-val location hash))
	(let ((entity (elmo-msgdb-overview-get-entity
		       location
		       (elmo-folder-msgdb folder)))
	      (elmo-hash-minimum-size 63)
	      header)
	  (when entity
	    (setq header (elmo-shimbun-entity-to-header entity))
	    (unless hash
	      (elmo-shimbun-folder-set-header-hash-internal
	       folder
	       (setq hash (elmo-make-hash))))
	    (elmo-set-hash-val (elmo-msgdb-overview-entity-get-id entity)
			       header
			       hash)
	    header)))))

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

(defun elmo-shimbun-entity-to-header (entity)
  (let (message-id shimbun-id)
    (if (setq message-id (elmo-msgdb-overview-entity-get-extra-field
			  entity "x-original-id"))
	(setq shimbun-id (elmo-msgdb-overview-entity-get-id entity))
      (setq message-id (elmo-msgdb-overview-entity-get-id entity)
	    shimbun-id nil))
    (elmo-set-work-buf
     (set-buffer-multibyte t)
     (shimbun-make-header
      (elmo-msgdb-overview-entity-get-number entity)
      (shimbun-mime-encode-string
       (decode-mime-charset-string
	(elmo-msgdb-overview-entity-get-subject-no-decode entity)
	elmo-mime-charset))
      (shimbun-mime-encode-string
       (decode-mime-charset-string
	(elmo-msgdb-overview-entity-get-from-no-decode entity)
	elmo-mime-charset))
      (elmo-msgdb-overview-entity-get-date entity)
      message-id
      (elmo-msgdb-overview-entity-get-references entity)
      0
      0
      (elmo-msgdb-overview-entity-get-extra-field entity "xref")
      (and shimbun-id
	   (list (cons "x-shimbun-id" shimbun-id)))))))

(defsubst elmo-shimbun-folder-header-hash-setup (folder headers)
  (let ((hash (or (elmo-shimbun-folder-header-hash-internal folder)
		  (elmo-make-hash (length headers)))))
    (dolist (header headers)
      (elmo-set-hash-val (shimbun-header-id header) header hash))
    (elmo-shimbun-folder-set-header-hash-internal folder hash)))

(defun elmo-shimbun-get-headers (folder)
  (let* ((shimbun (elmo-shimbun-folder-shimbun-internal folder))
	 (key (concat (shimbun-server-internal shimbun)
		      "." (shimbun-current-group-internal shimbun)))
	 (elmo-hash-minimum-size 63)
	 headers)
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
    (elmo-shimbun-folder-set-headers-internal folder headers)
    (when headers
      (elmo-shimbun-folder-header-hash-setup folder headers))
    (elmo-shimbun-folder-set-last-check-internal folder (current-time))))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-shimbun-folder)
					    name)
  (if (string= name "")
      folder
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
       (or (cdr (elmo-string-matched-assoc (elmo-folder-name-internal folder)
					   elmo-shimbun-index-range-alist))
	   elmo-shimbun-default-index-range))
      folder)))

(luna-define-method elmo-folder-open-internal ((folder elmo-shimbun-folder))
  (when (elmo-shimbun-folder-shimbun-internal folder)
    (shimbun-open-group
     (elmo-shimbun-folder-shimbun-internal folder)
     (elmo-shimbun-folder-group-internal folder))
    (let ((inhibit-quit t))
      (unless (elmo-map-folder-location-alist-internal folder)
	(elmo-map-folder-location-setup
	 folder
	 (elmo-msgdb-location-load (elmo-folder-msgdb-path folder))))
      (when (and (elmo-folder-plugged-p folder)
		 (elmo-shimbun-headers-check-p folder))
	(elmo-shimbun-get-headers folder)
	(elmo-map-folder-update-locations
	 folder
	 (elmo-map-folder-list-message-locations folder))))))

(luna-define-method elmo-folder-reserve-status-p ((folder elmo-shimbun-folder))
  t)

(luna-define-method elmo-folder-local-p ((folder elmo-shimbun-folder))
  nil)

(luna-define-method elmo-message-use-cache-p ((folder elmo-shimbun-folder)
					      number)
  elmo-shimbun-use-cache)

(luna-define-method elmo-folder-close-internal :after ((folder
							elmo-shimbun-folder))
  (shimbun-close-group
   (elmo-shimbun-folder-shimbun-internal folder))
  (elmo-shimbun-folder-set-headers-internal
   folder nil)
  (elmo-shimbun-folder-set-header-hash-internal
   folder nil)
  (elmo-shimbun-folder-set-entity-hash-internal
   folder nil)
  (elmo-shimbun-folder-set-last-check-internal
   folder nil))

(luna-define-method elmo-folder-plugged-p ((folder elmo-shimbun-folder))
  (elmo-plugged-p
   "shimbun"
   (and (elmo-shimbun-folder-shimbun-internal folder)
	(shimbun-server-internal (elmo-shimbun-folder-shimbun-internal folder)))
   nil nil
   (and (elmo-shimbun-folder-shimbun-internal folder)
	(shimbun-server-internal (elmo-shimbun-folder-shimbun-internal folder)))))

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
  (elmo-shimbun-folder-set-entity-hash-internal folder nil)
  (elmo-shimbun-folder-set-last-check-internal folder nil)
  (luna-call-next-method))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-shimbun-folder))
  (expand-file-name
   (concat (shimbun-server-internal
	    (elmo-shimbun-folder-shimbun-internal folder))
	   "/"
	   (elmo-shimbun-folder-group-internal folder))
   (expand-file-name "shimbun" elmo-msgdb-directory)))

(defun elmo-shimbun-msgdb-create-entity (folder number)
  (let ((header (elmo-shimbun-folder-shimbun-header
		 folder
		 (elmo-map-message-location folder number)))
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
					      numlist seen-list)
  (let* (overview number-alist mark-alist entity
		  i percent number length pair msgid gmark seen)
    (setq length (length numlist))
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
	(setq number (elmo-msgdb-overview-entity-get-number entity))
	(setq msgid (elmo-msgdb-overview-entity-get-id entity))
	(setq number-alist
	      (elmo-msgdb-number-add number-alist
				     number msgid))
	(setq seen (member msgid seen-list))
	(if (setq gmark (or (elmo-msgdb-global-mark-get msgid)
			    (if (elmo-file-cache-status
				 (elmo-file-cache-get msgid))
				(if seen nil elmo-msgdb-unread-cached-mark)
			      (if seen
				  (if elmo-shimbun-use-cache
				      elmo-msgdb-read-uncached-mark)
				elmo-msgdb-new-mark))))
	    (setq mark-alist
		  (elmo-msgdb-mark-append mark-alist
					  number gmark))))
      (when (> length elmo-display-progress-threshold)
	(setq i (1+ i))
	(setq percent (/ (* i 100) length))
	(elmo-display-progress
	 'elmo-folder-msgdb-create "Creating msgdb..."
	 percent))
      (setq numlist (cdr numlist)))
    (message "Creating msgdb...done")
    (elmo-msgdb-sort-by-date
     (list overview number-alist mark-alist))))

(luna-define-method elmo-folder-message-file-p ((folder elmo-shimbun-folder))
  nil)

(defsubst elmo-shimbun-update-overview (folder shimbun-id header)
  (let ((entity (elmo-msgdb-overview-get-entity shimbun-id
						(elmo-folder-msgdb folder)))
	(message-id (shimbun-header-id header))
	references)
    (unless (string= shimbun-id message-id)
      (elmo-msgdb-overview-entity-set-extra-field
       entity "x-original-id" message-id)
      (elmo-shimbun-header-set-extra-field
       header "x-shimbun-id" shimbun-id)
      (elmo-set-hash-val message-id
			 entity
			 (elmo-shimbun-folder-entity-hash folder))
      (elmo-set-hash-val shimbun-id
			 entity
			 (elmo-shimbun-folder-entity-hash folder)))
    (elmo-msgdb-overview-entity-set-from
     entity
     (elmo-mime-string (shimbun-header-from header)))
    (elmo-msgdb-overview-entity-set-subject
     entity
     (elmo-mime-string (shimbun-header-subject header)))
    (elmo-msgdb-overview-entity-set-date
     entity (shimbun-header-date header))
    (when (setq references
		(or (elmo-msgdb-get-last-message-id
		     (elmo-field-body "in-reply-to"))
		    (elmo-msgdb-get-last-message-id
		     (elmo-field-body "references"))))
      (elmo-msgdb-overview-entity-set-references
       entity
       (or (elmo-msgdb-overview-entity-get-id
	    (elmo-get-hash-val
	     references
	     (elmo-shimbun-folder-entity-hash folder)))
	   references)))))

(luna-define-method elmo-map-message-fetch ((folder elmo-shimbun-folder)
					    location strategy
					    &optional section unseen)
  (if (elmo-folder-plugged-p folder)
      (let ((header (elmo-shimbun-folder-shimbun-header
		     folder
		     location))
	    shimbun-id)
	(shimbun-article (elmo-shimbun-folder-shimbun-internal folder)
			 header)
	(when (elmo-string-match-member
	       (elmo-folder-name-internal folder)
	       elmo-shimbun-update-overview-folder-list)
	  (elmo-shimbun-update-overview folder location header))
	(when (setq shimbun-id
		    (elmo-shimbun-header-extra-field header "x-shimbun-id"))
	  (goto-char (point-min))
	  (insert (format "X-Shimbun-Id: %s\n" shimbun-id)))
	t)
    (error "Unplugged")))

(luna-define-method elmo-message-encache :around ((folder
						   elmo-shimbun-folder)
						  number &optional read)
  (if (elmo-folder-plugged-p folder)
      (luna-call-next-method)
    (if elmo-enable-disconnected-operation
	(elmo-message-encache-dop folder number read)
      (error "Unplugged"))))

(luna-define-method elmo-folder-list-messages-internal :around
  ((folder elmo-shimbun-folder) &optional nohide)
  (if (elmo-folder-plugged-p folder)
      (luna-call-next-method)
    t))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-shimbun-folder))
  (let ((expire-days (shimbun-article-expiration-days
		      (elmo-shimbun-folder-shimbun-internal folder))))
    (elmo-uniq-list
     (nconc
      (delq nil
	    (mapcar
	     (lambda (ov)
	       (when (and (elmo-msgdb-overview-entity-get-extra-field
			   ov "xref")
			  (if expire-days
			      (< (elmo-shimbun-lapse-seconds
				  (elmo-shimbun-parse-time-string
				   (elmo-msgdb-overview-entity-get-date ov)))
				 (* expire-days 86400 ; seconds per day
				    ))
			    t))
		 (elmo-msgdb-overview-entity-get-id ov)))
	     (elmo-msgdb-get-overview (elmo-folder-msgdb folder))))
      (mapcar
       (lambda (header)
	 (or (elmo-shimbun-header-extra-field header "x-shimbun-id")
	     (shimbun-header-id header)))
       (elmo-shimbun-folder-headers-internal folder))))))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-shimbun-folder)
						 &optional one-level)
  (let ((prefix (elmo-folder-prefix-internal folder)))
    (cond ((elmo-shimbun-folder-shimbun-internal folder)
	   (unless (elmo-shimbun-folder-group-internal folder)
	     (mapcar
	      (lambda (fld)
		(concat prefix
			(shimbun-server-internal
			 (elmo-shimbun-folder-shimbun-internal folder))
			"." fld))
	      (shimbun-groups (elmo-shimbun-folder-shimbun-internal folder)))))
	  ;; the rest are for "@/" group
	  (one-level
	   (mapcar
	    (lambda (server) (list (concat prefix server)))
	    (shimbun-servers-list)))
	  (t
	   (let (folders)
	     (dolist (server (shimbun-servers-list))
	       (setq folders
		     (append folders
			     (mapcar
			      (lambda (fld) (concat prefix server "." fld))
			      (shimbun-groups
			       (shimbun-open server
					     (let ((fld
						    (elmo-make-folder
						     (concat prefix server))))
					       (luna-make-entity
						'shimbun-elmo-mua
						:folder fld))))))))
	     folders)))))

(luna-define-method elmo-folder-exists-p ((folder elmo-shimbun-folder))
  (if (elmo-shimbun-folder-group-internal folder)
      (progn
	(member
	 (elmo-shimbun-folder-group-internal folder)
	 (shimbun-groups (elmo-shimbun-folder-shimbun-internal
			  folder))))
    t))

(require 'product)
(product-provide (provide 'elmo-shimbun) (require 'elmo-version))

;;; elmo-shimbun.el ends here
