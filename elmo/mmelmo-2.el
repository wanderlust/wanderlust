;;; mmelmo-2.el -- mm-backend (for FLIM 1.13.x) by ELMO.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news
;; Time-stamp: <2000-03-21 17:39:07 teranisi>

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

(require 'mmelmo-imap4)
(require 'mmelmo)
(require 'mmbuffer)

(defvar mmelmo-force-reload nil)
(defvar mmelmo-sort-field-list nil)

(eval-and-compile
  (luna-define-class mime-elmo-entity (mime-buffer-entity)
		     (imap folder number msgdb size))
  (luna-define-internal-accessors 'mime-elmo-entity))

(luna-define-method initialize-instance :after ((entity mime-elmo-entity)
						&rest init-args)
  "The initialization method for elmo.
mime-elmo-entity has its own member variable,
`imap', `folder', `msgdb' and `size'.
imap:   boolean. if non-nil, entity becomes mime-elmo-imap4-entity class.
folder: string.  folder name.
msgdb:  msgdb of elmo.
size:   size of the entity."
  (if (mime-elmo-entity-imap-internal entity)
      ;; use imap part fetching.
      ;; child mime-entity's class becomes `mime-elmo-imap4-entity'
      ;; which implements `entity-buffer' method.
      (progn
	(let (new-entity)
	  (mime-buffer-entity-set-buffer-internal entity nil)
	  (setq new-entity
		(mmelmo-imap4-get-mime-entity
		 (mime-elmo-entity-folder-internal entity) ; folder
		 (mime-elmo-entity-number-internal entity) ; number
		 (mime-elmo-entity-msgdb-internal entity)  ; msgdb
		 ))
	  (mime-entity-set-content-type-internal
	   entity
	   (mime-entity-content-type-internal new-entity))
	  (mime-entity-set-encoding-internal
	   entity
	   (mime-entity-encoding-internal new-entity))
	  (mime-entity-set-children-internal
	   entity
	   (mime-entity-children-internal new-entity))
	  (mime-elmo-entity-set-size-internal
	   entity
	   (mime-elmo-entity-size-internal new-entity))
	  (mime-entity-set-representation-type-internal
	   entity 'mime-elmo-imap4-entity)
	  entity))
    (set-buffer (mime-buffer-entity-buffer-internal entity))
    (mmelmo-original-mode)
    (when (mime-root-entity-p entity)
      (let ((buffer-read-only nil)
	    header-end body-start)
	(erase-buffer)
	(elmo-read-msg-with-buffer-cache
	 (mime-elmo-entity-folder-internal entity)
	 (mime-elmo-entity-number-internal entity)
	 (current-buffer)
	 (mime-elmo-entity-msgdb-internal entity)
	 mmelmo-force-reload)
	(goto-char (point-min))
	(if (re-search-forward
	     (concat "^" (regexp-quote mail-header-separator) "$\\|^$" )
	     nil t)
	    (setq header-end (match-beginning 0)
		  body-start (if (= (match-end 0) (point-max))
				 (point-max)
			       (1+ (match-end 0))))
	  (setq header-end (point-min)
		body-start (point-min)))
	(mime-buffer-entity-set-header-start-internal entity (point-min))
	(mime-buffer-entity-set-header-end-internal entity header-end)
	(mime-buffer-entity-set-body-start-internal entity body-start)
	(mime-buffer-entity-set-body-end-internal entity (point-max))
	(save-restriction
	  (narrow-to-region (mime-buffer-entity-header-start-internal entity)
			    (mime-buffer-entity-header-end-internal entity))
	  (mime-entity-set-content-type-internal
	   entity
	   (let ((str (std11-fetch-field "Content-Type")))
	     (if str
		 (mime-parse-Content-Type str)
	       ))))))
    entity))

(luna-define-method mime-insert-header ((entity mime-elmo-entity)
					&optional invisible-fields
					visible-fields)
  (mmelmo-insert-sorted-header-from-buffer
   (mime-entity-buffer entity)
   (mime-buffer-entity-header-start-internal entity)
   (mime-buffer-entity-header-end-internal entity)
   invisible-fields visible-fields mmelmo-sort-field-list))

(luna-define-method mime-insert-text-content :around ((entity
						       mime-elmo-entity))
  (luna-call-next-method)
  (run-hooks 'mmelmo-entity-content-inserted-hook))

;(luna-define-method mime-entity-content ((entity mime-elmo-entity))
;  (mime-decode-string
;   (with-current-buffer (mime-buffer-entity-buffer-internal entity)
;     (buffer-substring (mime-buffer-entity-body-start-internal entity)
;		       (mime-buffer-entity-body-end-internal entity)))
;   (mime-entity-encoding entity)))

(provide 'mmelmo-2)

;;; mmelmo-2.el ends here
