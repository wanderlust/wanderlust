;;; mmelmo.el -- mm-backend by ELMO.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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
(require 'elmo-vars)
(require 'elmo-util)
(require 'mime-parse)
(require 'mmbuffer)

(require 'product)
(product-provide (provide 'mmelmo) (require 'elmo-version))
(require 'mmelmo-imap4)

(eval-and-compile
  (luna-define-class mime-elmo-entity (mime-buffer-entity)
		     (imap folder number msgdb size))
  (luna-define-internal-accessors 'mime-elmo-entity))

(defvar mmelmo-force-reload nil)
(defvar mmelmo-sort-field-list nil)

(defvar mmelmo-header-max-column fill-column
  "*Inserted header is folded with this value.
If function is specified, its return value is used.")

(defvar mmelmo-header-inserted-hook nil
  "*A hook called when header is inserted.")

(defvar mmelmo-entity-content-inserted-hook nil
  "*A hook called when entity-content is inserted.")

(defun mmelmo-get-original-buffer ()
  (let ((ret-val (get-buffer (concat mmelmo-entity-buffer-name "0"))))
    (if (not ret-val)
	(save-excursion
	  (set-buffer (setq ret-val
			    (get-buffer-create
			     (concat mmelmo-entity-buffer-name "0"))))
	  (mmelmo-original-mode)))
    ret-val))

(defun mmelmo-cleanup-entity-buffers ()
  "Cleanup entity buffers of mmelmo."
  (mapcar (lambda (x)
	    (if (string-match mmelmo-entity-buffer-name x)
		(kill-buffer x)))
	  (mapcar 'buffer-name (buffer-list))))

;; For FLIM 1-13.x
(defun-maybe mime-entity-body (entity)
  (luna-send entity 'mime-entity-body))

(defun mmelmo-insert-sorted-header-from-buffer (buffer
						start end
						&optional invisible-fields
						visible-fields
						sorted-fields)
  (let ((the-buf (current-buffer))
	(mode-obj (mime-find-field-presentation-method 'wide))
	field-decoder
	f-b p f-e field-name field field-body
        vf-alist (sl sorted-fields))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (re-search-forward std11-field-head-regexp nil t)
	  (setq f-b (match-beginning 0)
		p (match-end 0)
		field-name (buffer-substring f-b p)
		f-e (std11-field-end))
	  (when (mime-visible-field-p field-name
				      visible-fields invisible-fields)
	    (setq field (intern
			 (capitalize (buffer-substring f-b (1- p))))
		  field-body (buffer-substring p f-e)
		  field-decoder (inline (mime-find-field-decoder-internal
					 field mode-obj)))
            (setq vf-alist (append (list
                                    (cons field-name
                                          (list field-body field-decoder)))
                                   vf-alist))))
        (and vf-alist
             (setq vf-alist
                   (sort vf-alist
                         (function (lambda (s d)
                                     (let ((n 0) re
                                           (sf (car s))
                                           (df (car d)))
                                       (catch 'done
                                         (while (setq re (nth n sl))
                                           (setq n (1+ n))
                                           (and (string-match re sf)
                                                (throw 'done t))
                                           (and (string-match re df)
                                                (throw 'done nil)))
                                         t)))))))
        (with-current-buffer the-buf
          (while vf-alist
            (let* ((vf (car vf-alist))
                   (field-name (car vf))
                   (field-body (car (cdr vf)))
                   (field-decoder (car (cdr (cdr vf)))))
              (insert field-name)
	      (insert (if field-decoder
			  (funcall field-decoder field-body
                                   (string-width field-name)
				   (if (functionp mmelmo-header-max-column)
				       (funcall mmelmo-header-max-column)
				     mmelmo-header-max-column))
			;; Don't decode
			field-body))
              (insert "\n"))
            (setq vf-alist (cdr vf-alist)))
	  (run-hooks 'mmelmo-header-inserted-hook))))))

(defun mmelmo-original-mode ()
  (setq major-mode 'mmelmo-original-mode)
  (setq buffer-read-only t)
  (elmo-set-buffer-multibyte nil)
  (setq mode-name "MMELMO-Original"))

;; For FLIMs without rfc2231 feature .
(if (not (fboundp 'mime-parse-parameters-from-list))
    (defun mime-parse-parameters-from-list (attrlist)
      (let (ret-val)
	(if (not (eq (% (length attrlist) 2) 0))
	    (message "Invalid attributes."))
	(while attrlist
	  (setq ret-val (append ret-val
				(list (cons (downcase (car attrlist))
					    (car (cdr attrlist))))))
	  (setq attrlist (cdr (cdr attrlist))))
	ret-val)))

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
   (mime-buffer-entity-buffer-internal entity)
   (mime-buffer-entity-header-start-internal entity)
   (mime-buffer-entity-header-end-internal entity)
   invisible-fields visible-fields mmelmo-sort-field-list))

(luna-define-method mime-insert-text-content :around ((entity
						       mime-elmo-entity))
  (luna-call-next-method)
  (run-hooks 'mmelmo-entity-content-inserted-hook))

(luna-define-method mime-entity-body ((entity mime-elmo-entity))
  (with-current-buffer (mime-buffer-entity-buffer-internal entity)
    (buffer-substring (mime-buffer-entity-body-start-internal entity)
		      (mime-buffer-entity-body-end-internal entity))))

;;(luna-define-method mime-entity-content ((entity mime-elmo-entity))
;;  (mime-decode-string
;;   (with-current-buffer (mime-buffer-entity-buffer-internal entity)
;;     (buffer-substring (mime-buffer-entity-body-start-internal entity)
;;		       (mime-buffer-entity-body-end-internal entity)))
;;   (mime-entity-encoding entity)))

;;; mmelmo.el ends here
