;;; mmelmo-imap4-1.el -- MM backend of IMAP4 for ELMO (for FLIM 1.13.x).

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
(require 'mmbuffer)
(require 'mmelmo)
(defvar mmelmo-imap4-threshold nil)
(defvar mmelmo-imap4-skipped-parts nil)
(defvar mmelmo-imap4-current-message-structure nil)

;; Buffer local variable.
(defvar mmelmo-imap4-fetched nil)
(make-variable-buffer-local 'mmelmo-imap4-fetched)

(defun mmelmo-imap4-node-id-to-string (node-id)
  (let ((i (length node-id))
	result)
    (while (> i 0)
      (setq result 
	    (concat result 
		    (if result 
			(concat "." (int-to-string 
				     (+ 1 (nth (- i 1) node-id))))
		      (int-to-string (or 
				      (+ 1 (nth (- i 1) node-id))
				      0)))))
      (setq i (- i 1)))
    (or result "0")))

;; parse IMAP4 body structure entity recursively.
(defun mmelmo-imap4-parse-bodystructure-object (folder 
						number msgdb
						node-id object parent)
  (cond
   ((listp (car object));; multipart
    (let (cur-obj children content-type ret-val (num 0))
      (setq ret-val
	    (luna-make-entity
	     (mm-expand-class-name 'elmo-imap4)
	     :folder   folder
	     :number   number
	     :msgdb    msgdb
	     :parent   parent
	     :node-id  node-id))
      (while (and (setq cur-obj (car object))
		  (listp cur-obj))
	(setq children
	      (append children
		      (list
		       (mmelmo-imap4-parse-bodystructure-object
			folder number msgdb
			(append (list num) node-id)
			cur-obj
			ret-val ; myself as parent
			))))
	(setq num (+ num 1))
	(setq object (cdr object)))
      (mime-entity-set-children-internal ret-val children)
      (setq content-type (list (cons 'type 'multipart)))
      (if (elmo-imap4-nth 0 object)
	  (setq content-type (append content-type 
				     (list (cons 'subtype 
						 (intern 
						  (downcase
						   (elmo-imap4-nth
						    0
						    object))))))))
      (setq content-type (append content-type 
				 (mime-parse-parameters-from-list
				  (elmo-imap4-nth 1 object))))
      (mime-entity-set-content-type-internal ret-val content-type)
      ret-val))
   (t ;; singlepart
    (let (content-type ret-val)
      ;; append size information into location
      (setq content-type (list (cons 'type (intern (downcase (car object))))))
      (if (elmo-imap4-nth 1 object)
	  (setq content-type (append content-type
				     (list 
				      (cons 'subtype 
					    (intern 
					     (downcase
					      (elmo-imap4-nth 1 object))))))))
      (if (elmo-imap4-nth 2 object)
	  (setq content-type (append content-type 
				     (mime-parse-parameters-from-list
				      (elmo-imap4-nth 2 object)))))
      (setq ret-val 
	    (luna-make-entity
	     (mm-expand-class-name 'elmo-imap4)
	     :folder folder
	     :number number
	     :size (nth 6 object)
	     :content-type content-type
	     :parent parent
	     :node-id node-id))
      (mime-entity-set-encoding-internal ret-val
					 (and (elmo-imap4-nth 5 object)
					      (downcase 
					       (elmo-imap4-nth 5 object))))
      ret-val))))

(defun mmelmo-imap4-multipart-p (entity)
  (eq (cdr (assq 'type (mime-entity-content-type entity))) 'multipart))

(defun mmelmo-imap4-rfc822part-p (entity)
  (eq (cdr (assq 'type (mime-entity-content-type entity))) 'rfc822))

(defun mmelmo-imap4-textpart-p (entity)
  (eq (cdr (assq 'type (mime-entity-content-type entity))) 'text))
      
(defun mmelmo-imap4-get-mime-entity (folder number msgdb)
  (let* ((spec (elmo-folder-get-spec folder))
	 (session (elmo-imap4-get-session spec)))
    (elmo-imap4-session-select-mailbox session (elmo-imap4-spec-mailbox spec))
    (mmelmo-imap4-parse-bodystructure-object
     folder
     number
     msgdb
     nil ; node-id
     (elmo-imap4-response-value
      (elmo-imap4-response-value
       (elmo-imap4-send-command-wait
	session
	(format 
	 (if elmo-imap4-use-uid
	     "uid fetch %s bodystructure"
	   "fetch %s bodystructure")
	 number)) 'fetch) 'bodystructure)
     nil ; parent
     )))

(defun mmelmo-imap4-read-part (entity)
  (if (or (not mmelmo-imap4-threshold)
	  (not (mime-elmo-entity-size-internal entity))
	  (and (mime-elmo-entity-size-internal entity)
	       mmelmo-imap4-threshold
	       (<= (mime-elmo-entity-size-internal entity)
		   mmelmo-imap4-threshold)))
      (progn
	(cond ((mmelmo-imap4-multipart-p entity)) ; noop
	      (t (insert (elmo-imap4-read-part 
			  (mime-elmo-entity-folder-internal entity)
			  (mime-elmo-entity-number-internal entity)
			  (mmelmo-imap4-node-id-to-string 
			   (mime-entity-node-id-internal entity))))))
	(setq mmelmo-imap4-fetched t)
	(mime-buffer-entity-set-body-start-internal entity (point-min))
	(mime-buffer-entity-set-body-end-internal entity (point-max)))
    (setq mmelmo-imap4-fetched nil)
    (mime-buffer-entity-set-body-start-internal entity (point-min))
    (mime-buffer-entity-set-body-end-internal entity (point-min))
    (setq mmelmo-imap4-skipped-parts 
	  (append
	   mmelmo-imap4-skipped-parts
	   (list (mmelmo-imap4-node-id-to-string 
		  (mime-entity-node-id-internal entity)))))))

(defun mmelmo-imap4-insert-body (entity)
  (mime-buffer-entity-set-body-start-internal entity (- (point) 1))
  (if (or (not mmelmo-imap4-threshold)
	  (not (mime-elmo-entity-size-internal entity))
	  (and (mime-elmo-entity-size-internal entity)
	       mmelmo-imap4-threshold	
	       (<= (mime-elmo-entity-size-internal entity)
		   mmelmo-imap4-threshold)))
      (insert (elmo-imap4-read-part
	       (mime-elmo-entity-folder-internal entity)
	       (mime-elmo-entity-number-internal entity) "1"))
    (setq mmelmo-imap4-skipped-parts 
	  (append
	   mmelmo-imap4-skipped-parts
	   (list (mmelmo-imap4-node-id-to-string 
		  (mime-entity-node-id-internal entity)))))))

;;; mime-elmo-imap4-entity class definitions.
(luna-define-class mime-elmo-imap4-entity (mime-buffer-entity)
		   (imap folder number msgdb size))
(luna-define-internal-accessors 'mime-elmo-imap4-entity)

(luna-define-method initialize-instance ((entity mime-elmo-imap4-entity)
					 &rest init-args)
  "The initialization method for elmo-imap4.
mime-elmo-entity has its own instance variable 
`imap', `folder', `msgdb', and `size'.
These value must be specified as argument for `luna-make-entity'."  
  (apply (car (luna-class-find-functions
	       (luna-find-class 'standard-object)
	       'initialize-instance))
	 entity init-args))

(defun mmelmo-imap4-mime-entity-buffer (entity)
  (if (mime-buffer-entity-buffer-internal entity)
      (save-excursion
	(set-buffer (mime-buffer-entity-buffer-internal entity))
	(unless (mime-root-entity-p entity)
	  (unless mmelmo-imap4-fetched
	    (setq mmelmo-imap4-skipped-parts nil) ; No need?
	    (let ((mmelmo-imap4-threshold
		   (mime-elmo-entity-size-internal entity)))
	      (mime-buffer-entity-set-buffer-internal entity nil)
	      (message "Fetching skipped part...")
	      (mmelmo-imap4-mime-entity-buffer entity)
	      (message "Fetching skipped part...done."))
	    (setq mmelmo-imap4-fetched t)))
	(mime-buffer-entity-buffer-internal entity))
    ;; No buffer exist.
    (save-excursion
      (set-buffer (get-buffer-create 
		   (concat mmelmo-entity-buffer-name
			   (mmelmo-imap4-node-id-to-string 
			    (mime-entity-node-id-internal entity)))))
      (mmelmo-original-mode)
      (mime-buffer-entity-set-buffer-internal entity (current-buffer))
      (let ((buffer-read-only nil))
	(erase-buffer)
	(mime-entity-node-id entity)
	(if (mime-root-entity-p entity)
	    (progn
	      ;; root entity
	      (setq mmelmo-imap4-current-message-structure entity)
	      (setq mime-message-structure entity)
	      (setq mmelmo-imap4-skipped-parts nil)
	      ;; insert header
	      (insert (elmo-imap4-read-part 
		       (mime-elmo-entity-folder-internal entity)
		       (mime-elmo-entity-number-internal entity)
		       "header"))
	      (mime-buffer-entity-set-header-start-internal 
	       entity (point-min))
	      (mime-buffer-entity-set-header-end-internal
	       entity (max (- (point) 1) 1))
	      (if (null (mime-entity-children-internal entity))
		  (progn
		    (mime-buffer-entity-set-body-start-internal 
		     entity (point))
		    ;; insert body if size is OK.
		    (mmelmo-imap4-insert-body entity)
		    (mime-buffer-entity-set-body-end-internal 
		     entity (point)))))
	  (setq mime-message-structure 
		mmelmo-imap4-current-message-structure)
	  (mmelmo-imap4-read-part entity)))
      (current-buffer))))

; mime-entity-children

;; override generic function for dynamic body fetching.
(luna-define-method mime-entity-body ((entity 
				       mime-elmo-imap4-entity))
  (save-excursion
    (set-buffer (mmelmo-imap4-mime-entity-buffer entity))
    (buffer-substring (mime-buffer-entity-body-start-internal entity)
		      (mime-buffer-entity-body-end-internal entity))))

(luna-define-method mime-entity-content ((entity 
					  mime-elmo-imap4-entity))
  (save-excursion
    (set-buffer (mmelmo-imap4-mime-entity-buffer entity))
    (mime-decode-string
     (buffer-substring (mime-buffer-entity-body-start-internal entity)
		       (mime-buffer-entity-body-end-internal entity))
     (mime-entity-encoding entity))))

(luna-define-method mime-entity-fetch-field ((entity mime-elmo-imap4-entity) 
					     field-name)
  (save-excursion
    (save-restriction
      (when (mime-buffer-entity-buffer-internal entity)
	(set-buffer (mime-buffer-entity-buffer-internal entity))
	(if (and (mime-buffer-entity-header-start-internal entity)
		 (mime-buffer-entity-header-end-internal entity))
	    (progn
	      (narrow-to-region 
	       (mime-buffer-entity-header-start-internal entity)
	       (mime-buffer-entity-header-end-internal entity))
	      (std11-fetch-field field-name))
	  nil)))))

(luna-define-method mime-insert-header ((entity mime-elmo-imap4-entity)
					&optional invisible-fields 
					visible-fields)
  (mmelmo-insert-sorted-header-from-buffer
   (mmelmo-imap4-mime-entity-buffer entity)
   (mime-buffer-entity-header-start-internal entity)
   (mime-buffer-entity-header-end-internal entity)
   invisible-fields visible-fields))

(luna-define-method mime-entity-header-buffer ((entity mime-elmo-imap4-entity))
  (mime-buffer-entity-buffer-internal entity))

(luna-define-method mime-entity-body-buffer ((entity mime-elmo-imap4-entity))
  (mime-buffer-entity-buffer-internal entity))

(luna-define-method mime-write-entity-content ((entity mime-elmo-imap4-entity)
					       filename)
  (let ((mmelmo-imap4-threshold (mime-elmo-entity-size-internal entity)))
    (if (mime-buffer-entity-buffer-internal entity)
	(save-excursion
	  (set-buffer (mime-buffer-entity-buffer-internal entity))
	  (unless mmelmo-imap4-fetched
	    (setq mmelmo-imap4-skipped-parts nil) ; No need?
	    (mime-buffer-entity-set-buffer-internal entity nil) ; To re-fetch.
	    ))
      (unless mmelmo-imap4-fetched
	(setq mmelmo-imap4-skipped-parts nil) ; No need?
	(let ((mmelmo-imap4-threshold (mime-elmo-entity-size-internal entity)))
	  (mime-buffer-entity-set-buffer-internal entity nil) ; To re-fetch.
	  (message "Fetching skipped part...")
	  (mime-buffer-entity-set-buffer-internal
	   entity
	   (mmelmo-imap4-mime-entity-buffer entity))
	  (message "Fetching skipped part...done.")))
      (with-current-buffer (mime-buffer-entity-buffer-internal entity)
	(mime-write-decoded-region 
	 (mime-buffer-entity-body-start-internal entity)
	 (mime-buffer-entity-body-end-internal entity)
	 filename
	 (or (mime-entity-encoding entity) "7bit"))))))

(provide 'mmelmo-imap4-2)

;;; mmelmo-imap4-2.el ends here
