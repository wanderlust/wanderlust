;;; mmelmo-imap4-1.el -- MM backend of IMAP4 for ELMO (for FLIM 1.12.x).

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

(require 'mmelmo)

(defvar mmelmo-imap4-threshold nil)
(defvar mmelmo-imap4-skipped-parts nil)
(defvar mmelmo-imap4-current-message-structure nil)

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
(defun mmelmo-imap4-parse-bodystructure-entity (location node-id entity parent)
  (cond
   ((listp (car entity));; multipart
    (let (cur-entity
	  children
	  content-type ret-val
	  (num 0))
      (setq ret-val
	    (make-mime-entity-internal 'elmo-imap4
				       location
				       nil	; content-type
				       nil            ; children
				       parent         ; parent
				       node-id	; node-id
				       ))
      (while (and (setq cur-entity (car entity))
		  (listp cur-entity))
	(setq children
	      (append children
		      (list
		       (mmelmo-imap4-parse-bodystructure-entity
			(list (nth 0 location)
			      (nth 1 location)
			      (nth 2 location)
			      (append (list num) node-id))
			(append (list num) node-id)
			cur-entity
			ret-val ; myself as parent
			))))
	(setq num (+ num 1))
	(setq entity (cdr entity))
	)
      (mime-entity-set-children-internal ret-val children)
      (setq content-type (list (cons 'type 'multipart)))
      (setq content-type (append content-type
				 (list (cons 'subtype
					     (intern
					      (downcase (car entity)))))))
      (setq content-type (append content-type
				 (mime-parse-parameters-from-list
				  (elmo-imap4-nth 1 entity))))
      (mime-entity-set-content-type-internal ret-val content-type)
      ret-val))
   (t ;; singlepart
    (let (content-type result)
      ;; append size information into location
      (setq location (append location (list (nth 6 entity))))
      (setq content-type (list (cons 'type (intern (downcase (car entity))))))
      (if (elmo-imap4-nth 1 entity)
	  (setq content-type (append content-type
				     (list
				      (cons 'subtype
					    (intern
					     (downcase
					      (elmo-imap4-nth 1 entity))))))))
      (if (elmo-imap4-nth 2 entity)
	  (setq content-type (append content-type
				     (mime-parse-parameters-from-list
				      (elmo-imap4-nth 2 entity)))))
      (setq result (make-mime-entity-internal 'elmo-imap4
					      location
					      content-type	; content-type
					      nil     ; children
					      parent  ; parent
					      node-id ; node-id
					      ))
      (mime-entity-set-encoding-internal result
					 (and (elmo-imap4-nth 5 entity)
					      (downcase
					       (elmo-imap4-nth 5 entity))))
      result))))

(defun mmelmo-imap4-parse-bodystructure-string (location string)
  (save-excursion
    (let ((tmp-buffer (get-buffer-create " *ELMO bodystructure TMP*"))
	  (raw-buffer (current-buffer))
	  str
	  entity)
      (set-buffer tmp-buffer)
      (erase-buffer)
      (insert string)
      (goto-char (point-min))
      (when (search-forward "FETCH" nil t)
	(narrow-to-region (match-end 0) (point-max))
	(while (re-search-forward "{\\([0-9]+\\)}\r\n" nil t)
	  (goto-char (+ (point)
			(string-to-int (elmo-match-buffer 1))))
	  (setq str (buffer-substring (match-end 0) (point)))
	  (delete-region (match-beginning 0) (point))
	  (insert (prin1-to-string str))); (insert "\""))
	(setq entity
	      (nth 1 (memq 'BODYSTRUCTURE
			   (read (buffer-string)))))
	(set-buffer raw-buffer)
	(mmelmo-imap4-parse-bodystructure-entity location nil entity nil)
	))))

(defun mmelmo-imap4-multipart-p (entity)
  (eq (cdr (assq 'type (mime-entity-content-type entity))) 'multipart)
  )

(defun mmelmo-imap4-rfc822part-p (entity)
  (eq (cdr (assq 'type (mime-entity-content-type entity))) 'rfc822)
  )

(defun mmelmo-imap4-textpart-p (entity)
  (eq (cdr (assq 'type (mime-entity-content-type entity))) 'text)
  )
      
(defun mmelmo-imap4-get-mime-entity (location)
  (save-excursion
    (let* ((spec (elmo-folder-get-spec (nth 0 location)))
	   (msg (nth 1 location))
	   (connection (elmo-imap4-get-connection spec))
	   (process (elmo-imap4-connection-get-process connection))
	   (read-it t)
	   response errmsg ret-val bytes)
      (when (elmo-imap4-spec-folder spec)
	(save-excursion
	  (when (not (string= (elmo-imap4-connection-get-cwf connection)
			      (elmo-imap4-spec-folder spec)))
	    (if (null (setq response
			    (elmo-imap4-select-folder
			     (elmo-imap4-spec-folder spec) connection)))
		(error "Select folder failed")))
	  (elmo-imap4-send-command (process-buffer process)
				   process
				   (format "uid fetch %s bodystructure"
					   msg))
	  (if (null (setq response (elmo-imap4-read-contents
				    (process-buffer process) process)))
	      (error "Fetching body structure failed")))
	(mmelmo-imap4-parse-bodystructure-string location
						 response); make mime-entity
	))))

(defun mmelmo-imap4-read-part (entity location)
  (if (or (not mmelmo-imap4-threshold)
	  (not (nth 4 location))
	  (and (nth 4 location)
	       mmelmo-imap4-threshold
	       (<= (nth 4 location) mmelmo-imap4-threshold)))
      (cond ((mmelmo-imap4-multipart-p entity)) ; noop
	    (t
	     (insert (elmo-imap4-read-part
		      (nth 0 location)
		      (nth 1 location)
		      (mmelmo-imap4-node-id-to-string
		       (nth 3 location))))
	     (mime-entity-set-body-start-internal entity (point-min))
	     (mime-entity-set-body-end-internal entity (point-max))))
    (setq mmelmo-imap4-skipped-parts
	  (append
	   mmelmo-imap4-skipped-parts
	   (list (mmelmo-imap4-node-id-to-string
		  (nth 3 location)))))))

(defun mmelmo-imap4-read-body (entity)
  (let ((location (mime-entity-location-internal entity)))
    (mime-entity-set-body-start-internal entity (- (point) 1))
    (if (or (not mmelmo-imap4-threshold)
	    (not (nth 4 location))
	    (and (nth 4 location)
		 mmelmo-imap4-threshold
		 (<= (nth 4 location) mmelmo-imap4-threshold)))
	(insert (elmo-imap4-read-part (nth 0 location)
				      (nth 1 location)
				      "1"
				      ))
      (setq mmelmo-imap4-skipped-parts
	    (append
	     mmelmo-imap4-skipped-parts
	     (list
	      (mmelmo-imap4-node-id-to-string
	       (nth 3 location))))))))

;;; mm-backend definitions for elmo-imap4
(mm-define-backend elmo-imap4 (elmo))

(mm-define-method initialize-instance ((entity elmo-imap4))
  (let ((new-entity (mmelmo-imap4-get-mime-entity
		     (mime-entity-location-internal entity))))
    ;; ...
    (aset entity 1
	  (mime-entity-location-internal new-entity))
    (mime-entity-set-content-type-internal
     entity
     (mime-entity-content-type-internal new-entity))
    (mime-entity-set-encoding-internal
     entity
     (mime-entity-encoding-internal new-entity))
    (mime-entity-set-children-internal
     entity
     (mime-entity-children-internal new-entity))
    (mime-entity-set-body-start-internal
     entity
     (mime-entity-body-start-internal new-entity))
    (mime-entity-set-body-end-internal
     entity
     (mime-entity-body-end-internal new-entity))))

(mm-define-method entity-buffer ((entity elmo-imap4))
  (let ((buffer (get-buffer-create
		 (concat mmelmo-entity-buffer-name
			 (mmelmo-imap4-node-id-to-string
			  (mime-entity-node-id-internal entity)))))
	(location (mime-entity-location-internal entity)))
    (set-buffer buffer)
    (mmelmo-original-mode)
    (mime-entity-set-buffer-internal entity buffer)  ; set buffer.
    (let ((buffer-read-only nil))
      (erase-buffer)
      (if (nth 3 location)   ; not top
	  (progn
	    (setq mime-message-structure mmelmo-imap4-current-message-structure)
	    (mmelmo-imap4-read-part entity location))
	;; TOP
	(setq mmelmo-imap4-current-message-structure entity)
	(setq mime-message-structure entity)
	(setq mmelmo-imap4-skipped-parts nil)
	;;      (setq mmelmo-fetched-entire-message nil)
	;; header
	(insert (elmo-imap4-read-part (nth 0 location)
				      (nth 1 location)
				      "header"
				      ))
	(mime-entity-set-header-start-internal entity (point-min))
	(mime-entity-set-header-end-internal entity (- (point) 1))
	(if (not (mime-entity-children-internal entity)) ; body part!
	    (progn
	      (mmelmo-imap4-read-body entity)
	      (mime-entity-set-body-end-internal entity (point))
	      ))))
    buffer))

(mm-define-method entity-point-min ((entity elmo-imap4))
  (let ((buffer (mime-entity-buffer-internal entity)))
    (set-buffer buffer)
    (point-min)))

(mm-define-method entity-point-max ((entity elmo-imap4))
  (let ((buffer (mime-entity-buffer-internal entity)))
    (set-buffer buffer)
    (point-max)))

(mm-define-method entity-children ((entity elmo-imap4))
  (let* ((content-type (mime-entity-content-type entity))
         (primary-type (mime-content-type-primary-type content-type)))
    (cond ((eq primary-type 'multipart)
           (mime-parse-multipart entity)
           )
          ((and (eq primary-type 'message)
                (memq (mime-content-type-subtype content-type)
                      '(rfc822 news external-body)
                      ))
	   (save-excursion
	     (set-buffer (mime-entity-buffer-internal entity))
	     (mime-entity-set-body-start-internal entity (point-min))
	     (mime-entity-set-body-end-internal entity (point-max)))
           (mime-parse-encapsulated entity)
           ))
    ))

(mm-define-method entity-body-start ((entity elmo-imap4))
  (point-min))

(mm-define-method entity-body-end ((entity elmo-imap4))
  (point-max))

;; override generic function for dynamic body fetching.
(mm-define-method entity-content ((entity elmo-imap4))
  (save-excursion
    (set-buffer (mime-entity-buffer entity))
    (mime-decode-string
     (buffer-substring (mime-entity-body-start entity)
		       (mime-entity-body-end entity))
     (mime-entity-encoding entity))))

(mm-define-method fetch-field ((entity elmo-imap4) field-name)
  (save-excursion
    (let ((buf (mime-entity-buffer-internal entity)))
      (when buf
	(set-buffer buf)
	(save-restriction
	  (if (and (mime-entity-header-start-internal entity)
		   (mime-entity-header-end-internal entity))
	      (progn
		(narrow-to-region
		 (mime-entity-header-start-internal entity)
		 (mime-entity-header-end-internal entity))
		(std11-fetch-field field-name))
	    nil))))))

(provide 'mmelmo-imap4-1)

;;; mmelmo-imap4-1.el ends here
