;;; elmo-mime.el -- MIME module for ELMO.

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
(require 'elmo-vars)
(require 'mmbuffer)
(require 'mmimap)
(require 'mime-view)

(eval-and-compile
  (luna-define-class mime-elmo-buffer-entity (mime-buffer-entity) ())
  (luna-define-class mime-elmo-imap-entity (mime-imap-entity) ()))

;; Provide backend
(provide 'mmelmo-imap)
(provide 'mmelmo-buffer)

(defvar elmo-message-ignored-field-list mime-view-ignored-field-list)
(defvar elmo-message-visible-field-list mime-view-visible-field-list)
(defvar elmo-message-sorted-field-list nil)

(defcustom elmo-mime-header-max-column fill-column
  "*Header max column number. Default is `fill-colmn'.
If a symbol of function is specified, the function is called and its return
value is used."
  :type '(choice (integer :tag "Column Number")
		 (function :tag "Function"))
  :group 'elmo)

(luna-define-method initialize-instance :after ((entity mime-elmo-buffer-entity)
						&rest init-args)
  entity)

(luna-define-method initialize-instance :around ((entity mime-elmo-imap-entity)
						 &rest init-args)
  (luna-call-next-method))

;;; Insert sorted header.
(defsubst elmo-mime-insert-header-from-buffer (buffer
					       start end
					       &optional invisible-fields
					       visible-fields
					       sort-fields)
  (let ((the-buf (current-buffer))
	(mode-obj (mime-find-field-presentation-method 'wide))
	field-decoder
	f-b p f-e field-name field field-body
        vf-alist (sl sort-fields))
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
				   (if (functionp elmo-mime-header-max-column)
				       (funcall elmo-mime-header-max-column)
				     elmo-mime-header-max-column))
			;; Don't decode
			field-body))
              (insert "\n"))
            (setq vf-alist (cdr vf-alist)))
	  (run-hooks 'mmelmo-header-inserted-hook))))))

(luna-define-generic elmo-mime-insert-sorted-header (entity
						     &optional invisible-fields
						     visible-fields
						     sorted-fields)
  "Insert sorted header fields of the ENTITY.")

(luna-define-method elmo-mime-insert-sorted-header ((entity
						     mime-elmo-buffer-entity)
						    &optional invisible-fields
						    visible-fields
						    sorted-fields)
  (elmo-mime-insert-header-from-buffer
   (mime-buffer-entity-buffer-internal entity)
   (mime-buffer-entity-header-start-internal entity)
   (mime-buffer-entity-header-end-internal entity)
   invisible-fields visible-fields sorted-fields))

(luna-define-method elmo-mime-insert-sorted-header ((entity
						     mime-elmo-imap-entity)
						    &optional invisible-fields
						    visible-fields
						    sorted-fields)
  (let ((the-buf (current-buffer))
	buf p-min p-max)
    (with-temp-buffer
      (insert (mime-imap-entity-header-string entity))
      (setq buf (current-buffer)
	    p-min (point-min)
	    p-max (point-max))
      (set-buffer the-buf)
      (elmo-mime-insert-header-from-buffer buf p-min p-max
					   invisible-fields visible-fields))))

(luna-define-method mime-insert-text-content :around
  ((entity mime-elmo-buffer-entity))
  (luna-call-next-method)
  (run-hooks 'elmo-message-text-content-inserted-hook))

(luna-define-method mime-insert-text-content :around
  ((entity mime-elmo-imap-entity))
  (luna-call-next-method)
  (run-hooks 'elmo-message-text-content-inserted-hook))

(defun elmo-mime-insert-header (entity situation)
  (elmo-mime-insert-sorted-header
   entity
   elmo-message-ignored-field-list
   elmo-message-visible-field-list
   elmo-message-sorted-field-list)
  (run-hooks 'elmo-message-header-inserted-hook))

(defun elmo-make-mime-message-location (folder number strategy rawbuf unread)
;; Return the MIME message location structure.
;; FOLDER is the ELMO folder structure.
;; NUMBER is the number of the message in the FOLDER.
;; STRATEGY is the message fetching strategy.
;; RAWBUF is the output buffer for original message.
;; If second optional argument UNREAD is non-nil, message is not marked
;; as read.
  (if (and strategy
	   (eq (elmo-fetch-strategy-entireness strategy) 'section))
      (luna-make-entity
       'mime-elmo-imap-location
       :folder folder
       :number number
       :rawbuf rawbuf
       :strategy strategy)
    (with-current-buffer rawbuf
      (let (buffer-read-only)
	(erase-buffer)
	(if strategy
	    (elmo-message-fetch folder number strategy
				nil (current-buffer)
				unread))))
    rawbuf))

(defun elmo-mime-message-display (folder number viewbuf rawbuf original-mode
					 &optional ignore-cache unread)
  "Display MIME message. 
A message in the FOLDER with NUMBER is displayed on the VIEWBUF using RAWBUF.
VIEWBUF is a view buffer and RAWBUF is a raw buffer.
ORIGINAL is the major mode of RAWBUF.
If optional argument IGNORE-CACHE is specified, existing cache is ignored.
If second optional argument UNREAD is specified, message is displayed but
keep it as unread.
Return non-nil if not entire message was fetched."
  (let (mime-display-header-hook ; Do nothing.
	entity strategy)
    (setq entity (elmo-msgdb-overview-get-entity number
						 (elmo-folder-msgdb-internal
						  folder)))
    (setq strategy (elmo-find-fetch-strategy folder entity
					     ignore-cache))
    (mime-display-message
     (mime-open-entity
      (if (and strategy
	       (eq (elmo-fetch-strategy-entireness strategy) 'section))
	  'elmo-imap
	'elmo-buffer)
      (elmo-make-mime-message-location
       folder number strategy rawbuf unread))
     viewbuf nil nil original-mode)
    (if strategy
	(or (elmo-fetch-strategy-use-cache strategy)
	    (eq (elmo-fetch-strategy-entireness strategy)
		'section)))))

(defun elmo-mime-display-as-is (folder number viewbuf rawbuf original-mode
				       &optional ignore-cache unread)
  "Display MIME message. 
A message in the FOLDER with NUMBER is displayed on the VIEWBUF using RAWBUF.
VIEWBUF is a view buffer and RAWBUF is a raw buffer.
ORIGINAL is the major mode of RAWBUF.
If optional argument IGNORE-CACHE is specified, existing cache is ignored.
If second optional argument UNREAD is specified, message is displayed but
keep it as unread.
Return non-nil if cache is used."
  (let ((entity (elmo-msgdb-overview-get-entity number
						(elmo-folder-msgdb-internal
						 folder)))
	mime-display-header-hook ; Do nothing.
	cache-file strategy use-cache)
    (setq cache-file (elmo-file-cache-get
		      (elmo-msgdb-overview-entity-get-id entity)))
    (setq use-cache (eq (elmo-file-cache-status cache-file) 'entire))
    (setq strategy (elmo-make-fetch-strategy
		    'entire use-cache (elmo-message-use-cache-p folder number)
		    (elmo-file-cache-path
		     cache-file)))
    (elmo-mime-display-as-is-internal
     (mime-open-entity
      'elmo-buffer
      (elmo-make-mime-message-location
       folder number strategy rawbuf unread))
     viewbuf nil nil original-mode)
    (elmo-fetch-strategy-use-cache strategy)))

;; Replacement of mime-display-message.
(defun elmo-mime-display-as-is-internal (message
					 &optional preview-buffer
					 mother default-keymap-or-function
					 original-major-mode keymap)
  (mime-maybe-hide-echo-buffer)
  (let ((win-conf (current-window-configuration)))
    (or preview-buffer
	(setq preview-buffer
	      (concat "*Preview-" (mime-entity-name message) "*")))
    (or original-major-mode
	(setq original-major-mode major-mode))
    (let ((inhibit-read-only t))
      (set-buffer (get-buffer-create preview-buffer))
      (widen)
      (erase-buffer)
      (if mother
	  (setq mime-mother-buffer mother))
      (setq mime-preview-original-window-configuration win-conf)
      (setq major-mode 'mime-view-mode)
      (setq mode-name "MIME-View")

      (mime-insert-entity message)
      ;(insert (mime-entity-body message))
      ;(insert (mime-entity-body message))

      (decode-coding-region (point-min) (point-max) 'undecided)

      (save-restriction
	(std11-narrow-to-header)
	(run-hooks 'elmo-message-header-inserted-hook))
;      (mime-display-entity message nil
;			   `((entity-button . invisible)
;			     (header . visible)
;			     (major-mode . ,original-major-mode))
;			   preview-buffer)

      (use-local-map
       (or keymap
	   (if default-keymap-or-function
	       (mime-view-define-keymap default-keymap-or-function)
	     mime-view-mode-default-map)))
      (let ((point
	     (next-single-property-change (point-min) 'mime-view-entity)))
	(if point
	    (goto-char point)
	  (goto-char (point-min))
	  (search-forward "\n\n" nil t)))
      (run-hooks 'mime-view-mode-hook)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      preview-buffer)))

(require 'product)
(product-provide (provide 'elmo-mime) (require 'elmo-version))

;; elmo-mime.el ends here