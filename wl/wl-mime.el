;;; wl-mime.el -- SEMI implementations of MIME processing on Wanderlust.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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

(require 'mime-view)
(require 'mime-edit)
(require 'mime-play)
(require 'mmelmo)

(eval-when-compile
  (defalias-maybe 'Meadow-version 'ignore))

(defvar xemacs-betaname)
(defvar xemacs-codename)
(defvar enable-multibyte-characters)
(defvar mule-version)

;;; Draft

(defalias 'wl-draft-editor-mode 'mime-edit-mode)

(defalias 'wl-draft-decode-message-in-buffer
  'mime-edit-decode-message-in-buffer)

(defun wl-draft-yank-current-message-entity ()
  "Yank currently displayed message entity.
By setting following-method as yank-content."
  (let ((wl-draft-buffer (current-buffer))
	(mime-view-following-method-alist
	 (list (cons 'mmelmo-original-mode
		     (function wl-draft-yank-to-draft-buffer))))
	(mime-preview-following-method-alist
	 (list (cons 'mmelmo-original-mode
		     (function wl-draft-yank-to-draft-buffer)))))
    (if (get-buffer (wl-current-message-buffer))
	(save-excursion
	  (save-restriction
	    (set-buffer (wl-current-message-buffer))
	    (widen)
	    (mime-preview-follow-current-entity))))))

(defalias 'wl-draft-enclose-digest-region 'mime-edit-enclose-digest-region)

(defun wl-draft-preview-message ()
  ""
  (interactive)
  (let* (recipients-message
	 (mime-display-header-hook 'wl-highlight-headers)
	 mime-view-ignored-field-list ; all header.
	 (mime-edit-translate-buffer-hook
	  (append
	   '((lambda ()
	       (setq recipients-message
		     (concat "Recipients: "
			     (mapconcat
			      'identity
			      (wl-draft-deduce-address-list
			       (current-buffer)
			       (point-min)
			       (save-excursion
				 (re-search-forward
				  (concat "^"
					  (regexp-quote mail-header-separator)
					  "$")
				  nil t)
				 (point)))
			      ", ")))
	       (run-hooks 'wl-draft-send-hook)))
	   mime-edit-translate-buffer-hook)))
    (mime-edit-preview-message)
    (let ((buffer-read-only nil))
      (when wl-highlight-body-too
	(wl-highlight-body))
      (run-hooks 'wl-draft-preview-message-hook))
    (message recipients-message)))

(defalias 'wl-draft-caesar-region  'mule-caesar-region)

(defalias 'wl-draft-insert-message 'mime-edit-insert-message)

(defalias 'wl-draft-insert-mail 'mime-edit-insert-mail)

;;; Message

(defun wl-message-decode-mode (outbuf inbuf)
  (let ((mime-view-content-header-filter-hook 'wl-highlight-headers)
	(mime-display-header-hook 'wl-highlight-headers))
    (mime-view-mode nil nil nil inbuf outbuf)))

(defun wl-message-decode-with-all-header (outbuf inbuf)
  (let ((mime-view-ignored-field-regexp "^:$")
	(mime-view-content-header-filter-hook 'wl-highlight-headers)
	(mime-display-header-hook 'wl-highlight-headers)
	mime-view-ignored-field-list)
    (mime-view-mode nil nil nil inbuf outbuf)))

(defun wl-message-delete-mime-out-buf ()
  (let (mime-out-buf mime-out-win)
    (if (setq mime-out-buf (get-buffer mime-echo-buffer-name))
	(if (setq mime-out-win (get-buffer-window mime-out-buf))
	    (delete-window mime-out-win)))))

(defun wl-message-request-partial (folder number)
  (elmo-set-work-buf
   (elmo-read-msg-no-cache folder number (current-buffer))
;;;(mime-parse-buffer nil 'mime-buffer-entity)
   (mime-parse-buffer nil)))

(defalias 'wl-message-read            'mime-preview-scroll-up-entity)
(defalias 'wl-message-next-content    'mime-preview-move-to-next)
(defalias 'wl-message-prev-content    'mime-preview-move-to-previous)
(defalias 'wl-message-play-content    'mime-preview-play-current-entity)
(defalias 'wl-message-extract-content 'mime-preview-extract-current-entity)
(defalias 'wl-message-quit            'mime-preview-quit)
(defalias 'wl-message-button-dispatcher-internal
  'mime-button-dispatcher)

;;; Summary
(defun wl-summary-burst-subr (children target number)
  ;; returns new number.
  (let (content-type message-entity granch)
    (while children
      (setq content-type (mime-entity-content-type (car children)))
      (if (eq (cdr (assq 'type content-type)) 'multipart)
          (setq number (wl-summary-burst-subr
			(mime-entity-children (car children))
			target
			number))
        (when (and (eq (cdr (assq 'type content-type)) 'message)
                   (eq (cdr (assq 'subtype content-type)) 'rfc822))
          (message (format "Bursting...%s" (setq number (+ 1 number))))
          (setq message-entity
                (car (mime-entity-children (car children))))
	  (elmo-append-msg target
			   (mime-entity-body (car children))
			   (mime-entity-fetch-field message-entity
						    "Message-ID"))))
      (setq children (cdr children)))
    number))

(defun wl-summary-burst ()
  ""
  (interactive)
  (let ((raw-buf (wl-message-get-original-buffer))
	children message-entity content-type target)
    (save-excursion
      (setq target wl-summary-buffer-folder-name)
      (while (not (elmo-folder-writable-p target))
	(setq target
	      (wl-summary-read-folder wl-default-folder "to extract to")))
      (wl-summary-set-message-buffer-or-redisplay)
      (save-excursion
	(set-buffer (get-buffer wl-message-buf-name))
	(setq message-entity (get-text-property (point-min) 'mime-view-entity)))
      (set-buffer raw-buf)
      (setq children (mime-entity-children message-entity))
      (when children
	(message "Bursting...")
	(wl-summary-burst-subr children target 0)
	(message "Bursting...done"))
      (if (elmo-folder-plugged-p target)
	  (elmo-commit target)))
    (wl-summary-sync-update3)))

;; internal variable.
(defvar wl-mime-save-dir nil "Last saved directory.")
;;; Yet another save method.
(defun wl-mime-save-content (entity situation)
  (let ((filename (read-file-name "Save to file: "
				  (expand-file-name
				   (or (mime-entity-safe-filename entity)
				       ".")
				   (or wl-mime-save-dir
				       wl-tmp-dir)))))
    (while (file-directory-p filename)
      (setq filename (read-file-name "Please set filename (not directory): "
				     filename)))
    (if (file-exists-p filename)
	(or (yes-or-no-p (format "File %s exists. Save anyway? " filename))
	    (error "Not saved")))
    (setq wl-mime-save-dir (file-name-directory filename))
    (mime-write-entity-content entity filename)))

;;; Yet another combine method.
(defun wl-mime-combine-message/partial-pieces (entity situation)
  "Internal method for wl to combine message/partial messages automatically."
  (interactive)
  (let* ((msgdb (save-excursion
		  (set-buffer wl-message-buffer-cur-summary-buffer)
		  wl-summary-buffer-msgdb))
	 (mime-display-header-hook 'wl-highlight-headers)
	 (folder wl-message-buffer-cur-folder)
	 (id (or (cdr (assoc "id" situation)) ""))
	 (mother (current-buffer))
	 subject-id overviews
	 (root-dir (expand-file-name
		    (concat "m-prts-" (user-login-name))
		    temporary-file-directory))
	 full-file)
    (setq root-dir (concat root-dir "/" (replace-as-filename id)))
    (setq full-file (concat root-dir "/FULL"))
    (if (or (file-exists-p full-file)
	    (not (y-or-n-p "Merge partials? ")))
	(with-current-buffer mother
	  (mime-store-message/partial-piece entity situation))
      (setq subject-id
	    (eword-decode-string
	     (decode-mime-charset-string
	      (mime-entity-read-field entity 'Subject)
	      wl-summary-buffer-mime-charset)))
      (if (string-match "[0-9\n]+" subject-id)
	  (setq subject-id (substring subject-id 0 (match-beginning 0))))
      (setq overviews (elmo-msgdb-get-overview msgdb))
      (catch 'tag
	(while overviews
	  (when (string-match
		 (regexp-quote subject-id)
		 (elmo-msgdb-overview-entity-get-subject (car overviews)))
	    (let* ((message
		    ;; request message at the cursor in Subject buffer.
		    (wl-message-request-partial
		     folder
		     (elmo-msgdb-overview-entity-get-number (car overviews))))
		   (situation (mime-entity-situation message))
		   (the-id (or (cdr (assoc "id" situation)) "")))
	      (when (string= (downcase the-id)
			     (downcase id))
		(with-current-buffer mother
		  (mime-store-message/partial-piece message situation))
		(if (file-exists-p full-file)
		    (throw 'tag nil)))))
	  (setq overviews (cdr overviews)))
	(message "Not all partials found.")))))

(defun wl-mime-header-presentation-method (entity situation)
  (let ((mmelmo-sort-field-list wl-message-sort-field-list))
    (mime-insert-header entity
			wl-message-ignored-field-list
			wl-message-visible-field-list)
    (wl-highlight-headers)))

;;; Setup methods.
(defun wl-mime-setup ()
  (set-alist 'mime-preview-quitting-method-alist
	     'mmelmo-original-mode 'wl-message-exit)
  (set-alist 'mime-view-over-to-previous-method-alist
	     'mmelmo-original-mode 'wl-message-exit)
  (set-alist 'mime-view-over-to-next-method-alist
	     'mmelmo-original-mode 'wl-message-exit)
  (set-alist 'mime-preview-over-to-previous-method-alist
	     'mmelmo-original-mode 'wl-message-exit)
  (set-alist 'mime-preview-over-to-next-method-alist
	     'mmelmo-original-mode 'wl-message-exit)
  (add-hook 'wl-summary-redisplay-hook 'wl-message-delete-mime-out-buf)
  (add-hook 'wl-message-exit-hook 'wl-message-delete-mime-out-buf)

  (ctree-set-calist-strictly
   'mime-acting-condition
   '((type . message) (subtype . partial)
     (method .  wl-mime-combine-message/partial-pieces)
     (request-partial-message-method . wl-message-request-partial)
     (major-mode . mmelmo-original-mode)))
  (ctree-set-calist-strictly
   'mime-acting-condition
   '((mode . "extract")
     (major-mode . mmelmo-original-mode)
     (method . wl-mime-save-content)))
  (set-alist 'mime-preview-following-method-alist
	     'mmelmo-original-mode
	     (function wl-message-follow-current-entity))
  (set-alist 'mime-view-following-method-alist
	     'mmelmo-original-mode
	     (function wl-message-follow-current-entity))
  (set-alist 'mime-edit-message-inserter-alist
	     'wl-draft-mode (function wl-draft-insert-current-message))
  (set-alist 'mime-edit-mail-inserter-alist
	     'wl-draft-mode (function wl-draft-insert-get-message))
  (set-alist 'mime-edit-split-message-sender-alist
	     'wl-draft-mode
	     (cdr (assq 'mail-mode mime-edit-split-message-sender-alist)))
  (set-alist 'mime-raw-representation-type-alist
	     'mmelmo-original-mode 'binary)
  ;; Sort and highlight header fields.
  (or wl-message-ignored-field-list
      (setq wl-message-ignored-field-list
	    mime-view-ignored-field-list))
  (or wl-message-visible-field-list
      (setq wl-message-visible-field-list
	    mime-view-visible-field-list))
  (set-alist 'mime-header-presentation-method-alist
	     'mmelmo-original-mode
	     (function wl-mime-header-presentation-method))
  (add-hook 'mmelmo-entity-content-inserted-hook 'wl-highlight-body))
  

(require 'product)
(product-provide (provide 'wl-mime) (require 'wl-version))

;;; wl-mime.el ends here
