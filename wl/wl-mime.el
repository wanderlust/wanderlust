;;; wl-mime.el --- SEMI implementations of MIME processing on Wanderlust.

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
(require 'elmo)

;;; Draft

(defalias 'wl-draft-editor-mode 'mime-edit-mode)

(defalias 'wl-draft-decode-message-in-buffer
  'mime-edit-decode-message-in-buffer)

(defun wl-draft-yank-current-message-entity ()
  "Yank currently displayed message entity.
By setting following-method as yank-content.

If region is active, yank region contents instead. \(this feature is available
if and only if `transient-mark-mode' \(GNU Emacs\) or `zmacs-regions' \(XEmacs\)
has Non-nil value\)"
  (let ((wl-draft-buffer (current-buffer))
	(mime-view-following-method-alist
	 (list (cons 'wl-original-message-mode
		     (function wl-draft-yank-to-draft-buffer))))
	(mime-preview-following-method-alist
	 (list (cons 'wl-original-message-mode
		     (function wl-draft-yank-to-draft-buffer)))))
    (if (get-buffer (wl-current-message-buffer))
	(save-excursion
	  (set-buffer (wl-current-message-buffer))
	  (save-restriction
	    (widen)
	    (if (wl-region-exists-p)
		(wl-mime-preview-follow-current-region)
	      (mime-preview-follow-current-entity)))))))

;; modified mime-preview-follow-current-entity from mime-view.el
(defun wl-mime-preview-follow-current-region ()
  "Write follow message to current region.
It calls following-method selected from variable
`mime-preview-following-method-alist'."
  (interactive)
  (let ((r-beg (region-beginning))
	(r-end (region-end))
	(entity (get-text-property (point-min)
				   'mime-view-entity)))
    (let* ((mode (mime-preview-original-major-mode 'recursive))
	   (new-name
	    (format "%s-active-region" (buffer-name)))
	   new-buf
	   (the-buf (current-buffer))
	   fields)
      (save-excursion
	(set-buffer (setq new-buf (get-buffer-create new-name)))
	(erase-buffer)
	(insert ?\n)
	(insert-buffer-substring the-buf r-beg r-end)
	(goto-char (point-min))
	(let ((current-entity
	       (if (and (eq (mime-entity-media-type entity) 'message)
			(eq (mime-entity-media-subtype entity) 'rfc822))
		   (car (mime-entity-children entity))
		 entity)))
	  (while (and current-entity
		      (if (and (eq (mime-entity-media-type
				    current-entity) 'message)
			       (eq (mime-entity-media-subtype
				    current-entity) 'rfc822))
			  nil
			(mime-insert-header current-entity fields)
			t))
	    (setq fields (std11-collect-field-names)
		  current-entity (mime-entity-parent current-entity))
	    ))
	(let ((rest mime-view-following-required-fields-list)
	      field-name ret)
	  (while rest
	    (setq field-name (car rest))
	    (or (std11-field-body field-name)
		(progn
		  (save-excursion
		    (set-buffer the-buf)
		    (let ((entity (when mime-mother-buffer
				    (set-buffer mime-mother-buffer)
				    (get-text-property (point)
						       'mime-view-entity))))
		      (while (and entity
				  (null (setq ret (mime-entity-fetch-field
						   entity field-name))))
			(setq entity (mime-entity-parent entity)))))
		  (if ret
		      (insert (concat field-name ": " ret "\n"))
		    )))
	    (setq rest (cdr rest))
	    ))
	)
      (let ((f (cdr (assq mode mime-preview-following-method-alist))))
	(if (functionp f)
	    (funcall f new-buf)
	  (message
	   "Sorry, following method for %s is not implemented yet."
	   mode)
	  ))
      )))

(defalias 'wl-draft-enclose-digest-region 'mime-edit-enclose-digest-region)

(defun wl-draft-preview-message ()
  "Preview editing message."
  (interactive)
  (let* (recipients-message
	 (current-point (point))
	 (config-exec-flag wl-draft-config-exec-flag)
	 (parent-folder wl-draft-parent-folder)
	 (mime-display-header-hook 'wl-highlight-headers)
	 (mime-header-encode-method-alist
	  (append
	   '((wl-draft-eword-encode-address-list
	      .  (To Cc Bcc Resent-To Resent-Cc Resent-Bcc From)))
	   (if (boundp 'mime-header-encode-method-alist)
	       (symbol-value 'mime-header-encode-method-alist))))
	 mime-view-ignored-field-list	; all header.
	 (mime-edit-translate-buffer-hook
	  (append
	   (list
	    (function
	     (lambda ()
	       (let ((wl-draft-config-exec-flag config-exec-flag)
		     (wl-draft-parent-folder parent-folder))
		 (goto-char current-point)
		 (run-hooks 'wl-draft-send-hook)
		 (setq recipients-message
		       (condition-case err
			   (concat "Recipients: "
				   (mapconcat
				    'identity
				    (wl-draft-deduce-address-list
				     (current-buffer)
				     (point-min)
				     (save-excursion
				       (goto-char (point-min))
				       (re-search-forward
					(concat
					 "^"
					 (regexp-quote mail-header-separator)
					 "$")
					nil t)
				       (point)))
				    ", "))
			 (error
			  (kill-buffer (current-buffer))
			  (signal (car err) (cdr err)))))))))
	   mime-edit-translate-buffer-hook)))
    (mime-edit-preview-message)
    (let ((buffer-read-only nil))
      (when wl-highlight-body-too
	(wl-highlight-body))
      (run-hooks 'wl-draft-preview-message-hook))
    (message "%s" recipients-message)))

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
   (elmo-message-fetch (wl-folder-get-elmo-folder folder)
		       number
		       (elmo-make-fetch-strategy 'entire)
		       nil
		       (current-buffer)
		       'unread)
   (mime-parse-buffer nil)))

(defalias 'wl-message-read		'mime-preview-scroll-up-entity)
(defalias 'wl-message-next-content	'mime-preview-move-to-next)
(defalias 'wl-message-prev-content	'mime-preview-move-to-previous)
(defalias 'wl-message-play-content	'mime-preview-play-current-entity)
(defalias 'wl-message-extract-content	'mime-preview-extract-current-entity)
(defalias 'wl-message-quit		'mime-preview-quit)
(defalias 'wl-message-button-dispatcher-internal
  'mime-button-dispatcher)

(defsubst wl-mime-node-id-to-string (node-id)
  (if (consp node-id)
      (mapconcat (function (lambda (num) (format "%s" (1+ num))))
		 (reverse node-id)
		 ".")
    "0"))

(defun wl-message-delete-current-part ()
  "Delete a part under the cursor from the multipart message."
  (interactive)
  (save-restriction
    (widen)
    (let* ((entity (get-text-property (point) 'mime-view-entity))
	   (node-id (mime-entity-node-id entity))
	   (header-start (mime-buffer-entity-header-start-internal entity))
	   (body-end (mime-buffer-entity-body-end-internal entity))
	   (folder (wl-folder-get-elmo-folder wl-message-buffer-cur-folder))
	   (number wl-message-buffer-cur-number)
	   (msgid (elmo-message-field folder number 'message-id))
	   (orig-buf wl-message-buffer-original-buffer))
      (if (eq (luna-class-name entity) 'mime-elmo-imap-entity)
	  (error "Please fetch the entire message (by typing 'C-u .') and try again"))
      (with-current-buffer orig-buf
	(unless (string-equal
		 (buffer-string)
		 (elmo-message-fetch folder number
				     (elmo-make-fetch-strategy 'entire)))
	  (error "Buffer content differs from actual message")))
      (when (and (elmo-folder-writable-p folder)
		 (buffer-live-p orig-buf)
		 node-id
		 (yes-or-no-p
		  (format "Do you really want to delete part %s? "
			  (wl-mime-node-id-to-string node-id))))
	(with-temp-buffer
	  (insert-buffer orig-buf)
	  (delete-region header-start body-end)
	  (goto-char header-start)
	  (insert "Content-Type: text/plain; charset=US-ASCII\n\n")
	  (insert "** This part has been removed by Wanderlust **\n\n")
	  (elmo-folder-append-buffer folder t))

	(elmo-folder-append-messages
	 (wl-folder-get-elmo-folder wl-trash-folder)
	 folder (list number) nil)
	(elmo-folder-delete-messages folder (list number))

	(when (file-exists-p (elmo-cache-get-path msgid))
	  (delete-file (elmo-cache-get-path msgid)))

	(mime-preview-quit)
	(wl-summary-toggle-disp-msg 'off)
	(wl-summary-sync nil "update")))))

;;; Summary
(defun wl-summary-burst-subr (message-entity target number)
  ;; returns new number.
  (let (content-type entity)
    (setq content-type (mime-entity-content-type message-entity))
    (cond ((eq (cdr (assq 'type content-type)) 'multipart)
	   (dolist (entity (mime-entity-children message-entity))
	     (setq number (wl-summary-burst-subr
			   entity
			   target
			   number))))
	  ((and (eq (cdr (assq 'type content-type)) 'message)
		(eq (cdr (assq 'subtype content-type)) 'rfc822))
	   (message "Bursting...%s" (setq number (+ 1 number)))
	   (setq entity
		 (car (mime-entity-children message-entity)))
	   (with-temp-buffer
	     (insert (mime-entity-body message-entity))
	     (elmo-folder-append-buffer
	      target
	      (mime-entity-fetch-field entity "Message-ID")))))
    number))

(defun wl-summary-burst (&optional arg)
  "De-capsulate embedded messages in MIME format.
With ARG, ask destination folder."
  (interactive "P")
  (let ((raw-buf (wl-summary-get-original-buffer))
	(view-buf wl-message-buffer)
	children message-entity content-type target)
    (save-excursion
      (setq target wl-summary-buffer-elmo-folder)
      (when (or arg (not (elmo-folder-writable-p target)))
	(let ((fld (wl-summary-read-folder wl-default-folder "to extract to")))
	  (setq target (wl-folder-get-elmo-folder fld))))
      (wl-summary-set-message-buffer-or-redisplay)
      (with-current-buffer view-buf
	(setq message-entity (get-text-property (point-min) 'mime-view-entity)))
      (when message-entity
	(message "Bursting...")
	(with-current-buffer raw-buf
	  (wl-summary-burst-subr message-entity target 0))
	(message "Bursting...done"))
      (if (elmo-folder-plugged-p target)
	  (elmo-folder-check target)))
    (wl-summary-sync-update)))

;; internal variable.
(defvar wl-mime-save-directory nil "Last saved directory.")
;;; Yet another save method.
(defun wl-mime-save-content (entity situation)
  (let ((filename (read-file-name "Save to file: "
				  (expand-file-name
				   (or (mime-entity-safe-filename entity)
				       ".")
				   (or wl-mime-save-directory
				       wl-temporary-file-directory)))))
    (while (file-directory-p filename)
      (setq filename (read-file-name "Please set filename (not directory): "
				     filename)))
    (if (and (file-exists-p filename)
	     (not (yes-or-no-p (format "File %s exists. Save anyway? "
				       filename))))
	(message "Not saved")
      (setq wl-mime-save-directory (file-name-directory filename))
      (mime-write-entity-content entity filename))))

;;; Yet another combine method.
(defun wl-mime-combine-message/partial-pieces (entity situation)
  "Internal method for wl to combine message/partial messages automatically."
  (interactive)
  (let* ((msgdb (save-excursion
		  (set-buffer wl-message-buffer-cur-summary-buffer)
		  (wl-summary-buffer-msgdb)))
	 (mime-display-header-hook 'wl-highlight-headers)
	 (folder wl-message-buffer-cur-folder)
	 (id (or (cdr (assoc "id" situation)) ""))
	 (mother (current-buffer))
	 (summary-buf wl-message-buffer-cur-summary-buffer)
	 subject-id overviews
	 (root-dir (expand-file-name
		    (concat "m-prts-" (user-login-name))
		    temporary-file-directory))
	 full-file point)
    (setq root-dir (concat root-dir "/" (replace-as-filename id)))
    (setq full-file (concat root-dir "/FULL"))
    (if (or (file-exists-p full-file)
	    (not (y-or-n-p "Merge partials? ")))
	(with-current-buffer mother
	  (mime-store-message/partial-piece entity situation)
	  (setq wl-message-buffer-cur-summary-buffer summary-buf)
	  (make-variable-buffer-local 'mime-preview-over-to-next-method-alist)
	  (setq mime-preview-over-to-next-method-alist
		(cons (cons 'mime-show-message-mode 'wl-message-exit)
		      mime-preview-over-to-next-method-alist))
	  (make-variable-buffer-local 'mime-preview-over-to-previous-method-alist)
	  (setq mime-preview-over-to-previous-method-alist
		(cons (cons 'mime-show-message-mode 'wl-message-exit)
		      mime-preview-over-to-previous-method-alist)))
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
		     (elmo-msgdb-overview-entity-get-number
		      (car overviews))))
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

(defun wl-mime-display-text/plain (entity situation)
  (let ((beg (point)))
    (mime-display-text/plain entity situation)
    (wl-highlight-message beg (point-max) t t)))

(defun wl-mime-display-header (entity situation)
  (let ((elmo-message-ignored-field-list
	 (if wl-message-buffer-all-header-flag
	     nil
	   wl-message-ignored-field-list))
	(elmo-message-visible-field-list wl-message-visible-field-list)
	(elmo-message-sorted-field-list wl-message-sort-field-list))
    (elmo-mime-insert-header entity situation)
    (wl-highlight-headers)))

(defun wl-mime-decrypt-application/pgp-encrypted (entity situation)
  (let ((summary-buffer wl-message-buffer-cur-summary-buffer)
	(original-buffer wl-message-buffer-original-buffer))
    (mime-decrypt-application/pgp-encrypted entity situation)
    (setq wl-message-buffer-cur-summary-buffer summary-buffer)
    (setq wl-message-buffer-original-buffer original-buffer)))
   

;;; Setup methods.
(defun wl-mime-setup ()
  (set-alist 'mime-preview-quitting-method-alist
	     'wl-original-message-mode 'wl-message-exit)
  (set-alist 'mime-view-over-to-previous-method-alist
	     'wl-original-message-mode 'wl-message-exit)
  (set-alist 'mime-view-over-to-next-method-alist
	     'wl-original-message-mode 'wl-message-exit)
  (set-alist 'mime-preview-over-to-previous-method-alist
	     'wl-original-message-mode 'wl-message-exit)
  (set-alist 'mime-preview-over-to-next-method-alist
	     'wl-original-message-mode 'wl-message-exit)
  (add-hook 'wl-summary-redisplay-hook 'wl-message-delete-mime-out-buf)
  (add-hook 'wl-message-exit-hook 'wl-message-delete-mime-out-buf)

  (ctree-set-calist-strictly
   'mime-preview-condition
   '((type . text) (subtype . plain)
     (body . visible)
     (body-presentation-method . wl-mime-display-text/plain)
     (major-mode . wl-original-message-mode)))

  (ctree-set-calist-strictly
   'mime-acting-condition
   '((type . message) (subtype . partial)
     (method .  wl-mime-combine-message/partial-pieces)
     (request-partial-message-method . wl-message-request-partial)
     (major-mode . wl-original-message-mode)))

  (ctree-set-calist-strictly
   'mime-acting-condition
   '((type . application) (subtype . pgp-encrypted)
     (method . wl-mime-decrypt-application/pgp-encrypted)
     (major-mode . wl-original-message-mode)))

  (ctree-set-calist-strictly
   'mime-acting-condition
   '((mode . "extract")
     (major-mode . wl-original-message-mode)
     (method . wl-mime-save-content)))
  (set-alist 'mime-preview-following-method-alist
	     'wl-original-message-mode
	     (function wl-message-follow-current-entity))
  (set-alist 'mime-view-following-method-alist
	     'wl-original-message-mode
	     (function wl-message-follow-current-entity))
  (set-alist 'mime-edit-message-inserter-alist
	     'wl-draft-mode (function wl-draft-insert-current-message))
  (set-alist 'mime-edit-mail-inserter-alist
	     'wl-draft-mode (function wl-draft-insert-get-message))
  (set-alist 'mime-edit-split-message-sender-alist
	     'wl-draft-mode
	     (cdr (assq 'mail-mode mime-edit-split-message-sender-alist)))
  (set-alist 'mime-raw-representation-type-alist
	     'wl-original-message-mode 'binary)
  ;; Sort and highlight header fields.
  (or wl-message-ignored-field-list
      (setq wl-message-ignored-field-list
	    mime-view-ignored-field-list))
  (or wl-message-visible-field-list
      (setq wl-message-visible-field-list
	    mime-view-visible-field-list))
  (set-alist 'mime-header-presentation-method-alist
	     'wl-original-message-mode
	     'wl-mime-display-header)
  ;; To avoid overriding wl-draft-mode-map.
  (when (boundp 'mime-setup-signature-key-alist)
    (unless (assq 'wl-draft-mode mime-setup-signature-key-alist)
      (setq mime-setup-signature-key-alist
	    (cons '(wl-draft-mode . "\C-c\C-w")
		  mime-setup-signature-key-alist)))))

(require 'product)
(product-provide (provide 'wl-mime) (require 'wl-version))

;;; wl-mime.el ends here
