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
(require 'wl-vars)

(eval-when-compile
  (defalias-maybe 'pgg-decrypt-region 'ignore)
  (defalias-maybe 'pgg-display-output-buffer 'ignore)
  (defalias-maybe 'pgg-verify-region 'ignore))

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
		     (function wl-draft-yank-to-draft-buffer))))
	(message-buffer (wl-current-message-buffer)))
    (if message-buffer
	(save-excursion
	  (set-buffer message-buffer)
	  (save-restriction
	    (widen)
	    (cond
	     ((wl-region-exists-p)
	      (wl-mime-preview-follow-current-region))
	     ((not (wl-message-mime-analysis-p
		    (wl-message-buffer-display-type)))
	      (wl-mime-preview-follow-no-mime
	       (wl-message-buffer-display-type)))
	     (t
	      (mime-preview-follow-current-entity)))))
      (error "No message."))))

;; modified mime-preview-follow-current-entity from mime-view.el
(defun wl-mime-preview-follow-no-mime (display-type)
  "Write follow message to current message, without mime.
It calls following-method selected from variable
`mime-preview-following-method-alist'."
  (interactive)
  (let* ((mode (mime-preview-original-major-mode 'recursive))
	 (new-name (format "%s-no-mime" (buffer-name)))
	 new-buf min beg end
	 (entity (get-text-property (point-min) 'elmo-as-is-entity))
	 (the-buf (current-buffer))
	 fields)
    (save-excursion
      (goto-char (point-min))
      (setq min (point-min)
	    beg (re-search-forward "^$" nil t)
	    end (point-max)))
    (save-excursion
      (set-buffer (setq new-buf (get-buffer-create new-name)))
      (erase-buffer)
      (insert-buffer-substring the-buf beg end)
      (goto-char (point-min))
      ;; Insert all headers.
      (let ((elmo-mime-display-header-analysis
	     (wl-message-mime-analysis-p display-type 'header)))
	(elmo-mime-insert-sorted-header entity))
      (let ((f (cdr (assq mode mime-preview-following-method-alist))))
	(if (functionp f)
	    (funcall f new-buf)
	  (message
	   "Sorry, following method for %s is not implemented yet."
	   mode))))))

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
	       (if (and entity
			(eq (mime-entity-media-type entity) 'message)
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
		  current-entity (mime-entity-parent current-entity))))
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
		      (insert (concat field-name ": " ret "\n")))))
	    (setq rest (cdr rest)))))
      (let ((f (cdr (assq mode mime-preview-following-method-alist))))
	(if (functionp f)
	    (funcall f new-buf)
	  (message
	   "Sorry, following method for %s is not implemented yet."
	   mode))))))

(defalias 'wl-draft-enclose-digest-region 'mime-edit-enclose-digest-region)

(defun wl-draft-preview-attributes-list ()
  (if (listp (car wl-draft-preview-attributes-list))
      (elmo-uniq-list
       (nconc (and (wl-message-mail-p)
		   (copy-sequence
		    (cdr (assq 'mail wl-draft-preview-attributes-list))))
	      (and (wl-message-news-p)
		   (copy-sequence
		    (cdr (assq 'news wl-draft-preview-attributes-list))))))
    wl-draft-preview-attributes-list))

(defun wl-draft-show-attributes-buffer (attribute-values)
  (let* ((cur-win (selected-window))
	 (size (min
		(- (window-height cur-win)
		   window-min-height 1)
		(- (window-height cur-win)
		   (max
		    window-min-height
		    (1+ wl-draft-preview-attributes-buffer-lines))))))
    (split-window cur-win (if (> size 0) size window-min-height))
    (select-window (next-window))
    (let ((pop-up-windows nil))
      (switch-to-buffer (get-buffer-create
			 wl-draft-preview-attributes-buffer-name)))
    (with-current-buffer
	(get-buffer wl-draft-preview-attributes-buffer-name)
      (setq buffer-read-only t)
      (let (buffer-read-only)
	(erase-buffer)
	(dolist (pair attribute-values)
	  (insert (capitalize (symbol-name (car pair))) ": "
		  (format "%s" (or (cdr pair) ""))
		  "\n"))
	(goto-char (point-min))
	(wl-highlight-headers)))
    (select-window cur-win)))

(defun wl-draft-hide-attributes-buffer ()
  (let (window buffer)
    (when (setq window (get-buffer-window
			wl-draft-preview-attributes-buffer-name))
      (select-window window)
      (delete-window))
    (when (setq buffer (get-buffer wl-draft-preview-attributes-buffer-name))
      (kill-buffer buffer))))

(defun wl-draft-attribute-recipients ()
  (concat (mapconcat
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
	   ", ")))

(defun wl-draft-attribute-envelope-from ()
  (or wl-envelope-from
      (wl-address-header-extract-address wl-from)))

(defun wl-draft-attribute-smtp-posting-server ()
  (or wl-smtp-posting-server
      (progn (require 'smtp) smtp-server)
      "localhost"))

(defun wl-draft-attribute-smtp-posting-port ()
  (or wl-smtp-posting-port
      (progn (require 'smtp) smtp-service)))

(defun wl-draft-attribute-newsgroups ()
  (std11-field-body "Newsgroups"))

(defun wl-draft-nntp-attribute (attribute &optional alternatives)
  (let ((config (cdr (elmo-string-matched-assoc
		      (std11-field-body "newsgroups")
		      wl-nntp-posting-config-alist)))
	entry)
    (when (stringp config)
      (setq config (list (cons 'server config))))
    (if (setq entry (assq attribute config))
	;; maybe nil
	(cdr entry)
      (let (value)
	(while alternatives
	  (if (setq value (symbol-value (car alternatives)))
	      (setq alternatives nil)
	    (setq alternatives (cdr alternatives))))
	value))))

(defun wl-draft-attribute-nntp-posting-server ()
  (wl-draft-nntp-attribute
   'server
   '(wl-nntp-posting-server elmo-nntp-default-server)))

(defun wl-draft-attribute-nntp-posting-port ()
  (wl-draft-nntp-attribute
   'point
   '(wl-nntp-posting-port elmo-nntp-default-port)))

(defun wl-draft-attribute-value (attr)
  (let ((name (symbol-name attr))
	fsymbol symbol)
    (cond ((and (setq fsymbol (intern-soft
			       (format "wl-draft-attribute-%s" name)))
		(fboundp fsymbol))
	   (funcall fsymbol))
	  ((and (setq symbol (intern-soft (format "wl-%s" name)))
		(boundp symbol))
	   (symbol-value symbol))
	  ((boundp attr)
	   (symbol-value attr)))))

(defun wl-mime-quit-preview ()
  "Quitting method for mime-view."
  (let* ((temp (and (boundp 'mime-edit-temp-message-buffer) ;; for SEMI <= 1.14.6
		    mime-edit-temp-message-buffer))
	 (window (selected-window))
	 buf)
    (mime-preview-kill-buffer)
    (set-buffer temp)
    (setq buf mime-edit-buffer)
    (kill-buffer temp)
    (select-window window)
    (switch-to-buffer buf)))

(defun wl-draft-preview-message ()
  "Preview editing message."
  (interactive)
  (let* (attribute-values
	 (orig-buffer (current-buffer))
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
	  (cons
	   (lambda ()
	     (let ((wl-draft-config-exec-flag config-exec-flag)
		   (wl-draft-parent-folder parent-folder)
		   (copy-buffer (current-buffer)))
	       (wl-copy-local-variables
		orig-buffer
		copy-buffer
		(with-current-buffer orig-buffer
		  (append wl-draft-config-variables
			  (wl-draft-clone-local-variables))))
	       (goto-char current-point)
	       (run-hooks 'wl-draft-send-hook)
	       (condition-case err
		   (setq attribute-values
			 (mapcar
			  (lambda (attr)
			    (cons attr (wl-draft-attribute-value attr)))
			  (if wl-draft-preview-attributes
			      (wl-draft-preview-attributes-list)
			    '(recipients))))
		 (error
		  (kill-buffer (current-buffer))
		  (signal (car err) (cdr err))))))
	   mime-edit-translate-buffer-hook)))
    (mime-edit-preview-message)
    (make-local-variable 'mime-preview-quitting-method-alist)
    (setq mime-preview-quitting-method-alist
	  '((mime-temp-message-mode . wl-mime-quit-preview)))
    (let ((buffer-read-only nil))
      (when wl-highlight-body-too
	(wl-highlight-body))
      (run-hooks 'wl-draft-preview-message-hook))
    (make-local-hook 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook #'wl-draft-hide-attributes-buffer nil t)
    (if wl-draft-preview-attributes
	(ignore-errors ; in case when the window is too small
	 (wl-draft-show-attributes-buffer attribute-values))
      (message (concat "Recipients: "
		       (cdr (assq 'recipients attribute-values)))))))

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
	   (filename (mime-entity-safe-filename entity))
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
		 (elmo-message-fetch-string
		  folder number
		  (elmo-make-fetch-strategy 'entire)))
	  (error "Buffer content differs from actual message")))
      (when (and (elmo-folder-writable-p folder)
		 (buffer-live-p orig-buf)
		 node-id
		 (yes-or-no-p
		  (format "Do you really want to delete part %s? "
			  (wl-mime-node-id-to-string node-id))))
	(when (with-temp-buffer
		(insert-buffer orig-buf)
		(delete-region header-start body-end)
		(goto-char header-start)
		(insert "Content-Type: text/plain; charset=US-ASCII\n")
		(when filename
		  (insert
		   "Content-Disposition:"
		   (mime-encode-field-body
			 (concat ""
				 (and filename
				      (concat " inline; filename=" (std11-wrap-as-quoted-string filename))))
			 "Content-Disposition")
		   "\n"))
		(insert "\n")
		(insert "** This part has been removed by Wanderlust **\n\n")
		(elmo-folder-append-buffer folder))

	  (elmo-folder-move-messages
	   folder (list number)
	   (wl-folder-get-elmo-folder wl-trash-folder))
	  (when (and (elmo-cache-get-path msgid)
		     (file-exists-p (elmo-cache-get-path msgid)))
	    (delete-file (elmo-cache-get-path msgid)))

	  (mime-preview-quit)
	  (wl-summary-delete-messages-on-buffer (list number))
	  (wl-summary-toggle-disp-msg 'off)
	  (setq wl-message-buffer nil)
	  (wl-summary-sync nil "update"))))))

(defun wl-message-decrypt-pgp-nonmime ()
  "Decrypt PGP encrypted region"
  (interactive)
  (require 'pgg)
  (save-excursion
    (beginning-of-line)
    (if (or (re-search-forward "^-+END PGP MESSAGE-+$" nil t)
	    (re-search-backward "^-+END PGP MESSAGE-+$" nil t))
	(let (beg end status)
	  (setq end (match-end 0))
	  (if (setq beg (re-search-backward "^-+BEGIN PGP MESSAGE-+$" nil t))
	      (let ((inhibit-read-only t)
		    (buffer-file-coding-system wl-cs-autoconv))
		(setq status (pgg-decrypt-region beg end))
		(pgg-display-output-buffer beg end status))
	    (message "Cannot find pgp encrypted region")))
      (message "Cannot find pgp encrypted region"))))

(defun wl-message-verify-pgp-nonmime ()
  "Verify PGP signed region"
  (interactive)
  (require 'pgg)
  (save-excursion
    (beginning-of-line)
    (if (and (or (re-search-forward "^-+END PGP SIGNATURE-+$" nil t)
		 (re-search-backward "^-+END PGP SIGNATURE-+$" nil t))
	     (re-search-backward "^-+BEGIN PGP SIGNED MESSAGE-+$" nil t))
	(let (status)
	  (let* ((beg (point))
		 (situation (mime-preview-find-boundary-info))
		 (p-end (aref situation 1))
		 (entity (aref situation 2))
		 (count 0))
	    (goto-char p-end)
	    (while (< beg (point))
	      (if (re-search-backward "^-+BEGIN PGP SIGNED MESSAGE-+$" nil t)
		  (setq count (+ count 1))
		(debug)))
	    (with-temp-buffer
	      (set-buffer-multibyte nil)
	      (insert (mime-entity-body entity))
	      (goto-char (point-max))
	      (while (> count 0)
		(if (re-search-backward "^-+BEGIN PGP SIGNED MESSAGE-+$" nil t)
		    (setq count (- count 1))
		  (debug)))
	      (let ((r-beg (point))
		    (r-end (re-search-forward "^-+END PGP SIGNATURE-+$" nil t)))
		(if r-end
		    (setq status (pgg-verify-region r-beg r-end nil 'fetch))
		  (debug)))))
	  (mime-show-echo-buffer)
	  (set-buffer mime-echo-buffer-name)
	  (set-window-start
	   (get-buffer-window mime-echo-buffer-name)
	   (point-max))
	  (insert-buffer-substring
	   (if status pgg-output-buffer pgg-errors-buffer)))
      (message "Cannot find pgp signed region"))))

;; XXX: encrypted multipart isn't represented as multipart
(defun wl-mime-preview-application/pgp (parent-entity entity situation)
  (require 'pgg)
  (goto-char (point-max))
  (let ((p (point))
	raw-buf to-buf representation-type child-entity)
    (goto-char p)
    (save-restriction
      (narrow-to-region p p)
      (setq to-buf (current-buffer))
      (with-temp-buffer
	(setq raw-buf (current-buffer))
	(mime-insert-entity entity)
	(when (progn
		(goto-char (point-min))
		(re-search-forward "^-+BEGIN PGP MESSAGE-+$" nil t))
	  (pgg-decrypt-region (point-min)(point-max))
	  (delete-region (point-min) (point-max))
	  (insert-buffer pgg-output-buffer)
	  (setq representation-type 'elmo-buffer))
	(setq child-entity (mime-parse-message
			    (mm-expand-class-name representation-type)
			    nil
			    parent-entity
			    (mime-entity-node-id-internal parent-entity)))
	(mime-display-entity
	 child-entity
	 nil
	 `((header . visible)
	   (body . visible)
	   (entity-button . invisible))
	 to-buf)))))

(defun wl-mime-preview-application/pgp-encrypted (entity situation)
  (let* ((entity-node-id (mime-entity-node-id entity))
	 (mother (mime-entity-parent entity))
	 (knum (car entity-node-id))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (orig-entity (nth onum (mime-entity-children mother))))
    (wl-mime-preview-application/pgp entity orig-entity situation)))

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
	     (elmo-folder-append-buffer target))))
    number))

(defun wl-summary-burst (&optional arg)
  "De-capsulate embedded messages in MIME format.
With ARG, ask destination folder."
  (interactive "P")
  (let ((raw-buf (wl-summary-get-original-buffer))
	(view-buf wl-message-buffer)
	message-entity target)
    (save-excursion
      (when (and (null arg)
		 (elmo-folder-writable-p wl-summary-buffer-elmo-folder))
	(setq target wl-summary-buffer-elmo-folder))
      (while (null target)
	(let ((name (wl-summary-read-folder wl-default-folder
					    "to extract to")))
	  (setq target (wl-folder-get-elmo-folder name))
	  (unless (elmo-folder-writable-p target)
	    (message "%s is not writable" name)
	    (setq target nil)
	    (sit-for 1))))
      (wl-summary-set-message-buffer-or-redisplay)
      (with-current-buffer view-buf
	(setq message-entity
	      (get-text-property (point-min) 'mime-view-entity)))
      (when message-entity
	(message "Bursting...")
	(with-current-buffer raw-buf
	  (wl-summary-burst-subr message-entity target 0))
	(message "Bursting...done"))
      (if (elmo-folder-plugged-p target)
	  (elmo-folder-check target)))
    (when (and target
	       (string= wl-summary-buffer-folder-name
			(elmo-folder-name-internal target)))
      (save-excursion (wl-summary-sync-update)))))

;; internal variable.
(defvar wl-mime-save-directory nil "Last saved directory.")
;;; Yet another save method.
(defun wl-mime-save-content (entity situation)
  (let ((filename (expand-file-name
		   (read-file-name "Save to file: "
				   (expand-file-name
				    (or (mime-entity-safe-filename entity)
					".")
				    (or wl-mime-save-directory
					wl-temporary-file-directory))))))
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
  (let* ((folder (with-current-buffer wl-message-buffer-cur-summary-buffer
		   wl-summary-buffer-elmo-folder))
	 (mime-display-header-hook 'wl-highlight-headers)
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
      (catch 'tag
	(elmo-folder-do-each-message-entity (entity folder)
	  (when (string-match
		 (regexp-quote subject-id)
		 (elmo-message-entity-field entity 'subject 'decode))
	    (let* ((message
		    ;; request message at the cursor in Subject buffer.
		    (wl-message-request-partial
		     (elmo-folder-name-internal folder)
		     (elmo-message-entity-number entity)))
		   (situation (mime-entity-situation message))
		   (the-id (or (cdr (assoc "id" situation)) "")))
	      (when (string= (downcase the-id)
			     (downcase id))
		(with-current-buffer mother
		  (mime-store-message/partial-piece message situation))
		(if (file-exists-p full-file)
		    (throw 'tag nil))))))
	(message "Not all partials found.")))))

(defun wl-mime-display-text/plain (entity situation)
  (let ((beg (point)))
    (mime-display-text/plain entity situation)
    (wl-highlight-message beg (point-max) t t)))

(defun wl-mime-display-header (entity situation)
  (let ((elmo-message-ignored-field-list
	 (if wl-message-buffer-require-all-header
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
   'mime-preview-condition
   '((type . application)(subtype . pgp-encrypted)
     (encoding . t)
     (body . invisible)
     (body-presentation-method . wl-mime-preview-application/pgp-encrypted)
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
