;;; wl-mime.el -- tm implementations of MIME processing on Wanderlust.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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

(autoload 'mime/editor-mode "tm-edit" nil t)
(autoload 'mime/edit-again "tm-edit" nil t)

(eval-when-compile (require 'tm-edit))

(defalias 'wl-draft-editor-mode 'mime/editor-mode)

(defun wl-draft-decode-message-in-buffer (&optional default-content-type)
  (when default-content-type
    (insert "Content-type: " default-content-type "\n")
    (insert "\n"))
  (mime-editor::edit-again 'code-conversion))

(defun wl-draft-yank-current-message-entity ()
  "Yank currently displayed message entity.
By setting following-method as yank-content."
  (let ((wl-draft-buffer (current-buffer))
	(mime-viewer/following-method-alist
	 (list (cons 'wl-message-original-mode
		     (function wl-draft-yank-to-draft-buffer)))))
    (if (get-buffer (wl-current-message-buffer))
	(save-excursion
	  (save-restriction
	    (set-buffer (wl-current-message-buffer))
	    (setq mime::preview/mother-buffer nil)
	    (widen)
	    (mime-viewer/follow-content))))))

(defmacro wl-draft-enclose-digest-region (beg end)
  (` (mime-editor/enclose-region "digest" (, beg) (, end))))

(defun wl-draft-preview-message ()
  (interactive)
  (let ((mime-viewer/content-header-filter-hook 'wl-highlight-headers)
	(mime-viewer/ignored-field-regexp "^:$")
	(mime-editor/translate-buffer-hook
	 (append
	  (list 'wl-draft-config-exec)
	  mime-editor/translate-buffer-hook)))
    (mime-editor/preview-message)
    (let ((buffer-read-only nil))
    (let ((buffer-read-only nil))
      (when wl-highlight-body-too
	(wl-highlight-body))
      (run-hooks 'wl-draft-preview-message-hook)))))

(defmacro wl-draft-caesar-region (beg end)
  (` (tm:caesar-region)))

(defalias 'wl-draft-insert-message 'mime-editor/insert-message)

(defalias 'wl-draft-insert-mail 'mime-editor/insert-mail)

;;; Message

(defun wl-message-decode-mode (outbuf inbuf)
  (let ((mime-viewer/content-header-filter-hook 'wl-highlight-headers))
    (mime/viewer-mode nil nil nil inbuf outbuf)))

(defun wl-message-decode-with-all-header (outbuf inbuf)
  (let ((mime-viewer/ignored-field-regexp "^:$")
	(mime-viewer/content-header-filter-hook 'wl-highlight-headers))
    (mime/viewer-mode nil nil nil inbuf outbuf)))

(defun wl-message-delete-mime-out-buf ()
  (let (mime-out-buf mime-out-win)
    (if (setq mime-out-buf (get-buffer mime/output-buffer-name))
	(if (setq mime-out-win (get-buffer-window mime-out-buf))
	    (delete-window mime-out-win)))))

(defun wl-message-request-partial (folder number)
  (elmo-set-work-buf
   (elmo-read-msg-no-cache folder number (current-buffer))
   (mime/parse-message nil nil)))

(defalias 'wl-message-read            'mime-viewer/scroll-up-content)
(defalias 'wl-message-next-content    'mime-viewer/next-content)
(defalias 'wl-message-prev-content    'mime-viewer/previous-content)
(defalias 'wl-message-play-content    'mime-viewer/play-content)
(defalias 'wl-message-extract-content 'mime-viewer/extract-content)
(defalias 'wl-message-quit            'mime-viewer/quit)
(defalias 'wl-message-button-dispatcher-internal
  'tm:button-dispatcher)

;;; Summary
(defun wl-summary-burst-subr (children target number)
  ;; returns new number.
  (let (content-type message-entity granch)
    (while children
      (setq content-type (mime::content-info/type (car children)))
      (if (string-match "multipart" content-type)
	  (setq number (wl-summary-burst-subr
			(mime::content-info/children (car children))
			target
			number))
	(when (string= "message/rfc822" (downcase content-type))
	  (message (format "Bursting...%s" (setq number (+ 1 number))))
	  (setq message-entity
		(car (mime::content-info/children (car children))))
	  (save-restriction
	    (narrow-to-region (mime::content-info/point-min message-entity)
			      (mime::content-info/point-max message-entity))
	    (elmo-append-msg target
			     (buffer-substring (point-min) (point-max))
			     (std11-field-body "Message-ID")))))
      (setq children (cdr children)))))

(defun wl-summary-burst ()
  (interactive)
  (let ((raw-buf (wl-message-get-original-buffer))
	target
	children message-entity content-type)
    (save-excursion
      (setq target wl-summary-buffer-folder-name)
      (while (not (elmo-folder-writable-p target))
	(setq target
	      (wl-summary-read-folder wl-default-folder "to extract to ")))
      (wl-summary-set-message-buffer-or-redisplay)
      (set-buffer raw-buf)
      (setq children (mime::content-info/children mime::article/content-info))
      (message "Bursting...")
      (when children
	(wl-summary-burst-subr children target 0))
      (message "Bursting...done"))
    (if (elmo-folder-plugged-p target)
	(elmo-commit target))
    (wl-summary-sync-update3)))

;; internal variable.
(defvar wl-mime-save-dir nil "Last saved directory.")
;;; Yet another save method.
(defun wl-mime-save-content (beg end cal)
  (goto-char beg)
  (let* ((name
	  (save-restriction
	    (narrow-to-region beg end)
	    (mime-article/get-filename cal)))
	 (encoding (cdr (assq 'encoding cal)))
	 (filename (read-file-name "Save to file: "
				   (expand-file-name
				    (or name ".")
				    (or wl-mime-save-dir
					wl-tmp-dir))))
	 tmp-buf)
    (while (file-directory-p filename)
      (setq filename (read-file-name "Please set filename (not directory): "
				     filename)))
    (if (file-exists-p filename)
        (or (yes-or-no-p (format "File %s exists. Save anyway? " filename))
            (error "Not saved")))
    (setq wl-mime-save-dir (file-name-directory filename))
    (setq tmp-buf (generate-new-buffer (file-name-nondirectory filename)))
    (re-search-forward "\n\n")
    (append-to-buffer tmp-buf (match-end 0) end)
    (save-excursion
      (set-buffer tmp-buf)
      (mime-decode-region (point-min)(point-max) encoding)
      (as-binary-output-file (write-file filename))
      (kill-buffer tmp-buf))))

;;; Yet another combine method.
(defun wl-mime-combine-message/partial-pieces (beg end cal)
  (interactive)
  (let* (folder
	 (msgdb (save-excursion
		  (set-buffer wl-message-buffer-cur-summary-buffer)
		  (setq folder wl-summary-buffer-folder-name)
		  wl-summary-buffer-msgdb))
	 (mime-viewer/content-header-filter-hook 'wl-highlight-headers)
	 (id (cdr (assoc "id" cal)))
	 (mother mime::article/preview-buffer)
	 (target (cdr (assq 'major-mode cal)))
	 (article-buffer (buffer-name (current-buffer)))
	 (subject-buf (eval (cdr (assq 'summary-buffer-exp cal))))
	 subject-id overviews
	 (root-dir (expand-file-name
		    (concat "m-prts-" (user-login-name)) mime/tmp-dir))
	 full-file)
    (setq root-dir (concat root-dir "/" (replace-as-filename id)))
    (setq full-file (concat root-dir "/FULL"))
    (if (null target)
	(error "%s is not supported. Sorry." target))
    (if (or (file-exists-p full-file)
	    (not (y-or-n-p "Merge partials?")))
	(mime-article/decode-message/partial beg end cal)
      (message "Merging...")
      (let (cinfo the-id parameters)
	(setq subject-id
	      (eword-decode-string
	       (decode-mime-charset-string
		(std11-field-body "Subject")
		wl-summary-buffer-mime-charset)))
	(if (string-match "[0-9\n]+" subject-id)
	    (setq subject-id (substring subject-id 0 (match-beginning 0))))
	(setq overviews (elmo-msgdb-get-overview msgdb))
	(catch 'tag
	  (while overviews
	    (when (string-match
		   (regexp-quote subject-id)
		   (elmo-msgdb-overview-entity-get-subject
		    (car overviews)))
	      (setq cinfo
		    (wl-message-request-partial
		     folder
		     (elmo-msgdb-overview-entity-get-number (car overviews))))
	      (setq parameters (mime::content-info/parameters cinfo))
	      (setq the-id (assoc-value "id" parameters))
	      (if (string= the-id id)
		  (progn
		    (set-buffer elmo-work-buf-name)
		    (setq mime::article/preview-buffer
			  (get-buffer wl-message-buf-name))
		    (mime-article/decode-message/partial
		     (point-min)(point-max) parameters)
		    (if (file-exists-p full-file)
			(throw 'tag nil)))))
	    (setq overviews (cdr overviews)))
	  (message "Not all partials found."))))))

(defun wl-mime-setup ()
  (require 'tm-view)
  (set-alist 'mime-viewer/quitting-method-alist
	     'wl-message-original-mode 'wl-message-exit)
  (set-alist 'mime-viewer/over-to-previous-method-alist
	     'wl-message-original-mode 'wl-message-exit)
  (set-alist 'mime-viewer/over-to-next-method-alist
	     'wl-message-original-mode  'wl-message-exit)
  (add-hook 'wl-summary-redisplay-hook 'wl-message-delete-mime-out-buf)
  (add-hook 'wl-message-exit-hook      'wl-message-delete-mime-out-buf)
  (set-atype 'mime/content-decoding-condition
	     '((type . "message/partial")
	       (method . wl-message-combine-message/partial-pieces)
	       (major-mode . wl-message-original-mode)
	       (summary-buffer-exp . wl-summary-buffer-name)))
  (set-atype 'mime/content-decoding-condition
	     '((mode . "extract")
	       (method . wl-mime-save-content)
	       (major-mode . wl-message-original-mode))
	     'remove
	     '((method "tm-file"  nil 'file 'type 'encoding 'mode 'name)
	       (mode . "extract"))
	     'replacement)
  (set-alist 'mime-viewer/following-method-alist
	     'wl-message-original-mode
	     (function wl-message-follow-current-entity))

  (set-alist 'mime-editor/message-inserter-alist
	     'wl-draft-mode (function wl-draft-insert-current-message))
  (set-alist 'mime-editor/mail-inserter-alist
	     'wl-draft-mode (function wl-draft-insert-get-message))
  (set-alist 'mime-editor/split-message-sender-alist
	     'wl-draft-mode
	     (cdr (assq 'mail-mode
			mime-editor/split-message-sender-alist)))
  (setq mime-viewer/code-converter-alist
	(append
	 (list (cons 'wl-message-original-mode 'mime-charset/decode-buffer))
	 mime-viewer/code-converter-alist))
  (defvar-maybe mime::preview/mother-buffer nil))
  
(require 'product)
(product-provide (provide 'tm-wl) (require 'wl-version))

;;; tm-wl.el ends here
