;;; wl-xmas.el --- Wanderlust modules for XEmacsen.

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003
;;  Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000, 2001, 2002, 2003 Katsumi Yamaoka <yamaoka@jpl.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Katsumi Yamaoka <yamaoka@jpl.org>
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

(eval-when-compile
  (require 'wl-folder)
  (require 'wl-summary)
  (require 'wl-draft)
  (require 'wl-message)
  (require 'wl-highlight)
  (defvar-maybe wl-draft-mode-map (make-sparse-keymap))
  (defalias-maybe 'toolbar-make-button-list 'ignore))

(add-hook 'wl-folder-mode-hook 'wl-setup-folder)
(add-hook 'wl-folder-mode-hook 'wl-folder-init-icons)

(add-hook 'wl-init-hook 'wl-biff-init-icons)
(add-hook 'wl-init-hook 'wl-plugged-init-icons)

(add-hook 'wl-summary-mode-hook 'wl-setup-summary)

(add-hook 'wl-message-display-internal-hook 'wl-setup-message)

(defvar wl-use-toolbar (if (featurep 'toolbar) 'default-toolbar nil))
(defvar wl-plugged-glyph nil)
(defvar wl-unplugged-glyph nil)
(defvar wl-biff-mail-glyph nil)
(defvar wl-biff-nomail-glyph nil)

(defvar wl-folder-toolbar
  '([wl-folder-jump-to-current-entity
     wl-folder-jump-to-current-entity t "Enter Current Folder"]
    [wl-folder-next-entity
     wl-folder-next-entity t "Next Folder"]
    [wl-folder-prev-entity
     wl-folder-prev-entity t "Previous Folder"]
    [wl-folder-check-current-entity
     wl-folder-check-current-entity t "Check Current Folder"]
    [wl-folder-sync-current-entity
     wl-folder-sync-current-entity t "Sync Current Folder"]
    [wl-draft
     wl-draft t "Write a New Message"]
    [wl-folder-goto-draft-folder
     wl-folder-goto-draft-folder t "Go to Draft Folder"]
    [wl-folder-empty-trash
     wl-folder-empty-trash t "Empty Trash"]
    [wl-exit
     wl-exit t "Quit Wanderlust"]
    )
  "The Folder buffer toolbar.")

(defvar wl-summary-toolbar
  '([wl-summary-read
     wl-summary-read t "Read Messages"]
    [wl-summary-next
     wl-summary-next t "Next Message"]
    [wl-summary-prev
     wl-summary-prev t "Previous Message"]
    [wl-summary-jump-to-current-message
     wl-summary-jump-to-current-message t "Jump to Current Message"]
    [wl-summary-sync-force-update
     wl-summary-sync-force-update t "Sync Current Folder"]
    [wl-summary-dispose
     wl-summary-dispose t "Dispose Current Message"]
    [wl-summary-set-flags
     wl-summary-set-flags t "Set Flags"]
    [wl-draft
     wl-summary-write-current-folder t "Write for Current Folder"]
    [wl-summary-reply
     wl-summary-reply t "Reply to Current Message" ]
    [wl-summary-reply-with-citation
     wl-summary-reply-with-citation t "Reply to Current Message with Citation"]
    [wl-summary-forward
     wl-summary-forward t "Forward Current Message"]
    [wl-summary-exit
     wl-summary-exit t "Exit Current Summary"]
    )
  "The Summary buffer toolbar.")

(defvar wl-message-toolbar
  '([wl-message-read
     wl-message-read t "Read Contents"]
    [wl-message-next-content
     wl-message-next-content t "Next Content"]
    [wl-message-prev-content
     wl-message-prev-content t "Previous Content"]
    [wl-message-quit
     wl-message-quit t "Back to Summary"]
    [wl-message-play-content
     wl-message-play-content t "Play Content"]
    [wl-message-extract-content
     wl-message-extract-content t "Extract Content"]
    )
  "The Message buffer toolbar.")

(defalias 'wl-draft-insert-signature 'insert-signature);; for draft toolbar.

(defvar wl-draft-toolbar
  '([wl-draft-send-from-toolbar
     wl-draft-send-from-toolbar t "Send Current Draft"]
    [wl-draft-yank-original
     wl-draft-yank-original t "Yank Displaying Message"]
    [wl-draft-insert-signature
     wl-draft-insert-signature t "Insert Signature"]
    [wl-draft-kill
     wl-draft-kill t "Kill Current Draft"]
    [wl-draft-save-and-exit
     wl-draft-save-and-exit t "Save Draft and Exit"]
    )
  "The Draft buffer toolbar.")

(defun wl-xmas-setup-toolbar (bar)
  (let ((dir wl-icon-directory)
	icon up down disabled name)
    (when dir
      (while bar
	(setq icon (aref (car bar) 0)
	      name (symbol-name icon)
	      bar (cdr bar))
	(unless (boundp icon)
	  (setq up (expand-file-name (concat name "-up.xpm") dir)
		down (expand-file-name (concat name "-down.xpm") dir)
		disabled (expand-file-name (concat name "-disabled.xpm") dir))
	  (if (file-exists-p up)
	      (set icon (toolbar-make-button-list
			 up (and (file-exists-p down) down)
			 (and (file-exists-p disabled) disabled)))
	    (setq bar nil
		  dir nil)))))
    dir))

(defun wl-xmas-make-icon-glyph (icon-string icon-file
					    &optional locale tag-set)
  (let ((glyph (make-glyph (vector 'string :data icon-string))))
    (when wl-highlight-folder-with-icon
      (set-glyph-image glyph
		       (vector 'xpm :file (expand-file-name
					   icon-file wl-icon-directory))
		       locale tag-set 'prepend))
    glyph))

(eval-when-compile
  (defsubst wl-xmas-setup-folder-toolbar ()
    (and wl-use-toolbar
	 (wl-xmas-setup-toolbar wl-folder-toolbar)
	 (set-specifier (symbol-value wl-use-toolbar)
			(cons (current-buffer) wl-folder-toolbar))))

  (defsubst wl-xmas-setup-summary-toolbar ()
    (and wl-use-toolbar
	 (wl-xmas-setup-toolbar wl-summary-toolbar)
	 (set-specifier (symbol-value wl-use-toolbar)
			(cons (current-buffer) wl-summary-toolbar))))

  (defsubst wl-xmas-setup-draft-toolbar ()
    (and wl-use-toolbar
	 (wl-xmas-setup-toolbar wl-draft-toolbar)
	 (set-specifier (symbol-value wl-use-toolbar)
			(cons (current-buffer) wl-draft-toolbar)))))

(defun wl-xmas-setup-message-toolbar ()
  (and wl-use-toolbar
       (wl-xmas-setup-toolbar wl-message-toolbar)
       (set-specifier (symbol-value wl-use-toolbar)
		      (cons (current-buffer) wl-message-toolbar))))

(defvar wl-folder-toggle-icon-list
  '((wl-folder-opened-glyph       . wl-opened-group-folder-icon)
    (wl-folder-closed-glyph       . wl-closed-group-folder-icon)))

(eval-when-compile
  (defsubst wl-xmas-highlight-folder-group-line (glyph text-face numbers)
    (let ((start (match-beginning 1))
	  (end (match-end 1)))
      (let ((extent (or (map-extents
			 (lambda (extent maparg)
			   (and (eq start (extent-start-position extent))
				(eq end (extent-end-position extent))
				extent))
			 nil start start nil nil 'end-glyph)
			(make-extent start end))))
	(set-extent-properties extent `(end-open t start-closed t invisible t))
	(set-extent-end-glyph
	 extent
	 (or (get glyph 'glyph)
	     (put glyph 'glyph
		  (wl-xmas-make-icon-glyph
		   (buffer-substring-no-properties start end)
		   (symbol-value
		    (cdr (assq glyph wl-folder-toggle-icon-list))))))))
      (let ((inhibit-read-only t))
	(when wl-use-highlight-mouse-line
	  (put-text-property start (point-at-eol) 'mouse-face 'highlight))
	(setq start end
	      end (point-at-eol))
	(if (and wl-highlight-folder-by-numbers
		 numbers (nth 0 numbers) (nth 1 numbers)
		 (re-search-forward "[0-9-]+/[0-9-]+/[0-9-]+" end t))
	    (let* ((unsync (nth 0 numbers))
		   (unread (nth 1 numbers))
		   (face (cond ((and unsync (zerop unsync))
				(if (and unread (zerop unread))
				    'wl-highlight-folder-zero-face
				  'wl-highlight-folder-unread-face))
			       ((and unsync
				     (>= unsync
					 wl-folder-many-unsync-threshold))
				'wl-highlight-folder-many-face)
			       (t
				'wl-highlight-folder-few-face))))
	      (if (numberp wl-highlight-folder-by-numbers)
		  (progn
		    (put-text-property start (match-beginning 0)
				       'face text-face)
		    (put-text-property (match-beginning 0) (point) 'face face))
		(put-text-property start end 'face face)))
	  (put-text-property start end 'face text-face))))))

(defun wl-highlight-folder-current-line (&optional numbers)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (fld-name)
      (cond
       (;; opened folder group
	(and (wl-folder-buffer-group-p)
	     (looking-at wl-highlight-folder-opened-regexp))
	(wl-xmas-highlight-folder-group-line 'wl-folder-opened-glyph
					     'wl-highlight-folder-opened-face
					     numbers))
       (;; closed folder group
	(and (wl-folder-buffer-group-p)
	     (looking-at wl-highlight-folder-closed-regexp))
	(wl-xmas-highlight-folder-group-line 'wl-folder-closed-glyph
					     'wl-highlight-folder-closed-face
					     numbers))
       (;; basic folder
	(and (setq fld-name (wl-folder-get-folder-name-by-id
			     (get-text-property (point) 'wl-folder-entity-id)))
	     (looking-at "[ \t]+\\([^ \t]+\\)"))
	(let ((start (match-beginning 1)))
	  (let ((extent (or (map-extents
			     (lambda (extent maparg)
			       (and (eq start (extent-start-position extent))
				    (eq start (extent-end-position extent))
				    extent))
			     nil start start nil nil 'begin-glyph)
			    (make-extent start start))))
	    (let (type)
	      (set-extent-begin-glyph
	       extent
	       (cond
		((string= fld-name wl-trash-folder);; trash folder
		 (let ((num (nth 2 numbers)));; number of messages
		   (get (if (or (not num) (zerop num))
			    'wl-folder-trash-empty-glyph
			  'wl-folder-trash-glyph)
			'glyph)))
		((string= fld-name wl-draft-folder);; draft folder
		 (get 'wl-folder-draft-glyph 'glyph))
		((string= fld-name wl-queue-folder);; queue folder
		 (get 'wl-folder-queue-glyph 'glyph))
		(;; and one of many other folders
		 (setq type (or (elmo-folder-type fld-name)
				(elmo-folder-type-internal
				 (elmo-make-folder fld-name))))
		 (get (intern (format "wl-folder-%s-glyph" type)) 'glyph))))))
	  (let ((end (point-at-eol)))
	    (when wl-use-highlight-mouse-line
	      (put-text-property start end 'mouse-face 'highlight))
	    (let ((text-face
		   (if (looking-at (format "^[ \t]*\\(?:%s\\|%s\\)"
					   wl-folder-unsubscribe-mark
					   wl-folder-removed-mark))
		       'wl-highlight-folder-killed-face
		     'wl-highlight-folder-unknown-face)))
	      (if (and wl-highlight-folder-by-numbers
		       numbers (nth 0 numbers) (nth 1 numbers)
		       (re-search-forward "[0-9-]+/[0-9-]+/[0-9-]+" end t))
		  (let* ((unsync (nth 0 numbers))
			 (unread (nth 1 numbers))
			 (face (cond
				((and unsync (zerop unsync))
				 (if (and unread (zerop unread))
				     'wl-highlight-folder-zero-face
				   'wl-highlight-folder-unread-face))
				((and unsync
				      (>= unsync
					  wl-folder-many-unsync-threshold))
				 'wl-highlight-folder-many-face)
				(t
				 'wl-highlight-folder-few-face))))
		    (if (numberp wl-highlight-folder-by-numbers)
			(progn
			  (put-text-property start (match-beginning 0)
					     'face text-face)
			  (put-text-property (match-beginning 0)
					     (match-end 0)
					     'face face))
		      ;; Remove previous face.
		      (put-text-property start (match-end 0) 'face nil)
		      (put-text-property start (match-end 0) 'face face)))
		(put-text-property start end 'face text-face))))))))))

(defun wl-highlight-plugged-current-line ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  extent switch)
      (beginning-of-line)
      (when (looking-at "[ \t]*\\(\\[\\([^]]+\\)\\]\\)")
	(setq switch (elmo-match-buffer 2))
	(when (and (setq extent (extent-at (match-end 1) nil nil nil 'at))
		   (extent-end-glyph extent))
	  (delete-extent extent))
	(setq extent (make-extent (match-beginning 1) (match-end 1)))
	(set-extent-property extent 'end-open t)
	(set-extent-property extent 'start-closed t)
	(set-extent-property extent 'invisible t)
	(set-extent-end-glyph extent (if (string= switch wl-plugged-plug-on)
					 wl-plugged-glyph
				       wl-unplugged-glyph))))))

(defun wl-plugged-set-folder-icon (folder string)
  (let ((string (copy-sequence string))
	(len (length string))
	type)
    (if (string= folder wl-queue-folder)
	(put-text-property 0 len 'begin-glyph
			   (get 'wl-folder-queue-glyph 'glyph)
			   string)
      (if (setq type (elmo-folder-type folder))
	  (put-text-property 0 len
			     'begin-glyph
			     (get (intern (format "wl-folder-%s-glyph" type))
				  'glyph)
			     string)))
    string))

(defvar wl-folder-internal-icon-list
  ;; alist of (glyph . icon-file)
  '((wl-folder-nntp-glyph	. wl-nntp-folder-icon)
    (wl-folder-imap4-glyph	. wl-imap-folder-icon)
    (wl-folder-pop3-glyph	. wl-pop-folder-icon)
    (wl-folder-localdir-glyph	. wl-localdir-folder-icon)
    (wl-folder-localnews-glyph	. wl-localnews-folder-icon)
    (wl-folder-internal-glyph	. wl-internal-folder-icon)
    (wl-folder-multi-glyph	. wl-multi-folder-icon)
    (wl-folder-filter-glyph	. wl-filter-folder-icon)
    (wl-folder-archive-glyph	. wl-archive-folder-icon)
    (wl-folder-pipe-glyph	. wl-pipe-folder-icon)
    (wl-folder-maildir-glyph	. wl-maildir-folder-icon)
    (wl-folder-nmz-glyph	. wl-nmz-folder-icon)
    (wl-folder-shimbun-glyph	. wl-shimbun-folder-icon)
    (wl-folder-file-glyph	. wl-file-folder-icon)
    (wl-folder-trash-empty-glyph . wl-empty-trash-folder-icon)
    (wl-folder-draft-glyph	. wl-draft-folder-icon)
    (wl-folder-queue-glyph	. wl-queue-folder-icon)
    (wl-folder-trash-glyph	. wl-trash-folder-icon)))

(defun wl-folder-init-icons ()
  (dolist (icon wl-folder-internal-icon-list)
    (unless (get (car icon) 'glyph)
      (put (car icon) 'glyph
	   (wl-xmas-make-icon-glyph "" (symbol-value (cdr icon)))))))

(defun wl-plugged-init-icons ()
  (unless wl-plugged-glyph
    (setq wl-plugged-glyph (wl-xmas-make-icon-glyph
			    wl-plug-state-indicator-on wl-plugged-icon)
	  wl-unplugged-glyph (wl-xmas-make-icon-glyph
			      wl-plug-state-indicator-off wl-unplugged-icon))
    (let ((extent (make-extent nil nil)))
      (let ((keymap (make-sparse-keymap)))
	(define-key keymap 'button2
	  (make-modeline-command-wrapper 'wl-toggle-plugged))
	(set-extent-keymap extent keymap)
	(set-extent-property extent 'help-echo
			     "button2 toggles plugged status"))
      (setq wl-modeline-plug-state-on (cons extent wl-plugged-glyph)
	    wl-modeline-plug-state-off (cons extent wl-unplugged-glyph)))))

(defun wl-biff-init-icons ()
  (unless wl-biff-mail-glyph
    (setq wl-biff-mail-glyph (wl-xmas-make-icon-glyph
			      wl-biff-state-indicator-on
			      wl-biff-mail-icon)
	  wl-biff-nomail-glyph (wl-xmas-make-icon-glyph
				wl-biff-state-indicator-off
				wl-biff-nomail-icon))
    (let ((extent (make-extent nil nil)))
      (let ((keymap (make-sparse-keymap)))
	(define-key keymap 'button2
	  (make-modeline-command-wrapper 'wl-biff-check-folders))
	(set-extent-keymap extent keymap)
	(set-extent-property extent 'help-echo "button2 checks new mails"))
      (setq wl-modeline-biff-state-on (cons extent wl-biff-mail-glyph)
	    wl-modeline-biff-state-off (cons extent wl-biff-nomail-glyph)))))

(defun wl-make-date-string ()
  (let ((s (current-time-string)))
    (string-match "\\`\\([A-Z][a-z][a-z]\\) +[A-Z][a-z][a-z] +[0-9][0-9]? *[0-9][0-9]?:[0-9][0-9]:[0-9][0-9] *[0-9]?[0-9]?[0-9][0-9]"
		  s)
    (concat (wl-match-string 1 s) ", "
	    (timezone-make-date-arpa-standard s (current-time-zone)))))

(defun wl-setup-folder ()
  (and (featurep 'scrollbar)
       (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (wl-xmas-setup-folder-toolbar))

(defvar dragdrop-drop-functions)

(defun wl-setup-summary ()
  (make-local-variable 'dragdrop-drop-functions)
  (setq dragdrop-drop-functions '((wl-dnd-default-drop-message t t)))
  (and (featurep 'scrollbar)
       (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (wl-xmas-setup-summary-toolbar))

(defalias 'wl-setup-message 'wl-xmas-setup-message-toolbar)

(defun wl-message-define-keymap ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "D" 'wl-message-delete-current-part)
    (define-key keymap "l" 'wl-message-toggle-disp-summary)
    (define-key keymap "\C-c:d" 'wl-message-decrypt-pgp-nonmime)
    (define-key keymap "\C-c:v" 'wl-message-verify-pgp-nonmime)
    (define-key keymap "w" 'wl-draft)
    (define-key keymap 'button4 'wl-message-wheel-down)
    (define-key keymap 'button5 'wl-message-wheel-up)
    (define-key keymap [(shift button4)] 'wl-message-wheel-down)
    (define-key keymap [(shift button5)] 'wl-message-wheel-up)
    (set-keymap-parent wl-message-button-map keymap)
    (define-key wl-message-button-map 'button2
      'wl-message-button-dispatcher)
    keymap))

(defun wl-message-wheel-up (event)
  (interactive "e")
  (if (string-match (regexp-quote wl-message-buffer-name)
		    (regexp-quote (buffer-name)))
      (wl-message-prev-page)
    (let ((cur-buf (current-buffer))
	  proceed)
      (save-selected-window
	(select-window (event-window event))
	(set-buffer cur-buf)
	(setq proceed (wl-message-next-page)))
      (when proceed
	(if (memq 'shift (event-modifiers event))
	    (wl-summary-down t)
	  (wl-summary-next t))))))

(defun wl-message-wheel-down (event)
  (interactive "e")
  (if (string-match (regexp-quote wl-message-buffer-name)
		    (regexp-quote (buffer-name)))
      (wl-message-prev-page)
    (let ((cur-buf (current-buffer))
	  proceed)
      (save-selected-window
	(select-window (event-window event))
	(set-buffer cur-buf)
	(setq proceed (wl-message-prev-page)))
      (when proceed
	(if (memq 'shift (event-modifiers event))
	    (wl-summary-up t)
	  (wl-summary-prev t))))))

(defun wl-draft-overload-menubar ()
  (when (featurep 'menubar)
    (add-menu-item '("Mail") "Send, Keep Editing"
		   'wl-draft-send t "Send Mail")
    (add-menu-item '("Mail") "Send Message"
		   'wl-draft-send-and-exit t "Send and Exit")
    (delete-menu-item '("Mail" "Send Mail"))
    (delete-menu-item '("Mail" "Send and Exit"))
    (add-menu-item '("Mail") "Preview Message"
		   'wl-draft-preview-message t "Cancel")
    (add-menu-item '("Mail") "Save Draft and Exit"
		   'wl-draft-save-and-exit t "Cancel")
    (add-menu-item '("Mail") "Kill Current Draft"
		   'wl-draft-kill t "Cancel")
    (delete-menu-item '("Mail" "Cancel"))))

(defun wl-draft-mode-setup ()
  (require 'derived)
  (define-derived-mode wl-draft-mode mail-mode "Draft"
    "draft mode for Wanderlust derived from mail mode.
See info under Wanderlust for full documentation.

Special commands:
\\{wl-draft-mode-map}"))

(defun wl-draft-key-setup ()
  (define-key wl-draft-mode-map "\C-c\C-y" 'wl-draft-yank-original)
  (define-key wl-draft-mode-map "\C-c\C-s" 'wl-draft-send)
  (define-key wl-draft-mode-map "\C-c\C-c" 'wl-draft-send-and-exit)
  (define-key wl-draft-mode-map "\C-c\C-z" 'wl-draft-save-and-exit)
  (define-key wl-draft-mode-map "\C-c\C-k" 'wl-draft-kill)
  (define-key wl-draft-mode-map "\C-l" 'wl-draft-highlight-and-recenter)
  (define-key wl-draft-mode-map "\C-i" 'wl-complete-field-body-or-tab)
  (define-key wl-draft-mode-map "\C-c\C-r" 'wl-draft-caesar-region)
  (define-key wl-draft-mode-map "\M-t" 'wl-toggle-plugged)
  (define-key wl-draft-mode-map "\C-c\C-o" 'wl-jump-to-draft-buffer)
  (define-key wl-draft-mode-map "\C-c\C-e" 'wl-draft-config-exec)
  (define-key wl-draft-mode-map "\C-c\C-j" 'wl-template-select)
  (define-key wl-draft-mode-map "\C-c\C-p" 'wl-draft-preview-message)
;;   (define-key wl-draft-mode-map "\C-x\C-s" 'wl-draft-save)
  (define-key wl-draft-mode-map "\C-c\C-a" 'wl-addrmgr)
  (define-key wl-draft-mode-map "\C-xk"    'wl-draft-mimic-kill-buffer)
  (define-key wl-draft-mode-map "\C-c\C-d" 'wl-draft-elide-region)
  (define-key wl-draft-mode-map "\C-a" 'wl-draft-beginning-of-line)
  (define-key wl-draft-mode-map "\M-p" 'wl-draft-previous-history-element)
  (define-key wl-draft-mode-map "\M-n" 'wl-draft-next-history-element))

(defun wl-draft-overload-functions ()
  (wl-mode-line-buffer-identification)
  ;; (local-set-key "\C-c\C-s" 'wl-draft-send);; override
  (wl-xmas-setup-draft-toolbar)
  (wl-draft-overload-menubar))

(defalias 'wl-defface 'defface)

(defun wl-read-event-char ()
  "Get the next event."
  (let ((event (next-command-event)))
    (sit-for 0)
    ;; We junk all non-key events.  Is this naughty?
    (while (not (or (key-press-event-p event)
		    (button-press-event-p event)))
      (dispatch-event event)
      (setq event (next-command-event)))
    (cons (and (key-press-event-p event)
	       (event-to-character event))
	  event)))

(defun wl-completing-read-multiple (prompt
				    table
				    &optional predicate
				    require-match initial-input)
  "Read multiple strings in the minibuffer"
  (split-string (completing-read prompt table predicate require-match
				 initial-input) ","))

(require 'product)
(product-provide (provide 'wl-xmas) (require 'wl-version))

;;; wl-xmas.el ends here
