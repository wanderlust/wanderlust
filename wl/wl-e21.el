;;; wl-e21.el -- Wanderlust modules for Emacs 21.

;; Copyright 2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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
  (defvar-maybe wl-draft-mode-map (make-sparse-keymap)))

(defvar wl-use-toolbar (and (display-graphic-p)
			    (image-type-available-p 'xpm)))
(defvar wl-plugged-image nil)
(defvar wl-unplugged-image nil)

(defvar wl-folder-toolbar
  '([wl-folder-jump-to-current-entity
     wl-folder-jump-to-current-entity t "Enter Current Folder"]
    [wl-folder-next-entity
     wl-folder-next-entity t "Next Folder"]
    [wl-folder-prev-entity
     wl-folder-prev-entity t "Previous Folder"]
    [wl-folder-check-current-entity
     wl-folder-check-current-entity t "Check Current Folder"]
    ;;[wl-draft
    ;; wl-draft t "Write a New Message"]
    [wl-folder-sync-current-entity
     wl-folder-sync-current-entity t "Sync Current Folder"]
    [wl-draft
     wl-draft t "Write a New Message"]
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
    [wl-summary-delete
     wl-summary-delete t "Delete Current Message"]
    [wl-summary-mark-as-important
     wl-summary-mark-as-important t "Mark Current Message as Important"]
    [wl-draft
     wl-draft t "Write a New Message"]
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
    )
  "The Draft buffer toolbar.")

(defun wl-e21-setup-toolbar (bar)
  (let ((load-path (cons wl-icon-dir load-path))
	(success t)
	icon up down disabled name success)
    (while bar
      (setq icon (aref (car bar) 0)
	    bar (cdr bar))
      (unless (boundp icon)
	(setq name (symbol-name icon)
	      up (find-image `((:type xpm :file ,(concat name "-up.xpm")
				      :ascent center)
			       (:type xbm :file ,(concat name "-up.xbm")
				      :ascent center))))
	(if up
	    (progn
	      (setq down (find-image
			  `((:type xpm :file ,(concat name "-down.xpm")
				   :ascent center)
			    (:type xbm :file ,(concat name "-down.xbm")
				   :ascent center)))
		    disabled (find-image
			      `((:type xpm :file ,(concat name "-disabled.xpm")
				       :ascent center)
				(:type xbm :file ,(concat name "-disabled.xbm")
				       :ascent center))))
	      (set icon (vector down up disabled disabled)))
	  (setq bar nil
		success nil))))
    success))

(defvar wl-e21-toolbar-configurations
  '((auto-resize-tool-bar        . t)
    (auto-raise-tool-bar-buttons . t)
    (tool-bar-button-margin      . 0)
    (tool-bar-button-relief      . 2)))

(defun wl-e21-make-toolbar-buttons (keymap defs)
  (let ((configs wl-e21-toolbar-configurations)
	config)
    (while (setq config (pop configs))
      (set (make-local-variable (car config)) (cdr config))))
  (modify-frame-parameters (selected-frame) '((tool-bar-lines . 1)))
  (let ((n (1- (length defs)))
	def)
    (while (>= n 0)
      (setq def (nth n defs)
	    n (1- n))
      (define-key keymap (vector 'tool-bar (aref def 1))
	(list 'menu-item (aref def 3) (aref def 1)
	      :enable (aref def 2)
	      :image (symbol-value (aref def 0)))))))

(defun wl-e21-setup-folder-toolbar ()
  (and wl-use-toolbar
       (wl-e21-setup-toolbar wl-folder-toolbar)
       (wl-e21-make-toolbar-buttons wl-folder-mode-map wl-folder-toolbar)))

(defun wl-e21-setup-summary-toolbar ()
  (and wl-use-toolbar
       (wl-e21-setup-toolbar wl-summary-toolbar)
       (wl-e21-make-toolbar-buttons wl-summary-mode-map wl-summary-toolbar)))

(defun wl-e21-setup-message-toolbar ()
  (and wl-use-toolbar
       (wl-e21-setup-toolbar wl-message-toolbar)
       (wl-e21-make-toolbar-buttons (current-local-map) wl-message-toolbar)))

(defun wl-e21-setup-draft-toolbar ()
  (and wl-use-toolbar
       (wl-e21-setup-toolbar wl-draft-toolbar)
       (wl-e21-make-toolbar-buttons wl-draft-mode-map wl-draft-toolbar)))

(defun wl-e21-insert-image (image &optional string)
  (unless string
    (setq string " "))
  (let* ((start (point))
	 (end (+ start (length string))))
    (if (stringp image)
	(progn
	  (insert string)
	  (let ((ovl (make-overlay start end)))
	    (overlay-put ovl 'before-string image)
	    (overlay-put ovl 'evaporate t)
	    (add-text-properties start end
				 '(invisible t intangible t
					     rear-nonsticky t))))
      (insert-image image string))
    (put-text-property start end 'wl-e21-icon t)))

(defun wl-e21-make-icon-image (icon-string icon-file)
  (if wl-highlight-folder-with-icon
      (let ((load-path (cons wl-icon-dir load-path)))
	(cond ((let (case-fold-search)
		 ;; It may be a default value.
		 (string-match "\\.xpm$" icon-file))
	       (find-image
		`((:type xpm :file ,icon-file :ascent center)
		  (:type xbm
			 :file ,(concat
				 (substring icon-file 0 (match-beginning 0))
				 ".xbm")
			 :ascent center))))
	      ((let ((case-fold-search t))
		 (string-match "\\.\\(x[bp]m\\|png\\|gif\\)$" icon-file))
	       (find-image
		`((:type ,(intern (downcase (match-string 1 icon-file)))
			 :file ,icon-file :ascent center))))))
    icon-string))

(eval-when-compile
  (defsubst wl-e21-highlight-folder-group-icon (image &optional string-face)
    (let ((string (match-string-no-properties 1))
	  (start (goto-char (match-beginning 1)))
	  (inhibit-read-only t))
      (delete-region start (match-end 1))
      (unless (get image 'image)
	(put image 'image (wl-e21-make-icon-image
			   string
			   (symbol-value
			    (cdr (assq image wl-folder-toggle-icon-list))))))
      (setq image (get image 'image))
      (wl-e21-insert-image image string)
      (when (stringp image)
	(put-text-property (line-beginning-position) (line-end-position)
			   'face string-face))
      (when wl-use-highlight-mouse-line
	(put-text-property start (line-end-position)
			   'mouse-face 'highlight)))))

(defun wl-highlight-folder-current-line (&optional numbers)
  (interactive)
  (save-excursion
    (beginning-of-line)
    ;; put an icon
    (let (fld-name)
      (cond
       (;; opened folder group
	(looking-at wl-highlight-folder-opened-regexp)
	(wl-e21-highlight-folder-group-icon 'wl-folder-opened-image
					    'wl-highlight-folder-opened-face))
       (;; closed folder group
	(looking-at wl-highlight-folder-closed-regexp)
	(wl-e21-highlight-folder-group-icon 'wl-folder-closed-image
					    'wl-highlight-folder-closed-face))
       (;; basic folder
	(and (setq fld-name (wl-folder-get-folder-name-by-id
			     (get-text-property (point) 'wl-folder-entity-id)))
	     (looking-at "[ \t]+\\([^ \t]+\\)"))
	(goto-char (1- (match-beginning 1)))
	(let ((inhibit-read-only t))
	  (if (get-text-property (point) 'wl-e21-icon)
	      (delete-char 1)
	    (forward-char 1))
	  (let ((start (point))
		type)
	    (wl-e21-insert-image
	     (cond
	      ((string= fld-name wl-trash-folder);; trash folder
	       (let ((num (nth 2 numbers)));; number of messages
		 (get (if (or (not num) (zerop num))
			  'wl-folder-trash-empty-image
			'wl-folder-trash-image)
		      'image)))
	      ((string= fld-name wl-draft-folder);; draft folder
	       (get 'wl-folder-draft-image 'image))
	      ((string= fld-name wl-queue-folder);; queue folder
	       (get 'wl-folder-queue-image 'image))
	      (;; and one of many other folders
	       (setq type (elmo-folder-get-type fld-name))
	       (get (intern (format "wl-folder-%s-image" type)) 'image))))
	    (when wl-use-highlight-mouse-line
	      (put-text-property start (line-end-position)
				 'mouse-face 'highlight)))))))
    (let ((inhibit-read-only t))
      (if (and numbers (nth 0 numbers) (nth 1 numbers))
	  (let ((unsync (nth 0 numbers))
		(unread (nth 1 numbers))
		(inhibit-read-only t))
	    (put-text-property
	     (line-beginning-position) (line-end-position)
	     'face
	     (cond ((and unsync (zerop unsync))
		    (if (and unread (zerop unread))
			'wl-highlight-folder-zero-face
		      'wl-highlight-folder-unread-face))
		   ((and unsync
			 (>= unsync wl-folder-many-unsync-threshold))
		    'wl-highlight-folder-many-face)
		   (t
		    'wl-highlight-folder-few-face))))
	(beginning-of-line)
	(put-text-property (point) (line-end-position) 'face
			   (if (looking-at (format "^[ ]*\\(%s\\|%s\\)"
						   wl-folder-unsubscribe-mark
						   wl-folder-removed-mark))
			       'wl-highlight-folder-killed-face
			     'wl-highlight-folder-unknown-face))))))

(defun wl-highlight-plugged-current-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "[ \t]*\\(\\[\\([^]]+\\)\\]\\)")
      (let ((inhibit-read-only t))
	(add-text-properties (match-beginning 1) (goto-char (match-end 1))
			     '(invisible t intangible t rear-nonsticky t))
	(when (get-text-property (point) 'wl-e21-icon)
	  (delete-char 1))
	(wl-e21-insert-image
	 (if (string= wl-plugged-plug-on (elmo-match-buffer 2))
	     wl-plugged-image
	   wl-unplugged-image))))))

(defun wl-plugged-set-folder-icon (folder string)
  (let ((istring (concat " " string))
	type)
    (cond ((string= folder wl-queue-folder)
	   (put-text-property 0 1 'display
			      (get 'wl-folder-queue-image 'image) istring)
	   istring)
	  ((setq type (elmo-folder-get-type folder))
	   (put-text-property 0 1 'display
			      (get (intern (format "wl-folder-%s-image" type))
				   'image)
			      istring)
	   istring)
	  (t
	   string))))

(defvar wl-folder-internal-icon-list
  ;; alist of (image . icon-file)
  '((wl-folder-nntp-image         . wl-nntp-folder-icon)
    (wl-folder-imap4-image        . wl-imap-folder-icon)
    (wl-folder-pop3-image         . wl-pop-folder-icon)
    (wl-folder-localdir-image     . wl-localdir-folder-icon)
    (wl-folder-localnews-image    . wl-localnews-folder-icon)
    (wl-folder-internal-image     . wl-internal-folder-icon)
    (wl-folder-multi-image        . wl-multi-folder-icon)
    (wl-folder-filter-image       . wl-filter-folder-icon)
    (wl-folder-archive-image      . wl-archive-folder-icon)
    (wl-folder-pipe-image         . wl-pipe-folder-icon)
    (wl-folder-maildir-image      . wl-maildir-folder-icon)
    (wl-folder-trash-empty-image  . wl-empty-trash-folder-icon)
    (wl-folder-draft-image        . wl-draft-folder-icon)
    (wl-folder-queue-image        . wl-queue-folder-icon)
    (wl-folder-trash-image        . wl-trash-folder-icon)))

(defvar wl-folder-toggle-icon-list
  '((wl-folder-opened-image       . wl-opened-group-folder-icon)
    (wl-folder-closed-image       . wl-closed-group-folder-icon)))

(defun wl-folder-init-icons ()
  (let ((load-path (cons wl-icon-dir load-path))
	(icons wl-folder-internal-icon-list)
	icon name case-fold-search)
    (while (setq icon (pop icons))
      (unless (get (car icon) 'image)
	(setq name (symbol-value (cdr icon)))
	(put (car icon) 'image
	     (cond ((let (case-fold-search)
		      ;; It may be a default value.
		      (string-match "\\.xpm$" name))
		    (find-image
		     `((:type xpm :file ,name :ascent center)
		       (:type xbm
			      :file ,(concat
				      (substring name 0 (match-beginning 0))
				      ".xbm")
			      :ascent center))))
		   ((let ((case-fold-search t))
		      (string-match "\\.\\(x[bp]m\\|png\\|gif\\)$" name))
		    (find-image
		     `((:type ,(intern (downcase (match-string 1 name)))
			      :file ,name :ascent center))))))))))

(defun wl-plugged-init-icons ()
  (unless wl-plugged-image
    (setq wl-plug-state-indicator-on (concat "[" wl-plugged-plug-on "]")
	  wl-plugged-image (wl-e21-make-icon-image
			    wl-plug-state-indicator-on
			    wl-plugged-icon)))
  (unless wl-unplugged-image
    (setq wl-plug-state-indicator-off (concat "[" wl-plugged-plug-off "]")
	  wl-unplugged-image (wl-e21-make-icon-image
			      wl-plug-state-indicator-off
			      wl-unplugged-icon)))
  (let ((props (list 'local-map (purecopy (make-mode-line-mouse2-map
					   #'wl-toggle-plugged))
		     'help-echo "mouse-2 toggles plugged status")))
    (add-text-properties 0 (length wl-plug-state-indicator-on)
			 (nconc props (unless (stringp wl-plugged-image)
					(list 'display wl-plugged-image)))
			 wl-plug-state-indicator-on)
    (add-text-properties 0 (length wl-plug-state-indicator-off)
			 (nconc props (unless (stringp wl-unplugged-image)
					(list 'display wl-unplugged-image)))
			 wl-plug-state-indicator-off)))

(defun wl-make-date-string ()
  (format-time-string "%a, %d %b %Y %T %z"))

(defalias 'wl-e21-setup-folder 'wl-e21-setup-folder-toolbar)

(defalias 'wl-e21-setup-summary 'wl-e21-setup-summary-toolbar)

(defun wl-message-overload-functions ()
  (wl-e21-setup-message-toolbar)
  (let ((keymap (current-local-map)))
    (define-key keymap "l" 'wl-message-toggle-disp-summary)
    (define-key keymap [mouse-2] 'wl-message-refer-article-or-url)
    (define-key keymap [mouse-4] 'wl-message-wheel-down)
    (define-key keymap [mouse-5] 'wl-message-wheel-up)
    (define-key keymap [S-mouse-4] 'wl-message-wheel-down)
    (define-key keymap [S-mouse-5] 'wl-message-wheel-up)
    (set-keymap-parent wl-message-button-map keymap))
  (define-key wl-message-button-map [mouse-2] 'wl-message-button-dispatcher))

(defun wl-message-wheel-up (event)
  (interactive "e")
  (if (string-match wl-message-buf-name (buffer-name))
      (wl-message-next-page)
    (let ((cur-buf (current-buffer))
	  proceed)
      (save-selected-window
	(select-window (posn-window (event-start event)))
	(set-buffer cur-buf)
	(setq proceed (wl-message-next-page)))
      (if proceed
	  (if (memq 'shift (event-modifiers event))
	      (wl-summary-down t)
	    (wl-summary-next t))))))

(defun wl-message-wheel-down (event)
  (interactive "e")
  (if (string-match wl-message-buf-name (buffer-name))
      (wl-message-prev-page)
    (let ((cur-buf (current-buffer))
	  proceed)
      (save-selected-window
	(select-window (posn-window (event-start event)))
	(set-buffer cur-buf)
	(setq proceed (wl-message-prev-page)))
      (if proceed
	  (if (memq 'shift (event-modifiers event))
	      (wl-summary-up t)
	    (wl-summary-prev t))))))

(defun wl-draft-overload-menubar ()
  (let ((keymap (current-local-map)))
    (define-key keymap [menu-bar mail send]
      '("Send Message" . wl-draft-send-and-exit))
    (define-key keymap [menu-bar mail send-stay]
      '("Send, Keep Editing" . wl-draft-send))
    (define-key keymap [menu-bar mail cancel]
      '("Kill Current Draft" . wl-draft-kill))
    (define-key keymap [menu-bar mail yank]
      '("Cite Message" . wl-draft-yank-original))
    (define-key keymap [menu-bar mail signature]
      '("Insert Signature" . insert-signature))
    (define-key keymap [menu-bar headers fcc]
      '("FCC" . wl-draft-fcc))))

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
  (define-key wl-draft-mode-map "\C-c\C-a" 'wl-draft-insert-x-face-field)
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
  (define-key wl-draft-mode-map "\C-x\C-s" 'wl-draft-save)
  (define-key wl-draft-mode-map "\C-xk"    'wl-draft-mimic-kill-buffer))

(defun wl-draft-overload-functions ()
  (setq mode-line-buffer-identification
	(wl-mode-line-buffer-identification
	 (if wl-show-plug-status-on-modeline
	     '("" wl-plug-state-indicator "Wanderlust: %12b")
	   '("Wanderlust: %12b"))))
  (local-set-key "\C-c\C-s" 'wl-draft-send);; override
  (wl-e21-setup-draft-toolbar)
  (wl-draft-overload-menubar))

(defalias 'wl-defface 'defface)

(provide 'wl-e21)

;;; wl-e21.el ends here
