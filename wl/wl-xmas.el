;;; wl-xmas.el -- Wanderlust modules for XEmacsen.

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

(eval-when-compile
  (require 'wl-folder)
  (require 'wl-summary)
  (require 'wl-draft)  
  (require 'wl-message)
  (require 'wl-highlight)
  (defvar-maybe wl-draft-mode-map (make-sparse-keymap)))

(defun wl-xmas-setup-toolbar (bar)
  (let ((dir wl-icon-dir)
	icon up down disabled name)
    (when dir
      (while bar
	(setq icon (aref (car bar) 0)
	      name (symbol-name icon)
	      bar (cdr bar))
	(when (not (boundp icon))
	  (setq up (concat dir elmo-path-sep name "-up.xpm"))
	  (setq down (concat dir elmo-path-sep name "-down.xpm"))
	  (setq disabled (concat dir elmo-path-sep name "-disabled.xpm"))
	  (if (not (file-exists-p up))
	      (setq bar nil
		    dir nil)
	    (set icon (toolbar-make-button-list
		       up (and (file-exists-p down) down)
		       (and (file-exists-p disabled) disabled)))))))
    dir))

(defvar wl-use-toolbar (if (featurep 'toolbar) 'default-toolbar nil))
(defvar wl-plugged-glyph nil)
(defvar wl-unplugged-glyph nil)

(defvar wl-folder-toolbar
  '([wl-folder-jump-to-current-entity
     wl-folder-jump-to-current-entity t "Enter Current Folder"]
    [wl-folder-next-entity
     wl-folder-next-entity t "Next Folder"]
    [wl-folder-prev-entity
     wl-folder-prev-entity t "Previous Folder"]
    [wl-folder-check-current-entity
     wl-folder-check-current-entity t "Check Current Folder"]
;    [wl-draft
;     wl-draft t "Write a New Message"]
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

(defalias 'wl-draft-insert-signature 'insert-signature) ;; for draft toolbar.

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

(defun wl-xmas-setup-folder-toolbar ()
  (and wl-use-toolbar
       (wl-xmas-setup-toolbar wl-folder-toolbar)
       (set-specifier (symbol-value wl-use-toolbar)
		      (cons (current-buffer) wl-folder-toolbar))))

(defun wl-xmas-setup-summary-toolbar ()
  (and wl-use-toolbar
       (wl-xmas-setup-toolbar wl-summary-toolbar)
       (set-specifier (symbol-value wl-use-toolbar)
		      (cons (current-buffer) wl-summary-toolbar))))

(defun wl-xmas-setup-message-toolbar ()
  (and wl-use-toolbar
       (wl-xmas-setup-toolbar wl-message-toolbar)
       (set-specifier (symbol-value wl-use-toolbar)
		      (cons (current-buffer) wl-message-toolbar))))

(defun wl-xmas-setup-draft-toolbar ()
  (and wl-use-toolbar
       (wl-xmas-setup-toolbar wl-draft-toolbar)
       (set-specifier (symbol-value wl-use-toolbar)
		      (cons (current-buffer) wl-draft-toolbar))))

;; XEmacs implementations.
(defun wl-highlight-folder-current-line (&optional numbers)
  (interactive)
  (save-excursion
    (let ((highlights (list "opened" "closed"))
	  (inhibit-read-only t)
	  (fld-name (wl-folder-get-folder-name-by-id
		     (get-text-property (point) 'wl-folder-entity-id)))
	  fregexp fsymbol bol eol matched type extent num type glyph)
      (setq eol (progn (end-of-line) (point))
	    bol (progn (beginning-of-line) (point)))
      (when (and fld-name (looking-at "[ \t]+\\([^ \t]+\\)"))
	(if (and (setq extent (extent-at (match-beginning 1) nil nil nil 'at))
		 (extent-begin-glyph extent))
	    (delete-extent extent))
	(setq extent (make-extent (match-beginning 1) (match-beginning 1)))
	(cond
	 ((string= fld-name wl-trash-folder) ;; set trash folder icon
	  (setq num (nth 2 numbers)) ;; number of messages
	  (set-extent-begin-glyph extent
				  (if (or (null num) 
					  (eq num 0))
				      wl-folder-trash-empty-glyph
				    wl-folder-trash-glyph)))
	 ((string= fld-name wl-draft-folder) ;; set draft folder icon
	  (set-extent-begin-glyph extent wl-folder-draft-glyph))
	 ((string= fld-name wl-queue-folder)
	  (set-extent-begin-glyph extent wl-folder-queue-glyph))
	 ((and (setq type (elmo-folder-get-type fld-name))
	       (or numbers ;; XXX dirty...!!
		   (not (assoc fld-name wl-folder-group-alist))))
	  ;; not group folder.
	  (set-extent-begin-glyph extent 
				  (symbol-value
				   (intern (format "wl-folder-%s-glyph"
						   type)))))))
      (when (and numbers (nth 0 numbers) (nth 1 numbers))
	(setq fsymbol 
	      (let ((unsync (nth 0 numbers))
		    (unread (nth 1 numbers)))
		(cond ((and unsync (eq unsync 0))
		       (if (and unread (> unread 0))
			   'wl-highlight-folder-unread-face
			 'wl-highlight-folder-zero-face))
		      ((and unsync 
			    (>= unsync wl-folder-many-unsync-threshold))
		       'wl-highlight-folder-many-face)
		      (t
		       'wl-highlight-folder-few-face))))
	(put-text-property bol eol 'face nil)
	(put-text-property bol eol 'face fsymbol)
	(setq matched t))
      (while highlights
	(setq fregexp (symbol-value 
		       (intern (format "wl-highlight-folder-%s-regexp" 
				       (car highlights)))))
	(if (not wl-highlight-group-folder-by-numbers)
	    (setq fsymbol (intern (format "wl-highlight-folder-%s-face"
					  (car highlights)))))
	(when (looking-at fregexp)
	  (setq extent (make-extent (match-beginning 1) (match-end 1))
		glyph (intern (format "wl-folder-%s-glyph"
				      (car highlights))))
	  (if (null (symbol-value glyph))
	      (set glyph (wl-xmas-make-icon-glyph
			  (extent-string extent)
			  (symbol-value
			   (cdr (assq glyph wl-folder-toggle-icon-list))))))
	  (setq glyph (symbol-value glyph))
	  (set-extent-property extent 'end-open t)
	  (set-extent-property extent 'start-closed t)
	  (set-extent-property extent 'invisible t)
	  (set-extent-end-glyph extent glyph)
	  (put-text-property bol eol 'face nil)
	  (put-text-property bol eol 'face fsymbol)
	  (setq matched t highlights nil))
	(setq highlights (cdr highlights)))
      (when (not matched)
	(put-text-property bol eol 'face nil)
	(if (looking-at (format "^[ ]*\\(%s\\|%s\\)"
				wl-folder-unsubscribe-mark
				wl-folder-removed-mark))
	    (put-text-property bol eol 'face
			       'wl-highlight-folder-killed-face)
	  (put-text-property bol eol 'face
			     'wl-highlight-folder-unknown-face)))
      (if wl-use-highlight-mouse-line 
	  (wl-highlight-folder-mouse-line))
      (if (and (featurep 'dragdrop) wl-use-dnd)
	  (wl-dnd-set-drop-target bol eol)))))

(defun wl-highlight-plugged-current-line ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  extent switch)
      (beginning-of-line)
      (when (looking-at "[ \t]*\\(\\[\\([^]]+\\)\\]\\)")
	(setq switch (elmo-match-buffer 2))
	(if (and (setq extent (extent-at (match-end 1) nil nil nil 'at))
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
	(put-text-property 0 len 'begin-glyph wl-folder-queue-glyph string)
      (if (setq type (elmo-folder-get-type folder))
	  (put-text-property 0 len
			     'begin-glyph
			     (symbol-value
			      (intern (format "wl-folder-%s-glyph" type)))
			     string)))
    string))

(defvar wl-folder-internal-icon-list
  ;; alist of (glyph . icon-file)
  '((wl-folder-nntp-glyph         . wl-nntp-folder-icon)
    (wl-folder-imap4-glyph        . wl-imap-folder-icon)
    (wl-folder-pop3-glyph         . wl-pop-folder-icon)
    (wl-folder-localdir-glyph     . wl-localdir-folder-icon)
    (wl-folder-localnews-glyph    . wl-localnews-folder-icon)
    (wl-folder-internal-glyph     . wl-internal-folder-icon)
    (wl-folder-multi-glyph        . wl-multi-folder-icon)
    (wl-folder-filter-glyph       . wl-filter-folder-icon)
    (wl-folder-archive-glyph      . wl-archive-folder-icon)
    (wl-folder-pipe-glyph         . wl-pipe-folder-icon)
    (wl-folder-maildir-glyph      . wl-maildir-folder-icon)
    (wl-folder-trash-empty-glyph  . wl-empty-trash-folder-icon)
    (wl-folder-draft-glyph        . wl-draft-folder-icon)
    (wl-folder-queue-glyph        . wl-queue-folder-icon)
    (wl-folder-trash-glyph        . wl-trash-folder-icon)))

(defvar wl-folder-toggle-icon-list
  '((wl-folder-opened-glyph       . wl-opened-group-folder-icon)
    (wl-folder-closed-glyph       . wl-closed-group-folder-icon)))

(defun wl-xmas-make-icon-glyph (icon-string icon-file &optional locale tag-set)
  (let ((glyph (make-glyph (vector 'string :data icon-string))))
    (if wl-highlight-folder-with-icon
	(set-glyph-image glyph
			 (vector 'xpm :file (expand-file-name
					     icon-file wl-icon-dir))
			 locale tag-set 'prepend))
    glyph))

(defun wl-folder-init-icons ()  
  (mapcar
   (lambda (x)
     (if (null (symbol-value (car x)))
	 (set (car x) (wl-xmas-make-icon-glyph "" (symbol-value (cdr x))))))
   wl-folder-internal-icon-list))

(defun wl-plugged-init-icons ()
  (unless wl-plugged-glyph
    (setq wl-plugged-glyph
	  (wl-xmas-make-icon-glyph
	   (concat "[" wl-plugged-plug-on "]")
	   wl-plugged-icon))
    (let ((extent (make-extent nil nil))
	  (toggle-keymap (make-sparse-keymap)))
      (define-key toggle-keymap 'button2
	(make-modeline-command-wrapper 'wl-toggle-plugged))
      (set-extent-keymap extent toggle-keymap)
      (set-extent-property extent 'help-echo "button2 toggles plugged status")
      (setq wl-plug-state-indicator-on
	    (cons extent wl-plugged-glyph))))
  (unless wl-unplugged-glyph
    (setq wl-unplugged-glyph
	  (wl-xmas-make-icon-glyph
	   (concat "[" wl-plugged-plug-off "]")
	   wl-unplugged-icon))
    (let ((extent (make-extent nil nil))
	  (toggle-keymap (make-sparse-keymap)))
      (define-key toggle-keymap 'button2
	(make-modeline-command-wrapper 'wl-toggle-plugged))
      (set-extent-keymap extent toggle-keymap)
      (set-extent-property extent 'help-echo "button2 toggles plugged status")
      (setq wl-plug-state-indicator-off
	    (cons extent wl-unplugged-glyph)))))

(defun wl-make-date-string ()
  (let ((s (current-time-string)))
    (string-match "\\`\\([A-Z][a-z][a-z]\\) +[A-Z][a-z][a-z] +[0-9][0-9]? *[0-9][0-9]?:[0-9][0-9]:[0-9][0-9] *[0-9]?[0-9]?[0-9][0-9]"
		  s)
    (concat (wl-match-string 1 s) ", "
	    (timezone-make-date-arpa-standard s (current-time-zone)))))


(defun wl-xmas-setup-folder ()
  (and (featurep 'scrollbar)
       (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (wl-xmas-setup-folder-toolbar))

(defun wl-xmas-setup-summary ()
  (make-local-variable 'dragdrop-drop-functions)
  (setq dragdrop-drop-functions '((wl-dnd-default-drop-message t t)))
  (and (featurep 'scrollbar)
       (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (wl-xmas-setup-summary-toolbar))

(defun wl-message-overload-functions ()
  (wl-xmas-setup-message-toolbar)
  (local-set-key "l" 'wl-message-toggle-disp-summary)
  (local-set-key 'button2 'wl-message-refer-article-or-url)
  (local-set-key 'button4 'wl-message-wheel-down)
  (local-set-key 'button5 'wl-message-wheel-up)
  (local-set-key [(shift button4)] 'wl-message-wheel-down)
  (local-set-key [(shift button5)] 'wl-message-wheel-up)
  (set-keymap-parent wl-message-button-map (current-local-map))
  (define-key wl-message-button-map 'button2
    'wl-message-button-dispatcher))

(defun wl-message-wheel-up (event)
  (interactive "e")
  (let ((cur-buf (current-buffer))
	proceed)
    (save-selected-window
      (select-window (event-window event))
      (set-buffer cur-buf)
      (setq proceed (wl-message-next-page)))
    (if proceed
	(if (memq 'shift (event-modifiers event))
	    (wl-summary-down t)
	  (wl-summary-next t)))))

(defun wl-message-wheel-down (event)
  (interactive "e")
  (let ((cur-buf (current-buffer))
	proceed)
    (save-selected-window
      (select-window (event-window event))
      (set-buffer cur-buf)
      (setq proceed (wl-message-prev-page)))
    (if proceed
	(if (memq 'shift (event-modifiers event))
	    (wl-summary-up t)
	  (wl-summary-prev t)))))

(defun wl-draft-overload-menubar ()
  (when (featurep 'menubar)
    (add-menu-item '("Mail") "Send, Keep Editing" 
		   'wl-draft-send t "Send Mail")
    (add-menu-item '("Mail") "Send Message" 
		   'wl-draft-send-and-exit t "Send and Exit")
    (delete-menu-item '("Mail" "Send Mail"))
    (delete-menu-item '("Mail" "Send and Exit"))))

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
  (local-set-key "\C-c\C-s" 'wl-draft-send) ; override
  (wl-xmas-setup-draft-toolbar)
  (wl-draft-overload-menubar))

(defalias 'wl-defface 'defface)

(provide 'wl-xmas)

;;; wl-xmas.el ends here
