;;; wl-mule.el -- Wanderlust modules for Mule compatible Emacsen.
;;                (Mule2.3@19.28, Mule2.3@19.34, Emacs 20.x)

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
  (require 'wl-vars)
  (defvar-maybe wl-draft-mode-map (make-sparse-keymap)))

(defun wl-draft-mode-setup ()
  (require 'derived)
  (define-derived-mode wl-draft-mode mail-mode "Draft"
    "draft mode for Wanderlust derived from mail mode.
See info under Wanderlust for full documentation.

Special commands:
\\{wl-draft-mode-map}"))

;; Common implementations.
(defun wl-highlight-folder-current-line (&optional numbers)
  "Highlight current folder line."
  (interactive)
  (save-excursion
    (let ((highlights (list "opened" "closed"))
	  (inhibit-read-only t)
	  (fld-name (wl-folder-get-folder-name-by-id
		     (get-text-property (point) 'wl-folder-entity-id)))
	  fregexp fsymbol bol eol matched type extent num type)
      (beginning-of-line)
      (setq bol (point))
      (save-excursion (end-of-line) (setq eol (point)))
      (if (and numbers (nth 0 numbers) (nth 1 numbers))
	  (progn
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
	    (put-text-property bol eol 'face fsymbol)
	    (setq matched t)))
      (catch 'highlighted
	(while highlights
	  (setq fregexp (symbol-value
			 (intern (format "wl-highlight-folder-%s-regexp"
					 (car highlights)))))
	  (if (not wl-highlight-group-folder-by-numbers)
	      (setq fsymbol (intern (format "wl-highlight-folder-%s-face"
					    (car highlights)))))
	  (when (looking-at fregexp)
	    (put-text-property bol eol 'face fsymbol)
	    (setq matched t)
	    (throw 'highlighted nil))
	  (setq highlights (cdr highlights))))
      (if (not matched)
	  (if (looking-at (format "^[ ]*\\(%s\\|%s\\)"
				  wl-folder-unsubscribe-mark
				  wl-folder-removed-mark))
	      (put-text-property bol eol 'face
				 'wl-highlight-folder-killed-face)
	    (put-text-property bol eol 'face
			       'wl-highlight-folder-unknown-face)))
      (if wl-use-highlight-mouse-line
	  (wl-highlight-folder-mouse-line)))))
  
(defun wl-highlight-plugged-current-line ())
(defun wl-plugged-set-folder-icon (folder string)
  string)

(defun wl-folder-init-icons ()) ; dummy.
(defun wl-plugged-init-icons ()) ; dummy.

(defun wl-xmas-setup-folder ()) ; dummy
(defun wl-xmas-setup-summary ())
(defun wl-xmas-setup-draft-toolbar ())

(defun wl-message-overload-functions ()
  (local-set-key "l" 'wl-message-toggle-disp-summary)
  (local-set-key [mouse-2] 'wl-message-refer-article-or-url)
  (local-set-key [mouse-4] 'wl-message-wheel-down)
  (local-set-key [mouse-5] 'wl-message-wheel-up)
  (local-set-key [S-mouse-4] 'wl-message-wheel-down)
  (local-set-key [S-mouse-5] 'wl-message-wheel-up))

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

(defun wl-draft-key-setup ()
  (define-key wl-draft-mode-map "\C-c\C-y" 'wl-draft-yank-original)
  (define-key wl-draft-mode-map "\C-c\C-a" 'wl-draft-insert-x-face-field)
  (define-key wl-draft-mode-map "\C-c\C-s" 'wl-draft-send)
  (define-key wl-draft-mode-map "\C-c\C-c" 'wl-draft-send-and-exit)
  (define-key wl-draft-mode-map "\C-c\C-z" 'wl-draft-save-and-exit)
  (define-key wl-draft-mode-map "\C-c\C-k" 'wl-draft-kill)
  (define-key wl-draft-mode-map "\C-l"     'wl-draft-highlight-and-recenter)
  (define-key wl-draft-mode-map "\C-i"     'wl-complete-field-body-or-tab)
  (define-key wl-draft-mode-map "\C-c\C-r" 'wl-draft-caesar-region)
  (define-key wl-draft-mode-map "\M-t"     'wl-toggle-plugged)
  (define-key wl-draft-mode-map "\C-c\C-o" 'wl-jump-to-draft-buffer)
  (define-key wl-draft-mode-map "\C-c\C-e" 'wl-draft-config-exec)
  (define-key wl-draft-mode-map "\C-c\C-j" 'wl-template-select)
  (define-key wl-draft-mode-map "\C-c\C-p" 'wl-draft-preview-message)
  (define-key wl-draft-mode-map "\C-x\C-s" 'wl-draft-save)
  (define-key wl-draft-mode-map "\C-xk" 'wl-draft-mimic-kill-buffer))

(defun wl-draft-overload-menubar ()
  (local-set-key [menu-bar mail send]
    '("Send Message" . wl-draft-send-and-exit))
  (local-set-key [menu-bar mail send-stay]
    '("Send, Keep Editing" . wl-draft-send))
  (local-set-key [menu-bar mail cancel]
    '("Kill Current Draft" . wl-draft-kill))
  (local-set-key [menu-bar mail yank]
    '("Cite Message" . wl-draft-yank-original))
  (local-set-key [menu-bar mail signature]
    '("Insert Signature" . insert-signature))
  (local-set-key [menu-bar headers fcc]
    '("FCC" . wl-draft-fcc)))

(defun wl-draft-overload-functions ()
  (setq mode-line-buffer-identification
	(format "Wanderlust: %s" (buffer-name)))
  (local-set-key "\C-c\C-s" 'wl-draft-send)    ; override
  (wl-draft-overload-menubar)
  (when wl-show-plug-status-on-modeline
    (setq mode-line-format (wl-make-modeline))))

(defalias 'wl-make-modeline 'wl-make-modeline-subr)

;; for "ja-mule-canna-2.3.mini" on PocketBSD
(defun-maybe make-face (a))

(eval-when-compile
  (require 'static))
(static-cond
 ((and (fboundp 'defface)
       (not (featurep 'tinycustom)))
  (defalias 'wl-defface 'defface)
  (eval-when-compile
    (defun wl-face-spec-set-match-display (display frame))
    (defun wl-frame-parameter (frame property &optional default))
    (defun wl-get-frame-properties (&optional frame))))
 (t
  (defmacro wl-defface (face spec doc &rest args)
    (nconc (list 'wl-declare-face (list 'quote face) spec)))

  (defun wl-declare-face (face spec)
    (make-face face)
    (while spec
      (let* ((entry (car spec))
	     (display (nth 0 entry))
	     (atts (nth 1 entry)))
	(setq spec (cdr spec))
	(when (wl-face-spec-set-match-display display nil)
	  (apply 'wl-face-attributes-set face nil atts)))))

  (defconst wl-face-attributes
    '((:bold set-face-bold-p)
      (:italic set-face-italic-p)
      (:underline set-face-underline-p)
      (:foreground set-face-foreground)
      (:background set-face-background)
      (:stipple set-face-stipple)))

  (defun wl-face-attributes-set (face frame &rest atts)
    "For FACE on FRAME set the attributes [KEYWORD VALUE]....
Each keyword should be listed in `custom-face-attributes'.

If FRAME is nil, set the default face."
    (while atts
      (let* ((name (nth 0 atts))
	     (value (nth 1 atts))
	     (fun (nth 1 (assq name wl-face-attributes))))
	(setq atts (cdr (cdr atts)))
	(condition-case nil
	    (funcall fun face value frame)
	  (error nil)))))

  (defun wl-frame-parameter (frame property &optional default)
    "Return FRAME's value for property PROPERTY."
    (or (cdr (assq property (frame-parameters frame)))
	default))

  (eval-when-compile
    (defun-maybe x-display-grayscale-p ()))

  (defun wl-get-frame-properties (&optional frame)
    "Return a plist with the frame properties of FRAME used by custom."
    (list (cons 'type window-system)
	  (cons 'class (or (wl-frame-parameter frame 'display-type)
			   (when window-system
			     (cond ((x-display-color-p)
				    'color)
				   ((and (fboundp 'x-display-grayscale-p)
					 (x-display-grayscale-p))
				    'grayscale)
				   (t 'mono)))))
	  (cons 'background (or (wl-frame-parameter frame 'background-mode)
				wl-highlight-background-mode))))

  (defun wl-face-spec-set-match-display (display frame)
    "Non-nil iff DISPLAY matches FRAME.
If FRAME is nil, the current FRAME is used."
    ;; This is a kludge to get started, we really should use specifiers!
    (if (eq display t)
	t
      (let* ((props (wl-get-frame-properties frame))
	     (type (cdr (assq 'type props)))
	     (class (cdr (assq 'class props)))
	     (background (cdr (assq 'background props)))
	     (match t)
	     (entries display)
	     entry req options)
	(while (and entries match)
	  (setq entry (car entries)
		entries (cdr entries)
		req (car entry)
		options (cdr entry)
		match (cond ((eq req 'type)
			     (memq type options))
			    ((eq req 'class)
			     (memq class options))
			    ((eq req 'background)
			     (memq background options))
			    (t
			     (message (format "\
Warning: Unknown req `%S' with options `%S'" req options))
			     nil))))
	match)))))

(provide 'wl-mule)

;;; wl-mule.el ends here
