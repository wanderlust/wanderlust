;;; wl-e21.el --- Wanderlust modules for Emacs 21.  -*- lexical-binding: t -*-

;; Copyright (C) 2000,2001 Katsumi Yamaoka <yamaoka@jpl.org>
;; Copyright (C) 2000,2001 Yuuichi Teranishi <teranisi@gohome.org>

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
;; This module uses `before-string' overlay property to show icon
;; images instead of `insert-image', so don't delete such overlays
;; sloppily.  Here is a sample code to show icons in the buffer.
;;
;;(let (image icon from to overlay)
;;  ;; The function `find-image' will look for an image first on `load-path'
;;  ;; and then in `data-directory'.
;;  (let ((load-path (cons wl-icon-directory load-path)))
;;    (setq image (find-image (list (list :type 'xpm :file wl-nntp-folder-icon
;;					:ascent 'center)))))
;;  ;; `propertize' is a convenient function in such a case.
;;  ;; String must have one or more length to wear an image.
;;  (setq icon (propertize "any string" 'display image))
;;  (pop-to-buffer (get-buffer-create "*wl-e21-demo*"))
;;  (erase-buffer)
;;  (insert "   ")
;;  (setq from (point))
;;  (insert "-fj.wanderlust:0/0/0")
;;  (setq to (point))
;;  (insert "\n")
;;  (setq overlay (make-overlay from to))
;;  ;; Put an image.
;;  (overlay-put overlay 'before-string icon)
;;  ;; Put a mark to indicate that this overlay is made by `wl-e21'.
;;  ;; It is not always necessarily.
;;  (overlay-put overlay 'wl-e21-icon t)
;;  ;; Make it to be removable.
;;  (overlay-put overlay 'evaporate t))
;;
;; Note that a port of Emacs to some platforms (e.g. MS-Windoze) does
;; not yet support images.  It is a pity that neither icons nor tool-
;; bars will not be displayed in such systems.

;;; Code:
;;

(require 'elmo)
(require 'wl-vars)

(add-hook 'wl-init-hook 'wl-biff-init-icons)
(add-hook 'wl-init-hook 'wl-plugged-init-icons)

(defvar wl-use-toolbar (image-type-available-p 'xpm))
(defvar wl-plugged-image nil)
(defvar wl-unplugged-image nil)
(defvar wl-biff-mail-image nil)
(defvar wl-biff-nomail-image nil)

(eval-and-compile
  (defmacro wl-e21-display-image-p ()
    '(and wl-highlight-folder-with-icon
	  (image-type-available-p 'xpm))))

(eval-and-compile
  (defun wl-e21-find-image (specs)
    (let ((image-load-path (cons 'wl-icon-directory image-load-path)))
      (find-image specs))))

(defun wl-e21-setup-toolbar (bar)
  (when (and wl-use-toolbar
	     (wl-e21-display-image-p))
    (let ((props '(:type xpm :ascent center
			 :color-symbols (("backgroundToolBarColor" . "None"))
			 :file))
	  (success t)
	  icon up down disabled name)
      (while bar
	(setq icon (aref (pop bar) 0))
	(unless (boundp icon)
	  (setq name (symbol-name icon)
		up (wl-e21-find-image `((,@props ,(concat name "-up.xpm")))))
	  (if up
	      (progn
		(setq down (wl-e21-find-image
			    `((,@props ,(concat name "-down.xpm"))))
		      disabled (wl-e21-find-image
				`((,@props ,(concat name "-disabled.xpm")))))
		(if (and down disabled)
		    (set icon (vector down up disabled disabled))
		  (set icon up)))
	    (setq bar nil
		  success nil))))
      success)))

(defvar wl-e21-toolbar-configurations
  '((auto-resize-tool-bars       . t)
    (auto-raise-tool-bar-buttons . t)
    (tool-bar-button-margin      . 2)
    (tool-bar-button-relief      . 1)))

(defun wl-e21-make-toolbar-buttons (keymap defs)
  (let ((configs wl-e21-toolbar-configurations)
	config)
    (while (setq config (pop configs))
      (set (make-local-variable (car config)) (cdr config))))
  ;; Invalidate the default bindings.
  (let ((keys (cdr (key-binding [tool-bar] t)))
	item)
    (unless (eq (caar keys) 'keymap) ;; Emacs >= 24
      (while (setq item (pop keys))
	(when (setq item (car-safe item))
	  (define-key keymap (vector 'tool-bar item) 'undefined)))))
  (let ((n (length defs))
	def)
    (while (>= n 0)
      (setq n (1- n)
	    def (nth n defs))
      (define-key keymap (vector 'tool-bar (aref def 1))
	(list 'menu-item (aref def 3) (aref def 1)
	      :enable (aref def 2)
	      :image (symbol-value (aref def 0)))))))

(defun wl-highlight-plugged-current-line ()
  (interactive)
  (when (wl-e21-display-image-p)
    (save-excursion
      (beginning-of-line)
      (when (looking-at "[[:blank:]]*\\(\\[\\([^]]+\\)\\]\\)")
	(let* ((start (match-beginning 1))
	       (end (match-end 1))
	       (status (match-string-no-properties 2))
	       (image (if (string-equal wl-plugged-plug-on status)
			  wl-plugged-image
			wl-unplugged-image)))
	  (when image
	    (let (overlay)
	      (let ((overlays (overlays-in start end)))
		(while (and (setq overlay (pop overlays))
			    (not (overlay-get overlay 'wl-e21-icon)))))
	      (unless overlay
		(setq overlay (make-overlay start end))
		(overlay-put overlay 'wl-e21-icon t)
		(overlay-put overlay 'evaporate t))
	      (overlay-put overlay 'display image))))))))

(defun wl-plugged-set-folder-icon (folder string)
  (let (image type)
    (when (wl-e21-display-image-p)
      (setq image
	    (cond ((string= folder wl-queue-folder)
		   (get 'wl-folder-queue-image 'image))
		  ((setq type (or (elmo-folder-type folder)
				  (elmo-folder-type-internal
				   (elmo-make-folder folder))))
		   (get (intern (format "wl-folder-%s-image" type))
			'image)))))
    (if image
	(concat (propertize " " 'display image 'invisible t) string)
      string)))

(declare-function wl-toggle-plugged "wl"
		  (&optional arg queue-flush-only))

(defun wl-plugged-init-icons ()
  (let ((props (when (display-mouse-p)
		 (list 'local-map (purecopy (make-mode-line-mouse-map
					     'mouse-2 #'wl-toggle-plugged))
		       'help-echo "mouse-2 toggles plugged status"))))
    (if (wl-e21-display-image-p)
	(progn
	  (unless wl-plugged-image
	    (setq wl-plugged-image (wl-e21-find-image
				    `((:type xpm
					     :file ,wl-plugged-icon
					     :ascent center)))
		  wl-unplugged-image (wl-e21-find-image
				      `((:type xpm
					       :file ,wl-unplugged-icon
					       :ascent center)))))
	  (setq wl-modeline-plug-state-on
		(apply 'propertize wl-plug-state-indicator-on
		       `(display ,wl-plugged-image ,@props))
		wl-modeline-plug-state-off
		(apply 'propertize wl-plug-state-indicator-off
		       `(display ,wl-unplugged-image ,@props))))
      (if props
	  (setq wl-modeline-plug-state-on
		(apply 'propertize wl-plug-state-indicator-on props)
		wl-modeline-plug-state-off
		(apply 'propertize wl-plug-state-indicator-off props))
	(setq wl-modeline-plug-state-on wl-plug-state-indicator-on
	      wl-modeline-plug-state-off wl-plug-state-indicator-off)))))

(declare-function wl-biff-check-folders "wl-util" ())

(defun wl-biff-init-icons ()
  (let ((props (when (display-mouse-p)
		 (list 'local-map (purecopy (make-mode-line-mouse-map
					     'mouse-2 #'wl-biff-check-folders))
		       'help-echo "mouse-2 checks new mails"))))
    (if (wl-e21-display-image-p)
	(progn
	  (unless wl-biff-mail-image
	    (setq wl-biff-mail-image (wl-e21-find-image
				      `((:type xpm
					       :file ,wl-biff-mail-icon
					       :ascent center)))
		  wl-biff-nomail-image (wl-e21-find-image
					`((:type xpm
						 :file ,wl-biff-nomail-icon
						 :ascent center)))))
	  (setq wl-modeline-biff-state-on
		(apply 'propertize wl-biff-state-indicator-on
		       `(display ,wl-biff-mail-image ,@props))
		wl-modeline-biff-state-off
		(apply 'propertize wl-biff-state-indicator-off
		       `(display ,wl-biff-nomail-image ,@props))))
      (if props
	  (setq wl-modeline-biff-state-on
		(apply 'propertize wl-biff-state-indicator-on props)
		wl-modeline-biff-state-off
		(apply 'propertize wl-biff-state-indicator-off props))
	(setq wl-modeline-biff-state-on wl-biff-state-indicator-on
	      wl-modeline-biff-state-off wl-biff-state-indicator-off)))))

(defalias 'wl-defface 'defface)
(make-obsolete 'wl-defface 'defface "26 May 2020")

(put 'wl-modeline-biff-state-on 'risky-local-variable t)
(put 'wl-modeline-biff-state-off 'risky-local-variable t)
(put 'wl-modeline-plug-state-on 'risky-local-variable t)
(put 'wl-modeline-plug-state-off 'risky-local-variable t)

(require 'product)
(product-provide (provide 'wl-e21) (require 'wl-version))

;;; wl-e21.el ends here
