;;; wl-demo.el --- Opening demo on Wanderlust

;; Copyright (C) 1998,1999,2000,2001 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000,2001 Katsumi Yamaoka <yamaoka@jpl.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defconst wl-demo-copyright-notice
  "Copyright (C) 1998-2004 Yuuichi Teranishi <teranisi@gohome.org>"
  "A declaration of the copyright on Wanderlust.")

(eval-when-compile
  (require 'cl))
(require 'path-util)
(require 'wl-vars)
(require 'wl-version)
(require 'wl-highlight)

(defun wl-demo-icon-name ()
  "A function to determine logo file name."
  (catch 'found
    (dolist (pair wl-demo-icon-name-alist)
      (when (eval (car pair))
	(throw 'found (eval (cdr pair)))))))

(defvar wl-logo-ascii "\
        o$                  oo$$$$$$ooo
     oo$$$      o$$      o$$$\"\"\"\"\"\"$$$$$o
  $$$$$$\"     o$$$\"    o$\"\"          \"$$$
    $$\"      o$\"\"    o$\"              $$$
   $\"      oo$\"     $\"                $$$
 o$     oo\"\"$$     $                  $$
o$$  oo$\"  \"$$$o  $                 o$$
$$$$\"\"       \"$$oo$    o          o$\"
               \"$$o   \"$$$o oooo$\"\"
                 $$       \"\"\"\"
	       Wanderlust
                  \"$
Yet Another Message Interface On Emacsen"
  "Ascii picture used to splash the startup screen.")

(eval-and-compile
  (when wl-on-emacs21
    ;; `display-images-p' has not been available in Emacs versions
    ;; prior to Emacs 21.0.105.
    (defalias-maybe 'display-images-p 'display-graphic-p)))

;; Avoid byte compile warnings.
(eval-when-compile
  (autoload 'bitmap-insert-xbm-file "bitmap" nil t)
  (autoload 'create-image "image")
  (autoload 'device-on-window-system-p "device")
  (autoload 'image-type-available-p "image")
  (autoload 'insert-image "image")
  (autoload 'make-glyph "glyphs")
  (autoload 'set-glyph-face "glyphs")
  (autoload 'set-specifier "specifier")
  (defalias-maybe 'frame-char-height 'ignore)
  (defalias-maybe 'frame-char-width 'ignore)
  (defalias-maybe 'glyph-height 'ignore)
  (defalias-maybe 'glyph-width 'ignore)
  (defalias-maybe 'image-size 'ignore)
  (defalias-maybe 'make-extent 'ignore)
  (defalias-maybe 'propertize 'ignore)
  (defalias-maybe 'set-extent-end-glyph 'ignore)
  (defalias-maybe 'window-pixel-height 'ignore)
  (defalias-maybe 'window-pixel-width 'ignore))

(defvar wl-demo-bitmap-mule-available-p 'unknown
  "Internal variable to say whether the BITMAP-MULE package is available.")

(defun wl-demo-image-type-alist ()
  "Return an alist of available logo image types on the current frame."
  (if (or (and (featurep 'xemacs)
	       (device-on-window-system-p))
	  window-system)
      (let ((xpm
	     (when (or (and (featurep 'xemacs)
			    (featurep 'xpm))
		       (and wl-on-emacs21
			    (display-images-p)
			    (image-type-available-p 'xpm)))
	       '("xpm" . xpm)))
	    (xbm
	     (when (or (featurep 'xemacs)
		       (and wl-on-emacs21
			    (display-images-p)
			    (image-type-available-p 'xbm))
		       (eq t wl-demo-bitmap-mule-available-p)
		       (and (eq 'unknown wl-demo-bitmap-mule-available-p)
			    (module-installed-p 'bitmap)
			    (setq wl-demo-bitmap-mule-available-p t)))
	       '("xbm" . xbm)))
	    (bitmap
	     (when (and (not (featurep 'xemacs))
			(or (eq t wl-demo-bitmap-mule-available-p)
			    (and (eq 'unknown wl-demo-bitmap-mule-available-p)
				 (module-installed-p 'bitmap)
				 (setq wl-demo-bitmap-mule-available-p t))))
	       '("bitmap" . bitmap))))
	(if (and wl-on-emacs21
		 (image-type-available-p 'xbm))
	    ;; Prefer xbm rather than bitmap on Emacs 21.
	    (delq nil (list xpm xbm bitmap '("ascii")))
	  (delq nil (list xpm bitmap xbm '("ascii")))))
    '(("ascii"))))

(defun wl-demo-xpm-set-background ()
  "A filter function to set xpm background.
`wl-demo-background-color' is used for the background color."
  (when (search-forward "None" nil t)
    (replace-match wl-demo-background-color)))

(defun wl-demo-image-filter (file type)
  "Get filtered image data.
FILE is the image file name.
TYPE is the filter function."
  (let ((filter (catch 'found
		  (dolist (pair wl-demo-image-filter-alist)
		    (when (eq (car pair) type)
		      (throw 'found (cdr pair)))))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents file)
      (goto-char (point-min))
      (when filter
	(funcall filter))
      (buffer-string))))

(defun wl-demo-insert-image (image-type)
  "Insert a logo image at the point and position it to be centered.
IMAGE-TYPE specifies what a type of image should be displayed.
Return a number of lines that an image occupies in the buffer."
  (let ((file (cond ((eq 'xpm image-type)
		     (concat (wl-demo-icon-name) ".xpm"))
		    ((eq 'bitmap image-type)
		     (concat (wl-demo-icon-name) ".img"))
		    ((eq 'xbm image-type)
		     (concat (wl-demo-icon-name) ".xbm"))))
	image width height)
    (when (featurep 'xemacs)
      (when (boundp 'default-gutter-visible-p)
	(set-specifier (symbol-value 'default-gutter-visible-p)
		       nil (current-buffer)))
      (when (featurep 'scrollbar)
	(set-specifier (symbol-value 'scrollbar-height) 0 (current-buffer))
	(set-specifier (symbol-value 'scrollbar-width) 0 (current-buffer))))
    (if (and file
	     (if (and wl-icon-directory
		      (file-directory-p wl-icon-directory))
		 (setq file (expand-file-name file wl-icon-directory))
	       (message "You have to specify the value of `wl-icon-directory'")
	       nil)
	     (if (file-exists-p file)
		 (if (file-readable-p file)
		     t
		   (message "Permission denied: %s" file)
		   nil)
	       (message "File not found: %s" file)
	       nil))
	(progn
	  (cond ((featurep 'xemacs)
		 (setq width (window-pixel-width)
		       height (window-pixel-height)
		       image (make-glyph (vector image-type ':data
						 (wl-demo-image-filter
						  file image-type))))
		 (when (eq 'xbm image-type)
		   (set-glyph-face image 'wl-highlight-logo-face))
		 (insert-char ?\  (max 0 (/ (+ (* (- width (glyph-width image))
						  (window-width)) width)
					    (* 2 width))))
		 (set-extent-end-glyph (make-extent (point) (point)) image)
		 (insert "\n")
		 (/ (+ (* 2 (glyph-height image) (window-height)) height)
		    (* 2 height)))
		((and wl-on-emacs21
		      (or (eq 'xpm image-type)
			  (and (eq 'xbm image-type)
			       (image-type-available-p 'xbm))))
		 ;; Use the new redisplay engine on Emacs 21.
		 (setq image (create-image (wl-demo-image-filter file
								 image-type)
					   image-type t)
		       width (image-size image)
		       height (cdr width)
		       width (car width))
		 (when (eq 'xbm image-type)
		   (let ((bg (face-background 'wl-highlight-logo-face))
			 (fg (face-foreground 'wl-highlight-logo-face)))
		     (when (stringp bg)
		       (plist-put (cdr image) ':background bg))
		     (when (stringp fg)
		       (plist-put (cdr image) ':foreground fg))))
		 (insert (propertize " " 'display
				     (list 'space ':align-to
					   (max 0 (round (- (window-width)
							    width)
							 2)))))
		 (insert-image image)
		 (insert "\n")
		 (round height))
		((eq 'bitmap image-type)
		 ;; Use ready-composed bitmap image.
		 (require 'bitmap)
		 (let ((coding-system-for-read 'iso-2022-7bit)
		       (input-coding-system '*iso-2022-jp*))
		   (insert-file-contents file))
		 (goto-char (point-max))
		 (unless (bolp)
		   (insert "\n"))
		 (setq width 0)
		 (while (progn
			  (end-of-line 0)
			  (not (bobp)))
		   (setq width (max width (current-column))))
		 ;; Emacs 21.1 would fail to decode composite chars
		 ;; if it has been built without fixing coding.c.
		 (when (and wl-on-emacs21
			    (>= width 80))
		   (erase-buffer)
		   (let ((coding-system-for-read 'raw-text))
		     (insert-file-contents file))
		   (goto-char (point-max))
		   (unless (bolp)
		     (insert "\n"))
		   (setq width 0)
		   (while (progn
			    (end-of-line 0)
			    (not (bobp)))
		     ;; Decode bitmap data line by line.
		     (decode-coding-region (line-beginning-position)
					   (point)
					   'iso-2022-7bit)
		     (setq width (max width (current-column)))))
		 (indent-rigidly (point-min) (point-max)
				 (max 0 (/ (1+ (- (window-width) width)) 2)))
		 (put-text-property (point-min) (point-max)
				    'face 'wl-highlight-logo-face)
		 (count-lines (point-min) (goto-char (point-max))))
		((eq 'xbm image-type)
		 (message "Composing a bitmap image...")
		 (require 'bitmap)
		 (bitmap-insert-xbm-file file)
		 (backward-char)
		 (indent-rigidly (point-min) (point-max)
				 (max 0 (/ (1+ (- (window-width)
						  (current-column)))
					   2)))
		 (put-text-property (point-min) (point-max)
				    'face 'wl-highlight-logo-face)
		 (message "Composing a bitmap image...done")
		 (count-lines (point-min) (goto-char (point-max))))))
      (insert wl-logo-ascii)
      (put-text-property (point-min) (point) 'face 'wl-highlight-logo-face)
      (unless (bolp)
	(insert "\n"))
      (setq width 0)
      (while (progn
	       (end-of-line 0)
	       (not (bobp)))
	(setq width (max width (current-column))))
      (indent-rigidly (point-min) (point-max)
		      (max 0 (/ (1+ (- (window-width) width)) 2)))
      (count-lines (point-min) (goto-char (point-max))))))

(defun wl-demo-set-background-color ()
  "Set background color of the demo buffer."
  (cond
   (wl-on-emacs21
    ;; I think there should be a better way to set face background in the
    ;; buffer local way. But I don't know how to do it on Emacs21.
    (goto-char (point-max))
    (dotimes (i (- (window-height)
		   (count-lines (point-min) (point)) 1)) ; 1 means modeline
      (insert ?\n))
    (put-text-property (point-min) (point-max)
		       'face 'wl-highlight-demo-face)
    (set-face-background 'wl-highlight-demo-face
			 wl-demo-background-color))
   ((featurep 'xemacs)
    (set-face-background 'default wl-demo-background-color
			 (current-buffer)))))

(defun wl-demo-insert-text (height)
  "Insert a version and the copyright message after a logo image.  HEIGHT
should be a number of lines that an image occupies in the buffer."
  (let* ((height (- (window-height) height 1))
	 (text (format (cond ((<= height 2)
			      "version %s - \"%s\"\n%s")
			     ((eq height 3)
			      "version %s - \"%s\"\n\n%s")
			     (t
			      "\nversion %s - \"%s\"\n\n%s"))
		       (product-version-string (product-find 'wl-version))
		       (product-code-name (product-find 'wl-version))
		       wl-demo-copyright-notice))
	 start)
    (goto-char (point-min))
    (insert-char ?\n (max 0 (/ (- height 4) 2)))
    (setq start (goto-char (point-max)))
    (if wl-on-emacs21
	(let ((bg (face-background 'wl-highlight-demo-face))
	      (fg (face-foreground 'wl-highlight-demo-face)))
	  (insert (propertize text
			      'face (nconc '(variable-pitch :slant oblique)
					   (when (stringp bg)
					     (list ':background bg))
					   (when (stringp fg)
					     (list ':foreground fg))))))
      (insert text)
      (put-text-property start (point) 'face 'wl-highlight-demo-face))
    (let ((fill-column (window-width)))
      (center-region start (point)))))

(defun wl-demo (&optional image-type)
  "Demo on the startup screen.  IMAGE-TYPE should be a symbol which
overrides the variable `wl-demo-display-logo'.  It will prompt user
for the type of image when it is called interactively with a prefix
argument."
  (interactive "P")
  (let ((selection (wl-demo-image-type-alist))
	type)
    (if (and image-type (interactive-p))
	(setq type (completing-read "Image type: " selection nil t)
	      image-type (when (assoc type selection)
			   (cdr (assoc type selection))))
      (if (setq type (assoc (format "%s" (or image-type wl-demo-display-logo))
			    selection))
	  (setq image-type (cdr type))
	(setq image-type (when wl-demo-display-logo
			   (cdr (car selection)))))))
  (let ((buffer (let ((default-enable-multibyte-characters t)
		      (default-mc-flag t)
		      (default-line-spacing 0))
		  (get-buffer-create "*WL Demo*"))))
    (switch-to-buffer buffer)
    (setq buffer-read-only nil)
    (buffer-disable-undo)
    (erase-buffer)
    (setq truncate-lines t
	  tab-width 8)
    (set (make-local-variable 'tab-stop-list)
	 '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
    (wl-demo-insert-text (wl-demo-insert-image image-type))
    (when wl-demo-background-color
      (wl-demo-set-background-color))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (sit-for (if (featurep 'lisp-float-type)
		 (/ (float 5) (float 10))
	       1))
    buffer))

(require 'product)
(product-provide (provide 'wl-demo) (require 'wl-version))

;;; wl-demo.el ends here
