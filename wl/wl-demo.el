;;; wl-demo.el --- Opening demo on Wanderlust  -*- lexical-binding: t -*-

;; Copyright (C) 1998,1999,2000,2001,2002,2003,2004
;;      Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000,2001,2004
;;      Katsumi Yamaoka <yamaoka@jpl.org>

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
  "Copyright (C) 1998-2012 Yuuichi Teranishi <teranisi@gohome.org>"
  "A declaration of the copyright on Wanderlust.")

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
  (defalias-maybe 'display-images-p 'display-graphic-p))

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
  (defalias-maybe 'face-background-name 'ignore)
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
  (if window-system
      (let ((xpm
	     (when (and (display-images-p)
			(image-type-available-p 'xpm))
	       '("xpm" . xpm)))
	    (xbm
	     (when (or (and (display-images-p)
			    (image-type-available-p 'xbm))
		       (eq t wl-demo-bitmap-mule-available-p)
		       (and (eq 'unknown wl-demo-bitmap-mule-available-p)
			    (module-installed-p 'bitmap)
			    (setq wl-demo-bitmap-mule-available-p t)))
	       '("xbm" . xbm)))
	    (bitmap
	     (when (or (eq t wl-demo-bitmap-mule-available-p)
		       (and (eq 'unknown wl-demo-bitmap-mule-available-p)
			    (module-installed-p 'bitmap)
			    (setq wl-demo-bitmap-mule-available-p t)))
	       '("bitmap" . bitmap))))
	(if (image-type-available-p 'xbm)
	    ;; Prefer xbm rather than bitmap on Emacs 21.
	    (delq nil (list xpm xbm bitmap '("ascii")))
	  (delq nil (list xpm bitmap xbm '("ascii")))))
    '(("ascii"))))

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

(defun wl-demo-insert-image (itype)
  "Insert a logo image at the point and position it to be centered.
ITYPE specifies what a type of image should be displayed.
Return a number of lines that an image occupies in the buffer."
  (let ((file (cond ((eq 'xpm itype)
		     (concat (wl-demo-icon-name) ".xpm"))
		    ((eq 'bitmap itype)
		     (concat (wl-demo-icon-name) ".img"))
		    ((eq 'xbm itype)
		     (concat (wl-demo-icon-name) ".xbm"))))
	image width height)

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
	  (cond ((or (eq 'xpm itype)
		     (and (eq 'xbm itype)
			  (image-type-available-p 'xbm)))
		 ;; Use the new redisplay engine on Emacs 21.
		 (setq image (create-image (wl-demo-image-filter file
								 itype)
					   itype t)
		       width (image-size image)
		       height (cdr width)
		       width (car width))
		 (when (eq 'xbm itype)
		   (let ((bg (face-background 'wl-highlight-demo-face))
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
		((eq 'bitmap itype)
		 ;; Use ready-composed bitmap image.
		 (require 'bitmap)
		 (let ((coding-system-for-read 'iso-2022-7bit))
		   (insert-file-contents file))
		 (goto-char (point-max))
		 (unless (bolp)
		   (insert "\n"))

		 ;; Emacs 21.x may fail on computing the end of the
		 ;; column if there're bitmap characters.
;;;		 (setq width 0)
;;;		 (while (progn
;;;			  (end-of-line 0)
;;;			  (not (bobp)))
;;;		   (setq width (max width (current-column))))
		 (setq width 1024)
		 (while (progn
			  (end-of-line 0)
			  (not (bobp)))
		   (setq width (min width (current-column))))

		 ;; Emacs 21.1 would fail to decode composite chars
		 ;; if it has been built without fixing coding.c.
		 (when (>= width 80)
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
		 (put-text-property (point-min) (point-max) 'fixed-width t)
		 (count-lines (point-min) (goto-char (point-max))))
		((eq 'xbm itype)
		 (message "Composing a bitmap image...")
		 (require 'bitmap)
		 (bitmap-insert-xbm-file file)
		 (backward-char)
		 (indent-rigidly (point-min) (point-max)
				 (max 0 (/ (1+ (- (window-width)
						  (current-column)))
					   2)))
		 (put-text-property (point-min) (point-max) 'fixed-width t)
		 (message "Composing a bitmap image...done")
		 (count-lines (point-min) (goto-char (point-max))))))
      (insert wl-logo-ascii)
      (unless (bolp)
	(insert "\n"))
      (setq width 0)
      (while (progn
	       (end-of-line 0)
	       (not (bobp)))
	(setq width (max width (current-column))))
      (indent-rigidly (point-min) (point-max)
		      (max 0 (/ (1+ (- (window-width) width)) 2)))
      (put-text-property (point-min) (point-max) 'fixed-width t)
      (count-lines (point-min) (goto-char (point-max))))))

(defun wl-demo-setup-properties ()
  "Set up properties of the demo buffer."
  ;; I think there should be a better way to set face background
  ;; for the buffer only. But I don't know how to do it on Emacs21.
  (goto-char (point-max))
  (dotimes (_i (- (window-height)
		  (count-lines (point-min) (point))))
    (insert ?\n))
  (let* ((fg (face-foreground 'wl-highlight-demo-face))
	 (bg (face-background 'wl-highlight-demo-face))
	 (oblique (nconc '(variable-pitch :slant oblique)
			 (when (stringp bg)
			   (list ':background bg))
			 (when (stringp fg)
			   (list ':foreground fg))))
	 (start (text-property-any (point-min) (point-max) 'fixed-width t))
	 end)
    (if start
	(progn
	  (put-text-property (point-min) start 'face oblique)
	  (setq end (or (text-property-not-all start (point-max)
					       'fixed-width t)
			(point-max)))
	  (put-text-property start end 'face
			     (nconc '(wl-highlight-logo-face)
				    (when (stringp bg)
				      (list ':background bg))))
	  (put-text-property end (point-max) 'face oblique))
      (put-text-property (point-min) (point-max) 'face oblique))))

(defun wl-demo-insert-text (height)
  "Insert a version and the copyright message after a logo image.  HEIGHT
should be a number of lines that an image occupies in the buffer."
  (let* ((height (- (window-height) height 1))
	 (text (format (cond ((<= height 2)
			      "version %s - \"%s\"\n%s\n")
			     ((eq height 3)
			      "version %s - \"%s\"\n\n%s\n")
			     (t
			      "\nversion %s - \"%s\"\n\n%s\n"))
		       (product-version-string (product-find 'wl-version))
		       (product-code-name (product-find 'wl-version))
		       wl-demo-copyright-notice))
	 start)
    (goto-char (point-min))
    (insert-char ?\n (max 0 (/ (- height 4) 2)))
    (setq start (goto-char (point-max)))
    (insert text)
    (let ((fill-column (window-width)))
      (center-region start (point)))))

(defun wl-demo (&optional itype)
  "Demo on the startup screen.  ITYPE should be a symbol which
overrides the variable `wl-demo-display-logo'.  It will prompt user
for the type of image when it is called interactively with a prefix
argument."
  (interactive "P")
  (let ((selection (wl-demo-image-type-alist))
	type)
    (if (and itype (called-interactively-p 'interactive))
	(setq type (completing-read "Image type: " selection nil t)
	      itype (cdr (assoc type selection)))
      (if (setq type (assoc (format "%s" (or itype wl-demo-display-logo))
			    selection))
	  (setq itype (cdr type))
	(setq itype (when wl-demo-display-logo
			   (cdr (car selection)))))))
  (let ((buffer (let ((line-spacing 0))
		  (get-buffer-create "*WL Demo*"))))
    (switch-to-buffer buffer)
    (setq buffer-read-only nil)
    (buffer-disable-undo)
    (erase-buffer)
    (setq truncate-lines t
	  tab-width 8)
    (set (make-local-variable 'tab-stop-list)
	 '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
    (wl-demo-insert-text (wl-demo-insert-image itype))
    (wl-demo-setup-properties)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (sit-for (if (featurep 'lisp-float-type)
		 (/ (float 5) (float 10))
	       1))
    buffer))

;; Prune functions provided temporarily to avoid compile warnings.
(eval-when-compile
  (dolist (fn '(face-background-name
		frame-char-height frame-char-width glyph-height glyph-width
		image-size make-extent propertize set-extent-end-glyph
		window-pixel-height window-pixel-width))
    (when (and (get fn 'defalias-maybe)
	       (eq (symbol-function fn) 'ignore))
      (put fn 'defalias-maybe nil)
      (fmakunbound fn))))

(require 'product)
(product-provide (provide 'wl-demo) (require 'wl-version))

;;; wl-demo.el ends here
