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

;; Using BITMAP-MULE to compose a logo image will take a long time.
;; It will be used if you are using Mule, Emacs 20 or Emacs 21 without
;; a new redisplay engine.  If it makes you irritated, you can inhibit
;; the use of BITMAP-MULE on the startup screen by the following line
;; in your .wl file:
;;
;; (setq wl-demo-display-logo nil)

;;; Code:

(defconst wl-demo-copyright-notice
  "Copyright (C) 1998-2001 Yuuichi Teranishi <teranisi@gohome.org>"
  "A declaration of the copyright on Wanderlust.")

(eval-when-compile
  (require 'cl))
(require 'wl-vars)
(require 'wl-version)
(require 'wl-highlight)

(defconst wl-demo-icon-name
  (concat "wl-" (wl-version-status)
	  (if (string-match "^... Dec \\([ 1][0-9]\\|2[0-4]\\)"
			    (current-time-string))
	      "-xmas-logo"
	    "-logo"))
  "Basename of the logo file.")

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
  (defalias-maybe 'bitmap-insert-xbm-file 'ignore)
  (defalias-maybe 'create-image 'ignore)
  (defalias-maybe 'device-on-window-system-p 'ignore)
  (defalias-maybe 'frame-char-height 'ignore)
  (defalias-maybe 'frame-char-width 'ignore)
  (defalias-maybe 'glyph-height 'ignore)
  (defalias-maybe 'glyph-width 'ignore)
  (defalias-maybe 'image-size 'ignore)
  (defalias-maybe 'image-type-available-p 'ignore)
  (defalias-maybe 'insert-image 'ignore)
  (defalias-maybe 'make-extent 'ignore)
  (defalias-maybe 'make-glyph 'ignore)
  (defalias-maybe 'propertize 'ignore)
  (defalias-maybe 'set-extent-end-glyph 'ignore)
  (defalias-maybe 'set-glyph-face 'ignore)
  (defalias-maybe 'set-specifier 'ignore)
  (defalias-maybe 'window-pixel-height 'ignore)
  (defalias-maybe 'window-pixel-width 'ignore))

(defun wl-demo-insert-image (image-type)
  "Insert a logo image at the point and position it to be centered.
IMAGE-TYPE specifies what a type of image should be displayed.
Return a number of lines that an image occupies in the buffer."
  (let ((file (cond ((eq image-type 'xpm)
		     (concat wl-demo-icon-name ".xpm"))
		    ((memq image-type '(xbm bitmap))
		     (concat wl-demo-icon-name ".xbm"))))
	image width height)
    (when (featurep 'xemacs)
      (when (boundp 'default-gutter-visible-p)
	(set-specifier (symbol-value 'default-gutter-visible-p)
		       nil (current-buffer)))
      (set-specifier (symbol-value 'scrollbar-height) 0 (current-buffer))
      (set-specifier (symbol-value 'scrollbar-width) 0 (current-buffer)))
    (if (and file
	     (if (and wl-icon-dir
		      (file-directory-p wl-icon-dir))
		 (setq file (expand-file-name file wl-icon-dir))
	       (message "You should specify the value of `wl-icon-dir'")
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
		       image (make-glyph (vector image-type ':file file)))
		 (when (eq 'xbm image-type)
		   (set-glyph-face image 'wl-highlight-logo-face))
		 (insert-char ?\  (max 0 (/ (+ (* (- width (glyph-width image))
						  (window-width)) width)
					    (* 2 width))))
		 (set-extent-end-glyph (make-extent (point) (point)) image)
		 (insert "\n")
		 (/ (+ (* 2 (glyph-height image) (window-height)) height)
		    (* 2 height)))
		((eq 'bitmap image-type)
		 (message "Composing a bitmap image...")
		 (save-restriction
		   (narrow-to-region (point) (point))
		   (bitmap-insert-xbm-file file)
		   (backward-char)
		   (indent-rigidly (point-min) (point-max)
				   (max 0 (/ (1+ (- (window-width)
						    (current-column)))
					     2)))
		   (put-text-property (point-min) (point-max)
				      'face 'wl-highlight-logo-face)
		   (message "Composing a bitmap image...done")
		   (count-lines (point-min) (goto-char (point-max)))))
		((>= emacs-major-version 21)
		 (if (eq 'xpm image-type)
		     (setq image (create-image file 'xpm)
			   width (image-size image)
			   height (cdr width)
			   width (car width))
		   (with-temp-buffer
		     (setq case-fold-search t)
		     (insert-file-contents file)
		     (goto-char (point-min))
		     (re-search-forward "\
^#define[[:blank:]]+[^[:blank:]]+_width[[:blank:]]+")
		     (setq width (read (current-buffer)))
		     (goto-char (point-min))
		     (re-search-forward "\
^#define[[:blank:]]+[^[:blank:]]+_height[[:blank:]]+")
		     (setq height (read (current-buffer)))
		     (search-forward "{")
		     (delete-region (point-min) (point))
		     (while (re-search-forward "[^0-9a-fx]+" nil t)
		       (replace-match ""))
		     (goto-char (point-min))
		     (insert "\"")
		     (while (search-forward "0x" nil t)
		       (replace-match "\\\\x"))
		     (goto-char (point-max))
		     (insert "\"")
		     (goto-char (point-min))
		     (setq image (create-image (read (current-buffer))
					       'xbm t
					       ':width width
					       ':height height)
			   width (/ (float width) (frame-char-width))
			   height (/ (float height) (frame-char-height)))
		     (let ((bg (face-background 'wl-highlight-logo-face))
			   (fg (face-foreground 'wl-highlight-logo-face)))
		       (when (stringp bg)
			 (plist-put (cdr image) ':background bg))
		       (when (stringp fg)
			 (plist-put (cdr image) ':foreground fg)))))
		 (insert (propertize " " 'display
				     (list 'space ':align-to
					   (max 0 (round (- (window-width)
							    width)
							 2)))))
		 (insert-image image)
		 (insert "\n")
		 (round height))))
      (save-restriction
	(narrow-to-region (point) (point))
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
	(count-lines (point-min) (goto-char (point-max)))))))

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
					   (if (stringp bg)
					       (list ':background bg))
					   (if (stringp fg)
					       (list ':foreground fg))))))
      (insert text)
      (put-text-property start (point) 'face 'wl-highlight-demo-face))
    (let ((fill-column (window-width)))
      (center-region start (point)))))

(defun wl-demo-image-type-alist ()
  "Return an alist of available logo image types on the current frame."
  (if (or (and (featurep 'xemacs)
	       (device-on-window-system-p))
	  window-system)
      (let ((selection (append (when (or (and (featurep 'xemacs)
					      (featurep 'xpm))
					 (and wl-on-emacs21
					      (display-images-p)
					      (image-type-available-p 'xpm)))
				 '(("xpm" . xpm)))
			       (when (or (featurep 'xemacs)
					 (and wl-on-emacs21
					      (display-images-p)
					      (image-type-available-p 'xbm)))
				 '(("xbm" . xbm))))))
	(unless (or selection
		    (featurep 'xemacs))
	  (condition-case nil
	      (require 'bitmap)
	    (error)))
	(when (featurep 'bitmap)
	  (setq selection (append selection '(("bitmap" . bitmap)))))
	(append selection '(("ascii"))))
    '(("ascii"))))

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
  (when (eq 'bitmap image-type)
    ;; Composing a bitmap image takes a long time. :-<
    (wl-demo 'ascii))
  (let ((buffer (let ((default-enable-multibyte-characters t)
		      (default-mc-flag t)
		      (default-line-spacing 0))
		  (get-buffer-create "*WL Demo*"))))
    (switch-to-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq truncate-lines t
	  tab-width 8)
    (set (make-local-variable 'tab-stop-list)
	 '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
    (wl-demo-insert-text (wl-demo-insert-image image-type))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (sit-for (if (featurep 'lisp-float-type)
		 (/ (float 5) (float 10))
	       1))
    buffer))

(require 'product)
(product-provide (provide 'wl-demo) (require 'wl-version))

;;; wl-demo.el ends here
