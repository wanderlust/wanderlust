;;; wl-demo.el -- Opening demo on Wanderlust.

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

(defconst wl-demo-copyright-notice
  "Copyright (C) 1998-2001 Yuuichi Teranishi <teranisi@gohome.org>")

(require 'wl-vars)
(require 'wl-version)
(require 'wl-highlight)

(defconst wl-demo-icon-name (concat "wl-" (wl-version-status) "-logo"))

;; Avoid byte compile warnings.
(eval-when-compile
  (defalias-maybe 'bitmap-compose 'ignore)
  (defalias-maybe 'bitmap-decode-xbm 'ignore)
  (defalias-maybe 'bitmap-read-xbm-buffer 'ignore)
  (defalias-maybe 'bitmap-read-xbm-file 'ignore)
  (defalias-maybe 'create-image 'ignore)
  (defalias-maybe 'device-on-window-system-p 'ignore)
  (defalias-maybe 'display-graphic-p 'ignore)
  (defalias-maybe 'frame-char-height 'ignore)
  (defalias-maybe 'frame-char-width 'ignore)
  (defalias-maybe 'frame-parameter 'ignore)
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

;;
;; demo ;-)
;;

(defvar wl-logo-ascii "        o$                  oo$$$$$$ooo
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
Yet Another Message Interface On Emacsen")

(eval-when-compile
  (defmacro wl-demo-with-temp-file-buffer (file &rest forms)
    "Create a temporary buffer, insert FILE's contents without
any conversions and evaluate FORMS there like `progn'."
    ( `(with-temp-buffer
	 (let ((coding-system-for-read 'binary)
	       (input-coding-system '*noconv*)
	       auto-mode-alist
	       file-name-handler-alist
	       format-alist
	       insert-file-contents-access-hook
	       insert-file-contents-post-hook
	       insert-file-contents-pre-hook
	       interpreter-mode-alist)
	   (insert-file-contents (, file))
	   (,@ forms)))))
  (put 'wl-demo-with-temp-file-buffer 'lisp-indent-function 1))

(eval-when-compile
  (defmacro wl-logo-xpm ()
    ;; (WIDTH HEIGHT DATA)
    (let ((file (expand-file-name (concat wl-demo-icon-name ".xpm")
				  wl-icon-dir)))
      (if (file-exists-p file)
	  (wl-demo-with-temp-file-buffer file
	    (re-search-forward
	     (concat "\"[\t ]*\\([0-9]+\\)[\t ]+\\([0-9]+\\)"
		     "[\t ]+[0-9]+[\t ]+[0-9]+[\t ]*\""))
	    (list 'list
		  (string-to-number (match-string 1))
		  (string-to-number (match-string 2))
		  (buffer-string))))))
  (defmacro wl-logo-xbm ()
    ;; (WIDTH HEIGHT DATA)
    (let ((file (expand-file-name (concat wl-demo-icon-name ".xbm")
				  wl-icon-dir)))
      (if (file-exists-p file)
	  (wl-demo-with-temp-file-buffer file
	    (let ((case-fold-search t)
		  width height)
	      (search-forward "width")
	      (setq width (read (current-buffer)))
	      (goto-char (point-min))
	      (search-forward "height")
	      (setq height (read (current-buffer)))
	      (goto-char (point-min))
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
	      (list 'list width height (read (current-buffer))))))))
  (defmacro wl-logo-bitmap ()
    ;; (DECODED-P . DATA)
    (let ((file (expand-file-name (concat wl-demo-icon-name ".xbm")
				  wl-icon-dir)))
      (if (file-exists-p file)
	  (if (condition-case nil
		  (require 'bitmap)
		(error nil))
	      (list 'cons t (bitmap-decode-xbm (bitmap-read-xbm-file file)))
	    (wl-demo-with-temp-file-buffer file
	      (list 'cons nil (buffer-string))))))))

(let ((xpm (wl-logo-xpm)))
  (if (and xpm
	   (or (and (featurep 'xemacs)
		    (featurep 'xpm))
	       (condition-case nil
		   (require 'image)
		 (error nil))))
      (progn
	(put 'wl-logo-xpm 'width (car xpm))
	(put 'wl-logo-xpm 'height (nth 1 xpm))
	(put 'wl-logo-xpm 'image
	     (if (featurep 'xemacs)
		 (make-glyph (vector 'xpm ':data (nth 2 xpm)))
	       (condition-case nil
		   (let ((image-types '(xpm)))
		     (create-image (nth 2 xpm) 'xpm t))
		 (error
		  (put 'wl-logo-xpm 'width nil)
		  (put 'wl-logo-xpm 'height nil)
		  nil)))))))

(let (width height)
  (let ((xbm (wl-logo-xbm)))
    (setq width (car xbm)
	  height (nth 1 xbm))
    (if (and xbm
	     (or (featurep 'xemacs)
		 (condition-case nil
		     (require 'image)
		   (error nil))))
	(progn
	  (put 'wl-logo-xbm 'width width)
	  (put 'wl-logo-xbm 'height height)
	  (put 'wl-logo-xbm 'image
	       (if (featurep 'xemacs)
		   (make-glyph (vector 'xbm ':data xbm))
		 (condition-case nil
		     (let ((image-types '(xbm)))
		       (create-image (nth 2 xbm) 'xbm t
				     ':width (car xbm) ':height (nth 1 xbm)))
		   (error
		    (put 'wl-logo-xbm 'width nil)
		    (put 'wl-logo-xbm 'height nil)
		    nil)))))))
  (if (and width
	   (not (featurep 'xemacs))
	   (condition-case nil
	       (require 'bitmap)
	     (error nil)))
      (progn
	(put 'wl-logo-bitmap 'width width)
	(put 'wl-logo-bitmap 'height height)
	(let ((default-enable-multibyte-characters t)
	      (default-mc-flag t))
	  (with-temp-buffer
	    (let* ((bm (wl-logo-bitmap))
		   (cmp (if (car bm)
			    (cdr bm)
			  (insert (cdr bm))
			  (prog1
			      (bitmap-decode-xbm (bitmap-read-xbm-buffer
						  (current-buffer)))
			    (erase-buffer))))
		   (len (length cmp))
		   (i 1))
	      (insert (bitmap-compose (aref cmp 0)))
	      (while (< i len)
		(insert "\n" (bitmap-compose (aref cmp i)))
		(setq i (1+ i)))
	      (put 'wl-logo-bitmap 'image (buffer-string))))))))

(eval-when-compile
  (defmacro wl-demo-image-type-alist ()
    '(append (if (and (get 'wl-logo-xpm 'width)
		      (or (and (featurep 'xemacs)
			       (featurep 'xpm)
			       (device-on-window-system-p))
			  (and wl-on-emacs21
			       (display-graphic-p)
			       (image-type-available-p 'xpm))))
		 '(("xpm" . xpm)))
	     (if (and (get 'wl-logo-xbm 'width)
		      (or (and (featurep 'xemacs)
			       (device-on-window-system-p))
			  (and wl-on-emacs21
			       (display-graphic-p)
			       (image-type-available-p 'xbm))))
		 '(("xbm" . xbm)))
	     (if (and (get 'wl-logo-bitmap 'width)
		      (not (featurep 'xemacs))
		      window-system
		      (featurep 'bitmap))
		 '(("bitmap" . bitmap)))
	     '(("ascii")))))

(defun wl-demo (&optional image-type)
  "Demo on the startup screen.
Optional IMAGE-TYPE overrides the variable `wl-demo-display-logo'."
  (interactive "P")
  (let ((selection (wl-demo-image-type-alist))
	type)
    (if (and image-type (interactive-p))
	(setq type (completing-read "Image type: " selection nil t)
	      image-type (if (assoc type selection)
			     (cdr (assoc type selection))))
      (if (setq type (assoc (format "%s" (or image-type wl-demo-display-logo))
			    selection))
	  (setq image-type (cdr type))
	(setq image-type (cdr (car selection))))))
  (if image-type
      (setq image-type (intern (format "wl-logo-%s" image-type))))
  (let ((demo-buf (let ((default-enable-multibyte-characters t)
			(default-mc-flag t)
			(default-line-spacing 0))
		    (get-buffer-create "*WL Demo*"))))
    (switch-to-buffer demo-buf)
    (erase-buffer)
    (setq truncate-lines t
	  tab-width 8)
    (set (make-local-variable 'tab-stop-list)
	 '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
    (when (and (featurep 'xemacs)
	       (device-on-window-system-p))
      (if (boundp 'default-gutter-visible-p)
	  (set-specifier (symbol-value 'default-gutter-visible-p)
			 nil demo-buf))
      (set-specifier (symbol-value 'scrollbar-height) 0 demo-buf)
      (set-specifier (symbol-value 'scrollbar-width) 0 demo-buf))
    (let ((ww (window-width))
	  (wh (window-height))
	  rest)
      (if image-type
	  (let ((lw (get image-type 'width))
		(lh (get image-type 'height))
		(image (get image-type 'image)))
	    (cond
	     ((featurep 'xemacs)
	      (if (eq 'wl-logo-xbm image-type)
		  (set-glyph-face image 'wl-highlight-logo-face))
	      (setq rest (- wh 1 (/ (+ (* lh wh) (window-pixel-height) -1)
				    (window-pixel-height))))
	      (insert-char ?\  (max 0 (/ (- (* (window-pixel-width) (1+ ww))
					    (* lw ww))
					 2 (window-pixel-width))))
	      (set-extent-end-glyph (make-extent (point) (point)) image))
	     ((and wl-on-emacs21
		   (display-graphic-p)
		   (not (eq 'wl-logo-bitmap image-type)))
	      (if (eq 'wl-logo-xbm image-type)
		  (let ((bg (face-background 'wl-highlight-logo-face))
			(fg (face-foreground 'wl-highlight-logo-face)))
		    (if (stringp bg)
			(plist-put (cdr image) ':background bg))
		    (if (stringp fg)
			(plist-put (cdr image) ':foreground fg))))
	      (setq rest (/ (- (* wh (frame-char-height)) lh 1)
			    (frame-char-height)))
	      (insert (propertize " " 'display
				  (list 'space ':align-to
					(max 0 (/ (- (* (frame-char-width)
							(1+ ww)) lw)
						  2 (frame-char-width))))))
	      (insert-image image))
	     (t
	      (insert image)
	      (put-text-property (point-min) (point) 'face
				 'wl-highlight-logo-face)
	      (setq rest (/ (- (* 16 wh) lh 8) 16))
	      (indent-rigidly (point-min) (point-max)
			      (/ (- (* 8 (1+ ww)) lw) 16))))
	    (goto-char (point-min)))
	(insert (or wl-logo-ascii (product-name (product-find 'wl-version))))
	(put-text-property (point-min) (point) 'face 'wl-highlight-logo-face)
	(setq rest (- wh (count-lines (point-min) (point)) 1))
	(let ((lw (current-column))
	      (lh (count-lines (point-min) (point))))
	  (while (progn (beginning-of-line) (not (bobp)))
	    (backward-char)
	    (setq lw (max lw (current-column))))
	  (indent-rigidly (point) (point-max) (max 0 (/ (- ww lw) 2)))))
      (insert-char ?\n (max 0 (/ (- rest 4) 2)))
      (goto-char (point-max))
      (insert "\n")
      (let ((start (point))
	    (text (format (cond ((<= rest 2)
				 "version %s - \"%s\"\n%s")
				((eq rest 3)
				 "version %s - \"%s\"\n\n%s")
				(t
				 "\nversion %s - \"%s\"\n\n%s"))
			  (product-version-string (product-find 'wl-version))
			  (product-code-name (product-find 'wl-version))
			  wl-demo-copyright-notice)))
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
	(let ((fill-column ww))
	  (center-region start (point))))
      (goto-char (point-min))
      (sit-for (if (featurep 'lisp-float-type) (/ (float 5) (float 10)) 1))
      demo-buf)))

(require 'product)
(product-provide (provide 'wl-demo) (require 'wl-version))

;;; wl-demo.el ends here
