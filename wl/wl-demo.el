;;; wl-demo.el -- Opening demo on Wanderlust.

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

(defconst wl-demo-copyright-notice
  "Copyright (C) 1998-2000 Yuuichi Teranishi <teranisi@gohome.org>")

(require 'wl-vars)
(require 'wl-highlight)
(provide 'wl-demo)

;; Avoid byte compile warnings.
(eval-when-compile
  (mapcar
   (function (lambda (fn) (or (fboundp fn) (fset fn 'ignore))))
   '(bitmap-compose
     bitmap-decode-xbm bitmap-read-xbm-buffer bitmap-read-xbm-file
     create-image device-on-window-system-p display-graphic-p
     frame-char-height frame-char-width image-type-available-p
     insert-image make-extent make-glyph set-extent-end-glyph
     set-glyph-face set-specifier window-pixel-height
     window-pixel-width)))

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
    (let ((file (expand-file-name "wl-logo.xpm" wl-icon-dir)))
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
    (let ((file (expand-file-name "wl-logo.xbm" wl-icon-dir)))
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
    (let ((file (expand-file-name "wl-logo.xbm" wl-icon-dir)))
      (if (file-exists-p file)
	  (if (condition-case nil (require 'bitmap) (error nil))
	      (list 'cons t (bitmap-decode-xbm
			     (bitmap-read-xbm-file file)))
	    (wl-demo-with-temp-file-buffer file
	      (list 'cons nil (buffer-string))))))))

(let ((xpm (wl-logo-xpm)))
  (if (and xpm (or (and (featurep 'xemacs)
			(featurep 'xpm))
		   (and (condition-case nil (require 'image) (error nil))
			(image-type-available-p 'xpm))))
      (progn
	(put 'wl-logo-xpm 'width (car xpm))
	(put 'wl-logo-xpm 'height (nth 1 xpm))
	(put 'wl-logo-xpm 'image
	     (if (featurep 'xemacs)
		 (make-glyph (vector 'xpm ':data (nth 2 xpm)))
	       (create-image (nth 2 xpm) 'xpm t))))))

(let ((xbm (wl-logo-xbm))
      (bm (wl-logo-bitmap)))
  (if (and xbm (or (featurep 'xemacs)
		   (featurep 'image)
		   (condition-case nil (require 'bitmap) (error nil))))
      (progn
	(put 'wl-logo-xbm 'width (car xbm))
	(put 'wl-logo-xbm 'height (nth 1 xbm))
	(put 'wl-logo-xbm 'image
	     (cond
	      ((featurep 'xemacs)
	       (make-glyph (vector 'xbm ':data xbm)))
	      ((featurep 'image)
	       (create-image (nth 2 xbm) 'xbm t
			     ':width (car xbm) ':height (nth 1 xbm)))
	      (t
	       (let ((default-enable-multibyte-characters t)
		     (default-mc-flag t))
		 (with-temp-buffer
		   (let* ((cmp (if (car bm)
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
		     (buffer-string))))))))))

(defun wl-demo (&optional image-type)
  "Demo on the startup screen.
Optional IMAGE-TYPE overrides the variable `wl-demo-display-logo'."
  (interactive "P")
  (let ((demo-buf (let ((default-enable-multibyte-characters t)
			(default-mc-flag t)
			(default-line-spacing 0))
		    (get-buffer-create "*WL Demo*"))))
    (switch-to-buffer demo-buf)
    (cond ((and (featurep 'xemacs) (device-on-window-system-p))
	   (if (boundp 'default-gutter-visible-p)
	       (set-specifier (symbol-value 'default-gutter-visible-p)
			      nil demo-buf))
	   (set-specifier (symbol-value 'scrollbar-height) 0 demo-buf)
	   (set-specifier (symbol-value 'scrollbar-width) 0 demo-buf))
	  ((and (> emacs-major-version 20) window-system)
	   (make-local-hook 'kill-buffer-hook)
	   (let ((frame (selected-frame)))
	     (add-hook 'kill-buffer-hook
		       (` (lambda ()
			    (if (frame-live-p (, frame))
				(set-face-background
				 'fringe
				 (, (face-background 'fringe frame))
				 (, frame)))))
		       nil t)
	     (set-face-background 'fringe (face-background 'default frame)
				  frame))))
    (erase-buffer)
    (setq truncate-lines t)
    (let* ((wl-demo-display-logo
	    (if (and image-type (interactive-p))
		(let* ((selection '(("xbm" . xbm) ("xpm" . xpm) ("ascii")))
		       (type (completing-read "Image type: " selection nil t)))
		  (if (assoc type selection)
		      (cdr (assoc type selection))
		    t))
	      (or image-type wl-demo-display-logo)))
	   (logo (if (cond ((featurep 'xemacs)
			    (device-on-window-system-p))
			   ((featurep 'image)
			    (display-graphic-p))
			   (t window-system))
		     (cond ((and (eq 'xbm wl-demo-display-logo)
				 (get 'wl-logo-xbm 'width))
			    'wl-logo-xbm)
			   (wl-demo-display-logo
			    (cond ((get 'wl-logo-xpm 'width)
				   'wl-logo-xpm)
				  ((get 'wl-logo-xbm 'width)
				   'wl-logo-xbm))))))
	   (ww (window-width))
	   (wh (window-height))
	   rest)
      (if logo
	  (let ((lw (get logo 'width))
		(lh (get logo 'height))
		(image (get logo 'image)))
	    (cond
	     ((featurep 'xemacs)
	      (if (eq 'wl-logo-xbm logo)
		  (set-glyph-face image 'wl-highlight-logo-face))
	      (setq rest (- wh 1 (/ (+ (* lh wh) (window-pixel-height) -1)
				    (window-pixel-height))))
	      (insert-char ?\  (max 0 (/ (- (* (window-pixel-width) (1+ ww))
					    (* lw ww))
					 2 (window-pixel-width))))
	      (set-extent-end-glyph (make-extent (point) (point)) image))
	     ((featurep 'image)
	      (if (eq 'wl-logo-xbm logo)
		  (progn
		    (plist-put (cdr image) ':background
			       (face-background 'wl-highlight-logo-face))
		    (plist-put (cdr image) ':foreground
			       (face-foreground 'wl-highlight-logo-face))))
	      (setq rest (/ (- (* wh (frame-char-height)) lh 1)
			    (frame-char-height)))
	      (insert-char ?\  (max 0 (/ (- (* (frame-char-width) (1+ ww)) lw)
					 2 (frame-char-width))))
	      (insert-image image))
	     (t
	      (insert image)
	      (put-text-property (point-min) (point) 'face
				 'wl-highlight-logo-face)
	      (setq rest (/ (- (* 16 wh) lh 8) 16))
	      (indent-rigidly (point-min) (point-max)
			      (/ (- (* 8 (1+ ww)) lw) 16))))
	    (goto-char (point-min)))
	(insert (or wl-logo-ascii wl-appname))
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
      (let ((start (point)))
	(insert (format (cond ((<= rest 2)
			       "version %s - \"%s\"\n%s")
			      ((eq rest 3)
			       "version %s - \"%s\"\n\n%s")
			      (t
			       "\nversion %s - \"%s\"\n\n%s"))
			wl-version wl-codename wl-demo-copyright-notice))
	(put-text-property start (point) 'face 'wl-highlight-demo-face)
	(let ((fill-column ww))
	  (center-region start (point))))
      (goto-char (point-min))
      (sit-for (if (featurep 'lisp-float-type) (/ (float 5) (float 10)) 1))
      demo-buf)))

;;; wl-demo.el ends here
