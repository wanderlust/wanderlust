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

(require 'wl-vars)
(provide 'wl-demo)
(if (featurep 'xemacs)
    (require 'wl-xmas))

(eval-when-compile
  (defun-maybe device-on-window-system-p ())
  (defun-maybe glyph-height (a))
  (defun-maybe glyph-width (a))
  (defun-maybe make-extent (a b))
  (defun-maybe make-glyph (a))
  (defun-maybe set-extent-end-glyph (a b))
  (defun-maybe startup-center-spaces (a))
  (defun-maybe window-pixel-height ())
  (defun-maybe window-pixel-width ())
  (condition-case nil
      (require 'bitmap)
    (error nil))
  (defun-maybe bitmap-compose (a))
  (defun-maybe bitmap-decode-xbm (a))
  (defun-maybe bitmap-read-xbm-file (a))
  (unless (boundp ':data)
    (set (make-local-variable ':data) nil))
  (unless (boundp ':type)
    (set (make-local-variable ':type) nil))
  (condition-case nil
      (require 'image)
    (error nil))
  (defun-maybe frame-char-height ())
  (defun-maybe frame-char-width ())
  (defun-maybe image-type-available-p (a)))

(static-condition-case nil
    (progn
      (insert-image '(image))
      (defalias 'wl-insert-image 'insert-image))
  (wrong-number-of-arguments
   (defun wl-insert-image (image)
     (insert-image image "x")))
  (void-function
   (defun wl-insert-image (image))))

;;
;; demo ;-)
;;
(eval-when-compile
  (cond ((or (featurep 'xemacs) (featurep 'image))
	 (defmacro wl-title-logo ()
	   (let ((file (expand-file-name "wl-logo.xpm" wl-icon-dir)))
	     (if (file-exists-p file)
		 (let ((buffer (generate-new-buffer " *wl-logo*"))
		       (coding-system-for-read 'binary)
		       buffer-file-format format-alist
		       insert-file-contents-post-hook
		       insert-file-contents-pre-hook)
		   (prog1
		       (save-excursion
			 (set-buffer buffer)
			 (insert-file-contents file)
			 (buffer-string))
		     (kill-buffer buffer)))))))
	((condition-case nil
	     (require 'bitmap)
	   (error nil))
	 (defmacro wl-title-logo ()
	   (let ((file (expand-file-name "wl-logo.xbm" wl-icon-dir)))
	     (if (file-exists-p file)
		 (condition-case nil
		     (bitmap-decode-xbm (bitmap-read-xbm-file file))
		   (error (message "Bitmap Logo is not used.")))))))
	(t
	 (defmacro wl-title-logo ()))))

(defconst wl-title-logo
  (cond ((or (and (featurep 'xemacs)
                  (featurep 'xpm)
                  (device-on-window-system-p))
             (and (eval-when-compile (featurep 'image))
                  (image-type-available-p 'xpm)))
	 (wl-title-logo))
	((and window-system
	      (condition-case nil
		  (require 'bitmap)
		(error nil)))
	 (let ((cmp (wl-title-logo)))
	   (if cmp
	       (condition-case nil
		   (let ((len (length cmp))
			 (bitmap (bitmap-compose (aref cmp 0)))
			 (i 1))
		     (while (< i len)
		       (setq bitmap (concat bitmap "\n"
					    (bitmap-compose (aref cmp i)))
			     i (1+ i)))
		     bitmap)
		 (error nil)))))))

(defun wl-demo ()
  (interactive)
  (let ((demo-buf (get-buffer-create "*WL Demo*"))
	logo-ext start)
    (switch-to-buffer demo-buf)
    (erase-buffer)
    (if (and wl-demo-display-logo wl-title-logo)
	(cond
         ((featurep 'xemacs)
          (let ((wl-logo (make-glyph (vector 'xpm :data wl-title-logo))))
            (insert-char ?\n (max 1 (/ (- (window-height) 6
                                          (/ (glyph-height wl-logo)
                                             (/ (window-pixel-height)
                                                (window-height)))) 2)))
            (indent-to (startup-center-spaces wl-logo))
            (insert-char ?\ (max 0 (/ (- (window-width)
                                         (/ (glyph-width wl-logo)
                                            (/ (window-pixel-width)
                                               (window-width)))) 2)))
            (setq logo-ext (make-extent (point)(point)))
            (set-extent-end-glyph logo-ext wl-logo)))
         ((featurep 'image)
          (let ((wl-logo (list 'image :type 'xpm :data wl-title-logo))
                pixel-width pixel-height)
            (with-temp-buffer
              (insert wl-title-logo)
              (goto-char (point-min))
              (skip-syntax-forward "^\"")
              (when (looking-at "\"[ \t]*\\([0-9]+\\)[ \t]*\\([0-9]+\\)")
                (setq pixel-width (string-to-int (match-string 1))
                      pixel-height (string-to-int (match-string 2)))))
            (insert-char ?\n (max 1 (/ (- (window-height) 6
                                          (/ pixel-height
                                             (frame-char-height))) 2)))
            (insert-char ?\  (max 0 (/ (- (window-width)
                                          (/ pixel-width
                                             (frame-char-width))) 2)))
            (wl-insert-image wl-logo)
            (insert "\n")))
         (t
	  (insert wl-title-logo)
	  (indent-rigidly (point-min) (point-max)
			  (max 0 (/ (- (window-width) (current-column)) 2)))
	  (insert "\n")
	  (goto-char (point-min))
	  (insert-char ?\n (max 0 (/ (- (window-height)
					(count-lines (point) (point-max))
					6) 2)))
	  (goto-char (point-max))))
      (insert-char ?\n (max 1 (- (/ (window-height) 3) 2))))
    (setq start (point))
    (insert "\n" (if (and wl-demo-display-logo wl-title-logo)
		     ""
		   (concat wl-appname "\n")))
    (let ((fill-column (window-width)))
      (center-region start (point)))
    (setq start (point))
    (put-text-property (point-min) (point) 'face 'wl-highlight-logo-face)
    (insert (format "\nversion %s - \"%s\"\n\n"
		    wl-version wl-codename
		    ))
    (insert "Copyright (C) 1998-2000 Yuuichi Teranishi <teranisi@gohome.org>")
    (put-text-property start (point-max) 'face 'wl-highlight-demo-face)
    (let ((fill-column (window-width)))
      (center-region start (point)))
    (goto-char (point-min))
    (sit-for
     (if (featurep 'lisp-float-type) (/ (float 5) (float 10)) 1))
    ;;(if (featurep 'xemacs) (delete-extent logo-ext))
    demo-buf))

;;; wl-demo.el ends here
