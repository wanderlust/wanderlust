;;; wl-highlight.el --- Hilight modules for Wanderlust.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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

(if (and (featurep 'xemacs)
	 (featurep 'dragdrop))
    (require 'wl-dnd))
(require 'wl-vars)
(provide 'wl-highlight)			; circular dependency

(eval-when-compile
  (cond (wl-on-xemacs
	 (require 'wl-xmas))
	(wl-on-emacs21
	 (require 'wl-e21))
	(t
	 (require 'wl-mule)))
  (defun-maybe extent-begin-glyph (a))
  (defun-maybe delete-extent (a))
  (defun-maybe make-extent (a b))
  (defun-maybe set-extent-begin-glyph (a b))
  (defun-maybe set-extent-end-glyph (a b))
  (defun-maybe extent-at (a b c d e))
  (defun-maybe wl-dnd-set-drop-target (a b))
  (defun-maybe wl-dnd-set-drag-starter (a b)))

(put 'wl-defface 'lisp-indent-function 'defun)

(defgroup wl-faces nil
  "Wanderlust, Faces."
  :prefix "wl-highlight-"
  :group 'wl-highlight
  :group 'wl)

(defgroup wl-summary-faces nil
  "Wanderlust, Faces of summary buffer."
  :prefix "wl-highlight-"
  :group 'wl-highlight
  :group 'wl-summary)

(defgroup wl-folder-faces nil
  "Wanderlust, Faces of folder buffer."
  :prefix "wl-highlight-"
  :group 'wl-highlight
  :group 'wl-folder)

(defgroup wl-message-faces nil
  "Wanderlust, Faces of message buffer."
  :prefix "wl-highlight-"
  :group 'wl-highlight)

;; for message header and signature

(wl-defface wl-highlight-message-headers
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "gray" :bold t))
    (((class color)
      (background light))
     (:foreground "gray50" :bold t)))
  "Face used for displaying header names."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-header-contents
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "LightSkyBlue" :bold t))
    (((class color)
      (background light))
     (:foreground "purple" :bold t)))
  "Face used for displaying header content."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-important-header-contents
  '(
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((class color)
      (background dark))
     (:foreground "yellow" :bold t))
    (((class color)
      (background light))
     (:foreground "brown" :bold t)))
  "Face used for displaying contents of special headers."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-important-header-contents2
  '(
    (((type tty)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background dark))
     (:foreground "orange" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkSlateBlue" :bold t)))
  "Face used for displaying contents of special headers."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-citation-header
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "SkyBlue"))
    (((class color)
      (background light))
     (:foreground "DarkGreen")))
  "Face used for displaying header of quoted texts."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-unimportant-header-contents
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "GreenYellow" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen" :bold t)))
  "Face used for displaying contents of unimportant headers."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-signature
  '((((class color)
      (background dark))
     (:foreground "khaki"))
    (((class color)
      (background light))
     (:foreground "DarkSlateBlue")))
  "Face used for displaying signature."
  :group 'wl-message-faces
  :group 'wl-faces)

;; for draft

(wl-defface wl-highlight-header-separator-face
  '(
    (((type tty)
      (background dark))
     (:foreground "black" :background "yellow"))
    (((class color))
     (:foreground "Black" :background "DarkKhaki")))
  "Face used for displaying header separator."
  :group 'wl-draft
  :group 'wl-faces)

;; important messages

(wl-defface wl-highlight-summary-important-face
  '(
    (((type tty)
      (background dark))
     (:foreground "magenta"))
    (((class color)
      (background dark))
     (:foreground "orange"))
    (((class color)
      (background light))
     (:foreground "purple")))
  "Face used for displaying important messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-new-face
  '(
    (((type tty)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background dark))
     (:foreground "tomato"))
    (((class color)
      (background light))
     (:foreground "tomato")))
  "Face used for displaying new messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-displaying-face
  '((t
     (:underline t :bold t)))
  "Face used for displaying message."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-thread-indent-face
  '((t
     (:foreground "gray40")))
  "Face used for displaying indented thread."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; unimportant messages

(wl-defface wl-highlight-summary-unread-face
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "LightSkyBlue"))
    (((class color)
      (background light))
     (:foreground "RoyalBlue")))
  "Face used for displaying unread messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-deleted-face
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "gray"))
    (((class color)
      (background light))
     (:foreground "DarkKhaki")))
  "Face used for displaying messages mark as deleted."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-refiled-face
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background light))
     (:foreground "firebrick")))
  "Face used for displaying messages mark as refiled."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-copied-face
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background light))
     (:foreground "blue")))
  "Face used for displaying messages mark as copied."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; obsolete.
(wl-defface wl-highlight-summary-temp-face
  '(
    (((type tty)
      (background dark))
     (:foreground "gold"))
    (((class color))
     (:foreground "HotPink1")))
  "Face used for displaying messages mark as temp."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-target-face
  '(
    (((type tty)
      (background dark))
     (:foreground "gold"))
    (((class color))
     (:foreground "HotPink1")))
  "Face used for displaying messages mark as target."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-low-read-face
  '(
    (((type tty)
      (background dark))
     (:foreground "yellow" :italic t))
    (((class color)
      (background dark))
     (:foreground "PaleGreen" :italic t))
    (((class color)
      (background light))
     (:foreground "Green3" :italic t)))
  "Face used for displaying low interest read messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-high-read-face
  '(
    (((type tty))
     (:bold t))
    (((class color)
      (background dark))
     (:foreground "PaleGreen" :bold t))
    (((class color)
      (background light))
     (:foreground "SeaGreen" :bold t)))
  "Face used for displaying high interest read messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-low-unread-face
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan" :italic t))
    (((class color)
      (background dark))
     (:foreground "LightSkyBlue" :italic t))
    (((class color)
      (background light))
     (:foreground "RoyalBlue" :italic t)))
  "Face used for displaying low interest unread messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-high-unread-face
  '(
    (((type tty))
     (:foreground "red" :bold t))
    (((class color)
      (background dark))
     (:foreground "tomato" :bold t))
    (((class color)
      (background light))
     (:foreground "tomato" :bold t)))
  "Face used for displaying high interest unread messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; ordinary messages

(wl-defface wl-highlight-summary-thread-top-face
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "GreenYellow"))
    (((class color)
      (background light))
     (:foreground "green4")))
  "Face used for displaying top thread message."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-normal-face
  '(
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((class color)
      (background dark))
     (:foreground "PaleGreen"))
    (((class color)
      (background light))
     (:foreground "SeaGreen")))
  "Face used for displaying normal message."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; folder

(wl-defface wl-highlight-folder-unknown-face
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "pink"))
    (((class color)
      (background light))
     (:foreground "RoyalBlue")))
  "Face used for displaying unread folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-killed-face
  '(
    (((type tty)
      (background dark))
     (:foreground "gray"))
    (((class color))
     (:foreground "gray50")))
  "Face used for displaying killed folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-zero-face
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "SkyBlue"))
    (((class color)
      (background light))
     (:foreground "BlueViolet")))
  "Face used for displaying folder needs no sync."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-few-face
  '(
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((class color)
      (background dark))
     (:foreground "orange"))
    (((class color)
      (background light))
     (:foreground "OrangeRed3")))
  "Face used for displaying folder contains few unsync messages."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-many-face
  '(
    (((type tty)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background dark))
     (:foreground "HotPink1"))
    (((class color)
      (background light))
     (:foreground "tomato")))
  "Face used for displaying folder contains many unsync messages."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-unread-face
  '(
    (((type tty)
      (background dark))
     (:foreground "magenta"))
    (((class color)
      (background dark))
     (:foreground "gold"))
    (((class color)
      (background light))
     (:foreground "MediumVioletRed")))
  "Face used for displaying unread folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-opened-face
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "PaleGreen"))
    (((class color)
      (background light))
     (:foreground "ForestGreen")))
  "Face used for displaying opened group folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-closed-face
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "GreenYellow"))
    (((class color)
      (background light))
     (:foreground "DarkOliveGreen4")))
  "Face used for displaying closed group folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-path-face
  '((t
     (:bold t :underline t)))
  "Face used for displaying path."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-demo-face
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "GreenYellow"))
    (((class color)
      (background light))
     (:foreground "blue2")))
  "Face used for displaying demo."
  :group 'wl-faces)

(wl-defface wl-highlight-logo-face
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "SkyBlue"))
    (((class color)
      (background light))
     (:foreground "SteelBlue")))
  "Face used for displaying demo."
  :group 'wl-faces)

(wl-defface wl-highlight-refile-destination-face
  '((((class color)
      (background dark))
     (:foreground "pink"))
    (((class color)
      (background light))
     (:foreground "red")))
  "Face used for displaying refile destination."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; cited face

(wl-defface wl-highlight-message-cited-text-1
  '(
    (((type tty)
      (background dark))
     (:foreground "magenta"))
    (((class color)
      (background dark))
     (:foreground "HotPink1"))
    (((class color)
      (background light))
     (:foreground "ForestGreen")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-2
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color))
     (:foreground "violet")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-3
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color))
     (:foreground "orchid3")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-4
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color))
     (:foreground "purple1")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-5
  '(
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((class color))
     (:foreground "MediumPurple1")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-6
  '(
    (((type tty)
      (background dark))
     (:foreground "red"))
    (((class color))
     (:foreground "PaleVioletRed")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-7
  '(
    (((type tty)
      (background dark))
     (:foreground "magenta"))
    (((class color))
     (:foreground "LightPink")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-8
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color))
     (:foreground "salmon")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-9
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color))
     (:foreground "SandyBrown")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-10
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color))
     (:foreground "wheat")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defvar wl-highlight-folder-opened-regexp " *\\(\\[\\-\\]\\)")
(defvar wl-highlight-folder-closed-regexp " *\\(\\[\\+\\]\\)")
(defvar wl-highlight-folder-leaf-regexp "[ ]*\\([-%\\+]\\)\\(.*\\):.*$")

(defvar wl-highlight-summary-unread-regexp " *[0-9]+[^0-9]\\(!\\|U\\)")
(defvar wl-highlight-summary-important-regexp " *[0-9]+[^0-9]\\$")
(defvar wl-highlight-summary-new-regexp " *[0-9]+[^0-9]N")
(defvar wl-highlight-summary-deleted-regexp " *[0-9]+D")
(defvar wl-highlight-summary-refiled-regexp " *[0-9]+o")
(defvar wl-highlight-summary-copied-regexp " *[0-9]+O")
(defvar wl-highlight-summary-target-regexp " *[0-9]+\\*")
;;(defvar wl-highlight-summary-thread-top-regexp " *[0-9]+[^0-9][^0-9]../..\(.*\)..:.. \\[")

(defvar wl-highlight-citation-face-list
  '(wl-highlight-message-cited-text-1
    wl-highlight-message-cited-text-2
    wl-highlight-message-cited-text-3
    wl-highlight-message-cited-text-4
    wl-highlight-message-cited-text-5
    wl-highlight-message-cited-text-6
    wl-highlight-message-cited-text-7
    wl-highlight-message-cited-text-8
    wl-highlight-message-cited-text-9
    wl-highlight-message-cited-text-10))

(defmacro wl-delete-all-overlays ()
  "Delete all momentary overlays."
  '(let ((overlays (overlays-in (point-min) (point-max)))
	 overlay)
     (while (setq overlay (car overlays))
       (if (overlay-get overlay 'wl-momentary-overlay)
	   (delete-overlay overlay))
       (setq overlays (cdr overlays)))))

(defun wl-highlight-summary-displaying ()
  (interactive)
  (wl-delete-all-overlays)
  (let (bol eol ov)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (setq bol (point))
      (setq ov (make-overlay bol eol))
      (overlay-put ov 'face 'wl-highlight-summary-displaying-face)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'wl-momentary-overlay t))))

(defun wl-highlight-folder-group-line (numbers)
  (end-of-line)
  (let ((eol (point))
	bol)
    (beginning-of-line)
    (setq bol (point))
    (let ((text-face (cond ((looking-at wl-highlight-folder-opened-regexp)
			    'wl-highlight-folder-opened-face)
			   ((looking-at wl-highlight-folder-closed-regexp)
			    'wl-highlight-folder-closed-face))))
      (if (and wl-highlight-folder-by-numbers
	       (re-search-forward "[0-9-]+/[0-9-]+/[0-9-]+" eol t))
	  (let* ((unsync (nth 0 numbers))
		 (unread (nth 1 numbers))
		 (face (cond ((and unsync (zerop unsync))
			      (if (and unread (> unread 0))
				  'wl-highlight-folder-unread-face
				'wl-highlight-folder-zero-face))
			     ((and unsync
				   (>= unsync wl-folder-many-unsync-threshold))
			      'wl-highlight-folder-many-face)
			     (t
			      'wl-highlight-folder-few-face))))
	    (if (numberp wl-highlight-folder-by-numbers)
		(progn
		  (put-text-property bol (match-beginning 0) 'face text-face)
		  (put-text-property (match-beginning 0) (match-end 0)
				     'face face))
	      ;; Remove previous face.
	      (put-text-property bol (match-end 0) 'face nil)
	      (put-text-property bol (match-end 0) 'face face)))
	(put-text-property bol eol 'face text-face)))))

(defun wl-highlight-summary-line-string (line mark temp-mark indent)
  (let (fsymbol)
    (cond ((and (string= temp-mark "+")
		(member mark (list wl-summary-unread-cached-mark
				   wl-summary-unread-uncached-mark
				   wl-summary-new-mark)))
	   (setq fsymbol 'wl-highlight-summary-high-unread-face))
	  ((and (string= temp-mark "-")
		(member mark (list wl-summary-unread-cached-mark
				   wl-summary-unread-uncached-mark
				   wl-summary-new-mark)))
	   (setq fsymbol 'wl-highlight-summary-low-unread-face))
	  ((string= temp-mark "o")
	   (setq fsymbol 'wl-highlight-summary-refiled-face))
	  ((string= temp-mark "O")
	   (setq fsymbol 'wl-highlight-summary-copied-face))
	  ((string= temp-mark "D")
	   (setq fsymbol 'wl-highlight-summary-deleted-face))
	  ((string= temp-mark "*")
	   (setq fsymbol 'wl-highlight-summary-temp-face))
	  ((string= mark wl-summary-new-mark)
	   (setq fsymbol 'wl-highlight-summary-new-face))
	  ((member mark (list wl-summary-unread-cached-mark
			      wl-summary-unread-uncached-mark))
	   (setq fsymbol 'wl-highlight-summary-unread-face))
	  ((or (string= mark wl-summary-important-mark))
	   (setq fsymbol 'wl-highlight-summary-important-face))
	  ((string= temp-mark "-")
	   (setq fsymbol 'wl-highlight-summary-low-read-face))
	  ((string= temp-mark "+")
	   (setq fsymbol 'wl-highlight-summary-high-read-face))
	  (t (if (zerop (length indent))
		 (setq fsymbol 'wl-highlight-summary-thread-top-face)
	       (setq fsymbol 'wl-highlight-summary-normal-face))))
    (put-text-property 0 (length line) 'face fsymbol line))
  (if wl-use-highlight-mouse-line
      (put-text-property 0 (length line) 'mouse-face 'highlight line)))

(defun wl-highlight-summary-current-line (&optional smark regexp temp-too)
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  (case-fold-search nil) temp-mark status-mark
	  (deactivate-mark nil)
	  (sregexp (concat
		    "^"
		    wl-summary-buffer-number-regexp
		    "\\(.\\)\\(.\\)../..\(.*\)..:.. \\("
		    wl-highlight-thread-indent-string-regexp
		    "\\)[[<]"))
	  fregexp fsymbol bol eol matched thread-top looked-at dest ds)
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (setq bol (point))
      (if smark
	  (setq status-mark smark)
	(setq looked-at (looking-at sregexp))
	(when looked-at
	  (setq status-mark (buffer-substring (match-beginning 2)
					      (match-end 2)))))
      (when temp-too
	(unless looked-at
	  (setq looked-at (looking-at sregexp)))
	(when looked-at
	  (setq temp-mark (buffer-substring (match-beginning 1)
					    (match-end 1)))
	  (cond
	   ((string= temp-mark "*")
	    (setq fsymbol 'wl-highlight-summary-temp-face))
	   ((string= temp-mark "D")
	    (setq fsymbol 'wl-highlight-summary-deleted-face))
	   ((string= temp-mark "O")
	    (setq fsymbol 'wl-highlight-summary-copied-face
		  dest t))
	   ((string= temp-mark "o")
	    (setq fsymbol 'wl-highlight-summary-refiled-face
		  dest t)))))
      (if (not fsymbol)
	  (cond
	   ((and (string= temp-mark "+")
		 (member status-mark (list wl-summary-unread-cached-mark
					   wl-summary-unread-uncached-mark
					   wl-summary-new-mark)))
	    (setq fsymbol 'wl-highlight-summary-high-unread-face))
	   ((and (string= temp-mark "-")
		 (member status-mark (list wl-summary-unread-cached-mark
					   wl-summary-unread-uncached-mark
					   wl-summary-new-mark)))
	    (setq fsymbol 'wl-highlight-summary-low-unread-face))
	   ((string= status-mark wl-summary-new-mark)
	    (setq fsymbol 'wl-highlight-summary-new-face))
	   ((member status-mark (list wl-summary-unread-cached-mark
				      wl-summary-unread-uncached-mark))
	    (setq fsymbol 'wl-highlight-summary-unread-face))
	   ((string= status-mark wl-summary-important-mark)
	    (setq fsymbol 'wl-highlight-summary-important-face))
	   ;; score mark
	   ((string= temp-mark "-")
	    (setq fsymbol 'wl-highlight-summary-low-read-face))
	   ((string= temp-mark "+")
	    (setq fsymbol 'wl-highlight-summary-high-read-face))
	   ;;
	   (t (if (and looked-at
		       (string= (buffer-substring
				 (match-beginning 3)
				 (match-end 3)) ""))
		  (setq fsymbol 'wl-highlight-summary-thread-top-face)
		(setq fsymbol 'wl-highlight-summary-normal-face)))))
      (put-text-property bol eol 'face fsymbol)
      (when dest
	(put-text-property (next-single-property-change
			    (next-single-property-change
			     bol 'wl-summary-destination
			     nil eol)
			    'wl-summary-destination nil eol)
			   eol
			   'face
			   'wl-highlight-refile-destination-face))
      (if wl-use-highlight-mouse-line
	  (put-text-property bol
;;; Use bol instead of (1- (match-end 0))
;;;			     (1- (match-end 0))
			     eol 'mouse-face 'highlight))
;;;   (put-text-property (match-beginning 3) (match-end 3)
;;;			 'face 'wl-highlight-thread-indent-face)
      ;; Dnd stuff.
      (if wl-use-dnd
	  (wl-dnd-set-drag-starter bol eol)))))

(defun wl-highlight-folder (start end)
  "Highlight folder between start and end.
Faces used:
  wl-highlight-folder-unknown-face      unread messages
  wl-highlight-folder-zero-face         folder needs no sync
  wl-highlight-folder-few-face          folder contains few unsync messages
  wl-highlight-folder-many-face         folder contains many unsync messages
  wl-highlight-folder-opened-face       opened group folder
  wl-highlight-folder-closed-face       closed group folder

Variables used:
  wl-highlight-folder-opened-regexp     matches opened group folder
  wl-highlight-folder-closed-regexp     matches closed group folder
"
  (interactive "r")
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (let* ((lines (count-lines start end))
	 (real-end end)
	 gc-message)
    (save-excursion
      (save-restriction
	(widen)
	(narrow-to-region start end)
	(save-restriction
	  (goto-char start)
	  (while (not (eobp))
	    (wl-highlight-folder-current-line)
	    (forward-line 1)))))))

(defun wl-highlight-folder-path (folder-path)
  "Highlight current folder path...overlay"
  (save-excursion
    (wl-delete-all-overlays)
    (let ((fp folder-path) ov)
      (goto-char (point-min))
      (while (and fp
		  (not (eobp)))
	(beginning-of-line)
	(or (looking-at "^[ ]*\\[[\\+-]\\]\\(.+\\):.*\n")
	    (looking-at "^[ ]*\\([^ \\[].+\\):.*\n"))
	(when (equal
	       (get-text-property (point) 'wl-folder-entity-id)
	       (car fp))
	  (setq fp (cdr fp))
	  (setq ov (make-overlay
		    (match-beginning 1)
		    (match-end 1)))
	  (setq wl-folder-buffer-cur-point (point))
	  (overlay-put ov 'face 'wl-highlight-folder-path-face)
	  (overlay-put ov 'evaporate t)
	  (overlay-put ov 'wl-momentary-overlay t))
	(forward-line 1)))))

(defun wl-highlight-refile-destination-string (string)
  (put-text-property 0 (length string) 'face
		     'wl-highlight-refile-destination-face
		     string))

(defun wl-highlight-summary-all ()
  "For evaluation"
  (interactive)
  (wl-highlight-summary (point-min)(point-max)))

(defun wl-highlight-summary (start end)
  "Highlight summary between start and end.
Faces used:
  wl-highlight-summary-unread-face      unread messages
  wl-highlight-summary-important-face   important messages
  wl-highlight-summary-deleted-face     messages mark as deleted
  wl-highlight-summary-refiled-face     messages mark as refiled
  wl-highlight-summary-copied-face      messages mark as copied
  wl-highlight-summary-new-face         new messages

Variables used:
  wl-highlight-summary-unread-regexp    matches unread messages
  wl-highlight-summary-important-regexp matches important messages
  wl-highlight-summary-deleted-regexp   matches messages mark as deleted
  wl-highlight-summary-refiled-regexp   matches messages mark as refiled
  wl-highlight-summary-copied-regexp    matches messages mark as copied
  wl-highlight-summary-new-regexp       matches new messages
"
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (let (lines too-big gc-message e p hend i percent)
    (save-excursion
      (unless wl-summary-lazy-highlight
	(setq lines (count-lines start end)
	      too-big (and wl-highlight-max-summary-lines
			   (> lines wl-highlight-max-summary-lines))))
      (goto-char start)
      (setq i 0)
      (while (and (not (eobp))
		  (< (point) end))
	(wl-highlight-summary-current-line nil nil
					   (or wl-summary-lazy-highlight
					       wl-summary-scored))
	(when (and (not wl-summary-lazy-highlight)
		   (> lines elmo-display-progress-threshold))
	  (setq i (+ i 1))
	  (setq percent (/ (* i 100) lines))
	  (if (or (zerop (% percent 5)) (= i lines))
	      (elmo-display-progress
	       'wl-highlight-summary "Highlighting..."
	       percent)))
	(forward-line 1))
      (unless wl-summary-lazy-highlight
	(message "Highlighting...done")))))

(defun wl-highlight-summary-window (&optional win beg)
  "Highlight summary window.
This function is defined for `window-scroll-functions'"
  (if wl-summary-highlight
      (with-current-buffer (window-buffer win)
	(wl-highlight-summary (window-start win)
			      (save-excursion
				(goto-char (window-start win))
				(forward-line (frame-height))
				(point)))
	(set-buffer-modified-p nil))))

(defun wl-highlight-headers (&optional for-draft)
  (let ((beg (point-min))
	(end (or (save-excursion (re-search-forward "^$" nil t)
				 (point))
		 (point-max))))
    (wl-highlight-message beg end nil)
    (unless for-draft
      (when wl-highlight-x-face-function
	(funcall wl-highlight-x-face-function)))
    (run-hooks 'wl-highlight-headers-hook)))

(defun wl-highlight-body-all ()
  (wl-highlight-message (point-min) (point-max) t t))

(defun wl-highlight-body ()
  (let ((beg (or (save-excursion (goto-char (point-min))
				 (re-search-forward "^$" nil t))
		 (point-min)))
	(end (point-max)))
    (wl-highlight-message beg end t)))

(defun wl-highlight-body-region (beg end)
  (wl-highlight-message beg end t t))

(defun wl-highlight-signature-search-simple (beg end)
  "Search signature area in the body message between BEG and END.
Returns start point of signature."
  (save-excursion
    (goto-char end)
    (if (re-search-backward "\n--+ *\n" beg t)
	(if (eq (char-after (point)) ?\n)
	    (1+ (point))
	  (point))
      end)))

(defun wl-highlight-signature-search (beg end)
  "Search signature area in the body message between BEG and END.
Returns start point of signature."
  (save-excursion
    (goto-char end)
    (or
     ;; look for legal signature separator (check at first for fasten)
     (re-search-backward "\n-- \n" beg t)

     ;; look for dual separator
     (save-excursion
       (and
	(re-search-backward "^[^A-Za-z0-9> \t\n]+ *$" beg t)
	(> (- (match-end 0) (match-beginning 0)) 10);; "10" is a magic number.
	(re-search-backward
	 (concat "^"
		 (regexp-quote (buffer-substring (match-beginning 0) (match-end 0)))
		 "$") beg t)))

     ;; look for user specified signature-separator
     (if (stringp wl-highlight-signature-separator)
	 (re-search-backward wl-highlight-signature-separator nil t);; case one string
       (let ((sep wl-highlight-signature-separator))		;; case list
	 (while (and sep
		     (not (re-search-backward (car sep) beg t)))
	   (setq sep (cdr sep)))
	 (point)))	;; if no separator found, returns end.
     )))

(defun wl-highlight-message (start end hack-sig &optional body-only)
  "Highlight message headers between start and end.
Faces used:
  wl-highlight-message-headers			  the part before the colon
  wl-highlight-message-header-contents		  the part after the colon
  wl-highlight-message-important-header-contents  contents of \"special\"
                                                  headers
  wl-highlight-message-important-header-contents2 contents of \"special\"
                                                  headers
  wl-highlight-message-unimportant-header-contents contents of unimportant
                                                   headers
  wl-highlight-message-cited-text	           quoted text from other
                                                   messages
  wl-highlight-message-citation-header             header of quoted texts
  wl-highlight-message-signature                   signature

Variables used:
  wl-highlight-important-header-regexp	 what makes a \"special\" header
  wl-highlight-important-header2-regexp	 what makes a \"special\" header
  wl-highlight-unimportant-header-regexp what makes a \"special\" header
  wl-highlight-citation-prefix-regexp	 matches lines of quoted text
  wl-highlight-citation-header-regexp	 matches headers for quoted text

If HACK-SIG is true,then we search backward from END for something that
looks like the beginning of a signature block, and don't consider that a
part of the message (this is because signatures are often incorrectly
interpreted as cited text.)"
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (let ((too-big (and wl-highlight-max-message-size
		      (> (- end start)
			 wl-highlight-max-message-size)))
	(real-end end)
	current  beg
	e p hend)
    (if too-big
	nil
      (save-excursion
	(save-restriction
	  (widen)
	  ;; take off signature
	  (if (and hack-sig (not too-big))
	      (setq end (funcall wl-highlight-signature-search-function
				 (- end wl-max-signature-size) end)))
	  (if (and hack-sig
		   (not (eq end real-end)))
	      (put-text-property end (point-max)
				 'face 'wl-highlight-message-signature))
	  (narrow-to-region start end)
	  (save-restriction
	    ;; narrow down to just the headers...
	    (goto-char start)
	    ;; If this search fails then the narrowing performed above
	    ;; is sufficient
	    (if (re-search-forward (format
				    "^$\\|%s"
				    (regexp-quote mail-header-separator))
				   nil t)
		(narrow-to-region (point-min) (match-beginning 0)))
	    ;; highlight only when header is not too-big.
	    (when (or (null wl-highlight-max-header-size)
		      (< (point) wl-highlight-max-header-size))
	      (goto-char start)
	      (while (and (not body-only)
			  (not (eobp)))
		(cond
		 ((looking-at "^[^ \t\n:]+[ \t]*:")
		  (put-text-property (match-beginning 0) (match-end 0)
				     'face 'wl-highlight-message-headers)
		  (setq p (match-end 0))
		  (setq hend (save-excursion (std11-field-end end)))
		  (cond
		   ((catch 'match
		      (let ((regexp-alist wl-highlight-message-header-alist))
			(while regexp-alist
			  (when (save-match-data
				  (looking-at (caar regexp-alist)))
			    (put-text-property p hend 'face
					       (cdar regexp-alist))
			    (throw 'match t))
			  (setq regexp-alist (cdr regexp-alist)))
			(throw 'match nil))))
		   (t
		    (put-text-property
		     p hend 'face 'wl-highlight-message-header-contents)))
		  (goto-char hend))
		 ;; ignore non-header field name lines
		 (t (forward-line 1))))))
	  (let (prefix prefix-face-alist pair end)
	    (while (not (eobp))
	      (cond
	       ((looking-at mail-header-separator)
		(put-text-property (match-beginning 0) (match-end 0)
				   'face 'wl-highlight-header-separator-face)
		(goto-char (match-end 0)))
	       ((null wl-highlight-force-citation-header-regexp)
		nil)
	       ((looking-at wl-highlight-force-citation-header-regexp)
		(setq current 'wl-highlight-message-citation-header)
		(setq end (match-end 0)))
	       ((null wl-highlight-citation-prefix-regexp)
		nil)
	       ((looking-at wl-highlight-citation-prefix-regexp)
		(setq prefix (buffer-substring (point)
					       (match-end 0)))
		(setq pair (assoc prefix prefix-face-alist))
		(unless pair
		  (setq prefix-face-alist
			(append prefix-face-alist
				(list
				 (setq pair
				       (cons
					prefix
					(nth
					 (% (length prefix-face-alist)
					    (length
					     wl-highlight-citation-face-list))
					 wl-highlight-citation-face-list)))))))
		(unless wl-highlight-highlight-citation-too
		  (goto-char (match-end 0)))
		(setq current (cdr pair)))
	       ((null wl-highlight-citation-header-regexp)
		nil)
	       ((looking-at wl-highlight-citation-header-regexp)
		(setq current 'wl-highlight-message-citation-header)
		(setq end (match-end 0)))
	       (t (setq current nil)))
	      (cond (current
		     (setq p (point))
		     (forward-line 1) ; this is to put the \n in the face too
		     (let ()
;;;		       ((inhibit-read-only t))
		       (put-text-property p (or end (point))
					  'face current)
		       (setq end nil))
		     (forward-char -1)))
	      (forward-line 1)))
	  (run-hooks 'wl-highlight-message-hook))))))

;; highlight-mouse-line for folder mode

(defun wl-highlight-folder-mouse-line ()
  (interactive)
  (let* ((end (save-excursion (end-of-line) (point)))
	 (beg (progn
		(re-search-forward "[^ ]" end t)
		(1- (point))))
	 (inhibit-read-only t))
    (put-text-property beg end 'mouse-face 'highlight)))

(require 'product)
(product-provide (provide 'wl-highlight) (require 'wl-version))

;;; wl-highlight.el ends here
