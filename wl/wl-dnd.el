;;; wl-dnd.el --- dragdrop support on Wanderlust.

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

(static-cond
 ((featurep 'offix)
  (defun start-drag (event what &optional typ)
    (if (numberp typ)
	(funcall (intern "offix-start-drag") event what typ)
      (funcall (intern "offix-start-drag") event what))))
 ((featurep 'cde)
  (defun start-drag (event what &optional typ)
    (if (not typ)
	(funcall (intern "cde-start-drag-internal") event nil
		 (list what))
      (funcall (intern "cde-start-drag-internal") event t what))))
 (t (defun start-drag (event what &optional typ))))

(defun wl-dnd-start-drag (event)
  (interactive "@e")
  (mouse-set-point event)
  (start-drag event (concat
		     wl-summary-buffer-folder-name " "
		     (int-to-string (wl-summary-message-number)))))

(defun wl-dnd-drop-func (event object text)
  (interactive "@e")
  (mouse-set-point event)
  (beginning-of-line)
  (when (looking-at "^[ ]*\\([^\\[].+\\):.*\n")
    (let* ((src-spec (save-match-data
		       (split-string (nth 2 (car (cdr object)))
				     " ")))
	   (src-fld (nth 0 src-spec))
	   (number  (string-to-int (nth 1 src-spec)))
	   target)
      (setq target
	    (wl-folder-get-folder-name-by-id (get-text-property
					      (point)
					      'wl-folder-entity-id)))
      (message "%s is dropped at %s." number
	       (buffer-substring (match-beginning 1)
				 (match-end 1)))
      (set-buffer (wl-summary-get-buffer src-fld))
      (save-match-data
	(wl-summary-jump-to-msg number))
      (funcall (symbol-function 'wl-summary-refile) number target)
      (select-window (get-buffer-window (current-buffer)))
      ))
  t)

(defun wl-dnd-default-drop-message (event object)
  (message "Dropping here is meaningless.")
  t)

(defun wl-dnd-set-drop-target (beg end)
  (let (ext substr)
    (setq ext (make-extent beg end))
    (set-extent-property
     ext
     'experimental-dragdrop-drop-functions
     '((wl-dnd-drop-func t t (buffer-substring beg end))))))
;;; (set-extent-property ext 'mouse-face 'highlight)

(defun wl-dnd-set-drag-starter (beg end)
  (let (ext kmap)
    (setq ext (make-extent beg end))
;;; (set-extent-property ext 'mouse-face 'isearch)
    (setq kmap (make-keymap))
    (define-key kmap [button1] 'wl-dnd-start-drag)
    (set-extent-property ext 'keymap kmap)))

(require 'product)
(product-provide (provide 'wl-dnd) (require 'wl-version))

;;; wl-dnd.el ends here
