;;; wl-nemacs.el -- Wanderlust modules for Nemacs.

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

(defun wl-message-overload-functions ()
  (local-set-key "l" 'wl-message-toggle-disp-summary))

(defun wl-message-wheel-up (event)
  (interactive "e"))
(defun wl-message-wheel-down (event)
  (interactive "e"))

(defun wl-highlight-folder-current-line (&optional numbers))
(defun wl-highlight-folder-path (folder-path))
(defun wl-highlight-summary (start end))
(defun wl-highlight-folder-group-line (numbers))
(defun wl-highlight-summary-line-string (line mark indent before-indent))
(defun wl-highlight-body-region (beg end))
(defun wl-highlight-message (start end hack-sig &optional body-only))
(defun wl-highlight-summary-current-line (&optional smark regexp temp-too))

(defun wl-highlight-plugged-current-line ())
(defun wl-plugged-set-folder-icon (folder string)
  string)

(defmacro wl-defface (face spec doc &rest args)
  (` (defvar (, face) (, spec) (, doc))))

(defun wl-draft-mode-setup ()
  (defalias 'wl-draft-mode 'mail-mode))
(defun wl-draft-key-setup ())

;; ???
(defvar mime-article/kanji-code-alist
  (list (cons t (mime-charset-to-coding-system default-mime-charset))))

(defun wl-draft-overload-functions ()
  (wl-mode-line-buffer-identification)
  (local-set-key "\C-c\C-y" 'wl-draft-yank-original)
  (local-set-key "\C-c\C-s" 'wl-draft-send)
  (local-set-key "\C-c\C-a" 'wl-draft-insert-x-face-field)
  (local-set-key "\C-c\C-c" 'wl-draft-send-and-exit)
  (local-set-key "\C-c\C-z" 'wl-draft-save-and-exit)
  (local-set-key "\C-c\C-k" 'wl-draft-kill)
  (local-set-key "\C-l"     'wl-draft-highlight-and-recenter)
  (local-set-key "\C-i"     'wl-complete-field-body-or-tab)
  (local-set-key "\C-c\C-r" 'wl-draft-caesar-region)
  (local-set-key "\M-t"     'wl-toggle-plugged)
  (local-set-key "\C-c\C-o" 'wl-jump-to-draft-buffer)
  (local-set-key "\C-c\C-e" 'wl-draft-config-exec)
  (local-set-key "\C-c\C-j" 'wl-template-select)
  (local-set-key "\C-c\C-p" 'wl-draft-preview-message)
  (local-set-key "\C-x\C-s" 'wl-draft-save)
  (local-set-key "\C-xk"    'wl-draft-mimic-kill-buffer))

;;; Emulations.

(defvar-maybe user-mail-address nil)
(defvar-maybe mail-send-actions nil)
(defvar-maybe mail-default-headers nil)
(defvar-maybe mail-citation-hook nil)
(defvar-maybe mail-yank-hooks nil)
(defvar-maybe mail-mailer-swallows-blank-line nil)

(defvar mail-send-actions nil)

(defun-maybe mail-indent-citation ()
  "Modify text just inserted from a message to be cited.
The inserted text should be the region.
When this function returns, the region is again around the modified text.

Normally, indent each nonblank line `mail-indentation-spaces' spaces.
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line."
  (let ((start (point)))
    (mail-yank-clear-headers start (mark t))
    (if (null mail-yank-prefix)
	(indent-rigidly start (mark t) mail-indentation-spaces)
      (save-excursion
	(goto-char start)
	(while (< (point) (mark t))
	  (insert mail-yank-prefix)
	  (forward-line 1))))))

(defun-maybe mail-yank-clear-headers (start end)
  (save-excursion
    (goto-char start)
    (if (search-forward "\n\n" end t)
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char start)
	  (while (let ((case-fold-search t))
		   (re-search-forward mail-yank-ignored-headers nil t))
	    (beginning-of-line)
	    (delete-region (point)
			   (progn (re-search-forward "\n[^ \t]")
				  (forward-char -1)
				  (point))))))))

(defun-maybe find-file-name-handler (filename operation))

(defun-maybe read-event ()
  (setq unread-command-events
	(if (fboundp 'read-char-exclusive)
	    (read-char-exclusive)
	  ;; XXX Emacs18.59 does not have read-char-exclusive().
	  (read-char))))

(defmacro easy-menu-define (a b c d)
  (` (defvar (, a) nil (, c))))
(defmacro easy-menu-add (a)
  (` nil))

(defun copy-face (a b))
(defun make-face (a))
(defun set-face-foreground (a b))
(defun set-face-background (a b))
(defun set-face-underline-p (a b))
(defun set-face-font (a b))

;;; XXX cl's member() brings evil upon MIME-View.
;; cl is always called after poe-18, so `(require 'poe-18)' is
;; a dead duck... We MUST re-load it certainly.
(load-library "poe-18")

(require 'product)
(product-provide (provide 'wl-nemacs) (require 'wl-version))

;;; wl-nemacs.el ends here
