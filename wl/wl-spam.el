;;; wl-spam.el --- Spam filtering interface for Wanderlust.

;; Copyright (C) 2003 Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Keywords: mail, net news, spam

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

(eval-when-compile (require 'cl))

(require 'elmo-spam)
(require 'wl-summary)
(require 'wl-action)
(require 'wl-highlight)

(defgroup wl-spam nil
  "Spam configuration for wanderlust."
  :group 'wl)

(defcustom wl-spam-folder "+spam"
  "*Spam folder."
  :type 'string
  :group 'wl-spam)

(defcustom wl-spam-undecided-folder-regexp-list '("inbox")
  "*List of folder regexp which is contained undecided domain."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-spam)

(defcustom wl-spam-ignored-folder-regexp-list
  (list (regexp-opt (list wl-draft-folder
			  wl-trash-folder
			  wl-queue-folder)))
  "*List of folder regexp which is contained ignored domain."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-spam)

(defcustom wl-spam-auto-check-folder-regexp-list nil
  "*List of Folder regexp which check spam automatically."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-spam)

(defcustom wl-spam-auto-check-marks
  (list wl-summary-new-uncached-mark
	wl-summary-new-cached-mark)
  "Persistent marks to check spam automatically."
  :type '(choice (const :tag "All marks" all)
		 (repeat (string :tag "Mark")))
  :group 'wl-spam)

(wl-defface wl-highlight-summary-spam-face
  '((((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color))
     (:foreground "LightSlateGray")))
  "Face used for displaying messages mark as spam."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defcustom wl-spam-mark-action-list
  '(("s"
     spam
     nil
     wl-summary-register-temp-mark
     wl-summary-exec-action-spam
     wl-highlight-summary-spam-face
     "Mark messages as spam."))
  "A variable to define Mark & Action for spam.
Append this value to `wl-summary-mark-action-list' by `wl-spam-setup'.

See `wl-summary-mark-action-list' for the detail of element."
  :type '(repeat (list
		  (string :tag "Temporary mark")
		  (symbol :tag "Action name")
		  (symbol :tag "Argument function")
		  (symbol :tag "Set mark function")
		  (symbol :tag "Exec function")
		  (symbol :tag "Face symbol")
		  (string :tag "Document string")))
  :group 'wl-spam)

(defun wl-spam-domain (folder-name)
  (cond ((string= folder-name wl-spam-folder)
	 'spam)
	((wl-string-match-member folder-name
				 wl-spam-undecided-folder-regexp-list)
	 'undecided)
	((wl-string-match-member folder-name
				 wl-spam-ignored-folder-regexp-list)
	 'ignore)
	(t
	 'good)))

(defsubst wl-spam-auto-check-message-p (folder number)
  (or (eq wl-spam-auto-check-marks 'all)
      (member (wl-summary-message-mark folder number)
	      wl-spam-auto-check-marks)))

(defsubst wl-spam-map-spam-messages (folder numbers function &rest args)
  (let ((total (length numbers)))
    (message "Checking spam...")
    (elmo-with-progress-display (> total elmo-display-progress-threshold)
	(elmo-spam-check-spam total "Checking spam...")
      (dolist (number (elmo-spam-list-spam-messages (elmo-spam-processor)
						    folder
						    numbers))
	(apply function number args)))
    (message "Checking spam...done")))

(defun wl-spam-register-spam-messages (folder numbers)
  (let ((total (length numbers)))
    (message "Registering spam...")
    (elmo-with-progress-display (> total elmo-display-progress-threshold)
	(elmo-spam-register total "Registering spam...")
      (elmo-spam-register-spam-messages (elmo-spam-processor)
					wl-summary-buffer-elmo-folder
					numbers))
    (message "Registering spam...done")))

(defun wl-spam-register-good-messages (folder numbers)
  (let ((total (length numbers)))
    (message "Registering good...")
    (elmo-with-progress-display (> total elmo-display-progress-threshold)
	(elmo-spam-register total "Registering good...")
      (elmo-spam-register-good-messages (elmo-spam-processor)
					wl-summary-buffer-elmo-folder
					numbers))
    (message "Registering good...done")))

(defun wl-spam-save-status (&optional force)
  (interactive "P")
  (let ((processor (elmo-spam-processor (not force))))
    (when (or force
	      (and processor (elmo-spam-modified-p processor)))
      (elmo-spam-save-status processor))))

;; insinuate into summary mode
(defvar wl-summary-spam-map nil)

(unless wl-summary-spam-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'wl-summary-spam)
    (define-key map "c" 'wl-summary-test-spam)
    (define-key map "C" 'wl-summary-mark-spam)
    (define-key map "s" 'wl-summary-register-as-spam)
    (define-key map "S" 'wl-summary-register-as-spam-all)
    (define-key map "n" 'wl-summary-register-as-good)
    (define-key map "N" 'wl-summary-register-as-good-all)
    (setq wl-summary-spam-map map)))

(eval-when-compile
  ;; Avoid compile warnings
  (defalias-maybe 'wl-summary-spam 'ignore))

(defun wl-summary-test-spam (&optional folder number)
  (interactive)
  (let ((folder (or folder wl-summary-buffer-elmo-folder))
	(number (or number (wl-summary-message-number)))
	spam)
    (message "Cheking spam...")
    (when (setq spam (elmo-spam-message-spam-p (elmo-spam-processor)
					       folder number))
      (wl-summary-spam number))
    (message "Cheking spam...done")
    (when (interactive-p)
      (message "No: %d is %sa spam message." number (if spam "" "not ")))))

(defun wl-summary-mark-spam (&optional all)
  "Set spam mark to messages which is spam classification."
  (interactive "P")
  (let (numbers)
    (if all
	(setq numbers wl-summary-buffer-number-list)
      (dolist (number wl-summary-buffer-number-list)
	(when (wl-spam-auto-check-message-p wl-summary-buffer-elmo-folder
					    number)
	  (setq numbers (cons number numbers)))))
    (cond (numbers
	   (wl-spam-map-spam-messages wl-summary-buffer-elmo-folder
				      numbers
				      #'wl-summary-spam))
	  ((interactive-p)
	   (message "No message to test.")))))

(defun wl-summary-register-as-spam ()
  (interactive)
  (let ((number (wl-summary-message-number)))
    (when number
      (wl-spam-register-spam-messages wl-summary-buffer-elmo-folder
				      (list number)))))

(defun wl-summary-register-as-spam-all ()
  (interactive)
  (wl-spam-register-spam-messages wl-summary-buffer-elmo-folder
				  wl-summary-buffer-number-list))

(defun wl-summary-target-mark-register-as-spam ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  wl-summary-buffer-disp-msg)
      (wl-spam-register-spam-messages wl-summary-buffer-elmo-folder
				      wl-summary-buffer-target-mark-list)
      (dolist (number wl-summary-buffer-target-mark-list)
	(wl-summary-unset-mark number)))))

(defun wl-summary-register-as-good ()
  (interactive)
  (let ((number (wl-summary-message-number)))
    (when number
      (wl-spam-register-good-messages wl-summary-buffer-elmo-folder
				      (list number)))))

(defun wl-summary-register-as-good-all ()
  (interactive)
  (wl-spam-register-good-messages wl-summary-buffer-elmo-folder
				  wl-summary-buffer-number-list))

(defun wl-summary-target-mark-register-as-good ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  wl-summary-buffer-disp-msg)
      (wl-spam-register-good-messages wl-summary-buffer-elmo-folder
				      wl-summary-buffer-target-mark-list)
      (dolist (number wl-summary-buffer-target-mark-list)
	(wl-summary-unset-mark number)))))

;; hook functions and other
(defun wl-summary-auto-check-spam ()
  (when (elmo-string-match-member (wl-summary-buffer-folder-name)
				  wl-spam-auto-check-folder-regexp-list)
    (wl-summary-mark-spam)))

(defun wl-summary-exec-action-spam (mark-list)
  (let ((domain (wl-spam-domain (elmo-folder-name-internal
				 wl-summary-buffer-elmo-folder)))
	(total (length mark-list)))
    (wl-folder-confirm-existence (elmo-make-folder wl-spam-folder))
    (when (memq domain '(undecided good))
      (message "Registering spam...")
      (elmo-with-progress-display (> total elmo-display-progress-threshold)
	  (elmo-spam-register total "Registering spam...")
	(elmo-spam-register-spam-messages (elmo-spam-processor)
					  wl-summary-buffer-elmo-folder
					  (mapcar #'car mark-list)
					  (eq domain 'good)))
      (message "Registering spam...done"))
    (wl-summary-move-mark-list-messages mark-list
					wl-spam-folder
					"Refiling spam...")))

(defun wl-summary-exec-action-refile-with-register (mark-list)
  (let* ((processor (elmo-spam-processor))
	 (folder wl-summary-buffer-elmo-folder)
	 (domain (wl-spam-domain (elmo-folder-name-internal folder)))
	 spam-list good-list total)
    (unless (eq domain 'ignore)
      (dolist (info mark-list)
	(case (wl-spam-domain (nth 2 info))
	  (spam
	   (setq spam-list (cons (car info) spam-list)))
	  (good
	   (setq good-list (cons (car info) good-list)))))
      (case domain
	(spam (setq spam-list nil))
	(good (setq good-list nil)))
      (when (or spam-list good-list)
	(when spam-list
	  (setq total (length spam-list))
	  (message "Registering spam...")
	  (elmo-with-progress-display (> total elmo-display-progress-threshold)
	      (elmo-spam-register total "Registering spam...")
	    (elmo-spam-register-spam-messages processor folder spam-list
					      (eq domain 'good)))
	  (message "Registering spam...done"))
	(when good-list
	  (setq total (length good-list))
	  (message "Registering good...")
	  (elmo-with-progress-display (> total elmo-display-progress-threshold)
	      (elmo-spam-register total "Registering good...")
	    (elmo-spam-register-good-messages processor folder good-list
					      (eq domain 'spam)))
	  (message "Registering good...done"))))
    ;; execute refile messages
    (wl-summary-exec-action-refile mark-list)))

(defun wl-message-check-spam ()
  (let ((original (wl-message-get-original-buffer))
	(number wl-message-buffer-cur-number)
	spam)
    (message "Cheking spam...")
    (when (setq spam (elmo-spam-buffer-spam-p (elmo-spam-processor) original))
      (with-current-buffer wl-message-buffer-cur-summary-buffer
	(wl-summary-spam number)))
    (message "Cheking spam...done")
    (message "No: %d is %sa spam message." number (if spam "" "not "))))

(defun wl-refile-guess-by-spam (entity)
  (when (elmo-spam-message-spam-p (elmo-spam-processor)
				  wl-summary-buffer-elmo-folder
				  (elmo-message-entity-number entity))
    wl-spam-folder))

(defun wl-spam-setup ()
  (add-hook 'wl-summary-sync-updated-hook #'wl-summary-auto-check-spam)
  (let ((actions wl-summary-mark-action-list)
	action)
    (while actions
      (setq action  (car actions)
	    actions (cdr actions))
      (when (eq (wl-summary-action-symbol action) 'refile)
	(setcar (nthcdr 4 action) 'wl-summary-exec-action-refile-with-register)
	(setq actions nil))))
  (when wl-spam-mark-action-list
    (setq wl-summary-mark-action-list (append
				       wl-summary-mark-action-list
				       wl-spam-mark-action-list))
    (dolist (action wl-spam-mark-action-list)
      (setq wl-summary-reserve-mark-list
	    (cons (wl-summary-action-mark action)
		  wl-summary-reserve-mark-list))
      (setq wl-summary-skip-mark-list
	    (cons (wl-summary-action-mark action)
		  wl-summary-skip-mark-list))))
  (define-key wl-summary-mode-map "k" wl-summary-spam-map)
  (define-key
    wl-summary-mode-map "mk" 'wl-summary-target-mark-spam)
  (define-key
    wl-summary-mode-map "ms" 'wl-summary-target-mark-register-as-spam)
  (define-key
    wl-summary-mode-map "mn" 'wl-summary-target-mark-register-as-good))

(require 'product)
(product-provide (provide 'wl-spam) (require 'wl-version))

(unless noninteractive
  (wl-spam-setup))

;;; wl-spam.el ends here
