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

(defgroup wl-spam nil
  "Spam configuration for wanderlust."
  :group 'wl)

(defcustom wl-spam-folder-name "+spam"
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

(defcustom wl-spam-auto-check-policy-alist '(("inbox" . mark))
  "*Alist of Folder regexp which check spam automatically and policy."
  :type '(repeat (cons (regexp :tag "Folder Regexp")
		       (choice (const :tag "Target mark" mark)
			       (const :tag "Refile mark" refile)
			       (const :tag "none" nil))))
  :group 'wl-spam)


(defun wl-spam-folder-guess-domain (folder-name)
  (cond ((string= folder-name wl-spam-folder-name)
	 'spam)
	((wl-string-match-member folder-name
				 wl-spam-undecided-folder-regexp-list)
	 'undecided)
	((wl-string-match-member folder-name
				 wl-spam-ignored-folder-regexp-list)
	 'ignore)
	(t
	 'good)))

(defsubst wl-spam-map-spam-messages (folder numbers function &rest args)
  (let ((total (length numbers)))
    (elmo-with-progress-display (> total elmo-display-progress-threshold)
	(elmo-spam-check-spam total "Checking spam...")
      (dolist (number (elmo-spam-list-spam-messages (elmo-spam-processor)
						    folder
						    numbers))
	(apply function number args)))
    (message "Checking spam...done")))

;; insinuate into summary mode
(defvar wl-summary-spam-map nil)

(unless wl-summary-spam-map
  (let ((map (make-sparse-keymap)))
    (define-key map "*" 'wl-summary-target-mark-spam)
    (define-key map "o" 'wl-summary-refile-spam)
    (define-key map "s" 'wl-summary-register-as-spam)
    (define-key map "S" 'wl-summary-register-as-spam-all)
    (define-key map "n" 'wl-summary-register-as-good)
    (define-key map "N" 'wl-summary-register-as-good-all)
    (setq wl-summary-spam-map map)
    (define-key wl-summary-mode-map "k" wl-summary-spam-map)))

(eval-when-compile
  ;; Avoid compile warnings
  (defalias-maybe 'wl-summary-target-mark 'ignore)
  (defalias-maybe 'wl-summary-refile-mark 'ignore))

(defun wl-summary-target-mark-spam (&optional folder)
  "Set target mark to messages which is guessed spam in FOLDER."
  (interactive)
  (wl-spam-map-spam-messages (or folder wl-summary-buffer-elmo-folder)
			     wl-summary-buffer-number-list
			     #'wl-summary-target-mark))

(defun wl-summary-refile-spam (&optional folder)
  "Set refile mark to messages which is guessed spam in FOLDER."
  (interactive)
  (wl-spam-map-spam-messages (or folder wl-summary-buffer-elmo-folder)
			     wl-summary-buffer-number-list
			     #'wl-summary-refile
			     wl-spam-folder-name))

(defun wl-summary-register-as-spam (&optional all)
  (interactive "P")
  (let ((numbers (if all
		     wl-summary-buffer-number-list
		   (list (wl-summary-message-number)))))
    (elmo-spam-register-spam-messages (elmo-spam-processor)
				      wl-summary-buffer-elmo-folder
				      numbers)))

(defun wl-summary-register-as-spam-all ()
  (interactive)
  (wl-summary-register-as-spam 'all))

(defun wl-summary-register-as-good (&optional all)
  (interactive "P")
  (let ((numbers (if all
		     wl-summary-buffer-number-list
		   (list (wl-summary-message-number)))))
    (elmo-spam-register-good-messages (elmo-spam-processor)
				      wl-summary-buffer-elmo-folder
				      numbers)))

(defun wl-summary-register-as-good-all ()
  (interactive)
  (wl-summary-register-as-good 'all))

;; hook functions and other
(defun wl-summary-auto-check-spam ()
  (case (cdr (elmo-string-matched-assoc (wl-summary-buffer-folder-name)
					wl-spam-auto-check-policy-alist))
    (mark
     (wl-summary-target-mark-spam))
    (refile
     (wl-summary-refile-spam))))

(defun wl-summary-exec-action-refile-with-register (mark-list)
  (let ((processor (elmo-spam-processor))
	(folder wl-summary-buffer-elmo-folder)
	spam-list good-list)
    (when (eq (wl-spam-folder-guess-domain
	       (elmo-folder-name-internal folder))
	      'undecided)
      (dolist (info mark-list)
	(case (wl-spam-folder-guess-domain (nth 2 info))
	  (spam
	   (setq spam-list (cons (car info) spam-list)))
	  (good
	   (setq good-list (cons (car info) good-list)))))
      (let ((total (+ (length spam-list) (length good-list))))
	(elmo-with-progress-display (> total elmo-display-progress-threshold)
	    (elmo-spam-register total "Register spam...")
	  (when spam-list
	    (elmo-spam-register-spam-messages processor folder spam-list))
	  (when good-list
	    (elmo-spam-register-good-messages processor folder good-list)))
	(message "Register spam...done")))
    ;; execute refile messages
    (wl-summary-exec-action-refile mark-list)))

(defun wl-refile-guess-by-spam (entity)
  (when (elmo-spam-message-spam-p (elmo-spam-processor)
				  wl-summary-buffer-elmo-folder
				  (elmo-message-entity-number entity))
    wl-spam-folder-name))

(require 'product)
(product-provide (provide 'wl-spam) (require 'wl-version))

;;; wl-sapm.el ends here
