;;; wl-version.el -- Version information for Wanderlust.

;; Copyright (C) 2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000 TAKAHASHI Kaoru <kaoru@kaisei.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	TAKAHASHI Kaoru <kaoru@kaisei.org>
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
(require 'product)
(require 'elmo-version)			; product-version-as-string
(provide 'wl-version)			; before product-provide

;; product-define in the first place
(product-provide 'wl-version
  (product-define
   "Wanderlust" nil
   (eval-when-compile
     (product-version (product-find 'elmo-version))) ; equals to ELMO version.
   "Roam"))

;; set version-string
(product-version-as-string 'wl-version)

;; require wl-util after product-provide.
(eval-when-compile (require 'wl-util))	; wl-match-string

(defun wl-version (&optional with-codename)
  "Return Wanderlust version.
If WITH-CODENAME add codename."
  (product-string-1 'wl-version with-codename))

(defun wl-version-show (&optional arg)
  "Print Wanderlust version.
If ARG insert string at point."
  (interactive "P")
  (if arg
      (insert (message "%s" (wl-version t)))
    (message "%s" (wl-version t))))

(defvar wl-version-status-alist
  '(((eq (% (nth 1 (product-version (product-find 'wl-version))) 2) 0)
     . "stable")
    (t . "beta"))
  "An alist to define the version status.")

(defun wl-version-status ()
  "Return version status (\"stable\" or \"beta\")."
  (let ((salist wl-version-status-alist)
	status)
    (while salist
      (when (eval (car (car salist)))
	(setq status (cdr (car salist)))
	(setq salist nil))
      (setq salist (cdr salist)))
    status))

;; compile warning
(defvar mule-version)
(defvar nemacs-version)
(defvar emacs-beta-version)
(defvar xemacs-codename)
(defvar mime-edit-insert-user-agent-field)
(defvar mime-edit-user-agent-value)
(defvar mime-editor/version)
(defvar mime-editor/codename)

(defun wl-generate-user-agent-string ()
  "A candidate of `wl-generate-mailer-string-func'.
Insert User-Agent field instead of X-Mailer field."
  (concat "User-Agent: "
	  (wl-generate-user-agent-string-1
	   (or (and (boundp 'mime-edit-insert-user-agent-field)
		    mime-edit-insert-user-agent-field)
	       (and (boundp 'mime-editor/version)
		    mime-editor/version)))))

(defun wl-generate-user-agent-string-1 (&optional verbose)
  "Return User-Agent field value.
If VERBOSE return with SEMI, FLIM and APEL version."
  (if (not verbose)
      ;; Don't use product-string-verbose for short User-Agent field value.
      (concat (product-string-1 'wl-version t) " "
	      (wl-extended-emacs-version3 "/" t))
    ;; verbose
    (cond
     ;; SEMI
     ((and (boundp 'mime-edit-user-agent-value) mime-edit-user-agent-value)
      (concat (product-string-verbose 'wl-version) " "
	      mime-edit-user-agent-value))
     ;; tm
     ((and (boundp 'mime-editor/version) mime-editor/version)
      (concat (product-string-verbose 'wl-version) " "
	      "tm/" mime-editor/version
	      (if (and (boundp 'mime-editor/codename)
		       mime-editor/codename)
		  (concat " (" mime-editor/codename ")"))
	      (if (and (boundp 'mime-library-product)
		       mime-library-product)
		  (concat " " (aref mime-library-product 0)
			  "/"
			  (mapconcat 'int-to-string
				     (aref mime-library-product 1)
				     ".")
			  " (" (aref mime-library-product 2) ")"))
	      (condition-case nil
		  (progn
		    (require 'apel-ver)
		    (concat " " (apel-version)))
		(file-error nil))
	      " " (wl-extended-emacs-version3 "/" t)))
     ;; error case
     (t
      (product-string-1 'wl-version nil)))))

;; from gnus
(defun wl-extended-emacs-version (&optional with-codename)
  "Stringified Emacs version.
If WITH-CODENAME add XEmacs codename."
  (cond
   ((string-match "^\\([0-9]+\\.[0-9]+\\)\\.[.0-9]+$" emacs-version)
    (concat "Emacs " (wl-match-string 1 emacs-version)
	    (and (boundp 'mule-version)(concat "/Mule " mule-version))))
   ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		  emacs-version)
    (concat (wl-match-string 1 emacs-version)
	    (format " %d.%d" emacs-major-version emacs-minor-version)
	    (if (and (boundp 'emacs-beta-version)
		     emacs-beta-version)
		(format "b%d" emacs-beta-version))
	    (if with-codename
		(if (boundp 'xemacs-codename)
		    (concat " - \"" xemacs-codename "\"")))))
   (t emacs-version)))

(defun wl-extended-emacs-version2 (&optional delimiter with-codename)
  "Stringified Emacs version.
Separate DELIMITER (default is \" \").  If WITH-CODENAME add XEmacs codename."
  (cond
   ((and (boundp 'mule-version)
	 mule-version
	 (string-match "\\([0-9]+\.[0-9]+\\)\\(.*$\\)" mule-version))
    (format "Mule%s%s@%d.%d%s"
	    (or delimiter " ")
	    (wl-match-string 1 mule-version)
	    emacs-major-version
	    emacs-minor-version
	    (if with-codename
		(wl-match-string 2 mule-version)
	      "")))
   ((string-match "^\\([0-9]+\\.[0-9]+\\)\\.[.0-9]+$" emacs-version)
    (if (boundp 'nemacs-version)
	(concat "Nemacs" (or delimiter " ")
		nemacs-version
		"@"
		(substring emacs-version
			   (match-beginning 1)
			   (match-end 1)))
      (concat "Emacs" (or delimiter " ")
	      (wl-match-string 1 emacs-version))))
   ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		  emacs-version)
    (concat (wl-match-string 1 emacs-version)
	    (or delimiter " ")
	    (format "%d.%d" emacs-major-version emacs-minor-version)
	    (if (and (boundp 'emacs-beta-version)
		     emacs-beta-version)
		(format "b%d" emacs-beta-version))
	    (if (and with-codename
		     (boundp 'xemacs-codename)
		     xemacs-codename)
		(format " (%s)" xemacs-codename))))
   (t emacs-version)))

(defun wl-extended-emacs-version3 (&optional delimiter with-codename)
  "Stringified Emacs version.
Separate DELIMITER (default is \" \").  If WITH-CODENAME add XEmacs codename."
  (cond
   ((and (boundp 'mule-version)
	 mule-version
	 (string-match "\\([0-9]+\.[0-9]+\\)\\(.*$\\)" mule-version))
    (format "Emacs%s%d.%d Mule%s%s%s"
	    (or delimiter " ")
	    emacs-major-version
	    emacs-minor-version
	    (or delimiter " ")
	    (wl-match-string 1 mule-version)
	    (if with-codename
		(wl-match-string 2 mule-version)
	      "")))
   ((string-match "^\\([0-9]+\\.[0-9]+\\)\\.[.0-9]+$" emacs-version)
    (if (boundp 'nemacs-version)
	(let ((nemacs-codename-assoc '(("3.3.2" . " (FUJIMUSUME)")
				       ("3.3.1" . " (HINAMATSURI)")
				       ("3.2.3" . " (YUMENO-AWAYUKI)"))))
	  (format "Emacs%s%s Nemacs%s%s%s"
		  (or delimiter " ")
		  (wl-match-string 1 emacs-version)
		  (or delimiter " ")
		  nemacs-version
		  (or (and with-codename
			   (cdr (assoc nemacs-version
				       nemacs-codename-assoc)))
		      "")))
      (concat "Emacs" (or delimiter " ")
	      (wl-match-string 1 emacs-version))))
   ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		  emacs-version)
    (concat (wl-match-string 1 emacs-version)
	    (or delimiter " ")
	    (format "%d.%d" emacs-major-version emacs-minor-version)
	    (if (and (boundp 'emacs-beta-version)
		     emacs-beta-version)
		(format "b%d" emacs-beta-version))
	    (if (and with-codename
		     (boundp 'xemacs-codename)
		     xemacs-codename)
		(format " (%s)" xemacs-codename))))
   (t emacs-version)))


;; for backward compatibility
(defconst wl-appname (product-name (product-find 'wl-version)))
(make-obsolete-variable
 'wl-appname
 "use (product-name (product-find 'wl-version)) insteaed.")

(defconst wl-version (product-version-string (product-find 'wl-version)))
(make-obsolete-variable
 'wl-version
 "use (product-version-string (product-find 'wl-version)) instead.")

(defconst wl-codename (product-code-name (product-find 'wl-version)))
(make-obsolete-variable
 'wl-codename
 "use (product-code-name (product-find 'wl-version)) instead.")

;;; wl-version.el ends here
