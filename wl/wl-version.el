;;; wl-version.el -- Version information for Wanderlust.

;; Copyright 2000 Yuuichi Teranishi <teranisi@gohome.org>

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
(require 'elmo-version)
(require 'product)
(provide 'wl-version)

(product-provide 'wl-version
  (product-define
   "Wanderlust" nil
   (product-version (product-find 'elmo-version)) ; equals to ELMO version.
   "Roam"))

(defun wl-version (&optional with-codename)
  "Print Wanderlust version."
  (interactive)
  (product-string-1 'wl-version with-codename))

(defun wl-version-show ()
  (interactive)
  (message "%s" (wl-version t)))


;; for backward compatibility
(defconst wl-appname  (product-name (product-find 'wl-version)))
(make-obsolete-variable
 'wl-appname
 "use (product-name (product-find 'wl-version)) insteaed.")

(defconst wl-version
  (progn (product-string-1 'wl-version)	; for product-set-version-string
	 (product-version-string (product-find 'wl-version))))
(make-obsolete-variable
 'wl-version
 "use (product-version-string (product-find 'wl-version)) instead.")

(defconst wl-codename (product-code-name (product-find 'wl-version)))
(make-obsolete-variable
 'wl-codename
 "use (product-code-name (product-find 'wl-version)) instead.")

;; wl-version.el ends here
