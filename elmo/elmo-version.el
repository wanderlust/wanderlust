;;; elmo-version.el -- Version information for ELMO.

;; Copyright 2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

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
(provide 'elmo-version)

(product-provide 'elmo-version
  (product-define "ELMO" nil '(2 3 90)))

;; set version-string
(if (fboundp 'product-version-as-string)
    (product-version-as-string 'elmo-version)
  (product-string-1 'elmo-version))

(defun elmo-version ()
  "Return ELMO version."
  (product-string-1 'elmo-version))


;; for backward compatibility
(defconst elmo-appname (product-name (product-find 'elmo-version)))
(make-obsolete-variable
 'elmo-appname
 "use (product-name (product-find 'elmo-version)) insteaed.")

(defconst elmo-version (product-version-string (product-find 'elmo-version)))
(make-obsolete-variable
 'elmo-version
 "use (product-version-string (product-find 'elmo-version)) instead.")

;;; elmo-version.el ends here
