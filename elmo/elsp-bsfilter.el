;;; elsp-bsfilter.el --- Bsfilter support for elmo-spam.

;; Copyright (C) 2004 Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Copyright (C) 2004 Yuuichi Teranishi <teranisi@gohome.org>

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
(require 'elmo-spam)
(require 'luna)

(defgroup elmo-spam-bsfilter nil
  "Spam bsfilter configuration."
  :group 'elmo-spam)

(defcustom elmo-spam-bsfilter-shell-program "ruby"
  "*"
  :type 'string
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-shell-switch nil
  "*"
  :type 'string
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-program (exec-installed-p "bsfilter")
  "*Program name of the Bsfilter."
  :type '(string :tag "Program name of the bsfilter")
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-args nil
  "*Argument list for bsfilter."
  :type '(repeat string)
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-update-switch "--synchronous-auto-update"
  "*The switch that Bsfilter uses to update database with classify."
  :type 'string
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-database-directory nil
  "*Directory path of the Bsfilter databases."
  :type '(choice (directory :tag "Location of the Bsfilter database directory")
		 (const :tag "Use the default"))
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-debug nil
  "Non-nil to debug elmo bsfilter spam backend."
  :type 'boolean
  :group 'elmo-spam-bsfilter-debug)

(eval-and-compile
  (luna-define-class elsp-bsfilter (elsp-generic)))

(defsubst elsp-bsfilter-call-bsfilter (&rest args)
  (apply #'call-process-region
	 (point-min) (point-max)
	 elmo-spam-bsfilter-shell-program
	 nil (if elmo-spam-bsfilter-debug
		 (get-buffer-create "*Debug ELMO Bsfilter*"))
	 nil (delq nil
		   (append (list elmo-spam-bsfilter-shell-switch
				 elmo-spam-bsfilter-program)
			   elmo-spam-bsfilter-args
			   (elmo-flatten args)))))

(luna-define-method elmo-spam-buffer-spam-p ((processor elsp-bsfilter)
					     buffer &optional register)
  (with-current-buffer buffer
    (= 0 (elsp-bsfilter-call-bsfilter
	  (if register elmo-spam-bsfilter-update-switch)
	  (if elmo-spam-bsfilter-database-directory
	      (list "--homedir" elmo-spam-bsfilter-database-directory))))))

(defsubst elsp-bsfilter-register-buffer (buffer spam restore)
  (with-current-buffer buffer
    (elsp-bsfilter-call-bsfilter
     "--update"
     (if elmo-spam-bsfilter-database-directory
	 (list "--homedir" elmo-spam-bsfilter-database-directory))
     (if restore (if spam "--sub-clean" "--sub-spam"))
     (if spam "--add-spam" "--add-clean"))))

(luna-define-method elmo-spam-register-spam-buffer ((processor elsp-bsfilter)
						    buffer &optional restore)
  (elsp-bsfilter-register-buffer buffer t restore))

(luna-define-method elmo-spam-register-good-buffer ((processor elsp-bsfilter)
						    buffer &optional restore)
  (elsp-bsfilter-register-buffer buffer nil restore))

(require 'product)
(product-provide (provide 'elsp-bsfilter) (require 'elmo-version))

;;; elsp-bsfilter.el ends here
