;;; elsp-bogofilter.el --- Bogofilter support for elmo-spam.

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
(require 'elmo-spam)
(require 'luna)

(defgroup elmo-spam-bogofilter nil
  "Spam bogofilter configuration."
  :group 'elmo-spam)

(defcustom elmo-spam-bogofilter-program "bogofilter"
  "*Program name of the Bogofilter."
  :type '(string :tag "Program name of the bogofilter")
  :group 'elmo-spam-bogofilter)

(defcustom elmo-spam-bogofilter-args nil
  "*Argument list for bogofilter."
  :type '(repeat string)
  :group 'elmo-spam-bogofilter)

(defcustom elmo-spam-bogofilter-database-directory nil
  "*Directory path of the Bogofilter databases."
  :type '(choice (directory :tag "Location of the Bogofilter database directory")
		 (const :tag "Use the default"))
  :group 'elmo-spam-bogofilter)

(defcustom elmo-spam-bogofilter-arguments-alist
  '((classify . ((if register "-u")
		 (if elmo-spam-bogofilter-database-directory
		     (list "-d" elmo-spam-bogofilter-database-directory))))
    (register . ((if spam "-s" "-n")
		 (if restore (if spam "-N" "-S"))
		 (if elmo-spam-bogofilter-database-directory
		     (list "-d" elmo-spam-bogofilter-database-directory)))))
  "*An alist of options that are used with call bogofilter process.
Each element is a list of following:
\(TYPE . LIST-EXP)
TYPE is a symbol from `classify' or `register'.
LIST-EXP is an expression to get list of options."
  :type '(repeat (cons (choice (const :tag "Classify" classify)
			       (const :tag "Register" register))
		       (repeat sexp)))
  :group 'elmo-spam-bogofilter)

(defcustom elmo-spam-bogofilter-debug nil
  "Non-nil to debug elmo bogofilter spam backend."
  :type 'boolean
  :group 'elmo-spam-bogofilter)


(eval-and-compile
  (luna-define-class elsp-bogofilter (elsp-generic)))

(defsubst elmo-spam-bogofilter-call (&rest args)
  (apply #'call-process-region
	 (point-min) (point-max)
	 elmo-spam-bogofilter-program
	 nil (if elmo-spam-bogofilter-debug
		 (get-buffer-create "*Debug ELMO SPAM Bogofilter*"))
	 nil
	 (delq nil (append elmo-spam-bogofilter-args
			   (elmo-flatten args)))))

(defmacro elmo-spam-bogofilter-arguments (type)
  `(mapcar #'eval
	   (cdr (assq ,type elmo-spam-bogofilter-arguments-alist))))

(luna-define-method elmo-spam-buffer-spam-p ((processor elsp-bogofilter)
					     buffer &optional register)
  (with-current-buffer buffer
    (= 0 (elmo-spam-bogofilter-call
	  (elmo-spam-bogofilter-arguments 'classify)))))

(defsubst elsp-bogofilter-register-buffer (buffer spam restore)
  (with-current-buffer buffer
    (elmo-spam-bogofilter-call
     (elmo-spam-bogofilter-arguments 'register))))

(luna-define-method elmo-spam-register-spam-buffer ((processor elsp-bogofilter)
						    buffer &optional restore)
  (elsp-bogofilter-register-buffer buffer t restore))

(luna-define-method elmo-spam-register-good-buffer ((processor elsp-bogofilter)
						    buffer &optional restore)
  (elsp-bogofilter-register-buffer buffer nil restore))

(require 'product)
(product-provide (provide 'elsp-bogofilter) (require 'elmo-version))

;;; elsp-bogofilter.el ends here
