;;; elmo-database.el --- Database module for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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
(require 'elmo-vars)

(defvar elmo-database-msgid nil)
(defvar elmo-database-msgid-filename "msgid")

(defun elmo-database-get (dbsym dbname)
  (if (not (and (symbol-value dbsym)
		(database-live-p (symbol-value dbsym))))
      (set dbsym (open-database (expand-file-name
				 dbname
				 elmo-msgdb-directory
				 )))
    (symbol-value dbsym)))

(defun elmo-database-close ()
  (and elmo-database-msgid
       (database-live-p elmo-database-msgid)
       (close-database elmo-database-msgid)))

(defun elmo-database-msgid-put (msgid folder number)
  (let ((db (elmo-database-get 'elmo-database-msgid
			       elmo-database-msgid-filename))
	print-length)
    (and msgid db
	 (progn
	   (remove-database msgid db)
	   (put-database msgid (prin1-to-string
				(list folder number)) db)))))

(defun elmo-database-msgid-delete (msgid)
  (remove-database msgid (elmo-database-get
			  'elmo-database-msgid
			  elmo-database-msgid-filename)))

(defun elmo-database-msgid-get (msgid)
  (let ((match (get-database msgid (elmo-database-get
				    'elmo-database-msgid
				    elmo-database-msgid-filename))))
    (and match (read match))))

(require 'product)
(product-provide (provide 'elmo-database) (require 'elmo-version))

;;; elmo-database.el ends here
