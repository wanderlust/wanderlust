;;; elmo-null.el --- /dev/null Folder Interface for ELMO.  -*- lexical-binding: t -*-

;; Copyright (C) 2005 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
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
(require 'elmo)
(require 'elmo-nntp)

(eval-and-compile
  (luna-define-class elmo-null-folder (elmo-folder)))

(luna-define-method elmo-folder-initialize ((folder elmo-null-folder) _name)
  folder)

(luna-define-method elmo-folder-expand-msgdb-path ((_folder elmo-null-folder))
  (expand-file-name "null" (expand-file-name "internal" elmo-msgdb-directory)))

(luna-define-method elmo-folder-open ((_folder elmo-null-folder)
				      &optional _load-msgdb))

(luna-define-method elmo-folder-commit ((_folder elmo-null-folder)))

(luna-define-method elmo-folder-diff ((_folder elmo-null-folder))
  '(0 . 0))

(luna-define-method elmo-folder-list-messages ((_folder elmo-null-folder)
					       &optional _visible-only _in-msgdb)
  nil)

(luna-define-method elmo-folder-list-flagged ((_folder elmo-null-folder)
					      _flag &optional _in-msgdb)
  nil)

(luna-define-method elmo-folder-count-flags ((_folder elmo-null-folder))
  nil)

(luna-define-method elmo-folder-have-subfolder-p ((_folder elmo-null-folder))
  nil)

(luna-define-method elmo-folder-exists-p ((_folder elmo-null-folder))
  t)

(luna-define-method elmo-folder-writable-p ((_folder elmo-null-folder))
  t)

(defun elmo-folder-append-messages-*-null (_dst-folder
					   _src-folder
					   numbers
					   _same-number)
  (elmo-progress-notify 'elmo-folder-move-messages (length numbers))
  numbers)

(luna-define-method elmo-folder-synchronize ((_folder elmo-null-folder)
					     &optional
					     _disable-killed
					     _ignore-msgdb
					     _no-check
					     _mask)
  nil)

(require 'product)
(product-provide (provide 'elmo-null) (require 'elmo-version))

;;; elmo-null.el.el ends here
