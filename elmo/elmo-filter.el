;;; elmo-filter.el -- Filtered Folder Interface for ELMO.

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
(require 'elmo-msgdb)

(defun elmo-filter-msgdb-create (spec numlist new-mark already-mark
				      seen-mark important-mark seen-list)
  (if (eq (nth 2 spec) 'partial)
      (elmo-msgdb-create (nth 2 spec)
			 numlist
			 new-mark
			 already-mark
			 seen-mark important-mark seen-list)
    (elmo-msgdb-create-as-numlist (nth 2 spec)
				  numlist
				  new-mark
				  already-mark
				  seen-mark important-mark seen-list)))

(defun elmo-filter-msgdb-create-as-numlist (spec numlist new-mark already-mark
						 seen-mark important-mark
						 seen-list)
  (elmo-msgdb-create-as-numlist (nth 2 spec)
				numlist
				new-mark
				already-mark
				seen-mark important-mark seen-list))
  
(defun elmo-filter-list-folders (spec &optional hierarchy)
  nil)

(defun elmo-filter-append-msg (spec string &optional msg no-see)
  (elmo-call-func (nth 2 spec) "append" string))

(defun elmo-filter-read-msg (spec number outbuf &optional msgdb unread)
  (elmo-call-func (nth 2 spec) "read-msg" number outbuf msgdb unread))

(defun elmo-filter-delete-msgs (spec msgs)
  (elmo-call-func (nth 2 spec) "delete-msgs" msgs))

(defun elmo-filter-list-folder (spec)
  (elmo-search (nth 2 spec) (nth 1 spec)))

(defun elmo-filter-list-folder-unread (spec number-alist mark-alist
					    unread-marks)
  (elmo-list-filter
   (mapcar 'car number-alist)
   (elmo-list-folder-unread
    (nth 2 spec) number-alist mark-alist unread-marks)))

(defun elmo-filter-list-folder-important (spec number-alist)
  (elmo-list-filter
   (mapcar 'car number-alist)
   (elmo-list-folder-important (nth 2 spec) number-alist)))

(defun elmo-filter-folder-diff (spec folder &optional number-list)
  (if (or (elmo-multi-p folder)
	  (not (and (vectorp (nth 1 spec))
		    (string-match "^last$"
				  (elmo-filter-key (nth 1 spec))))))
      (cons nil (cdr (elmo-folder-diff (nth 2 spec))))
    (elmo-generic-folder-diff spec folder number-list)))

(defun elmo-filter-max-of-folder (spec)
  (elmo-max-of-folder (nth 2 spec)))

(defun elmo-filter-folder-exists-p (spec)
  (elmo-folder-exists-p (nth 2 spec)))

(defun elmo-filter-folder-creatable-p (spec)
  (elmo-call-func (nth 2 spec) "folder-creatable-p"))

(defun elmo-filter-create-folder (spec)
  (elmo-create-folder (nth 2 spec)))

(defun elmo-filter-search (spec condition &optional from-msgs)
  ;; search from messages in this folder
  (elmo-list-filter
   from-msgs
   (elmo-search (nth 2 spec) condition
		(elmo-filter-list-folder spec))))

(defun elmo-filter-use-cache-p (spec number)
  (elmo-call-func (nth 2 spec) "use-cache-p" number))

(defun elmo-filter-local-file-p (spec number)
  (elmo-call-func (nth 2 spec) "local-file-p" number))

(defun elmo-filter-commit (spec)
  (elmo-commit (nth 2 spec)))

(defun elmo-filter-plugged-p (spec)
  (elmo-folder-plugged-p (nth 2 spec)))

(defun elmo-filter-set-plugged (spec plugged add)
  (elmo-folder-set-plugged (nth 2 spec) plugged add))

(defun elmo-filter-get-msg-filename (spec number &optional loc-alist)
  ;; This function may be called when elmo-filter-local-file-p()
  ;; returns t.
  (elmo-call-func (nth 2 spec) "get-msg-filename" number loc-alist))

(defun elmo-filter-sync-number-alist (spec number-alist)
  (elmo-call-func (nth 2 spec) "sync-number-alist" number-alist))

(defun elmo-filter-server-diff (spec)
  (elmo-call-func (nth 2 spec) "server-diff"))

(require 'product)
(product-provide (provide 'elmo-filter) (require 'elmo-version))

;;; elmo-filter.el ends here
