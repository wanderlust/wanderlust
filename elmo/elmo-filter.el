;;; elmo-filter.el -- Filtered Folder Interface for ELMO.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news
;; Time-stamp: <00/07/10 17:55:56 teranisi>

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

(defun elmo-filter-read-msg (spec number outbuf)
  (elmo-call-func (nth 2 spec) "read-msg" number outbuf))

(defun elmo-filter-delete-msgs (spec msgs)
  (elmo-call-func (nth 2 spec) "delete-msgs" msgs))

(defun elmo-filter-list-folder (spec)
  (let ((filter (nth 1 spec))
	(folder (nth 2 spec))
	msgs)
    (cond 
     ((vectorp filter)
      (cond ((string= (elmo-filter-key filter)
		      "last")
	     (setq msgs (elmo-list-folder folder))
	     (nthcdr (max (- (length msgs) 
			     (string-to-int (elmo-filter-value filter)))
			  0)
		     msgs))
	    ((string= (elmo-filter-key filter)
		      "first")
	     (setq msgs (elmo-list-folder folder))
	     (let ((rest (nthcdr (string-to-int (elmo-filter-value filter) )
				 msgs)))
	       (mapcar '(lambda (x) 
			  (delete x msgs)) rest))
	     msgs)))
     ((listp filter)
      (elmo-search folder filter)))))

(defun elmo-filter-list-folder-unread (spec mark-alist unread-marks)
  (let ((filter (nth 1 spec))
	(folder (nth 2 spec))
	msgs pair)
    (cond 
     ((vectorp filter)
      (cond ((string= (elmo-filter-key filter)
		      "last")
	     (setq msgs (elmo-list-folder-unread folder mark-alist 
						 unread-marks))
	     (nthcdr (max (- (length msgs) 
			     (string-to-int (elmo-filter-value filter)))
			  0)
		     msgs))
	    ((string= (elmo-filter-key filter)
		      "first")
	     (setq msgs (elmo-list-folder-unread folder
						 mark-alist
						 unread-marks))
	     (let ((rest (nthcdr (string-to-int (elmo-filter-value filter) )
				 msgs)))
	       (mapcar '(lambda (x) 
			  (delete x msgs)) rest))
	     msgs)))
     ((listp filter)
      (elmo-list-filter 
       (elmo-search folder filter)
       (elmo-list-folder-unread folder mark-alist unread-marks))))))

(defun elmo-filter-list-folder-important (spec overview)
  (let ((filter (nth 1 spec))
	(folder (nth 2 spec))
	msgs pair)
    (cond 
     ((vectorp filter)
      (cond ((string= (elmo-filter-key filter)
		      "last")
	     (setq msgs (elmo-list-folder-important folder overview))
	     (nthcdr (max (- (length msgs) 
			     (string-to-int (elmo-filter-value filter)))
			  0)
		     msgs))
	    ((string= (elmo-filter-key filter)
		      "first")
	     (setq msgs (elmo-list-folder-important folder overview))
	     (let ((rest (nthcdr (string-to-int (elmo-filter-value filter) )
				 msgs)))
	       (mapcar '(lambda (x) 
			  (delete x msgs)) rest))
	     msgs)))
     ((listp filter)
      (elmo-list-filter 
       (mapcar
	'(lambda (x) (elmo-msgdb-overview-entity-get-number x))
	overview)
       (elmo-list-folder-important folder overview))))))

(defun elmo-filter-max-of-folder (spec)
  (elmo-max-of-folder (nth 2 spec)))

(defun elmo-filter-folder-exists-p (spec)
  (elmo-folder-exists-p (nth 2 spec)))

(defun elmo-filter-folder-creatable-p (spec)
  (elmo-call-func (nth 2 spec) "folder-creatable-p"))

(defun elmo-filter-create-folder (spec)
  (elmo-create-folder (nth 2 spec)))

(defun elmo-filter-search (spec condition &optional numlist)
  ;; search from messages in this folder
  (elmo-list-filter 
   numlist 
   (elmo-call-func (nth 2 spec) "search" condition 
		   (elmo-filter-list-folder spec))))

(defun elmo-filter-use-cache-p (spec number)
  (elmo-call-func (nth 2 spec) "use-cache-p" number))

(defun elmo-filter-local-file-p (spec number)
  (elmo-call-func (nth 2 spec) "local-file-p" number))

(defun elmo-filter-commit (spec)
  (elmo-commit (nth 2 spec)))

(defun elmo-filter-clear-killed (spec)
  (elmo-clear-killed (nth 2 spec)))

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

(provide 'elmo-filter)

;;; elmo-filter.el ends here
