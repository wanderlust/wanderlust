;;; elmo-pipe.el -- PIPE Interface for ELMO.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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

(defsubst elmo-pipe-spec-src (spec)
  (nth 1 spec))

(defsubst elmo-pipe-spec-dst (spec)
  (nth 2 spec))

(defalias 'elmo-pipe-msgdb-create 'elmo-pipe-msgdb-create-as-numlist)

(defun elmo-pipe-msgdb-create-as-numlist (spec numlist new-mark already-mark
					       seen-mark important-mark
					       seen-list)
  (elmo-msgdb-create-as-numlist (elmo-pipe-spec-dst spec)
				numlist new-mark already-mark
				seen-mark important-mark seen-list))

(defun elmo-pipe-list-folders (spec &optional hierarchy)
  nil)

(defun elmo-pipe-append-msg (spec string &optional msg no-see)
  (elmo-append-msg (elmo-pipe-spec-dst spec) string))

(defun elmo-pipe-read-msg (spec number outbuf)
  (elmo-call-func (elmo-pipe-spec-dst spec)
		  "read-msg"
		  number outbuf))

(defun elmo-pipe-delete-msgs (spec msgs)
  (elmo-delete-msgs (elmo-pipe-spec-dst spec) msgs))

(defvar elmo-pipe-drained-hook nil "A hook called when the pipe is flushed.")

(defun elmo-pipe-drain (src dst)
  (let ((msgdb (elmo-msgdb-load src))
	elmo-nntp-use-cache
	elmo-imap4-use-cache
	elmo-pop3-use-cache) ; Inhibit caching while moving messages.
    (message "Checking %s..." src)
    (elmo-move-msgs src (elmo-list-folder src) dst msgdb)
    (elmo-msgdb-save src msgdb)
    (run-hooks 'elmo-pipe-drained-hook)))

(defun elmo-pipe-list-folder (spec)
  (elmo-pipe-drain (elmo-pipe-spec-src spec)
		   (elmo-pipe-spec-dst spec))
  (elmo-list-folder (elmo-pipe-spec-dst spec)))

(defun elmo-pipe-list-folder-unread (spec mark-alist unread-marks)
  (elmo-list-folder-unread (elmo-pipe-spec-dst spec) mark-alist unread-marks))
  
(defun elmo-pipe-list-folder-important (spec overview)
  (elmo-list-folder-important (elmo-pipe-spec-dst spec) overview))

(defun elmo-pipe-max-of-folder (spec)
  (let ((src-length (length (elmo-list-folder (elmo-pipe-spec-src spec))))
	(dst-list (elmo-list-folder (elmo-pipe-spec-dst spec))))
    (cons (+ src-length (elmo-max-of-list dst-list))
	  (+ src-length (length dst-list)))))

(defun elmo-pipe-folder-exists-p (spec)
  (and (elmo-folder-exists-p (elmo-pipe-spec-src spec))
       (elmo-folder-exists-p (elmo-pipe-spec-dst spec))))

(defun elmo-pipe-folder-creatable-p (spec)
  (or (elmo-folder-creatable-p (elmo-pipe-spec-src spec))
      (elmo-folder-creatable-p (elmo-pipe-spec-dst spec))))

(defun elmo-pipe-create-folder (spec)
  (if (and (not (elmo-folder-exists-p (elmo-pipe-spec-src spec)))
	   (elmo-folder-creatable-p (elmo-pipe-spec-src spec)))
      (elmo-create-folder (elmo-pipe-spec-src spec)))
  (if (and (not (elmo-folder-exists-p (elmo-pipe-spec-dst spec)))
	   (elmo-folder-creatable-p (elmo-pipe-spec-dst spec)))
      (elmo-create-folder (elmo-pipe-spec-dst spec))))

(defun elmo-pipe-search (spec condition &optional numlist)
  (elmo-search (elmo-pipe-spec-dst spec) condition numlist))

(defun elmo-pipe-use-cache-p (spec number)
  (elmo-use-cache-p (elmo-pipe-spec-dst spec) number))

(defun elmo-pipe-commit (spec)
  (elmo-commit (elmo-pipe-spec-src spec))
  (elmo-commit (elmo-pipe-spec-dst spec)))

(defun elmo-pipe-plugged-p (spec)
  (and (elmo-folder-plugged-p (elmo-pipe-spec-src spec))
       (elmo-folder-plugged-p (elmo-pipe-spec-dst spec))))

(defun elmo-pipe-set-plugged (spec plugged add)
  (elmo-folder-set-plugged (elmo-pipe-spec-src spec) plugged add)
  (elmo-folder-set-plugged (elmo-pipe-spec-dst spec) plugged add))

(defun elmo-pipe-local-file-p (spec number)
  (elmo-local-file-p (elmo-pipe-spec-dst spec) number))

(defun elmo-pipe-get-msg-filename (spec number &optional loc-alist)
  (elmo-get-msg-filename (elmo-pipe-spec-dst spec) number loc-alist))

(defun elmo-pipe-sync-number-alist (spec number-alist)
  (elmo-call-func (elmo-pipe-spec-src spec)
		  "sync-number-alist" number-alist)) ; ??

(defun elmo-pipe-server-diff (spec)
  nil)

(provide 'elmo-pipe)

;;; elmo-pipe.el ends here
