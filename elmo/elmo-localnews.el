;;; elmo-localnews.el -- Local News Spool Interface for ELMO.

;; Copyright 1998,1999,2000 OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;;                          Yuuichi Teranishi <teranisi@gohome.org>

;; Author:  OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
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
(require 'elmo-localdir)

(defmacro elmo-localnews-as-newsdir (&rest body)
  (` (let ((elmo-localdir-folder-path elmo-localnews-folder-path))
       (,@ body))))

(defun elmo-localnews-msgdb-create-as-numlist (spec numlist new-mark
						    already-mark seen-mark
						    important-mark seen-list)
  (when numlist
    (elmo-localnews-as-newsdir
     (elmo-localdir-msgdb-create-as-numlist spec numlist new-mark
					    already-mark seen-mark
					    important-mark seen-list))))

(defalias 'elmo-localnews-msgdb-create 'elmo-localnews-msgdb-create-as-numlist)

(defun elmo-localnews-list-folders (spec &optional hierarchy)
  (let ((folder (concat "=" (nth 1 spec))))
    (elmo-localnews-as-newsdir
     (elmo-localdir-list-folders-subr folder hierarchy))))

(defun elmo-localnews-append-msg (spec string &optional msg no-see)
  (elmo-localnews-as-newsdir
   (elmo-localdir-append-msg spec string)))

(defun elmo-localnews-delete-msgs (dir number)
  (elmo-localnews-as-newsdir
   (elmo-localdir-delete-msgs dir number)))

(defun elmo-localnews-read-msg (spec number outbuf)
  (elmo-localnews-as-newsdir
   (elmo-localdir-read-msg spec number outbuf)))

(defun elmo-localnews-list-folder (spec)
  (elmo-localnews-as-newsdir
   (elmo-localdir-list-folder-subr spec)))

(defun elmo-localnews-max-of-folder (spec)
  (elmo-localnews-as-newsdir
   (elmo-localdir-list-folder-subr spec t)))

(defun elmo-localnews-check-validity (spec validity-file)
  (elmo-localnews-as-newsdir
   (elmo-localdir-check-validity spec validity-file)))

(defun elmo-localnews-sync-validity (spec validity-file)
  (elmo-localnews-as-newsdir
   (elmo-localdir-sync-validity spec validity-file)))

(defun elmo-localnews-folder-exists-p (spec)
  (elmo-localnews-as-newsdir
   (elmo-localdir-folder-exists-p spec)))

(defun elmo-localnews-folder-creatable-p (spec)
  t)

(defun elmo-localnews-create-folder (spec)
  (elmo-localnews-as-newsdir
   (elmo-localdir-create-folder spec)))

(defun elmo-localnews-delete-folder (spec)
  (elmo-localnews-as-newsdir
   (elmo-localdir-delete-folder spec)))

(defun elmo-localnews-rename-folder (old-spec new-spec)
  (elmo-localnews-as-newsdir
   (elmo-localdir-rename-folder old-spec new-spec)))

(defun elmo-localnews-search (spec condition &optional from-msgs)
  (elmo-localnews-as-newsdir
   (elmo-localdir-search spec condition from-msgs)))

(defun elmo-localnews-copy-msgs (dst-spec msgs src-spec
					  &optional loc-alist same-number)
  (elmo-localdir-copy-msgs
   dst-spec msgs src-spec loc-alist same-number))

(defun elmo-localnews-pack-number (spec msgdb arg)
  (elmo-localnews-as-newsdir
   (elmo-localdir-pack-number spec msgdb arg)))

(defun elmo-localnews-use-cache-p (spec number)
  nil)

(defun elmo-localnews-local-file-p (spec number)
  t)

(defun elmo-localnews-get-msg-filename (spec number &optional loc-alist)
  (elmo-localnews-as-newsdir
   (elmo-localdir-get-msg-filename spec number loc-alist)))

(defalias 'elmo-localnews-sync-number-alist 'elmo-generic-sync-number-alist)
(defalias 'elmo-localnews-list-folder-unread
  'elmo-generic-list-folder-unread)
(defalias 'elmo-localnews-list-folder-important
  'elmo-generic-list-folder-important)
(defalias 'elmo-localnews-commit 'elmo-generic-commit)
(defalias 'elmo-localnews-folder-diff 'elmo-generic-folder-diff)

(require 'product)
(product-provide (provide 'elmo-localnews) (require 'elmo-version))

;;; elmo-localnews.el ends here
