;;; elmo-localdir.el --- Localdir Interface for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Copyright (C) 1999,2000      Kenichi OKADA  <okada@opaopa.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA <muse@ba2.so-net.ne.jp>
;;	Kenichi OKADA <okada@opaopa.org>
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
(eval-when-compile (require 'cl))

(require 'elmo-msgdb)
(require 'elmo)

(defcustom elmo-localdir-folder-path "~/Mail"
  "*Local mail directory (MH format) path."
  :type 'directory
  :group 'elmo)

(defvar elmo-localdir-lockfile-list nil)

;;; ELMO Local directory folder
(eval-and-compile
  (luna-define-class elmo-localdir-folder (elmo-folder)
		     (dir-name directory))
  (luna-define-internal-accessors 'elmo-localdir-folder))

;;; elmo-localdir specific methods.
(luna-define-generic elmo-localdir-folder-path (folder)
  "Return local directory path of the FOLDER.")

(luna-define-generic elmo-localdir-folder-name (folder name)
  "Return directory NAME for FOLDER.")

(luna-define-method elmo-localdir-folder-path ((folder elmo-localdir-folder))
  elmo-localdir-folder-path)

(luna-define-method elmo-localdir-folder-name ((folder elmo-localdir-folder)
					       name)
  name)

(luna-define-method elmo-folder-initialize ((folder
					     elmo-localdir-folder)
					    name)
  (elmo-localdir-folder-set-dir-name-internal folder name)
  (if (file-name-absolute-p name)
      (elmo-localdir-folder-set-directory-internal
       folder
       (expand-file-name name))
    (elmo-localdir-folder-set-directory-internal
     folder
     (expand-file-name
      (elmo-localdir-folder-name folder name)
      (elmo-localdir-folder-path folder))))
  folder)

;; open, check, commit, and close are generic.

(luna-define-method elmo-folder-exists-p ((folder elmo-localdir-folder))
  (file-directory-p (elmo-localdir-folder-directory-internal folder)))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-localdir-folder))
  (expand-file-name
   (mapconcat
    'identity
    (mapcar
     'elmo-replace-string-as-filename
     (split-string
      (let ((dir-name (elmo-localdir-folder-dir-name-internal folder)))
	(if (file-name-absolute-p dir-name)
	    (expand-file-name dir-name)
	  dir-name))
      "/"))
    "/")
   (expand-file-name ;;"localdir"
    (symbol-name (elmo-folder-type-internal folder))
    elmo-msgdb-directory)))

(luna-define-method elmo-message-file-name ((folder
					     elmo-localdir-folder)
					    number)
  (expand-file-name (int-to-string number)
		    (elmo-localdir-folder-directory-internal folder)))

(luna-define-method elmo-folder-message-file-number-p ((folder
							elmo-localdir-folder))
  t)

(luna-define-method elmo-folder-message-file-directory ((folder
							 elmo-localdir-folder))
  (elmo-localdir-folder-directory-internal folder))

(luna-define-method elmo-folder-message-make-temp-file-p
  ((folder elmo-localdir-folder))
  t)

(luna-define-method elmo-folder-message-make-temp-files ((folder
							  elmo-localdir-folder)
							 numbers
							 &optional
							 start-number)
  (let ((temp-dir (elmo-folder-make-temporary-directory folder))
	(cur-number (or start-number 0)))
    (dolist (number numbers)
      (elmo-copy-file
       (expand-file-name
	(int-to-string number)
	(elmo-localdir-folder-directory-internal folder))
       (expand-file-name
	(int-to-string (if start-number cur-number number))
	temp-dir))
      (incf cur-number))
    temp-dir))

(defun elmo-localdir-msgdb-create-entity (dir number)
  (elmo-msgdb-create-overview-entity-from-file
   number (expand-file-name (int-to-string number) dir)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-localdir-folder)
					      numbers
					      flag-table)
  (when numbers
    (let ((dir (elmo-localdir-folder-directory-internal folder))
	  overview number-alist mark-alist entity message-id
	  num gmark
	  (i 0)
	  (len (length numbers)))
      (message "Creating msgdb...")
      (while numbers
	(setq entity
	      (elmo-localdir-msgdb-create-entity
	       dir (car numbers)))
	(if (null entity)
	    ()
	  (setq num (elmo-msgdb-overview-entity-get-number entity))
	  (setq overview
		(elmo-msgdb-append-element
		 overview entity))
	  (setq message-id (elmo-msgdb-overview-entity-get-id entity))
	  (setq number-alist
		(elmo-msgdb-number-add number-alist
				       num
				       message-id))
	  (if (setq gmark (or (elmo-msgdb-global-mark-get message-id)
			      (elmo-msgdb-mark
			       (elmo-flag-table-get flag-table message-id)
			       (elmo-file-cache-status
				(elmo-file-cache-get message-id))
			       'new)))
	      (setq mark-alist
		    (elmo-msgdb-mark-append
		     mark-alist
		     num
		     gmark))))
	(when (> len elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (elmo-display-progress
	   'elmo-localdir-msgdb-create-as-numbers "Creating msgdb..."
	   (/ (* i 100) len)))
	(setq numbers (cdr numbers)))
      (message "Creating msgdb...done")
      (list overview number-alist mark-alist))))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-localdir-folder)
						 &optional one-level)
  (elmo-mapcar-list-of-list
   (lambda (x) (concat (elmo-folder-prefix-internal folder) x))
   (elmo-list-subdirectories
    (elmo-localdir-folder-path folder)
    (or (elmo-localdir-folder-dir-name-internal folder) "")
    one-level)))

(defsubst elmo-localdir-list-subr (folder &optional nonsort)
  (let ((flist (mapcar 'string-to-int
		       (directory-files
			(elmo-localdir-folder-directory-internal folder)
			nil "^[0-9]+$" t)))
	(killed (elmo-msgdb-killed-list-load (elmo-folder-msgdb-path folder))))
    (if nonsort
	(cons (or (elmo-max-of-list flist) 0)
	      (if killed
		  (- (length flist)
		     (elmo-msgdb-killed-list-length killed))
		(length flist)))
      (sort flist '<))))

(luna-define-method elmo-folder-append-buffer ((folder elmo-localdir-folder)
					       unread
					       &optional number)
  (let ((filename (elmo-message-file-name
		   folder
		   (or number
		       (1+ (car (elmo-folder-status folder)))))))
    (when (file-writable-p filename)
      (write-region-as-binary
       (point-min) (point-max) filename nil 'no-msg)
      t)))

(luna-define-method elmo-folder-append-messages :around
  ((folder elmo-localdir-folder)
   src-folder numbers &optional same-number)
  (if (elmo-folder-message-file-p src-folder)
      (let ((dir (elmo-localdir-folder-directory-internal folder))
	    (succeeds numbers)
	    (next-num (1+ (car (elmo-folder-status folder)))))
	(while numbers
	  (elmo-copy-file
	   (elmo-message-file-name src-folder (car numbers))
	   (expand-file-name
	    (int-to-string
	     (if same-number (car numbers) next-num))
	    dir))
	  (elmo-progress-notify 'elmo-folder-move-messages)
	  (if (and (setq numbers (cdr numbers))
		   (not same-number))
	      (setq next-num
		    (if (elmo-localdir-locked-p)
			;; MDA is running.
			(1+ (car (elmo-folder-status folder)))
		      (1+ next-num)))))
	succeeds)
    (luna-call-next-method)))

(luna-define-method elmo-folder-delete-messages ((folder elmo-localdir-folder)
						 numbers)
  (dolist (number numbers)
    (elmo-localdir-delete-message folder number))
  t)

(defun elmo-localdir-delete-message (folder number)
  "Delete message in the FOLDER with NUMBER."
  (let ((filename (elmo-message-file-name folder number)))
    (when (and (string-match "[0-9]+" filename) ; for safety.
	       (file-exists-p filename)
	       (file-writable-p filename)
	       (not (file-directory-p filename)))
      (delete-file filename)
      t)))

(luna-define-method elmo-message-fetch-internal ((folder elmo-localdir-folder)
						 number strategy
						 &optional section unread)
  (when (file-exists-p (elmo-message-file-name folder number))
    (insert-file-contents-as-binary
     (elmo-message-file-name folder number))))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-localdir-folder) &optional nohide)
  (elmo-localdir-list-subr folder))

(luna-define-method elmo-folder-status ((folder elmo-localdir-folder))
  (elmo-localdir-list-subr folder t))

(luna-define-method elmo-folder-creatable-p ((folder elmo-localdir-folder))
  t)

(luna-define-method elmo-folder-writable-p ((folder elmo-localdir-folder))
  t)

(luna-define-method elmo-folder-create ((folder elmo-localdir-folder))
  (let ((dir (elmo-localdir-folder-directory-internal folder)))
    (if (file-directory-p dir)
	()
      (if (file-exists-p dir)
	  (error "Create folder failed")
	(elmo-make-directory dir))
      t)))

(luna-define-method elmo-folder-delete :before ((folder elmo-localdir-folder))
  (let ((dir (elmo-localdir-folder-directory-internal folder)))
    (if (not (file-directory-p dir))
	(error "No such directory: %s" dir)
      (elmo-delete-match-files dir "[0-9]+" t)
      t)))

(luna-define-method elmo-folder-rename-internal ((folder elmo-localdir-folder)
						 new-folder)
  (let* ((old (elmo-localdir-folder-directory-internal folder))
	 (new (elmo-localdir-folder-directory-internal new-folder))
	 (new-dir (directory-file-name (file-name-directory new))))
    (unless (file-directory-p old)
      (error "No such directory: %s" old))
    (when (file-exists-p new)
      (error "Already exists directory: %s" new))
    (unless (file-directory-p new-dir)
      (elmo-make-directory new-dir))
    (rename-file old new)
    t))

(defsubst elmo-localdir-field-condition-match (folder condition
						      number number-list)
  (elmo-file-field-condition-match
   (expand-file-name (int-to-string number)
		     (elmo-localdir-folder-directory-internal folder))
   condition number number-list))

(luna-define-method elmo-folder-pack-numbers ((folder elmo-localdir-folder))
  (let* ((dir (elmo-localdir-folder-directory-internal folder))
	 (msgdb (elmo-folder-msgdb folder))
	 (onum-alist (elmo-msgdb-get-number-alist msgdb))
	 (omark-alist (elmo-msgdb-get-mark-alist msgdb))
	 (new-number 1)			; first ordinal position in localdir
	 flist onum mark new-mark-alist total)
    (setq flist
	  (if elmo-pack-number-check-strict
	      (elmo-folder-list-messages folder) ; allow localnews
	    (mapcar 'car onum-alist)))
    (setq total (length flist))
    (while flist
      (when (> total elmo-display-progress-threshold)
	(elmo-display-progress
	 'elmo-folder-pack-numbers "Packing..."
	 (/ (* new-number 100) total)))
      (setq onum (car flist))
      (when (not (eq onum new-number))		; why \=() is wrong..
	(elmo-bind-directory
	 dir
	 ;; xxx  nfs,hardlink
	 (rename-file (int-to-string onum) (int-to-string new-number) t))
	;; update overview
	(elmo-msgdb-overview-entity-set-number
	 (elmo-msgdb-overview-get-entity onum msgdb)
	 new-number)
	;; update number-alist
	(and (assq onum onum-alist)
	     (setcar (assq onum onum-alist) new-number)))
      ;; update mark-alist
      (when (setq mark (cadr (assq onum omark-alist)))
	(setq new-mark-alist
	      (elmo-msgdb-mark-append
	       new-mark-alist
	       new-number mark)))
      (setq new-number (1+ new-number))
      (setq flist (cdr flist)))
    (message "Packing...done")
    (elmo-folder-set-msgdb-internal
     folder
     (elmo-make-msgdb
      (elmo-msgdb-get-overview msgdb)
      onum-alist
      new-mark-alist))))

(luna-define-method elmo-folder-message-file-p ((folder elmo-localdir-folder))
  t)

(luna-define-method elmo-message-file-name ((folder elmo-localdir-folder)
					    number)
  (expand-file-name
   (int-to-string number)
   (elmo-localdir-folder-directory-internal folder)))

(defun elmo-localdir-locked-p ()
  (if elmo-localdir-lockfile-list
      (let ((lock elmo-localdir-lockfile-list))
	(catch 'found
	  (while lock
	    (if (file-exists-p (car lock))
		(throw 'found t))
	    (setq lock (cdr lock)))))))

(require 'product)
(product-provide (provide 'elmo-localdir) (require 'elmo-version))

;;; elmo-localdir.el ends here
