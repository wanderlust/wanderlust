;;; elmo-cache2.el -- Cache Folder Interface for ELMO.

;; Author: Kenichi OKADA <okada@opaopa.org>
;; Keywords: mail, net news
;; Time-stamp: <00/03/01 09:58:07 teranisi>

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

(require 'emu)
(require 'std11)

(require 'elmo-cache)
(require 'elmo-msgdb)

(defsubst elmo-cache-get-folder-directory (spec)
  (if (file-name-absolute-p (nth 1 spec))
      (nth 1 spec) ; already full path.
    (expand-file-name (nth 1 spec)
		      (expand-file-name elmo-cache-dirname elmo-msgdb-dir))))

(defun elmo-cache-msgdb-expand-path (spec)
  (let ((fld-name (nth 1 spec)))
    (expand-file-name fld-name
		      (expand-file-name "internal/cache"
					elmo-msgdb-dir))))

(defun elmo-cache-number-to-filename (spec number)
  (let ((number-alist
	 (elmo-cache-list-folder-subr spec nil t)))
    (elmo-msgid-to-cache
     (cdr (assq number number-alist)))))

(if (boundp 'nemacs-version)
    (defsubst elmo-cache-insert-header (file)
      "Insert the header of the article (Does not work on nemacs)."
      (as-binary-input-file
       (insert-file-contents file)))
  (defsubst elmo-cache-insert-header (file)
    "Insert the header of the article."
    (let ((beg 0)
	  insert-file-contents-pre-hook   ; To avoid autoconv-xmas...
	  insert-file-contents-post-hook
	  format-alist)
      (when (file-exists-p file)
	;; Read until header separator is found.
	(while (and (eq elmo-localdir-header-chop-length
			(nth 1 
			     (as-binary-input-file 
			      (insert-file-contents
			       file nil beg
			       (incf beg elmo-localdir-header-chop-length)))))
		    (prog1 (not (search-forward "\n\n" nil t))
		      (goto-char (point-max)))))))))

(defsubst elmo-cache-msgdb-create-overview-entity-from-file (number file)
  (save-excursion
    (let ((tmp-buffer (get-buffer-create " *ELMO Cache Temp*"))
	  insert-file-contents-pre-hook   ; To avoid autoconv-xmas...
	  insert-file-contents-post-hook header-end
	  (attrib (file-attributes file))
	  ret-val size mtime)
      (set-buffer tmp-buffer)
      (erase-buffer)
      (if (not (file-exists-p file))
	  ()
	(setq size (nth 7 attrib))
	(setq mtime (timezone-make-date-arpa-standard
		     (current-time-string (nth 5 attrib)) (current-time-zone)))
	;; insert header from file.
	(catch 'done
	  (condition-case nil
	      (elmo-cache-insert-header file)
	    (error (throw 'done nil)))
	  (goto-char (point-min))
	  (setq header-end
		(if (re-search-forward "\\(^--.*$\\)\\|\\(\n\n\\)" nil t)
		    (point)
		  (point-max)))
	  (narrow-to-region (point-min) header-end)
	  (setq ret-val (elmo-msgdb-create-overview-from-buffer number size mtime))
	  (kill-buffer tmp-buffer))
	ret-val))))

(defun elmo-cache-msgdb-create-as-numlist (spec numlist new-mark
						   already-mark seen-mark
						   important-mark seen-list)
  (when numlist
    (let ((dir (elmo-cache-get-folder-directory spec))
	  (nalist (elmo-cache-list-folder-subr spec nil t))
	  overview number-alist mark-alist entity message-id
	  i percent len num seen gmark)
      (setq len (length numlist))
      (setq i 0)
      (message "Creating msgdb...")
      (while numlist
	(setq entity
	      (elmo-cache-msgdb-create-overview-entity-from-file
	       (car numlist)
	       (expand-file-name
		(elmo-msgid-to-cache
		 (setq message-id (cdr (assq (car numlist) nalist)))) dir)))
	(if (null entity)
	    ()
	  (setq num (elmo-msgdb-overview-entity-get-number entity))
	  (setq overview
		(elmo-msgdb-append-element
		 overview entity))
	  (setq number-alist
		(elmo-msgdb-number-add number-alist num message-id))
	  (setq seen (member message-id seen-list))
	  (if (setq gmark (or (elmo-msgdb-global-mark-get message-id)
			      (if seen
				  nil
				new-mark)))
	      (setq mark-alist
		    (elmo-msgdb-mark-append 
		     mark-alist 
		     num
		     gmark))))
	(setq i (1+ i))
	(setq percent (/ (* i 100) len))
	(message "Creating msgdb...%d%%" percent)
	(setq numlist (cdr numlist)))
      (message "Creating msgdb...done.")
      (list overview number-alist mark-alist))))

(defalias 'elmo-cache-msgdb-create 'elmo-cache-msgdb-create-as-numlist)

(defun elmo-cache-list-folders (spec &optional hierarchy)
  (let ((folder (concat "!" (nth 1 spec))))
    (elmo-cache-list-folders-subr folder hierarchy)))

(defun elmo-cache-list-folders-subr (folder &optional hierarchy)
  (let ((case-fold-search t)
	folders curdir dirent relpath abspath attr
	subprefix subfolder)
    (condition-case ()
	(progn
	  (setq curdir
		(expand-file-name (nth 1 (elmo-folder-get-spec folder))
				  (expand-file-name elmo-cache-dirname elmo-msgdb-dir)))
	  (if (string-match "^[+=$!]$" folder) ;; localdir, archive, localnews
	      (setq subprefix folder)
	    (setq subprefix (concat folder elmo-path-sep))
	    ;; include parent
	    (setq folders (list folder)))
	  (setq dirent (directory-files curdir nil "^[01][0-9A-F]$"))
	  (catch 'done
	   (while dirent
	    (setq relpath (car dirent))
	    (setq dirent (cdr dirent))
	    (setq abspath (expand-file-name relpath curdir))
	    (and
	     (eq (nth 0 (setq attr (file-attributes abspath))) t)
	     (setq subfolder (concat subprefix relpath))
	     (setq folders (nconc folders (list subfolder))))))
	  folders)
      (file-error folders))))

(defsubst elmo-cache-list-folder-subr (spec &optional nonsort nonalist)
  (let* ((dir (elmo-cache-get-folder-directory spec))
	 (flist (directory-files dir nil "^[^@]+@[^@]+$" t))
	 (folder (concat "!" (nth 1 spec)))
	 (number-alist (or (elmo-msgdb-number-load (elmo-msgdb-expand-path folder))
		       (list nil)))
	 nlist)
    (setq nlist
	  (mapcar '(lambda (filename)
		     (elmo-cache-filename-to-number filename number-alist))
		  flist))
    (if nonalist
	number-alist
      (if nonsort
	  (cons (or (elmo-max-of-list nlist) 0) (length nlist))
	(sort nlist '<)))))

(defsubst elmo-cache-filename-to-number (filename number-alist)
  (let* ((msgid (elmo-cache-to-msgid filename))
	 number)
    (or (car (rassoc msgid number-alist))
	(prog1
	    (setq number (+ (or (caar (last number-alist))
				0) 1))
	  (if (car number-alist)
	      (nconc number-alist
		     (list (cons number msgid)))
	    (setcar number-alist (cons number msgid)))))))

(defun elmo-cache-append-msg (spec string message-id &optional msg no-see)
  (let ((dir (elmo-cache-get-folder-directory spec))
	(tmp-buffer (get-buffer-create " *ELMO Temp buffer*"))
	filename)
    (save-excursion
      (set-buffer tmp-buffer)
      (erase-buffer)
      (setq filename (expand-file-name (elmo-msgid-to-cache message-id) dir))
      (unwind-protect
	  (if (file-writable-p filename)
	      (progn
		(insert string)
		(as-binary-output-file
		 (write-region (point-min) (point-max) filename nil 'no-msg))
		t)
	    nil)
	(kill-buffer tmp-buffer)))))

(defun elmo-cache-delete-msg (spec number)
  (let* ((dir (elmo-cache-get-folder-directory spec))
	 (file (expand-file-name
	       (elmo-cache-number-to-filename spec number) dir)))
    (if (and (file-exists-p file)
	     (file-writable-p file)
	     (not (file-directory-p file)))
	(progn (delete-file file)
	       t))))

(defun elmo-cache-read-msg (spec number outbuf &optional set-mark)
  (save-excursion
    (let* ((dir (elmo-cache-get-folder-directory spec))
	   (file (expand-file-name 
		  (elmo-cache-number-to-filename spec number) dir)))
      (set-buffer outbuf)
      (erase-buffer)
      (when (file-exists-p file)
	(as-binary-input-file (insert-file-contents file))
	(elmo-delete-cr-get-content-type)))))

(defun elmo-cache-delete-msgs (spec msgs)
  (mapcar '(lambda (msg) (elmo-cache-delete-msg spec msg))
	  msgs))

(defun elmo-cache-list-folder (spec); called by elmo-cache-search()
  (elmo-cache-list-folder-subr spec))

(defun elmo-cache-max-of-folder (spec)
  (elmo-cache-list-folder-subr spec t))

(defun elmo-cache-check-validity (spec validity-file)
  t)

(defun elmo-cache-sync-validity (spec validity-file)
  t)

(defun elmo-cache-folder-exists-p (spec)
  (file-directory-p (elmo-cache-get-folder-directory spec)))

(defun elmo-cache-folder-creatable-p (spec)
  nil)

(defun elmo-cache-create-folder (spec)
  nil)

(defun elmo-cache-search (spec condition &optional from-msgs)
  (let* ((number-alist (elmo-cache-list-folder-subr spec nil t))
	 (msgs (or from-msgs (mapcar 'car number-alist)))
	 (num (length msgs))
	 (i 0) case-fold-search ret-val)
    (while msgs
      (if (elmo-file-field-condition-match
	   (expand-file-name 
	    (elmo-msgid-to-cache
	     (cdr (assq (car msgs) number-alist)))
	    (elmo-cache-get-folder-directory spec))
					    condition)
	  (setq ret-val (cons (car msgs) ret-val)))
      (setq i (1+ i))
      (message "Searching...%d%%" (/ (* i 100) num))
      (setq msgs (cdr msgs)))
    (nreverse ret-val)))

;;; (localdir, maildir, localnews) -> cache
(defun elmo-cache-copy-msgs (dst-spec msgs src-spec
				      &optional loc-alist same-number)
  (let ((dst-dir
	 (elmo-cache-get-folder-directory dst-spec))
	(next-num (1+ (car (elmo-cache-list-folder-subr dst-spec t))))
	(number-alist
	 (elmo-msgdb-number-load
	  (elmo-msgdb-expand-path nil src-spec))))
    (if same-number (error "Not implemented"))
    (while msgs
      (elmo-copy-file
       ;; src file
       (elmo-call-func src-spec "get-msg-filename" (car msgs) loc-alist)
       ;; dst file
       (expand-file-name
	(elmo-msgid-to-cache
	 (cdr (assq (if same-number (car msgs) next-num) number-alist)))
	 dst-dir))
      (if (and (setq msgs (cdr msgs))
	       (not same-number))
	  (setq next-num (1+ next-num))))
    t))

(defun elmo-cache-use-cache-p (spec number)
  nil)

(defun elmo-cache-local-file-p (spec number)
  t)

(defun elmo-cache-get-msg-filename (spec number &optional loc-alist)
  (expand-file-name
   (elmo-cache-number-to-filename spec number)
   (elmo-cache-get-folder-directory spec)))

(defalias 'elmo-cache-sync-number-alist 
  'elmo-generic-sync-number-alist)
(defalias 'elmo-cache-list-folder-unread 
  'elmo-generic-list-folder-unread)
(defalias 'elmo-cache-list-folder-important
  'elmo-generic-list-folder-important)
(defalias 'elmo-cache-commit 'elmo-generic-commit)

(provide 'elmo-cache2)

;;; elmo-cache2.el ends here
