;;; elmo-filter.el --- Filtered Folder Interface for ELMO.

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
(require 'elmo)

;;; ELMO filter folder
(eval-and-compile
  (luna-define-class elmo-filter-folder (elmo-folder)
		     (condition target require-msgdb))
  (luna-define-internal-accessors 'elmo-filter-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-filter-folder)
					    name)
  (let (pair)
    (setq pair (elmo-parse-search-condition name))
    (elmo-filter-folder-set-condition-internal folder
					       (car pair))
    (if (string-match "^ */\\(.*\\)$" (cdr pair))
	(elmo-filter-folder-set-target-internal
	 folder
	 (elmo-make-folder (elmo-match-string 1 (cdr pair))))
      (error "Folder syntax error `%s'" (elmo-folder-name-internal folder)))
    (elmo-filter-folder-set-require-msgdb-internal
     folder
     (elmo-folder-search-requires-msgdb-p
      (elmo-filter-folder-target-internal folder)
      (elmo-filter-folder-condition-internal folder)))
    folder))

(luna-define-method elmo-folder-open-internal ((folder elmo-filter-folder))
  (elmo-folder-open-internal (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-msgdb :around ((folder elmo-filter-folder))
  ;; Load target's msgdb if required.
  (if (elmo-filter-folder-require-msgdb-internal folder)
      (elmo-folder-msgdb (elmo-filter-folder-target-internal folder)))
  ;; Load msgdb of itself.
  (luna-call-next-method))

(luna-define-method elmo-folder-check ((folder elmo-filter-folder))
  (if (elmo-filter-folder-require-msgdb-internal folder)
      (elmo-folder-synchronize (elmo-filter-folder-target-internal folder))))

(luna-define-method elmo-folder-close-internal ((folder elmo-filter-folder))
  (elmo-folder-close-internal (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-close :after ((folder elmo-filter-folder))
  ;; Clear target msgdb if it is used.
  (if (elmo-filter-folder-require-msgdb-internal folder)
      (elmo-folder-set-msgdb-internal (elmo-filter-folder-target-internal
				       folder) nil)))

(luna-define-method elmo-folder-set-message-modified ((folder
						       elmo-filter-folder)
						      modified)
  (if (elmo-filter-folder-require-msgdb-internal folder)
      (elmo-folder-set-message-modified-internal
       (elmo-filter-folder-target-internal folder) modified)
    (elmo-folder-set-message-modified-internal folder modified)))

(luna-define-method elmo-folder-commit :around ((folder elmo-filter-folder))
  ;; Save target msgdb if it is used.
  (if (elmo-filter-folder-require-msgdb-internal folder)
      (elmo-folder-commit (elmo-filter-folder-target-internal folder)))
  (luna-call-next-method))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-filter-folder))
  (expand-file-name
   (elmo-replace-string-as-filename (elmo-folder-name-internal folder))
   (expand-file-name "filter" elmo-msgdb-directory)))

(luna-define-method elmo-folder-newsgroups ((folder elmo-filter-folder))
  (elmo-folder-newsgroups (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-find-fetch-strategy
  ((folder elmo-filter-folder) entity &optional ignore-cache)
  (elmo-find-fetch-strategy
   (elmo-filter-folder-target-internal folder)
   entity ignore-cache))

(luna-define-method elmo-folder-get-primitive-list ((folder
						     elmo-filter-folder))
  (list (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-contains-type ((folder elmo-filter-folder)
					       type)
  (elmo-folder-contains-type
   (elmo-filter-folder-target-internal folder)
   type))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-filter-folder)
					      numlist flag-table)
  (if (elmo-filter-folder-require-msgdb-internal folder)
      (let* ((target-folder (elmo-filter-folder-target-internal folder))
	     (len (length numlist))
	     (msgdb (elmo-folder-msgdb target-folder))
	     (new-msgdb (elmo-make-msgdb))
	     message-id entity)
	(when (> len elmo-display-progress-threshold)
	  (elmo-progress-set 'elmo-folder-msgdb-create
			     len "Creating msgdb..."))
	(unwind-protect
	    (dolist (number numlist)
	      (setq entity (elmo-msgdb-message-entity msgdb number))
	      (when entity
		(elmo-msgdb-append-entity new-msgdb entity
					  (elmo-msgdb-flags msgdb number)))
	      (elmo-progress-notify 'elmo-folder-msgdb-create))
	  (elmo-progress-clear 'elmo-folder-msgdb-create))
	new-msgdb)
    ;; Does not require msgdb.
    (elmo-folder-msgdb-create
     (elmo-filter-folder-target-internal folder)
     numlist flag-table)))

(luna-define-method elmo-folder-append-buffer ((folder elmo-filter-folder)
					       &optional flag number)
  (elmo-folder-append-buffer
   (elmo-filter-folder-target-internal folder)
   flag number))

(luna-define-method elmo-message-fetch ((folder elmo-filter-folder)
					number strategy
					&optional section outbuf unseen)
  (elmo-message-fetch
   (elmo-filter-folder-target-internal folder)
   number strategy section outbuf unseen))

(luna-define-method elmo-folder-delete-messages ((folder elmo-filter-folder)
						 numbers)
  (elmo-folder-delete-messages
   (elmo-filter-folder-target-internal folder) numbers))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-filter-folder) &optional nohide)
  (let ((target (elmo-filter-folder-target-internal folder)))
    (if (or (elmo-folder-plugged-p target)
	    (not (elmo-folder-persistent-p folder)))
	;; search target folder
	(elmo-folder-search
	 target
	 (elmo-filter-folder-condition-internal folder))
      ;; not available
      t)))

(defsubst elmo-filter-folder-list-unreads (folder)
  (elmo-list-filter
   (elmo-folder-list-messages folder nil 'in-msgdb)
   (elmo-folder-list-unreads
    (elmo-filter-folder-target-internal folder))))

(luna-define-method elmo-folder-list-unreads :around ((folder
						       elmo-filter-folder))
  (if (elmo-filter-folder-require-msgdb-internal folder)
      (elmo-filter-folder-list-unreads folder)
    (luna-call-next-method)))

(defsubst elmo-filter-folder-list-importants (folder)
  (elmo-list-filter
   (elmo-folder-list-messages folder nil 'in-msgdb)
   (elmo-folder-list-importants
    (elmo-filter-folder-target-internal folder))))

(luna-define-method elmo-folder-list-importants :around ((folder
							  elmo-filter-folder))
  (if (elmo-filter-folder-require-msgdb-internal folder)
      (elmo-filter-folder-list-importants folder)
    (luna-call-next-method)))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-filter-folder)
						 &optional one-level)
  (let* ((target (elmo-filter-folder-target-internal folder))
	 (prefix (and (string-match
		       (concat "^\\(.*\\)"
			       (regexp-quote
				(elmo-folder-name-internal
				 target))
			       "$")
		       (elmo-folder-name-internal folder))
		      (match-string 1 (elmo-folder-name-internal
				       folder)))))
    (elmo-mapcar-list-of-list
     (lambda (x) (concat prefix x))
     (elmo-folder-list-subfolders target one-level))))

(luna-define-method elmo-folder-diff :around ((folder elmo-filter-folder))
  (let ((condition (elmo-filter-folder-condition-internal folder))
	diff)
    (if (vectorp condition)
	(cond
	 ((and (string= (elmo-filter-key condition) "flag")
	       (or (string= (elmo-filter-value condition) "any")
		   (string= (elmo-filter-value condition) "digest")
		   (string= (elmo-filter-value condition) "unread")))
	  (setq diff (elmo-folder-diff (elmo-filter-folder-target-internal
					folder)))
	  (if (consp diff)
	      (cons (car diff) (car diff))
	    (cons (car diff) (nth 1 diff))))
	 ((string= "last" (elmo-filter-key condition))
	  (luna-call-next-method))
	 (t
	  (cons nil (cdr (elmo-folder-diff (elmo-filter-folder-target-internal
					    folder))))))
      (luna-call-next-method))))

(luna-define-method elmo-folder-status ((folder elmo-filter-folder))
  (elmo-folder-status
   (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-exists-p ((folder elmo-filter-folder))
  (elmo-folder-exists-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-creatable-p ((folder elmo-filter-folder))
  (elmo-folder-creatable-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-writable-p ((folder elmo-filter-folder))
  (elmo-folder-writable-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-create ((folder elmo-filter-folder))
  (elmo-folder-create (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-search ((folder elmo-filter-folder)
					condition &optional numbers)
  ;; search from messages in this folder
  (elmo-list-filter
   numbers
   (elmo-folder-search (elmo-filter-folder-target-internal folder)
		       condition
		       (elmo-folder-list-messages folder))))

(luna-define-method elmo-message-use-cache-p ((folder elmo-filter-folder)
					      number)
  (elmo-message-use-cache-p (elmo-filter-folder-target-internal folder)
			    number))

(luna-define-method elmo-folder-message-file-p ((folder elmo-filter-folder))
  (elmo-folder-message-file-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-plugged-p ((folder elmo-filter-folder))
  (elmo-folder-plugged-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-set-plugged ((folder elmo-filter-folder)
					     plugged &optional add)
  (elmo-folder-set-plugged (elmo-filter-folder-target-internal folder)
			   plugged add))

(luna-define-method elmo-message-file-name ((folder elmo-filter-folder)
					    number)
  (elmo-message-file-name (elmo-filter-folder-target-internal folder)
			  number))

(luna-define-method elmo-folder-mark-as-read :around ((folder
						       elmo-filter-folder)
						      numbers
						      &optional ignore-flag)
  (elmo-folder-mark-as-read (elmo-filter-folder-target-internal folder)
			    numbers ignore-flag)
  (luna-call-next-method))

(luna-define-method elmo-folder-unmark-read :around ((folder
						      elmo-filter-folder)
						     numbers
						     &optional ignore-flag)
  (elmo-folder-unmark-read (elmo-filter-folder-target-internal folder)
			   numbers ignore-flag)
  (luna-call-next-method))

(luna-define-method elmo-folder-mark-as-important :around ((folder
							    elmo-filter-folder)
							   numbers
							   &optional
							   ignore-flag)
  (elmo-folder-mark-as-important (elmo-filter-folder-target-internal folder)
				 numbers ignore-flag)
    (luna-call-next-method))

(luna-define-method elmo-folder-unmark-important :around ((folder
							   elmo-filter-folder)
							  numbers
							  &optional
							  ignore-flag)
  (elmo-folder-unmark-important (elmo-filter-folder-target-internal folder)
				numbers ignore-flag)
  (luna-call-next-method))

(luna-define-method elmo-folder-mark-as-answered :around ((folder
							   elmo-filter-folder)
							  numbers)
  (elmo-folder-mark-as-answered (elmo-filter-folder-target-internal folder)
				numbers)
  (luna-call-next-method))


(luna-define-method elmo-folder-unmark-answered :around ((folder
							  elmo-filter-folder)
							 numbers)
  (elmo-folder-unmark-answered (elmo-filter-folder-target-internal folder)
			       numbers)
  (luna-call-next-method))

(require 'product)
(product-provide (provide 'elmo-filter) (require 'elmo-version))

;;; elmo-filter.el ends here
