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
(require 'elmo)

;;; ELMO filter folder
(eval-and-compile
  (luna-define-class elmo-filter-folder (elmo-folder)
		     (condition target))
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
    folder))

(luna-define-method elmo-folder-open-internal ((folder elmo-filter-folder))
  (elmo-folder-open-internal (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-check ((folder elmo-filter-folder))
  (elmo-folder-check (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-close-internal ((folder elmo-filter-folder))
  (elmo-folder-close-internal (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-filter-folder))
  (expand-file-name
   (elmo-replace-string-as-filename (elmo-folder-name-internal folder))
   (expand-file-name "filter" elmo-msgdb-dir)))

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
					      numlist new-mark already-mark
					      seen-mark important-mark
					      seen-list)
  (elmo-folder-msgdb-create (elmo-filter-folder-target-internal folder)
			    numlist
			    new-mark
			    already-mark
			    seen-mark important-mark seen-list))

(luna-define-method elmo-folder-append-buffer ((folder elmo-filter-folder)
					       unread &optional number)
  (elmo-folder-append-buffer
   (elmo-filter-folder-target-internal folder)
   unread number))

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
  (elmo-folder-search (elmo-filter-folder-target-internal folder)
		      (elmo-filter-folder-condition-internal folder)))

(defsubst elmo-filter-folder-list-unreads-internal (folder unread-marks
							   mark-alist)
  (let ((unreads (elmo-folder-list-unreads-internal
		  (elmo-filter-folder-target-internal folder)
		  unread-marks mark-alist)))
    (unless (listp unreads)
      (setq unreads
	    (delq nil
		  (mapcar
		   (function
		    (lambda (x)
		      (if (member (cadr x) unread-marks)
			  (car x))))
		   (elmo-msgdb-get-mark-alist (elmo-folder-msgdb folder))))))
    (elmo-list-filter
     (mapcar 'car (elmo-msgdb-get-number-alist
		   (elmo-folder-msgdb folder)))
     unreads)))

(luna-define-method elmo-folder-list-unreads-internal
  ((folder elmo-filter-folder)
   unread-marks &optional mark-alist)
  (elmo-filter-folder-list-unreads-internal folder unread-marks mark-alist))

(defsubst elmo-filter-folder-list-importants-internal (folder important-mark)
  (let ((importants (elmo-folder-list-importants-internal
		     (elmo-filter-folder-target-internal folder)
		     important-mark)))
    (if (listp importants)
	(elmo-list-filter
	 (mapcar 'car (elmo-msgdb-get-number-alist
		       (elmo-folder-msgdb folder)))
	 importants)
      t)))

(luna-define-method elmo-folder-list-importants-internal
  ((folder elmo-filter-folder)
   important-mark)
  (elmo-filter-folder-list-importants-internal folder important-mark))

(luna-define-method elmo-folder-diff :around ((folder elmo-filter-folder)
					      &optional numbers)
  (if (not (and (vectorp (elmo-filter-folder-condition-internal
			  folder))
		(string-match "^last$"
			      (elmo-filter-key
			       (elmo-filter-folder-condition-internal
				folder)))))
      (cons nil (cdr (elmo-folder-diff (elmo-filter-folder-target-internal
					folder))))
    (luna-call-next-method)))

(luna-define-method elmo-folder-status ((folder elmo-filter-folder))
  (elmo-folder-status
   (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-exists-p ((folder elmo-filter-folder))
  (elmo-folder-exists-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-creatable-p ((folder elmo-filter-folder))
  (elmo-folder-creatable-p (elmo-filter-folder-target-internal folder)))

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

(luna-define-method elmo-folder-mark-as-read ((folder elmo-filter-folder)
					      numbers)
  (elmo-folder-mark-as-read (elmo-filter-folder-target-internal folder)
			    numbers))

(luna-define-method elmo-folder-unmark-read ((folder elmo-filter-folder)
					      numbers)
  (elmo-folder-unmark-read (elmo-filter-folder-target-internal folder)
			   numbers))

(luna-define-method elmo-folder-mark-as-important ((folder elmo-filter-folder)
						   numbers)
  (elmo-folder-mark-as-important (elmo-filter-folder-target-internal folder)
				 numbers))

(luna-define-method elmo-folder-unmark-important ((folder elmo-filter-folder)
						  numbers)
  (elmo-folder-unmark-important (elmo-filter-folder-target-internal folder)
				numbers))


(require 'product)
(product-provide (provide 'elmo-filter) (require 'elmo-version))

;;; elmo-filter.el ends here
