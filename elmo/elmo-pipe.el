;;; elmo-pipe.el -- PIPE Interface for ELMO.

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

;;; ELMO pipe folder
(eval-and-compile
  (luna-define-class elmo-pipe-folder (elmo-folder)
		     (src dst copy))
  (luna-define-internal-accessors 'elmo-pipe-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-pipe-folder)
					    name)
  (when (string-match "^\\([^|]*\\)|\\(:?\\)\\(.*\\)$" name)
    (elmo-pipe-folder-set-src-internal folder
				       (elmo-make-folder
					(elmo-match-string 1 name)))
    (elmo-pipe-folder-set-dst-internal folder
				       (elmo-make-folder
					(elmo-match-string 3 name)))
    (elmo-pipe-folder-set-copy-internal folder
					(string= ":" (elmo-match-string 2 name))))
  folder)

(luna-define-method elmo-folder-get-primitive-list ((folder elmo-pipe-folder))
  (elmo-flatten
   (mapcar
    'elmo-folder-get-primitive-list
    (list (elmo-pipe-folder-src-internal folder)
	  (elmo-pipe-folder-dst-internal folder)))))

(luna-define-method elmo-folder-contains-type ((folder elmo-pipe-folder)
					       type)
  (or (elmo-folder-contains-type (elmo-pipe-folder-src-internal folder) type)
      (elmo-folder-contains-type (elmo-pipe-folder-dst-internal folder) type)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-pipe-folder)
					      numlist new-mark already-mark
					      seen-mark important-mark
					      seen-list)
  (elmo-folder-msgdb-create (elmo-pipe-folder-dst-internal folder)
			    numlist new-mark already-mark
			    seen-mark important-mark seen-list))

(luna-define-method elmo-folder-append-messages ((folder elmo-pipe-folder)
						 src-folder numbers
						 unread-marks
						 &optional same-number)
  (elmo-folder-append-messages (elmo-pipe-folder-dst-internal folder)
			       src-folder numbers
			       unread-marks
			       same-number))

(luna-define-method elmo-folder-append-buffer ((folder elmo-pipe-folder)
					       unread &optional number)
  (elmo-folder-append-buffer (elmo-pipe-folder-dst-internal folder)
			     unread number))

(luna-define-method elmo-message-fetch ((folder elmo-pipe-folder)
					number strategy
					&optional section outbuf unseen)
  (elmo-message-fetch (elmo-pipe-folder-dst-internal folder)
		      number strategy section outbuf unseen))

(luna-define-method elmo-folder-delete-messages ((folder elmo-pipe-folder)
						 numbers)
  (elmo-folder-delete-messages (elmo-pipe-folder-dst-internal folder)
			       numbers))

(defvar elmo-pipe-drained-hook nil "A hook called when the pipe is flushed.")

(defun elmo-pipe-drain (src dst copy)
  "Move all messages of SRC to DST."
  (let ((elmo-inhibit-number-mapping t) ; No need to use UIDL
	msgs len)
    (message "Checking %s..." (elmo-folder-name-internal src))
    ;; for load killed-list
    (elmo-folder-open src)
    (setq msgs (elmo-folder-list-messages src)
	  len (length msgs))
    (when (> len elmo-display-progress-threshold)
      (elmo-progress-set 'elmo-folder-move-messages
			 len "Moving messages..."))
    (unwind-protect
	(elmo-folder-move-messages src msgs dst
				   nil nil copy)
      (elmo-progress-clear 'elmo-folder-move-messages))
    (if (and copy msgs)
	(progn
	  (elmo-msgdb-append-to-killed-list src msgs)
	  ;; for save killed-list instead of elmo-folder-close-internal
	  (elmo-msgdb-killed-list-save
	   (elmo-folder-msgdb-path src)
	   (elmo-folder-killed-list-internal src)))))
  ;; Don't save msgdb here.
  ;; Because summary view of original folder is not updated yet.
  (elmo-folder-close-internal src)
  (run-hooks 'elmo-pipe-drained-hook))

(luna-define-method elmo-folder-open-internal ((folder elmo-pipe-folder))
  (elmo-folder-open-internal (elmo-pipe-folder-dst-internal folder))
  (let ((src-folder (elmo-pipe-folder-src-internal folder))
	(dst-folder (elmo-pipe-folder-dst-internal folder))
	(copy (elmo-pipe-folder-copy-internal folder)))
    (when (and (elmo-folder-plugged-p src-folder)
	       (elmo-folder-plugged-p dst-folder))
      (elmo-pipe-drain src-folder dst-folder copy))))

(luna-define-method elmo-folder-close-internal ((folder elmo-pipe-folder))
  (elmo-folder-close-internal(elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-pipe-folder) &optional nohide)
  (elmo-folder-list-messages-internal (elmo-pipe-folder-dst-internal
				       folder) nohide))

(luna-define-method elmo-folder-list-unreads-internal
  ((folder elmo-pipe-folder) unread-marks &optional mark-alist)
  (elmo-folder-list-unreads-internal (elmo-pipe-folder-dst-internal folder)
				     unread-marks mark-alist))

(luna-define-method elmo-folder-list-importants-internal
  ((folder elmo-pipe-folder) important-mark)
  (elmo-folder-list-importants-internal (elmo-pipe-folder-dst-internal folder)
					important-mark))

(luna-define-method elmo-folder-status ((folder elmo-pipe-folder))
  (elmo-folder-open-internal (elmo-pipe-folder-src-internal folder))
  (elmo-folder-open-internal (elmo-pipe-folder-dst-internal folder))
  (let* ((elmo-inhibit-number-mapping t)
	 (src-length (length (elmo-folder-list-messages
			      (elmo-pipe-folder-src-internal folder))))
	 (dst-list (elmo-folder-list-messages
		    (elmo-pipe-folder-dst-internal folder))))
    (prog1 (cons (+ src-length (elmo-max-of-list dst-list))
		 (+ src-length (length dst-list)))
      ;; No save.
      (elmo-folder-close-internal (elmo-pipe-folder-src-internal folder))
      (elmo-folder-close-internal (elmo-pipe-folder-dst-internal folder)))))

(luna-define-method elmo-folder-exists-p ((folder elmo-pipe-folder))
  (and (elmo-folder-exists-p (elmo-pipe-folder-src-internal folder))
       (elmo-folder-exists-p (elmo-pipe-folder-dst-internal folder))))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-pipe-folder))
  ;; Share with destination...OK?
  (elmo-folder-expand-msgdb-path (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-newsgroups ((folder elmo-pipe-folder))
  (elmo-folder-newsgroups (elmo-pipe-folder-src-internal folder)))

(luna-define-method elmo-folder-creatable-p ((folder elmo-pipe-folder))
  (and (elmo-folder-creatable-p (elmo-pipe-folder-src-internal folder))
       (elmo-folder-creatable-p (elmo-pipe-folder-dst-internal folder))))

(luna-define-method elmo-folder-writable-p ((folder elmo-pipe-folder))
  (elmo-folder-writable-p (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-create ((folder elmo-pipe-folder))
  (if (and (not (elmo-folder-exists-p (elmo-pipe-folder-src-internal folder)))
	   (elmo-folder-creatable-p (elmo-pipe-folder-src-internal folder)))
      (elmo-folder-create (elmo-pipe-folder-src-internal folder)))
  (if (and (not (elmo-folder-exists-p (elmo-pipe-folder-dst-internal folder)))
	   (elmo-folder-creatable-p (elmo-pipe-folder-dst-internal folder)))
      (elmo-folder-create (elmo-pipe-folder-dst-internal folder))))

(luna-define-method elmo-folder-search ((folder elmo-pipe-folder)
					condition &optional numlist)
  (elmo-folder-search (elmo-pipe-folder-dst-internal folder)
		      condition numlist))

(luna-define-method elmo-message-use-cache-p ((folder elmo-pipe-folder) number)
  (elmo-message-use-cache-p (elmo-pipe-folder-dst-internal folder) number))

(luna-define-method elmo-folder-check ((folder elmo-pipe-folder))
  (elmo-folder-close-internal folder)
  (elmo-folder-open-internal folder))

(luna-define-method elmo-folder-plugged-p ((folder elmo-pipe-folder))
  (and (elmo-folder-plugged-p (elmo-pipe-folder-src-internal folder))
       (elmo-folder-plugged-p (elmo-pipe-folder-dst-internal folder))))

(luna-define-method elmo-folder-message-file-p ((folder elmo-pipe-folder))
  (elmo-folder-message-file-p (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-message-file-p ((folder elmo-pipe-folder) number)
  (elmo-message-file-p (elmo-pipe-folder-dst-internal folder) number))

(luna-define-method elmo-message-file-name ((folder elmo-pipe-folder) number)
  (elmo-message-file-name (elmo-pipe-folder-dst-internal folder) number))

(luna-define-method elmo-folder-message-file-number-p ((folder
							elmo-pipe-folder))
  (elmo-folder-message-file-number-p (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-message-file-directory ((folder
							 elmo-pipe-folder))
  (elmo-folder-message-file-directory
   (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-message-make-temp-file-p
  ((folder elmo-pipe-folder))
  (elmo-folder-message-make-temp-file-p
   (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-message-make-temp-files ((folder
							  elmo-pipe-folder)
							 numbers
							 &optional
							 start-number)
  (elmo-folder-message-make-temp-files
   (elmo-pipe-folder-dst-internal folder) numbers start-number))

(luna-define-method elmo-folder-mark-as-read ((folder elmo-pipe-folder)
					      numbers)
  (elmo-folder-mark-as-read (elmo-pipe-folder-dst-internal folder)
			    numbers))

(luna-define-method elmo-folder-unmark-read ((folder elmo-pipe-folder)
					      numbers)
  (elmo-folder-unmark-read (elmo-pipe-folder-dst-internal folder)
			   numbers))

(luna-define-method elmo-folder-unmark-important ((folder elmo-pipe-folder)
						  numbers)
  (elmo-folder-unmark-important (elmo-pipe-folder-dst-internal folder)
				numbers))

(luna-define-method elmo-folder-mark-as-important ((folder elmo-pipe-folder)
						   numbers)
  (elmo-folder-mark-as-important (elmo-pipe-folder-dst-internal folder)
				 numbers))

(luna-define-method elmo-folder-pack-numbers ((folder elmo-pipe-folder))
  (elmo-folder-pack-numbers (elmo-pipe-folder-dst-internal folder)))

(require 'product)
(product-provide (provide 'elmo-pipe) (require 'elmo-version))

;;; elmo-pipe.el ends here
