;;; elmo-dop.el -- Modules for Disconnected Operations on ELMO.

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
(require 'elmo-vars)
(require 'elmo-msgdb)
(require 'elmo-util)

;; global variable.
(defvar elmo-dop-queue nil
  "A list of (folder-name function-to-be-called argument-list).
Automatically loaded/saved.")

(defvar elmo-dop-folder (concat "+" (expand-file-name "dop"
						      elmo-msgdb-dir))
  "A folder for `elmo-folder-append-messages' disconnected operations.")

(defmacro elmo-make-dop-queue (fname method arguments)
  "Make a dop queue."
  (` (vector (, fname) (, method) (, arguments))))

(defmacro elmo-dop-queue-fname (queue)
  "Return the folder name string of the QUEUE."
  (` (aref (, queue) 0)))

(defmacro elmo-dop-queue-method (queue)
  "Return the method symbol of the QUEUE."
  (` (aref (, queue) 1)))

(defmacro elmo-dop-queue-arguments (queue)
  "Return the arguments of the QUEUE."
  (` (aref (, queue) 2)))

(defun elmo-dop-queue-append (fname method arguments)
  "Append to disconnected operation queue."
  (let ((queue (elmo-make-dop-queue fname method arguments)))
    (setq elmo-dop-queue (nconc elmo-dop-queue (list queue)))))

(defun elmo-dop-queue-flush (&optional force)
  "Flush disconnected operations.
If optional argument FORCE is non-nil, try flushing all operation queues
even an operation concerns the unplugged folder."
  (elmo-dop-queue-merge)
  (let ((queue elmo-dop-queue)
	(count 0)
	len)
    (while queue
      (if (or force (elmo-folder-plugged-p (elmo-make-folder (caar queue))))
	  (setq count (1+ count)))
      (setq queue (cdr queue)))
    (when (> count 0)
      (if (elmo-y-or-n-p
	   (format "%d pending operation(s) exists.  Perform now? " count)
	   (not elmo-dop-flush-confirm) t)
	  (progn
	    (message "")
	    (sit-for 0)
	    (let ((queue elmo-dop-queue)
		  (performed 0)
		  (i 0)
		  (num (length elmo-dop-queue))
		  folder func failure)
	      (while queue
		;; now perform pending processes.
		(setq failure nil)
		(setq i (+ 1 i))
		(message "Flushing queue....%d/%d." i num)
		(condition-case err
		    (apply (elmo-dop-queue-method (car queue))
			   (elmo-dop-queue-fname (car queue))
			   (elmo-dop-queue-arguments queue))
		  (quit  (setq failure t))
		  (error (setq failure err)))
		(if failure
		    ();
		  (setq elmo-dop-queue (delq (car queue) elmo-dop-queue))
		  (setq performed (+ 1 performed)))
		(setq queue (cdr queue)))
	      (message "%d/%d operation(s) are performed successfully."
		       performed num)
	      (sit-for 0) ; 
	      (elmo-dop-queue-save)))
	(if (elmo-y-or-n-p "Clear all pending operations? "
			   (not elmo-dop-flush-confirm) t)
	    (progn
	      (setq elmo-dop-queue nil)
	      (message "All pending operations are cleared.")
	      (elmo-dop-queue-save))
	  (message "")))
      count)))

(defvar elmo-dop-merge-funcs nil)
(defun elmo-dop-queue-merge ()
  (let ((queue elmo-dop-queue)
        new-queue match-queue que)
    (while (setq que (car queue))
      (if (and
	   (member (cadr que) elmo-dop-merge-funcs)
	   (setq match-queue
		 (car (delete nil
			      (mapcar
			       (lambda (new-queue)
				 (if (and
				      (string= (car que) (car new-queue))
				      (string= (cadr que) (cadr new-queue)))
				     new-queue))
			       new-queue)))))
	  (setcar (cddr match-queue)
		  (append (nth 2 match-queue) (nth 2 que)))
	(setq new-queue (append new-queue (list que))))
      (setq queue (cdr queue)))
    (setq elmo-dop-queue new-queue)))


;;; Execution is delayed.


;;; Offline append:
;; If appended message is local file or cached, it is saved in
;; .elmo/dop/1 2 3 4 ...
;; then msgdb-path/append file is created and contain message number list.
;; ex. (1 3 5)

(defun elmo-folder-append-buffer-dop (folder unread &optional number)
  )

(defun elmo-folder-delete-messages-dop (folder numbers)
  )

(defun elmo-folder-encache-dop (folder numbers)
  )

(defun elmo-create-folder-dop (folder)
  )

;;; Execute as subsutitute for plugged operation.
(defun elmo-folder-status-dop (folder)
  (let* ((number-alist (elmo-msgdb-number-load
			(elmo-folder-msgdb-path folder)))
	 (number-list (mapcar 'car number-alist))
	 (i 0)
	 max-num)
    ;; number of messages which are queued as append should be added
    ;; to max-num and length.
    (setq max-num
	  (or (nth (max (- (length number-list) 1) 0) number-list)
	      0))
    (cons max-num number-list)))

(require 'product)
(product-provide (provide 'elmo-dop) (require 'elmo-version))

;;; elmo-dop.el ends here
