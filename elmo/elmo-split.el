;;; elmo-split.el --- Split messages according to the user defined rules.

;; Copyright (C) 2002 Yuuichi Teranishi <teranisi@gohome.org>

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
;; Put following lines on your .emacs.
;;
;; (autoload 'elmo-split "elmo-split" "Split messages on the folder." t)
;;
;; A command elmo-split is provided. If you enter:
;;
;; M-x elmo-split
;;
;; Messages in the `elmo-split-folder' are splitted to the folders
;; according to the definition of `elmo-split-rule'.
;; 

(require 'elmo)

;;; Code:
(defcustom elmo-split-rule nil
  "Split rule for the command `elmo-split'.
The format of this variable is a list of RULEs which has form like:
\(CONDITION FOLDER [continue]\)

The 1st element CONDITION is a sexp which consists of following.

1. Functions which accept argument FIELD-NAME and VALUE.
FIELD-NAME is a symbol of the field name.

`equal'             ... True if the field value equals to VALUE.
                        Case of the letters are ignored.
`match'             ... True if the field value matches to VALUE.
                        VALUE can contain \\& and \\N which will substitute
                        from matching \\(\\) patterns in the previous VALUE.
`address-equal'     ... True if one of the addresses in the field equals to
                        VALUE. Case of the letters are ignored.
`address-match'     ... True if one of the addresses in the field matches to
                        VALUE.
                        VALUE can contain \\& and \\N which will substitute
                        from matching \\(\\) patterns in the previous VALUE.

2. Functions which accept any number of arguments.

`or'                ... True if one of the argument returns true.
`and'               ... True if all of the arguments return true.

3. A symbol.

When a symbol is specified, it is evaluated.

The 2nd element FOLDER is the name of the folder to split messages into.

When the 3rd element `continue' is specified as symbol, evaluating rules is
not stopped even when the condition is satisfied.

Example:

\(setq elmo-split-rule
      ;; Messages from spammers are stored in `+junk'
      '(((or (address-equal from \"i.am@spammer\")
	     (address-equal from \"dull-work@dull-boy\")
	     (address-equal from \"death-march@software\")
	     (address-equal from \"ares@aon.at\")
	     (address-equal from \"get-money@richman\"))
	 \"+junk\")
	;; Messages from mule mailing list are stored in `%mule'
	((equal x-ml-name \"mule\") \"%mule\")
	;; Messages from wanderlust mailing list are stored in `%wanderlust'
	;; and continue evaluating following rules.
	((equal x-ml-name \"wanderlust\") \"%wanderlust\" continue)
	;; Messages from DoCoMo user are stored in `+docomo-{username}'.
	((match from \"\\\\(.*\\\\)@docomo\\\\.ne\\\\.jp\")
	 \"+docomo-\\\\1\")
	;; Unmatched mails go to `+inbox'.
	(t \"+inbox\")))"
  :group 'elmo
  :type 'sexp)

(defcustom elmo-split-folder "%inbox"
  "Target folder or list of folders for splitting."
  :type '(choice (string :tag "folder name")
		 (repeat (string :tag "folder name")))
  :group 'elmo)

(defcustom elmo-split-log-coding-system 'x-ctext
  "A coding-system for writing log file."
  :type 'coding-system
  :group 'elmo)

(defcustom elmo-split-log-file "~/.elmo/split-log"
  "The file name of the split log."
  :type 'file
  :group 'elmo)

;;;
(defvar elmo-split-match-string-internal nil
  "Internal variable for string matching.  Don't touch this variable by hand.")

;;;
(defun elmo-split-or (buffer &rest args)
  (catch 'done
    (dolist (arg args)
      (if (elmo-split-eval buffer arg)
	  (throw 'done t)))
    nil))

(defun elmo-split-and (buffer &rest args)
  (catch 'done
    (dolist (arg args)
      (unless (elmo-split-eval buffer arg)
	(throw 'done nil)))
    t))

(defun elmo-split-address-equal (buffer field value)
  (with-current-buffer buffer
    (let ((addrs (mapcar
		  'std11-address-string
		  (std11-parse-addresses-string
		   (std11-field-body (symbol-name field)))))
	  (case-fold-search t)
	  result)
      (while addrs
	(when (string-match (concat "^"
				    (regexp-quote value)
				    "$") (car addrs))
	  (setq addrs nil
		result t))
	(setq addrs (cdr addrs)))
      result)))

(defun elmo-split-address-match (buffer field value)
  (with-current-buffer buffer
    (let ((addrs (mapcar
		  'std11-address-string
		  (std11-parse-addresses-string
		   (std11-field-body (symbol-name field)))))
	  result)
      (while addrs
	(when (string-match value (car addrs))
	  (setq elmo-split-match-string-internal (car addrs)
		addrs nil
		result t))
	(setq addrs (cdr addrs)))
      result)))

(defun elmo-split-equal (buffer field value)
  (with-current-buffer buffer
    (let ((field-value (std11-field-body (symbol-name field))))
      (equal field-value value))))

(defun elmo-split-match (buffer field value)
  (with-current-buffer buffer
    (let ((field-value (std11-field-body (symbol-name field))))
      (and field-value
	   (when (string-match value field-value)
	     (setq elmo-split-match-string-internal field-value))))))

(defun elmo-split-eval (buffer sexp)
  (cond
   ((consp sexp)
    (apply (intern (concat "elmo-split-" (symbol-name (car sexp))))
	   buffer
	   (cdr sexp)))
   ((stringp sexp)
    (std11-field-body sexp))
   (t (eval sexp))))

(defun elmo-split-log (message reharsal)
  (with-current-buffer (get-buffer-create "*elmo-split*")
    (goto-char (point-max))
    (let ((start (point))
	  (coding-system-for-write elmo-split-log-coding-system))
      (insert message)
      (if reharsal
	  (progn
	    (pop-to-buffer (current-buffer))
	    (sit-for 0))
	(write-region start (point) elmo-split-log-file t 'no-msg)))))

;;;###autoload
(defun elmo-split (&optional arg)
  "Split messages in the `elmo-split-folder' according to `elmo-split-rule'.
If prefix argument ARG is specified, do a reharsal (no harm)."
  (interactive "P")
  (unless elmo-split-rule
    (error "Split rule doest not exist. Set `elmo-split-rule' first."))
  (let ((folders (if (listp elmo-split-folder)
		     elmo-split-folder
		   (list elmo-split-folder))))
    (dolist (folder folders)
      (elmo-split-subr (elmo-make-folder folder) arg))))

(defun elmo-split-subr (folder &optional reharsal)
  (let ((elmo-inhibit-display-retrieval-progress t)
	(count 0)
	(fcount 0)
	msgs fname target-folder failure)
    (message "Splitting...")
    (elmo-folder-open-internal folder)
    (setq msgs (elmo-folder-list-messages folder))
    (elmo-progress-set 'elmo-split (length msgs) "Splitting...")
    (unwind-protect
	(progn
	  (with-temp-buffer
	    (dolist (msg msgs)
	      (erase-buffer)
	      (when (ignore-errors
		      (elmo-message-fetch folder msg
					  (elmo-make-fetch-strategy 'entire)
					  nil (current-buffer) 'unread))
		(catch 'terminate
		  (dolist (rule elmo-split-rule)
		    (setq elmo-split-match-string-internal nil)
		    (when (elmo-split-eval (current-buffer) (car rule))
		      (if elmo-split-match-string-internal
			  (setq fname (elmo-expand-newtext
				       (nth 1 rule)
				       elmo-split-match-string-internal))
			(setq fname (nth 1 rule)))
		      (unless reharsal
			(setq failure nil)
			(condition-case nil
			    (progn
			      (setq target-folder (elmo-make-folder fname))
			      (unless (elmo-folder-exists-p target-folder)
				(when
				    (and
				     (elmo-folder-creatable-p
				      target-folder)
				     (y-or-n-p
				      (format
				       "Folder %s does not exist, Create it? "
				       fname)))
				  (elmo-folder-create target-folder)))
			      (elmo-folder-open-internal target-folder)
			      (elmo-folder-append-buffer target-folder 'unread)
			      (elmo-folder-close-internal target-folder))
			  (error (setq failure t)
				 (incf fcount)))
			(unless failure
			  (ignore-errors
			    (elmo-folder-delete-messages folder (list msg))))
			(incf count))
		      (elmo-split-log
		       (concat "From "
			       (nth 1 (std11-extract-address-components
				       (or (std11-field-body "from") "")))
			       "  " (or (std11-field-body "date") "") "\n"
			       " Subject: "
			       (eword-decode-string (or (std11-field-body
							 "subject") ""))
			       "\n"
			       (if reharsal
				   "  Test: "
				 "  Folder: ")
			       fname "/0" "\n")
		       reharsal)
		      (unless (eq (nth 2 rule) 'continue)
			(throw 'terminate nil))))))
	      (elmo-progress-notify 'elmo-split)))
	  (elmo-folder-close-internal folder))
      (elmo-progress-clear 'elmo-split))
    (run-hooks 'elmo-split-hook)
    (message
     (concat
      (cond
       ((eq count 0)
	"No message is splitted")
       ((eq count 1)
	"1 message is splitted")
       (t
	(format "%d messages are splitted" count)))
      (if (eq fcount 0)
	  "."
	(format " (%d failure)." fcount))))))

(provide 'elmo-split)

;;; elmo-split.el ends here
