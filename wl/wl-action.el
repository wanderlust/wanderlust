;;; wl-action.el --- Mark and actions in the Summary mode for Wanderlust.

;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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

(require 'wl-summary)

(defsubst wl-summary-action-mark (action)
  (nth 0 action))
(defsubst wl-summary-action-symbol (action)
  (nth 1 action))
(defsubst wl-summary-action-argument-function (action)
  (nth 2 action))
(defsubst wl-summary-action-set-function (action)
  (nth 3 action))
(defsubst wl-summary-action-exec-function (action)
  (nth 4 action))
(defsubst wl-summary-action-face (action)
  (nth 5 action))
(defsubst wl-summary-action-docstring (action)
  (nth 6 action))

;; Set mark
(defun wl-summary-set-mark (&optional set-mark number interactive data)
  (interactive)
  "Set temporary mark SET-MARK on the message with NUMBER.
NUMBER is the message number to set the mark on.
INTERACTIVE is set as t if it have to run interactively.
DATA is passed to the set-action function of the action as an argument.
Return number if put mark succeed"
  (let* ((set-mark (or set-mark
		       (completing-read "Mark: " wl-summary-mark-action-list)))
	 (current (wl-summary-message-number))
	 (action (assoc set-mark wl-summary-mark-action-list))
	 visible mark cur-mark)
    (save-excursion
      ;; Put mark
      (setq visible (or
		     ;; not-interactive and visible
		     (and number (wl-summary-jump-to-msg number))
		     ;; interactive
		     (and (null number) current))
	    number (or number current)
	    cur-mark (nth 1 (wl-summary-registered-temp-mark number)))
      (if (and cur-mark
	       (wl-summary-reserve-temp-mark-p cur-mark))
	  (if interactive
	      (error "Already marked as `%s'" cur-mark)
	    (setq number nil))
	(when (and interactive
		   (null data)
		   (wl-summary-action-argument-function action))
	  (setq data (funcall (wl-summary-action-argument-function action)
			      (wl-summary-action-symbol action)
			      number)))
	(wl-summary-unset-mark number)
	(when visible
	  (wl-summary-mark-line set-mark)
	  (when wl-summary-highlight
	    (wl-highlight-summary-current-line))
	  (when data
	    (wl-summary-print-destination number data)))
	;; Set action.
	(funcall (wl-summary-action-set-function action)
		 number
		 (wl-summary-action-mark action)
		 data)
	(set-buffer-modified-p nil)))
    ;; Move the cursor.
    (if (or interactive (interactive-p))
	(if (eq wl-summary-move-direction-downward nil)
	    (wl-summary-prev)
	  (wl-summary-next)))
    ;; Return value.
    number))

(defun wl-summary-register-target-mark (number mark data)
  (or (memq number wl-summary-buffer-target-mark-list)
      (setq wl-summary-buffer-target-mark-list
	    (cons number wl-summary-buffer-target-mark-list))))

(defun wl-summary-unregister-target-mark (number)
  (delq number wl-summary-buffer-target-mark-list))

(defun wl-summary-have-target-mark-p (number)
  (memq number wl-summary-buffer-target-mark-list))

(defun wl-summary-target-mark-set-action (action)
  (unless (eq (wl-summary-action-symbol action) 'target-mark)
    (save-excursion
      (goto-char (point-min))
      (let ((numlist wl-summary-buffer-number-list)
	    number mlist data)
	;; use firstly marked message.
	(when (wl-summary-action-argument-function action)
	  (while numlist
	    (if (memq (car numlist) wl-summary-buffer-target-mark-list)
		(setq number (car numlist)
		      numlist nil))
	    (setq numlist (cdr numlist)))
	  (wl-summary-jump-to-msg number)
	  (setq data (funcall (wl-summary-action-argument-function action)
			      (wl-summary-action-symbol action) number)))
	(while (not (eobp))
	  (when (string= (wl-summary-temp-mark) "*")
	    (let (wl-summary-buffer-disp-msg)
	      (when (setq number (wl-summary-message-number))
		(wl-summary-set-mark (wl-summary-action-mark action)
				     number nil data)
		(setq wl-summary-buffer-target-mark-list
		      (delq number wl-summary-buffer-target-mark-list)))))
	  (forward-line 1))
	(setq mlist wl-summary-buffer-target-mark-list)
	(while mlist
	  (wl-summary-register-temp-mark (car mlist)
					 (wl-summary-action-mark action) data)
	  (setq wl-summary-buffer-target-mark-list
		(delq (car mlist) wl-summary-buffer-target-mark-list))
	  (setq mlist (cdr mlist)))))))

;; wl-summary-buffer-temp-mark-list specification
;; ((1 "D" nil)(2 "o" "+fuga")(3 "O" "+hoge"))
(defun wl-summary-register-temp-mark (number mark mark-info)
  (let ((elem (assq number wl-summary-buffer-temp-mark-list)))
    (setq wl-summary-buffer-temp-mark-list
	  (delq elem wl-summary-buffer-temp-mark-list)))
  (setq wl-summary-buffer-temp-mark-list
	(cons (list number mark mark-info) wl-summary-buffer-temp-mark-list)))

(defun wl-summary-unregister-temp-mark (number)
  (let ((elem (assq number wl-summary-buffer-temp-mark-list)))
    (setq wl-summary-buffer-temp-mark-list
	  (delq elem wl-summary-buffer-temp-mark-list))))

(defun wl-summary-registered-temp-mark (number)
  (assq number wl-summary-buffer-temp-mark-list))

(defun wl-summary-collect-temp-mark (mark &optional begin end)
  (if (or begin end)
      (save-excursion
	(save-restriction
	  (let (mark-list)
	    (narrow-to-region (or begin (point-min))(or end (point-max)))
	    (goto-char (point-min))
	    ;; for thread...
	    (if (eq wl-summary-buffer-view 'thread)
		(let (number entity mark-info)
		  (while (not (eobp))
		    (setq number (wl-summary-message-number)
			  entity (wl-thread-get-entity number)
			  mark-info (wl-summary-registered-temp-mark number))
		    ;; toplevel message mark.
		    (when (string= (nth 1 mark-info) mark)
		      (setq mark-list (cons mark-info mark-list)))
		    ;; When thread is closed...children should also be checked.
		    (unless (wl-thread-entity-get-opened entity)
		      (dolist (msg (wl-thread-get-children-msgs number))
			(setq mark-info (wl-summary-registered-temp-mark
					 msg))
			(when (string= (nth 1 mark-info) mark)
			  (setq mark-list (cons mark-info mark-list)))))
		    (forward-line 1)))
	      (let (number mark-info)
		(while (not (eobp))
		  (setq number (wl-summary-message-number)
			mark-info (wl-summary-registered-temp-mark number))
		  (when (string= (nth 1 mark-info) mark)
		    (setq mark-list (cons mark-info mark-list)))
		  (forward-line 1))))
	    mark-list)))
    (let (mark-list)
      (dolist (mark-info wl-summary-buffer-temp-mark-list)
	(when (string= (nth 1 mark-info) mark)
	  (setq mark-list (cons mark-info mark-list))))
      mark-list)))

;; Unset mark
(defun wl-summary-unset-mark (&optional number interactive)
  "Unset temporary mark of the message with NUMBER.
NUMBER is the message number to unset the mark.
If not specified, the message on the cursor position is treated.
Optional INTERACTIVE is non-nil when it should be called interactively.
Return number if put mark succeed"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((buffer-read-only nil)
	  visible mark action)
      (if number
	  (setq visible (wl-summary-jump-to-msg number))
	(setq visible t))
      (setq number (or number (wl-summary-message-number)))
      ;; Delete mark on buffer.
      (when visible
	(setq mark (wl-summary-temp-mark))
	(unless (string= mark " ")
	  (delete-backward-char 1)
	  (insert (or (wl-summary-get-score-mark number)
		      " "))
	  (setq action (assoc mark wl-summary-mark-action-list))
	  (when wl-summary-highlight
	    (wl-highlight-summary-current-line))
	  (when (wl-summary-action-argument-function action)
	    (wl-summary-remove-destination)))
	(set-buffer-modified-p nil))
      ;; Remove from temporal mark structure.
      (wl-summary-unregister-target-mark number)
      (wl-summary-unregister-temp-mark number)))
  ;; Move the cursor.
  ;;  (if (or interactive (interactive-p))
  ;;      (if (eq wl-summary-move-direction-downward nil)
  ;;	  (wl-summary-prev)
  ;;	(wl-summary-next))))
  )

(defun wl-summary-make-destination-numbers-list (mark-list)
  (let (dest-numbers dest-number)
    (dolist (elem mark-list)
      (setq dest-number (assoc (nth 2 elem) dest-numbers))
      (if dest-number
	  (unless (memq (car elem) (cdr dest-number))
	    (nconc dest-number (list (car elem))))
	(setq dest-numbers (nconc dest-numbers
				  (list
				   (list (nth 2 elem)
					 (car elem)))))))
    dest-numbers))

(defun wl-summary-move-mark-list-messages (mark-list folder-name message)
  (if (null mark-list)
      (message "No marks")
    (save-excursion
      (let ((start (point))
	    (refiles (mapcar 'car mark-list))
	    (refile-failures 0)
	    refile-len
	    dst-msgs			; loop counter
	    result)
	;; begin refile...
	(setq refile-len (length refiles))
	(goto-char start)		; avoid moving cursor to
					; the bottom line.
	(message message)
	(when (> refile-len elmo-display-progress-threshold)
	  (elmo-progress-set 'elmo-folder-move-messages
			     refile-len message))
	(setq result nil)
	(condition-case nil
	    (setq result (elmo-folder-move-messages
			  wl-summary-buffer-elmo-folder
			  refiles
			  (if (eq folder-name 'null)
			      'null
			    (wl-folder-get-elmo-folder folder-name))
			  (wl-summary-buffer-msgdb)
			  (not (null (cdr dst-msgs)))
			  nil ; no-delete
			  nil ; same-number
			  t))
	  (error nil))
	(when result		; succeeded.
	  ;; update buffer.
	  (wl-summary-delete-messages-on-buffer refiles)
	  ;; update wl-summary-buffer-temp-mark-list.
	  (dolist (mark-info mark-list)
	    (setq wl-summary-buffer-temp-mark-list
		  (delq mark-info wl-summary-buffer-temp-mark-list))))
	(elmo-progress-clear 'elmo-folder-move-messages)
	(message (concat message "done"))
	(wl-summary-set-message-modified)
	;; Return the operation failed message numbers.
	(if result
	    0
	  (length refiles))))))

(defun wl-summary-get-refile-destination-subr (action number learn)
  (let* ((number (or number (wl-summary-message-number)))
	 (msgid (and number
		     (elmo-message-field wl-summary-buffer-elmo-folder
					 number 'message-id)))
	 (entity (and number
		      (elmo-message-entity wl-summary-buffer-elmo-folder
					   number)))
	 folder cur-mark tmp-folder)
    (catch 'done
      (when (null entity)
	(message "Cannot decide destination.")
	(throw 'done nil))
      (when (null number)
	(message "No message.")
	(throw 'done nil))
      (setq folder (wl-summary-read-folder
		    (or (wl-refile-guess entity) wl-trash-folder)
		    (format "for %s " action)))
      ;; Cache folder hack by okada@opaopa.org
      (when (and (eq (elmo-folder-type-internal
		      (wl-folder-get-elmo-folder
		       (wl-folder-get-realname folder))) 'cache)
		 (not (string= folder
			       (setq tmp-folder
				     (concat "'cache/"
					     (elmo-cache-get-path-subr
					      (elmo-msgid-to-cache msgid)))))))
	(setq folder tmp-folder)
	(message "Force refile to %s." folder))
      (if (string= folder (wl-summary-buffer-folder-name))
	  (error "Same folder"))
      (if (or (not (elmo-folder-writable-p (wl-folder-get-elmo-folder folder)))
	      (string= folder wl-queue-folder)
	      (string= folder wl-draft-folder))
	  (error "Don't set as target: %s" folder))
      ;; learn for refile.
      (when learn
	(wl-refile-learn entity folder))
      folder)))

;;; Actions
(defun wl-summary-define-mark-action ()
  (interactive)
  (dolist (action wl-summary-mark-action-list)
    (fset (intern (format "wl-summary-%s" (wl-summary-action-symbol action)))
	  `(lambda (&optional number data)
	     ,(wl-summary-action-docstring action)
	     (interactive)
	     (wl-summary-set-mark ,(wl-summary-action-mark action)
				  number (interactive-p) data)))
    (fset (intern (format "wl-summary-%s-region"
			  (wl-summary-action-symbol action)))
	  `(lambda (beg end)
	     ,(wl-summary-action-docstring action)
	     (interactive "r")
	     (goto-char beg)
	     (wl-summary-mark-region-subr
	      (quote ,(intern (format "wl-summary-%s"
				      (wl-summary-action-symbol action))))
	      beg end
	      (if (quote ,(wl-summary-action-argument-function action))
		  (funcall (function 
			    ,(wl-summary-action-argument-function action))
			   (quote ,(wl-summary-action-symbol action))
			   (wl-summary-message-number))))))
    (fset (intern (format "wl-summary-target-mark-%s"
			  (wl-summary-action-symbol action)))
	  `(lambda ()
	     ,(wl-summary-action-docstring action)
	     (interactive)
	     (wl-summary-target-mark-set-action (quote ,action))))
    (fset (intern (format "wl-thread-%s"
			  (wl-summary-action-symbol action)))
	  `(lambda (arg)
	     ,(wl-summary-action-docstring action)
	     (interactive "P")
	     (wl-thread-call-region-func
	      (quote ,(intern (format "wl-summary-%s-region"
				      (wl-summary-action-symbol action))))
	      arg)
	     (if arg
		 (wl-summary-goto-top-of-current-thread))
	     (if (not wl-summary-move-direction-downward)
		 (wl-summary-prev)
	       (wl-thread-goto-bottom-of-sub-thread)
	       (if wl-summary-buffer-disp-msg
		   (wl-summary-redisplay)))))))

(defun wl-summary-get-dispose-folder (folder)
  (if (string= folder wl-trash-folder)
      'null
    (let* ((type (or (wl-get-assoc-list-value wl-dispose-folder-alist folder)
		     'trash)))
      (cond ((stringp type)
	     type)
	    ((or (equal type 'remove) (equal type 'null))
	     'null)
	    (t;; (equal type 'trash)
	     (let ((trash-folder (wl-folder-get-elmo-folder wl-trash-folder)))
	       (unless (elmo-folder-exists-p trash-folder)
		 (if (y-or-n-p
		      (format "Trash Folder %s does not exist, create it? "
			      wl-trash-folder))
		     (elmo-folder-create trash-folder)
		   (error "Trash Folder is not created"))))
	     wl-trash-folder)))))

;; Dispose action.
(defun wl-summary-exec-action-dispose (mark-list)
  (wl-summary-move-mark-list-messages mark-list
				      (wl-summary-get-dispose-folder
				       (wl-summary-buffer-folder-name))
				      "Disposing messages..."))

;; Delete action.
(defun wl-summary-exec-action-delete (mark-list)
  (wl-summary-move-mark-list-messages mark-list
				      'null
				      "Deleting messages..."))

;; Refile action
(defun wl-summary-set-action-refile (number mark data)
  (let ((policy (wl-get-assoc-list-value wl-refile-policy-alist
					 (wl-summary-buffer-folder-name)))
	(elem wl-summary-mark-action-list))
    (if (eq policy 'copy)
	(while elem
	  (when (eq (wl-summary-action-symbol (car elem)) 'copy)
	    (wl-summary-register-temp-mark number
					   (wl-summary-action-mark (car elem))
					   data)
	    (setq elem nil))
	  (setq elem (cdr elem)))
      (wl-summary-register-temp-mark number mark data)
      (setq wl-summary-buffer-prev-refile-destination data))))

(defun wl-summary-get-refile-destination (action number)
  "Decide refile destination."
  (wl-summary-get-refile-destination-subr action number t))

(defun wl-summary-exec-action-refile (mark-list)
  (save-excursion
    (let ((start (point))
	  (failures 0)
	  (refile-len (length mark-list))
	  dst-msgs ; loop counter
	  result)
      ;; begin refile...
      (setq dst-msgs
	    (wl-summary-make-destination-numbers-list mark-list))
      (goto-char start)	; avoid moving cursor to the bottom line.
      (when (> refile-len elmo-display-progress-threshold)
	(elmo-progress-set 'elmo-folder-move-messages
			   refile-len "Refiling messages..."))
      (while dst-msgs
	(setq result nil)
	(condition-case nil
	    (setq result (elmo-folder-move-messages
			  wl-summary-buffer-elmo-folder
			  (cdr (car dst-msgs))
			  (wl-folder-get-elmo-folder
			   (car (car dst-msgs)))
			  (wl-summary-buffer-msgdb)
			  (not (null (cdr dst-msgs)))
			  nil ; no-delete
			  nil ; same-number
			  t))
	  (error nil))
	(if result		; succeeded.
	    (progn
	      ;; update buffer.
	      (wl-summary-delete-messages-on-buffer (cdr (car dst-msgs)))
	      (setq wl-summary-buffer-temp-mark-list
		    (wl-delete-associations 
		     (cdr (car dst-msgs))
		     wl-summary-buffer-temp-mark-list)))
	  (setq failures
		(+ failures (length (cdr (car dst-msgs))))))
	(setq dst-msgs (cdr dst-msgs)))
      (elmo-progress-clear 'elmo-folder-move-messages)
      failures)))

;; Copy action
(defun wl-summary-get-copy-destination (action number)
  (wl-summary-get-refile-destination-subr action number nil))

(defun wl-summary-exec-action-copy (mark-list)
  (save-excursion
    (let ((start (point))
	  (failures 0)
	  (refile-len (length mark-list))
	  dst-msgs ; loop counter
	  result)
      ;; begin refile...
      (setq dst-msgs
	    (wl-summary-make-destination-numbers-list mark-list))
      (goto-char start)	; avoid moving cursor to the bottom line.
      (when (> refile-len elmo-display-progress-threshold)
	(elmo-progress-set 'elmo-folder-move-messages
			   refile-len "Copying messages..."))
      (while dst-msgs
	(setq result nil)
	(condition-case nil
	    (setq result (elmo-folder-move-messages
			    wl-summary-buffer-elmo-folder
			    (cdr (car dst-msgs))
			    (wl-folder-get-elmo-folder
			     (car (car dst-msgs)))
			    (wl-summary-buffer-msgdb)
			    (not (null (cdr dst-msgs)))
			    t ; t is no-delete (copy)
			    nil ; same-number
			    t))
	  (error nil))
	(if result		; succeeded.
	    (progn
	      ;; update buffer.
	      (wl-summary-delete-copy-marks-on-buffer (cdr (car dst-msgs)))
	      (setq wl-summary-buffer-temp-mark-list
		    (wl-delete-associations 
		     (cdr (car dst-msgs))
		     wl-summary-buffer-temp-mark-list)))
	  (setq failures
		(+ failures (length (cdr (car dst-msgs))))))
	(setq dst-msgs (cdr dst-msgs)))
      (elmo-progress-clear 'elmo-folder-move-messages)
      failures)))

;; Prefetch.
(defun wl-summary-exec-action-prefetch (mark-list)
  (save-excursion
    (let* ((buffer-read-only nil)
	   (count 0)
	   (length (length mark-list))
	   (mark-list-copy (copy-sequence mark-list))
	   (pos (point))
	   (failures 0)
	   new-mark)
      (dolist (mark-info mark-list-copy)
	(message "Prefetching...(%d/%d)"
		 (setq count (+ 1 count)) length)
	(setq new-mark (wl-summary-prefetch-msg (car mark-info)))
	(if new-mark
	    (progn
	      (wl-summary-unset-mark (car mark-info))
	      (when (wl-summary-jump-to-msg (car mark-info))
		(wl-summary-persistent-mark) ; move
		(delete-backward-char 1)
		(insert new-mark)
		(when wl-summary-highlight
		  (wl-highlight-summary-current-line))
		(save-excursion
		  (goto-char pos)
		  (sit-for 0))))
	  (incf failures)))
      (message "Prefetching...done")
      0)))

;; Resend.
(defun wl-summary-get-resend-address (action number)
  "Decide resend address."
  (wl-complete-field-to "Resend message to: "))

(defun wl-summary-exec-action-resend (mark-list)
  (let ((failure 0))
    (dolist (mark-info mark-list)
      (if (condition-case nil
	      (progn
		(wl-summary-exec-action-resend-subr (car mark-info)
						    (nth 2 mark-info))
		t)
	    (error))
	  (wl-summary-unmark (car mark-info))
	(incf failure)))
    failure))

(defun wl-summary-exec-action-resend-subr (number address)
  "Resend the message with NUMBER to ADDRESS."
  (message "Resending message to %s..." address)
  (let ((folder wl-summary-buffer-elmo-folder))
    (save-excursion
      ;; We first set up a normal mail buffer.
      (set-buffer (get-buffer-create " *wl-draft-resend*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (setq wl-sent-message-via nil)
      ;; Insert our usual headers.
      (wl-draft-insert-from-field)
      (wl-draft-insert-date-field)
      (insert "To: " address "\n")
      (goto-char (point-min))
      ;; Rename them all to "Resent-*".
      (while (re-search-forward "^[A-Za-z]" nil t)
	(forward-char -1)
	(insert "Resent-"))
      (widen)
      (forward-line)
      (delete-region (point) (point-max))
      (let ((beg (point)))
	;; Insert the message to be resent.
	(insert
	 (with-temp-buffer
	   (elmo-message-fetch folder number
			       (elmo-make-fetch-strategy 'entire)
			       nil (current-buffer) 'unread)
	   (buffer-string)))
	(goto-char (point-min))
	(search-forward "\n\n")
	(forward-char -1)
	(save-restriction
	  (narrow-to-region beg (point))
	  (wl-draft-delete-fields wl-ignored-resent-headers)
	  (goto-char (point-max)))
	(insert mail-header-separator)
	;; Rename all old ("Previous-")Resent headers.
	(while (re-search-backward "^\\(Previous-\\)*Resent-" beg t)
	  (beginning-of-line)
	  (insert "Previous-"))
	;; Quote any "From " lines at the beginning.
	(goto-char beg)
	(when (looking-at "From ")
	  (replace-match "X-From-Line: ")))
      ;; Send it.
      (wl-draft-dispatch-message)
      (kill-buffer (current-buffer)))
    (message "Resending message to %s...done" address)))

;;;
(defun wl-summary-remove-destination ()
  (save-excursion
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  (buf (current-buffer))
	  sol eol rs re)
      (beginning-of-line)
      (setq sol (point))
      (search-forward "\r")
      (forward-char -1)
      (setq eol (point))
      (setq rs (next-single-property-change sol 'wl-summary-destination
					    buf eol))
      (setq re (next-single-property-change rs 'wl-summary-destination
					    buf eol))
      (put-text-property rs re 'wl-summary-destination nil)
      (put-text-property rs re 'invisible nil)
      (goto-char re)
      (delete-char (- eol re)))))

(defun wl-summary-collect-numbers-region (begin end)
  "Return a list of message number in the region specified by BEGIN and END."
  (save-excursion
    (save-restriction
      (let (numbers)
	(narrow-to-region (or begin (point-min))(or end (point-max)))
	(goto-char (point-min))
	;; for thread...
	(if (eq wl-summary-buffer-view 'thread)
	    (let (number entity mark-info)
	      (while (not (eobp))
		(setq numbers (cons (wl-summary-message-number) numbers)
		      entity (wl-thread-get-entity number))
		;; When thread is closed...children should also be checked.
		(unless (wl-thread-entity-get-opened entity)
		  (dolist (msg (wl-thread-get-children-msgs number))
		    (setq numbers (cons msg numbers))))
		(forward-line 1)))
	  (let (number mark-info)
	    (while (not (eobp))
	      (setq numbers (cons (wl-summary-message-number) numbers))
	      (forward-line 1))))
	numbers))))

(defun wl-summary-exec (&optional numbers)
  (interactive)
  (let ((failures 0)
	collected pair action modified)
    (dolist (action wl-summary-mark-action-list)
      (setq collected (cons (cons 
			     (wl-summary-action-mark action)
			     nil) collected)))
    (dolist (mark-info wl-summary-buffer-temp-mark-list)
      (if numbers
	  (when (memq (nth 0 mark-info) numbers)
	    (setq pair (assoc (nth 1 mark-info) collected)))
	(setq pair (assoc (nth 1 mark-info) collected)))
      (setq pair (assoc (nth 1 mark-info) collected))
      (setcdr pair (cons mark-info (cdr pair))))
    ;; collected is a pair of
    ;; mark-string and a list of mark-info
    (dolist (pair collected)
      (setq action (assoc (car pair) wl-summary-mark-action-list))
      (when (and (cdr pair) (wl-summary-action-exec-function action))
	(setq modified t)
	(setq failures (+ failures (funcall
				    (wl-summary-action-exec-function action)
				    (cdr pair))))))
    (when modified
      (wl-summary-set-message-modified))
    (run-hooks 'wl-summary-exec-hook)
    ;; message buffer is not up-to-date
    (unless (and wl-message-buffer
		 (eq (wl-summary-message-number)
		     (with-current-buffer wl-message-buffer
		       wl-message-buffer-cur-number)))
      (wl-summary-toggle-disp-msg 'off)
      (setq wl-message-buffer nil))
    (set-buffer-modified-p nil)
    (when (> failures 0)
      (format "%d execution(s) were failed" failures))))

(defun wl-summary-exec-region (beg end)
  (interactive "r")
  (wl-summary-exec
   (wl-summary-collect-numbers-region beg end)))

(defun wl-summary-read-folder (default &optional purpose ignore-error
				no-create init)
  (let ((fld (completing-read
	      (format "Folder name %s(%s): " (or purpose "")
		      default)
	      'wl-folder-complete-folder
	      nil nil (or init wl-default-spec)
	      'wl-read-folder-hist)))
    (if (or (string= fld wl-default-spec)
	    (string= fld ""))
	(setq fld default))
    (setq fld (elmo-string (wl-folder-get-realname fld)))
    (if (string-match "\n" fld)
	(error "Not supported folder name: %s" fld))
    (unless no-create
      (if ignore-error
	  (condition-case nil
	      (wl-folder-confirm-existence
	       (wl-folder-get-elmo-folder
		fld))
	    (error))
	(wl-folder-confirm-existence (wl-folder-get-elmo-folder
				      fld))))
    fld))

(defun wl-summary-print-destination (msg-num folder)
  "Print refile destination on line."
  (when folder
    (wl-summary-remove-destination)
    (save-excursion
      (let ((inhibit-read-only t)
	    (folder (copy-sequence folder))
	    (buffer-read-only nil)
	    len rs re c)
	(setq len (string-width folder))
	(if (< len 1) ()
	  ;;(end-of-line)
	  (beginning-of-line)
	  (search-forward "\r")
	  (forward-char -1)
	  (setq re (point))
	  (setq c 0)
	  (while (< c len)
	    (forward-char -1)
	    (setq c (+ c (char-width (following-char)))))
	  (and (> c len) (setq folder (concat " " folder)))
	  (setq rs (point))
	  (when wl-summary-width
	    (put-text-property rs re 'invisible t))
	  (put-text-property rs re 'wl-summary-destination t)
	  (goto-char re)
	  (wl-highlight-refile-destination-string folder)
	  (insert folder)
	  (set-buffer-modified-p nil))))))

(defsubst wl-summary-reserve-temp-mark-p (mark)
  "Return t if temporal MARK should be reserved."
  (member mark wl-summary-reserve-mark-list))

(defun wl-summary-refile-prev-destination ()
  "Refile message to previously refiled destination."
  (interactive)
  (funcall (symbol-function 'wl-summary-refile)
	   wl-summary-buffer-prev-refile-destination
	   (wl-summary-message-number))
  (if (eq wl-summary-move-direction-downward nil)
      (wl-summary-prev)
    (wl-summary-next)))

(defsubst wl-summary-no-auto-refile-message-p (msg)
  (member (elmo-msgdb-get-mark (wl-summary-buffer-msgdb) msg)
	  wl-summary-auto-refile-skip-marks))

(defun wl-summary-auto-refile (&optional open-all)
  "Set refile mark automatically according to 'wl-refile-guess-by-rule'."
  (interactive "P")
  (message "Marking...")
  (save-excursion
    (if (and (eq wl-summary-buffer-view 'thread)
	     open-all)
	(wl-thread-open-all))
    (let* ((spec (wl-summary-buffer-folder-name))
	   checked-dsts
	   (count 0)
	   number dst thr-entity)
      (goto-line 1)
      (while (not (eobp))
	(setq number (wl-summary-message-number))
	(dolist (number (cons number
			      (and (eq wl-summary-buffer-view 'thread)
				   ;; process invisible children.
				   (not (wl-thread-entity-get-opened
					 (setq thr-entity
					       (wl-thread-get-entity number))))
				   (wl-thread-entity-get-descendant
				    thr-entity))))
	  (when (and (not (wl-summary-no-auto-refile-message-p
			   number))
		     (setq dst
			   (wl-folder-get-realname
			    (wl-refile-guess-by-rule
			     (elmo-msgdb-overview-get-entity
			      number (wl-summary-buffer-msgdb)))))
		     (not (equal dst spec))
		     (let ((pair (assoc dst checked-dsts))
			   ret)
		       (if pair
			   (cdr pair)
			 (setq ret
			       (condition-case nil
				   (progn
				     (wl-folder-confirm-existence
				      (wl-folder-get-elmo-folder dst))
				     t)
				 (error)))
			 (setq checked-dsts (cons (cons dst ret) checked-dsts))
			 ret)))
	    (if (funcall (symbol-function 'wl-summary-refile) number dst)
		(incf count))
	    (message "Marking...%d message(s)." count)))
	(forward-line))
      (if (eq count 0)
	  (message "No message was marked.")
	(message "Marked %d message(s)." count)))))

(defun wl-summary-unmark (&optional number)
  "Unmark marks (temporary, refile, copy, delete)of current line.
If optional argument NUMBER is specified, unmark message specified by NUMBER."
  (interactive)
  (wl-summary-unset-mark number (interactive-p)))

(defun wl-summary-target-mark (&optional number)
  "Put target mark '*' on current message.
If optional argument NUMBER is specified, mark message specified by NUMBER."
  (interactive)
  (wl-summary-set-mark "*" number (interactive-p)))

(defun wl-summary-unmark-region (beg end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if (eq wl-summary-buffer-view 'thread)
	  (progn
	    (while (not (eobp))
	      (let* ((number (wl-summary-message-number))
		     (entity (wl-thread-get-entity number)))
		(if (wl-thread-entity-get-opened entity)
		    ;; opened...unmark line.
		    (wl-summary-unmark)
		  ;; closed
		  (wl-summary-delete-marks-on-buffer
		   (wl-thread-get-children-msgs number))))
	      (forward-line 1)))
	(while (not (eobp))
	  (wl-summary-unmark)
	  (forward-line 1))))))

(defun wl-summary-mark-region-subr (function beg end data)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if (eq wl-summary-buffer-view 'thread)
	  (progn
	    (while (not (eobp))
	      (let* ((number (wl-summary-message-number))
		     (entity (wl-thread-get-entity number))
		     (wl-summary-move-direction-downward t)
		     children)
		(if (wl-thread-entity-get-opened entity)
		    ;; opened...delete line.
		    (funcall function number data)
		  ;; closed
		  (setq children (wl-thread-get-children-msgs number))
		  (while children
		    (funcall function (pop children) data)))
		(forward-line 1))))
	(while (not (eobp))
	  (funcall function (wl-summary-message-number) data)
	  (forward-line 1))))))

(defun wl-summary-target-mark-region (beg end)
  (interactive "r")
  (wl-summary-mark-region-subr 'wl-summary-target-mark beg end nil))

(defun wl-summary-target-mark-all ()
  (interactive)
  (wl-summary-target-mark-region (point-min) (point-max))
  (setq wl-summary-buffer-target-mark-list
	(mapcar 'car
		(elmo-msgdb-get-number-alist (wl-summary-buffer-msgdb)))))

(defun wl-summary-delete-all-mark (mark)
  (goto-char (point-min))
  (while (not (eobp))
    (when (string= (wl-summary-temp-mark) mark)
      (wl-summary-unmark))
    (forward-line 1))
  (let (deleted)
    (dolist (mark-info wl-summary-buffer-temp-mark-list)
      (when (string= (nth 1 mark-info) mark)
	(setq deleted (cons mark-info deleted))))
    (dolist (delete deleted)
      (setq wl-summary-buffer-temp-mark-list
	    (delq delete wl-summary-buffer-temp-mark-list)))))

(defun wl-summary-unmark-all ()
  "Unmark all according to what you input."
  (interactive)
  (let ((unmarks (string-to-char-list (read-from-minibuffer "Unmark: ")))
	cur-mark)
    (save-excursion
      (while unmarks
	(setq cur-mark (char-to-string (car unmarks)))
	(wl-summary-delete-all-mark cur-mark)
	(setq unmarks (cdr unmarks))))))

(defun wl-summary-target-mark-thread ()
  (interactive)
  (wl-thread-call-region-func 'wl-summary-target-mark-region t))

(require 'product)
(product-provide (provide 'wl-action) (require 'wl-version))

;;; wl-action.el ends here
