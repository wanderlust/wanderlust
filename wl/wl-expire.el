;;; wl-expire.el -- Message expire modules for Wanderlust.

;; Copyright 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;;                          Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Masahiro MURATA <muse@ba2.so-net.ne.jp>
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

(require 'wl-summary)
(require 'wl-thread)
(require 'wl-folder)

;;; Code:

(eval-when-compile
  (require 'wl-util)
  (require 'elmo-archive))

;; Variables

(defvar wl-expired-alist nil)
(defvar wl-expired-alist-file-name "expired-alist")
(defvar wl-expired-log-alist nil)
(defvar wl-expired-log-alist-file-name "expired-log")

(defun wl-expired-alist-load ()
  (elmo-object-load (expand-file-name
		     wl-expired-alist-file-name
		     elmo-msgdb-dir)))

(defun wl-expired-alist-save (&optional alist)
  (elmo-object-save (expand-file-name
		     wl-expired-alist-file-name
		     elmo-msgdb-dir)
		    (or alist wl-expired-alist)))

(defsubst wl-expire-msg-p (msg-num mark-alist)
  (cond ((consp wl-summary-expire-reserve-marks)
	 (let ((mark (nth 1 (assq msg-num mark-alist))))
	   (not (or (member mark wl-summary-expire-reserve-marks)
		    (and wl-summary-buffer-disp-msg
			 (eq msg-num wl-summary-buffer-current-msg))))))
	((eq wl-summary-expire-reserve-marks 'all)
	 (not (or (assq msg-num mark-alist)
		  (and wl-summary-buffer-disp-msg
		       (eq msg-num wl-summary-buffer-current-msg)))))
	((eq wl-summary-expire-reserve-marks 'none)
	 t)
	(t
	 (error "invalid marks: %s" wl-summary-expire-reserve-marks))))

(defmacro wl-expire-make-sortable-date (date)
  (` (timezone-make-sortable-date
      (aref (, date) 0) (aref (, date) 1) (aref (, date) 2)
      (timezone-make-time-string
       (aref (, date) 3) (aref (, date) 4) (aref (, date) 5)))))

(defsubst wl-expire-date-p (key-datevec date)
  (let ((datevec (condition-case nil
		     (timezone-fix-time date nil nil)
		   (error nil))))
    (and
     datevec (> (aref datevec 1) 0)
     (string<
      (wl-expire-make-sortable-date datevec)
      (wl-expire-make-sortable-date key-datevec)))))

(defun wl-expire-delete-reserve-marked-msgs-from-list (msgs mark-alist)
  (let ((dlist msgs))
    (while dlist
      (unless (wl-expire-msg-p (car dlist) mark-alist)
	(setq msgs (delq (car dlist) msgs)))
      (setq dlist (cdr dlist)))
    msgs))

(defun wl-expire-delete (folder delete-list msgdb &optional no-reserve-marks)
  "Delete message for expire."
  (unless no-reserve-marks
    (setq delete-list
	  (wl-expire-delete-reserve-marked-msgs-from-list
	   delete-list (elmo-msgdb-get-mark-alist msgdb))))
  (when delete-list
   (let ((mess
	 (format "Expiring (delete) %s msgs..."
		 (length delete-list))))
    (message "%s" mess)
    (if (elmo-delete-msgs folder
			  delete-list
			  msgdb)
	(progn
	  (elmo-msgdb-delete-msgs folder
				  delete-list
				  msgdb
				  t)
	  (wl-expire-append-log folder delete-list nil 'delete)
	  (message "%s" (concat mess "done")))
      (error (concat mess "failed!")))))
  (cons delete-list (length delete-list)))

(defun wl-expire-refile (folder refile-list msgdb dst-folder
				&optional no-reserve-marks preserve-number copy)
  "Refile message for expire. If COPY is non-nil, copy message."
  (when (not (string= folder dst-folder))
    (unless no-reserve-marks
      (setq refile-list
	    (wl-expire-delete-reserve-marked-msgs-from-list
	     refile-list (elmo-msgdb-get-mark-alist msgdb))))
    (when refile-list
     (let* ((doingmes (if copy
			 "Copying %s"
		       "Expiring (move %s)"))
	   (mess (format (concat doingmes " %s msgs...")
			 dst-folder (length refile-list))))
      (message "%s" mess)
      (unless (or (elmo-folder-exists-p dst-folder)
		  (elmo-create-folder dst-folder))
	(error "%s: create folder failed" dst-folder))
      (if wl-expire-add-seen-list
	  (elmo-msgdb-add-msgs-to-seen-list
	   dst-folder
	   refile-list
	   msgdb
	   (concat wl-summary-important-mark
		   wl-summary-read-uncached-mark)))
      (if (elmo-move-msgs folder
			  refile-list
			  dst-folder
			  msgdb
			  nil nil t
			  copy
			  preserve-number)
	  (progn
	    (wl-expire-append-log folder refile-list dst-folder (if copy 'copy 'move))
	    (message "%s" (concat mess "done")))
	(error (concat mess "failed!")))))
    (cons refile-list (length refile-list))))

(defun wl-expire-refile-with-copy-reserve-msg
  (folder refile-list msgdb dst-folder
	  &optional no-reserve-marks preserve-number copy)
  "Refile message for expire.
If REFILE-LIST includes reserve mark message, so copy."
  (when (not (string= folder dst-folder))
    (let ((msglist refile-list)
	  (mark-alist (elmo-msgdb-get-mark-alist msgdb))
	  (number-alist (elmo-msgdb-get-number-alist msgdb))
	  (ret-val t)
	  (copy-reserve-message)
	  (copy-len 0)
	  msg msg-id)
      (message "Expiring (move %s) %s msgs..."
	       dst-folder (length refile-list))
      (unless (or (elmo-folder-exists-p dst-folder)
		  (elmo-create-folder dst-folder))
	(error "%s: create folder failed" dst-folder))
      (while (setq msg (wl-pop msglist))
	(unless (wl-expire-msg-p msg mark-alist)
	  (setq msg-id (cdr (assq msg number-alist)))
	  (if (assoc msg-id wl-expired-alist)
	      ;; reserve mark message already refiled or expired
	      (setq refile-list (delq msg refile-list))
	    ;; reserve mark message not refiled
	    (wl-append wl-expired-alist (list (cons msg-id dst-folder)))
	    (setq copy-reserve-message t))))
      (when refile-list
	(if wl-expire-add-seen-list
	    (elmo-msgdb-add-msgs-to-seen-list
	     dst-folder
	     refile-list
	     msgdb
	     (concat wl-summary-important-mark
		     wl-summary-read-uncached-mark)))
	(unless
	    (setq ret-val
		  (elmo-move-msgs folder
				  refile-list
				  dst-folder
				  msgdb
				  nil nil t
				  copy-reserve-message
				  preserve-number))
	  (error "expire: move msgs to %s failed" dst-folder))
	(wl-expire-append-log folder refile-list dst-folder
			   (if copy-reserve-message 'copy 'move))
	(setq copy-len (length refile-list))
	(when copy-reserve-message
	  (setq refile-list
		(wl-expire-delete-reserve-marked-msgs-from-list
		 refile-list
		 mark-alist))
	  (when refile-list
	   (if (setq ret-val
		    (elmo-delete-msgs folder
				      refile-list
				      msgdb))
	      (progn
		(elmo-msgdb-delete-msgs folder
					refile-list
					msgdb
					t)
		(wl-expire-append-log folder refile-list nil 'delete))))))
      (let ((mes (format "Expiring (move %s) %s msgs..."
			 dst-folder (length refile-list))))
	(if ret-val
	    (message (concat mes "done"))
	  (error (concat mes "failed!"))))
      (cons refile-list copy-len))))

(defun wl-expire-archive-get-folder (src-folder &optional fmt)
  "Get archive folder name from src-folder."
  (let* ((spec (elmo-folder-get-spec src-folder))
	 (fmt (or fmt wl-expire-archive-folder-name-fmt))
	 (archive-spec (char-to-string
			(car (rassq 'archive elmo-spec-alist))))
	 dst-folder-base dst-folder-fmt prefix)
    (cond ((eq (car spec) 'localdir)
	   (setq dst-folder-base (concat archive-spec (nth 1 spec))))
	  ((stringp (nth 1 spec))
	   (setq dst-folder-base
		 (elmo-concat-path (format "%s%s" archive-spec (car spec))
				   (nth 1 spec))))
	  (t
	   (setq dst-folder-base
		 (elmo-concat-path (format "%s%s" archive-spec (car spec))
				   (elmo-replace-msgid-as-filename
				    src-folder)))))
    (setq dst-folder-fmt (format fmt
				 dst-folder-base
				 wl-expire-archive-folder-type))
    (setq dst-folder-base (format "%s;%s"
				  dst-folder-base
				  wl-expire-archive-folder-type))
    (when (and wl-expire-archive-folder-prefix
	       (stringp (nth 1 spec)))
      (cond ((eq wl-expire-archive-folder-prefix 'short)
	     (setq prefix (file-name-nondirectory (nth 1 spec))))
	    (t
	     (setq prefix (nth 1 spec))))
      (setq dst-folder-fmt (concat dst-folder-fmt ";" prefix))
      (setq dst-folder-base (concat dst-folder-base ";" prefix)))
    (cons dst-folder-base dst-folder-fmt)))

(defsubst wl-expire-archive-get-max-number (dst-folder-base &optional regexp)
  (let ((files (reverse (sort (elmo-list-folders dst-folder-base)
			      'string<)))
	(regexp (or regexp wl-expire-archive-folder-num-regexp))
	filenum in-folder)
    (catch 'done
      (while files
	(when (string-match regexp (car files))
	  (setq filenum (elmo-match-string 1 (car files)))
	  (setq in-folder (elmo-max-of-folder (car files)))
	  (throw 'done (cons in-folder filenum)))
	(setq files (cdr files))))))

(defun wl-expire-archive-number-delete-old (dst-folder-base
					    preserve-number msgs mark-alist
					    &optional no-confirm regexp file)
  (let ((len 0) (max-num 0)
	folder-info dels)
    (if (or (and file (setq folder-info
			    (cons (elmo-max-of-folder file) nil)))
	    (setq folder-info (wl-expire-archive-get-max-number dst-folder-base
								regexp)))
	(progn
	  (setq len (cdar folder-info))
	  (when preserve-number
	    ;; delete small number than max number of dst-folder
	    (setq max-num (caar folder-info))
	    (while (and msgs (>= max-num (car msgs)))
	      (wl-append dels (list (car msgs)))
	      (setq msgs (cdr msgs)))
	    (setq dels (wl-expire-delete-reserve-marked-msgs-from-list
			dels mark-alist))
	    (unless (and dels
			 (or (or no-confirm (not wl-expire-delete-oldmsg-confirm))
			     (progn
			       (if (eq major-mode 'wl-summary-mode)
				   (wl-thread-jump-to-msg (car dels)))
			       (y-or-n-p (format "Delete old messages %s? "
						 dels)))))
	      (setq dels nil)))
	  (list msgs dels max-num (cdr folder-info) len))
      (list msgs dels 0 "0" 0))))

(defun wl-expire-archive-number1 (folder delete-list msgdb
					 &optional preserve-number no-delete)
  "Standard function for `wl-summary-expire'.
Refile to archive folder followed message number."
  (let* ((elmo-archive-treat-file t)	;; treat archive folder as a file.
	 (dst-folder-fmt (funcall
			  wl-expire-archive-get-folder-func folder))
	 (dst-folder-base (car dst-folder-fmt))
	 (dst-folder-fmt (cdr dst-folder-fmt))
	 (refile-func (if no-delete
			  'wl-expire-refile
			'wl-expire-refile-with-copy-reserve-msg))
	 tmp dels dst-folder
	 prev-arcnum arcnum msg arcmsg-list
	 deleted-list ret-val)
    (setq tmp (wl-expire-archive-number-delete-old
	       dst-folder-base preserve-number delete-list
	       (elmo-msgdb-get-mark-alist msgdb)
	       no-delete))
    (when (and (not no-delete)
	       (setq dels (nth 1 tmp)))
      (wl-append deleted-list (car (wl-expire-delete folder dels msgdb))))
    (setq delete-list (car tmp))
    (catch 'done
      (while t
	(if (setq msg (wl-pop delete-list))
	    (setq arcnum (/ msg wl-expire-archive-files))
	  (setq arcnum nil))
	(when (and prev-arcnum
		   (not (eq arcnum prev-arcnum)))
	  (setq dst-folder (format dst-folder-fmt
				   (* prev-arcnum wl-expire-archive-files)))
	  (and (setq ret-val
		     (funcall
		      refile-func
		      folder arcmsg-list msgdb dst-folder t preserve-number
		      no-delete))
	       (wl-append deleted-list (car ret-val)))
	  (setq arcmsg-list nil))
	(if (null msg)
	    (throw 'done t))
	(wl-append arcmsg-list (list msg))
	(setq prev-arcnum arcnum)))
    deleted-list
    ))

(defun wl-expire-archive-number2 (folder delete-list msgdb
					 &optional preserve-number no-delete)
  "Standard function for `wl-summary-expire'.
Refile to archive folder followed the number of message in one archive folder."
  (let* ((elmo-archive-treat-file t)	;; treat archive folder as a file.
	 (dst-folder-fmt (funcall
			  wl-expire-archive-get-folder-func folder))
	 (dst-folder-base (car dst-folder-fmt))
	 (dst-folder-fmt (cdr dst-folder-fmt))
	 (refile-func (if no-delete
			  'wl-expire-refile
			'wl-expire-refile-with-copy-reserve-msg))
	 (len 0) (filenum 0)
	 tmp dels dst-folder
	 arc-len msg arcmsg-list
	 deleted-list ret-val)
    (setq tmp (wl-expire-archive-number-delete-old
	       dst-folder-base preserve-number delete-list
	       (elmo-msgdb-get-mark-alist msgdb)
	       no-delete))
    (when (and (not no-delete)
	       (setq dels (nth 1 tmp)))
      (wl-append deleted-list (car (wl-expire-delete folder dels msgdb))))
    (setq delete-list (car tmp)
	  filenum (string-to-int (nth 3 tmp))
	  len (nth 4 tmp)
	  arc-len len)
    (catch 'done
      (while t
	(if (setq msg (wl-pop delete-list))
	    (setq len (1+ len))
	  (setq len (1+ wl-expire-archive-files)))
	(when (> len wl-expire-archive-files)
	  (when arcmsg-list
	    (setq dst-folder (format dst-folder-fmt filenum))
	    (and (setq ret-val
		       (funcall
			refile-func
			folder arcmsg-list msgdb dst-folder t preserve-number
			no-delete))
		 (wl-append deleted-list (car ret-val)))
	    (setq arc-len (+ arc-len (cdr ret-val))))
	  (setq arcmsg-list nil)
	  (if (< arc-len wl-expire-archive-files)
	      (setq len (1+ arc-len))
	    (setq filenum (+ filenum wl-expire-archive-files)
		  len (- len arc-len)	;; maybe 1
		  arc-len (1- len)	;; maybe 0
		  )))
	(if (null msg)
	    (throw 'done t))
	(wl-append arcmsg-list (list msg))))
    deleted-list
    ))

(defun wl-expire-archive-date (folder delete-list msgdb
				      &optional preserve-number no-delete)
  "Standard function for `wl-summary-expire'.
Refile to archive folder followed message date."
  (let* ((elmo-archive-treat-file t)	;; treat archive folder as a file.
	 (number-alist (elmo-msgdb-get-number-alist msgdb))
	 (overview (elmo-msgdb-get-overview msgdb))
	 (dst-folder-fmt (funcall
			  wl-expire-archive-get-folder-func
			  folder
			  wl-expire-archive-date-folder-name-fmt
			  ))
	 (dst-folder-base (car dst-folder-fmt))
	 (dst-folder-fmt (cdr dst-folder-fmt))
	 (refile-func (if no-delete
			  'wl-expire-refile
			'wl-expire-refile-with-copy-reserve-msg))
	 tmp dels dst-folder date time
	 msg arcmsg-alist arcmsg-list
	 deleted-list ret-val)
    (setq tmp (wl-expire-archive-number-delete-old
	       dst-folder-base preserve-number delete-list
	       (elmo-msgdb-get-mark-alist msgdb)
	       no-delete
	       wl-expire-archive-date-folder-num-regexp))
    (when (and (not no-delete)
	       (setq dels (nth 1 tmp)))
      (wl-append deleted-list (car (wl-expire-delete folder dels msgdb))))
    (setq delete-list (car tmp))
    (while (setq msg (wl-pop delete-list))
      (setq date (elmo-msgdb-overview-entity-get-date
		  (assoc (cdr (assq msg number-alist)) overview)))
      (setq time
	    (condition-case nil
		(timezone-fix-time date nil nil)
	      (error [0 0 0 0 0 0 0])))
      (if (= (aref time 1) 0)	;; if (month == 0)
	  (aset time 0 0))	;;    year = 0
      (setq dst-folder (format dst-folder-fmt
			       (aref time 0)  ;; year
			       (aref time 1)  ;; month
			       ))
      (setq arcmsg-alist
	    (wl-append-assoc-list
	     dst-folder
	     msg
	     arcmsg-alist)))
    (while arcmsg-alist
      (setq dst-folder (caar arcmsg-alist))
      (setq arcmsg-list (cdar arcmsg-alist))
      (and (setq ret-val
		 (funcall
		  refile-func
		  folder arcmsg-list msgdb dst-folder t preserve-number
		  no-delete))
	   (wl-append deleted-list (car ret-val)))
      (setq arcmsg-alist (cdr arcmsg-alist)))
    deleted-list
    ))

(defsubst wl-expire-folder-p (folder)
  (wl-get-assoc-list-value wl-expire-alist folder))

(defun wl-summary-expire (&optional folder-name notsummary nolist)
  (interactive)
  (let ((folder (or folder-name wl-summary-buffer-folder-name))
	(alist wl-expire-alist)
	expires)
    (when (and (or (setq expires (wl-expire-folder-p folder))
		   (progn (and (interactive-p)
			       (message "no match %s in wl-expire-alist"
					folder))
			  nil))
	       (or (not (interactive-p))
		   (y-or-n-p (format "Expire %s? " folder))))
      (let* ((msgdb (or wl-summary-buffer-msgdb
			(elmo-msgdb-load folder)))
	     (number-alist (elmo-msgdb-get-number-alist msgdb))
	     (mark-alist (elmo-msgdb-get-mark-alist msgdb))
	     expval rm-type val-type value more args
	     delete-list)
	(save-excursion
	  (setq expval (car expires)
		rm-type (nth 1 expires)
		args (cddr expires))
	  (setq val-type (car expval)
		value (nth 1 expval)
		more (nth 2 expval))
	  (run-hooks 'wl-summary-expire-pre-hook)
	  (cond
	   ((eq val-type nil))
	   ((eq val-type 'number)
	    (let* ((msgs (if (not nolist)
			     (elmo-list-folder folder)
			   (mapcar 'car number-alist)))
		   (msglen (length msgs))
		   (more (or more (1+ value)))
		   count)
	      (when (>= msglen more)
		(setq count (- msglen value))
		(while (and msgs (> count 0))
		  (when (assq (car msgs) number-alist) ;; don't expire new message
		    (wl-append delete-list (list (car msgs)))
		    (when (or (not wl-expire-number-with-reserve-marks)
			      (wl-expire-msg-p (car msgs) mark-alist))
		      (setq count (1- count))))
		  (setq msgs (cdr msgs))))))
	   ((eq val-type 'date)
	    (let* ((overview (elmo-msgdb-get-overview msgdb))
		   (key-date (elmo-date-get-offset-datevec
			      (timezone-fix-time (current-time-string)
						 (current-time-zone) nil)
			      value t)))
	      (while overview
		(when (wl-expire-date-p
		       key-date
		       (elmo-msgdb-overview-entity-get-date
			(car overview)))
		  (wl-append delete-list
			     (list (elmo-msgdb-overview-entity-get-number
				    (car overview)))))
		(setq overview (cdr overview)))))
	   (t
	    (error "%s: not supported" val-type)))
	  (when delete-list
	    (or wl-expired-alist
		(setq wl-expired-alist (wl-expired-alist-load)))
	    (setq delete-list
		  (cond ((eq rm-type nil) nil)
			((eq rm-type 'remove)
			 (car (wl-expire-delete folder delete-list msgdb)))
			((eq rm-type 'trash)
			 (car (wl-expire-refile folder delete-list msgdb wl-trash-folder)))
			((stringp rm-type)
			 (car (wl-expire-refile folder delete-list msgdb rm-type)))
			((fboundp rm-type)
			 (apply rm-type (append (list folder delete-list msgdb)
						args)))
			(t
			 (error "%s: invalid type" rm-type))))
	    (when (and (not notsummary) delete-list)
	      (wl-summary-delete-messages-on-buffer delete-list t)
	      (wl-summary-folder-info-update)
	      (wl-summary-set-message-modified)
	      (wl-summary-set-mark-modified)
	      (sit-for 0)
	      (set-buffer-modified-p nil))
	    (wl-expired-alist-save))
	  (run-hooks 'wl-summary-expire-hook)
	  (if delete-list
	      (message "Expiring %s is done" folder)
	    (and (interactive-p)
		 (message "No expire"))))
	delete-list
	))))

(defun wl-folder-expire-entity (entity)
  (cond
   ((consp entity)
    (let ((flist (nth 2 entity)))
      (while flist
	(wl-folder-expire-entity (car flist))
	(setq flist (cdr flist)))))
   ((stringp entity)
    (when (wl-expire-folder-p entity)
      (let ((update-msgdb (cond
			   ((consp wl-expire-folder-update-msgdb)
			    (wl-string-match-member
			     entity
			     wl-expire-folder-update-msgdb))
			   (t
			    wl-expire-folder-update-msgdb)))
	    (wl-summary-highlight (if (or (wl-summary-sticky-p entity)
					  (wl-summary-always-sticky-folder-p
					   entity))
				      wl-summary-highlight))
	    wl-auto-select-first ret-val)
	(save-window-excursion
	  (save-excursion
	    (and update-msgdb
		 (wl-summary-goto-folder-subr entity 'force-update nil))
	    (setq ret-val (wl-summary-expire entity (not update-msgdb)))
	    (if update-msgdb
		(wl-summary-save-status 'keep)
	      (if ret-val
		  (wl-folder-check-entity entity))))))))))

;; Command

(defun wl-folder-expire-current-entity ()
  (interactive)
  (let ((entity-name
	 (or (wl-folder-get-folder-name-by-id
	      (get-text-property (point) 'wl-folder-entity-id))
	     (wl-folder-get-realname (wl-folder-folder-name)))))
    (when (and entity-name
	       (or (not (interactive-p))
		   (y-or-n-p (format "Expire %s? " entity-name))))
      (wl-folder-expire-entity
       (wl-folder-search-entity-by-name entity-name
					wl-folder-entity))
      (if (get-buffer wl-summary-buffer-name)
	  (kill-buffer wl-summary-buffer-name))
      (message "Expiring %s is done" entity-name))))

;;; Archive

(defun wl-folder-archive-current-entity ()
  (interactive)
  (let ((entity-name
	 (or (wl-folder-get-folder-name-by-id
	      (get-text-property (point) 'wl-folder-entity-id))
	     (wl-folder-get-realname (wl-folder-folder-name)))))
    (when (and entity-name
	       (or (not (interactive-p))
		   (y-or-n-p (format "Archive %s? " entity-name))))
      (wl-folder-archive-entity
       (wl-folder-search-entity-by-name entity-name
					wl-folder-entity))
      (message "Archiving %s is done" entity-name))))

(defun wl-archive-number1 (folder archive-list msgdb)
  (wl-expire-archive-number1 folder archive-list msgdb t t))

(defun wl-archive-number2 (folder archive-list msgdb)
  (wl-expire-archive-number2 folder archive-list msgdb t t))

(defun wl-archive-date (folder archive-list msgdb)
  (wl-expire-archive-date folder archive-list msgdb t t))

(defun wl-archive-folder (folder archive-list msgdb dst-folder)
  (let* ((elmo-archive-treat-file t)	;; treat archive folder as a file.
	 copied-list ret-val)
    (setq archive-list
	  (car (wl-expire-archive-number-delete-old
		nil t archive-list
		(elmo-msgdb-get-mark-alist msgdb)
		t ;; no-confirm
		nil dst-folder)))
    (when archive-list
      (and (setq ret-val
		 (wl-expire-refile
		  folder archive-list msgdb dst-folder t t t)) ;; copy!!
	   (wl-append copied-list ret-val)))
    copied-list
    ))

(defun wl-summary-archive (&optional arg folder-name notsummary nolist)
  (interactive "P")
  (let* ((folder (or folder-name wl-summary-buffer-folder-name))
	 (msgdb (or wl-summary-buffer-msgdb
		    (elmo-msgdb-load folder)))
	 (msgs (if (not nolist)
		   (elmo-list-folder folder)
		 (mapcar 'car (elmo-msgdb-get-number-alist msgdb))))
	 (alist wl-archive-alist)
	 func dst-folder archive-list)
    (if arg
	(let ((wl-default-spec (char-to-string
				(car (rassq 'archive elmo-spec-alist)))))
	  (setq dst-folder (wl-summary-read-folder
			    (concat wl-default-spec (substring folder 1))
			    "for archive"))))
    (run-hooks 'wl-summary-archive-pre-hook)
    (if dst-folder
	(wl-archive-folder folder msgs msgdb dst-folder)
      (when (and (catch 'match
		   (while alist
		     (when (string-match (caar alist) folder)
		       (setq func (cadar alist))
		       (throw 'match t))
		     (setq alist (cdr alist)))
		   (and (interactive-p)
			(message "No match %s in wl-archive-alist" folder))
		   (throw 'match nil))
		 (or (not (interactive-p))
		     (y-or-n-p (format "Archive %s? " folder))))
	(setq archive-list
	      (funcall func folder msgs msgdb))
	(run-hooks 'wl-summary-archive-hook)
	(if archive-list
	    (message "Archiving %s is done" folder)
	  (and (interactive-p)
	       (message "No archive")))))))

(defun wl-folder-archive-entity (entity)
  (cond
   ((consp entity)
    (let ((flist (nth 2 entity)))
      (while flist
	(wl-folder-archive-entity (car flist))
	(setq flist (cdr flist)))))
   ((stringp entity)
    (wl-summary-archive nil entity t))))

;; append log

(defun wl-expire-append-log (src-folder msgs dst-folder action)
  (when wl-expire-use-log
    (save-excursion
      (let ((tmp-buf (get-buffer-create " *wl-expire work*"))
	    (filename (expand-file-name wl-expired-log-alist-file-name
					elmo-msgdb-dir)))
	(set-buffer tmp-buf)
	(erase-buffer)
	(if dst-folder
	    (insert (format "%s\t%s -> %s\t%s\n"
			    action
			    src-folder dst-folder msgs))
	  (insert (format "%s\t%s\t%s\n"
			  action
			  src-folder msgs)))
	(if (file-writable-p filename)
	    (write-region (point-min) (point-max)
			  filename t 'no-msg)
	  (message (format "%s is not writable." filename)))
	(kill-buffer tmp-buf)))))

(provide 'wl-expire)

;;; wl-expire.el ends here
