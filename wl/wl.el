;;; wl.el -- Wanderlust bootstrap.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA <muse@ba2.so-net.ne.jp>
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

(require 'elmo2)
;; from x-face.el
(unless (and (fboundp 'defgroup)
             (fboundp 'defcustom))
  (require 'backquote)
  (defmacro defgroup (&rest args))
  (defmacro defcustom (symbol value &optional doc &rest args)
    (let ((doc (concat "*" (or doc ""))))
      (` (defvar (, symbol) (, value) (, doc))))))

(require 'wl-vars)
(require 'wl-util)
(require 'wl-version)

(cond (wl-on-xemacs
       (require 'wl-xmas))
      (wl-on-emacs21
       (require 'wl-e21))
      (wl-on-nemacs
       (require 'wl-nemacs))
      (t
       (require 'wl-mule)))

(provide 'wl) ; circular dependency
(require 'wl-folder)
(require 'wl-summary)
(require 'wl-thread)
(require 'wl-address)

(wl-draft-mode-setup)
(require 'wl-draft)
(wl-draft-key-setup)

(require 'wl-demo)
(require 'wl-highlight)

(eval-when-compile
  (require 'smtp)
  (require 'wl-score)
  (unless wl-on-nemacs
    (require 'wl-fldmgr))
  (if wl-use-semi
      (require 'wl-mime)
    (require 'tm-wl)))

(defun wl-plugged-init (&optional make-alist)
  (setq elmo-plugged wl-plugged)
  (if wl-reset-plugged-alist
      (elmo-set-plugged elmo-plugged))
  (when make-alist
    (wl-make-plugged-alist))
  ;; Plug status.
  (setq elmo-plugged (setq wl-plugged (elmo-plugged-p))
	wl-modeline-plug-status wl-plugged)
  (if wl-plugged
      (wl-toggle-plugged t 'flush)))

(defun wl-toggle-plugged (&optional arg queue-flush-only)
  (interactive)
  (elmo-quit) ; Disconnect current connection.
  (unless queue-flush-only
    (cond
     ((eq arg 'on)
      (setq wl-plugged t))
     ((eq arg 'off)
      (setq wl-plugged nil))
     (t (setq wl-plugged (null wl-plugged))))
    (elmo-set-plugged wl-plugged))
  (setq elmo-plugged wl-plugged
	wl-modeline-plug-status wl-plugged)
  (save-excursion
    (let ((summaries (wl-collect-summary)))
      (while summaries
	(set-buffer (pop summaries))
	(wl-summary-msgdb-save)
	;; msgdb is saved, but cache is not saved yet.
	(wl-summary-set-message-modified))))
  (setq wl-biff-check-folders-running nil)
  (if wl-plugged
      (progn
	;; flush queue!!
	(elmo-dop-queue-flush)
	(unless queue-flush-only (wl-biff-start))
	(if (and wl-draft-enable-queuing
		 wl-auto-flush-queue)
	    (wl-draft-queue-flush))
	(when (and (eq major-mode 'wl-summary-mode)
		   (elmo-folder-plugged-p wl-summary-buffer-folder-name))
	  (let* ((msgdb-dir (elmo-msgdb-expand-path
			     wl-summary-buffer-folder-name))
		 (seen-list (elmo-msgdb-seen-load msgdb-dir)))
	    (setq seen-list
		  (wl-summary-flush-pending-append-operations seen-list))
	    (elmo-msgdb-seen-save msgdb-dir seen-list)))
	(run-hooks 'wl-plugged-hook))
    (wl-biff-stop)
    (run-hooks 'wl-unplugged-hook))
  (force-mode-line-update t))

;;; wl-plugged-mode

(defvar wl-plugged-port-label-alist
  (list (cons elmo-default-nntp-port "nntp")
	(cons elmo-default-imap4-port "imap4")
	(cons elmo-default-pop3-port "pop3")))
	;;(cons elmo-pop-before-smtp-port "pop3")

(defconst wl-plugged-switch-variables
  '(("Queuing" . wl-draft-enable-queuing)
    ("AutoFlushQueue" . wl-auto-flush-queue)
    ("DisconnectedOperation" . elmo-enable-disconnected-operation)))

(defvar wl-plugged-buf-name "Plugged")
(defvar wl-plugged-mode-map nil)
(defvar wl-plugged-alist nil)
(defvar wl-plugged-switch nil)
(defvar wl-plugged-winconf nil)
(defvar wl-plugged-sending-queue-alist nil)
(defvar wl-plugged-dop-queue-alist nil)
(defvar wl-plugged-alist-modified nil)

(defvar wl-plugged-mode-menu-spec
  '("Plugged"
    ["Toggle plugged" wl-plugged-toggle t]
    ["Toggle All plugged" wl-plugged-toggle-all t]
    ["Prev Port"      wl-plugged-move-to-previous t]
    ["Next Port"      wl-plugged-move-to-next t]
    ["Prev Server"    wl-plugged-move-to-previous-server t]
    ["Next Server"    wl-plugged-move-to-next-server t]
    ["Flush queue"    wl-plugged-flush-queue t]
    "----"
    ["Exit"           wl-plugged-exit t]))

(eval-and-compile
  (if wl-on-xemacs
      (defun wl-plugged-setup-mouse ()
	(define-key wl-plugged-mode-map 'button2 'wl-plugged-click))
    (if wl-on-nemacs
	(defun wl-plugged-setup-mouse ())
      (defun wl-plugged-setup-mouse ()
	(define-key wl-plugged-mode-map [mouse-2] 'wl-plugged-click)))))

(unless wl-plugged-mode-map
  (setq wl-plugged-mode-map (make-sparse-keymap))
  (define-key wl-plugged-mode-map " "    'wl-plugged-toggle)
  (define-key wl-plugged-mode-map "\C-m" 'wl-plugged-toggle)
  (define-key wl-plugged-mode-map "\M-t" 'wl-plugged-toggle-all)
  (define-key wl-plugged-mode-map "q"    'wl-plugged-exit)
  (define-key wl-plugged-mode-map "\C-t" 'wl-plugged-exit)
  (define-key wl-plugged-mode-map "F"    'wl-plugged-flush-queue)
  (define-key wl-plugged-mode-map "P"    'wl-plugged-move-to-previous-server)
  (define-key wl-plugged-mode-map "N"    'wl-plugged-move-to-next-server)
  (define-key wl-plugged-mode-map "p"    'wl-plugged-move-to-previous)
  (define-key wl-plugged-mode-map "n"    'wl-plugged-move-to-next)
  (define-key wl-plugged-mode-map "\e\t" 'wl-plugged-move-to-previous)
  (define-key wl-plugged-mode-map "\t"   'wl-plugged-move-to-next)
  (wl-plugged-setup-mouse)
  (easy-menu-define
   wl-plugged-mode-menu
   wl-plugged-mode-map
   "Menu used in Plugged mode."
   wl-plugged-mode-menu-spec))

(defun wl-plugged-mode ()
  "Mode for setting Wanderlust plugged.
See info under Wanderlust for full documentation.

Special commands:
\\{wl-plugged-mode-map}

Entering Plugged mode calls the value of `wl-plugged-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map wl-plugged-mode-map)
  (setq major-mode 'wl-plugged-mode)
  (setq mode-name "Plugged")
  (easy-menu-add wl-plugged-mode-menu)
  (wl-mode-line-buffer-identification)
  (setq wl-plugged-switch wl-plugged)
  (setq wl-plugged-alist-modified nil)
  (setq buffer-read-only t)
  (run-hooks 'wl-plugged-mode-hook))

(defmacro wl-plugged-string (plugged &optional time)
  (` (if (, time) wl-plugged-auto-off
       (if (, plugged) wl-plugged-plug-on wl-plugged-plug-off))))

(defmacro wl-plugged-server-indent ()
  (` (make-string wl-plugged-server-indent ? )))

(defun wl-plugged-set-variables ()
  (setq wl-plugged-sending-queue-alist
	(wl-plugged-sending-queue-info))
  (setq wl-plugged-dop-queue-alist
	(wl-plugged-dop-queue-info))
  (setq wl-plugged-alist
	(sort (copy-sequence elmo-plugged-alist)
	      '(lambda (a b)
		 (string< (caar a) (caar b))))))

(defun wl-plugged-sending-queue-info ()
  ;; sending queue status
  (let (alist msgs sent-via server port)
    (setq msgs (elmo-list-folder wl-queue-folder))
    (while msgs
      (setq sent-via (wl-draft-queue-info-operation (car msgs) 'get-sent-via))
      (while sent-via
	(when (eq (nth 1 (car sent-via)) 'unplugged)
	  (setq server (car (nth 2 (car sent-via)))
		port (cdr (nth 2 (car sent-via))))
	  (elmo-plugged-p server port)	;; add elmo-plugged-alist if nothing.
	  (setq alist
		(wl-append-assoc-list
		 (cons server port)
		 (car msgs)
		 alist)))
	(setq sent-via (cdr sent-via)))
      (setq msgs (cdr msgs)))
    alist))

(defun wl-plugged-sending-queue-status (qinfo)
  ;; sending queue status
  (let ((len (length (cdr qinfo))))
    (concat (wl-plugged-set-folder-icon
	     wl-queue-folder
	     (wl-folder-get-petname wl-queue-folder))
	    (if (> len 1)
		(format ": %d msgs (" len)
	      (format ": %d msg (" len))
	    (mapconcat (function int-to-string) (cdr qinfo) ",")
	    ")")))

(defun wl-plugged-dop-queue-info ()
  ;; dop queue status
  (let* ((count 0)
	 elmo-dop-queue dop-queue last alist server-info
	 ope operation)
    (elmo-dop-queue-load)
    (elmo-dop-queue-merge)
    (setq dop-queue (sort elmo-dop-queue '(lambda (a b)
					    (string< (car a) (car b)))))
    (wl-append dop-queue (list nil)) ;; terminate(dummy)
    (setq last (caar dop-queue)) ;; first
    (while dop-queue
      (setq ope (cons (nth 1 (car dop-queue))
		      (length (nth 2 (car dop-queue)))))
      (if (string= last (caar dop-queue))
	  (wl-append operation (list ope))
	;;(setq count (1+ count))
	(when (and last (setq server-info (elmo-folder-portinfo last)))
	  (setq alist
		(wl-append-assoc-list
		 (cons (car server-info) (nth 1 server-info)) ;; server port
		 (cons last operation)
		 alist)))
	(setq last (caar dop-queue)
	      operation (list ope)))
      (setq dop-queue (cdr dop-queue)))
    alist))

(defun wl-plugged-dop-queue-status (qinfo &optional column)
  ;; dop queue status
  (let ((operations (cdr qinfo))
	(column (or column wl-plugged-queue-status-column)))
    (mapconcat
     '(lambda (folder-ope)
	(concat (wl-plugged-set-folder-icon
		 (car folder-ope)
		 (wl-folder-get-petname (car folder-ope)))
		"("
		(mapconcat
		 '(lambda (ope)
		    (if (> (cdr ope) 0)
			(format "%s:%d" (car ope) (cdr ope))
		      (format "%s" (car ope))))
		 (cdr folder-ope) ",")
		")"))
     operations
     (concat "\n" (wl-set-string-width column "")))))

(defun wl-plugged-drawing (plugged-alist)
  (let ((buffer-read-only nil)
	(alist plugged-alist)
	(vars wl-plugged-switch-variables)
	last server port label plugged time
	line len qinfo column)
    (erase-buffer)
    (while vars
      (insert (format "%s:[%s]%s"
		      (caar vars)
		      (wl-plugged-string (symbol-value (cdar vars)))
		      (if (cdr vars) " " "")))
      (setq vars (cdr vars)))
    (insert "\n")
    (let ((elmo-plugged wl-plugged-switch))
      (setq line (format "[%s](wl-plugged)"
			 (wl-plugged-string (elmo-plugged-p))))
      ;; sending queue status
      (when (setq qinfo (assoc (cons nil nil) wl-plugged-sending-queue-alist))
	(setq line (concat
		    (wl-set-string-width wl-plugged-queue-status-column line)
		    (wl-plugged-sending-queue-status qinfo))))
      (insert line "\n"))
    (while alist
      (setq server (caaar alist)
	    port (cdaar alist)
	    label (nth 1 (car alist))
	    plugged (nth 2 (car alist))
	    time (nth 3 (car alist)))
      (unless (string= last server)
	;; server plug
	(insert (format "%s[%s]%s\n"
			(wl-plugged-server-indent)
			(wl-plugged-string
			 (elmo-plugged-p server nil plugged-alist))
			server))
	(setq last server))
      ;; port plug
      (setq line
	    (format "%s[%s]%s"
		    (make-string wl-plugged-port-indent ? )
		    (wl-plugged-string plugged time)
		    (cond
		     ((stringp port)
		      port)
		     (t
		      (format "%s(%d)"
			      (or label
				  (cdr (assq port wl-plugged-port-label-alist))
				  "")
			      port)))))
      (setq column (max (if line (1+ (string-width line)) 0)
			wl-plugged-queue-status-column))
      (cond
       ;; sending queue status
       ((setq qinfo (assoc (cons server port) wl-plugged-sending-queue-alist))
	(setq line
	      (concat
	       (wl-set-string-width column line)
	       (wl-plugged-sending-queue-status qinfo))))
       ;; dop queue status
       ((setq qinfo (assoc (cons server port) wl-plugged-dop-queue-alist))
	(setq line
	      (concat
	       (wl-set-string-width column line)
	       (wl-plugged-dop-queue-status qinfo column)))))
      (insert line "\n")
      (setq alist (cdr alist)))
    (delete-region (1- (point-max)) (point-max)) ;; delete line at the end.
    (goto-char (point-min))
    (while (not (eobp))
      (wl-highlight-plugged-current-line)
      (forward-line 1)))
  (set-buffer-modified-p nil)
  (count-lines (point-min) (point-max)))

(defun wl-plugged-redrawing-switch (indent switch &optional time)
  (beginning-of-line)
  (when (re-search-forward
	 (format "^%s\\[\\([^]]+\\)\\]" (make-string indent ? )))
    (goto-char (match-beginning 1))
    (delete-region (match-beginning 1) (match-end 1))
    (insert (wl-plugged-string switch time))
    (wl-highlight-plugged-current-line)
    (forward-line 1)))

(defun wl-plugged-redrawing (plugged-alist)
  (let ((buffer-read-only nil)
	(alist plugged-alist)
	last server port plugged time)
    (goto-char (point-min))
    (wl-plugged-redrawing-switch 0 (elmo-plugged-p))
    (while alist
      (setq server (caaar alist)
	    port (cdaar alist)
	    plugged (nth 2 (car alist))
	    time (nth 3 (car alist)))
      (unless (string= last server)
	;; server plug
	(wl-plugged-redrawing-switch
	 wl-plugged-server-indent
	 (elmo-plugged-p server nil plugged-alist))
	(setq last server))
      ;; port plug
      (wl-plugged-redrawing-switch
       wl-plugged-port-indent plugged time)
      (setq alist (cdr alist))))
  (set-buffer-modified-p nil))

(defun wl-plugged-change ()
  (interactive)
  (if (not elmo-plugged-alist)
      (message "No plugged info")
    (setq wl-plugged-winconf (current-window-configuration))
    (let* ((cur-win (selected-window))
	   (max-lines (if (eq major-mode 'wl-summary-mode)
			  (/ (frame-height) 2)
			(window-height)))
	   window-lines lines)
      (save-excursion
	(set-buffer (get-buffer-create wl-plugged-buf-name))
	(wl-plugged-mode)
	(buffer-disable-undo (current-buffer))
	(delete-windows-on (current-buffer))
	(wl-plugged-set-variables)
	(setq lines (wl-plugged-drawing wl-plugged-alist)))
      (select-window cur-win)
      (setq window-lines (min max-lines (max lines window-min-height)))
      (when (> (- (window-height) window-lines) window-min-height)
	(split-window cur-win (- (window-height) window-lines)))
      (switch-to-buffer wl-plugged-buf-name)
      (condition-case nil
	  (progn
	    (enlarge-window (- window-lines (window-height)))
	    (when (fboundp 'pos-visible-in-window-p)
	      (goto-char (point-min))
	      (while (and (<= (window-height) max-lines)
			  (not (pos-visible-in-window-p (1- (point-max)))))
		(enlarge-window 2))))
	(error))
      (goto-char (point-min))
      (forward-line 1)
      (wl-plugged-move-to-next)))) ;; goto first entry

(defsubst wl-plugged-get-server ()
  (save-excursion
    (end-of-line)
    (wl-plugged-move-to-previous-server)
    (beginning-of-line)
    (when (looking-at (format "^%s\\[[^]]+\\]\\(.*\\)"
			      (wl-plugged-server-indent)))
      (elmo-match-buffer 1))))

(defun wl-plugged-toggle ()
  (interactive)
  (let ((cur-point (point)))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; swtich variable
       ((bobp)
	(let (variable switch name)
	  (goto-char cur-point)
	  (when (and (not (bobp))
		     (not (eq (char-before) ? )))
	    (if (re-search-backward " [^ ]+" nil t)
		(forward-char 1)
	      (re-search-backward "^[^ ]+" nil t)))
	  (when (looking-at "\\([^ :[]+\\):?\\[\\([^]]+\\)\\]")
	    (setq name (elmo-match-buffer 1))
	    (setq switch (not (string= (elmo-match-buffer 2) wl-plugged-plug-on)))
	    (when (setq variable (cdr (assoc name wl-plugged-switch-variables)))
	      (set variable switch))
	    (goto-char (match-beginning 2))
	    (let ((buffer-read-only nil))
	      (delete-region (match-beginning 2) (match-end 2))
	      (insert (wl-plugged-string switch))
	      (set-buffer-modified-p nil)))))
       ;; swtich plug
       ((looking-at "^\\( *\\)\\[\\([^]]+\\)\\]\\([^ \n]*\\)")
	(let* ((indent (length (elmo-match-buffer 1)))
	       (switch (elmo-match-buffer 2))
	       (name (elmo-match-buffer 3))
	       (plugged (not (string= switch wl-plugged-plug-on)))
	       (alist wl-plugged-alist)
	       server port)
	  (cond
	   ((eq indent wl-plugged-port-indent)  ;; toggle port plug
	    (cond
	     ((string-match "\\([^([]*\\)(\\([^)[]+\\))" name)
	      (setq port (string-to-int (elmo-match-string 2 name))))
	     (t
	      (setq port name)))
	    (setq server (wl-plugged-get-server))
	    (elmo-set-plugged plugged server port nil alist))
	   ((eq indent wl-plugged-server-indent)  ;; toggle server plug
	    (elmo-set-plugged plugged name nil nil alist))
	   ((eq indent 0)  ;; toggle all plug
	    (elmo-set-plugged plugged nil nil nil alist)))
	  ;; redraw
	  (wl-plugged-redrawing wl-plugged-alist)
	  ;; show plugged status in modeline
	  (let ((elmo-plugged wl-plugged-switch))
	    (setq wl-plugged-switch (elmo-plugged-p)
		  wl-modeline-plug-status wl-plugged-switch)
	    (force-mode-line-update t))))))
    (setq wl-plugged-alist-modified t)
    (goto-char cur-point)))

(defun wl-plugged-click (e)
  (interactive "e")
  (mouse-set-point e)
  (wl-plugged-toggle))

(defun wl-plugged-toggle-all ()
  (interactive)
  (let ((cur-point (point)))
    (setq wl-plugged-switch (not wl-plugged-switch))
    (elmo-set-plugged wl-plugged-switch nil nil nil wl-plugged-alist)
    (wl-plugged-redrawing wl-plugged-alist)
    (goto-char cur-point)
    (setq wl-plugged-alist-modified t)
    ;; show plugged status in modeline
    (setq wl-modeline-plug-status wl-plugged-switch)
    (force-mode-line-update t)))

(defun wl-plugged-exit ()
  (interactive)
  (setq ;;elmo-plugged-alist wl-plugged-alist
	wl-plugged wl-plugged-switch
	wl-plugged-alist nil
	wl-plugged-sending-queue-alist nil
	wl-plugged-dop-queue-alist nil)
  (run-hooks 'wl-plugged-exit-hook)
  (when wl-plugged-alist-modified
    (wl-toggle-plugged (if wl-plugged 'on 'off) t))
  (kill-buffer (current-buffer))
  (if wl-plugged-winconf
      (set-window-configuration wl-plugged-winconf)))

(defun wl-plugged-flush-queue ()
  (interactive)
  (let ((cur-point (point))
	(dop-status (elmo-dop-queue-flush))
	(send-status (wl-draft-queue-flush)))
    (unless (or dop-status send-status)
      (message "No processing queue."))
    (wl-plugged-set-variables)
    (wl-plugged-drawing wl-plugged-alist)
    (goto-char cur-point)))

(defun wl-plugged-move-to-next ()
  (interactive)
  (when (re-search-forward "\\[\\([^]]+\\)\\]" nil t)
    (let ((pos (match-beginning 1)))
      (if (invisible-p pos)
	  (goto-char (next-visible-point pos))
	(goto-char pos)))))

(defun wl-plugged-move-to-previous ()
  (interactive)
  (if (eq (char-before) ?\]) (forward-char -1))
  (when (re-search-backward "\\[\\([^]]+\\)\\]" nil t)
    (let ((pos (match-beginning 1)))
      (if (invisible-p pos)
	  (goto-char (next-visible-point pos))
	(goto-char pos)))))

(defun wl-plugged-move-to-next-server ()
  (interactive)
  (let ((regexp
	 (format "^%s\\[\\([^]]+\\)\\]" (wl-plugged-server-indent)))
	point)
    (save-excursion
      (end-of-line)
      (if (re-search-forward regexp nil t)
	  (setq point (match-beginning 1))))
    (if point (goto-char point))))

(defun wl-plugged-move-to-previous-server ()
  (interactive)
  (let ((regexp
	 (format "^%s\\[\\([^]]+\\)\\]" (wl-plugged-server-indent))))
    (if (re-search-backward regexp nil t)
	(goto-char (match-beginning 1)))))

;;; end of wl-plugged-mode

(defun wl-save ()
  "Save summary and folder status."
  (interactive)
  (wl-save-status 'keep-summary))

(defun wl-save-status (&optional keep-summary)
  (message "Saving summary and folder status...")
  (let (summary-buf)
    (save-excursion
      (let ((summaries (wl-collect-summary)))
	(while summaries
	  (set-buffer (car summaries))
	  (unless keep-summary
	    (wl-summary-cleanup-temp-marks))
	  (wl-summary-save-status keep-summary)
	  (unless keep-summary
	    (kill-buffer (car summaries)))
	  (setq summaries (cdr summaries))))))
  (wl-refile-alist-save)
  (wl-folder-info-save)
  (and (featurep 'wl-fldmgr) (wl-fldmgr-exit))
  (wl-crosspost-alist-save)
  (message "Saving summary and folder status...done"))

(defun wl-exit ()
  (interactive)
  (when (or (not wl-interactive-exit)
	    (y-or-n-p "Quit Wanderlust? "))
    (elmo-quit)
    (wl-biff-stop)
    (run-hooks 'wl-exit-hook)
    (wl-save-status)
    (wl-folder-cleanup-variables)
    (elmo-cleanup-variables)
    (wl-kill-buffers
     (format "^\\(%s\\)$"
	     (mapconcat 'identity
			(list (format "%s\\(:.*\\)?"
				      (default-value 'wl-message-buf-name))
			      wl-original-buf-name
			      wl-folder-buffer-name
			      wl-plugged-buf-name)
			"\\|")))
    (elmo-buffer-cache-clean-up)
    (if (fboundp 'mmelmo-cleanup-entity-buffers)
	(mmelmo-cleanup-entity-buffers))
    (setq wl-init nil)
    (unless wl-on-nemacs
      (remove-hook 'kill-emacs-hook 'wl-save-status))
    t)
  (message "") ;; empty minibuffer.
  )

(defun wl-init (&optional arg)
  (when (not wl-init)
    (setq elmo-plugged wl-plugged)
    (let (succeed demo-buf)
      (if wl-demo
	  (setq demo-buf (wl-demo)))
      (unless wl-on-nemacs
	(add-hook 'kill-emacs-hook 'wl-save-status))
      (unwind-protect
	  (progn
	    (wl-address-init)
	    (wl-draft-setup)
	    (wl-refile-alist-setup)
	    (wl-crosspost-alist-load)
	    (if wl-use-semi
		(progn
		  (require 'wl-mime)
		  (setq elmo-use-semi t))
	      (require 'tm-wl)
	      (setq elmo-use-semi nil))
	    ;; defined above.
	    (wl-mime-setup)
	    (fset 'wl-summary-from-func-internal
		  (symbol-value 'wl-summary-from-func))
	    (fset 'wl-summary-subject-func-internal
		  (symbol-value 'wl-summary-subject-func))
	    (fset 'wl-summary-subject-filter-func-internal
		  (symbol-value 'wl-summary-subject-filter-func))
	    (setq elmo-no-from wl-summary-no-from-message)
	    (setq elmo-no-subject wl-summary-no-subject-message)
	    (setq succeed t)
	    (progn
	      (message "Checking environment...")
	      (wl-check-environment arg)
	      (message "Checking environment...done")))
	(if demo-buf
	    (kill-buffer demo-buf))
	(if succeed
	    (setq wl-init t))
	;; This hook may contain the functions `wl-plugged-init-icons' and
	;; `wl-biff-init-icons' for reasons of system internal to accord
	;; facilities for the Emacs variants.
	(run-hooks 'wl-init-hook)))))

(defun wl-check-environment (no-check-folder)
  (unless (featurep 'mime-setup)
    (require 'mime-setup))
  (unless wl-from
    (error "Please set `wl-from'"))
  ;; Message-ID
  (unless (string-match "[^.]\\.[^.]" (or wl-message-id-domain
					  (if wl-local-domain
					      (concat (system-name)
						      "." wl-local-domain)
					    (system-name))))
    (error "Please set `wl-local-domain' to get valid FQDN"))
  (if (string-match "@" (or wl-message-id-domain
			    (if wl-local-domain
				(concat (system-name)
					"." wl-local-domain)
			      (system-name))))
      (error "Please remove `@' from `wl-message-id-domain'"))
  ;; folders
  (when (not no-check-folder)
    (if (not (eq (elmo-folder-get-type wl-draft-folder) 'localdir))
	(error "%s is not allowed for draft folder" wl-draft-folder))
    (unless (elmo-folder-exists-p wl-draft-folder)
      (if (y-or-n-p
	   (format "Draft Folder %s does not exist, create it? "
		   wl-draft-folder))
	  (elmo-create-folder wl-draft-folder)
	(error "Draft Folder is not created")))
    (if (and wl-draft-enable-queuing
	     (not (elmo-folder-exists-p wl-queue-folder)))
	(if (y-or-n-p
	     (format "Queue Folder %s does not exist, create it? "
		     wl-queue-folder))
	    (elmo-create-folder wl-queue-folder)
	  (error "Queue Folder is not created"))))
  (when (not (eq no-check-folder 'wl-draft))
    (unless (elmo-folder-exists-p wl-trash-folder)
      (if (y-or-n-p
	   (format "Trash Folder %s does not exist, create it? "
		   wl-trash-folder))
	  (elmo-create-folder wl-trash-folder)
	(error "Trash Folder is not created")))
    (unless (elmo-folder-exists-p elmo-lost+found-folder)
      (elmo-create-folder elmo-lost+found-folder)))
  ;; tmp dir
  (unless (file-exists-p wl-tmp-dir)
    (if (y-or-n-p
	 (format "Temp directory (to save multipart) %s does not exist, create it now? "
		 wl-tmp-dir))
	(make-directory wl-tmp-dir)
      (error "Temp directory is not created"))))

;;;###autoload
(defun wl (&optional arg)
  "Start Wanderlust -- Yet Another Message Interface On Emacsen.
If ARG (prefix argument) is specified, folder checkings are skipped."
  (interactive "P")
  (or wl-init (wl-load-profile))
  (unwind-protect
      (wl-init arg)
    (wl-plugged-init (wl-folder arg))
    (sit-for 0))
  (unwind-protect
      (unless arg
	(run-hooks 'wl-auto-check-folder-pre-hook)
	(wl-folder-auto-check)
	(run-hooks 'wl-auto-check-folder-hook))
    (unless arg (wl-biff-start))
    (run-hooks 'wl-hook)))

;; Define some autoload functions WL might use.
(eval-and-compile
  ;; This little mapcar goes through the list below and marks the
  ;; symbols in question as autoloaded functions.
  (mapcar
   (function
    (lambda (package)
      (let ((interactive (nth 1 (memq ':interactive package))))
	(mapcar
	 (function
	  (lambda (function)
	    (let (keymap)
	      (when (consp function)
		(setq keymap (car (memq 'keymap function)))
		(setq function (car function)))
	      (autoload function (car package) nil interactive keymap))))
	 (if (eq (nth 1 package) ':interactive)
	     (cdddr package)
	   (cdr package))))))
   '(("wl-fldmgr" :interactive t
      wl-fldmgr-access-display-all wl-fldmgr-access-display-normal
      wl-fldmgr-add wl-fldmgr-clear-cut-entity-list wl-fldmgr-copy
      wl-fldmgr-copy-region wl-fldmgr-cut wl-fldmgr-cut-region
      wl-fldmgr-make-access-group wl-fldmgr-make-filter
      wl-fldmgr-make-group wl-fldmgr-make-multi
      wl-fldmgr-reconst-entity-hashtb wl-fldmgr-rename wl-fldmgr-delete
      wl-fldmgr-save-folders wl-fldmgr-set-petname wl-fldmgr-sort
      wl-fldmgr-subscribe wl-fldmgr-subscribe-region
      wl-fldmgr-unsubscribe wl-fldmgr-unsubscribe-region wl-fldmgr-yank )
     ("wl-fldmgr"
      (wl-fldmgr-mode-map keymap)
      wl-fldmgr-add-entity-hashtb)
     ("wl-expire" :interactive t
      wl-folder-archive-current-entity
      wl-folder-expire-current-entity wl-summary-archive
      wl-summary-expire )
     ("wl-score"
      wl-score-save wl-summary-rescore-msgs wl-summary-score-headers
      wl-summary-score-update-all-lines )
     ("wl-score" :interactive t
      wl-score-change-score-file wl-score-edit-current-scores
      wl-score-edit-file wl-score-flush-cache wl-summary-rescore
      wl-score-set-mark-below wl-score-set-expunge-below
      wl-summary-increase-score wl-summary-lower-score ))))

;; for backward compatibility
(defalias 'wl-summary-from-func-petname 'wl-summary-default-from)

(require 'product)
(product-provide (provide 'wl) (require 'wl-version))

;;; wl.el ends here
