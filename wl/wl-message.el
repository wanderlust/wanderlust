;;; wl-message.el -- Message displaying modules for Wanderlust.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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

(require 'wl-vars)
(require 'wl-highlight)

(eval-when-compile
  (if wl-use-semi
      (progn
	(require 'wl-mime)
	(require 'mime-view)
	(require 'mmelmo-imap4))
    (require 'tm-wl))
  (defalias-maybe 'event-window 'ignore)
  (defalias-maybe 'posn-window 'ignore)
  (defalias-maybe 'event-start 'ignore)
  (defalias-maybe 'mime-open-entity 'ignore))

(defvar wl-original-buf-name "*Message*")
(defvar wl-message-buf-name "Message")
(defvar wl-message-buffer-cur-summary-buffer nil)
(defvar wl-message-buffer-cur-folder nil)
(defvar wl-message-buffer-cur-number nil)

(defvar wl-original-buffer-cur-folder nil)
(defvar wl-original-buffer-cur-number nil)
(defvar wl-original-buffer-cur-msgdb  nil)

(defvar mmelmo-imap4-skipped-parts)

(make-variable-buffer-local 'wl-message-buffer-cur-folder)
(make-variable-buffer-local 'wl-message-buffer-cur-number)

(defvar wl-fixed-window-configuration nil)

(defun wl-message-buffer-window ()
  (let* ((mes-buf (concat "^" (default-value 'wl-message-buf-name)))
	 (start-win (selected-window))
	 (cur-win start-win))
    (catch 'found
      (while (progn
	       (setq cur-win (next-window cur-win))
	       (if (string-match mes-buf (buffer-name (window-buffer cur-win)))
		   (throw 'found cur-win))
	       (not (eq cur-win start-win)))))))

(defun wl-select-buffer (buffer)
  (let ((gbw (or (get-buffer-window buffer)
		 (wl-message-buffer-window)))
	(sum (car wl-message-window-size))
	(mes (cdr wl-message-window-size))
	whi)
    (when (and gbw
	       (not (eq (save-excursion (set-buffer (window-buffer gbw))
					wl-message-buffer-cur-summary-buffer)
			(current-buffer))))
      (delete-window gbw)
      (run-hooks 'wl-message-window-deleted-hook)
      (setq gbw nil))
    (if gbw
	(select-window gbw)
;;;   (if (or (null mes)
;;;	      wl-stay-folder-window)
;;;	  (delete-other-windows))
      (when wl-fixed-window-configuration
        (delete-other-windows)
        (and wl-stay-folder-window
             (wl-summary-toggle-disp-folder)))
      (setq whi (1- (window-height)))
      (if mes
	  (progn
	    (let ((total (+ sum mes)))
	      (setq sum (max window-min-height (/ (* whi sum) total)))
	      (setq mes (max window-min-height (/ (* whi mes) total))))
            (if (< whi (+ sum mes))
                (enlarge-window (- (+ sum mes) whi)))))
      (split-window (get-buffer-window (current-buffer)) sum)
      (other-window 1))
    (switch-to-buffer buffer)))

;;
;; called by wl-summary-mode buffer
;;
(defvar wl-message-func-called-hook nil)

(defun wl-message-scroll-down (amount)
  (let ((view-message-buffer (get-buffer-create wl-message-buf-name))
	(cur-buf (current-buffer)))
    (wl-select-buffer view-message-buffer)
    (if (bobp)
	()
      (scroll-down))
    (select-window (get-buffer-window cur-buf))))

(defun wl-message-scroll-up (amount)
  (let ((view-message-buffer (get-buffer-create wl-message-buf-name))
	(cur-buf (current-buffer)))
    (wl-select-buffer view-message-buffer)
    (save-excursion
      (save-restriction
	(widen)
	(forward-page 1)
	(if (pos-visible-in-window-p (point))
	    (wl-message-narrow-to-page 1)))) ; Go to next page.
    (if (eobp)
	()
      (scroll-up))
    (select-window (get-buffer-window cur-buf))))
  
(defun wl-message-follow-current-entity (buffer)
  "Follow to current message."
  (wl-draft-reply (wl-message-get-original-buffer)
		  nil wl-message-buffer-cur-summary-buffer) ; reply to all
  (let ((mail-reply-buffer buffer))
    (wl-draft-yank-from-mail-reply-buffer nil)))

(defun wl-message-original-mode ()
  (setq major-mode 'wl-message-original-mode)
  (setq mode-name "Original")
  (setq buffer-read-only t)
  (if (fboundp 'set-buffer-file-coding-system)
      (set-buffer-file-coding-system wl-cs-noconv)))

(defun wl-message-mode ()
  (interactive)
  (setq major-mode 'wl-message-mode)
  (setq buffer-read-only t)
  (setq mode-name "Message"))

(defun wl-message-get-buffer-create ()
  (let ((buf-name wl-message-buf-name))
    (or (get-buffer buf-name)
	(save-excursion
	  (set-buffer (get-buffer-create buf-name))
	  (wl-message-mode)
	  (run-hooks 'wl-message-buffer-created-hook)
	  (get-buffer buf-name)))))

(defun wl-message-original-get-buffer-create ()
  (or (get-buffer wl-original-buf-name)
      (save-excursion
	(set-buffer (get-buffer-create wl-original-buf-name))
	(wl-message-original-mode)
	(get-buffer wl-original-buf-name))))
  
(defun wl-message-exit ()
  (interactive)
  (let (summary-buf summary-win)
    (if (setq summary-buf wl-message-buffer-cur-summary-buffer)
	(if (setq summary-win (get-buffer-window summary-buf))
	    (select-window summary-win)
	  (switch-to-buffer summary-buf)
	  (wl-select-buffer wl-message-buf-name)
	  (select-window (get-buffer-window summary-buf))))
    (run-hooks 'wl-message-exit-hook)))

(defvar wl-message-mode-map nil)
(if wl-message-mode-map
    ()
  (setq wl-message-mode-map (make-sparse-keymap))
  (define-key wl-message-mode-map "q" 'wl-message-exit)
  (define-key wl-message-mode-map "n" 'wl-message-exit)
  (define-key wl-message-mode-map "p" 'wl-message-exit))

(defun wl-message-decode (outbuf inbuf flag)
  (cond
   ((eq flag 'all-header)
    (save-excursion
      (set-buffer inbuf)
      (let ((buffer-read-only nil))
	(decode-mime-charset-region (point-min)
				    (save-excursion
				      (goto-char (point-min))
				      (re-search-forward "^$" nil t)
				      (point))
				    wl-mime-charset)))
    (wl-message-decode-with-all-header outbuf inbuf))
   ((eq flag 'no-mime)
    (save-excursion
      (set-buffer inbuf)
      (let ((buffer-read-only nil))
	(save-excursion
	  (set-buffer outbuf)
	  (elmo-set-buffer-multibyte nil))
	(copy-to-buffer outbuf (point-min) (point-max))
	(set-buffer outbuf)
	(use-local-map wl-message-mode-map)
	(elmo-set-buffer-multibyte default-enable-multibyte-characters)
;;;	(decode-mime-charset-region (point-min) (point-max) wl-mime-charset)
	;; we can call decode-coding-region() directly, because multibyte flag is t.
	(decode-coding-region (point-min) (point-max) wl-cs-autoconv)
	(wl-highlight-message (point-min)
			      (save-excursion
				(goto-char (point-min))
				(re-search-forward "^$" nil t)) nil))))
   (t					; normal
    (save-excursion
      (set-buffer inbuf)
      (let ((buffer-read-only nil))
	(decode-mime-charset-region (point-min)
				    (save-excursion
				      (goto-char (point-min))
				      (re-search-forward "^$" nil t)
				      (point))
				    wl-mime-charset)))
    (wl-message-decode-mode outbuf inbuf))))

(defun wl-message-prev-page (&optional lines)
  "Scroll down this message.  Returns non-nil if top of message."
  (interactive)
  (let ((cur-buf (current-buffer))
	(view-message-buffer (get-buffer-create wl-message-buf-name))
	ret-val)
    (wl-select-buffer view-message-buffer)
    (move-to-window-line 0)
    (if (and wl-break-pages
	     (bobp)
	     (not (save-restriction (widen) (bobp))))
	(progn
	  (wl-message-narrow-to-page -1)
	  (goto-char (point-max))
	  (recenter -1))
      (if (not (bobp))
	  (scroll-down lines)
	(setq ret-val t)))
    (select-window (get-buffer-window cur-buf))
    ret-val))

(static-if (fboundp 'luna-make-entity)
    (defsubst wl-message-make-mime-entity (backend number backend folder msgdb)
      (luna-make-entity (mm-expand-class-name 'elmo)
			:location (get-buffer-create
				   (concat mmelmo-entity-buffer-name "0"))
			:imap (eq backend 'elmo-imap4)
			:folder folder
			:number number
			:msgdb msgdb :size 0))
  (defsubst wl-message-make-mime-entity (backend number backend folder msgdb)
    (mime-open-entity backend (list folder number msgdb nil))))

(defun wl-message-next-page (&optional lines)
  "Scroll up this message.  Returns non-nil if bottom of message."
  (interactive)
  (let ((cur-buf (current-buffer))
	(view-message-buffer (get-buffer-create wl-message-buf-name))
	ret-val)
    (wl-select-buffer view-message-buffer)
    (move-to-window-line -1)
    (if (save-excursion
	  (end-of-line)
	  (and (pos-visible-in-window-p)
	       (eobp)))
	(if (or (null wl-break-pages)
		(save-excursion
		  (save-restriction
		    (widen) (forward-line) (eobp))))
	    (setq ret-val t)
	  (wl-message-narrow-to-page 1)
	  (setq ret-val nil))
      (condition-case ()
	  (static-if (boundp 'window-pixel-scroll-increment)
	      ;; XEmacs 21.2.20 and later.
	      (let (window-pixel-scroll-increment)
		(scroll-up lines))
	    (scroll-up lines))
	(end-of-buffer
	 (goto-char (point-max))))
      (setq ret-val nil))
    (select-window (get-buffer-window cur-buf))
    ret-val
    ))

(defun wl-message-narrow-to-page (&optional arg)
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (condition-case ()
        (forward-page -1)               ; Beginning of current page.
      (beginning-of-buffer
       (goto-char (point-min))))
    (forward-char 1)  ; for compatibility with emacs-19.28 and emacs-19.29
    (widen)
    (cond
     ((> arg 0)	(forward-page arg))
     ((< arg 0) (forward-page (1- arg))))
    (forward-page)
    (if wl-break-pages
	(narrow-to-region (point)
			  (progn
			    (forward-page -1)
			    (if (and (eolp) (not (bobp)))
				(forward-line))
			    (point)))) ))

(defun wl-message-toggle-disp-summary ()
  (interactive)
  (let ((summary-buf (get-buffer wl-message-buffer-cur-summary-buffer))
	summary-win)
    (if (and summary-buf
	     (buffer-live-p summary-buf))
	(if (setq summary-win (get-buffer-window summary-buf))
	    (delete-window summary-win)
	  (switch-to-buffer summary-buf)
	  (wl-select-buffer wl-message-buf-name))
      (wl-summary-goto-folder-subr wl-message-buffer-cur-folder 'no-sync
				   nil nil t)
  					; no summary-buf
      (let ((sum-buf (current-buffer)))
	(wl-select-buffer wl-message-buf-name)
	(setq wl-message-buffer-cur-summary-buffer sum-buf)))))

(defun wl-message-normal-get-original-buffer ()
  (let ((ret-val (get-buffer wl-original-buf-name)))
    (if (not ret-val)
	(save-excursion
	  (set-buffer (setq ret-val
			    (get-buffer-create wl-original-buf-name)))
	  (wl-message-original-mode)))
    ret-val))


(if wl-use-semi
    (defalias 'wl-message-get-original-buffer
      'mmelmo-get-original-buffer)
  (defalias 'wl-message-get-original-buffer
    'wl-message-normal-get-original-buffer))

(defvar wl-message-redisplay-func 'wl-normal-message-redisplay)
(defvar wl-message-cache-used nil) ;whether cache is used or not.

(defun wl-message-redisplay (folder number flag msgdb &optional force-reload)
  (let ((default-mime-charset wl-mime-charset)
	(buffer-read-only nil))
    (setq wl-message-cache-used nil)
    (if wl-message-redisplay-func
	(funcall wl-message-redisplay-func
		 folder number flag msgdb force-reload))))

;; nil means don't fetch all.
(defun wl-message-decide-backend (folder number message-id size)
  (let ((dont-do-that (and
		       (not (setq wl-message-cache-used
				  (or
				   (elmo-buffer-cache-hit
				    (list folder number message-id))
				   (elmo-cache-exists-p message-id
							folder number))))
		       (integerp size)
		       (not (elmo-local-file-p folder number))
		       wl-fetch-confirm-threshold
		       (>= size wl-fetch-confirm-threshold)
		       (not (y-or-n-p
			     (format "Fetch entire message? (%dbytes)"
				     size))))))
    (message "")
    (cond ((and dont-do-that
		(eq (elmo-folder-number-get-type folder number) 'imap4)
		(not (and (elmo-use-cache-p folder number)
			  (elmo-cache-exists-p message-id folder number))))
	   'elmo-imap4)
	  (t (if (not dont-do-that) 'elmo)))))

(defmacro wl-message-original-buffer-folder ()
  wl-original-buffer-cur-folder)

(defmacro wl-message-original-buffer-number ()
  wl-original-buffer-cur-number)

(defun wl-message-set-original-buffer-information (folder number)
  (when (or (not (string= folder (or wl-original-buffer-cur-folder "")))
	    (not (eq number (or wl-original-buffer-cur-number 0))))
    (setq wl-original-buffer-cur-folder folder)
    (setq wl-original-buffer-cur-number number)))

;; Works on FLIM-1.9.0/SEMI-1.8.2 or later (maybe).
(defun wl-mmelmo-message-redisplay (folder number flag msgdb
					   &optional force-reload)
  (let* ((cur-buf (current-buffer))
	 (view-message-buffer (wl-message-get-buffer-create))
	 (message-id (cdr (assq number
				(elmo-msgdb-get-number-alist msgdb))))
	 (size (elmo-msgdb-overview-entity-get-size
		(elmo-msgdb-overview-get-entity number msgdb)))
	 (backend (wl-message-decide-backend folder number message-id size))
	 cur-entity ret-val header-end real-fld-num summary-win)
    (require 'mmelmo)
    (wl-select-buffer view-message-buffer)
    (set-buffer view-message-buffer)
    (unwind-protect
	(progn
	  (setq wl-message-buffer-cur-summary-buffer cur-buf)
	  (setq wl-message-buffer-cur-folder folder)
	  (setq wl-message-buffer-cur-number number)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (if backend
	      (let (mime-display-header-hook ;; bind to nil...
		    (wl-message-ignored-field-list
		     (if (eq flag 'all-header)
			 nil
		       wl-message-ignored-field-list))
		    (mmelmo-force-reload force-reload)
		    (mmelmo-imap4-threshold wl-fetch-confirm-threshold))
		(setq real-fld-num (elmo-get-real-folder-number
				    folder number))
		(setq cur-entity
		      (wl-message-make-mime-entity
		       backend
		       (if (eq backend 'elmo-imap4)
			   (cdr real-fld-num)
			 number)
		       backend
		       (if (eq backend 'elmo-imap4)
			   (car real-fld-num)
			 folder)
		       msgdb))
		(setq mmelmo-imap4-skipped-parts nil)
		;; mime-display-message sets buffer-read-only variable as t.
		;; which makes buffer read-only status confused...
		(mime-display-message cur-entity view-message-buffer
				      nil nil 'mmelmo-original-mode)
		(if mmelmo-imap4-skipped-parts
		    (progn
		      (message "Skipped fetching of %s."
			       (mapconcat
				(lambda (x)
				  (format "[%s]" x))
				mmelmo-imap4-skipped-parts ","))))
		(if (and (eq backend 'elmo-imap4)
			 (null mmelmo-imap4-skipped-parts))
		    (message "No required part was skipped."))
		(setq ret-val (not (eq backend 'elmo-imap4))))
	    (message "Skipped fetching.")
	    (setq ret-val nil)))
      (setq buffer-read-only nil)
      (wl-message-set-original-buffer-information folder number)
      (wl-message-overload-functions)
      ;; highlight body
      (when wl-highlight-body-too
	(wl-highlight-body))
      (condition-case ()
	  (wl-message-narrow-to-page)
	(error nil));; ignore errors.
      (setq mode-line-buffer-identification
	    (format "Wanderlust: << %s / %s >>"
		    (if (memq 'modeline wl-use-folder-petname)
			(wl-folder-get-petname folder)
		      folder) number))
      (goto-char (point-min))
      (unwind-protect
	  (save-excursion
	    (run-hooks 'wl-message-redisplay-hook))
	;; go back to summary mode
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(set-buffer cur-buf)
	(setq summary-win (get-buffer-window cur-buf))
	(if (window-live-p summary-win)
	    (select-window summary-win))))
    ret-val
    ))

(defun wl-normal-message-redisplay (folder number flag msgdb
					   &optional force-reload)
  (interactive)
  (let* ((cur-buf (current-buffer))
	 (original-message-buffer (wl-message-get-original-buffer))
	 (view-message-buffer (wl-message-get-buffer-create))
	 (message-id (cdr (assq number
				(elmo-msgdb-get-number-alist msgdb))))
	 (size (elmo-msgdb-overview-entity-get-size
		(elmo-msgdb-overview-get-entity number msgdb)))
	 header-end ret-val summary-win)
    (wl-select-buffer view-message-buffer)
    (unwind-protect
	(progn
	  (setq wl-message-buffer-cur-summary-buffer cur-buf)
	  (setq wl-message-buffer-cur-folder folder)
	  (setq wl-message-buffer-cur-number number)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (if (or (eq (elmo-folder-number-get-type folder number) 'localdir)
		  (not (and (not 
			     (setq wl-message-cache-used
				  (or
				   (elmo-buffer-cache-hit
				    (list folder number message-id))
				   (elmo-cache-exists-p message-id
							folder number))))
			    (integerp size)
			    wl-fetch-confirm-threshold
			    (>= size wl-fetch-confirm-threshold)
			    (not (y-or-n-p
				  (format "Fetch entire message? (%dbytes)"
					  size))))))
	      (progn
		(save-excursion
		  (set-buffer original-message-buffer)
		  (let ((buffer-read-only nil))
		    (elmo-read-msg-with-buffer-cache
		     folder number original-message-buffer msgdb force-reload)))
		;; decode MIME message.
		(wl-message-decode
		 view-message-buffer
		 original-message-buffer flag)
		(setq ret-val t))
	    (save-excursion
	      (set-buffer view-message-buffer)
	      (insert "\n\n"))))
      (setq buffer-read-only nil)
      (wl-message-set-original-buffer-information folder number)
      (wl-message-overload-functions)
      ;; highlight body
      (and wl-highlight-body-too (wl-highlight-body))
      (condition-case ()
	  (wl-message-narrow-to-page)
	(error nil)) ; ignore errors.
      (setq mode-line-buffer-identification
	    (format "Wanderlust: << %s / %s >>"
		    (if (memq 'modeline wl-use-folder-petname)
			(wl-folder-get-petname folder)
		      folder)
		    number))
      (goto-char (point-min))
      (unwind-protect
	  (run-hooks 'wl-message-redisplay-hook)
	;; go back to summary mode
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(set-buffer cur-buf)
	(setq summary-win (get-buffer-window cur-buf))
	(if (window-live-p summary-win)
	    (select-window summary-win)))
      ret-val
      )))

(defvar wl-message-button-map (make-sparse-keymap))

(defun wl-message-add-button (from to function &optional data)
  "Create a button between FROM and TO with callback FUNCTION and DATA."
  (add-text-properties
   from to
   (nconc (list 'wl-message-button-callback function)
	  (if data
	      (list 'wl-message-button-data data))))
  (let ((ov (make-overlay from to)))
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'local-map wl-message-button-map)
    (overlay-put ov 'evaporate t)))

(defun wl-message-button-dispatcher (event)
  "Select the button under point."
  (interactive "@e")
  (mouse-set-point event)
  (let ((callback (get-text-property (point) 'wl-message-button-callback))
	(data (get-text-property (point) 'wl-message-button-data)))
    (if callback
	(funcall callback data)
      (wl-message-button-dispatcher-internal event))))

(defun wl-message-button-refer-article (data)
  "Read article specified by Message-ID DATA at point."
  (switch-to-buffer-other-window
   wl-message-buffer-cur-summary-buffer)
  (if (wl-summary-jump-to-msg-by-message-id data)
      (wl-summary-redisplay)))

(defun wl-message-refer-article-or-url (e)
  "Read article specified by message-id around point.
If failed, attempt to execute button-dispatcher."
  (interactive "e")
  (let ((window (get-buffer-window (current-buffer)))
	mouse-window point beg end msg-id)
    (unwind-protect
	(progn
	  (mouse-set-point e)
	  (setq mouse-window (get-buffer-window (current-buffer)))
	  (setq point (point))
	  (setq beg (save-excursion (beginning-of-line) (point)))
	  (setq end (save-excursion (end-of-line) (point)))
	  (search-forward ">" end t)      ;Move point to end of "<....>".
	  (if (and (re-search-backward "\\(<[^<> \t\n]+@[^<> \t\n]+>\\)"
				       beg t)
		   (not (string-match "mailto:"
				      (setq msg-id (wl-match-buffer 1)))))
	      (progn
		(goto-char point)
		(switch-to-buffer-other-window
		 wl-message-buffer-cur-summary-buffer)
		(if (wl-summary-jump-to-msg-by-message-id msg-id)
		    (wl-summary-redisplay)))
	    (wl-message-button-dispatcher-internal e)))
      (if (eq mouse-window (get-buffer-window (current-buffer)))
	  (select-window window)))))

(defun wl-message-uu-substring (buf outbuf &optional first last)
  (save-excursion
    (set-buffer buf)
    (search-forward "\n\n")
    (let ((sp (point))
	  ep filename case-fold-search)
      (catch 'done
	(if first
	    (progn
	      (if (re-search-forward "^begin[ \t]+[0-9]+[ \t]+\\([^ ].*\\)" nil t)
		  (setq filename (buffer-substring (match-beginning 1)(match-end 1)))
		(throw 'done nil)))
	  (re-search-forward "^M.*$" nil t)) ; uuencoded string
	(beginning-of-line)
	(setq sp (point))
	(goto-char (point-max))
	(if last
	    (re-search-backward "^end" sp t)
	  (re-search-backward "^M.*$" sp t)) ; uuencoded string
	(forward-line 1)
	(setq ep (point))
	(set-buffer outbuf)
	(goto-char (point-max))
	(insert-buffer-substring buf sp ep)
	(set-buffer buf)
	filename))))

(require 'product)
(product-provide (provide 'wl-message) (require 'wl-version))

;;; wl-message.el ends here
