;;; im-wl.el -- IM/Nifty4U+ interface for Wanderlust.  (not completed.)

;; Copyright (C) 1998,1999 OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;; Copyright (C) 1998,1999 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;;	Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, news, Wanderlust, IM, Nifty4U+

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:

;;  General settings:
;;  (autoload 'wl-draft-send-with-imput-async "im-wl")
;;  (setq wl-draft-send-function 'wl-draft-send-with-imput-async)
;;
;;  for Nifty4U+ users:
;;  (add-hook 'wl-mail-setup-hook '(lambda () (require 'im-wl)))
;;  (setq wl-draft-config-alist
;;	   '(("^Newsgroups: nifty\\..*"
;;	      ;; to avoid header-encoding.
;;	      ;; [cf.  slrn-ja-0.9.4.6.jp4/doc/README.macros.euc]
;;	      ;(eword-field-encoding-method-alist . '((t .  iso-2022-jp-2)))
;;	      (wl-draft-send-function . 'wl-draft-send-with-imput-async)
;;	      (im-wl-dispatcher . '("~/nifty4u-plus/inews-nifty4u" "-h"))
;;	      (im-wl-dispatcher-error-msg
;;	       . (format "^%s :" (expand-file-name (car im-wl-dispatcher)))))))

;;; Code:
;;;(require 'emu)

;;; Variables:
(defvar im-wl-dispatcher
  '("imput" "-h" "-watch" "--debug=no" "-verbose" "--Queuing=yes")
  "Program to post an article and its arguments.
This is most commonly `imput(impost)' or `inews-nifty4u'.")

(defvar im-wl-dispatcher-error-msg (format "^%s: ERROR:" (car im-wl-dispatcher))
  "Error message of dispatcher.")

(defvar im-wl-default-temp-file-name "~/.imput-temp"
  "Default temporary file name (for async).")

;; xxx for Emacs18/19.x
(or (boundp 'shell-command-switch)
    (defvar shell-command-switch "-c"))

;; Buffer local variables (For async).
(defvar im-wl-buffer-editing-buffer nil)
(defvar im-wl-buffer-sending-buffer nil)
(defvar im-wl-buffer-kill-when-done nil)
(make-variable-buffer-local 'im-wl-buffer-editing-buffer)
(make-variable-buffer-local 'im-wl-buffer-sending-buffer)
(make-variable-buffer-local 'im-wl-buffer-kill-when-done)


;;;###autoload
(defun wl-draft-send-with-imput-async (editing-buffer kill-when-done)
  "Send the message in the current buffer with imput asynchronously."
  (let (buffer-process process-connection-type watch-buffer
	(sending-buffer (current-buffer))
	(error-msg-regexp im-wl-dispatcher-error-msg)
	(number wl-draft-buffer-message-number)
	msg)
    (with-current-buffer editing-buffer
      (if (elmo-message-file-p
	   (wl-folder-get-elmo-folder wl-draft-folder)
	   number)
	  (setq msg
		(elmo-message-file-name
		 (wl-folder-get-elmo-folder wl-draft-folder)
		 number))
	(with-temp-file (setq msg (make-temp-file "im-wl"))
	  (elmo-message-fetch (wl-folder-get-elmo-folder wl-draft-folder)
			      number (elmo-make-fetch-strategy 'entire)
			      nil (current-buffer)))))
    ;; current buffer is raw buffer.
    (save-excursion
      (goto-char (point-max))
      ;; require one newline at the end.
      (or (= (preceding-char) ?\n)
	  (insert ?\n))
      ;; Change header-delimiter to be what imput expects.
      (let (delimline
	    (case-fold-search t))
	(save-restriction
	  (std11-narrow-to-header mail-header-separator)
	  ;; Insert Message-ID: 'cause wl-do-fcc() does not take care..
	  (goto-char (point-min))
	  (when (and wl-insert-message-id
		     (not (re-search-forward "^Message-ID[ \t]*:" nil t)))
	    (insert (concat "Message-ID: "
			    (funcall wl-message-id-function) "\n")))
	  ;; Insert date field.
	  (goto-char (point-min))
	  (or (re-search-forward "^Date[ \t]*:" nil t)
	      (wl-draft-insert-date-field)))
	(run-hooks 'wl-mail-send-pre-hook) ;; X-PGP-Sig, Cancel-Lock
	(goto-char (point-min))
	(re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
	(replace-match "\n")
	(forward-char -1)
	(setq delimline (point-marker))
	;; ignore any blank lines in the header
	(goto-char (point-min))
	(while (and (re-search-forward "\n\n\n*" delimline t)
		    (< (point) delimline))
	  (replace-match "\n"))
	;; Find and handle any FCC fields.
	;; 'cause imput can NOT handle `Fcc: %IMAP'.
	(goto-char (point-min))
	(if (re-search-forward "^FCC:" delimline t)
	    (wl-draft-do-fcc delimline))))
    (set-buffer-modified-p t)
    (as-binary-output-file
     (write-region (point-min)(point-max) msg nil t))
    ;; The local variables must be binded to 'watch-buffer.
    (set-buffer (setq watch-buffer (generate-new-buffer " *Wl Watch*")))
    (setq im-wl-buffer-sending-buffer sending-buffer)
    (setq im-wl-buffer-editing-buffer editing-buffer)
    (setq im-wl-buffer-kill-when-done kill-when-done)
    (setq im-wl-dispatcher-error-msg error-msg-regexp)
    ;; Variables specified in wl-draft-config-alist are buffer-local, so
    ;; we have to run subprocess under the editing-buffer.
    ;; The filter function can find 'watch-buffer by process-buffer().
    (set-buffer sending-buffer)
    (setq buffer-process
	  ;; start-process-shell-command() is Emacs19/20's function.
	  (start-process
	   "DISPATCHER" watch-buffer
	   shell-file-name shell-command-switch
	   (format "%s < %s"
		   (mapconcat 'identity im-wl-dispatcher " ") msg)))
    (set-process-sentinel buffer-process 'im-wl-watch-process-async)
    (message "Sending a message in background")
    (if kill-when-done
	(wl-draft-hide editing-buffer))))

(defun im-wl-watch-process-async (process event)
  (let ((process-buffer (process-buffer process))
	editing-buffer kill-when-done raw-buffer)
    (set-buffer process-buffer)
    (setq editing-buffer im-wl-buffer-editing-buffer)
    (setq kill-when-done im-wl-buffer-kill-when-done)
    (setq raw-buffer im-wl-buffer-sending-buffer)
    (goto-char (point-min))
    (if (null (re-search-forward im-wl-dispatcher-error-msg nil t))
	(progn
	  ;; sent successfully.
	  (kill-buffer raw-buffer)
	  (kill-buffer process-buffer)
	  (if kill-when-done
	      (wl-draft-delete editing-buffer)))
      (ding)
      (message "Send failed")
      (kill-buffer raw-buffer)
      (switch-to-buffer editing-buffer)
      (condition-case ()
	  (progn
	    (split-window-vertically)
	    (select-window (next-window)))
	(error)) ; ignore error.
      (switch-to-buffer process-buffer)
      (beginning-of-line))))

(provide 'im-wl)

;;; im-wl.el ends here
