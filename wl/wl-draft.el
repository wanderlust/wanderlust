;;; wl-draft.el --- Message draft mode for Wanderlust.

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

(require 'sendmail)
(require 'wl-template)
(require 'emu)
(condition-case nil (require 'timezone) (error nil))
(require 'std11)
(require 'wl-vars)

(defvar x-face-add-x-face-version-header)
(defvar mail-reply-buffer)
(defvar mail-from-style)

(eval-when-compile
  (require 'elmo-pop3)
  (defalias-maybe 'x-face-insert 'ignore)
  (defalias-maybe 'x-face-insert-version-header 'ignore)
  (defalias-maybe 'wl-init 'ignore)
  (defalias-maybe 'wl-draft-mode 'ignore))

(eval-and-compile
  (autoload 'wl-addrmgr "wl-addrmgr"))

(defvar wl-draft-buf-name "Draft")
(defvar wl-draft-buffer-file-name nil)
(defvar wl-draft-field-completion-list nil)
(defvar wl-draft-verbose-send t)
(defvar wl-draft-verbose-msg nil)
(defvar wl-draft-queue-flushing nil)
(defvar wl-draft-config-variables nil)
(defvar wl-draft-config-exec-flag t)
(defvar wl-draft-buffer-cur-summary-buffer nil)
(defvar wl-draft-clone-local-variable-regexp "^\\(wl\\|mime\\)")
(defvar wl-draft-sendlog-filename "sendlog")
(defvar wl-draft-queue-save-filename "qinfo")
(defvar wl-draft-config-save-filename "config")
(defvar wl-draft-queue-flush-send-function 'wl-draft-dispatch-message)
(defvar wl-sent-message-via nil)
(defvar wl-sent-message-modified nil)
(defvar wl-sent-message-queued nil)
(defvar wl-draft-fcc-list nil)
(defvar wl-draft-reedit nil)
(defvar wl-draft-reply-buffer nil)
(defvar wl-draft-forward nil)
(defvar wl-draft-parent-folder nil)

(defvar wl-draft-config-sub-func-alist
  '((body		. wl-draft-config-sub-body)
    (top		. wl-draft-config-sub-top)
    (bottom		. wl-draft-config-sub-bottom)
    (header		. wl-draft-config-sub-header)
    (header-top		. wl-draft-config-sub-header-top)
    (header-bottom	. wl-draft-config-sub-header)
    (part-top		. wl-draft-config-sub-part-top)
    (part-bottom	. wl-draft-config-sub-part-bottom)
    (body-file		. wl-draft-config-sub-body-file)
    (top-file		. wl-draft-config-sub-top-file)
    (bottom-file	. wl-draft-config-sub-bottom-file)
    (header-file	. wl-draft-config-sub-header-file)
    (template		. wl-draft-config-sub-template)
    (x-face		. wl-draft-config-sub-x-face)))

(make-variable-buffer-local 'wl-draft-buffer-file-name)
(make-variable-buffer-local 'wl-draft-buffer-cur-summary-buffer)
(make-variable-buffer-local 'wl-draft-config-variables)
(make-variable-buffer-local 'wl-draft-config-exec-flag)
(make-variable-buffer-local 'wl-sent-message-via)
(make-variable-buffer-local 'wl-sent-message-queued)
(make-variable-buffer-local 'wl-draft-fcc-list)
(make-variable-buffer-local 'wl-draft-reply-buffer)
(make-variable-buffer-local 'wl-draft-parent-folder)

(defsubst wl-smtp-password-key (user mechanism server)
  (format "SMTP:%s/%s@%s"
	  user mechanism server))

(defmacro wl-smtp-extension-bind (&rest body)
  (` (let* ((smtp-sasl-mechanisms
	     (if wl-smtp-authenticate-type
		 (mapcar 'upcase
			 (if (listp wl-smtp-authenticate-type)
			     wl-smtp-authenticate-type
			   (list wl-smtp-authenticate-type)))))
	    (smtp-use-sasl (and smtp-sasl-mechanisms t))
	    (smtp-use-starttls (eq wl-smtp-connection-type 'starttls))
	    smtp-sasl-user-name smtp-sasl-properties sasl-read-passphrase)
       (if (and (string= (car smtp-sasl-mechanisms) "DIGEST-MD5")
		;; sendmail bug?
		(string-match "^\\([^@]*\\)@\\([^@]*\\)"
			      wl-smtp-posting-user))
	   (setq smtp-sasl-user-name (match-string 1 wl-smtp-posting-user)
		 smtp-sasl-properties (list 'realm
					    (match-string 2 wl-smtp-posting-user)))
	 (setq smtp-sasl-user-name wl-smtp-posting-user
	       smtp-sasl-properties nil))
       (setq sasl-read-passphrase
	     (function
	      (lambda (prompt)
		(elmo-get-passwd
		 (wl-smtp-password-key
		  smtp-sasl-user-name
		  (car smtp-sasl-mechanisms)
		  smtp-server)))))
       (,@ body))))

(defun wl-draft-insert-date-field ()
  "Insert Date field."
  (insert "Date: " (wl-make-date-string) "\n"))

(defun wl-draft-insert-from-field ()
  "Insert From field."
  ;; Put the "From:" field in unless for some odd reason
  ;; they put one in themselves.
  (let* ((login (or user-mail-address (user-login-name)))
	 (fullname (user-full-name)))
    (cond ((eq mail-from-style 'angles)
	   (insert "From: " fullname)
	   (let ((fullname-start (+ (point-min) (length "From: ")))
		 (fullname-end (point-marker)))
	     (goto-char fullname-start)
	     ;; Look for a character that cannot appear unquoted
	     ;; according to RFC 822.
	     (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
				    fullname-end 1)
		 (progn
		   ;; Quote fullname, escaping specials.
		   (goto-char fullname-start)
		   (insert "\"")
		   (while (re-search-forward "[\"\\]"
					     fullname-end 1)
		     (replace-match "\\\\\\&" t))
		   (insert "\""))))
	   (insert " <" login ">\n"))
	  ((eq mail-from-style 'parens)
	   (insert "From: " login " (")
	   (let ((fullname-start (point)))
	     (insert fullname)
	     (let ((fullname-end (point-marker)))
	       (goto-char fullname-start)
	       ;; RFC 822 says \ and nonmatching parentheses
	       ;; must be escaped in comments.
	       ;; Escape every instance of ()\ ...
	       (while (re-search-forward "[()\\]" fullname-end 1)
		 (replace-match "\\\\\\&" t))
	       ;; ... then undo escaping of matching parentheses,
	       ;; including matching nested parentheses.
	       (goto-char fullname-start)
	       (while (re-search-forward
		       "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
		       fullname-end 1)
		 (replace-match "\\1(\\3)" t)
		 (goto-char fullname-start))))
	   (insert ")\n"))
	  ((not mail-from-style)
	   (insert "From: " login "\n")))))

(defun wl-draft-insert-x-face-field ()
  "Insert X-Face header."
  (interactive)
  (if (not (file-exists-p wl-x-face-file))
      (error "File %s does not exist" wl-x-face-file)
    (beginning-of-buffer)
    (search-forward mail-header-separator nil t)
    (beginning-of-line)
    (wl-draft-insert-x-face-field-here)
    (run-hooks 'wl-draft-insert-x-face-field-hook))) ; highlight it if you want.

(defun wl-draft-insert-x-face-field-here ()
  "Insert X-Face field at point."
  (let ((x-face-string (elmo-get-file-string wl-x-face-file)))
    (when (string-match "^[ \t]*" x-face-string)
      (setq x-face-string (substring x-face-string (match-end 0))))
    (insert "X-Face: " x-face-string))
  (when (not (= (preceding-char) ?\n))	; for chomped (choped) x-face-string
    (insert ?\n))
  ;; Insert X-Face-Version: field
  (when (and (fboundp 'x-face-insert-version-header)
	     (boundp 'x-face-add-x-face-version-header)
	     x-face-add-x-face-version-header)
    (x-face-insert-version-header)))

(defun wl-draft-setup ()
  (let ((field wl-draft-fields)
	ret-val)
    (while field
      (setq ret-val (append ret-val
			    (list (cons (concat (car field) " ")
					(concat (car field) " ")))))
      (setq field (cdr field)))
    (setq wl-draft-field-completion-list ret-val)))

(defun wl-draft-make-mail-followup-to (recipients)
  (if (elmo-list-member
       (or wl-user-mail-address-list
	   (list (wl-address-header-extract-address wl-from)))
       recipients)
      (let ((rlist (elmo-list-delete
		    (or wl-user-mail-address-list
			(list (wl-address-header-extract-address wl-from)))
		    (copy-sequence recipients))))
	(if (elmo-list-member rlist (mapcar 'downcase
					    wl-subscribed-mailing-list))
	    rlist
	  (append rlist (list (wl-address-header-extract-address
			       wl-from)))))
    recipients))

(defun wl-draft-delete-myself-from-cc (to cc)
  (let ((myself (or wl-user-mail-address-list
		    (list (wl-address-header-extract-address wl-from)))))
    (cond (wl-draft-always-delete-myself ; always-delete option
	   (elmo-list-delete myself cc))
	  ((elmo-list-member (append to cc) ; subscribed mailing-list
			     (mapcar 'downcase wl-subscribed-mailing-list))
	   (elmo-list-delete myself cc))
	  (t cc))))

(defun wl-draft-forward (original-subject summary-buf)
  (let (references)
    (with-current-buffer (wl-message-get-original-buffer)
      (setq references (nconc
			(std11-field-bodies '("References" "In-Reply-To"))
			(list (std11-field-body "Message-Id"))))
      (setq references (delq nil references)
	    references (mapconcat 'identity references " ")
	    references (wl-draft-parse-msg-id-list-string references)
	    references (wl-delete-duplicates references)
	    references (when references
			 (mapconcat 'identity references "\n\t"))))
    (wl-draft "" (concat "Forward: " original-subject)
	      nil nil references nil nil nil nil nil nil summary-buf))
  (goto-char (point-max))
  (wl-draft-insert-message)
  (mail-position-on-field "To"))

(defun wl-draft-strip-subject-re (subject)
  "Remove \"Re:\" from subject lines. Shamelessly copied from Gnus."
  (if (string-match wl-subject-prefix-regexp subject)
      (substring subject (match-end 0))
    subject))

(defun wl-draft-reply-list-symbol (with-arg)
  "Return symbol `wl-draft-reply-*-argument-list' match condition.
Check WITH-ARG and From: field."
  (if (wl-address-user-mail-address-p (or (elmo-field-body "From") ""))
      (if with-arg
	  'wl-draft-reply-myself-with-argument-list
	'wl-draft-reply-myself-without-argument-list)
    (if with-arg
	'wl-draft-reply-with-argument-list
      'wl-draft-reply-without-argument-list)))

(defun wl-draft-reply (buf with-arg summary-buf)
  "Reply to BUF buffer message.
Reply to author if WITH-ARG is non-nil."
;;;(save-excursion
  (let (r-list
	to mail-followup-to cc subject in-reply-to references newsgroups
	from to-alist cc-alist decoder parent-folder)
    (set-buffer summary-buf)
    (setq parent-folder (wl-summary-buffer-folder-name))
    (set-buffer buf)
    (setq r-list (symbol-value (wl-draft-reply-list-symbol with-arg)))
    (catch 'done
      (while r-list
	(when (let ((condition (car (car r-list))))
		(cond ((stringp condition)
		       (std11-field-body condition))
		      ((listp condition)
		       (catch 'done
			 (while condition
			   (if (not (std11-field-body (car condition)))
			       (throw 'done nil))
			   (setq condition (cdr condition)))
			 t))
		      ((symbolp condition)
		       (funcall condition))))
	  (let ((r-to-list (nth 0 (cdr (car r-list))))
		(r-cc-list (nth 1 (cdr (car r-list))))
		(r-ng-list (nth 2 (cdr (car r-list)))))
	    (when (and (member "Followup-To" r-ng-list)
		       (string= (std11-field-body "Followup-To") "poster"))
	      (setq r-to-list (cons "From" r-to-list))
	      (setq r-ng-list (delete "Followup-To" (copy-sequence r-ng-list))))
	    (setq to (wl-concat-list (cons to
					   (elmo-multiple-fields-body-list
					    r-to-list))
				     ","))
	    (setq cc (wl-concat-list (cons cc
					   (elmo-multiple-fields-body-list
					    r-cc-list))
				     ","))
	    (setq newsgroups (wl-concat-list (cons newsgroups
						   (std11-field-bodies
						    r-ng-list))
					     ",")))
	  (throw 'done nil))
	(setq r-list (cdr r-list)))
      (error "No match field: check your `%s'"
	     (symbol-name (wl-draft-reply-list-symbol with-arg))))
    (setq subject (std11-field-body "Subject"))
    (setq to (wl-parse-addresses to)
	  cc (wl-parse-addresses cc))
    (with-temp-buffer			; to keep raw buffer unibyte.
      (elmo-set-buffer-multibyte default-enable-multibyte-characters)
      (setq decoder (mime-find-field-decoder 'Subject 'plain))
      (setq subject (if (and subject decoder)
			(funcall decoder subject) subject))
      (setq to-alist
	    (mapcar
	     (lambda (addr)
	       (setq decoder (mime-find-field-decoder 'To 'plain))
	       (cons (nth 1 (std11-extract-address-components addr))
		     (if decoder (funcall decoder addr) addr)))
	     to))
      (setq cc-alist
	    (mapcar
	     (lambda (addr)
	       (setq decoder (mime-find-field-decoder 'Cc 'plain))
	       (cons (nth 1 (std11-extract-address-components addr))
		     (if decoder (funcall decoder addr) addr)))
	     cc)))
    (and wl-reply-subject-prefix
	 (setq subject (concat wl-reply-subject-prefix
			       (wl-draft-strip-subject-re
				(or subject "")))))
    (setq in-reply-to (std11-field-body "Message-Id"))
    (setq references (nconc
		      (std11-field-bodies '("References" "In-Reply-To"))
		      (list in-reply-to)))
    (setq to (delq nil (mapcar 'car to-alist)))
    (setq cc (delq nil (mapcar 'car cc-alist)))
    ;; if subscribed mailing list is contained in cc or to
    ;; and myself is contained in cc,
    ;; delete myself from cc.
    (setq cc (wl-draft-delete-myself-from-cc to cc))
    (when wl-insert-mail-followup-to
      (setq mail-followup-to
	    (wl-draft-make-mail-followup-to (append to cc)))
      (setq mail-followup-to (wl-delete-duplicates mail-followup-to nil t)))
    (with-temp-buffer			; to keep raw buffer unibyte.
      (elmo-set-buffer-multibyte default-enable-multibyte-characters)
      (setq newsgroups (wl-parse newsgroups
				 "[ \t\f\r\n,]*\\([^ \t\f\r\n,]+\\)")
	    newsgroups (wl-delete-duplicates newsgroups)
	    newsgroups
	    (if newsgroups
		(mapconcat
		 (lambda (grp)
		   (setq decoder (mime-find-field-decoder 'Newsgroups 'plain))
		   (if decoder (funcall decoder grp) grp))
		 newsgroups ","))))
    (setq to (wl-delete-duplicates to nil t))
    (setq cc (wl-delete-duplicates
	      (append (wl-delete-duplicates cc nil t)
		      to (copy-sequence to))
	      t t))
    (and to (setq to (mapconcat
		      '(lambda (addr)
			 (if wl-draft-reply-use-address-with-full-name
			     (or (cdr (assoc addr to-alist)) addr)
			   addr))
		      to ",\n\t")))
    (and cc (setq cc (mapconcat
		      '(lambda (addr)
			 (if wl-draft-reply-use-address-with-full-name
			     (or (cdr (assoc addr cc-alist)) addr)
			   addr))
		      cc ",\n\t")))
    (and mail-followup-to
	 (setq mail-followup-to
	       (mapconcat
		'(lambda (addr)
		   (if wl-draft-reply-use-address-with-full-name
		       (or (cdr (assoc addr (append to-alist cc-alist))) addr)
		     addr))
		mail-followup-to ",\n\t")))
    (and (null to) (setq to cc cc nil))
    (setq references (delq nil references)
	  references (mapconcat 'identity references " ")
	  references (wl-draft-parse-msg-id-list-string references)
	  references (wl-delete-duplicates references)
	  references (if references
			 (mapconcat 'identity references "\n\t")))
    (wl-draft
     to subject in-reply-to cc references newsgroups mail-followup-to
     nil nil nil nil summary-buf nil parent-folder)
    (setq wl-draft-reply-buffer buf))
  (run-hooks 'wl-reply-hook))

(defun wl-draft-add-references ()
  (wl-draft-add-in-reply-to "References"))

(defun wl-draft-add-in-reply-to (&optional alt-field)
  (let* ((mes-id (save-excursion
		   (set-buffer mail-reply-buffer)
		   (std11-field-body "message-id")))
	 (field (or alt-field "In-Reply-To"))
	 (ref (std11-field-body field))
	 (ref-list nil) (st nil))
    (when (and mes-id ref)
      (while (string-match "<[^>]+>" ref st)
	(setq ref-list
	      (cons (substring ref (match-beginning 0) (setq st (match-end 0)))
		    ref-list)))
      (when (and ref-list
		 (member mes-id ref-list))
	(setq mes-id nil)))
    (when mes-id
      (save-excursion
	(when (mail-position-on-field field)
	  (forward-line)
	  (while (looking-at "^[ \t]")
	    (forward-line))
	  (setq mes-id (concat "\t" mes-id "\n")))
	(insert mes-id))
      t)))

(defun wl-draft-yank-from-mail-reply-buffer (decode-it
					     &optional ignored-fields)
  (interactive)
  (save-restriction
    (narrow-to-region (point)(point))
    (insert
     (with-current-buffer mail-reply-buffer
       (when decode-it
	 (decode-mime-charset-region (point-min) (point-max)
				     wl-mime-charset))
       (buffer-substring-no-properties
	(point-min) (point-max))))
    (when ignored-fields
      (goto-char (point-min))
      (wl-draft-delete-fields ignored-fields))
    (goto-char (point-max))
    (push-mark (point) nil t)
    (goto-char (point-min)))
  (let ((beg (point)))
    (cond (mail-citation-hook (run-hooks 'mail-citation-hook))
	  (mail-yank-hooks (run-hooks 'mail-yank-hooks))
	  (wl-draft-cite-function (funcall wl-draft-cite-function))) ; default cite
    (run-hooks 'wl-draft-cited-hook)
    (when (if wl-draft-add-references
	      (wl-draft-add-references)
	    (if wl-draft-add-in-reply-to
		(wl-draft-add-in-reply-to)))
      (wl-highlight-headers 'for-draft)) ; highlight when added References:
    (when wl-highlight-body-too
      (wl-highlight-body-region beg (point-max)))))

(defun wl-draft-confirm ()
  "Confirm send message."
  (interactive)
  (y-or-n-p (format "Send current draft as %s? "
		    (cond ((and (wl-message-mail-p) (wl-message-news-p))
			   "Mail and News")
			  ((wl-message-mail-p) "Mail")
			  ((wl-message-news-p) "News")))))

(defun wl-message-news-p ()
  "If exist valid Newsgroups field, return non-nil."
  (std11-field-body "Newsgroups"))

(defun wl-message-field-exists-p (field)
  "If FIELD exist and FIELD value is not empty, return non-nil."
  (let ((value (std11-field-body field)))
    (and value
	 (not (string= value "")))))

(defun wl-message-mail-p ()
  "If exist To, Cc or Bcc field, return non-nil."
  (or (wl-message-field-exists-p "To")
      (wl-message-field-exists-p "Resent-to")
      (wl-message-field-exists-p "Cc")
      (wl-message-field-exists-p "Bcc")
;;; This may be needed..
;;;   (wl-message-field-exists-p "Fcc")
      ))

(defun wl-draft-open-file (&optional file)
  "Open FILE for edit."
  (interactive)
;;;(interactive "*fFile to edit: ")
  (wl-draft-edit-string (elmo-get-file-string
			 (or file
			     (read-file-name "File to edit: "
					     (or wl-temporary-file-directory
						 "~/"))))))

(defun wl-draft-edit-string (string)
  (let ((cur-buf (current-buffer))
	(tmp-buf (get-buffer-create " *wl-draft-edit-string*"))
	to subject in-reply-to cc references newsgroups mail-followup-to
	content-type content-transfer-encoding from
	body-beg buffer-read-only)
    (set-buffer tmp-buf)
    (erase-buffer)
    (insert string)
    (setq to (std11-field-body "To"))
    (setq to (and to
		  (eword-decode-string
		   (decode-mime-charset-string
		    to
		    wl-mime-charset))))
    (setq subject (std11-field-body "Subject"))
    (setq subject (and subject
		       (eword-decode-string
			(decode-mime-charset-string
			 subject
			 wl-mime-charset))))
    (setq from (std11-field-body "From")
	  from (and from
		    (eword-decode-string
		     (decode-mime-charset-string
		      from
		      wl-mime-charset))))
    (setq in-reply-to (std11-field-body "In-Reply-To"))
    (setq cc (std11-field-body "Cc"))
    (setq cc (and cc
		  (eword-decode-string
		   (decode-mime-charset-string
		    cc
		    wl-mime-charset))))
    (setq references (std11-field-body "References"))
    (setq newsgroups (std11-field-body "Newsgroups"))
    (setq mail-followup-to (std11-field-body "Mail-Followup-To"))
    (setq content-type (std11-field-body "Content-Type"))
    (setq content-transfer-encoding (std11-field-body "Content-Transfer-Encoding"))
    (goto-char (point-min))
    (or (re-search-forward "\n\n" nil t)
	(search-forward (concat mail-header-separator "\n") nil t))
    (unwind-protect
	(set-buffer
	 (wl-draft to subject in-reply-to cc references newsgroups
		   mail-followup-to
		   content-type content-transfer-encoding
		   (buffer-substring (point) (point-max))
		   'edit-again nil
		   (if (member (nth 1 (std11-extract-address-components from))
			       wl-user-mail-address-list)
		       from)))
      (and to (mail-position-on-field "To"))
      (delete-other-windows)
      (kill-buffer tmp-buf)))
  (setq buffer-read-only nil) ;;??
  (run-hooks 'wl-draft-reedit-hook))

(defun wl-draft-insert-current-message (dummy)
  (interactive)
  (let (original-buffer
	mail-reply-buffer
	mail-citation-hook mail-yank-hooks
	wl-draft-add-references wl-draft-add-in-reply-to
	wl-draft-cite-function)
    (with-current-buffer wl-draft-buffer-cur-summary-buffer
      (with-current-buffer wl-message-buffer
	(setq original-buffer (wl-message-get-original-buffer))
	(if (zerop
	     (with-current-buffer original-buffer
	       (buffer-size)))
	    (error "No current message"))))
    (setq mail-reply-buffer original-buffer)
    (wl-draft-yank-from-mail-reply-buffer
     nil
     wl-ignored-forwarded-headers)))

(defun wl-draft-insert-get-message (dummy)
  (let ((fld (completing-read
	      "Folder name: "
	      (if (memq 'read-folder wl-use-folder-petname)
		  (wl-folder-get-entity-with-petname)
		wl-folder-entity-hashtb)
	      nil nil wl-default-spec
	      'wl-read-folder-hist))
	(number (call-interactively
		 (function (lambda (num)
			     (interactive "nNumber: ")
			     num))))
	(mail-reply-buffer (get-buffer-create "*wl-draft-insert-get-message*"))
	mail-citation-hook mail-yank-hooks
	wl-draft-cite-function)
    (unwind-protect
	(progn
	  (elmo-message-fetch (wl-folder-get-elmo-folder fld)
			      number
			      ;; No cache.
			      (elmo-make-fetch-strategy 'entire)
			      nil mail-reply-buffer)
	  (wl-draft-yank-from-mail-reply-buffer nil))
      (kill-buffer mail-reply-buffer))))

;;
;; default body citation func
;;
(defun wl-default-draft-cite ()
  (let ((mail-yank-ignored-headers "[^:]+:")
	(mail-yank-prefix "> ")
	(summary-buf wl-current-summary-buffer)
	(message-buf (get-buffer (wl-current-message-buffer)))
	from date cite-title num entity)
    (if (and summary-buf
	     (buffer-live-p summary-buf)
	     message-buf
	     (buffer-live-p message-buf))
	(progn
	  (with-current-buffer summary-buf
	    (setq num (save-excursion
			(set-buffer message-buf)
			wl-message-buffer-cur-number))
	    (setq entity (elmo-msgdb-overview-get-entity
			  num (wl-summary-buffer-msgdb)))
	    (setq date (elmo-msgdb-overview-entity-get-date entity))
	    (setq from (elmo-msgdb-overview-entity-get-from entity)))
	  (setq cite-title (format "At %s,\n%s wrote:"
				   (or date "some time ago")
				   (if wl-default-draft-cite-decorate-author
				     (wl-summary-from-func-internal
				      (or from "you"))
				     (or from "you"))))))
    (and cite-title
	 (insert cite-title "\n"))
    (mail-indent-citation)))

(defvar wl-draft-buffer nil "Draft buffer to yank content")
(defun wl-draft-yank-to-draft-buffer (buffer)
  "Yank BUFFER content to `wl-draft-buffer'."
  (set-buffer wl-draft-buffer)
  (let ((mail-reply-buffer buffer))
    (wl-draft-yank-from-mail-reply-buffer nil)
    (kill-buffer buffer)))

(defun wl-draft-yank-original (&optional arg)
  "Yank original message."
  (interactive "P")
  (if arg
      (let (buf mail-reply-buffer)
	(elmo-set-work-buf
	 (yank)
	 (setq buf (current-buffer)))
	(setq mail-reply-buffer buf)
	(wl-draft-yank-from-mail-reply-buffer nil))
    (wl-draft-yank-current-message-entity)))

(defun wl-draft-hide (editing-buffer)
  "Hide the editing draft buffer if possible."
  (when (and editing-buffer
	     (buffer-live-p editing-buffer))
    (set-buffer editing-buffer)
    (let ((sum-buf wl-draft-buffer-cur-summary-buffer)
	  fld-buf sum-win fld-win)
      (if (and wl-draft-use-frame
	       (> (length (visible-frame-list)) 1))
	  ;; hide draft frame
	  (delete-frame)
	;; hide draft window
	(or (one-window-p)
	    (delete-window)))
      ;; stay folder window if required
      (when wl-stay-folder-window
	(if (setq fld-buf (get-buffer wl-folder-buffer-name))
	    (if (setq fld-win (get-buffer-window fld-buf))
		(select-window fld-win)
	      (if wl-draft-resume-folder-window ;; resume folder window
		  (switch-to-buffer fld-buf)))))
      (if (buffer-live-p sum-buf)
	  (if (setq sum-win (get-buffer-window sum-buf t))
	      ;; if Summary is on the frame, select it.
	      (select-window sum-win)
	    ;; if summary is not on the frame, switch to it.
	    (if (and wl-stay-folder-window
		     (or wl-draft-resume-folder-window fld-win))
		(wl-folder-select-buffer sum-buf)
	      (switch-to-buffer sum-buf)))))))

(defun wl-draft-delete (editing-buffer)
  "kill the editing draft buffer and delete the file corresponds to it."
  (save-excursion
    (when editing-buffer
      (set-buffer editing-buffer)
      (if wl-draft-buffer-file-name
	  (progn
	    (if (file-exists-p wl-draft-buffer-file-name)
		(delete-file wl-draft-buffer-file-name))
	    (let ((msg (and wl-draft-buffer-file-name
			    (string-match "[0-9]+$" wl-draft-buffer-file-name)
			    (string-to-int
			     (match-string 0 wl-draft-buffer-file-name)))))
	      (wl-draft-config-info-operation msg 'delete))))
      (set-buffer-modified-p nil)		; force kill
      (kill-buffer editing-buffer))))

(defun wl-draft-kill (&optional force-kill)
  "Kill current draft buffer and quit editing."
  (interactive "P")
  (save-excursion
    (when (and (or (eq major-mode 'wl-draft-mode)
		   (eq major-mode 'mail-mode))
	       (or force-kill
		   (y-or-n-p "Kill Current Draft? ")))
      (let ((cur-buf (current-buffer)))
	(wl-draft-hide cur-buf)
	(wl-draft-delete cur-buf)))
    (message "")))

(defun wl-draft-fcc ()
  "Add a new Fcc field, with file name completion."
  (interactive)
  (or (mail-position-on-field "fcc" t)  ;Put new field after exiting Fcc.
      (mail-position-on-field "to"))
  (insert "\nFcc: "))

;; Imported from message.el.
(defun wl-draft-elide-region (b e)
  "Elide the text in the region.
An ellipsis (from `wl-draft-elide-ellipsis') will be inserted where the
text was killed."
  (interactive "r")
  (kill-region b e)
  (insert wl-draft-elide-ellipsis))

;; function for wl-sent-message-via

(defmacro wl-draft-sent-message-p (type)
  (` (eq (nth 1 (assq (, type) wl-sent-message-via)) 'sent)))

(defmacro wl-draft-set-sent-message (type result &optional server-port)
  (` (let ((element (assq (, type) wl-sent-message-via)))
       (if element
	   (unless (eq (nth 1 element) (, result))
	     (setcdr element (list (, result) (, server-port)))
	     (setq wl-sent-message-modified t))
	 (push (list (, type) (, result) (, server-port)) wl-sent-message-via)
	 (setq wl-sent-message-modified t)))))

(defun wl-draft-sent-message-results ()
  (let ((results wl-sent-message-via)
	unplugged-via sent-via)
    (while results
      (cond ((eq (nth 1 (car results)) 'unplugged)
	     (push (caar results) unplugged-via))
	    ((eq (nth 1 (car results)) 'sent)
	     (push (caar results) sent-via)))
      (setq results (cdr results)))
    (list unplugged-via sent-via)))

(defun wl-draft-write-sendlog (status proto server to id)
  "Write send log file, if `wl-draft-sendlog' is non-nil."
  (when wl-draft-sendlog
    (with-temp-buffer
      (let* ((filename (expand-file-name wl-draft-sendlog-filename
					 elmo-msgdb-directory))
	     (filesize (nth 7 (file-attributes filename)))
	     (server (if server (concat " server=" server) ""))
	     (to (if to (cond
			 ((memq proto '(fcc queue))
			  (format " folder=\"%s\"" to))
			 ((eq proto 'nntp)
			  (format " ng=<%s>" to))
			 (t
			  (concat " to="
				  (mapconcat
				   'identity
				   (mapcar '(lambda(x) (format "<%s>" x)) to)
				   ","))))
		   ""))
	     (id (if id (concat " id=" id) ""))
	     (time (wl-sendlog-time)))
	(insert (format "%s proto=%s stat=%s%s%s%s\n"
			time proto status server to id))
	(if (and wl-draft-sendlog-max-size filesize
		 (> filesize wl-draft-sendlog-max-size))
	    (rename-file filename (concat filename ".old") t))
	(if (file-writable-p filename)
	    (write-region-as-binary (point-min) (point-max)
				    filename t 'no-msg)
	  (message "%s is not writable." filename))))))

(defun wl-draft-get-header-delimiter (&optional delete)
  ;; If DELETE is non-nil, replace the header delimiter with a blank line
  (let (delimline)
    (goto-char (point-min))
    (when (re-search-forward
	   (concat "^" (regexp-quote mail-header-separator) "$\\|^$") nil t)
      (replace-match "")
      (if delete
	  (forward-char -1))
      (setq delimline (point-marker)))
    delimline))

(defun wl-draft-send-mail-with-qmail ()
  "Pass the prepared message buffer to qmail-inject.
Refer to the documentation for the variable `send-mail-function'
to find out how to use this."
  (if (and wl-draft-qmail-send-plugged
	   (not (elmo-plugged-p)))
      (wl-draft-set-sent-message 'mail 'unplugged)
    ;; send the message
    (run-hooks 'wl-mail-send-pre-hook) ;; X-PGP-Sig, Cancel-Lock
    (let ((id (std11-field-body "Message-ID"))
	  (to (std11-field-body "To")))
      (case
	  (as-binary-process
	   (apply
	    'call-process-region 1 (point-max) wl-qmail-inject-program
	    nil nil nil
	    wl-qmail-inject-args))
	;; qmail-inject doesn't say anything on it's stdout/stderr,
	;; we have to look at the retval instead
	(0   (progn
	       (wl-draft-set-sent-message 'mail 'sent)
	       (wl-draft-write-sendlog 'ok 'qmail nil (list to) id)))
	(1   (error "`qmail-inject' reported permanent failure"))
	(111 (error "`qmail-inject' reported transient failure"))
	;; should never happen
	(t   (error "`qmail-inject' reported unknown failure"))))))

(defun wl-draft-parse-msg-id-list-string (string)
  "Get msg-id list from STRING."
  (let (msg-id-list)
    (dolist (parsed-id (std11-parse-msg-ids-string string))
      (when (eq (car parsed-id) 'msg-id)
	(setq msg-id-list (cons (std11-msg-id-string parsed-id)
				msg-id-list))))
    (nreverse msg-id-list)))

(defun wl-draft-std11-parse-addresses (lal)
  (let ((ret (std11-parse-address lal)))
    (if ret
	(let ((dest (list (car ret))))
	  (setq lal (cdr ret))
	  (while (and (setq ret (std11-parse-ascii-token lal))
		      (string-equal (cdr (assq 'specials (car ret))) ",")
		      (setq ret (std11-parse-address (cdr ret)))
		      )
	    (setq dest (cons (car ret) dest))
	    (setq lal (cdr ret)))
	  (while (eq 'spaces (car (car lal)))
	    (setq lal (cdr lal)))
	  (if lal (error "Error while parsing address"))
	  (nreverse dest)))))

(defun wl-draft-parse-mailbox-list (field &optional remove-group-list)
  "Get mailbox list of FIELD from current buffer.
The buffer is expected to be narrowed to just the headers of the message.
If optional argument REMOVE-GROUP-LIST is non-nil, remove group list content
from current buffer."
  (save-excursion
    (let ((case-fold-search t)
	  (inhibit-read-only t)
	  addresses address
	  mailbox-list beg seq has-group-list)
      (goto-char (point-min))
      (while (re-search-forward (concat "^" (regexp-quote field) "[\t ]*:")
				nil t)
	(setq beg (point))
	(re-search-forward "^[^ \t]" nil 'move)
	(beginning-of-line)
	(skip-chars-backward "\n")
	(setq seq (std11-lexical-analyze
		   (buffer-substring-no-properties beg (point))))
	(setq addresses (wl-draft-std11-parse-addresses seq))
	(while addresses
	  (cond ((eq (car (car addresses)) 'group)
		 (setq has-group-list t)
		 (setq mailbox-list
		       (nconc mailbox-list
			      (mapcar
			       'std11-address-string
			       (nth 2 (car addresses))))))
		((eq (car (car addresses)) 'mailbox)
		 (setq address (nth 1 (car addresses)))
		 (setq mailbox-list
		       (nconc mailbox-list
			      (list
			       (std11-addr-to-string
				(if (eq (car address) 'phrase-route-addr)
				    (nth 2 address)
				  (cdr address))))))))
	  (setq addresses (cdr addresses)))
	(when (and remove-group-list has-group-list)
	  (delete-region beg (point))
	  (insert (wl-address-string-without-group-list-contents seq))))
      mailbox-list)))

(defun wl-draft-deduce-address-list (buffer header-start header-end)
  "Get address list suitable for smtp RCPT TO:<address>.
Group list content is removed if `wl-draft-remove-group-list-contents' is
non-nil."
  (let ((fields '("to" "cc" "bcc"))
	(resent-fields '("resent-to" "resent-cc" "resent-bcc"))
	(case-fold-search t)
	addrs recipients)
    (save-excursion
      (save-restriction
	(narrow-to-region header-start header-end)
	(goto-char (point-min))
	(save-excursion
	  (if (re-search-forward "^resent-to[\t ]*:" nil t)
	      (setq fields resent-fields)))
	(while fields
	  (setq recipients
		(nconc recipients
		       (wl-draft-parse-mailbox-list
			(car fields)
			wl-draft-remove-group-list-contents)))
	  (setq fields (cdr fields)))
	recipients))))

;;
;; from Semi-gnus
;;
(defun wl-draft-send-mail-with-smtp ()
  "Send the prepared message buffer with SMTP."
  (require 'smtp)
  (let* ((errbuf (if mail-interactive
		     (generate-new-buffer " smtp errors")
		   0))
	 (case-fold-search t)
	 (default-case-fold-search t)
	 (sender (or wl-envelope-from
		     (wl-address-header-extract-address wl-from)))
	 (delimline (save-excursion
		      (goto-char (point-min))
		      (re-search-forward
		       (concat "^" (regexp-quote mail-header-separator)
			       "$\\|^$") nil t)
		      (point-marker)))
	 (smtp-server
	  (or wl-smtp-posting-server
	      ;; Compatibility stuff for FLIM 1.12.5 or earlier.
	      ;; They don't accept a function as the value of `smtp-server'.
	      (if (functionp smtp-server)
		  (funcall
		   smtp-server
		   sender
		   ;; no harm..
		   (let (wl-draft-remove-group-list-contents)
		     (wl-draft-deduce-address-list
		      (current-buffer) (point-min) delimline)))
		(or smtp-server "localhost"))))
	 (smtp-service (or wl-smtp-posting-port smtp-service))
	 (smtp-local-domain (or smtp-local-domain wl-local-domain))
	 (id (std11-field-body "message-id"))
	 recipients)
    (if (not (elmo-plugged-p smtp-server smtp-service))
	(wl-draft-set-sent-message 'mail 'unplugged
				   (cons smtp-server smtp-service))
      (unwind-protect
	  (save-excursion
	    ;; Instead of `smtp-deduce-address-list'.
	    (setq recipients (wl-draft-deduce-address-list
			      (current-buffer) (point-min) delimline))
	    (unless recipients (error "No recipients"))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (if (eval mail-mailer-swallows-blank-line)
		(newline))
	    (run-hooks 'wl-mail-send-pre-hook) ;; X-PGP-Sig, Cancel-Lock
	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer)))
	    (wl-draft-delete-field "bcc" delimline)
	    (wl-draft-delete-field "resent-bcc" delimline)
	    (let (process-connection-type)
	      (as-binary-process
	       (when recipients
		 (wl-smtp-extension-bind
		  (condition-case err
		      (smtp-send-buffer sender recipients (current-buffer))
		    (error
		     (wl-draft-write-sendlog 'failed 'smtp smtp-server
					     recipients id)
		     (if (/= (nth 1 err) 334)
			 (elmo-remove-passwd
			  (wl-smtp-password-key
			   smtp-sasl-user-name
			   (car smtp-sasl-mechanisms)
			   smtp-server)))
		     (signal (car err) (cdr err)))))
		 (wl-draft-set-sent-message 'mail 'sent)
		 (wl-draft-write-sendlog
		  'ok 'smtp smtp-server recipients id)))))
	(if (bufferp errbuf)
	    (kill-buffer errbuf))))))

(defun wl-draft-send-mail-with-pop-before-smtp ()
  "Send the prepared message buffer with POP-before-SMTP."
  (require 'elmo-pop3)
  (let ((session
	 (luna-make-entity
	  'elmo-pop3-folder
	  :user   (or wl-pop-before-smtp-user
		      elmo-pop3-default-user)
	  :server (or wl-pop-before-smtp-server
		      elmo-pop3-default-server)
	  :port   (or wl-pop-before-smtp-port
		      elmo-pop3-default-port)
	  :auth   (or wl-pop-before-smtp-authenticate-type
		      elmo-pop3-default-authenticate-type)
	  :stream-type (or wl-pop-before-smtp-stream-type
			   elmo-pop3-default-stream-type))))
    (condition-case error
	(progn
	  (elmo-pop3-get-session session)
	  (when session (elmo-network-close-session session)))
      (error
       (elmo-network-close-session session)
       (signal (car error)(cdr error)))))
  (wl-draft-send-mail-with-smtp))

(defun wl-draft-insert-required-fields (&optional force-msgid)
  "Insert Message-ID, Date, and From field.
If FORCE-MSGID, ignore 'wl-insert-message-id'."
  ;; Insert Message-Id field...
  (goto-char (point-min))
  (when (and (or force-msgid
		 wl-insert-message-id)
	     (not (re-search-forward "^Message-ID[ \t]*:" nil t)))
    (insert (concat "Message-ID: "
		    (funcall wl-message-id-function)
		    "\n")))
  ;; Insert date field.
  (goto-char (point-min))
  (or (re-search-forward "^Date[ \t]*:" nil t)
      (wl-draft-insert-date-field))
  ;; Insert from field.
  (goto-char (point-min))
  (or (re-search-forward "^From[ \t]*:" nil t)
      (wl-draft-insert-from-field)))

(defun wl-draft-normal-send-func (editing-buffer kill-when-done)
  "Send the message in the current buffer."
  (save-restriction
    (std11-narrow-to-header mail-header-separator)
    (wl-draft-insert-required-fields)
    ;; Delete null fields.
    (goto-char (point-min))
    (while (re-search-forward "^[^ \t\n:]+:[ \t]*\n" nil t)
      (replace-match ""))
    ;; ignore any blank lines in the header
    (while (re-search-forward "\n\n\n*" nil t)
      (replace-match "\n")))
;;;  (run-hooks 'wl-mail-send-pre-hook) ;; X-PGP-Sig, Cancel-Lock
  (wl-draft-dispatch-message)
  (when kill-when-done
    ;; hide editing-buffer.
    (wl-draft-hide editing-buffer)
    ;; delete editing-buffer and its file.
    (wl-draft-delete editing-buffer)))

(defun wl-draft-dispatch-message (&optional mes-string)
  "Send the message in the current buffer.  Not modified the header fields."
  (let (delimline)
    (if (and wl-draft-verbose-send mes-string)
	(message mes-string))
    ;; get fcc folders.
    (setq delimline (wl-draft-get-header-delimiter t))
    (unless wl-draft-fcc-list
      (setq wl-draft-fcc-list (wl-draft-get-fcc-list delimline)))
    ;;
    (setq wl-sent-message-modified nil)
    (unwind-protect
	(progn
	  (if (and (wl-message-mail-p)
		   (not (wl-draft-sent-message-p 'mail)))
	      (if (or (not (or wl-draft-force-queuing
			       wl-draft-force-queuing-mail))
		      (memq 'mail wl-sent-message-queued))
		  (funcall wl-draft-send-mail-function)
		(push 'mail wl-sent-message-queued)
		(wl-draft-set-sent-message 'mail 'unplugged)))
	  (if (and (wl-message-news-p)
		   (not (wl-draft-sent-message-p 'news))
		   (not (wl-message-field-exists-p "Resent-to")))
	      (if (or (not (or wl-draft-force-queuing
			       wl-draft-force-queuing-news))
		      (memq 'news wl-sent-message-queued))
		  (funcall wl-draft-send-news-function)
		(push 'news wl-sent-message-queued)
		(wl-draft-set-sent-message 'news 'unplugged))))
      ;;
      (let* ((status (wl-draft-sent-message-results))
	     (unplugged-via (car status))
	     (sent-via (nth 1 status)))
	;; If one sent, process fcc folder.
	(if (and sent-via wl-draft-fcc-list)
	    (progn
	      (wl-draft-do-fcc (wl-draft-get-header-delimiter) wl-draft-fcc-list)
	      (setq wl-draft-fcc-list nil)))
	(if wl-draft-use-cache
	    (let ((id (std11-field-body "Message-ID"))
		  (elmo-enable-disconnected-operation t))
	      (elmo-file-cache-save (elmo-file-cache-get-path id)
				    nil)))
	;; If one unplugged, append queue.
	(when (and unplugged-via
		   wl-sent-message-modified)
	  (if wl-draft-enable-queuing
	      (progn
		(wl-draft-queue-append wl-sent-message-via)
		(setq wl-sent-message-modified 'requeue))
	    (error "Unplugged")))
	(when wl-draft-verbose-send
	  (if (and unplugged-via sent-via);; combined message
	      (progn
		(setq wl-draft-verbose-msg
		      (format "Sending%s and Queuing%s..."
			      sent-via unplugged-via))
		(message (concat wl-draft-verbose-msg "done")))
	    (if mes-string
		(message (concat mes-string
				 (if sent-via "done" "failed")))))))))
  (not wl-sent-message-modified)) ;; return value

(defun wl-draft-raw-send (&optional kill-when-done force-pre-hook mes-string)
  "Force send current buffer as raw message."
  (interactive)
  (save-excursion
    (let (wl-interactive-send
;;;	  wl-draft-verbose-send
	  (wl-mail-send-pre-hook (and force-pre-hook wl-mail-send-pre-hook))
	  (wl-news-send-pre-hook (and force-pre-hook wl-news-send-pre-hook))
	  mail-send-hook
	  mail-send-actions)
      (wl-draft-send kill-when-done mes-string))))

(defun wl-draft-clone-local-variables ()
  (let ((locals (buffer-local-variables))
	result)
    (while locals
      (when (and (consp (car locals))
		 (car (car locals))
		 (string-match wl-draft-clone-local-variable-regexp
			       (symbol-name (car (car locals)))))
	(wl-append result (list (car (car locals)))))
      (setq locals (cdr locals)))
    result))

(defun wl-draft-send (&optional kill-when-done mes-string)
  "Send current draft message.
If KILL-WHEN-DONE is non-nil, current draft buffer is killed"
  (interactive)
  ;; Don't call this explicitly.
  ;; Added to 'wl-draft-send-hook (by teranisi)
  ;; (wl-draft-config-exec)
  (run-hooks 'wl-draft-send-hook)
  (when (or (not wl-interactive-send)
	    (y-or-n-p "Do you really want to send current draft? "))
    (let ((send-mail-function 'wl-draft-raw-send)
	  (editing-buffer (current-buffer))
	  (sending-buffer (wl-draft-generate-clone-buffer
			   " *wl-draft-sending-buffer*"
			   (append wl-draft-config-variables
				   (wl-draft-clone-local-variables))))
	  (wl-draft-verbose-msg nil)
	  err)
      (unwind-protect
	  (save-excursion (set-buffer sending-buffer)
	    (if (and (not (wl-message-mail-p))
		     (not (wl-message-news-p)))
		(error "No recipient is specified"))
	    (expand-abbrev) ; for mail-abbrevs
	    (run-hooks 'mail-send-hook) ; translate buffer
	    (if wl-draft-verbose-send
		(message (or mes-string "Sending...")))
	    (funcall wl-draft-send-function editing-buffer kill-when-done)
	    ;; Now perform actions on successful sending.
	    (while mail-send-actions
	      (condition-case ()
		  (apply (car (car mail-send-actions))
			 (cdr (car mail-send-actions)))
		(error))
	      (setq mail-send-actions (cdr mail-send-actions)))
;;	    (if (or (eq major-mode 'wl-draft-mode)
;;		    (eq major-mode 'mail-mode))
;;		(local-set-key "\C-c\C-s" 'wl-draft-send)) ; override
	    (if wl-draft-verbose-send
		(message (concat (or wl-draft-verbose-msg
				     mes-string "Sending...")
				 "done"))))
	;; kill sending buffer, anyway.
	(and (buffer-live-p sending-buffer)
	     (kill-buffer sending-buffer))))))

(defun wl-draft-save ()
  "Save current draft.
Derived from `message-save-drafts' in T-gnus."
  (interactive)
  (if (buffer-modified-p)
      (progn
	(message "Saving %s..." wl-draft-buffer-file-name)
	(let ((msg (buffer-substring-no-properties (point-min) (point-max))))
	  (with-temp-file wl-draft-buffer-file-name
	    (insert msg)
	    ;; XXX Discard error to ignore invalid content. Is it dangerous?
	    (condition-case nil
		(mime-edit-translate-buffer)
	      (error))
	    (wl-draft-get-header-delimiter t)))
	(set-buffer-modified-p nil)
	(wl-draft-config-info-operation
	 (and (string-match "[0-9]+$" wl-draft-buffer-file-name)
	      (string-to-int
	       (match-string 0 wl-draft-buffer-file-name)))
	 'save)
	(message "Saving %s...done" wl-draft-buffer-file-name))
    (message "(No changes need to be saved)")))

(defun wl-draft-mimic-kill-buffer ()
  "Kill the current (draft) buffer with query."
  (interactive)
  (let ((bufname (read-buffer (format "Kill buffer: (default %s) "
				      (buffer-name))))
	wl-draft-use-frame)
    (if (or (not bufname)
	    (string-equal bufname "")
	    (string-equal bufname (buffer-name)))
	(wl-draft-save-and-exit)
      (kill-buffer bufname))))

(defun wl-draft-save-and-exit ()
  "Save current draft and exit current draft mode."
  (interactive)
  (wl-draft-save)
  (let ((editing-buffer (current-buffer)))
    (wl-draft-hide editing-buffer)
    (kill-buffer editing-buffer)))

(defun wl-draft-send-and-exit ()
  "Send current draft message and kill it."
  (interactive)
  (wl-draft-send t))

(defun wl-draft-send-from-toolbar ()
  (interactive)
  (let ((wl-interactive-send t))
    (wl-draft-send-and-exit)))

(defun wl-draft-delete-field (field &optional delimline replace)
  (wl-draft-delete-fields (regexp-quote field) delimline replace))

(defun wl-draft-delete-fields (field &optional delimline replace)
  (save-restriction
    (unless delimline
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t)
	  (setq delimline (point))
	(setq delimline (point-max))))
    (narrow-to-region (point-min) delimline)
    (goto-char (point-min))
    (let ((regexp (concat "^" field ":"))
	  (case-fold-search t))
      (while (not (eobp))
	(if (looking-at regexp)
	    (progn
	      (delete-region
	       (point)
	       (progn
		 (forward-line 1)
		 (if (re-search-forward "^[^ \t]" nil t)
		     (goto-char (match-beginning 0))
		   (point-max))))
	      (if replace
		  (insert (concat field ": " replace "\n"))))
	  (forward-line 1)
	  (if (re-search-forward "^[^ \t]" nil t)
	      (goto-char (match-beginning 0))
	    (point-max)))))))

(defun wl-draft-get-fcc-list (header-end)
  (let (fcc-list
	(case-fold-search t))
    (or (markerp header-end) (error "HEADER-END must be a marker"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^Fcc:[ \t]*" header-end t)
	(setq fcc-list
	      (cons (buffer-substring-no-properties
		     (point)
		     (progn
		       (end-of-line)
		       (skip-chars-backward " \t")
		       (point)))
		    fcc-list))
	(save-match-data
	  (wl-folder-confirm-existence
	   (wl-folder-get-elmo-folder (eword-decode-string (car fcc-list)))))
	(delete-region (match-beginning 0)
		       (progn (forward-line 1) (point)))))
    fcc-list))

(defun wl-draft-do-fcc (header-end &optional fcc-list)
  (let ((send-mail-buffer (current-buffer))
	(tembuf (generate-new-buffer " fcc output"))
	(case-fold-search t)
	beg end)
    (or (markerp header-end) (error "HEADER-END must be a marker"))
    (save-excursion
      (unless fcc-list
	(setq fcc-list (wl-draft-get-fcc-list header-end)))
      (set-buffer tembuf)
      (erase-buffer)
      ;; insert just the headers to avoid moving the gap more than
      ;; necessary (the message body could be arbitrarily huge.)
      (insert-buffer-substring send-mail-buffer 1 header-end)
      (wl-draft-insert-required-fields t)
      (goto-char (point-max))
      (insert-buffer-substring send-mail-buffer header-end)
      (let ((id (std11-field-body "Message-ID"))
	    (elmo-enable-disconnected-operation t)
	    cache-saved)
	(while fcc-list
	  (unless (or cache-saved
		      (elmo-folder-plugged-p
		       (wl-folder-get-elmo-folder (car fcc-list))))
	    (elmo-file-cache-save id nil) ;; for disconnected operation
	    (setq cache-saved t))
	  (if (elmo-folder-append-buffer
	       (wl-folder-get-elmo-folder
		(eword-decode-string (car fcc-list)))
	       (not wl-fcc-force-as-read))
	      (wl-draft-write-sendlog 'ok 'fcc nil (car fcc-list) id)
	    (wl-draft-write-sendlog 'failed 'fcc nil (car fcc-list) id))
	  (setq fcc-list (cdr fcc-list)))))
    (kill-buffer tembuf)))

(defun wl-draft-on-field-p ()
  (if (< (point)
	 (save-excursion
	   (goto-char (point-min))
	   (search-forward (concat "\n" mail-header-separator "\n") nil 0)
	   (point)))
      (if (bolp)
	  (if (bobp)
	      t
	    (save-excursion
	      (forward-line -1)
	      (if (or (looking-at ".*,[ \t]?$")
		      (looking-at "^[^ \t]+:[ \t]+.*:$")); group list name
		  nil t)))
	(let ((pos (point)))
	  (save-excursion
	    (beginning-of-line)
	    (if (looking-at "^[ \t]")
		nil
	      (if (re-search-forward ":" pos t) nil t)))))))

(defun wl-draft-random-alphabet ()
  (let ((alphabet '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z)))
    (nth (abs (% (random) 26)) alphabet)))

;;;;;;;;;;;;;;;;
;;;###autoload
(defun wl-draft (&optional to subject in-reply-to cc references newsgroups
			   mail-followup-to
			   content-type content-transfer-encoding
			   body edit-again summary-buf from parent-folder)
  "Write and send mail/news message with Wanderlust."
  (interactive)
  (require 'wl)
  (unless wl-init
    (wl-load-profile)
    (wl-folder-init)
    (elmo-init)
    (wl-plugged-init t))
  (let (wl-demo)
    (wl-init)) ; returns immediately if already initialized.

  (let (buf-name header-alist)
    (setq buf-name
	  (wl-draft-create-buffer
	   (or
	    (eq this-command 'wl-draft)
	    (eq this-command 'wl-summary-write)
	    (eq this-command 'wl-summary-write-current-folder))
	   parent-folder summary-buf))
    (setq header-alist
	  (list
	   (cons "From: " (or from wl-from))
	   (cons "To: " (or to
			    (and
			     (or (interactive-p)
				 (eq this-command 'wl-summary-write))
			     "")))
	   (cons "Cc: " cc)
	   (cons "Subject: " (or subject ""))
	   (cons "Newsgroups: " newsgroups)
	   (cons "Mail-Followup-To: " mail-followup-to)
	   (cons "In-Reply-To: " in-reply-to)
	   (cons "References: " references)))
    (setq header-alist (append header-alist
			       (wl-draft-default-headers)
			       (if body (list "" body))))
    (wl-draft-create-contents header-alist)
    (if edit-again
	(wl-draft-decode-body
	 content-type content-transfer-encoding))
    (wl-draft-insert-mail-header-separator)
    (wl-draft-prepare-edit)
    (if (interactive-p)
	(run-hooks 'wl-mail-setup-hook))

    (goto-char (point-min))
    (wl-user-agent-compose-internal) ;; user-agent
    (cond ((eq this-command 'wl-summary-write-current-newsgroup)
	   (mail-position-on-field "Subject"))
	  ((and (interactive-p) (null to))
	   (mail-position-on-field "To"))
	  (t
	   (goto-char (point-max))))
    buf-name))

(defun wl-draft-create-buffer (&optional full parent-folder summary-buf)
  (let* ((draft-folder (wl-folder-get-elmo-folder wl-draft-folder))
	 (parent-folder (or parent-folder (wl-summary-buffer-folder-name)))
	 (summary-buf (or summary-buf (wl-summary-get-buffer parent-folder)))
	buf-name file-name num change-major-mode-hook)
    (if (not (elmo-folder-message-file-p draft-folder))
	(error "%s folder cannot be used for draft folder" wl-draft-folder))
    (setq num (elmo-max-of-list
	       (or (elmo-folder-list-messages draft-folder) '(0))))
    (setq num (+ 1 num))
    ;; To get unused buffer name.
    (while (get-buffer (concat wl-draft-folder "/" (int-to-string num)))
      (setq num (+ 1 num)))
    (setq buf-name (find-file-noselect
		    (setq file-name
			  (elmo-message-file-name
			   (wl-folder-get-elmo-folder wl-draft-folder)
			   num))))
    (if wl-draft-use-frame
	(switch-to-buffer-other-frame buf-name)
      (switch-to-buffer buf-name))
    (set-buffer buf-name)
    (if (not (string-match (regexp-quote wl-draft-folder)
			   (buffer-name)))
	(rename-buffer (concat wl-draft-folder "/" (int-to-string num))))
    (if (or (eq wl-draft-reply-buffer-style 'full)
	    full)
	(delete-other-windows))
    (auto-save-mode -1)
    (wl-draft-mode)
    (make-local-variable 'truncate-partial-width-windows)
    (setq truncate-partial-width-windows nil)
    (setq truncate-lines wl-draft-truncate-lines)
    ;; Don't care about supersession.
    (setq buffer-file-name nil)
    (setq wl-sent-message-via nil)
    (setq wl-sent-message-queued nil)
    (setq wl-draft-buffer-file-name file-name)
    (setq wl-draft-config-exec-flag t)
    (setq wl-draft-parent-folder parent-folder)
    (setq wl-draft-buffer-cur-summary-buffer summary-buf)
    buf-name))

(defun wl-draft-create-contents (header-alist)
  "header-alist' sample
'(function  ;; funcall
  string    ;; insert string
  (string . string)    ;;  insert string string
  (string . function)  ;;  insert string (funcall)
  (string . nil)       ;;  insert nothing
  (function . (arg1 arg2 ..))  ;; call function with argument
  nil                  ;;  insert nothing
"
  (unless (eq major-mode 'wl-draft-mode)
    (error "wl-draft-create-header must be use in wl-draft-mode."))
  (let ((halist header-alist)
	field value)
    (while halist
      (cond
       ;; function
       ((functionp (car halist)) (funcall (car halist)))
       ;; string
       ((stringp (car halist)) (insert (car halist) "\n"))
       ;; cons
       ((consp (car halist))
	(setq field (car (car halist)))
	(setq value (cdr (car halist)))
	(cond
	 ((functionp field) (apply field value))
	 ((stringp field)
	  (cond
	   ((stringp value) (insert field value "\n"))
	   ((functionp value) (insert field (funcall value) "\n"))
	   ((not value))
	   (t
	    (debug))))
	 ;;
	 ((not field))
	 (t
	  (debug))
	 )))
      (setq halist (cdr halist)))))

(defun wl-draft-prepare-edit ()
  (unless (eq major-mode 'wl-draft-mode)
    (error "wl-draft-create-header must be use in wl-draft-mode."))
  (let (change-major-mode-hook)
    (wl-draft-editor-mode)
    (wl-draft-overload-functions)
    (wl-highlight-headers 'for-draft)
    (wl-draft-save)))

(defun wl-draft-decode-header ()
  (save-excursion
    (std11-narrow-to-header)
    (wl-draft-decode-message-in-buffer)
    (widen)))

(defun wl-draft-decode-body (&optional content-type content-transfer-encoding)
  (let ((content-type
	 (or content-type
		(std11-field-body "content-type")))
	(content-transfer-encoding
	 (or content-transfer-encoding
	     (std11-field-body "content-transfer-encoding")))
	delimline)
    (save-excursion
      (std11-narrow-to-header)
      (wl-draft-delete-field "content-type")
      (wl-draft-delete-field "content-transfer-encoding")
      (goto-char (point-max))
      (setq delimline (point-marker))
      (widen)
      (narrow-to-region delimline (point-max))
      (goto-char (point-min))
      (when content-type
	(insert "Content-type: " content-type "\n"))
      (when content-transfer-encoding
	(insert "Content-Transfer-Encoding: " content-transfer-encoding "\n"))
      (wl-draft-decode-message-in-buffer)
      (goto-char (point-min))
      (unless (re-search-forward "^$" (point-at-eol) t)
	(insert "\n"))
      (widen)
      delimline)))

;;; subroutine for wl-draft-create-contents
;;; must be used in wl-draft-mode
(defun wl-draft-check-new-line ()
  (if (not (= (preceding-char) ?\n))
      (insert ?\n)))

(defsubst wl-draft-insert-ccs (str cc)
  (let ((field
	 (if (functionp cc)
	     (funcall cc)
	   cc)))
    (if (and field
	     (null (and wl-draft-delete-myself-from-bcc-fcc
			(elmo-list-member
			 (mapcar 'wl-address-header-extract-address
				 (append
				  (wl-parse-addresses (std11-field-body "To"))
				  (wl-parse-addresses (std11-field-body "Cc"))))
			 (mapcar 'downcase wl-subscribed-mailing-list)))))
	(insert str field "\n"))))

(defsubst wl-draft-default-headers ()
  (list
   (cons "Mail-Reply-To: " (and wl-insert-mail-reply-to
				(wl-address-header-extract-address
				 wl-from)))
   (cons "" wl-generate-mailer-string-function)
   (cons "Reply-To: " mail-default-reply-to)
   (cons 'wl-draft-insert-ccs
	 (list "Bcc: " (or wl-bcc
			   (and mail-self-blind (user-login-name)))))
   (cons 'wl-draft-insert-ccs
	 (list "Fcc: " wl-fcc))
   (cons "Organization: " wl-organization)
   (and wl-auto-insert-x-face
	(file-exists-p wl-x-face-file)
	'wl-draft-insert-x-face-field-here) ;; allow nil
   mail-default-headers
   ;; check \n at th end of line for `mail-default-headers'
   'wl-draft-check-new-line
;   wl-draft-default-headers
;   'wl-draft-check-new-line
   ))

(defun wl-draft-insert-mail-header-separator (&optional delimline)
  (save-excursion
    (if delimline
	(goto-char delimline)
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t)
	  (delete-backward-char 1)
	(goto-char (point-max))))
    (wl-draft-check-new-line)
    (put-text-property (point)
		       (progn
			 (insert mail-header-separator "\n")
			 (1- (point)))
		       'category 'mail-header-separator)))

;;;;;;;;;;;;;;;;

(defun wl-draft-elmo-nntp-send ()
  (let ((elmo-nntp-post-pre-hook wl-news-send-pre-hook)
	(elmo-nntp-default-user
	 (or wl-nntp-posting-user elmo-nntp-default-user))
	(elmo-nntp-default-server
	 (or wl-nntp-posting-server elmo-nntp-default-server))
	(elmo-nntp-default-port
	 (or wl-nntp-posting-port elmo-nntp-default-port))
	(elmo-nntp-default-stream-type
	 (or wl-nntp-posting-stream-type elmo-nntp-default-stream-type)))
    (if (not (elmo-plugged-p elmo-nntp-default-server elmo-nntp-default-port))
	(wl-draft-set-sent-message 'news 'unplugged
				   (cons elmo-nntp-default-server
					 elmo-nntp-default-port))
      (elmo-nntp-post elmo-nntp-default-server (current-buffer))
      (wl-draft-set-sent-message 'news 'sent)
      (wl-draft-write-sendlog 'ok 'nntp elmo-nntp-default-server
			      (std11-field-body "Newsgroups")
			      (std11-field-body "Message-ID")))))

(defun wl-draft-generate-clone-buffer (name &optional local-variables)
  "Generate clone of current buffer named NAME."
  (let ((editing-buffer (current-buffer)))
    (save-excursion
      (set-buffer (generate-new-buffer name))
      (erase-buffer)
      (wl-draft-mode)
      (wl-draft-editor-mode)
      (insert-buffer editing-buffer)
      (message "")
      (while local-variables
	(make-local-variable (car local-variables))
	(set (car local-variables)
	     (save-excursion
	       (set-buffer editing-buffer)
	       (symbol-value (car local-variables))))
	(setq local-variables (cdr local-variables)))
      (current-buffer))))

(defun wl-draft-reedit (number)
  (let ((draft-folder (wl-folder-get-elmo-folder wl-draft-folder))
	(wl-draft-reedit t)
	buffer file-name change-major-mode-hook)
    (setq file-name (elmo-message-file-name draft-folder number))
    (unless (file-exists-p file-name)
      (error "File %s does not exist" file-name))
    (if (setq buffer (get-buffer 
		      (concat wl-draft-folder "/"
			      (number-to-string number))))
	(progn
	  (if wl-draft-use-frame
	      (switch-to-buffer-other-frame buffer)
	    (switch-to-buffer buffer))
	  (set-buffer buffer))
      (setq buffer (get-buffer-create (number-to-string number)))
      (if wl-draft-use-frame
	  (switch-to-buffer-other-frame buffer)
	(switch-to-buffer buffer))
      (set-buffer buffer)
      (insert-file-contents-as-binary file-name)
      (let((mime-edit-again-ignored-field-regexp
	    "^\\(Content-.*\\|Mime-Version\\):"))
	(wl-draft-decode-message-in-buffer))
      (wl-draft-insert-mail-header-separator)
      (if wl-draft-use-frame
	  (switch-to-buffer-other-frame buffer)
	(switch-to-buffer buffer))
      (set-buffer buffer)
      (if (not (string-match (regexp-quote wl-draft-folder)
			     (buffer-name)))
	  (rename-buffer (concat wl-draft-folder "/" (buffer-name))))
      (auto-save-mode -1)
      (wl-draft-mode)
      ;; Don't care about supersession.
      (make-local-variable 'truncate-partial-width-windows)
      (setq truncate-partial-width-windows nil)
      (setq truncate-lines wl-draft-truncate-lines)    
      (setq buffer-file-name nil)    
      (setq wl-sent-message-via nil)
      (setq wl-sent-message-queued nil)
      (setq wl-draft-buffer-file-name file-name)
      (wl-draft-config-info-operation number 'load)
      (goto-char (point-min))
      (wl-draft-overload-functions)
      (wl-draft-editor-mode)
      (wl-highlight-headers 'for-draft)
      (run-hooks 'wl-draft-reedit-hook)
      (goto-char (point-max))
      buffer)))

(defmacro wl-draft-body-goto-top ()
  (` (progn
       (goto-char (point-min))
       (if (re-search-forward mail-header-separator nil t)
	   (forward-char 1)
	 (goto-char (point-max))))))

(defmacro wl-draft-body-goto-bottom ()
  (` (goto-char (point-max))))

(defmacro wl-draft-config-body-goto-header ()
  (` (progn
       (goto-char (point-min))
       (if (re-search-forward mail-header-separator nil t)
	   (beginning-of-line)
	 (goto-char (point-max))))))

(defun wl-draft-config-sub-body (content)
  (wl-draft-body-goto-top)
  (delete-region (point) (point-max))
  (if content (insert (eval content))))

(defun wl-draft-config-sub-top (content)
  (wl-draft-body-goto-top)
  (if content (insert (eval content))))

(defun wl-draft-config-sub-bottom (content)
  (wl-draft-body-goto-bottom)
  (if content (insert (eval content))))

(defun wl-draft-config-sub-header (content)
  (wl-draft-config-body-goto-header)
  (if content (insert (concat (eval content) "\n"))))

(defun wl-draft-config-sub-header-top (content)
  (goto-char (point-min))
  (if content (insert (concat (eval content) "\n"))))

(defun wl-draft-config-sub-part-top (content)
  (goto-char (mime-edit-content-beginning))
  (if content (insert (concat (eval content) "\n"))))

(defun wl-draft-config-sub-part-bottom (content)
  (goto-char (mime-edit-content-end))
  (if content (insert (concat (eval content) "\n"))))

(defsubst wl-draft-config-sub-file (content)
  (let ((coding-system-for-read wl-cs-autoconv)
	(file (expand-file-name (eval content))))
    (if (file-exists-p file)
	(insert-file-contents file)
      (error "%s: no exists file" file))))

(defun wl-draft-config-sub-body-file (content)
  (wl-draft-body-goto-top)
  (delete-region (point) (point-max))
  (wl-draft-config-sub-file content))

(defun wl-draft-config-sub-top-file (content)
  (wl-draft-body-goto-top)
  (wl-draft-config-sub-file content))

(defun wl-draft-config-sub-bottom-file (content)
  (wl-draft-body-goto-bottom)
  (wl-draft-config-sub-file content))

(defun wl-draft-config-sub-header-file (content)
  (wl-draft-config-body-goto-header)
  (wl-draft-config-sub-file content))

(defun wl-draft-config-sub-template (content)
  (setq wl-draft-config-variables
	(wl-template-insert (eval content))))

(defun wl-draft-config-sub-x-face (content)
  (if (and (string-match "\\.xbm\\(\\.gz\\)?$" content)
	   (fboundp 'x-face-insert)) ; x-face.el is installed.
      (x-face-insert content)
    (wl-draft-replace-field "X-Face" (elmo-get-file-string content t) t)))

(defsubst wl-draft-config-sub-func (field content)
  (let (func)
    (if (setq func (assq field wl-draft-config-sub-func-alist))
	(let (wl-draft-config-variables)
	  (funcall (cdr func) content)
	  ;; for wl-draft-config-sub-template
	  (cons t wl-draft-config-variables)))))

(defsubst wl-draft-config-exec-sub (clist)
  (let (config local-variables)
    (while clist
      (setq config (car clist))
      (cond
       ((functionp config)
	(funcall config))
       ((consp config)
	(let ((field (car config))
	      (content (cdr config))
	      ret-val)
	  (cond
	   ((stringp field)
	    (wl-draft-replace-field field (eval content) t))
	   ((setq ret-val (wl-draft-config-sub-func field content))
	    (if (cdr ret-val) ;; for wl-draft-config-sub-template
		(wl-append local-variables (cdr ret-val))))
	   ((boundp field) ;; variable
	    (make-local-variable field)
	    (set field (eval content))
	    (wl-append local-variables (list field)))
	   (t
	    (error "%s: not variable" field)))))
       (t
	(error "%s: not supported type" config)))
      (setq clist (cdr clist)))
    local-variables))

(defun wl-draft-prepared-config-exec (&optional config-alist reply-buf)
  "Change headers in draft preparation time."
  (interactive)
  (unless wl-draft-reedit
    (let ((config-alist
	   (or config-alist
	       (and (boundp 'wl-draft-prepared-config-alist)
		    wl-draft-prepared-config-alist)	;; For compatible.
	       wl-draft-config-alist)))
      (if config-alist
	  (wl-draft-config-exec config-alist reply-buf)))))

(defun wl-draft-config-exec (&optional config-alist reply-buf)
  "Change headers in draft sending time."
  (interactive)
  (let ((case-fold-search t)
	(alist (or config-alist wl-draft-config-alist))
	(reply-buf (or reply-buf (and (buffer-live-p wl-draft-reply-buffer)
				      wl-draft-reply-buffer)))
	(local-variables wl-draft-config-variables)
	key clist found)
    (when (and (or (interactive-p)
		   wl-draft-config-exec-flag)
	       alist)
      (save-excursion
	(catch 'done
	  (while alist
	    (setq key (caar alist)
		  clist (cdar alist))
	    (cond
	     ((eq key 'reply)
	      (when (and
		     reply-buf
		     (save-excursion
		       (set-buffer reply-buf)
		       (save-restriction
			 (std11-narrow-to-header)
			 (goto-char (point-min))
			 (re-search-forward (car clist) nil t))))
		(wl-draft-config-exec-sub (cdr clist))
		(setq found t)))
	     ((stringp key)
	      (when (save-restriction
		      (std11-narrow-to-header mail-header-separator)
		      (goto-char (point-min))
		      (re-search-forward key nil t))
		(wl-append local-variables
			   (wl-draft-config-exec-sub clist))
		(setq found t)))
	     ((eval key)
	      (wl-append local-variables
			 (wl-draft-config-exec-sub clist))
	      (setq found t)))
	    (if (and found wl-draft-config-matchone)
		(throw 'done t))
	    (setq alist (cdr alist))))
	(if found
	    (setq wl-draft-config-exec-flag nil))
	(run-hooks 'wl-draft-config-exec-hook)
	(put-text-property (point-min)(point-max) 'face nil)
	(wl-highlight-message (point-min)(point-max) t)
	(setq wl-draft-config-variables
	      (elmo-uniq-list local-variables))))))

(defun wl-draft-replace-field (field content &optional add)
  (save-excursion
    (save-restriction
      (let ((case-fold-search t)
	    (inhibit-read-only t) ;; added by teranisi.
	    beg)
	(std11-narrow-to-header mail-header-separator)
	(goto-char (point-min))
	(if (re-search-forward (concat "^" (regexp-quote field) ":") nil t)
	    (if content
		;; replace field
		(progn
		  (setq beg (point))
		  (re-search-forward "^[^ \t]" nil 'move)
		  (beginning-of-line)
		  (skip-chars-backward "\n")
		  (delete-region beg (point))
		  (insert " " content))
	      ;; delete field
	      (save-excursion
		(beginning-of-line)
		(setq beg (point)))
	      (re-search-forward "^[^ \t]" nil 'move)
	      (beginning-of-line)
	      (delete-region beg (point)))
	  (when (and add content)
	    ;; add field
	    (goto-char (point-max))
	    (insert (concat field ": " content "\n"))))))))

(defun wl-draft-config-info-operation (msg operation)
  (let* ((msgdb-dir (elmo-folder-msgdb-path (wl-folder-get-elmo-folder
					     wl-draft-folder)))
	 (filename
	  (expand-file-name
	   (format "%s-%d" wl-draft-config-save-filename msg)
	   msgdb-dir))
	 element alist variable)
    (cond
     ((eq operation 'save)
      (let ((variables (elmo-uniq-list wl-draft-config-variables)))
	(while (setq variable (pop variables))
	  (when (boundp variable)
	    (wl-append alist
		       (list (cons variable (eval variable))))))
	(elmo-object-save filename alist)))
     ((eq operation 'load)
      (setq alist (elmo-object-load filename))
      (while (setq element (pop alist))
	(set (make-local-variable (car element)) (cdr element))
	(wl-append wl-draft-config-variables (list (car element)))))
     ((eq operation 'delete)
      (if (file-exists-p filename)
	  (delete-file filename))))))

(defun wl-draft-queue-info-operation (msg operation
					  &optional add-sent-message-via)
  (let* ((msgdb-dir (elmo-folder-msgdb-path
		     (wl-folder-get-elmo-folder wl-queue-folder)))
	 (filename
	  (expand-file-name
	   (format "%s-%d" wl-draft-queue-save-filename msg)
	   msgdb-dir))
	 element alist variable)
    (cond
     ((eq operation 'save)
      (let ((variables (elmo-uniq-list
			(append wl-draft-queue-save-variables
				wl-draft-config-variables
				(list 'wl-draft-fcc-list)))))
	(if add-sent-message-via
	    (progn
	      (push 'wl-sent-message-queued variables)
	      (push 'wl-sent-message-via variables)))
	(while (setq variable (pop variables))
	  (when (boundp variable)
	    (wl-append alist
		       (list (cons variable (eval variable))))))
	(elmo-object-save filename alist)))
     ((eq operation 'load)
      (setq alist (elmo-object-load filename))
      (while (setq element (pop alist))
	(set (make-local-variable (car element)) (cdr element))))
     ((eq operation 'get-sent-via)
      (setq alist (elmo-object-load filename))
      (cdr (assq 'wl-sent-message-via alist)))
     ((eq operation 'delete)
      (if (file-exists-p filename)
	  (delete-file filename))))))

(defun wl-draft-queue-append (wl-sent-message-via)
  (if wl-draft-verbose-send
      (message "Queuing..."))
  (let ((send-buffer (current-buffer))
	(folder (wl-folder-get-elmo-folder wl-queue-folder))
	(message-id (std11-field-body "Message-ID")))
    (if (elmo-folder-append-buffer folder t)
	(progn
	  (wl-draft-queue-info-operation
	   (car (elmo-folder-status folder))
	   'save wl-sent-message-via)
	  (wl-draft-write-sendlog 'ok 'queue nil wl-queue-folder message-id)
	  (when wl-draft-verbose-send
	    (setq wl-draft-verbose-msg "Queuing...")
	    (message "Queuing...done")))
      (wl-draft-write-sendlog 'failed 'queue nil wl-queue-folder message-id)
      (error "Queuing failed"))))

(defun wl-draft-queue-flush ()
  "Flush draft queue."
  (interactive)
  (let* ((queue-folder (wl-folder-get-elmo-folder wl-queue-folder))
	 (msgs2 (progn
		  (elmo-folder-open-internal queue-folder)
		  (elmo-folder-list-messages queue-folder)))
	 (i 0)
	 (performed 0)
	 (wl-draft-queue-flushing t)
	 msgs failure len buffer msgid sent-via)
    ;; get plugged send message
    (while msgs2
      (setq sent-via (wl-draft-queue-info-operation (car msgs2) 'get-sent-via))
      (catch 'found
	(while sent-via
	  (when (and (eq (nth 1 (car sent-via)) 'unplugged)
		     (or (not (nth 2 (car sent-via)))
			 (elmo-plugged-p
			  (car (nth 2 (car sent-via)))
			  (cdr (nth 2 (car sent-via))))))
	    (wl-append msgs (list (car msgs2)))
	    (throw 'found t))
	  (setq sent-via (cdr sent-via))))
      (setq msgs2 (cdr msgs2)))
    (when (> (setq len (length msgs)) 0)
      (if (elmo-y-or-n-p (format
			  "%d message(s) are in the sending queue.  Send now? "
			  len)
			 (not elmo-dop-flush-confirm) t)
	  (progn
	    (save-excursion
	      (setq buffer (get-buffer-create " *wl-draft-queue-flush*"))
	      (set-buffer buffer)
	      (while msgs
		;; reset buffer local variables
		(kill-all-local-variables)
		(erase-buffer)
		(setq i (+ 1 i)
		      failure nil)
		(setq wl-sent-message-via nil)
		(wl-draft-queue-info-operation (car msgs) 'load)
		(elmo-message-fetch queue-folder
				    (car msgs)
				    (elmo-make-fetch-strategy 'entire)
				    nil (current-buffer))
		(condition-case err
		    (setq failure (funcall
				   wl-draft-queue-flush-send-function
				   (format "Sending (%d/%d)..." i len)))
;;;		  (wl-draft-raw-send nil nil
;;;				     (format "Sending (%d/%d)..." i len))
		  (error
		   (elmo-display-error err t)
		   (setq failure t))
		  (quit
		   (setq failure t)))
		(if (eq wl-sent-message-modified 'requeue)
		    (progn
		      (elmo-folder-delete-messages
		       queue-folder (cons (car msgs) nil))
		      (wl-draft-queue-info-operation (car msgs) 'delete))
		  (unless failure
		    (elmo-folder-delete-messages
		     queue-folder (cons (car msgs) nil))
		    (wl-draft-queue-info-operation (car msgs) 'delete)
		    (setq performed (+ 1 performed))))
		(setq msgs (cdr msgs)))
	      (kill-buffer buffer)
	      (message "%d message(s) are sent." performed)))
	(message "%d message(s) are remained to be sent." len))
      (elmo-folder-close queue-folder)
      len)))

(defun wl-jump-to-draft-buffer (&optional arg)
  "Jump to the draft if exists."
  (interactive "P")
  (if arg
      (wl-jump-to-draft-folder)
    (let ((bufs (buffer-list))
	  (draft-regexp (concat
			 "^" (regexp-quote
			      (elmo-localdir-folder-directory-internal
			       (wl-folder-get-elmo-folder wl-draft-folder)))))
	  buf draft-bufs)
      (while bufs
	(if (and
	     (setq buf (with-current-buffer (car bufs)
			 wl-draft-buffer-file-name))
	     (string-match draft-regexp buf))
	    (setq draft-bufs (cons (buffer-name (car bufs)) draft-bufs)))
	(setq bufs (cdr bufs)))
      (cond
       ((null draft-bufs)
	(message "No draft buffer exist."))
       (t
	(setq draft-bufs
	      (sort draft-bufs (function (lambda (a b) (not (string< a b))))))
	(if (setq buf (cdr (member (buffer-name) draft-bufs)))
	    (setq buf (car buf))
	  (setq buf (car draft-bufs)))
	(switch-to-buffer buf))))))

(defun wl-jump-to-draft-folder ()
  (let ((msgs (reverse (elmo-folder-list-messages (wl-folder-get-elmo-folder
						   wl-draft-folder))))
	(mybuf (buffer-name))
	msg buf)
    (if (not msgs)
	(message "No draft message exist.")
      (if (string-match (concat "^" wl-draft-folder "/") mybuf)
	  (setq msg (cadr (memq
			   (string-to-int (substring mybuf (match-end 0)))
			   msgs))))
      (or msg (setq msg (car msgs)))
      (if (setq buf (get-buffer (format "%s/%d" wl-draft-folder msg)))
	  (switch-to-buffer buf)
	(wl-draft-reedit msg)))))

(defun wl-draft-highlight-and-recenter (&optional n)
  (interactive "P")
  (if wl-highlight-body-too
      (let ((beg (point-min))
	    (end (point-max)))
	(put-text-property beg end 'face nil)
	(wl-highlight-message beg end t)))
  (recenter n))

;;;; user-agent support by Sen Nagata

;; this appears to be necessarily global...
(defvar wl-user-agent-compose-p nil)
(defvar wl-user-agent-headers-and-body-alist nil)

;; this should be a generic function for mail-mode -- i wish there was
;; something like it in sendmail.el
(defun wl-user-agent-insert-header (header-name header-value)
  "Insert HEADER-NAME w/ value HEADER-VALUE into a message."
  ;; it seems like overriding existing headers is acceptable -- should
  ;; we provide an option?

  ;; plan was: unfold header (might be folded), remove existing value, insert
  ;;           new value
  ;; wl doesn't seem to fold header lines yet anyway :-)

  (let ((kill-whole-line t)
	end-of-line)
    (mail-position-on-field (capitalize header-name))
    (setq end-of-line (point))
    (beginning-of-line)
    (re-search-forward ":" end-of-line)
    (insert (concat " " header-value "\n"))
    (kill-line)))

;; this should be a generic function for mail-mode -- i wish there was
;; something like it in sendmail.el
;;
;; ** haven't dealt w/ case where the body is already set **
(defun wl-user-agent-insert-body (body-text)
  "Insert a body of text, BODY-TEXT, into a message."
  ;; code defensively... :-P
  (goto-char (point-min))
  (search-forward mail-header-separator)
  (forward-line 1)
  (insert body-text))

;;;###autoload
(defun wl-user-agent-compose (&optional to subject other-headers continue
					switch-function yank-action
					send-actions)
  "Support the `compose-mail' interface for wl.
Only support for TO, SUBJECT, and OTHER-HEADERS has been implemented.
Support for CONTINUE, YANK-ACTION, and SEND-ACTIONS has not
been implemented yet.  Partial support for SWITCH-FUNCTION now supported."

  (unless (featurep 'wl)
    (require 'wl))
  ;; protect these -- to and subject get bound at some point, so it looks
  ;; to be necessary to protect the values used w/in
  (let ((wl-user-agent-headers-and-body-alist other-headers)
	(wl-draft-use-frame (eq switch-function 'switch-to-buffer-other-frame))
	(wl-draft-reply-buffer-style 'split))
    (when (eq switch-function 'switch-to-buffer-other-window)
      (when (one-window-p t)
	(if (window-minibuffer-p) (other-window 1))
	(split-window))
      (other-window 1))
    (if to
	(if (wl-string-match-assoc "to" wl-user-agent-headers-and-body-alist
				   'ignore-case)
	    (setcdr
	     (wl-string-match-assoc "to" wl-user-agent-headers-and-body-alist
				    'ignore-case)
	     to)
	  (setq wl-user-agent-headers-and-body-alist
		(cons (cons "to" to)
		      wl-user-agent-headers-and-body-alist))))
    (if subject
	(if (wl-string-match-assoc "subject"
				   wl-user-agent-headers-and-body-alist
				   'ignore-case)
	    (setcdr
	     (wl-string-match-assoc "subject"
				    wl-user-agent-headers-and-body-alist
				    'ignore-case)
	     subject)
	  (setq wl-user-agent-headers-and-body-alist
		(cons (cons "subject" subject)
		      wl-user-agent-headers-and-body-alist))))
    ;; i think this is what we want to use...
    (unwind-protect
	(progn
	  ;; tell the hook-function to do its stuff
	  (setq wl-user-agent-compose-p t)
	  ;; because to get the hooks working, wl-draft has to think it has
	  ;; been called interactively
	  (call-interactively 'wl-draft))
      (setq wl-user-agent-compose-p nil))))

(defun wl-user-agent-compose-internal ()
  "Manipulate headers and/or a body of a draft message."
  ;; being called from wl-user-agent-compose?
  (if wl-user-agent-compose-p
      (progn
	;; insert headers
	(let ((headers wl-user-agent-headers-and-body-alist)
	      (case-fold-search t))
	  (while headers
	    ;; skip body
	    (if (not (string-match "^body$" (car (car headers))))
		(wl-user-agent-insert-header
		 (car (car headers)) (cdr (car headers)))
	      t)
	    (setq headers (cdr headers))))
	;; highlight headers (from wl-draft in wl-draft.el)
	(wl-highlight-headers 'for-draft)
	;; insert body
	(if (wl-string-match-assoc "body" wl-user-agent-headers-and-body-alist
				   'ignore-case)
	    (wl-user-agent-insert-body
	     (cdr (wl-string-match-assoc
		   "body"
		   wl-user-agent-headers-and-body-alist 'ignore-case)))))
    t))

(require 'product)
(product-provide (provide 'wl-draft) (require 'wl-version))

;;; wl-draft.el ends here
