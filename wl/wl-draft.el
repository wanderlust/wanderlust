;;; wl-draft.el -- Message draft mode for Wanderlust.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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

(require 'sendmail)
(require 'wl-template)
(require 'emu)
(if (module-installed-p 'timezone)
    (require 'timezone))
(require 'std11)
(require 'wl-vars)

(eval-when-compile
  (require 'smtp)
  (require 'elmo-pop3)
  (mapcar
   (function
    (lambda (symbol)
      (unless (boundp symbol)
	(set (make-local-variable symbol) nil))))
   '(x-face-add-x-face-version-header 
     mail-reply-buffer
     mail-from-style
     smtp-authenticate-type
     smtp-authenticate-user
     smtp-authenticate-passphrase
     smtp-connection-type
     ))
  (defun-maybe x-face-insert (a))
  (defun-maybe x-face-insert-version-header ())
  (defun-maybe wl-init (&optional a))
  (defun-maybe wl-draft-mode ()))

(defvar wl-draft-buf-name "Draft")
(defvar wl-caesar-region-func nil)
(defvar wl-draft-cite-func 'wl-default-draft-cite)
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
(defvar wl-draft-queue-flush-send-func 'wl-draft-dispatch-message)
(defvar wl-sent-message-via nil)
(defvar wl-sent-message-modified nil)
(defvar wl-draft-fcc-list nil)
(defvar wl-draft-reedit nil)
(defvar wl-draft-reply-buffer nil)
(defvar wl-draft-forward nil)

(defvar wl-draft-config-sub-func-alist
  '((body        . wl-draft-config-sub-body)
    (top         . wl-draft-config-sub-top)
    (bottom      . wl-draft-config-sub-bottom)
    (header      . wl-draft-config-sub-header)
    (body-file   . wl-draft-config-sub-body-file)
    (top-file    . wl-draft-config-sub-top-file)
    (bottom-file . wl-draft-config-sub-bottom-file)
    (header-file . wl-draft-config-sub-header-file)
    (template    . wl-draft-config-sub-template)
    (x-face      . wl-draft-config-sub-x-face)))

(make-variable-buffer-local 'wl-draft-buffer-file-name)
(make-variable-buffer-local 'wl-draft-buffer-cur-summary-buffer)
(make-variable-buffer-local 'wl-draft-config-variables)
(make-variable-buffer-local 'wl-draft-config-exec-flag)
(make-variable-buffer-local 'wl-sent-message-via)
(make-variable-buffer-local 'wl-draft-fcc-list)
(make-variable-buffer-local 'wl-draft-reply-buffer)

;;; SMTP binding by Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
(defvar wl-smtp-features
  '(((smtp-authenticate-type
      (if wl-smtp-authenticate-type
	  (intern (downcase (format "%s" wl-smtp-authenticate-type)))))
     ((smtp-authenticate-user wl-smtp-posting-user)
      ((smtp-authenticate-passphrase
	(elmo-get-passwd
	 (format "%s@%s" 
		 smtp-authenticate-user
		 smtp-server))))))
    (smtp-connection-type))
  "Additional SMTP features.")

(eval-when-compile
  (defun wl-smtp-parse-extension (exts parents)
    (let (bindings binding feature)
      (dolist (ext exts)
	(setq feature (if (listp (car ext)) (caar ext) (car ext))
	      binding 
	      (` ((, feature)
		  (or (, (if (listp (car ext))
			     (cadar ext)
			   (let ((wl-feature
				  (intern
				   (concat "wl-" (symbol-name feature))))) 
			     (if (boundp wl-feature) 
				 wl-feature))))
		      (and (boundp '(, feature)) (, feature))))))
	(when parents 
	  (setcdr binding (list (append '(and) parents (cdr binding)))))
	(setq bindings 
	      (nconc bindings (list binding)
		     (wl-smtp-parse-extension 
		      (cdr ext) (cons feature parents)))))
      bindings)))

(defmacro wl-smtp-extension-bind (&rest body)
  "Return a `let' form that binds all variables of SMTP extension.
After this is done, BODY will be executed in the scope
of the `let' form.

The variables bound and their default values are described by
the `wl-smtp-features' variable."
  (` (let* (, (wl-smtp-parse-extension wl-smtp-features nil))
       (,@ body))))

(defun wl-draft-insert-date-field ()
  (insert "Date: " (wl-make-date-string) "\n"))

(defun wl-draft-insert-from-field ()
  ;; Put the "From:" field in unless for some odd reason
  ;; they put one in themselves.
  (let* ((login (or user-mail-address (user-login-name)))
	 (fullname (user-full-name)))
    (cond ((eq mail-from-style 'angles)
	   (insert "From: " fullname)
	   (let ((fullname-start (+ (point-min) 6))
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
	  ((null mail-from-style)
	   (insert "From: " login "\n")))))

(defun wl-draft-insert-x-face-field ()
  "Insert x-face header."
  (interactive)
  (if (not (file-exists-p wl-x-face-file))
      (error "File %s does not exist" wl-x-face-file)
    (beginning-of-buffer)
    (search-forward mail-header-separator nil t)
    (beginning-of-line)
    (wl-draft-insert-x-face-field-here)
    (run-hooks 'wl-draft-insert-x-face-field-hook) ; highlight it if you want.
    ))

(defun wl-draft-insert-x-face-field-here ()
  "insert x-face field at point."
  (let ((x-face-string (elmo-get-file-string wl-x-face-file)))
    (if (string-match "^[ \t]*" x-face-string)
	(setq x-face-string (substring x-face-string (match-end 0))))
    (insert "X-Face: " x-face-string))
  (if (not (= (preceding-char) ?\n))
      (insert ?\n))
  (and (fboundp 'x-face-insert-version-header) ; x-face.el...
       (boundp 'x-face-add-x-face-version-header)
       x-face-add-x-face-version-header
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
    (if wl-draft-always-delete-myself
	(elmo-list-delete myself cc)
      (if (elmo-list-member myself cc)
	  (if (elmo-list-member (append to cc) 
				(mapcar 'downcase wl-subscribed-mailing-list))
	      ;; member list is contained in recipients.
	      (elmo-list-delete myself cc)
	    cc
	    )
	cc))))

(defun wl-draft-forward (original-subject summary-buf)
  (wl-draft "" (concat "Forward: " original-subject) 
	    nil nil nil nil nil nil nil nil nil summary-buf)
  (goto-char (point-max))
  (wl-draft-insert-message)
  (mail-position-on-field "To"))

(defun wl-draft-reply (buf no-arg summary-buf)
;  (save-excursion 
  (let ((r-list (if no-arg wl-draft-reply-without-argument-list
		  wl-draft-reply-with-argument-list))
	(eword-lexical-analyzer '(eword-analyze-quoted-string
				  eword-analyze-domain-literal
				  eword-analyze-comment
				  eword-analyze-spaces
				  eword-analyze-special
				  eword-analyze-encoded-word
				  eword-analyze-atom))
	to mail-followup-to cc subject in-reply-to references newsgroups
	from addr-alist)
    (set-buffer buf)
    (if (wl-address-user-mail-address-p 
	 (setq from
	       (wl-address-header-extract-address (std11-field-body "From"))))
	(setq to (mapconcat 'identity (elmo-multiple-field-body "To") ",")
	      cc (mapconcat 'identity (elmo-multiple-field-body "Cc") ",")
	      newsgroups (or (std11-field-body "Newsgroups") ""))
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
	(error "No match field: check your `wl-draft-reply-without-argument-list'")))
    (setq subject (std11-field-body "Subject"))
    (setq to (wl-parse-addresses to)
	  cc (wl-parse-addresses cc))
    (with-temp-buffer ; to keep raw buffer unibyte.
      (elmo-set-buffer-multibyte default-enable-multibyte-characters)
      (setq subject (or (and subject
			     (eword-decode-string
			      (decode-mime-charset-string
			       subject
			       wl-mime-charset)))))
      (if wl-draft-reply-use-address-with-full-name
	  (setq addr-alist
		(mapcar
		 '(lambda (addr)
		    (setq addr (eword-extract-address-components addr))
		    (cons (nth 1 addr)
			  (if (nth 0 addr)
			      (concat (nth 0 addr) " <" (nth 1 addr) ">")
			    (nth 1 addr))))
		 (append to cc)))))
    (and subject wl-reply-subject-prefix
	 (let ((case-fold-search t))
	   (not
	    (equal
	     (string-match (regexp-quote wl-reply-subject-prefix)
			   subject)
	     0)))
	 (setq subject (concat wl-reply-subject-prefix subject)))
    (and (setq in-reply-to (std11-field-body "Message-Id"))
	 (setq in-reply-to
	       (format "In your message of \"%s\"\n\t%s"
		       (or (std11-field-body "Date") "some time ago")
		       in-reply-to)))
    (setq references (nconc
		      (std11-field-bodies '("References" "In-Reply-To"))
		      (list in-reply-to)))
    (setq to (mapcar '(lambda (addr)
			(wl-address-header-extract-address
			 addr)) to))
    (setq cc (mapcar '(lambda (addr)
			(wl-address-header-extract-address
			 addr)) cc))
    ;; if subscribed mailing list is contained in cc or to
    ;; and myself is contained in cc,
    ;; delete myself from cc.
    (setq cc (wl-draft-delete-myself-from-cc to cc))
    (if wl-insert-mail-followup-to
	(progn
	  (setq mail-followup-to 
		(wl-draft-make-mail-followup-to (append to cc)))
	  (setq mail-followup-to (wl-delete-duplicates mail-followup-to 
						       nil t))))
    (setq newsgroups (wl-parse newsgroups 
			       "[ \t\f\r\n,]*\\([^ \t\f\r\n,]+\\)")
	  newsgroups (wl-delete-duplicates newsgroups)
	  newsgroups (if newsgroups (mapconcat 'identity newsgroups ",")))
    (setq to (wl-delete-duplicates to nil t))
    (setq cc (wl-delete-duplicates 
	      (append (wl-delete-duplicates cc nil t)
		      to (copy-sequence to))
	      t t))
    (and to (setq to (mapconcat
		      '(lambda (addr)
			 (if wl-draft-reply-use-address-with-full-name
			     (or (cdr (assoc addr addr-alist)) addr)
			   addr))
		      to ",\n\t")))
    (and cc (setq cc (mapconcat
		      '(lambda (addr)
			 (if wl-draft-reply-use-address-with-full-name
			     (or (cdr (assoc addr addr-alist)) addr)
			   addr))
		      cc ",\n\t")))
    (and mail-followup-to
	 (setq mail-followup-to
	       (mapconcat
		'(lambda (addr)
		   (if wl-draft-reply-use-address-with-full-name
		       (or (cdr (assoc addr addr-alist)) addr)
		     addr))
		mail-followup-to ",\n\t")))
    (and (null to) (setq to cc cc nil))
    (setq references (delq nil references)
	  references (mapconcat 'identity references " ")
	  references (wl-parse references "[^<]*\\(<[^>]+>\\)")
	  references (wl-delete-duplicates references)
	  references (if references 
			 (mapconcat 'identity references "\n\t")))
    (wl-draft
     to subject in-reply-to cc references newsgroups mail-followup-to
     nil nil nil nil summary-buf)
    (setq wl-draft-reply-buffer buf))
  (run-hooks 'wl-reply-hook))

(defun wl-draft-add-references ()
  (let* ((mes-id (save-excursion
                   (set-buffer mail-reply-buffer)
                   (std11-field-body "message-id")))
         (ref (std11-field-body "References"))
         (ref-list nil) (st nil))
    (when (and mes-id ref)
      (while (string-match "<[^>]+>" ref st)
        (setq ref-list
              (cons (substring ref (match-beginning 0) (setq st (match-end 0)))
                    ref-list)))
      (if (and ref-list
               (member mes-id ref-list))
          (setq mes-id nil)))
    (when mes-id
      (save-excursion
        (when (mail-position-on-field "References")
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
     (save-excursion
       (set-buffer mail-reply-buffer)
       (if decode-it
	   (decode-mime-charset-region (point-min) (point-max)
				       wl-mime-charset))       
       (buffer-substring-no-properties 
	(point-min) (point-max))))
    (when ignored-fields
      (goto-char (point-min))
      (wl-draft-delete-fields ignored-fields))
    (goto-char (point-max))
    (push-mark)
    (goto-char (point-min)))
  (let ((beg (point)))
    (cond (mail-citation-hook (run-hooks 'mail-citation-hook))
	  (mail-yank-hooks (run-hooks 'mail-yank-hooks))
	  (t (and wl-draft-cite-func
		  (funcall wl-draft-cite-func)))) ; default cite
    (run-hooks 'wl-draft-cited-hook)
    (and wl-draft-add-references
	 (if (wl-draft-add-references)
	     (let (wl-highlight-x-face-func)
	       (wl-highlight-headers))))
    (if wl-highlight-body-too
	(wl-highlight-body-region beg (point-max)))))

(defun wl-draft-confirm ()
  (interactive)
  (y-or-n-p (format "Send current draft as %s? "
		    (if (wl-message-mail-p)
			(if (wl-message-news-p) "Mail and News" "Mail")
		      "News"))))

(defun wl-message-news-p ()
  (std11-field-body "Newsgroups"))

(defun wl-message-field-exists-p (field)
  (let ((value (std11-field-body field)))
    (and value
	 (not (string= value "")))))

(defun wl-message-mail-p ()
  (or (wl-message-field-exists-p "To")
      (wl-message-field-exists-p "Cc")
      (wl-message-field-exists-p "Bcc")
      ;;(wl-message-field-exists-p "Fcc")		; This may be needed..
      ))

(defun wl-draft-open-file (&optional file)
  (interactive)				; "*fFile to edit: ")
  (wl-draft-edit-string (elmo-get-file-string 
			 (or file
			     (read-file-name "File to edit: " 
					     (or wl-tmp-dir "~/"))))))

(defun wl-draft-edit-string (string)
  (let ((cur-buf (current-buffer))
	(tmp-buf (get-buffer-create " *wl-draft-edit-string*"))
	to subject in-reply-to cc references newsgroups mail-followup-to
	content-type content-transfer-encoding
	body-beg buffer-read-only
	)
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
		   'edit-again
		   ))
      (and to (mail-position-on-field "To"))
      (delete-other-windows)
      (kill-buffer tmp-buf)))
  (setq buffer-read-only nil) ;;??
  (run-hooks 'wl-draft-reedit-hook))

(defun wl-draft-insert-current-message (dummy)
  (interactive)
  (let ((mail-reply-buffer (wl-message-get-original-buffer))
	mail-citation-hook mail-yank-hooks
	wl-draft-add-references wl-draft-cite-func)
    (if (eq 0
	    (save-excursion
	      (set-buffer mail-reply-buffer)
	      (buffer-size)))
	(error "No current message")
      (wl-draft-yank-from-mail-reply-buffer nil
					    wl-ignored-forwarded-headers))))

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
	wl-draft-cite-func)
    (unwind-protect
	(progn
	  (save-excursion
	    (elmo-read-msg-with-cache fld number mail-reply-buffer nil))
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
	  (save-excursion 
	    (set-buffer summary-buf)
	    (setq num 
		  (save-excursion
		    (set-buffer message-buf)
		    wl-message-buffer-cur-number))
	    (setq entity (assoc (cdr (assq num 
					   (elmo-msgdb-get-number-alist 
					    wl-summary-buffer-msgdb)))
				(elmo-msgdb-get-overview 
				 wl-summary-buffer-msgdb)))
	    (setq from (elmo-msgdb-overview-entity-get-from entity))
	    (setq date (elmo-msgdb-overview-entity-get-date entity)))
	  (setq cite-title (format "At %s,\n%s wrote:" 
				   (or date "some time ago")
				   (wl-summary-from-func-internal
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
  (interactive)
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
			     (elmo-match-string 0 wl-draft-buffer-file-name)))))
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
		   (y-or-n-p "Kill Current Draft?")))
      (let ((cur-buf (current-buffer)))
	(wl-draft-hide cur-buf)
	(wl-draft-delete cur-buf)))
    (message "")))

(defun wl-draft-fcc ()
  "Add a new FCC field, with file name completion."
  (interactive)
  (or (mail-position-on-field "fcc" t)  ;Put new field after exiting FCC.
      (mail-position-on-field "to"))
  (insert "\nFCC: "))

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
    (save-excursion
      (let* ((tmp-buf (get-buffer-create " *wl-draft-sendlog*"))
	     (filename (expand-file-name wl-draft-sendlog-filename
					 elmo-msgdb-dir))
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
	(set-buffer tmp-buf)
	(erase-buffer)
	(insert (format "%s proto=%s stat=%s%s%s%s\n"
			time proto status server to id))
	(if (and wl-draft-sendlog-max-size filesize
		 (> filesize wl-draft-sendlog-max-size))
	    (rename-file filename (concat filename ".old") t))
	(if (file-writable-p filename)
	    (write-region (point-min) (point-max) 
			  filename t 'no-msg)
	  (message (format "%s is not writable." filename)))
	(kill-buffer tmp-buf)))))

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
	(1   (error "qmail-inject reported permanent failure"))
	(111 (error "qmail-inject reported transient failure"))
	;; should never happen
	(t   (error "qmail-inject reported unknown failure"))))))

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
	(setq addresses (std11-parse-addresses seq))
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
	  (insert " " (wl-address-string-without-group-list-contents seq))))
      mailbox-list)))

(defun wl-draft-deduce-address-list (buffer header-start header-end)
  "Get address list suitable for smtp RCPT TO:<address>.
Group list content is removed if `wl-draft-remove-group-list-contents' is
non-nil."
  (let ((fields        '("to" "cc" "bcc"))
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
	    ;;(run-hooks 'wl-mail-send-pre-hook)
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
		  (let ((err (smtp-via-smtp sender recipients 
					    (current-buffer))))
		    (when (not (eq err t))
		      (wl-draft-write-sendlog 'failed 'smtp smtp-server
					      recipients id)
		      (error "Sending failed; SMTP protocol error:%s" err))))
		 (wl-draft-set-sent-message 'mail 'sent)
		 (wl-draft-write-sendlog 
		  'ok 'smtp smtp-server recipients id)))))
	(if (bufferp errbuf)
	    (kill-buffer errbuf))))))

(defun wl-draft-send-mail-with-pop-before-smtp ()
  "Send the prepared message buffer with POP-before-SMTP."
  (require 'elmo-pop3)
  (condition-case ()
      (elmo-pop3-get-connection 
       (list 'pop3
	     (or wl-pop-before-smtp-user
		 elmo-default-pop3-user)
	     (or wl-pop-before-smtp-authenticate-type
		 elmo-default-pop3-authenticate-type)
	     (or wl-pop-before-smtp-server
		 elmo-default-pop3-server)
	     (or wl-pop-before-smtp-port
		 elmo-default-pop3-port)
	     (or wl-pop-before-smtp-ssl
		 elmo-default-pop3-ssl)))
    (error))
  (wl-draft-send-mail-with-smtp))

(defun wl-draft-insert-required-fields (&optional force-msgid)
  ;; Insert Message-Id field...
  (goto-char (point-min))
  (when (and (or force-msgid
		 wl-insert-message-id)
	     (not (re-search-forward "^Message-ID[ \t]*:" nil t)))
    (insert (concat "Message-ID: "
		    (wl-draft-make-message-id-string)
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
  "Send the message in the current buffer. "
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
  (run-hooks 'wl-mail-send-pre-hook) ;; X-PGP-Sig, Cancel-Lock
  (wl-draft-dispatch-message)
  (when kill-when-done
    ;; hide editing-buffer.
    (wl-draft-hide editing-buffer)
    ;; delete editing-buffer and its file.
    (wl-draft-delete editing-buffer)))

(defun wl-draft-dispatch-message (&optional mes-string)
  "Send the message in the current buffer. Not modified the header fields."
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
	      (funcall wl-draft-send-mail-func))
	  (if (and (wl-message-news-p)
		   (not (wl-draft-sent-message-p 'news))
		   (not (wl-message-field-exists-p "Resent-to")))
	      (funcall wl-draft-send-news-func)))
      ;;
      (let* ((status (wl-draft-sent-message-results))
	     (unplugged-via (car status))
	     (sent-via (nth 1 status)))
	;; If one sent, process fcc folder.
	(when (and sent-via wl-draft-fcc-list)
	  (wl-draft-do-fcc (wl-draft-get-header-delimiter) wl-draft-fcc-list)
	  (setq wl-draft-fcc-list nil))
	;; If one unplugged, append queue.
	(when (and unplugged-via
		   wl-sent-message-modified)
	  (if wl-draft-enable-queuing
	      (wl-draft-queue-append wl-sent-message-via)
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
				 (if sent-via "done." "failed.")))))))))
  (not wl-sent-message-modified)) ;; return value

(defun wl-draft-raw-send (&optional kill-when-done force-pre-hook mes-string)
  "Force send current buffer as raw message."
  (interactive)
  (save-excursion
    (let (wl-interactive-send
;	  wl-draft-verbose-send
	  (wl-mail-send-pre-hook (and force-pre-hook wl-mail-send-pre-hook))
;	  wl-news-send-pre-hook
	  mail-send-hook
	  mail-send-actions)
      (wl-draft-send kill-when-done mes-string))))

(defun wl-draft-clone-local-variables ()
  (let ((locals (buffer-local-variables))
	result)
    (mapcar
     (function
      (lambda (local)
	(when (and (consp local)
		   (car local)
		   (string-match 
		    wl-draft-clone-local-variable-regexp
		    (symbol-name (car local))))
	  (wl-append result (list (car local))))))
     locals)
    result))

(defun wl-draft-send (&optional kill-when-done mes-string)
  "Send current draft message. 
If optional argument is non-nil, current draft buffer is killed"
  (interactive)
  (wl-draft-config-exec)
  (run-hooks 'wl-draft-send-hook)
  (when (or (not wl-interactive-send)
	    (y-or-n-p "Send current draft. OK?"))
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
	    (funcall wl-draft-send-func editing-buffer kill-when-done)
	    ;; Now perform actions on successful sending.
	    (while mail-send-actions
	      (condition-case ()
		  (apply (car (car mail-send-actions))
			 (cdr (car mail-send-actions)))
		(error))
	      (setq mail-send-actions (cdr mail-send-actions)))
	    (if (or (eq major-mode 'wl-draft-mode)
		    (eq major-mode 'mail-mode))
		(local-set-key "\C-c\C-s" 'wl-draft-send)) ; override
	    (if wl-draft-verbose-send
		(message (concat (or wl-draft-verbose-msg
				     mes-string "Sending...")
				 "done."))))
	;; kill sending buffer, anyway.
	(and (buffer-live-p sending-buffer)
	     (kill-buffer sending-buffer))))))

(defun wl-draft-save ()
  "Save current draft."
  (interactive)
  (save-buffer)
  (wl-draft-config-info-operation
   (and (string-match "[0-9]+$" wl-draft-buffer-file-name)
	(string-to-int
	 (elmo-match-string 0 wl-draft-buffer-file-name)))
   'save))

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

(defun wl-draft-delete-field (field &optional delimline)
  (wl-draft-delete-fields (regexp-quote field) delimline))

(defun wl-draft-delete-fields (regexp &optional delimline)
  (save-restriction
    (unless delimline
      (if (search-forward "\n\n" nil t)
	  (setq delimline (point))
	(setq delimline (point-max))))
    (narrow-to-region (point-min) delimline)
    (goto-char (point-min))
    (let ((regexp (concat "^" regexp ":"))
	  (case-fold-search t)
	  last)
      (while (not (eobp))
	(if (looking-at regexp)
	    (progn
	      (delete-region
	       (point)
	       (progn
		 (forward-line 1)
		 (if (re-search-forward "^[^ \t]" nil t)
		     (goto-char (match-beginning 0))
		   (point-max)))))
	  (forward-line 1)
	  (if (re-search-forward "^[^ \t]" nil t)
	      (goto-char (match-beginning 0))
	    (point-max)))))))

(defun wl-draft-get-fcc-list (header-end)
  (let (fcc-list
	(case-fold-search t))
    (or (markerp header-end) (error "header-end must be a marker"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^FCC:[ \t]*" header-end t)
	(setq fcc-list
	      (cons (buffer-substring-no-properties
		     (point)
		     (progn
		       (end-of-line)
		       (skip-chars-backward " \t")
		       (point)))
		    fcc-list))
	(save-match-data
	  (wl-folder-confirm-existence (eword-decode-string (car fcc-list))))
	(delete-region (match-beginning 0)
		       (progn (forward-line 1) (point)))))
    fcc-list))

(defun wl-draft-do-fcc (header-end &optional fcc-list)
  (let ((send-mail-buffer (current-buffer))
	(tembuf (generate-new-buffer " fcc output"))
	(case-fold-search t)
	beg end)
    (or (markerp header-end) (error "header-end must be a marker"))
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
		      (elmo-folder-plugged-p (car fcc-list)))
	    (elmo-cache-save id nil nil nil) ;; for disconnected operation
	    (setq cache-saved t))
	  (if (elmo-append-msg (eword-decode-string (car fcc-list))
			       (buffer-substring 
				(point-min) (point-max))
			       id)
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

;;;###autoload
(defun wl-draft (&optional to subject in-reply-to cc references newsgroups
			   mail-followup-to
			   content-type content-transfer-encoding
			   body edit-again summary-buf)
  "Write and send mail/news message with Wanderlust."
  (interactive)
  (unless (featurep 'wl)
    (require 'wl))
  (unless wl-init
    (wl-load-profile))
  (wl-init) ;; returns immediately if already initialized.
  (if (interactive-p)
      (setq summary-buf (wl-summary-get-buffer wl-summary-buffer-folder-name)))
  (let ((draft-folder-spec (elmo-folder-get-spec wl-draft-folder))
	buf-name file-name num wl-demo change-major-mode-hook)
    (if (not (eq (car draft-folder-spec) 'localdir))
	(error "%s folder cannot be used for draft folder" wl-draft-folder))
    (setq num (elmo-max-of-list (or (elmo-list-folder wl-draft-folder) '(0))))
    (setq num (+ 1 num))
    ;; To get unused buffer name.
    (while (get-buffer (concat wl-draft-folder "/" (int-to-string num)))
      (setq num (+ 1 num)))
    (setq buf-name (find-file-noselect 
		    (setq file-name
			  (elmo-get-msg-filename wl-draft-folder
						 num))))
    (if wl-draft-use-frame
	(switch-to-buffer-other-frame buf-name)
      (switch-to-buffer buf-name))
    (set-buffer buf-name)
    (if (not (string-match (regexp-quote wl-draft-folder)
			   (buffer-name)))
	(rename-buffer (concat wl-draft-folder "/" (int-to-string num))))
    (if (or (eq wl-draft-reply-buffer-style 'full)
	    (eq this-command 'wl-draft)
	    (eq this-command 'wl-summary-write)
	    (eq this-command 'wl-summary-write-current-newsgroup))
	(delete-other-windows))
    (auto-save-mode -1)
    (wl-draft-mode)
    (setq wl-sent-message-via nil)
    (if (stringp wl-from)
	(insert "From: " wl-from "\n"))
    (and (or (interactive-p)
	     (eq this-command 'wl-summary-write)
	     to)
	 (insert "To: " (or to "") "\n"))
    (and cc (insert "Cc: " (or cc "") "\n"))
    (insert "Subject: " (or subject "") "\n")
    (and newsgroups (insert "Newsgroups: " newsgroups "\n"))
    (and mail-followup-to (insert "Mail-Followup-To: " mail-followup-to "\n"))
    (and wl-insert-mail-reply-to 
	 (insert "Mail-Reply-To: " 
		 (wl-address-header-extract-address
		  wl-from) "\n"))
    (and in-reply-to (insert "In-Reply-To: " in-reply-to "\n"))
    (and references (insert "References: " references "\n"))
    (insert (funcall wl-generate-mailer-string-func) 
	    "\n")
    (setq wl-draft-buffer-file-name file-name)
    (if mail-default-reply-to
	(insert "Reply-To: " mail-default-reply-to "\n"))
    (if (or wl-bcc mail-self-blind)
	(insert "Bcc: " (or wl-bcc (user-login-name)) "\n"))
    (if wl-fcc
	(insert "FCC: " wl-fcc "\n"))
    (if wl-organization
	(insert "Organization: " wl-organization "\n"))
    (and wl-auto-insert-x-face
	 (file-exists-p wl-x-face-file)
	 (wl-draft-insert-x-face-field-here))
    (if mail-default-headers
	(insert mail-default-headers))
    (if (not (= (preceding-char) ?\n))
	(insert ?\n))
    (if edit-again
	(let (start)
	  (setq start (point))
	  (when content-type 
	    (insert "Content-type: " content-type "\n"))
	  (when content-transfer-encoding
	    (insert "Content-Transfer-encoding: " content-transfer-encoding "\n"))
	  (if (or content-type content-transfer-encoding)
	      (insert "\n"))
	  (and body (insert body))
	  (save-restriction
	    (narrow-to-region start (point))
	    (and edit-again
		 (wl-draft-decode-message-in-buffer))
	    (widen)
	    (goto-char start)
	    (put-text-property (point)
			       (progn
				 (insert mail-header-separator "\n")
				 (1- (point)))
			       'category 'mail-header-separator)))
      (put-text-property (point)
			 (progn
			   (insert mail-header-separator "\n")
			   (1- (point)))
			 'category 'mail-header-separator)
      (and body (insert body)))
    (if wl-on-nemacs
	(push-mark (point) t)
      (push-mark (point) t t))
    (as-binary-output-file
     (write-region (point-min)(point-max) wl-draft-buffer-file-name
		   nil t))
    (wl-draft-editor-mode)
    (wl-draft-overload-functions)
    (let (wl-highlight-x-face-func)
      (wl-highlight-headers))
    (goto-char (point-min))
    (if (interactive-p)
	(run-hooks 'wl-mail-setup-hook))
    (wl-user-agent-compose-internal) ;; user-agent
    (cond ((eq this-command 'wl-summary-write-current-newsgroup)
 	   (mail-position-on-field "Subject"))
 	  ((and (interactive-p) (null to))
 	   (mail-position-on-field "To"))
 	  (t
 	   (goto-char (point-max))))    
    (setq wl-draft-config-exec-flag t)
    (setq wl-draft-buffer-cur-summary-buffer (or summary-buf
						 (get-buffer
						  wl-summary-buffer-name)))    
    buf-name))

(defun wl-draft-elmo-nntp-send ()
  (let ((elmo-nntp-post-pre-hook wl-news-send-pre-hook)
	(elmo-default-nntp-user
	 (or wl-nntp-posting-user elmo-default-nntp-user))
	(elmo-default-nntp-server
	 (or wl-nntp-posting-server elmo-default-nntp-server))
	(elmo-default-nntp-port
	 (or wl-nntp-posting-port elmo-default-nntp-port))
	(elmo-default-nntp-ssl
	 (or wl-nntp-posting-ssl elmo-default-nntp-ssl)))
    (if (not (elmo-plugged-p elmo-default-nntp-server elmo-default-nntp-port))
 	(wl-draft-set-sent-message 'news 'unplugged
 				   (cons elmo-default-nntp-server
 					 elmo-default-nntp-port))
      (elmo-nntp-post elmo-default-nntp-server (current-buffer))
      (wl-draft-set-sent-message 'news 'sent)
      (wl-draft-write-sendlog 'ok 'nntp elmo-default-nntp-server
 			      (std11-field-body "Newsgroups")
 			      (std11-field-body "Message-ID")))))

(defun wl-draft-generate-clone-buffer (name &optional local-variables)
  "generate clone of current buffer named NAME."
  (let ((editing-buffer (current-buffer)))
    (save-excursion
      (set-buffer (generate-new-buffer name))
      (erase-buffer)
      (wl-draft-mode)
      (wl-draft-editor-mode)
      (insert-buffer editing-buffer)
      (message "")
      (when local-variables
	(mapcar
	 (function
	  (lambda (var)
	    (make-local-variable var)
	    (set var (save-excursion
		       (set-buffer editing-buffer)
		       (symbol-value var)))))
	 local-variables))
      (current-buffer))))

(defun wl-draft-reedit (number)
  (let ((draft-folder-spec (elmo-folder-get-spec wl-draft-folder))
	(wl-draft-reedit t)
	buf-name file-name change-major-mode-hook)
    (setq file-name (expand-file-name
		     (int-to-string number)
		     (expand-file-name
		      (nth 1 draft-folder-spec)
		      elmo-localdir-folder-path)))
    (unless (file-exists-p file-name)
      (error "File %s does not exist" file-name))
    (setq buf-name (find-file-noselect file-name))
    (if wl-draft-use-frame
	(switch-to-buffer-other-frame buf-name)
      (switch-to-buffer buf-name))
    (set-buffer buf-name)
    (if (not (string-match (regexp-quote wl-draft-folder)
			   (buffer-name)))
	(rename-buffer (concat wl-draft-folder "/" (buffer-name))))    
    (auto-save-mode -1)
    (wl-draft-mode)
    (setq wl-sent-message-via nil)
    (setq wl-draft-buffer-file-name file-name)
    (wl-draft-config-info-operation number 'load)
    (goto-char (point-min))
    (or (re-search-forward "\n\n" nil t)
	(search-forward (concat mail-header-separator "\n") nil t))
    (if wl-on-nemacs
	(push-mark (point) t)
      (push-mark (point) t t))
    (write-region (point-min)(point-max) wl-draft-buffer-file-name
		  nil t)
    (wl-draft-overload-functions)
    (wl-draft-editor-mode)
    (let (wl-highlight-x-face-func)
      (wl-highlight-headers))
    (run-hooks 'wl-draft-reedit-hook)
    (goto-char (point-max))
    buf-name
    ))

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
       ((or (functionp config)
	    (and (symbolp config)
		 (fboundp config)))
	(funcall config))
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
  (let* ((msgdb-dir (elmo-msgdb-expand-path wl-draft-folder))
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
  (let* ((msgdb-dir (elmo-msgdb-expand-path wl-queue-folder))
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
	    (push 'wl-sent-message-via variables))
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
	(message-id (std11-field-body "Message-ID")))
    (if (elmo-append-msg wl-queue-folder
			 (buffer-substring (point-min) (point-max))
			 message-id)
	(progn
	  (if message-id
	      (elmo-dop-lock-message message-id))
	  (wl-draft-queue-info-operation
	   (car (elmo-max-of-folder wl-queue-folder))
	   'save wl-sent-message-via)
	  (wl-draft-write-sendlog 'ok 'queue nil wl-queue-folder message-id)
	  (when wl-draft-verbose-send
	    (setq wl-draft-verbose-msg "Queuing...")
	    (message "Queuing...done.")))
      (wl-draft-write-sendlog 'failed 'queue nil wl-queue-folder message-id)
      (error "Queuing failed"))))

(defun wl-draft-queue-flush ()
  "Flush draft queue."
  (interactive)
  (let ((msgs2 (elmo-list-folder wl-queue-folder))
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
		     (elmo-plugged-p
		      (car (nth 2 (car sent-via)))
		      (cdr (nth 2 (car sent-via)))))
	    (wl-append msgs (list (car msgs2)))
	    (throw 'found t))
	  (setq sent-via (cdr sent-via))))
      (setq msgs2 (cdr msgs2)))
    (when (> (setq len (length msgs)) 0)
      (if (elmo-y-or-n-p (format
			  "%d message(s) are in the sending queue. Send now?"
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
		(elmo-read-msg-no-cache wl-queue-folder (car msgs) 
					(current-buffer))
		(condition-case err
		    (setq failure (funcall
				   wl-draft-queue-flush-send-func
				   (format "Sending (%d/%d)..." i len)))
;; 		    (wl-draft-raw-send nil nil
;; 				       (format "Sending (%d/%d)..." i len))
		  (error
		   (elmo-display-error err t)
		   (setq failure t))
		  (quit 
		   (setq failure t)))
		(unless failure
		  (elmo-delete-msgs wl-queue-folder (cons (car msgs) nil))
		  (wl-draft-queue-info-operation (car msgs) 'delete)
		  (elmo-dop-unlock-message (std11-field-body "Message-ID"))
		  (setq performed (+ 1 performed)))
		(setq msgs (cdr msgs)))
	      (kill-buffer buffer)
	      (message "%d message(s) are sent." performed)))
	(message "%d message(s) are remained to be sent." len))
      len)))

(defun wl-jump-to-draft-buffer (&optional arg)
  "Jump to the draft if exists."
  (interactive "P")
  (if arg
      (wl-jump-to-draft-folder)
    (let ((bufs (buffer-list))
	  (draft-regexp (concat
			 "^" (regexp-quote
			      (expand-file-name
			       (nth 1 (elmo-folder-get-spec wl-draft-folder))
			       (expand-file-name
				elmo-localdir-folder-path)))))
	  buf draft-bufs)
      (while bufs
	(if (and
	     (setq buf (buffer-file-name (car bufs)))
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
  (let ((msgs (reverse (elmo-list-folder wl-draft-folder)))
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
	(let ((case-fold-search t))
	  (mapcar
	   (lambda (x)
	     (let ((header-name (car x))
		   (header-value (cdr x)))
	       ;; skip body
	       (if (not (string-match "^body$" header-name))
		   (wl-user-agent-insert-header header-name header-value)
		 t)))
	   wl-user-agent-headers-and-body-alist))
	;; highlight headers (from wl-draft in wl-draft.el)
	(let (wl-highlight-x-face-func)
	  (wl-highlight-headers))
	;; insert body
	(if (wl-string-match-assoc "body" wl-user-agent-headers-and-body-alist
				   'ignore-case)
	    (wl-user-agent-insert-body
	     (cdr (wl-string-match-assoc
		   "body"
		   wl-user-agent-headers-and-body-alist 'ignore-case)))))
    t))

(provide 'wl-draft)

;;; wl-draft.el ends here
