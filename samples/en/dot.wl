;;; dot.wl -- sample setting file for Wanderlust	-*- emacs-lisp -*-

;; [[ Requirement Setting ]]

;; Following must be included in ~/.emacs
;; for .emacs begin
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
;; for .emacs end

;; Icon directory (XEmacs and Emacs21 only)
;; (No need if installed as XEmacs package.)
;(setq wl-icon-dir "/usr/local/lib/emacs/etc")


;; [[ SEMI Setting ]]

;; Disable inline display of HTML part.
;; Put before load `mime-setup'
(setq mime-setup-enable-inline-html nil)

;; Don't split large message.
(setq mime-edit-split-message nil)

;; If lines of message are larger than this value, treat it as `large'.
;(setq mime-edit-message-default-max-lines 1000)


;;; [[ Private Setting ]]

;; Header From:
;(setq wl-from "Your Name <e-mail@example.com>")

;; User's mail addresses.
(setq wl-user-mail-address-list
      (list (wl-address-header-extract-address wl-from)
	    ;; "e-mail2@example.com"
	    ;; "e-mail3@example.net" ...
	    ))

;; Subscribed mailing list.
(setq wl-subscribed-mailing-list
      '("wl@lists.airs.net"
	"apel-ja@m17n.org"
	"emacs-mime-ja@m17n.org"
	;; "ml@example.com" ...
	))

;; If (system-name) does not return FQDN,
;; set following as a local domain name without hostname.
;; ((system-name) "." wl-local-domain is used as domain part of Message-ID
;; and an argument of HELO in SMTP.
;(setq wl-local-domain "example.com")

;; Specific domain part for message-id.
;(setq wl-message-id-domain "hostname.example.com")


;;; [[ Server Setting ]]

;; Default IMAP4 server
(setq elmo-imap4-default-server "localhost")
;; Default POP server
(setq elmo-pop3-default-server "localhost")
;; SMTP server
(setq wl-smtp-posting-server "localhost")
;; Default NNTP server
(setq elmo-nntp-default-server "localhost")
;; NNTP server name for posting
(setq wl-nntp-posting-server elmo-nntp-default-server)

;; IMAP authenticate type setting
(setq elmo-imap4-default-authenticate-type 'clear) ; raw
;(setq elmo-imap4-default-authenticate-type 'cram-md5) ; CRAM-MD5

;; POP-before-SMTP
;(setq wl-draft-send-mail-function 'wl-draft-send-mail-with-pop-before-smtp)


;;; [[ Basic Setting ]]

;; Default folder for `wl-summary-goto-folder'.
;(setq wl-default-folder "+inbox")

;; Default string for folder name completion.
;(setq wl-default-spec "+")

;; Folder Carbon Copy
;(setq wl-fcc "+outbox")

;; Confirm before exitting Wanderlust.
(setq wl-interactive-exit t)

;; Confirm before sending message.
(setq wl-interactive-send t)

;; Create opened thread.
;(setq wl-thread-insert-opened t)

;; Keep folder window beside summary. (3 pane)
;(setq wl-stay-folder-window t)

;; Truncate long lines.
;(setq wl-message-truncate-lines t)
;(setq wl-draft-truncate-lines t)
;; Following line is needed for XEmacs.
;(setq truncate-partial-width-windows nil)

;; Open new frame for draft buffer.
;(setq wl-draft-use-frame t)

;; Divide thread by change of subject.
;(setq wl-summary-divide-thread-when-subject-changed t)

;; Thread view
;(setq wl-thread-indent-level 2)
;(setq wl-thread-have-younger-brother-str "+"
;      wl-thread-youngest-child-str	 "+"
;      wl-thread-vertical-str		 "|"
;      wl-thread-horizontal-str		 "-"
;      wl-thread-space-str		 " ")

;; display first message automatically.
;(setq wl-auto-select-first t)

;; goto next folder when exit from summary.
;(setq wl-auto-select-next t)

;; skip folder if there is no unread message.
;(setq wl-auto-select-next 'skip-no-unread)

;; jump to unread message in 'N' or 'P'.
;(setq wl-summary-move-order 'unread)

;; notify mail arrival
;(setq wl-biff-check-folder-list '("%inbox"))
;(setq wl-biff-notify-hook '(ding))


;;; [[ Network ]]

;; cache setting.
;; (messages in localdir, localnews, maildir are not cached.)
;(setq elmo-archive-use-cache nil)
;(setq elmo-nntp-use-cache t)
;(setq elmo-imap4-use-cache t)
;(setq elmo-pop3-use-cache t)

;; Enable disconnected operation in IMAP folder.
;(setq elmo-enable-disconnected-operation t)

;; Store draft message in queue folder if message is sent in unplugged status.
(setq wl-draft-enable-queuing t)
;; when plug status is changed from unplugged to plugged,
;; queued message is flushed automatically.
(setq wl-auto-flush-queue t)

;; offline at startup.
;(setq wl-plugged nil)
;; change plug status by server or port at startup.
;(add-hook 'wl-make-plugged-hook
;	  '(lambda ()
;	     ;; Add or Change plug status for SERVER and PORT.
;	     (elmo-set-plugged plugged(t/nil) server port)
;	     ;; When omit port, SEVERS all port was changes.
;	     ;; (Can't add plug status without PORT)
;	     (elmo-set-plugged plugged(t/nil) server)
;	     ))


;;; [[ Special Setting ]]

;; open unread group folder after checking.
;(add-hook 'wl-folder-check-entity-hook
;	  '(lambda ()
;	     (wl-folder-open-unread-folder entity)
;	     ))

;; Change summary display function.

;; get extra field values as overview information (only localdir folder).
(setq elmo-msgdb-extra-fields
      '("newsgroups"
	"list-id" "x-ml-name" "mailing-list"
	"x-mail-count" "x-ml-count" "x-sequence"))

;; ML message displays ML name and ML sequence number in subject.
(setq wl-summary-subject-function 'my-wl-summary-subject-func-ml)
(defun my-wl-summary-subject-func-ml (subject-string)
  (let ((folder wl-summary-buffer-folder-name)
	(subj subject-string) (sequence) (ml-name) (ml-count))
    (setq sequence (elmo-msgdb-overview-entity-get-extra-field
		    entity "x-sequence")
	  ml-name (or (elmo-msgdb-overview-entity-get-extra-field
		       entity "x-ml-name")
		      (and sequence
			   (car (split-string sequence " "))))
	  ml-count (or (elmo-msgdb-overview-entity-get-extra-field
			entity "x-mail-count")
		       (elmo-msgdb-overview-entity-get-extra-field
			entity "x-ml-count")
		       (and sequence
			    (cadr (split-string sequence " ")))))
    (if (string-match
	 "^\\s(\\(\\S)+\\)[ :]\\([0-9]+\\)\\s)[ \t]*"
	 subject-string)
	(progn
	  (setq subj (substring subject-string (match-end 0)))
	  (if (not ml-name) (setq ml-name (match-string 1 subject-string)))
	  (if (not ml-count) (setq ml-count (match-string 2 subject-string)))))
    (if (and ml-name ml-count)
	(if (string= folder wl-default-folder)
	    (format "(%s %05d) %s"
		    (car (split-string ml-name " "))
		    (string-to-int ml-count)
		    subj)
	  (format "#%05d %s"
		  (string-to-int ml-count) subj))
      subj)))

;; imput asynchronously.
;; (utils/im-wl.el is needed to be installed.
;;  Don't forget setting ~/.im/Config (Smtpservers).
;;  note that wl-draft-enable-queuing is not valid.)
;(autoload 'wl-draft-send-with-imput-async "im-wl")
;(setq wl-draft-send-function 'wl-draft-send-with-imput-async)


;; non-verbose User-Agent: field
;(setq wl-generate-mailer-string-function
;      (function
;       (lambda ()
;	 (concat "User-Agent: "
;		 (wl-generate-user-agent-string-1 nil)))))


;;; [[ Template ]]

;; template
(setq wl-template-alist
      '(("default"
	 ("From" . wl-from)
	 ("Organization" . "~/.wl sample")
	 (body . "Hello, this is XXX \n"))		;; body
	("report"
	 ("To" . "boss@example.com")
	 ("Subject" . "Report")
	 (top . "Sir, here is my report\n")		;; insert in top.
;;	 (bottom-file . "~/work/report.txt")	;; insert file in bottom
	 )
	))
;; Change headers in draft sending time.
(setq wl-draft-config-alist
      '((reply		;; see reply buffer
	 "^To: .*\\(test-notsend-wl@lists.airs.net\\)"
	 (template . "default"))		;; template
	("^To: .*\\(test-notsend-wl@lists.airs.net\\)"
	 wl-ml-draft-config-function		;; function
	 ("From" . wl-from)			;; variable
	 ("Organization" . "~/.wl sample"))	;; string
	("^Newsgroups: test.*"
	 ("Organization" . "organization for nntp."))
	))

;; Change headers in draft preparation time.
;(add-hook 'wl-mail-setup-hook
;	  '(lambda ()
;	     (unless wl-draft-reedit;	; don't apply when reedit.
;	       (wl-draft-config-exec wl-draft-config-alist))))


;; [[ Reply ]]
;; header value setting for mail reply.

;; Wide window for draft buffer.
;(setq wl-draft-reply-buffer-style 'full)

;; Remove fullname in reply message header.
;(setq wl-draft-reply-use-address-with-full-name nil)

;; "a" (without-argument) reply to author (Reply-To or From).
;; if 'X-ML-Name' and 'Reply-To' exists, reply to 'Reply-To'.
(setq wl-draft-reply-without-argument-list
      '((("X-ML-Name" "Reply-To") . (("Reply-To") nil nil))
	("X-ML-Name" . (("To" "Cc") nil nil))
	("Followup-To" . (nil nil ("Followup-To")))
	("Newsgroups" . (nil nil ("Newsgroups")))
	("Reply-To" . (("Reply-To") nil nil))
	("Mail-Reply-To" . (("Mail-Reply-To") nil nil))
	("From" . (("From") nil nil))))

;; "C-u a" (with-argument) reply to all.
(setq wl-draft-reply-with-argument-list
      '(("Followup-To" . (("From") nil ("Followup-To")))
	("Newsgroups" . (("From") nil ("Newsgroups")))
	("Mail-Followup-To" . (("Mail-Followup-To") nil ("Newsgroups")))
	("From" . (("From") ("To" "Cc") ("Newsgroups")))))


;;; [[ Message Display Settings ]]

;; Hidden header field in message buffer.
(setq wl-message-ignored-field-list
      '(".*Received:" ".*Path:" ".*Id:" "^References:"
	"^Replied:" "^Errors-To:"
	"^Lines:" "^Sender:" ".*Host:" "^Xref:"
	"^Content-Type:" "^Precedence:"
	"^Status:" "^X-VM-.*:"))

;; Displayed header field in message buffer.
;; This value precedes `wl-message-ignored-field-list'
(setq wl-message-visible-field-list '("^Message-Id:"))

;; X-Face (requires x-face or bitmap-mule)
(when window-system
  (cond ((and (featurep 'xemacs)		; for XEmacs
	      (module-installed-p 'x-face))
	 (autoload 'x-face-xmas-wl-display-x-face "x-face")
	 (setq wl-highlight-x-face-function 'x-face-xmas-wl-display-x-face))
	;; for Mule (GNU Emacs)
	((module-installed-p 'x-face-mule)
	 ;; for x-face-mule distributed with bitmap-mule 8.0 or later
	 (autoload 'x-face-decode-message-header "x-face-mule")
	 (setq wl-highlight-x-face-function 'x-face-decode-message-header))
	))

;; Scoring.
;; "all.SCORE" file is used regardless of wl-score-folder-alist.
;(setq wl-score-folder-alist
;      '(("^-comp\\."
;	 "news.comp.SCORE"
;	 "news.SCORE")
;	("^-"
;	 "news.SCORE")))

;; rule for auto refile.
;(setq wl-refile-rule-alist
;      '(
;	("x-ml-name"
;	 ("^Wanderlust" . "+wl")
;	 ("^Elisp" . "+elisp"))
;	("From"
;	 ("teranisi@isl.ntt.co.jp" . "+teranisi"))))

;; Marks to skip auto-refile (default is "N" "U" "!").
;; nil means all message is auto-refiled.
;(setq wl-summary-auto-refile-skip-marks nil)

;;; dot.wl ends here
