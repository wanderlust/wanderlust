;;;							-*- emacs-lisp -*-
;;; ~/.wl (setting file for Wanderlust)
;;;						Last-Modified: 1999-11-07
;;;

;; Following must be included in ~/.emacs
;; for .emacs begin
(require 'mime-setup)
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
;; for .emacs end

;;; [[ Private Setting ]]

;; Header From 
;(setq wl-from "Your Name <e-mail-address>")
;; Organization
;(setq wl-organization "")

;;; [[ Basic Setting ]]

;; A directory for message database.
(setq elmo-msgdb-dir "~/.elmo")

;; Home directory for MH (localdir) 
(setq elmo-localdir-folder-path "~/Mail")
;; Default IMAP4 server
(setq elmo-default-imap4-server "localhost")
;; Default POP server
(setq elmo-default-pop3-server "localhost")
;; Default NNTP server
(setq elmo-default-nntp-server "localhost")
;; NNTP server name for posting
(setq wl-nntp-posting-server elmo-default-nntp-server)
;; SMTP server
(setq wl-smtp-posting-server "localhost")

;; Icon directory (XEmacs)
;; (No need if installed as XEmacs package.)
;(setq wl-icon-dir "~/work/wl/etc")

;; If (system-name) does not return FQDN,
;; set following as a local domain name without hostname.
;; ((system-name) "." wl-local-domain is used as domain part of Message-ID
;; and an argument of HELO in SMTP.
;(setq wl-local-domain "localdomain")
;; Specific domain part for message-id.
;(setq wl-message-id-domain "localhost.localdomain")

;(setq wl-default-folder "+inbox")   ;; Default folder for 
				     ;; wl-summary-goto-folder.
;(setq wl-default-spec "+")	     ;; Default string for 
				     ;; folder name completion.

;(setq wl-fcc "+outbox")	     ;; Folder Carbon Copy

(setq wl-interactive-exit t)	     ;; Confirm at exit time.
(setq wl-interactive-send t)	     ;; Confirm at message sending time.

(setq wl-auto-select-first t)	     ;; display first message automatically.
(setq wl-auto-select-next t)	     ;; goto next folder when exit from 
				     ;; summary.
;(setq wl-summary-next-no-unread 'skip-no-unread)  
			             ;; folder is skipped if there is no 
                                     ;; unread.

(setq wl-summary-move-order 'unread) ;; jump to unread message in 'N' or 'P'.
(setq wl-thread-insert-opened t)     ;; Create opened thread.

;(setq wl-stay-folder-window t)	     ;; folder mode and summary mode is
				     ;; displayed at the same time.

;; cache setting.
;; (messages in localdir, localnews, maildir are not cached.)
;(setq elmo-archive-use-cache nil)
;(setq elmo-nntp-use-cache nil)
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
;	     (elmo-set-plugged plugged(t/nil) server port)
;	     (elmo-set-plugged plugged(t/nil) server)
;	     ))


;; highlight setting (for light background)

;; decide group folder color by number.
;(setq wl-highlight-group-folder-by-numbers nil)

(setq wl-highlight-message-header-alist
      '(("Subject[ \t]*:" . wl-highlight-message-subject-header-contents)
	("From[ \t]*:" . wl-highlight-message-from-header-contents)
	("\\(.*To\\|Cc\\|Newsgroups\\)[ \t]*:" . wl-highlight-message-important-header-contents)
	("\\(User-Agent\\|X-Mailer\\|X-Newsreader\\)[ \t]*:" .
	 wl-highlight-message-unimportant-header-contents)
	))
;; don't change color by citation level.
;(setq wl-highlight-citation-face-list
;      '(wl-highlight-message-cited-text-1))

(defun my-wl-set-face (face spec)
  (make-face face)
  (cond ((fboundp 'face-spec-set)
	 (face-spec-set face spec))
	(t
	 (wl-declare-face face spec))))

;; header.
(my-wl-set-face 'wl-highlight-message-subject-header-contents
		'((t (:foreground "blue" :bold t))))
(my-wl-set-face 'wl-highlight-message-from-header-contents
		'((t (:foreground "red" :bold t))))
(my-wl-set-face 'wl-highlight-message-important-header-contents
		'((t (:foreground "purple" :bold t))))
(my-wl-set-face 'wl-highlight-message-unimportant-header-contents
		'((t (:foreground "RoyalBlue" :bold t))))
(my-wl-set-face 'wl-highlight-message-headers
		'((t (:foreground "magenta3" :bold t))))
(my-wl-set-face 'wl-highlight-message-header-contents
		'((t (:foreground "brown" :bold nil))))
(my-wl-set-face 'wl-highlight-message-signature
		'((t (:foreground "blue"))))
;; citation.
(my-wl-set-face 'wl-highlight-message-citation-header
		'((t (:foreground "DarkGreen"))))
(my-wl-set-face 'wl-highlight-message-cited-text-1
		'((t (:foreground "forest green"))))
(my-wl-set-face 'wl-highlight-message-cited-text-2
		'((t (:foreground "SaddleBrown"))))
(my-wl-set-face 'wl-highlight-message-cited-text-3
		'((t (:foreground "orchid3"))))
(my-wl-set-face 'wl-highlight-message-cited-text-4
		'((t (:foreground "purple1"))))
(my-wl-set-face 'wl-highlight-message-cited-text-5
		'((t (:foreground "MediumPurple1"))))
(my-wl-set-face 'wl-highlight-message-cited-text-6
		'((t (:foreground "PaleVioletRed"))))
(my-wl-set-face 'wl-highlight-message-cited-text-7
		'((t (:foreground "LightPink"))))
(my-wl-set-face 'wl-highlight-message-cited-text-8
		'((t (:foreground "salmon"))))
(my-wl-set-face 'wl-highlight-message-cited-text-9
		'((t (:foreground "SandyBrown"))))
(my-wl-set-face 'wl-highlight-message-cited-text-10
		'((t (:foreground "wheat"))))
;; summary.
(my-wl-set-face 'wl-highlight-summary-important-face
		'((t (:foreground "purple"))))
(my-wl-set-face 'wl-highlight-summary-new-face
		'((t (:foreground "tomato"))))
(my-wl-set-face 'wl-highlight-summary-unread-face
		'((t (:foreground "RoyalBlue"))))
(my-wl-set-face 'wl-highlight-summary-deleted-face
		'((t (:foreground "gray"))))
(my-wl-set-face 'wl-highlight-summary-refiled-face
		'((t (:foreground "blue"))))
(my-wl-set-face 'wl-highlight-summary-temp-face
		'((t (:foreground "salmon"))))
(my-wl-set-face 'wl-highlight-summary-displaying-face
		'((t (:bold t :underline t))))
;; (thread)
(my-wl-set-face 'wl-highlight-summary-thread-top-face
		'((t (:foreground "green4"))))
(my-wl-set-face 'wl-highlight-summary-normal-face
		'((t (:foreground "SeaGreen"))))
;; folder
(my-wl-set-face 'wl-highlight-folder-unknown-face
		'((t (:foreground "RoyalBlue"))))
(my-wl-set-face 'wl-highlight-folder-killed-face
		'((t (:foreground "gray50"))))
(my-wl-set-face 'wl-highlight-folder-unread-face
		'((t (:foreground "brown"))))
(my-wl-set-face 'wl-highlight-folder-zero-face
		'((t (:foreground "blue4"))))
(my-wl-set-face 'wl-highlight-folder-few-face
		'((t (:foreground "tomato"))))
(my-wl-set-face 'wl-highlight-folder-many-face
		'((t (:foreground "HotPink1"))))
;; group
(my-wl-set-face 'wl-highlight-folder-opened-face
		'((t (:foreground "forest green"))))
(my-wl-set-face 'wl-highlight-folder-closed-face
		'((t (:foreground "DarkOliveGreen4"))))
;; demo
(my-wl-set-face 'wl-highlight-demo-face
		'((t (:foreground "blue2"))))


;;; [[ Special Setting ]]

;; compress ~/elmo  using jka-compr.
;(setq elmo-msgdb-overview-filename "overview.gz")
;(setq elmo-msgdb-number-filename "number.gz")
;(setq wl-summary-cache-file ".wl-summary-cache.gz")
;(setq wl-thread-top-file ".wl-thread-top.gz")


;; open unread group folder after checking.
(add-hook 'wl-folder-check-entity-hook
	  '(lambda ()
	     (wl-folder-open-unread-folder entity)
	     ))

;; User's mail addresses.
(setq wl-user-mail-address-list
      (list (wl-address-header-extract-address wl-from)
	    ;;"e-mail2@bbb.com" ...
	    ))

;; Subscribed mailing list
(setq wl-subscribed-mailing-list
      '("wl@lists.airs.net"
	"apel-ja@m17n.org"
	;;"ml@example.com" ...
	))

;; Change summary display function.

;; get extra field values as overview information (only localdir folder).
(setq elmo-msgdb-extra-fields '("newsgroups"
				"x-ml-name"
				"x-mail-count" "x-ml-count"
				"x-sequence"
				"mailing-list"))

;;; ML message displays ML name and ML sequence number in subject.
(setq wl-summary-subject-func 'my-wl-summary-subject-func-ml)
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
	 "^\\s(\\(.+\\)[ :]\\([0-9]+\\)\\s)[ \t]*"
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
;(setq wl-draft-send-func 'wl-draft-send-with-imput-async)


;; template
(setq wl-template-alist
      '(("default"
	 ("From" . wl-from)
	 ("Organization" . "~/.wl sample")
	 (body . "Hello, this is XXX \n"))		;; body
	("report"
	 ("To" . "boss@company.jp")
	 ("Subject" . "Report")
	 (top . "Sir, here is my report\n")		;; insert in top.
;;	 (file-bottom . "~/work/report.txt")	;; insert file in bottom
	 )
	))
;; Change headers in draft sending time.
(setq wl-draft-config-alist
      '((reply		;; see reply buffer
	 "^To: .*\\(test-notsend-wl@lists.airs.net\\)"
	 (template . "default"))		;; template
	("^To: .*\\(test-notsend-wl@lists.airs.net\\)"
	 wl-ml-draft-config-func		;; function
	 ("From" . wl-from)			;; variable
	 ("Organization" . "~/.wl sample"))	;; string
	("^Newsgroups: test.*"
	 ("Organization" . "organization for nntp."))
	))
;; Change headers in draft preparation time.
; (add-hook 'wl-mail-setup-hook
;           '(lambda ()
;              (unless wl-draft-reedit    ;; don't apply when reedit.
;                (wl-draft-config-exec wl-draft-config-alist))))

;; header value setting for mail reply.

;; "a" (without-argument) reply to author (Reply-To or From).
;; if 'X-ML-Name' and 'Reply-To' exists, reply to 'Reply-To'.
; (setq wl-draft-reply-without-argument-list
;       '((("X-ML-Name" "Reply-To") . (("Reply-To") nil nil))
; 	("X-ML-Name" . (("To" "Cc") nil nil))
; 	("Followup-To" . (nil nil ("Followup-To")))
; 	("Newsgroups" . (nil nil ("Newsgroups")))
; 	("Reply-To" . (("Reply-To") nil nil))
; 	("Mail-Reply-To" . (("Mail-Reply-To") nil nil))
; 	("From" . (("From") nil nil))))
; 
;; "C-u a" (with-argument) reply to all.
; (setq wl-draft-reply-with-argument-list
;       '(("Followup-To" . (("From") nil ("Followup-To")))
; 	("Newsgroups" . (("From") nil ("Newsgroups")))
; 	("Mail-Followup-To" . (("Mail-Followup-To") nil ("Newsgroups")))
; 	("From" . (("From") ("To" "Cc") ("Newsgroups")))))


;; X-Face (requires x-face (and x-face-mule))

(when (and window-system
	   (module-installed-p 'x-face))
  (cond (wl-on-xemacs				;; for XEmacs
	 (autoload 'x-face-xmas-wl-display-x-face "x-face" nil t)
	 (setq wl-highlight-x-face-func
	       'x-face-xmas-wl-display-x-face))
	((module-installed-p 'x-face-mule)	;; for Mule (GNU Emacs)
	 ;; after x-face-mule 0.20
	 (setq wl-highlight-x-face-func
	       (function
		(lambda (beg end)
		  (x-face-mule-x-face-decode-message-header beg end))))
	 (setq x-face-mule-highlight-x-face-style 'xmas)
	 (require 'x-face-mule)
	 )))

;; rule for auto refile.
;(setq wl-refile-rule-alist
;      '(
;     	("x-ml-name"
;     	 ("^Wanderlust" . "+wl")
;     	 ("^Elisp" . "+elisp"))
;     	("From"
;     	 ("teranisi@isl.ntt.co.jp" . "+teranisi"))))

;; Marks to skip auto-refile (default is "N" "U" "!").
;; nil means all message is auto-refiled.
;(setq wl-summary-auto-refile-skip-marks nil)

;; Scoring.
;; "all.SCORE" file is used regardless of wl-score-folder-alist.
; (setq wl-score-folder-alist
;       '(("^-comp\\."
; 	 "news.comp.SCORE"
; 	 "news.SCORE")
; 	("^-"
; 	 "news.SCORE")))
;; directory for storing score files.
; (setq wl-score-files-directory "~/.elmo/")


;;;
;;; end of file
;;;
