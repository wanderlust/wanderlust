;;; dot.wl -- sample setting file for Wanderlust	-*- emacs-lisp -*-

;; [[ $BF0:n$KI,MW$J@_Dj(B ]]

;; $B$^$:!"<!$N@_Dj$r(B ~/.emacs $B$J$I$K=q$$$F$/$@$5$$!#(B
;; XEmacs $B$N(B package $B$H$7$F%$%s%9%H!<%k$5$l$F$$$k>l9g$OI,MW$"$j$^$;$s!#(B
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; $B%"%$%3%s$rCV$/%G%#%l%/%H%j(B
;; XEmacs $B$N(B package $B$H$7$F%$%s%9%H!<%k$5$l$F$$$k>l9g$OI,MW$"$j$^$;$s!#(B
;(setq wl-icon-directory "/usr/local/lib/emacs/etc")


;; [[ SEMI $B$N@_Dj(B ]]

;; HTML $B%Q!<%H$rI=<($7$J$$(B
;; mime-setup $B$,%m!<%I$5$l$kA0$K5-=R$9$kI,MW$,$"$j$^$9!#(B
(setq mime-setup-enable-inline-html nil)

;; $BBg$-$$%a%C%;!<%8$rAw?.;~$KJ,3d$7$J$$(B
(setq mime-edit-split-message nil)

;; $BBg$-$$%a%C%;!<%8$H$_$J$99T?t$N@_Dj(B
;(setq mime-edit-message-default-max-lines 1000)


;;; [[ $B8D?M>pJs$N@_Dj(B ]]

;; From: $B$N@_Dj(B
;(setq wl-from "Your Name <e-mail@example.com>")

;; (system-name) $B$,(B FQDN $B$rJV$5$J$$>l9g!"(B
;; `wl-local-domain' $B$K%[%9%HL>$r=|$$$?%I%a%$%sL>$r@_Dj$7$F$/$@$5$$!#(B
;(setq wl-local-domain "example.com")

;; $B<+J,$N%a!<%k%"%I%l%9$N%j%9%H(B
(setq wl-user-mail-address-list
      (list (wl-address-header-extract-address wl-from)
	    ;; "e-mail2@example.com"
	    ;; "e-mail3@example.net" ...
	    ))

;; $B<+J,$N;22C$7$F$$$k%a!<%j%s%0%j%9%H$N%j%9%H(B
(setq wl-subscribed-mailing-list
      '("wl@ml.gentei.org"
	"apel-ja@m17n.org"
	"emacs-mime-ja@m17n.org"
	;; "ml@example.com" ...
	))

;;; [[ $B%5!<%P$N@_Dj(B ]]

;; IMAP $B%5!<%P$N@_Dj(B
(setq elmo-imap4-default-server "localhost")
;; POP $B%5!<%P$N@_Dj(B
(setq elmo-pop3-default-server "localhost")
;; SMTP $B%5!<%P$N@_Dj(B
(setq wl-smtp-posting-server "localhost")
;; $B%K%e!<%9%5!<%P$N@_Dj(B
(setq elmo-nntp-default-server "localhost")
;; $BEj9F@h$N%K%e!<%9%5!<%P(B
(setq wl-nntp-posting-server elmo-nntp-default-server)

;; IMAP $B%5!<%P$NG'>ZJ}<0$N@_Dj(B
(setq elmo-imap4-default-authenticate-type 'clear) ; $B@8%Q%9%o!<%I(B
;(setq elmo-imap4-default-authenticate-type 'cram-md5) ; CRAM-MD5

;; POP-before-SMTP
;(setq wl-draft-send-mail-function 'wl-draft-send-mail-with-pop-before-smtp)


;;; [[ $B4pK\E*$J@_Dj(B ]]

;; `wl-summary-goto-folder' $B$N;~$KA*Br$9$k%G%U%)%k%H$N%U%)%k%@(B
;(setq wl-default-folder "+inbox")

;; $B%U%)%k%@L>Jd40;~$K;HMQ$9$k%G%U%)%k%H$N%9%Z%C%/(B
;(setq wl-default-spec "+")

;; Folder Carbon Copy
;(setq wl-fcc "+outbox")

;; $B=*N;;~$K3NG'$9$k(B
(setq wl-interactive-exit t)

;; $B%a!<%kAw?.;~$K$O3NG'$9$k(B
(setq wl-interactive-send t)

;; $B%9%l%C%I$O>o$K3+$/(B
;(setq wl-thread-insert-opened t)

;; $B%5%^%j%P%C%U%!$N:8$K%U%)%k%@%P%C%U%!$rI=<($9$k(B (3$B%Z%$%sI=<((B)
;(setq wl-stay-folder-window t)

;; $BD9$$9T$r@Z$j=L$a$k(B
;(setq wl-message-truncate-lines t)
;(setq wl-draft-truncate-lines t)
;; XEmacs (21.4.6 $B$h$jA0(B) $B$N>l9g!"0J2<$bI,MW!#(B
;(setq truncate-partial-width-windows nil)

;; $B%I%i%U%H$r?7$7$$%U%l!<%`$G=q$/(B
;(setq wl-draft-use-frame t)

;; $B%9%l%C%II=<($N%$%s%G%s%H$rL5@)8B$K$9$k!#(B
;(setq wl-summary-indent-length-limit nil)
;(setq wl-summary-width nil)

;; $B%5%V%8%'%/%H$,JQ$o$C$?$i%9%l%C%I$r@Z$C$FI=<((B
;(setq wl-summary-divide-thread-when-subject-changed t)

;; $B%9%l%C%I$N8+$?L\$rJQ$($k(B
;(setq wl-thread-indent-level 2)
;(setq wl-thread-have-younger-brother-str "+"
;      wl-thread-youngest-child-str	 "+"
;      wl-thread-vertical-str		 "|"
;      wl-thread-horizontal-str		 "-"
;      wl-thread-space-str		 " ")

;; $B%5%^%j0\F08e$K@hF,%a%C%;!<%8$rI=<($9$k(B
;(setq wl-auto-select-first t)

;; $B%5%^%jFb$N0\F0$GL$FI%a%C%;!<%8$,$J$$$H<!$N%U%)%k%@$K0\F0$9$k(B
;(setq wl-auto-select-next t)

;; $BL$FI$,$J$$%U%)%k%@$OHt$P$9(B(SPC$B%-!<$@$1$GFI$_?J$a$k>l9g$OJXMx(B)
;(setq wl-auto-select-next 'skip-no-unread)

;; $BL$FI%a%C%;!<%8$rM%@hE*$KFI$`(B
;(setq wl-summary-move-order 'unread)

;; $BCe?.DLCN$N@_Dj(B
;(setq wl-biff-check-folder-list '("%inbox"))
;(setq wl-biff-notify-hook '(ding))


;;; [[ $B%M%C%H%o!<%/(B ]]

;; $B%U%)%k%@<oJL$4$H$N%-%c%C%7%e$N@_Dj(B
;; (localdir, localnews, maildir $B$O%-%c%C%7%e$G$-$J$$(B)
;(setq elmo-archive-use-cache nil)
;(setq elmo-nntp-use-cache t)
;(setq elmo-imap4-use-cache t)
;(setq elmo-pop3-use-cache t)

;; $B%*%U%i%$%s(B(unplugged)$BA`:n$rM-8z$K$9$k(B($B8=:_$O(BIMAP$B%U%)%k%@$N$_(B)
(setq elmo-enable-disconnected-operation t)

;; unplugged $B>uBV$GAw?.$9$k$H!$%-%e!<(B(`wl-queue-folder')$B$K3JG<$9$k(B
(setq wl-draft-enable-queuing t)
;; unplugged $B$+$i(B plugged $B$KJQ$($?$H$-$K!$%-%e!<$K$"$k%a%C%;!<%8$rAw?.$9$k(B
(setq wl-auto-flush-queue t)

;; $B5/F0;~$O%*%U%i%$%s>uBV$K$9$k(B
;(setq wl-plugged nil)
;; $B5/F0;~$K%]!<%H$4$H$N(Bplug$B>uBV$rJQ99$9$k(B
;(add-hook 'wl-make-plugged-hook
;	  '(lambda ()
;	     ;; server,port$B$N(Bplug$B>uBV$r?75,DI2C$b$7$/$OJQ99$9$k(B
;	     (elmo-set-plugged plugged$BCM(B(t/nil) server port)
;	     ;; port $B$r>JN,$9$k$H(Bserver$B$NA4(Bport$B$,JQ99$5$l$k(B
;	     ;; (port $B$r>JN,$7$F?75,$NDI2C$O$G$-$J$$(B)
;	     (elmo-set-plugged plugged$BCM(B(t/nil) server)
;	     ))


;;; [[ $BFC<l$J@_Dj(B ]]

;; $B%0%k!<%W$r(Bcheck$B$7$?8e$KL$FI$,$"$k%U%)%k%@$N%0%k!<%W$r<+F0E*$K3+$/(B
;(add-hook 'wl-folder-check-entity-hook
;	  '(lambda ()
;	     (wl-folder-open-unread-folder entity)
;	     ))

;; `wl-summary-exit' $B$K$h$j%U%#%k%?$5$l$kA0$N%U%)%k%@$KHt$V!#%U%#%k%?%U%)%k%@$r(B
;; `wl-summary-virtual' $B$G@8@.$5$l$k0l;~E*$J%U%)%k%@$H$7$F;H$&>l9g$KM-MQ!#(B
;(add-hook 'wl-summary-prepared-hook
;	  '(lambda ()
;	     (setq wl-summary-buffer-exit-function
;		   (when (eq 'filter
;			     (elmo-folder-type-internal wl-summary-buffer-elmo-folder))
;		     'wl-summary-unvirtual))))

;; $B%5%^%jI=<(4X?t$rJQ99$9$k(B

;; `elmo-message-entity-field' $B$G;2>H$7$?$$%U%#!<%k%I!#(B
;; $B<+F0%j%U%!%$%k$G;2>H$7$?$$%U%#!<%k%I$b@_Dj$9$k!#(B
(setq elmo-msgdb-extra-fields
      '(;; elsp-header $B$G;2>H$9$k%U%#!<%k%I(B
	"x-spam-flag"
	;; `modb-entity-field-extractor-alist' $B$GDj5A$5$l$?2>A[%U%#!<%k%I(B
	"ml-info"))

;; ML $B$N%a%C%;!<%8$G$"$l$P!$%5%^%j$N(B Subject $BI=<($K(B
;; ML$BL>(B $B$d(B ML$B$K$*$1$k%a%C%;!<%8HV9f$bI=<($9$k(B
(setq wl-summary-line-format "%n%T%P%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %#%~%s")
;; $B%U%)%k%@Kh$K%5%^%j$NI=<(7A<0$rJQ$($k@_Dj(B
;(setq wl-folder-summary-line-format-alist
;      '(("^%inbox\\.emacs\\.wl$" .
;	 "%-5l%T%P%M/%D %h:%m %-4S %[ %17f %] %t%C%s")
;	("^%" . "%T%P%M/%D %h:%m %-4S %[ %17f %] %t%C%s")
;	("^+" . "%n%T%P%M/%D %h:%m %-4S %[ %17f %] %t%C%s")))

;; imput $B$K$h$jHsF14|$GAw?.$9$k(B
;; (utils/im-wl.el $B$r%$%s%9%H!<%k$7$F$*$/I,MW$,$"$j$^$9!#(B
;;  $B$^$?!$(B~/.im/Config $B$N@_Dj(B(Smtpservers)$B$rK:$l$J$$$3$H$H!$(B
;;  wl-draft-enable-queuing $B$N5!G=$,F/$+$J$/$J$k$3$H$KCm0U!#(B)
;(autoload 'wl-draft-send-with-imput-async "im-wl")
;(setq wl-draft-send-function 'wl-draft-send-with-imput-async)


;; $BC;$$(B User-Agent: $B%U%#!<%k%I$r;H$&(B
;(setq wl-generate-mailer-string-function
;      'wl-generate-user-agent-string-1)

;; PGP $B$G0E9f2=$9$k;~$K<+J,$N8x3+80$b4^$a$k!#(B
;; ($B$=$N%a%C%;!<%8$r<+J,$G$bFI$a$k$h$&$K(B)
;(setq pgg-encrypt-for-me t)


;;; [[ $B%F%s%W%l!<%H(B ]]

;; $B%F%s%W%l!<%H$N@_Dj(B
;(setq wl-template-alist
;      '(("default"
;	 ("From" . wl-from)
;	 ("Organization" . "$B%G%U%)%k%H$NAH?%L>(B")
;	 (body . "  $B!{!{$G$9!#(B\n"))	; $BK\J8(B
;	("report"
;	 ("To" . "boss@example.com")
;	 ("Subject" . "$BJs9p(B")
;	 (top . "$B:#=5$NJs9p$G$9!#(B\n")	; $BK\J8@hF,$X$NA^F~(B
;	 (bottom-file . "~/work/report.txt") ; $BK\J8KvHx$X%U%!%$%k$NA^F~(B
;	 )
;	))

;; $B%I%i%U%H%P%C%U%!$NFbMF$K$h$j(B From $B$d(B Organization $B$J$I$N%X%C%@$r<+(B
;; $BF0E*$KJQ99$9$k(B
;(setq wl-draft-config-alist
;      '((reply				; $BJV?.85$N%P%C%U%!$r8+$k(B
;	 "^To: .*test-notsend-wl@lists\\.airs\\.net"
;	 (template . "default"))	; $B%F%s%W%l!<%H(B
;	("^To: .*test-notsend-wl@lists\\.airs\\.net"
;	 ding				; $B4X?t(B
;	 ("From" . wl-from)		; $BJQ?t(B
;	 ("Organization" . "$BAH?%L>(B"))	; $BJ8;zNs(B
;	("^Newsgroups: test.*"
;	 ("Organization" . "$B%K%e!<%9Ej9F;~$NAH?%L>(B"))
;	))

;; $B%I%i%U%H:n@.;~(B($BJV?.;~(B)$B$K!$<+F0E*$K%X%C%@$rJQ99$9$k(B
;(add-hook 'wl-mail-setup-hook
;	  '(lambda ()
;	     (unless wl-draft-reedit	; $B:FJT=8;~$OE,MQ$7$J$$(B
;	       (wl-draft-config-exec wl-draft-config-alist))))


;;; [[ $BJV?.;~$N@_Dj(B ]]

;; $BJV?.;~$N%&%#%s%I%&$r9-$/$9$k(B
;(setq wl-draft-reply-buffer-style 'full)

;; $BJV?.;~$N%X%C%@$KAj<j$NL>A0$rF~$l$J$$!#(B
;(setq wl-draft-reply-use-address-with-full-name nil)

;; $B%a!<%k$NJV?.;~$K08@h$rIU$1$kJ}?K$N@_Dj(B
;; $B2<5-JQ?t$N(B alist $B$NMWAG(B
;; ("$BJV?.85$KB8:_$9$k%U%#!<%k%I(B" .
;;   ('To$B%U%#!<%k%I(B' 'Cc$B%U%#!<%k%I(B' 'Newsgroups$B%U%#!<%k%I(B'))

;; "a" (without-argument)$B$G$O(B Reply-To: $B$d(B From: $B$J$I$G;XDj$5$l$?M#0l?M(B
;; $B$^$?$OM#0l$D$NEj9F@h$KJV?.$9$k!#$^$?!$(BX-ML-Name: $B$H(B Reply-To: $B$,$D$$(B
;; $B$F$$$k$J$i(B Reply-To: $B08$K$9$k!#(B
;(setq wl-draft-reply-without-argument-list
;      '((("X-ML-Name" "Reply-To") . (("Reply-To") nil nil))
;	("X-ML-Name" . (("To" "Cc") nil nil))
;	("Followup-To" . (nil nil ("Followup-To")))
;	("Newsgroups" . (nil nil ("Newsgroups")))
;	("Reply-To" . (("Reply-To") nil nil))
;	("Mail-Reply-To" . (("Mail-Reply-To") nil nil))
;	(wl-draft-self-reply-p . (("To") ("Cc") nil))
;	("From" . (("From") nil nil))))

;; 2.11.0 $B0JA0$N%G%U%)%k%H@_Dj(B
;(setq wl-draft-reply-without-argument-list
;      '(((wl-draft-self-reply-p
;	  "Followup-To") . (("To") ("Cc") ("Followup-To")))
;	((wl-draft-self-reply-p
;	  "Newsgroups") . (("To") ("Cc") ("Newsgroups")))
;	((wl-draft-self-reply-p
;	  "From") . (("To") ("Cc") nil))
;	("Followup-To" . (nil nil ("Followup-To")))
;	("Mail-Followup-To" . (("Mail-Followup-To") nil ("Newsgroups")))
;	("Reply-To" . (("Reply-To") ("To" "Cc" "From") ("Newsgroups")))
;	("From" . (("From") ("To" "Cc") ("Newsgroups")))))
;(setq wl-draft-reply-with-argument-list
;      '(((wl-draft-self-reply-p
;	  "Followup-To") . (("To") ("Cc") ("Followup-To")))
;	((wl-draft-self-reply-p
;	  "Newsgroups") . (("To") ("Cc") ("Newsgroups")))
;	((wl-draft-self-reply-p
;	  "From") . (("To") ("Cc") nil))
;	("Reply-To" . (("Reply-To") nil nil))
;	("Mail-Reply-To" . (("Mail-Reply-To") nil nil))
;	("From" . (("From") nil nil))))


;;; [[ $B%a%C%;!<%8I=<($N@_Dj(B ]]

;; $B1#$7$?$$%X%C%@$N@_Dj(B
(setq wl-message-ignored-field-list
      '(".*Received:" ".*Path:" ".*Id:" "^References:"
	"^Replied:" "^Errors-To:"
	"^Lines:" "^Sender:" ".*Host:" "^Xref:"
	"^Content-Type:" "^Precedence:"
	"^Status:" "^X-VM-.*:"))

;; $BI=<($9$k%X%C%@$N@_Dj(B
;; 'wl-message-ignored-field-list' $B$h$jM%@h$5$l$k(B
(setq wl-message-visible-field-list '("^Message-Id:"))

;; $BJ,3d$5$l$?%a%C%;!<%8$O<+F0E*$K7k9g$9$k(B
;(setq wl-message-auto-reassemble-message/partial t)

;; X-Face $B$rI=<($9$k(B
(when window-system
  (cond ((and (featurep 'xemacs)	; for XEmacs
	      (module-installed-p 'x-face))
	 (autoload 'x-face-xmas-wl-display-x-face "x-face")
	 (setq wl-highlight-x-face-function 'x-face-xmas-wl-display-x-face))

	;; for Emacs21
	((and (not (featurep 'xemacs))
	      (= emacs-major-version 21)
	      (module-installed-p 'x-face-e21))
	 (autoload 'x-face-decode-message-header "x-face-e21")
	 (setq wl-highlight-x-face-function 'x-face-decode-message-header))

	;; for Emacs 19.34, Emacs 20.x
	((module-installed-p 'x-face-mule)
	 ;; x-face-mule distributed with bitmap-mule 8.0 or later
	 (autoload 'x-face-decode-message-header "x-face-mule")
	 (setq wl-highlight-x-face-function 'x-face-decode-message-header))
	))

;; $B%9%3%"5!G=$N@_Dj(B
;; `wl-score-folder-alist' $B$N@_Dj$K4X$o$i$:I,$:(B "all.SCORE" $B$O;HMQ$5$l$k!#(B
;(setq wl-score-folder-alist
;      '(("^-comp\\."
;	 "news.comp.SCORE"
;	 "news.SCORE")
;	("^-"
;	 "news.SCORE")))


;; $B<+F0%j%U%!%$%k$N%k!<%k@_Dj(B
;(setq wl-refile-rule-alist
;      '(
;	("x-ml-name"
;	 ("^Wanderlust" . "+wl")
;	 ("^Elisp" . "+elisp"))
;	("From"
;	 ("foo@example\\.com" . "+foo"))))

;; $B<+F0%j%U%!%$%k$7$J$$1JB3%^!<%/$r@_Dj(B
;; $BI8=`$G$O(B "N" "U" "!" $B$K$J$C$F$*$j!"L$FI%a%C%;!<%8$r<+F0%j%U%!%$%k$7(B
;; $B$^$;$s!#(Bnil $B$G$9$Y$F$N%a%C%;!<%8$,BP>]$K$J$j$^$9!#(B
;(setq wl-summary-auto-refile-skip-marks nil)

;;; [[ spam $BMQ$N@_Dj(B ]]

;; $B%P%C%/%(%s%I$K(B bogofilter $B$r;H$&;v$r@_Dj(B
;(setq elmo-spam-scheme 'bogofilter)

;(require 'wl-spam)

;; $B0\F0$7$?;~$K<+F0$G(B spam $B$+$I$&$+%A%'%C%/$9$k%U%)%k%@$r@_Dj(B
;(setq wl-spam-auto-check-folder-regexp-list '("\\+inbox"))

;; $B%5%^%j%P%C%U%!$G(B `o' (wl-summary-refile) $B$7$?;~(B, *$B:G=i(B*$B$K(B spam $B$+$I(B
;; $B$&$+$rH=Dj$9$kMM$K$9$k(B
;(unless (memq 'wl-refile-guess-by-spam wl-refile-guess-functions)
;  (setq wl-refile-guess-functions
;	(cons #'wl-refile-guess-by-spam
;	      wl-refile-guess-functions)))

;; $B%5%^%j%P%C%U%!$G(B `C-o' (wl-summary-auto-refile) $B$7$?;~(B, *$B:G=i(B*$B$K(B
;; spam $B$+$I$&$+$rH=Dj$9$kMM$K$9$k(B
;(unless (memq 'wl-refile-guess-by-spam wl-auto-refile-guess-functions)
;  (setq wl-auto-refile-guess-functions
;	(cons #'wl-refile-guess-by-spam
;	      wl-auto-refile-guess-functions)))

;; refile-rule $B$rM%@h$7$?$$>l9g(B (spamfilter-wl.el $B$d(B bogofilter-wl.el
;; $B$HF1$8@_Dj(B) $B$O(B, $B$3$C$A$N@_Dj$rM-8z$K$9$k(B
;(unless (memq 'wl-refile-guess-by-spam wl-auto-refile-guess-functions)
;  (setq wl-auto-refile-guess-functions
;	(append wl-auto-refile-guess-functions
;		'(wl-refile-guess-by-spam))))

;;; dot.wl ends here
