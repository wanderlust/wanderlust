;;; dot.wl -- sample setting file for Wanderlust	-*- emacs-lisp -*-

;; [[ 動作に必要な設定 ]]

;; まず、次の設定を ~/.emacs などに書いてください。
;; XEmacs の package としてインストールされている場合は必要ありません。
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; アイコンを置くディレクトリ
;; XEmacs の package としてインストールされている場合は必要ありません。
;(setq wl-icon-directory "/usr/local/lib/emacs/etc")


;; [[ SEMI の設定 ]]

;; HTML パートを表示しない
;; mime-setup がロードされる前に記述する必要があります。
(setq mime-setup-enable-inline-html nil)

;; 大きいメッセージを送信時に分割しない
(setq mime-edit-split-message nil)

;; 大きいメッセージとみなす行数の設定
;(setq mime-edit-message-default-max-lines 1000)


;;; [[ 個人情報の設定 ]]

;; From: の設定
;(setq wl-from "Your Name <e-mail@example.com>")

;; (system-name) が FQDN を返さない場合、
;; `wl-local-domain' にホスト名を除いたドメイン名を設定してください。
;(setq wl-local-domain "example.com")

;; 自分のメールアドレスのリスト
(setq wl-user-mail-address-list
      (list (wl-address-header-extract-address wl-from)
	    ;; "e-mail2@example.com"
	    ;; "e-mail3@example.net" ...
	    ))

;; 自分の参加しているメーリングリストのリスト
(setq wl-subscribed-mailing-list
      '("wl@lists.airs.net"
	"apel-ja@m17n.org"
	"emacs-mime-ja@m17n.org"
	;; "ml@example.com" ...
	))

;;; [[ サーバの設定 ]]

;; IMAP サーバの設定
(setq elmo-imap4-default-server "localhost")
;; POP サーバの設定
(setq elmo-pop3-default-server "localhost")
;; SMTP サーバの設定
(setq wl-smtp-posting-server "localhost")
;; ニュースサーバの設定
(setq elmo-nntp-default-server "localhost")
;; 投稿先のニュースサーバ
(setq wl-nntp-posting-server elmo-nntp-default-server)

;; IMAP サーバの認証方式の設定
(setq elmo-imap4-default-authenticate-type 'clear) ; 生パスワード
;(setq elmo-imap4-default-authenticate-type 'cram-md5) ; CRAM-MD5

;; POP-before-SMTP
;(setq wl-draft-send-mail-function 'wl-draft-send-mail-with-pop-before-smtp)


;;; [[ 基本的な設定 ]]

;; `wl-summary-goto-folder' の時に選択するデフォルトのフォルダ
;(setq wl-default-folder "+inbox")

;; フォルダ名補完時に使用するデフォルトのスペック
;(setq wl-default-spec "+")

;; Folder Carbon Copy
;(setq wl-fcc "+outbox")

;; 終了時に確認する
(setq wl-interactive-exit t)

;; メール送信時には確認する
(setq wl-interactive-send t)

;; スレッドは常に開く
;(setq wl-thread-insert-opened t)

;; サマリバッファの左にフォルダバッファを表示する (3ペイン表示)
;(setq wl-stay-folder-window t)

;; 長い行を切り縮める
;(setq wl-message-truncate-lines t)
;(setq wl-draft-truncate-lines t)
;; XEmacs (21.4.6 より前) の場合、以下も必要。
;(setq truncate-partial-width-windows nil)

;; ドラフトを新しいフレームで書く
;(setq wl-draft-use-frame t)

;; スレッド表示のインデントを無制限にする。
;(setq wl-summary-indent-length-limit nil)
;(setq wl-summary-width nil)

;; サブジェクトが変わったらスレッドを切って表示
;(setq wl-summary-divide-thread-when-subject-changed t)

;; スレッドの見た目を変える
;(setq wl-thread-indent-level 2)
;(setq wl-thread-have-younger-brother-str "+"
;      wl-thread-youngest-child-str	 "+"
;      wl-thread-vertical-str		 "|"
;      wl-thread-horizontal-str		 "-"
;      wl-thread-space-str		 " ")

;; サマリ移動後に先頭メッセージを表示する
;(setq wl-auto-select-first t)

;; サマリ内の移動で未読メッセージがないと次のフォルダに移動する
;(setq wl-auto-select-next t)

;; 未読がないフォルダは飛ばす(SPCキーだけで読み進める場合は便利)
;(setq wl-auto-select-next 'skip-no-unread)

;; 未読メッセージを優先的に読む
;(setq wl-summary-move-order 'unread)

;; 着信通知の設定
;(setq wl-biff-check-folder-list '("%inbox"))
;(setq wl-biff-notify-hook '(ding))


;;; [[ ネットワーク ]]

;; フォルダ種別ごとのキャッシュの設定
;; (localdir, localnews, maildir はキャッシュできない)
;(setq elmo-archive-use-cache nil)
;(setq elmo-nntp-use-cache t)
;(setq elmo-imap4-use-cache t)
;(setq elmo-pop3-use-cache t)

;; オフライン(unplugged)操作を有効にする(現在はIMAPフォルダのみ)
(setq elmo-enable-disconnected-operation t)

;; unplugged 状態で送信すると，キュー(`wl-queue-folder')に格納する
(setq wl-draft-enable-queuing t)
;; unplugged から plugged に変えたときに，キューにあるメッセージを送信する
(setq wl-auto-flush-queue t)

;; 起動時はオフライン状態にする
;(setq wl-plugged nil)
;; 起動時にポートごとのplug状態を変更する
;(add-hook 'wl-make-plugged-hook
;	  '(lambda ()
;	     ;; server,portのplug状態を新規追加もしくは変更する
;	     (elmo-set-plugged plugged値(t/nil) server port)
;	     ;; port を省略するとserverの全portが変更される
;	     ;; (port を省略して新規の追加はできない)
;	     (elmo-set-plugged plugged値(t/nil) server)
;	     ))


;;; [[ 特殊な設定 ]]

;; グループをcheckした後に未読があるフォルダのグループを自動的に開く
;(add-hook 'wl-folder-check-entity-hook
;	  '(lambda ()
;	     (wl-folder-open-unread-folder entity)
;	     ))

;; サマリ表示関数を変更する

;; `elmo-msgdb-overview-entity-get-extra-field' で参照したいフィールド。
;; 自動リファイルで参照したいフィールドも設定する。
(setq elmo-msgdb-extra-fields
      '("newsgroups"
	"list-id" "x-ml-name" "mailing-list"
	"x-mail-count" "x-ml-count" "x-sequence"))

;; ML のメッセージであれば，サマリの Subject 表示に
;; ML名 や MLにおけるメッセージ番号も表示する
(setq wl-summary-line-format "%n%T%P%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %#%~%s")
;; フォルダ毎にサマリの表示形式を変える設定
;(setq wl-folder-summary-line-format-alist
;      '(("^%inbox\\.emacs\\.wl$" .
;	 "%-5l%T%P%M/%D %h:%m %-4S %[ %17f %] %t%C%s")
;	("^%" . "%T%P%M/%D %h:%m %-4S %[ %17f %] %t%C%s")
;	("^+" . "%n%T%P%M/%D %h:%m %-4S %[ %17f %] %t%C%s")))

;; imput により非同期で送信する
;; (utils/im-wl.el をインストールしておく必要があります。
;;  また，~/.im/Config の設定(Smtpservers)を忘れないことと，
;;  wl-draft-enable-queuing の機能が働かなくなることに注意。)
;(autoload 'wl-draft-send-with-imput-async "im-wl")
;(setq wl-draft-send-function 'wl-draft-send-with-imput-async)


;; 短い User-Agent: フィールドを使う
;(setq wl-generate-mailer-string-function
;      'wl-generate-user-agent-string-1)

;; PGP で暗号化する時に自分の公開鍵も含める。
;; (そのメッセージを自分でも読めるように)
;(setq pgg-encrypt-for-me t)


;;; [[ テンプレート ]]

;; テンプレートの設定
;(setq wl-template-alist
;      '(("default"
;	 ("From" . wl-from)
;	 ("Organization" . "デフォルトの組織名")
;	 (body . "  ○○です。\n"))	; 本文
;	("report"
;	 ("To" . "boss@example.com")
;	 ("Subject" . "報告")
;	 (top . "今週の報告です。\n")	; 本文先頭への挿入
;	 (bottom-file . "~/work/report.txt") ; 本文末尾へファイルの挿入
;	 )
;	))

;; ドラフトバッファの内容により From や Organization などのヘッダを自
;; 動的に変更する
;(setq wl-draft-config-alist
;      '((reply				; 返信元のバッファを見る
;	 "^To: .*test-notsend-wl@lists\\.airs\\.net"
;	 (template . "default"))	; テンプレート
;	("^To: .*test-notsend-wl@lists\\.airs\\.net"
;	 ding				; 関数
;	 ("From" . wl-from)		; 変数
;	 ("Organization" . "組織名"))	; 文字列
;	("^Newsgroups: test.*"
;	 ("Organization" . "ニュース投稿時の組織名"))
;	))

;; ドラフト作成時(返信時)に，自動的にヘッダを変更する
;(add-hook 'wl-mail-setup-hook
;	  '(lambda ()
;	     (unless wl-draft-reedit	; 再編集時は適用しない
;	       (wl-draft-config-exec wl-draft-config-alist))))


;;; [[ 返信時の設定 ]]

;; 返信時のウィンドウを広くする
;(setq wl-draft-reply-buffer-style 'full)

;; 返信時のヘッダに相手の名前を入れない。
;(setq wl-draft-reply-use-address-with-full-name nil)

;; メールの返信時に宛先を付ける方針の設定
;; 下記変数の alist の要素
;; ("返信元に存在するフィールド" .
;;   ('Toフィールド' 'Ccフィールド' 'Newsgroupsフィールド'))

;; "a" (without-argument)では Reply-To: や From: などで指定された唯一人
;; または唯一つの投稿先に返信する。また，X-ML-Name: と Reply-To: がつい
;; ているなら Reply-To: 宛にする。
;(setq wl-draft-reply-without-argument-list
;      '((("X-ML-Name" "Reply-To") . (("Reply-To") nil nil))
;	("X-ML-Name" . (("To" "Cc") nil nil))
;	("Followup-To" . (nil nil ("Followup-To")))
;	("Newsgroups" . (nil nil ("Newsgroups")))
;	("Reply-To" . (("Reply-To") nil nil))
;	("Mail-Reply-To" . (("Mail-Reply-To") nil nil))
;	(wl-draft-self-reply-p . (("To") ("Cc") nil))
;	("From" . (("From") nil nil))))

;; 2.11.0 以前のデフォルト設定
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


;;; [[ メッセージ表示の設定 ]]

;; 隠したいヘッダの設定
(setq wl-message-ignored-field-list
      '(".*Received:" ".*Path:" ".*Id:" "^References:"
	"^Replied:" "^Errors-To:"
	"^Lines:" "^Sender:" ".*Host:" "^Xref:"
	"^Content-Type:" "^Precedence:"
	"^Status:" "^X-VM-.*:"))

;; 表示するヘッダの設定
;; 'wl-message-ignored-field-list' より優先される
(setq wl-message-visible-field-list '("^Message-Id:"))


;; X-Face を表示する
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

;; スコア機能の設定
;; `wl-score-folder-alist' の設定に関わらず必ず "all.SCORE" は使用される。
;(setq wl-score-folder-alist
;      '(("^-comp\\."
;	 "news.comp.SCORE"
;	 "news.SCORE")
;	("^-"
;	 "news.SCORE")))


;; 自動リファイルのルール設定
;(setq wl-refile-rule-alist
;      '(
;	("x-ml-name"
;	 ("^Wanderlust" . "+wl")
;	 ("^Elisp" . "+elisp"))
;	("From"
;	 ("foo@example\\.com" . "+foo"))))

;; 自動リファイルしない永続マークを設定
;; 標準では "N" "U" "!" になっており、未読メッセージを自動リファイルし
;; ません。nil ですべてのメッセージが対象になります。
;(setq wl-summary-auto-refile-skip-marks nil)

;;; [[ spam 用の設定 ]]

;; バックエンドに bogofilter を使う事を設定
;(setq elmo-spam-scheme 'bogofilter)

;(require 'wl-spam)

;; 移動した時に自動で spam かどうかチェックするフォルダを設定
;(setq wl-spam-auto-check-folder-regexp-list '("\\+inbox"))

;; サマリバッファで `o' (wl-summary-refile) した時, *最初*に spam かど
;; うかを判定する様にする
;(unless (memq 'wl-refile-guess-by-spam wl-refile-guess-functions)
;  (setq wl-refile-guess-functions
;	(cons #'wl-refile-guess-by-spam
;	      wl-refile-guess-functions)))

;; サマリバッファで `C-o' (wl-summary-auto-refile) した時, *最初*に
;; spam かどうかを判定する様にする
;(unless (memq 'wl-refile-guess-by-spam wl-auto-refile-guess-functions)
;  (setq wl-auto-refile-guess-functions
;	(cons #'wl-refile-guess-by-spam
;	      wl-auto-refile-guess-functions)))

;; refile-rule を優先したい場合 (spamfilter-wl.el や bogofilter-wl.el
;; と同じ設定) は, こっちの設定を有効にする
;(unless (memq 'wl-refile-guess-by-spam wl-auto-refile-guess-functions)
;  (setq wl-auto-refile-guess-functions
;	(append wl-auto-refile-guess-functions
;		'(wl-refile-guess-by-spam))))

;;; dot.wl ends here
