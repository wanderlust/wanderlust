;;;							-*- emacs-lisp -*-
;;; ~/.wl (setting file for Wanderlust)
;;;

;; まず，次の設定を ~/.emacs などに書いてください。
;; ここから
(require 'mime-setup)
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
;; ここまで

;;; [[ 個人情報の設定 ]]

;; From の設定
;(setq wl-from "Your Name <e-mail-address>")
;; Organization の設定
;(setq wl-organization "")

;;; [[ 基本的な設定 ]]

;; メッセージデータベースを作るディレクトリ
(setq elmo-msgdb-dir "~/.elmo")

;; MH (localdir) のホーム
(setq elmo-localdir-folder-path "~/Mail")
;; IMAP4サーバの設定
(setq elmo-default-imap4-server "localhost")
;; POPサーバの設定
(setq elmo-default-pop3-server "localhost")
;; ニュースサーバの設定
(setq elmo-default-nntp-server "localhost")
;; 投稿先の ニュースサーバ
(setq wl-nntp-posting-server elmo-default-nntp-server)
;; メールを送信する先の (SMTP)サーバ
(setq wl-smtp-posting-server "localhost")

;; アイコンを置くディレクトリ (XEmacs のみ)
;; (XEmacs の package としてインストールされている場合、必要ありません)
;(setq wl-icon-dir "~/work/wl/etc")

;; (system-name) がFQDNを返さない場合、
;; 以下をホスト名を除いたドメイン名を設定してください。
;; ((system-name)  "." wl-local-domain が Message-ID の作成、
;; SMTP の HELO に使用 されます。)
;(setq wl-local-domain "localdomain")
;; Message-ID のドメインパートを強制的に指定
;(setq wl-message-id-domain "localhost.localdomain")

;(setq wl-default-folder "+inbox")   ;; wl-summary-goto-folder の時に選択する
				     ;; デフォルトのフォルダ
;(setq wl-default-spec "+")	     ;; フォルダ名補完時に使用する
				     ;; デフォルトのスペック

;(setq wl-fcc "+outbox")	     ;; Folder Carbon Copy

(setq wl-interactive-exit t)	     ;; 終了時に確認する
(setq wl-interactive-send t)	     ;; メール送信時には確認する

(setq wl-auto-select-first t)	     ;; サマリ移動後に先頭メッセージを表示する
(setq wl-auto-select-next t)	     ;; サマリ内の移動で未読メッセージがないと
				     ;; 次のフォルダに移動する
;(setq wl-auto-select-next 'skip-no-unread)
				     ;; 未読がないフォルダは飛ばす
				     ;; SPCキーだけで読み進める場合は便利
(setq wl-summary-move-order 'unread) ;; 未読メッセージを優先的に読む
(setq wl-thread-insert-opened t)     ;; thread作成時は常にopenにする

;(setq wl-stay-folder-window t)	     ;; サマリに移動したときにフォルダバッファ
				     ;; の右にサマリのバッファを表示する

;; フォルダ種別ごとのキャッシュの設定
;; (localdir, localnews, maildir はキャッシュしない)
;(setq elmo-archive-use-cache nil)
;(setq elmo-nntp-use-cache t)
;(setq elmo-imap4-use-cache t)
;(setq elmo-pop3-use-cache t)

;; オフライン(unplugged)操作を有効にする(現在はIMAPフォルダのみ)
;(setq elmo-enable-disconnected-operation t)

;; unplugged 状態で送信すると，キュー(`wl-queue-folder')に格納する
(setq wl-draft-enable-queuing t)
;; unplugged から plugged に変えると，キューにあるメッセージを送信する
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


;; highlightの設定 (明るい背景色の場合です)

;; グループを未読数により色分けしない。開閉状態により色分けする。
;(setq wl-highlight-group-folder-by-numbers nil)

(setq wl-highlight-message-header-alist
      '(("Subject[ \t]*:" . wl-highlight-message-subject-header-contents)
	("From[ \t]*:" . wl-highlight-message-from-header-contents)
	("\\(.*To\\|Cc\\|Newsgroups\\)[ \t]*:" . wl-highlight-message-important-header-contents)
	("\\(User-Agent\\|X-Mailer\\|X-Newsreader\\)[ \t]*:" .
	 wl-highlight-message-unimportant-header-contents)
	))
;; 引用レベルで色分けしない
;(setq wl-highlight-citation-face-list
;      '(wl-highlight-message-cited-text-1))

(defun my-wl-set-face (face spec)
  (make-face face)
  (cond ((fboundp 'face-spec-set)
	 (face-spec-set face spec))
	(t
	 (wl-declare-face face spec))))

;; メッセージヘッダ
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
;; 引用
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
;; サマリ
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
;; (スレッド)
(my-wl-set-face 'wl-highlight-summary-thread-top-face
		'((t (:foreground "green4"))))
(my-wl-set-face 'wl-highlight-summary-normal-face
		'((t (:foreground "SeaGreen"))))
;; フォルダ
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
;; グループ
(my-wl-set-face 'wl-highlight-folder-opened-face
		'((t (:foreground "forest green"))))
(my-wl-set-face 'wl-highlight-folder-closed-face
		'((t (:foreground "DarkOliveGreen4"))))
;; スタートアップデモ
(my-wl-set-face 'wl-highlight-demo-face
		'((t (:foreground "blue2"))))


;;; [[ 特殊な設定 ]]

;; jka-compr を利用して ~/elmo/SPEC/ 以下のデータベースを圧縮する
;(setq elmo-msgdb-overview-filename "overview.gz")
;(setq elmo-msgdb-number-filename "number.gz")
;(setq wl-summary-cache-file ".wl-summary-cache.gz")
;(setq wl-thread-top-file ".wl-thread-top.gz")


;; グループをcheckした後に未読があるフォルダのグループを自動的に開く
(add-hook 'wl-folder-check-entity-hook
	  '(lambda ()
	     (wl-folder-open-unread-folder entity)
	     ))

;; 自分のメールアドレスのリスト
(setq wl-user-mail-address-list
      (list (wl-address-header-extract-address wl-from)
	    ;;"e-mail2@bbb.com" ...
	    ))

;; 自分の参加しているメーリングリストのリスト
(setq wl-subscribed-mailing-list
      '("wl@lists.airs.net"
	"apel-ja@m17n.org"
	;;"ml@example.com" ...
	))

;; サマリ表示関数を変更する

;; サマリ表示において使用する情報を持つフィールドをoverview情報に
;; 入れる設定(ただし，localフォルダのみ)
;; 自動リファイルに必要なフィールドも設定
(setq elmo-msgdb-extra-fields '("newsgroups"
				"x-ml-name"
				"x-mail-count" "x-ml-count"
				"x-sequence"
				"mailing-list"))

;;; ML のメッセージであれば，サマリの Subject 表示に
;;; ML名 や MLにおけるメッセージ番号も表示する
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

;; imput により非同期で送信する
;; (utils/im-wl.el をインストールしておく必要があります。
;;  また，~/.im/Config の設定(Smtpservers)を忘れないことと，
;;  wl-draft-enable-queuing の機能が働かなくなることに注意。)
;(autoload 'wl-draft-send-with-imput-async "im-wl")
;(setq wl-draft-send-func 'wl-draft-send-with-imput-async)

;; テンプレートの設定
(setq wl-template-alist
      '(("default"
	 ("From" . wl-from)
	 ("Organization" . "~/.wl sample")
	 (body . "  ○○です。\n"))		;; 本文
	("report"
	 ("To" . "boss@company.jp")
	 ("Subject" . "報告")
	 (top . "今週の報告です。\n")		;; 本文先頭への挿入
;;	 (file-bottom . "~/work/report.txt")	;; 本文末尾へファイルの挿入
	 )
	))

;; ドラフトバッファの内容により From や Organization などのヘッダを自
;; 動的に変更する
(setq wl-draft-config-alist
      '((reply			;; 返信元のバッファを見る
	 "^To: .*\\(test-notsend-wl@lists.airs.net\\)"
	 (template . "default"))		;; テンプレート
	("^To: .*\\(test-notsend-wl@lists.airs.net\\)"
	 wl-ml-draft-config-func		;; 関数
	 ("From" . wl-from)			;; 変数
	 ("Organization" . "~/.wl sample"))	;; 文字列
	("^Newsgroups: test.*"
	 ("Organization" . "ニュース投稿時の組織名"))
	))
;; ドラフト作成時(返信時)に，自動的にヘッダを変更する
; (add-hook 'wl-mail-setup-hook
;           '(lambda ()
;              (unless wl-draft-reedit    ;; 再編集時は適用しない
;                (wl-draft-config-exec wl-draft-config-alist))))

;; メールの返信時に宛先を付ける方針の設定

;; 下記変数の alist の要素
;; ('返信元に存在するフィールド' .
;;   ('Toフィールド' 'Ccフィールド' 'Newsgroupsフィールド'))

;; "a" (without-argument)では Reply-To や From などで指定された唯一人
;; または唯一つの投稿先に返信する。また，X-ML-Name と Reply-To がつい
;; ているなら Reply-To 宛にする。
; (setq wl-draft-reply-without-argument-list
;       '((("X-ML-Name" "Reply-To") . (("Reply-To") nil nil))
; 	("X-ML-Name" . (("To" "Cc") nil nil))
; 	("Followup-To" . (nil nil ("Followup-To")))
; 	("Newsgroups" . (nil nil ("Newsgroups")))
; 	("Reply-To" . (("Reply-To") nil nil))
; 	("Mail-Reply-To" . (("Mail-Reply-To") nil nil))
; 	("From" . (("From") nil nil))))
; 
;; "C-u a" (with-argument)であれば関係する全ての人・投稿先に返信する。
; (setq wl-draft-reply-with-argument-list
;       '(("Followup-To" . (("From") nil ("Followup-To")))
; 	("Newsgroups" . (("From") nil ("Newsgroups")))
; 	("Mail-Followup-To" . (("Mail-Followup-To") nil ("Newsgroups")))
; 	("From" . (("From") ("To" "Cc") ("Newsgroups")))))


;; X-Face を表示する (要 x-face (and x-face-mule))

(when (and window-system
	   (module-installed-p 'x-face))
  (cond (wl-on-xemacs				;; for XEmacs
	 (autoload 'x-face-xmas-wl-display-x-face "x-face" nil t)
	 (setq wl-highlight-x-face-func
	       'x-face-xmas-wl-display-x-face))
	((module-installed-p 'x-face-mule)	;; for Mule (GNU Emacs)
	 ;; x-face-mule 0.20以後
	 (setq wl-highlight-x-face-func
	       (function
		(lambda (&optional beg end) ; for compatibility
		  (x-face-decode-message-header))))
	 (setq x-face-mule-highlight-x-face-style 'xmas)
	 (require 'x-face-mule)
	 )))

;; 自動リファイルのルール設定
;(setq wl-refile-rule-alist
;      '(
;     	("x-ml-name"
;     	 ("^Wanderlust" . "+wl")
;     	 ("^Elisp" . "+elisp"))
;     	("From"
;     	 ("teranisi@isl.ntt.co.jp" . "+teranisi"))))

;; 自動リファイルしない永続マークを設定
;; 標準では "N" "U" "!" になっており、未読メッセージを自動リファイルし
;; ません。nil ですべてのメッセージが対象になります。
;(setq wl-summary-auto-refile-skip-marks nil)

;; スコア機能の設定
;; wl-score-folder-alist の設定に関わらず必ず "all.SCORE" は使用される。
; (setq wl-score-folder-alist
;       '(("^-comp\\."
; 	 "news.comp.SCORE"
; 	 "news.SCORE")
; 	("^-"
; 	 "news.SCORE")))
;; スコアファイルを置くディレクトリ
; (setq wl-score-files-directory "~/.elmo/")

;;;
;;; end of file
;;;
