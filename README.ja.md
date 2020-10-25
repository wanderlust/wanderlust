Wanderlust - Yet Another Message Interface on Emacsen
=====================================================

Wanderlust は [Emacs][2] 上で動く [IMAP4rev1][1] 対応のメール/ニュース管理システムです。元々は[寺西裕一][3]さんによって開発されました。

ドキュメントは[付属のドキュメント][16] や [Wanderlust on EmacsWiki][4] を参照下さい。

Features
========

  * elisp のみによる実装。

  * [IMAP4rev1][5]、[NNTP][6]、POP([POP3][7]/APOP)、MH 形式のサポート。

  * [Mew][8] っぽい Folder Specification に基づくメッセージへの統一的アクセス。

  * Mew っぽいキーバインドとマーク処理。

  * スレッドと未読管理。

  * 購読フォルダの一覧を表示するフォルダモード。

  * メッセージキャッシュ、 Disconnected Operation。

  * MH 的 Fcc。(Fcc: %Backup も可)。

  * MIME 対応 (by SEMI)。

  * ニュース/メールの送信を統合したドラフト編集。

  * フォルダ一覧のアイコン表示。

  * 大きなパートを取り寄せずに表示 (IMAP4)。

  * メッセージの検索をサーバ側で実行 (IMAP4)。日本語検索も可。

  * 仮想フォルダ。

  * 多い日も安心の、マルチアーカイバ対応圧縮フォルダ。

  * フォルダ中の古い記事を自動的にアーカイブ/削除して整理する expire 機能。

  * 自動リファイル。

  * 定型メッセージの送信に便利なテンプレート機能。

Installation
============

INSTALL.ja を御覧ください。 

メーリングリスト
============

Wanderlust に関する議論はメーリングリストで行われています。

  * 日本語版: [wl@ml.gentei.org][11] (過去ログは news://news.gmane.io/gmane.mail.wanderlust.general.japanese )

 また、英語専用のリストとして

  * 英語版: [wl-en@ml.gentei.org][9] (過去ログは news://news.gmane.io/gmane.mail.wanderlust.general )

もあります (英語版のリストに投稿されたメッセージは日本語版にも配送されます)。

購読するには件名と本文を `subscribe YOUR NAME` をしたメールを[wl-ctl@ml.gentei.org][13] (日本語版) もしくは[wl-en-ctl@ml.gentei.org][14] (英語版) に送ってください。購読をやめるには本文を `unsubscribe` としたメールを、ガイドを得るには本文に `# guide` と書いたメールを送って下さい。


Development
===========

ソースコードのリポジトリは [Github][15] にあります。バグ報告やパッチの送付はメーリングリストへ送ってください。


[1]: http://en.wikipedia.org/wiki/Internet_Message_Access_Protocol

[2]: http://www.gnu.org/software/emacs/

[3]: mailto:teranisi@gohome.org

[4]: http://www.emacswiki.org/emacs/WanderLust

[5]: http://tools.ietf.org/html/rfc3501 "M. Crispin, 'INTERNET MESSAGE ACCESS PROTOCOL - VERSION 4rev1', RFC 3501, 2003"

[6]: http://tools.ietf.org/html/rfc977 "B. Kantor and P. Lapsley, 'Network News Transfer Protocol: A Proposed Standard for the Stream-Based Transmission of News', RFC 977, 1986"

[7]: http://tools.ietf.org/html/rfc1939 "J. Myers, M. Rose, 'Post Office Protocol - Version 3', RFC 1939, 1996"

[8]: http://www.mew.org/ "'Mew - Messaging in the Emacs World', (Copyright © 1994-2018 山本和彦)"

[9]: mailto:wl@ml.gentei.org

[11]: wl@ml.gentei.org

[13]: mailto:wl-ctl@ml.gentei.org

[14]: mailto:wl-en-ctl@ml.gentei.org

[15]: https://github.com/wanderlust/wanderlust

[16]: http://wanderlust.github.io/wl-docs/
