Wanderlust - Yet Another Message Interface on Emacsen
=====================================================

Wanderlust is a mail/news management system with [IMAP4rev1][1] support for
[Emacs][2]. It was originally developed by [Yuuichi Teranishi][3].

For documentation, see also: [Wanderlust on EmacsWiki][4]


Features
========

  * Implementation in elisp only.

  * Support of [IMAP4rev1][5], [NNTP][6], POP([POP3][7]/APOP), MH and Maildir.

  * Integrated access to messages based on Folder Specifications like [Mew][8].

  * Key bindings and mark processing like Mew.

  * Management of threads and unread messages.
 
  * Folder mode to select and edit subscribed folders.

  * Message cache, Disconnected Operation.

  * MH-like Fcc (Fcc: %Backup is possible).

  * Full support of MIME (by SEMI).

  * Draft editing of mail and news as a same interface.

  * Icon based interface for the list of Folder (XEmacs and Emacs 21).

  * Skip fetching of a large message part of MIME(IMAP4).

  * Server side searching (IMAP4), internationalized searching is available.

  * Virtual folder.

  * Compressed folder.

  * Automatic expiration of old messages.

  * Automatic refiling.

  * Draft templates.


Installation
============

See: `Install'


Mailing List
============

You can get information and help via the mailing list at:

  * English: [wl-en@ml.gentei.org][9] ([archives][10])

  * Japanese: [wl@ml.gentei.org][11] ([archives][12])

Emails to the English list are forwarded to the Japanese list.

To subscribe, send an email to [wl-ctl@ml.gentei.org][13] (Japanese) or
[wl-en-ctl@ml.gentei.org][14] (English). Use `subscribe YOUR NAME` as subject
and body. To unsubscribe, use `unsubscribe` as body. To get help, use `# guide`
as body.


Development
===========

The source code repository is available at [Github][15]. Patches that
fix bugs and improve Wanderlust are welcome and can be sent to the
Wanderlust mailing list.

[1]: http://en.wikipedia.org/wiki/Internet_Message_Access_Protocol

[2]: http://www.gnu.org/software/emacs/

[3]: mailto:teranisi@gohome.org

[4]: http://www.emacswiki.org/emacs/WanderLust

[5]: http://tools.ietf.org/html/rfc3501 "M. Crispin, 'INTERNET MESSAGE ACCESS PROTOCOL - VERSION 4rev1', RFC 3501, 2003"

[6]: http://tools.ietf.org/html/rfc977 "B. Kantor and P. Lapsley, 'Network News Transfer Protocol: A Proposed Standard for the Stream-Based Transmission of News', RFC 977, 1986"

[7]: http://tools.ietf.org/html/rfc1939 "J. Myers, M. Rose, 'Post Office Protocol - Version 3', RFC 1939, 1996"

[8]: http://www.mew.org/ "'Mew - Messaging in the Emacs World', (Copyright (C) 1994, 1995, 1996, 1997, 1998 Mew developing team)"

[9]: mailto:wl-en@ml.gentei.org

[10]: http://news.gmane.org/gmane.mail.wanderlust.general

[11]: wl@ml.gentei.org

[12]: http://news.gmane.org/gmane.mail.wanderlust.general.japanese

[13]: mailto:wl-ctl@ml.gentei.org

[14]: mailto:wl-en-ctl@ml.gentei.org

[15]: https://github.com/wanderlust/wanderlust
