;;; elmo-vars.el -- User variables for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

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
(require 'poe)

(eval-when-compile
  (defun-maybe dynamic-link (a))
  (defun-maybe dynamic-call (a b)))

;; IMAP4
(defvar elmo-default-imap4-mailbox "inbox"
  "*Default IMAP4 mailbox.")
(defvar elmo-default-imap4-server "localhost"
  "*Default IMAP4 server.")
(defvar elmo-default-imap4-authenticate-type 'login
  "*Default Authentication type for IMAP4.")
(defvar elmo-default-imap4-user (or (getenv "USER")
				    (getenv "LOGNAME")
				    (user-login-name))
  "*Default username for IMAP4.")
(defvar elmo-default-imap4-port 143
  "*Default Port number of IMAP.")
(defvar elmo-default-imap4-stream-type nil
  "*Default stream type for IMAP4.
Any symbol value of `elmo-network-stream-type-alist'.")
(defvar elmo-imap4-stream-type-alist nil
  "*Stream bindings for IMAP4.
This is taken precedence over `elmo-network-stream-type-alist'.")

;; POP3
(defvar elmo-default-pop3-user (or (getenv "USER")
				   (getenv "LOGNAME")
				   (user-login-name))
  "*Default username for POP3.")
(defvar elmo-default-pop3-server  "localhost"
  "*Default POP3 server.")
(defvar elmo-default-pop3-authenticate-type 'user
  "*Default Authentication type for POP3.")
(defvar elmo-default-pop3-port 110
  "*Default POP3 port.")
(defvar elmo-default-pop3-stream-type nil
  "*Default stream type for POP3.
Any symbol value of `elmo-network-stream-type-alist'.")
(defvar elmo-pop3-stream-type-alist nil
  "*Stream bindings for POP3.
This is taken precedence over `elmo-network-stream-type-alist'.")

;; NNTP
(defvar elmo-default-nntp-server  "localhost"
  "*Default NNTP server.")
(defvar elmo-default-nntp-user nil
  "*Default User of NNTP.  nil means no user authentication.")
(defvar elmo-default-nntp-port 119
  "*Default Port number of NNTP.")
(defvar elmo-default-nntp-stream-type nil
  "*Default stream type for NNTP.
Any symbol value of `elmo-network-stream-type-alist'.")
(defvar elmo-nntp-stream-type-alist nil
  "*Stream bindings for NNTP.
This is taken precedence over `elmo-network-stream-type-alist'.")

;; Local
(defvar elmo-localdir-folder-path "~/Mail"
  "*Local mail folder path.")
(defvar elmo-localnews-folder-path "~/News"
  "*Local news folder path.")
(defvar elmo-maildir-folder-path "~/Maildir"
  "*Maildir folder path.")
(defvar elmo-maildir-list '("\\+~/Maildir")
  "*All Folders that match this list will be treated as Maildir.
Each elements are regexp of folder name (This is obsolete).")

(defvar elmo-msgdb-dir "~/.elmo"
  "*ELMO Message Database path.")
(defvar elmo-passwd-alist-file-name "passwd"
  "*ELMO Password filename.")
(defvar elmo-passwd-life-time nil
  "*Duration of ELMO Password in seconds.  nil means infinity.")
(defvar elmo-warning-threshold 30000
  "*Display warning when the bytes of message exceeds this value.")
(defvar elmo-msg-appended-hook nil
  "A hook called when message is appended to database.")
(defvar elmo-msg-deleted-hook nil
  "A hook called when message is deleted from database.")
(defvar elmo-nntp-post-pre-hook nil
  "A hook called just before the nntp posting.")
(defvar elmo-lang "ja"
  "Language for displayed messages.")

(defvar elmo-mime-charset 'iso-2022-jp)

(defvar elmo-msgdb-mark-filename "mark"
  "Mark database.")
(defvar elmo-msgdb-overview-filename "overview"
  "Overview database.")
(defvar elmo-msgdb-number-filename "number"
  "Message number <=> Message-ID database.")
(defvar elmo-msgdb-location-filename "location"
  "Message number <=> Actual location symbol.")
(defvar elmo-msgdb-seen-filename "seen"
  "Seen message list for append.")
(defvar elmo-msgdb-killed-filename "killed"
  "Deleted messages... contains elmo-killed-msgs-list.")
(defvar elmo-msgdb-validity-filename "validity")
(defvar elmo-msgdb-flist-filename "flist"
  "Folder list cache (for access folder).")
(defvar elmo-msgdb-finfo-filename "finfo"
  "Folder information cache...list of '(filename . '(new unread all)).")
(defvar elmo-msgdb-append-list-filename "append"
  "Appended messages...Structure is same as number-alist.
For disconnected operations.")
(defvar elmo-msgdb-resume-list-filename "resume"
  "Resumed messages.  For disconnected operations.")
(defvar elmo-msgdb-lock-list-filename "lock"
  "Locked messages...list of message-id.
For disconnected operations.")
(defvar elmo-msgdb-global-mark-filename "global-mark"
  "Alist of global mark.")
(defvar elmo-lost+found-folder "+lost+found"
  "Lost and found.")
(defvar elmo-crosspost-alist-filename "crosspost-alist"
  "Alist of crosspost messages.")

(defvar elmo-use-server-diff t
  "Non-nil forces to get unread message information on server.")

(defvar elmo-imap4-disuse-server-flag-mailbox-regexp "^#mh" ; UW imapd
  "Regexp to match IMAP4 mailbox names whose message flags on server should be ignored.
(Except `\\Deleted' flag).")

(defvar elmo-msgdb-extra-fields nil
  "Extra fields for msgdb.")

(defvar elmo-queue-filename "queue"
  "*IMAP pending event queue is saved in this file.")
(defvar elmo-enable-disconnected-operation nil
  "*Enable disconnected operations.")

(defvar elmo-imap4-overview-fetch-chop-length 200
  "*Number of overviews to fetch in one request in imap4.")
(defvar elmo-nntp-overview-fetch-chop-length 200
 "*Number of overviews to fetch in one request in nntp.")
(defvar elmo-localdir-header-chop-length 2048
  "*Number of bytes to get header in one reading from file.")
(defvar elmo-imap4-force-login nil
  "*Non-nil forces to try 'login' if there is no 'auth' capability in imapd.")
(defvar elmo-imap4-use-select-to-update-status nil
  "*Some imapd have to send select command to update status.
(ex. UW imapd 4.5-BETA?).  For these imapd, you must set this variable t.")
(defvar elmo-imap4-use-modified-utf7 nil
  "*Use mofidied UTF-7 (rfc2060) encoding for IMAP4 folder name.")

(defvar elmo-auto-change-plugged 600
  "*Time to expire change plugged state automatically, as the number of seconds.
Don't change plugged state automatically if nil.")

(defvar elmo-plugged-condition 'one
  "*The condition for `elmo-plugged' becomes on.
If `all', when all ports are on.  If `one', when even one port is on.
If `independent', independent port plugged.
If function, return value of function.")

(defvar elmo-plug-on-servers nil)

(defvar elmo-plug-on-exclude-servers
  (list "localhost"
	(system-name)
	(and (string-match "[^.]+" (system-name))
	     (substring (system-name) 0 (match-end 0)))))

(defvar elmo-plugged-alist nil)

(defvar elmo-dop-flush-confirm t
  "*Flush disconnected operations queue with confirmation.")

(defvar elmo-path-sep "/"
  "*Path separator.")
(defvar elmo-plugged t)
(defvar elmo-use-semi nil)
(defvar elmo-no-subject "(No Subject in original.)"
  "*A string used when no subject field exists.")
(defvar elmo-no-from "nobody@nowhere?"
  "*A string used when no from field exists.")

(defvar elmo-multi-divide-number 100000
  "*Multi divider number.")

;;; User variables for elmo-archive.
(defvar elmo-archive-default-type 'zip
  "*Default archiver type.  The value must be a symbol.")

;; database dynamic linking
(defvar elmo-database-dl-module
  (expand-file-name "database.so" exec-directory))

(defvar elmo-database-dl-handle
  (if (and (fboundp 'dynamic-link)
	   (file-exists-p
	    elmo-database-dl-module))
      (if (fboundp 'open-database)
	  t ;;
	(dynamic-link elmo-database-dl-module))))

(if (and elmo-database-dl-handle
	 (integerp elmo-database-dl-handle))
    (dynamic-call "emacs_database_init" elmo-database-dl-handle))

(defvar elmo-use-database (or (featurep 'dbm)
			      (featurep 'gnudbm)
			      (featurep 'berkdb)
			      (featurep 'berkeley-db)
			      ;; static/dl-database
			      (fboundp 'open-database)))

(defvar elmo-date-match (not (boundp 'nemacs-version))
  "Date match is available or not.")

(defconst elmo-spec-alist
  '((?%  . imap4)
    (?-  . nntp)
    (?\+ . localdir)
    (?\* . multi)
    (?\/ . filter)
    (?\$ . archive)
    (?&  . pop3)
    (?=  . localnews)
    (?'  . internal)
    (?|  . pipe)
    (?.  . maildir)))

(defvar elmo-network-stream-type-alist
  '(("!"      ssl       ssl      open-ssl-stream)
    ("!!"     starttls  starttls starttls-open-stream)
    ("!socks" socks     socks    socks-open-network-stream))
  "An alist of (SPEC-STRING SYMBOL FEATURE OPEN-STREAM-FUNCTION).
SPEC-STRING is a string for stream-type spec (it must start with '!').
SYMBOL is a symbol which indicates the name of the stream type.
SYMBOL should be identical in this alist.
FEATURE is a symbol of the feature for OPEN-STREAM-FUNCTION.
OPEN-STREAM-FUNCTION is a function to open network stream.
Arguments for this function are NAME, BUFFER, HOST and SERVICE.")

(defvar elmo-debug nil)
(defconst mmelmo-entity-buffer-name "*MMELMO-BUFFER*")

(defvar elmo-folder-info-hashtb nil
  "Array of folder database information '(max length new unread).")

(defvar elmo-crosspost-message-alist nil
  "List of crosspost message.")

(defvar elmo-cache-expire-default-method "size"
  "Default expiration method.")

(defvar elmo-cache-expire-default-size 30000
  "Cache expiration disk size (Kilo bytes).  This must be float value.")

(defvar elmo-cache-expire-default-age 50
  "Cache expiration age (days).")
(defvar elmo-cache-dirname "cache"
  "Directory name for cache storage.")

(defvar elmo-use-buffer-cache t
  "Use buffer cache.")

(defvar elmo-buffer-cache-size 10
  "*Number of buffer for message cache.")

(defvar elmo-pack-number-check-strict t
  "Pack number strictly.")

(defvar elmo-have-link-count
  (not
   ;; OS/2: EMX always returns the link count "1" :-(
   (or (memq system-type '(OS/2 emx))
       ;; Meadow seems to have pseudo link count.(suggestion by S.YAMAGUCHI)
       (and (eq system-type 'windows-nt) (not (featurep 'meadow)))))
  "Your file system has link count, or not.")

(defvar elmo-weekday-name-en '["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])
(defvar elmo-weekday-name-ja '["日" "月" "火" "水" "木" "金" "土"])
(defvar elmo-weekday-name-fr '["Dim" "Lun" "Mar" "Mer" "Jeu" "Ven" "Sam"])
(defvar elmo-weekday-name-de '["Son" "Mon" "Die" "Mit" "Don" "Fre" "Sam"])

(defvar elmo-msgid-replace-string-alist
  '((":"  . " c")
    ("*"  . " a")
    ("?"  . " q")
    ("<"  . " l")
    (">"  . " g")
    ("\"" . " d")
    ("|"  . " p")
    ("/"  . " s")
    ("\\" . " b")))

(defvar elmo-archive-use-cache nil
  "Use cache in archive folder.")

(defvar elmo-nntp-use-cache t
  "Use cache in nntp folder.")

(defvar elmo-imap4-use-cache t
  "Use cache in imap4 folder.")

(defvar elmo-pop3-use-cache t
  "Use cache in pop3 folder.")

(defvar elmo-localdir-lockfile-list nil)

(defvar elmo-nntp-max-number-precedes-list-active nil
  "Non-nil means max number of msgdb is set as the max number of `list active'.
(Needed for inn 2.3 or later?).")

(defvar elmo-use-killed-list t
  "If non-nil, deleted messages are saved as `killed'
and do not appear again.")

(defvar elmo-pop3-send-command-synchronously nil
  "If non-nil, commands are send synchronously.
If server doesn't accept asynchronous commands, this variable should be
set as non-nil.")

(defvar elmo-hash-maximum-size 4096
  "Maximum size of hash table.")

(defvar elmo-use-decoded-cache (featurep 'xemacs)
  "Use cache of decoded mime charset string.")

(defvar elmo-use-overview-hashtb t
  "Use hash table of overview.")

(defvar elmo-display-progress-threshold 20
  "*Displaying progress gauge if number of messages are more than this value.")

(require 'product)
(product-provide (provide 'elmo-vars) (require 'elmo-version))

;;; elmo-vars.el ends here
