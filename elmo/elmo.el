;;; elmo.el --- Elisp Library for Message Orchestration.

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

(require 'luna)

(require 'elmo-version)			; reduce recursive-load-depth
(require 'elmo-vars)
(require 'elmo-util)
(require 'elmo-msgdb)

(eval-when-compile (require 'cl))

(if (or (featurep 'dbm)
	(featurep 'gnudbm)
	(featurep 'berkdb)
	(featurep 'berkeley-db))
    (require 'elmo-database))

(defcustom elmo-message-fetch-threshold 30000
  "Fetch threshold."
  :type 'integer
  :group 'elmo)

(defcustom elmo-message-fetch-confirm t
  "If non-nil, confirm fetching if message size is larger than
`elmo-message-fetch-threshold'.
Otherwise, entire fetching of the message is aborted without confirmation."
  :type 'boolean
  :group 'elmo)

(defcustom elmo-folder-update-threshold 500
  "Update threshold."
  :type 'integer
  :group 'elmo)

(defcustom elmo-folder-update-confirm t
  "Confirm if update number exceeds `elmo-folder-update-threshold'."
  :type 'boolean
  :group 'elmo)

(defvar elmo-message-displaying nil
  "A global switch to indicate message is displaying or not.")

;;; internal
(defvar elmo-folder-type-alist nil)

(defvar elmo-newsgroups-hashtb nil)

(elmo-define-error 'elmo-error "Error" 'error)
(elmo-define-error 'elmo-open-error "Cannot open" 'elmo-error)
(elmo-define-error 'elmo-authenticate-error "Login failed" 'elmo-open-error)
(elmo-define-error 'elmo-imap4-bye-error "IMAP4 session was terminated" 'elmo-open-error)

(defun elmo-define-folder (prefix backend)
  "Define a folder.
If a folder name begins with PREFIX, use BACKEND."
  (let ((pair (assq prefix elmo-folder-type-alist)))
    (if pair
	(progn
	  (setcar pair prefix)
	  (setcdr pair backend))
      (setq elmo-folder-type-alist (cons (cons prefix backend)
					 elmo-folder-type-alist)))))

(defmacro elmo-folder-type (name)
  "Get folder type from NAME string."
  (` (and (stringp (, name))
	  (cdr (assoc (string-to-char (, name)) elmo-folder-type-alist)))))

;;; ELMO folder
;; A elmo folder provides uniformed (orchestrated) access
;; to the internet messages.
(eval-and-compile
  (luna-define-class elmo-folder () (type   ; folder type symbol.
				     name   ; orignal folder name string.
				     prefix ; prefix for folder name
				     path   ; directory path for msgdb.
				     msgdb  ; msgdb (may be nil).
				     killed-list  ; killed list.
				     persistent   ; non-nil if persistent.
				     message-modified ; message is modified.
				     mark-modified    ; mark is modified.
				     process-duplicates  ; read or hide
				     biff   ; folder for biff
				     ))
  (luna-define-internal-accessors 'elmo-folder))

(luna-define-generic elmo-folder-initialize (folder name)
  ;; Initialize a FOLDER structure with NAME."
  )

(defmacro elmo-folder-send (folder message &rest args)
  "Let FOLDER receive the MESSAGE with ARGS."
  (` (luna-send (, folder) (, message) (, folder) (,@ args))))

;;;###autoload
(defun elmo-make-folder (name &optional non-persistent)
  "Make an ELMO folder structure specified by NAME.
If optional argument NON-PERSISTENT is non-nil, the folder msgdb is not saved."
  (let ((type (elmo-folder-type name))
	prefix split class folder original)
    (setq original (elmo-string name))
    (if type
	(progn
	  (setq prefix (substring name 0 1))
	  (setq name (substring name 1)))
      (setq type (intern (car (setq split (split-string name ":")))))
      (if (> (length split) 2)
	  (setq name (substring name (+ 1 (length (car split)))))
	(error "Error in folder name `%s'" original))
      (setq prefix (concat (car split) ":")))
    (setq class (format "elmo-%s" (symbol-name type)))
    (require (intern class))
    (setq folder (luna-make-entity (intern (concat class "-folder"))
				   :type type
				   :prefix prefix
				   :name original
				   :persistent (not non-persistent)))
    (save-match-data
      (elmo-folder-send folder 'elmo-folder-initialize name))))

;; Note that this function is for internal use only.
(luna-define-generic elmo-folder-msgdb (folder)
  "Return the msgdb of FOLDER (on-demand loading).
\(For internal use only.\)")

(luna-define-method elmo-folder-msgdb ((folder elmo-folder))
  (or (elmo-folder-msgdb-internal folder)
      (elmo-folder-set-msgdb-internal folder
				      (elmo-msgdb-load folder))))
(luna-define-generic elmo-folder-open (folder &optional load-msgdb)
  "Open and setup (load saved status) FOLDER.
If optional LOAD-MSGDB is non-nil, msgdb is loaded.
\(otherwise, msgdb is loaded on-demand)")

(luna-define-generic elmo-folder-open-internal (folder)
  "Open FOLDER (without loading saved folder status).")

(luna-define-generic elmo-folder-check (folder)
  "Check the FOLDER to obtain newest information at the next list operation.")

(luna-define-generic elmo-folder-clear (folder &optional keep-killed)
  "Clear FOLDER to the initial state.
If optional KEEP-KILLED is non-nil, killed-list is not cleared.")

(luna-define-generic elmo-folder-commit (folder)
  "Save current status of FOLDER.")

(luna-define-generic elmo-folder-close (folder)
  "Close, save and clearnup FOLDER.")

(luna-define-generic elmo-folder-close-internal (folder)
  "Close FOLDER (without saving folder status).")

(luna-define-generic elmo-folder-plugged-p (folder)
  "Returns t if FOLDER is plugged.")

(luna-define-generic elmo-folder-set-plugged (folder plugged &optional add)
  "Set FOLDER as plugged.")

(luna-define-generic elmo-net-port-info (folder)
  "Get port information of FOLDER.")

(luna-define-generic elmo-folder-use-flag-p (folder)
  "Returns t if FOLDER treats unread/important flag itself.")

(luna-define-generic elmo-folder-diff (folder)
  "Get diff of FOLDER.
Return value is cons cell or list:
 - a cons cell (new . all)
 - a list (new unread all)")

(luna-define-generic elmo-folder-status (folder)
  "Returns a cons cell of (MAX-NUMBER . MESSAGES) in the FOLDER.")

(luna-define-generic elmo-folder-reserve-status-p (folder)
  "If non-nil, the folder should not close folder after `elmo-folder-status'.")

(luna-define-generic elmo-folder-list-messages (folder &optional visible-only
						       in-msgdb)
  "Return a list of message numbers contained in FOLDER.
If optional VISIBLE-ONLY is non-nil, killed messages are not listed.
If second optional IN-MSGDB is non-nil, only messages in the msgdb are listed.")
(luna-define-method elmo-folder-list-messages ((folder elmo-folder)
					       &optional visible-only in-msgdb)
  (let ((list (if in-msgdb
		  t
		(elmo-folder-list-messages-internal folder visible-only))))
    (setq list
	  (if (listp list)
	      list
	    ;; Use current list.
	    (elmo-msgdb-list-messages (elmo-folder-msgdb folder))))
    (if visible-only
	(elmo-living-messages list (elmo-folder-killed-list-internal folder))
      list)))

(luna-define-generic elmo-folder-list-unreads (folder)
  "Return a list of unread message numbers contained in FOLDER.")
(luna-define-generic elmo-folder-list-importants (folder)
  "Return a list of important message numbers contained in FOLDER.")
(luna-define-generic elmo-folder-list-answereds (folder)
  "Return a list of answered message numbers contained in FOLDER.")

;; TODO: Should reconsider the structure of global mark.
(defun elmo-folder-list-messages-with-global-mark (folder mark)
  (let (entity msgs)
    (dolist (mark-pair (or elmo-msgdb-global-mark-alist
			   (setq elmo-msgdb-global-mark-alist
				 (elmo-object-load
				  (expand-file-name
				   elmo-msgdb-global-mark-filename
				   elmo-msgdb-directory)))))
      (if (and (string= mark (cdr mark-pair))
	       (setq entity
		     (elmo-msgdb-overview-get-entity (car mark-pair)
						     (elmo-folder-msgdb
						      folder))))
	  (setq msgs (cons (elmo-msgdb-overview-entity-get-number entity)
			   msgs))))
    msgs))

(luna-define-generic elmo-folder-list-flagged (folder flag &optional in-msgdb)
  "List messages in the FOLDER with FLAG.
FLAG is a symbol which is one of the following:
  `new'        (new messages)
  `unread'     (unread messages (new messages are included))
  `answered'   (answered or forwarded)
  `important'  (marked as important)
'sugar' flags:
  `read'       (not unread)
  `digest'     (unread + important)
  `any'        (digest + answered)

If optional IN-MSGDB is non-nil, retrieve flag information from msgdb.")

(luna-define-method elmo-folder-list-flagged ((folder elmo-folder) flag
					      &optional in-msgdb)
  ;; Currently, only in-msgdb is implemented.
  (elmo-msgdb-list-flagged (elmo-folder-msgdb folder) flag))

(luna-define-method elmo-folder-list-unreads ((folder elmo-folder))
  (elmo-folder-list-flagged folder 'unread))

(luna-define-method elmo-folder-list-importants ((folder elmo-folder))
  (elmo-folder-list-flagged folder 'important))

(luna-define-method elmo-folder-list-answereds ((folder elmo-folder))
  (elmo-folder-list-flagged folder 'answered))

(luna-define-generic elmo-folder-list-messages-internal (folder &optional
								visible-only)
  ;; Return a list of message numbers contained in FOLDER.
  ;; Return t if the message list is not available.
  )

(luna-define-generic elmo-folder-list-subfolders (folder &optional one-level)
  "Returns a list of subfolders contained in FOLDER.
If optional argument ONE-LEVEL is non-nil, only children of FOLDER is returned.
\(a folder which have children is returned as a list\)
Otherwise, all descendent folders are returned.")

(luna-define-generic elmo-folder-have-subfolder-p (folder)
  "Return non-nil when FOLDER has subfolders.")

(luna-define-generic elmo-folder-exists-p (folder)
  "Returns non-nil when FOLDER exists.")

(luna-define-generic elmo-folder-creatable-p (folder)
  "Returns non-nil when FOLDER is creatable.")

(luna-define-generic elmo-folder-writable-p (folder)
  "Returns non-nil when FOLDER is writable.")

(luna-define-generic elmo-folder-persistent-p (folder)
  "Return non-nil when FOLDER is persistent.")

(luna-define-generic elmo-folder-create (folder)
  "Create a FOLDER.")

(luna-define-generic elmo-message-deletable-p (folder number)
  "Returns non-nil when the message in the FOLDER with NUMBER is deletable.")

(luna-define-generic elmo-folder-delete (folder)
  "Delete FOLDER completely.")

(luna-define-generic elmo-folder-rename (folder new-name)
  "Rename FOLDER to NEW-NAME (string).")

(luna-define-generic elmo-folder-delete-messages (folder numbers)
  "Delete messages.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be deleted.")

(luna-define-generic elmo-folder-search (folder condition &optional numbers)
  "Search and return list of message numbers.
FOLDER is the ELMO folder structure.
CONDITION is a condition structure for searching.
If optional argument NUMBERS is specified and is a list of message numbers,
messages are searched from the list.")

(luna-define-generic elmo-message-match-condition (folder number
							  condition
							  numbers)
  "Return non-nil when the message in the FOLDER with NUMBER is matched.
CONDITION is a condition structure for testing.
NUMBERS is a list of message numbers,
use to be test for \"last\" and \"first\" predicates.")

(luna-define-generic elmo-folder-msgdb-create (folder numbers flag-table)
  "Create a message database (implemented in each backends).
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to create msgdb.
FLAG-TABLE is a hashtable of message-id and flag.")

(luna-define-generic elmo-folder-unmark-important (folder
						   numbers
						   &optional ignore-flags)
  "Un-mark messages as important.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be processed.
If IGNORE-FLAGS is non-nil, folder flags are not updated.")

(luna-define-generic elmo-folder-mark-as-important (folder
						    numbers
						    &optional ignore-flags)
  "Mark messages as important.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be processed.
If IGNORE-FLAGS is non-nil, folder flags are not updated.")

(luna-define-generic elmo-folder-unmark-read (folder numbers
						     &optional ignore-flags)
  "Un-mark messages as read.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be processed.
If IGNORE-FLAGS is non-nil, folder flags are not updated.")

(luna-define-generic elmo-folder-mark-as-read (folder numbers
						      &optional ignore-flags)
  "Mark messages as read.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be processed.
If IGNORE-FLAGS is non-nil, folder flags are not updated.")

(luna-define-generic elmo-folder-unmark-answered (folder numbers)
  "Un-mark messages as answered.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be processed.")

(luna-define-generic elmo-folder-mark-as-answered (folder numbers)
  "Mark messages as answered.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be processed.")

(luna-define-generic elmo-folder-append-buffer (folder &optional flag
						       number)
  "Append current buffer as a new message.
FOLDER is the destination folder(ELMO folder structure).
FLAG is the status of appended message.
If optional argument NUMBER is specified, the new message number is set
\(if possible\).")

(luna-define-generic elmo-folder-append-messages (folder
						  src-folder
						  numbers
						  &optional
						  same-number)
  "Append messages from folder.
FOLDER is the ELMO folder structure.
Caller should make sure FOLDER is `writable'.
(Can be checked with `elmo-folder-writable-p').
SRC-FOLDER is the source ELMO folder structure.
NUMBERS is the message numbers to be appended in the SRC-FOLDER.
If second optional argument SAME-NUMBER is specified,
message number is preserved (if possible).")

(luna-define-generic elmo-folder-pack-numbers (folder)
  "Pack message numbers of FOLDER.")

(luna-define-generic elmo-folder-update-number (folder)
  "Update number of FOLDER.")

(luna-define-generic elmo-folder-diff-async (folder)
  "Get diff of FOLDER asynchronously.")

(luna-define-generic elmo-folder-expand-msgdb-path (folder)
  "Expand path for FOLDER.")

(luna-define-generic elmo-folder-get-primitive-list (folder)
  "Get primitive folder structure list contained in FOLDER.")

(luna-define-generic elmo-folder-contains-type (folder type)
  "Returns t if FOLDER contains TYPE.")

(luna-define-generic elmo-folder-local-p (folder)
  "Returns t if FOLDER is local.")

(luna-define-generic elmo-folder-message-file-p (folder)
  "Returns t if all messages in the FOLDER are files.")

;;; Message methods.
(luna-define-generic elmo-message-use-cache-p (folder number)
  "Returns t if the message in the FOLDER with NUMBER uses cache.")

(luna-define-generic elmo-message-file-name (folder number)
  "Return the file name of a message specified by FOLDER and NUMBER.")

;;; For archive

;;; Use original file
(luna-define-generic elmo-folder-message-file-number-p (folder)
  "Return t if the file name in the FOLDER is the message number.")

(luna-define-generic elmo-folder-message-file-directory (folder)
  "Return the directory of the message files of FOLDER.")

;;; Use temporary file
(luna-define-generic elmo-folder-message-make-temp-file-p (folder)
  "Return t if the messages in the FOLDER makes local temporary file.")

(luna-define-generic elmo-folder-message-make-temp-files (folder
							  numbers
							  &optional
							  start-number)
  "Make a new temporary files from the messages in the FOLDER with NUMBERS.
If START-NUMBER is specified, temporary files begin from the number.
Otherwise, same number is used for temporary files.
Return newly created temporary directory name which contains temporary files.")

(luna-define-generic elmo-message-file-p (folder number)
  "Return t if message in the FOLDER with NUMBER is a file.")

(luna-define-generic elmo-find-fetch-strategy
  (folder entity &optional ignore-cache)
;; Returns the message fetching strategy suitable for the message.
;; FOLDER is the ELMO folder structure.
;; ENTITY is the overview entity of the message in the folder.
;; If optional argument IGNORE-CACHE is non-nil, cache is ignored.
;; Returned value is a elmo-fetch-strategy object.
;; If return value is nil, message should not be nil.
  )

(defmacro elmo-make-fetch-strategy (entireness
				    &optional
				    use-cache
				    save-cache
				    cache-path)
;; Make elmo-message-fetching strategy.
;; ENTIRENESS is 'entire or 'section.
;; 'entire means fetch message entirely at once.
;; 'section means fetch message section by section.
;; If optional USE-CACHE is non-nil, existing cache is used and otherwise,
;; existing cache is thrown away.
;; If SAVE-CACHE is non-nil, fetched message is saved.
;; CACHE-PATH is the cache path to be used as a message cache file.
  (` (vector (, entireness)
	     (, use-cache) (, save-cache) (, cache-path))))

(defmacro elmo-fetch-strategy-entireness (strategy)
  ;; Return entireness of STRATEGY.
  (` (aref (, strategy) 0)))

(defmacro elmo-fetch-strategy-use-cache (strategy)
  ;; Return use-cache of STRATEGY.
  (` (aref (, strategy) 1)))

(defmacro elmo-fetch-strategy-save-cache (strategy)
  ;; Return save-cache of STRATEGY.
  (` (aref (, strategy) 2)))

(defmacro elmo-fetch-strategy-cache-path (strategy)
  ;;  Return cache-path of STRATEGY.
  (` (aref (, strategy) 3)))

(luna-define-method elmo-find-fetch-strategy
  ((folder elmo-folder) entity &optional ignore-cache)
  (let (cache-file size message-id number)
    (setq size (elmo-msgdb-overview-entity-get-size entity))
    (setq message-id (elmo-msgdb-overview-entity-get-id entity))
    (setq number (elmo-msgdb-overview-entity-get-number entity))
    (setq cache-file (elmo-file-cache-get message-id))
    (setq ignore-cache (or ignore-cache
			   (null (elmo-message-use-cache-p folder number))))
    (if (or ignore-cache
	    (null (elmo-file-cache-status cache-file)))
	;; No cache or ignore-cache.
	(if (and (not (elmo-folder-local-p folder))
		 elmo-message-fetch-threshold
		 (integerp size)
		 (>= size elmo-message-fetch-threshold)
		 (or (not elmo-message-fetch-confirm)
		     (not (prog1 (y-or-n-p
				  (format "Fetch entire message(%dbytes)? "
					  size))
			    (message "")))))
	    ;; Don't fetch message at all.
	    nil
	  ;; Don't use existing cache and fetch entire message at once.
	  (elmo-make-fetch-strategy
	   'entire nil
	   (elmo-message-use-cache-p folder number)
	   (elmo-file-cache-path cache-file)))
      ;; Cache exists.
      (if (not ignore-cache)
	  (elmo-make-fetch-strategy
	   'entire
	   ;; ...But ignore current section cache and re-fetch
	   ;; if section cache.
	   (not (eq (elmo-file-cache-status cache-file) 'section))
	   ;; Save cache.
	   (elmo-message-use-cache-p folder number)
	   (elmo-file-cache-path cache-file))))))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-folder) &optional visible-only)
  t)

(defun elmo-folder-encache (folder numbers &optional unread)
  "Encache messages in the FOLDER with NUMBERS.
If UNREAD is non-nil, messages are not marked as read."
  (dolist (number numbers)
    (elmo-message-encache folder number unread)))

(luna-define-generic elmo-message-encache (folder number &optional read)
  "Encache message in the FOLDER with NUMBER.
If READ is non-nil, message is marked as read.")

(luna-define-method elmo-message-encache ((folder elmo-folder) number
					  &optional read)
  (elmo-message-fetch
   folder number
   (elmo-make-fetch-strategy 'entire
			     nil ;use-cache
			     t   ;save-cache
			     (elmo-file-cache-get-path
			      (elmo-message-field
			       folder number 'message-id)))
   nil nil (not read)))

(luna-define-generic elmo-message-fetch (folder number strategy
						&optional
						section
						outbuf
						unread)
  "Fetch a message and return as a string.
FOLDER is the ELMO folder structure.
NUMBER is the number of the message in the FOLDER.
STRATEGY is the message fetching strategy.
If optional argument SECTION is specified, only the SECTION of the message
is fetched (if possible).
If second optional argument OUTBUF is specified, fetched message is
inserted to the buffer and returns t if fetch was ended successfully.
If third optional argument UNREAD is non-nil, message is not marked as read.
Returns non-nil if fetching was succeed.")

(luna-define-generic elmo-message-fetch-with-cache-process (folder
							    number strategy
							    &optional
							    section
							    unread)
  "Fetch a message into current buffer with cache process.
FOLDER is the ELMO folder structure.
NUMBER is the number of the message in the FOLDER.
STRATEGY is the message fetching strategy.
If optional argument SECTION is specified, only the SECTION of the message
is fetched (if possible).
If second optional argument UNREAD is non-nil, message is not marked as read.
Returns non-nil if fetching was succeed.")

(luna-define-generic elmo-message-fetch-internal (folder number strategy
							 &optional
							 section
							 unread)
  "Fetch a message into current buffer.
FOLDER is the ELMO folder structure.
NUMBER is the number of the message in the FOLDER.
STRATEGY is the message fetching strategy.
If optional argument SECTION is specified, only the SECTION of the message
is fetched (if possible).
If second optional argument UNREAD is non-nil, message is not marked as read.
Returns non-nil if fetching was succeed.")

(luna-define-generic elmo-message-fetch-field (folder number field)
  "Fetch a message field value.
FOLDER is the ELMO folder structure.
NUMBER is the number of the message in the FOLDER.
FIELD is a symbol of the field name.")

(luna-define-generic elmo-message-folder (folder number)
  "Get primitive folder of the message.")

(luna-define-generic elmo-folder-process-crosspost (folder)
  "Process crosspost for FOLDER.
If NUMBER-ALIST is set, it is used as number-alist.
Return a cons cell of (NUMBER-CROSSPOSTS . NEW-MARK-ALIST).")

(luna-define-generic elmo-folder-append-msgdb (folder append-msgdb)
  "Append  APPEND-MSGDB to the current msgdb of the folder.")

(luna-define-generic elmo-folder-newsgroups (folder)
  "Return list of newsgroup name of FOLDER.")

(luna-define-generic elmo-folder-search-requires-msgdb-p (folder condition)
  "Return non-nil if searching in FOLDER by CONDITION requires msgdb fetch.")

(defun elmo-folder-search-requires-msgdb-p-internal (folder condition)
  (if (listp condition)
      (or (elmo-folder-search-requires-msgdb-p-internal
	   folder (nth 1 condition))
	  (elmo-folder-search-requires-msgdb-p-internal
	   folder (nth 2 condition)))
    (and (not (string= (elmo-filter-key condition) "last"))
	 (not (string= (elmo-filter-key condition) "first")))))

(luna-define-method elmo-folder-search-requires-msgdb-p ((folder elmo-folder)
							 condition)
  (elmo-folder-search-requires-msgdb-p-internal folder condition))

(luna-define-method elmo-folder-newsgroups ((folder elmo-folder))
  nil)

(luna-define-method elmo-folder-open ((folder elmo-folder)
				      &optional load-msgdb)
  (elmo-generic-folder-open folder load-msgdb))

(defun elmo-generic-folder-open (folder load-msgdb)
  (let ((inhibit-quit t))
    (if load-msgdb (elmo-folder-msgdb folder))
    (elmo-folder-set-killed-list-internal
     folder
     (elmo-msgdb-killed-list-load (elmo-folder-msgdb-path folder))))
  (elmo-folder-open-internal folder))

(luna-define-method elmo-folder-open-internal ((folder elmo-folder))
  nil ; default is do nothing.
  )

(luna-define-method elmo-folder-check ((folder elmo-folder))
  nil) ; default is noop.

(luna-define-method elmo-folder-commit ((folder elmo-folder))
  (elmo-generic-folder-commit folder))

(defun elmo-generic-folder-commit (folder)
  (when (elmo-folder-persistent-p folder)
    (when (elmo-folder-message-modified-internal folder)
      (elmo-msgdb-overview-save
       (elmo-folder-msgdb-path folder)
       (elmo-msgdb-get-overview (elmo-folder-msgdb folder)))
      (elmo-msgdb-number-save
       (elmo-folder-msgdb-path folder)
       (elmo-msgdb-get-number-alist (elmo-folder-msgdb folder)))
      (elmo-folder-set-info-max-by-numdb
       folder
       (elmo-folder-list-messages folder nil 'in-msgdb))
      (elmo-folder-set-message-modified-internal folder nil)
      (elmo-msgdb-killed-list-save
       (elmo-folder-msgdb-path folder)
       (elmo-folder-killed-list-internal folder)))
    (when (elmo-folder-mark-modified-internal folder)
      (elmo-msgdb-mark-save
       (elmo-folder-msgdb-path folder)
       (elmo-msgdb-get-mark-alist (elmo-folder-msgdb folder)))
      (elmo-folder-set-mark-modified-internal folder nil))))

(luna-define-method elmo-folder-close-internal ((folder elmo-folder))
  ;; do nothing.
  )

(luna-define-method elmo-folder-close ((folder elmo-folder))
  (elmo-generic-folder-close folder)
  (elmo-folder-close-internal folder))

(defun elmo-generic-folder-close (folder)
  (elmo-folder-commit folder)
  (elmo-folder-set-msgdb-internal folder nil)
  (elmo-folder-set-killed-list-internal folder nil))

(luna-define-method elmo-folder-plugged-p ((folder elmo-folder))
  t) ; default is plugged.

(luna-define-method elmo-folder-set-plugged ((folder elmo-folder) plugged
					     &optional add)
  nil) ; default is do nothing.

(luna-define-method elmo-folder-use-flag-p ((folder elmo-folder))
  nil) ; default is no flag.

(luna-define-method elmo-folder-persistent-p ((folder elmo-folder))
  (elmo-folder-persistent-internal folder))

(luna-define-method elmo-folder-creatable-p ((folder elmo-folder))
  nil) ; default is not creatable.

(luna-define-method elmo-folder-writable-p ((folder elmo-folder))
  nil) ; default is not writable.

(luna-define-method elmo-folder-delete ((folder elmo-folder))
  (when (yes-or-no-p (format "Delete msgdb of \"%s\"? "
			     (elmo-folder-name-internal folder)))
    (elmo-msgdb-delete-path folder)
    t))

(luna-define-method elmo-folder-rename ((folder elmo-folder) new-name)
  (let* ((new-folder (elmo-make-folder new-name)))
    (unless (eq (elmo-folder-type-internal folder)
		(elmo-folder-type-internal new-folder))
      (error "Not same folder type"))
    (if (or (file-exists-p (elmo-folder-msgdb-path new-folder))
	    (elmo-folder-exists-p new-folder))
	(error "Already exists folder: %s" new-name))
    (elmo-folder-send folder 'elmo-folder-rename-internal new-folder)
    (elmo-msgdb-rename-path folder new-folder)))

(defsubst elmo-folder-search-fast (folder condition numbers)
  "Search and return list of message numbers.
Return t if CONDITION is not treated.
FOLDER is the ELMO folder structure.
CONDITION is a condition structure for searching.
NUMBERS is a list of message numbers, messages are searched from the list."
  (if (and numbers
	   (vectorp condition))
      (cond
       ((string= (elmo-filter-key condition) "flag")
	(let ((msgdb (elmo-folder-msgdb folder)))
	  ;; msgdb should be synchronized at this point.
	  (cond
	   ((string= (elmo-filter-value condition) "unread")
	    (elmo-folder-list-unreads folder))
	   ((string= (elmo-filter-value condition) "important")
	    (elmo-folder-list-importants folder))
	   ((string= (elmo-filter-value condition) "answered")
	    (elmo-folder-list-answereds folder))
	   ((string= (elmo-filter-value condition) "digest")
	    (nconc (elmo-folder-list-unreads folder)
		   (elmo-folder-list-importants folder)))
	   ((string= (elmo-filter-value condition) "any")
	    (nconc (elmo-folder-list-unreads folder)
		   (elmo-folder-list-importants folder)
		   (elmo-folder-list-answereds folder))))))
       ((member (elmo-filter-key condition) '("first" "last"))
	(let ((len (length numbers))
	      (lastp (string= (elmo-filter-key condition) "last"))
	      (value (string-to-number (elmo-filter-value condition))))
	  (when (eq (elmo-filter-type condition) 'unmatch)
	    (setq lastp (not lastp)
		  value  (- len value)))
	  (if lastp
	      (nthcdr (max (- len value) 0) numbers)
	    (when (> value 0)
	      (let* ((numbers (copy-sequence numbers))
		     (last (nthcdr (1- value) numbers)))
		(when last
		  (setcdr last nil))
		numbers)))))
       (t
	t))
    t))

(luna-define-method elmo-folder-search ((folder elmo-folder)
					condition
					&optional numbers)
  (let ((numbers (or numbers (elmo-folder-list-messages folder)))
	results)
    (if (listp (setq results (elmo-folder-search-fast folder
						      condition
						      numbers)))
	results
      (let ((msgdb (elmo-folder-msgdb folder))
	    (len (length numbers))
	    matched)
	(when (> len elmo-display-progress-threshold)
	  (elmo-progress-set 'elmo-folder-search len "Searching..."))
	(unwind-protect
	    (dolist (number numbers)
	      (let (result)
		(setq result (elmo-msgdb-match-condition
			      msgdb
			      condition
			      number
			      numbers))
		(when (elmo-filter-condition-p result)
		  (setq result (elmo-message-match-condition
				folder
				number
				condition
				numbers)))
		(when result
		  (setq matched (cons number matched))))
	      (elmo-progress-notify 'elmo-folder-search))
	  (elmo-progress-clear 'elmo-folder-search))
	(nreverse matched)))))

(luna-define-method elmo-message-match-condition ((folder elmo-folder)
						  number condition
						  numbers)
  (let ((filename (cond
		   ((elmo-message-file-name folder number))
		   ((let* ((cache (elmo-file-cache-get
				   (elmo-message-field folder number
						       'message-id)))
			   (cache-path (elmo-file-cache-path cache)))
		      (when (and cache-path
				 (not (elmo-cache-path-section-p cache-path)))
			cache-path))))))
    (when (and filename
	       (file-readable-p filename))
      (with-temp-buffer
	(insert-file-contents-as-binary filename)
	(elmo-set-buffer-multibyte default-enable-multibyte-characters)
	;; Should consider charset?
	(decode-mime-charset-region (point-min) (point-max) elmo-mime-charset)
	(elmo-buffer-field-condition-match condition number numbers)))))

(luna-define-method elmo-folder-pack-numbers ((folder elmo-folder))
  nil) ; default is noop.

(luna-define-method elmo-folder-update-number ((folder elmo-folder))
  nil) ; default is noop.

(luna-define-method elmo-folder-message-file-p ((folder elmo-folder))
  nil) ; default is not file.

(luna-define-method elmo-folder-message-file-number-p ((folder elmo-folder))
  nil) ; default is not number.

(luna-define-method elmo-folder-message-make-temp-file-p ((folder elmo-folder))
  nil) ; default is not make temp file.

(luna-define-method elmo-message-file-name ((folder elmo-folder)
						   number)
  nil) ; default is no name.

(luna-define-method elmo-folder-local-p ((folder elmo-folder))
  t)   ; default is local.

(luna-define-method elmo-folder-have-subfolder-p ((folder elmo-folder))
  t)

;;; Folder info
;; Folder info is a message number information cache (hashtable)
(defsubst elmo-folder-get-info (folder &optional hashtb)
  "Return FOLDER info from HASHTB (default is `elmo-folder-info-hashtb')."
  (elmo-get-hash-val (elmo-folder-name-internal folder)
		     (or hashtb elmo-folder-info-hashtb)))

(defun elmo-folder-set-info-hashtb (folder max numbers &optional new unread)
  "Set FOLDER info (means MAX, NUMBERS, NEW and UNREAD)."
  (let ((info (elmo-folder-get-info folder)))
    (when info
      (or new     (setq new     (nth 0 info)))
      (or unread  (setq unread  (nth 1 info)))
      (or numbers (setq numbers (nth 2 info)))
      (or max     (setq max     (nth 3 info))))
    (elmo-set-hash-val (elmo-folder-name-internal folder)
		       (list new unread numbers max)
		       elmo-folder-info-hashtb)))

(defun elmo-folder-set-info-max-by-numdb (folder numbers)
  "Set FOLDER info by MSGDB-NUMBER in msgdb."
  (elmo-folder-set-info-hashtb
   folder
   (or (car (sort numbers '>)) 0)
   nil ;;(length num-db)
   ))

(defun elmo-folder-get-info-max (folder)
  "Return max number of FODLER from folder info."
  (nth 3 (elmo-folder-get-info folder)))

(defun elmo-folder-get-info-length (folder)
  "Return length of FODLER from folder info."
  (nth 2 (elmo-folder-get-info folder)))

(defun elmo-folder-get-info-unread (folder)
  "Return unread of FODLER from folder info."
  (nth 1 (elmo-folder-get-info folder)))

(defun elmo-folder-info-make-hashtb (info-alist hashtb)
  "Setup folder info hashtable by INFO-ALIST on HASHTB."
  (let* ((hashtb (or hashtb
		     (elmo-make-hash (length info-alist)))))
    (mapcar
     (lambda (x)
       (let ((info (cadr x)))
	 (and (intern-soft (car x) hashtb)
	      (elmo-set-hash-val (car x)
				 (list (nth 2 info)   ;; new
				       (nth 3 info)   ;; unread
				       (nth 1 info)   ;; length
				       (nth 0 info))  ;; max
				 hashtb))))
     info-alist)
    (setq elmo-folder-info-hashtb hashtb)))

(defsubst elmo-diff-new (diff)
  (car diff))

(defsubst elmo-diff-unread (diff)
  (when (consp (cdr diff))
    (nth 1 diff)))

(defsubst elmo-diff-all (diff)
  (if (consp (cdr diff))
      (nth 2 diff)
    (cdr diff)))

(defsubst elmo-strict-folder-diff (folder)
  "Return folder diff information strictly from FOLDER."
  (let ((in-db (sort (elmo-msgdb-list-messages (elmo-folder-msgdb folder))
		     '<))
	(in-folder  (elmo-folder-list-messages folder))
	append-list delete-list diff)
    (cons (if (equal in-folder in-db)
	      0
	    (setq diff (elmo-list-diff in-folder in-db nil))
	    (setq append-list (car diff))
	    (setq delete-list (cadr diff))
	    (if append-list
		(length append-list)
	      (if delete-list
		  (- 0 (length delete-list))
		0)))
	  (length in-folder))))

(luna-define-method elmo-folder-diff ((folder elmo-folder))
  (elmo-generic-folder-diff folder))

(defun elmo-generic-folder-diff (folder)
  (if (elmo-string-match-member (elmo-folder-name-internal folder)
				elmo-strict-diff-folder-list)
      (elmo-strict-folder-diff folder)
    (let ((cached-in-db-max (elmo-folder-get-info-max folder))
	  (in-folder (elmo-folder-status folder))
	  (in-db t)
	  unsync messages
	  in-db-max)
      (if (not cached-in-db-max)
	  (let ((number-list (elmo-folder-list-messages folder
							nil 'in-msgdb)))
	    ;; No info-cache.
	    (setq in-db (sort number-list '<))
	    (setq in-db-max (or (nth (max 0 (1- (length in-db))) in-db)
				0))
	    (elmo-folder-set-info-hashtb folder in-db-max nil))
	(setq in-db-max cached-in-db-max))
      (setq unsync (if (and in-db (car in-folder))
		       (- (car in-folder) in-db-max)
		     (if (and in-folder (null in-db))
			 (cdr in-folder)
		       (car in-folder))))
      (setq messages (cdr in-folder))
      (if (and unsync messages (> unsync messages))
	  (setq unsync messages))
      (cons (or unsync 0) (or messages 0)))))

(defvar elmo-folder-diff-async-callback nil)
(defvar elmo-folder-diff-async-callback-data nil)

(luna-define-method elmo-folder-diff-async ((folder elmo-folder))
  (and elmo-folder-diff-async-callback
       (funcall elmo-folder-diff-async-callback
		folder
		(elmo-folder-diff folder))))

(luna-define-method elmo-folder-get-primitive-list ((folder elmo-folder))
  (list folder))

(luna-define-method elmo-folder-contains-type ((folder elmo-folder) type)
  (eq (elmo-folder-type-internal folder) type))

(luna-define-method elmo-folder-append-messages ((folder elmo-folder)
						 src-folder
						 numbers
						 &optional
						 same-number)
  (elmo-generic-folder-append-messages folder src-folder numbers
				       same-number))

(defun elmo-generic-folder-append-messages (folder src-folder numbers
						   same-number)
  (let ((src-msgdb-exists (not (zerop (elmo-folder-length src-folder))))
	unseen table flag mark
	succeed-numbers failure cache id)
    (setq table (elmo-flag-table-load (elmo-folder-msgdb-path folder)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (while numbers
	(setq failure nil
	      id (and src-msgdb-exists
		      (elmo-message-field src-folder (car numbers)
					  'message-id))
	      mark (and src-msgdb-exists
			(elmo-message-mark src-folder (car numbers)))
	      flag (and id
			(cond
			 ((null mark) 'read)
			 ((member mark (elmo-msgdb-answered-marks))
			  'answered)
			 ;;
			 ((not (member mark (elmo-msgdb-unread-marks)))
			  'read))))
	(condition-case nil
	    (setq cache (elmo-file-cache-get id)
		  failure
		  (not
		   (and
		    (elmo-message-fetch
		     src-folder (car numbers)
		     (if (elmo-folder-plugged-p src-folder)
			 (elmo-make-fetch-strategy
			  'entire 'maybe nil
			  (and cache (elmo-file-cache-path cache)))
		       (or (and elmo-enable-disconnected-operation
				cache
				(eq (elmo-file-cache-status cache) 'entire)
				(elmo-make-fetch-strategy
				 'entire t nil
				 (elmo-file-cache-path cache)))
			   (error "Unplugged")))
		     nil (current-buffer)
		     'unread)
		    (> (buffer-size) 0)
		    (elmo-folder-append-buffer
		     folder
		     flag
		     (if same-number (car numbers))))))
	  (error (setq failure t)))
	;; FETCH & APPEND finished
	(unless failure
	  (when id
	    (elmo-flag-table-set table id flag))
	  (setq succeed-numbers (cons (car numbers) succeed-numbers)))
	(elmo-progress-notify 'elmo-folder-move-messages)
	(setq numbers (cdr numbers)))
      (when (elmo-folder-persistent-p folder)
	(elmo-flag-table-save (elmo-folder-msgdb-path folder) table))
      succeed-numbers)))

;; Arguments should be reduced.
(defun elmo-folder-move-messages (src-folder msgs dst-folder
					     &optional msgdb
					     no-delete-info
					     no-delete
					     same-number
					     save-unread)
  (save-excursion
    (let* ((messages msgs)
	   (elmo-inhibit-display-retrieval-progress t)
	   (len (length msgs))
	   succeeds i result)
      (if (eq dst-folder 'null)
	  (setq succeeds messages)
	(unless (elmo-folder-writable-p dst-folder)
	  (error "move: %d is not writable"
		 (elmo-folder-name-internal dst-folder)))
	(when messages
	  ;; src is already opened.
	  (elmo-folder-open-internal dst-folder)
	  (unless (setq succeeds (elmo-folder-append-messages dst-folder
							      src-folder
							      messages
							      same-number))
	    (error "move: append message to %s failed"
		   (elmo-folder-name-internal dst-folder)))
	  (elmo-folder-close dst-folder)))
      (if (and (not no-delete) succeeds)
	  (progn
	    (if (and (elmo-folder-delete-messages src-folder succeeds)
		     (elmo-folder-detach-messages src-folder succeeds))
		(setq result t)
	      (message "move: delete messages from %s failed."
		       (elmo-folder-name-internal src-folder))
	      (setq result nil))
	    result)
	(if no-delete
	    (progn
	      ;; (message "Copying messages...done")
	      t)
	  (if (eq len 0)
	      (message "No message was moved.")
	    (message "Moving messages failed.")
	    nil ; failure
	    ))))))

(defun elmo-folder-msgdb-path (folder)
  "Return the msgdb path for FOLDER."
  (or (elmo-folder-path-internal folder)
      (elmo-folder-set-path-internal
       folder
       (elmo-folder-expand-msgdb-path folder))))

(defun elmo-message-accessible-p (folder number)
  "Get accessibility of the message.
Return non-nil when message is accessible."
  (or (elmo-folder-plugged-p folder)
      (elmo-folder-local-p folder)
      (elmo-msgdb-get-cached (elmo-folder-msgdb folder) number)))

(luna-define-generic elmo-message-set-cached (folder number cached)
  "Set cache status of the message in the msgdb.
FOLDER is the ELMO folder structure.
NUMBER is a number of the message.
If CACHED is t, message is set as cached.")

(luna-define-method elmo-message-set-cached ((folder elmo-folder)
					     number cached)
  (when (elmo-msgdb-set-cached (elmo-folder-msgdb folder)
			       number
			       cached
			       (elmo-message-use-cache-p folder number))
    (elmo-folder-set-mark-modified-internal folder t)))

(defun elmo-message-copy-entity (entity)
  ;; 
  (elmo-msgdb-copy-overview-entity entity))

(defun elmo-message-entity-set-number (entity number)
  (elmo-msgdb-overview-entity-set-number entity number))

(luna-define-generic elmo-message-entity (folder key)
  "Return the message-entity structure which matches to the KEY.
KEY is a number or a string.
A number is for message number in the FOLDER.
A string is for message-id of the message.")

(luna-define-method elmo-message-entity ((folder elmo-folder) key)
  (elmo-msgdb-message-entity (elmo-folder-msgdb folder) key))

(luna-define-generic elmo-message-entity-parent (folder entity)
  "Return the parent message-entity structure in the FOLDER.
ENTITY is the message-entity to get the parent.")

(luna-define-method elmo-message-entity-parent ((folder elmo-folder) entity)
  (elmo-msgdb-get-parent-entity entity (elmo-folder-msgdb folder)))

(put 'elmo-folder-do-each-message-entity 'lisp-indent-function '1)
(def-edebug-spec elmo-folder-do-each-message-entity
  ((symbolp form &rest form) &rest form))

(defsubst elmo-folder-list-message-entities (folder)
  ;; List all message entities in the FOLDER.
  (mapcar
   (lambda (number) (elmo-message-entity folder number))
   (elmo-folder-list-messages folder nil t))) ; XXX killed-list is not used.

(defmacro elmo-folder-do-each-message-entity (spec &rest form)
  "Iterator for message entity in the folder.
\(elmo-folder-do-each-message-entity \(entity folder\)
 ... do the process using entity...
\)"
  `(dolist (,(car spec) (elmo-folder-list-message-entities ,(car (cdr spec))))
     ,@form))

(defmacro elmo-message-entity-number (entity)
  `(elmo-msgdb-overview-entity-get-number ,entity))

(defun elmo-message-entity-field (entity field &optional decode)
  "Get message entity field value.
ENTITY is the message entity structure obtained by `elmo-message-entity'.
FIELD is the symbol of the field name.
if optional DECODE is non-nil, returned value is decoded."
  (elmo-msgdb-message-entity-field entity field decode))

(defun elmo-message-entity-set-field (entity field value)
  "Set message entity field value.
ENTITY is the message entity structure.
FIELD is the symbol of the field name.
VALUE is the field value (raw)."
  (elmo-msgdb-message-entity-set-field entity field value))

(luna-define-generic elmo-folder-count-flags (folder)
  "Count flagged message number in the msgdb of the FOLDER.
Return a list of numbers (`new' `unread' `answered')")

(luna-define-method elmo-folder-count-flags ((folder elmo-folder))
  (let ((new 0)
	(unreads 0)
	(answered 0))
    (dolist (elem (elmo-msgdb-get-mark-alist (elmo-folder-msgdb folder)))
      (cond
       ((string= (cadr elem) elmo-msgdb-new-mark)
	(incf new))
       ((member (cadr elem) (elmo-msgdb-unread-marks))
	(incf unreads))
       ((member (cadr elem) (elmo-msgdb-answered-marks))
	(incf answered))))
    (list new unreads answered)))

(defun elmo-message-set-flag (folder number flag)
  "Set message flag.
FOLDER is a ELMO folder structure.
NUMBER is a message number to set flag.

FLAG is a symbol which is one of the following:
  `unread'    (set the message as unread)
  `answered'  (set the message as answered)
  `important' (set the message as important)
'sugar' flag:
  `read'      (remove new and unread flags)")

(defun elmo-message-unset-flag (folder number flag)
  "Unset message flag.
FOLDER is a ELMO folder structure.
NUMBER is a message number to set flag.

FLAG is a symbol which is one of the following:
  `unread'    (remove unread and new flag)
  `answered'  (remove answered flag)
  `important' (remove important flag)
'sugar' flag:
  `read'      (set unread flag)")

(luna-define-generic elmo-message-mark (folder number)
  "Get mark of the message.
FOLDER is the ELMO folder structure.
NUMBER is a number of the message.")

(luna-define-method elmo-message-mark ((folder elmo-folder) number)
  (when (zerop (elmo-folder-length folder))
    (error "Cannot treat this folder correctly."))
  (elmo-msgdb-get-mark (elmo-folder-msgdb folder) number))

(luna-define-generic elmo-message-field (folder number field)
  "Get message field value in the msgdb.
FOLDER is the ELMO folder structure.
NUMBER is a number of the message.
FIELD is a symbol of the field.")

(luna-define-method elmo-message-field ((folder elmo-folder) number field)
  (when (zerop (elmo-folder-length folder))
    (error "Cannot treat this folder correctly."))
  (elmo-msgdb-get-field (elmo-folder-msgdb folder) number field))

(luna-define-method elmo-message-use-cache-p ((folder elmo-folder) number)
  nil) ; default is not use cache.

(luna-define-method elmo-message-folder ((folder elmo-folder) number)
  folder) ; default is folder

(luna-define-method elmo-folder-unmark-important ((folder elmo-folder)
						  numbers
						  &optional ignore-flags)
  (when (elmo-folder-msgdb-internal folder)
    (dolist (number numbers)
      (elmo-msgdb-unset-flag (elmo-folder-msgdb folder)
			     folder
			     number
			     'important))))

(luna-define-method elmo-folder-mark-as-important ((folder elmo-folder)
						   numbers
						   &optional ignore-flags)
  (when (elmo-folder-msgdb-internal folder)
    (dolist (number numbers)
      (elmo-msgdb-set-flag (elmo-folder-msgdb folder)
			   folder
			   number
			   'important))))

(luna-define-method elmo-folder-unmark-read ((folder elmo-folder)
					     numbers
					     &optional ignore-flags)
  (when (elmo-folder-msgdb-internal folder)
    (dolist (number numbers)
      (elmo-msgdb-unset-flag (elmo-folder-msgdb folder)
			     folder
			     number
			     'read))))

(luna-define-method elmo-folder-mark-as-read ((folder elmo-folder)
					      numbers
					      &optional ignore-flag)
  (when (elmo-folder-msgdb-internal folder)
    (dolist (number numbers)
      (elmo-msgdb-set-flag (elmo-folder-msgdb folder)
			   folder
			   number
			   'read))))

(luna-define-method elmo-folder-unmark-answered ((folder elmo-folder) numbers)
  (when (elmo-folder-msgdb-internal folder)
    (dolist (number numbers)
      (elmo-msgdb-unset-flag (elmo-folder-msgdb folder)
			     folder
			     number
			     'answered))))

(luna-define-method elmo-folder-mark-as-answered ((folder elmo-folder) numbers)
  (when (elmo-folder-msgdb-internal folder)
    (dolist (number numbers)
      (elmo-msgdb-set-flag (elmo-folder-msgdb folder)
			   folder
			   number
			   'answered))))

(luna-define-method elmo-folder-process-crosspost ((folder elmo-folder))
  ;; Do nothing.
  )

;;(luna-define-generic elmo-folder-append-message-entity (folder entity
;;							       &optional
;;							       flag-table)
;;  "Append ENTITY to the folder.")

(defun elmo-generic-folder-append-msgdb (folder append-msgdb)
  (if append-msgdb
      (let* ((number-alist (elmo-msgdb-get-number-alist append-msgdb))
	     (all-alist (copy-sequence (append
					(elmo-msgdb-get-number-alist
					 (elmo-folder-msgdb folder))
					number-alist)))
	     (cur number-alist)
	     pair overview
	     to-be-deleted
	     mark-alist)
	(elmo-folder-set-msgdb-internal folder
					(elmo-msgdb-append
					 (elmo-folder-msgdb folder)
					 append-msgdb))
	(while cur
	  (setq all-alist (delq (car cur) all-alist))
	  ;; same message id exists.
	  (if (setq pair (rassoc (cdr (car cur)) all-alist))
	      (setq to-be-deleted (nconc to-be-deleted (list (car pair)))))
	  (setq cur (cdr cur)))
	(cond ((eq (elmo-folder-process-duplicates-internal folder)
		   'hide)
	       ;; Hide duplicates.
	       (setq overview (elmo-delete-if
			       (lambda (x)
				 (memq (elmo-msgdb-overview-entity-get-number
					x)
				       to-be-deleted))
			       (elmo-msgdb-get-overview append-msgdb)))
	       ;; Should be mark as read.
	       (elmo-folder-mark-as-read folder to-be-deleted)
	       (elmo-msgdb-set-overview append-msgdb overview))
	      ((eq (elmo-folder-process-duplicates-internal folder)
		   'read)
	       ;; Mark as read duplicates.
	       (elmo-folder-mark-as-read folder to-be-deleted))
	      (t
	       ;; Do nothing.
	       (setq to-be-deleted nil)))
	(length to-be-deleted))
    0))

(luna-define-method elmo-folder-append-msgdb ((folder elmo-folder)
					      append-msgdb)
  (elmo-generic-folder-append-msgdb folder append-msgdb))

(defun elmo-folder-confirm-appends (appends)
  (let ((len (length appends))
	in)
    (if (and elmo-folder-update-threshold
	     (> len elmo-folder-update-threshold)
	     elmo-folder-update-confirm)
	(if (y-or-n-p (format "Too many messages(%d).  Update all? " len))
	    appends
	  (setq in elmo-folder-update-threshold)
	  (catch 'end
	    (while t
	      (setq in (read-from-minibuffer "Update number: "
					     (int-to-string in))
		    in (string-to-int in))
	      (if (< len in)
		  (throw 'end len))
	      (if (y-or-n-p (format
			     "%d messages are killed (not appeared). OK? "
			     (max (- len in) 0)))
		  (throw 'end in))))
	  (nthcdr (max (- len in) 0) appends))
      (if (and elmo-folder-update-threshold
	       (> len elmo-folder-update-threshold)
	       (not elmo-folder-update-confirm))
	  (nthcdr (max (- len elmo-folder-update-threshold) 0) appends)
	appends))))

(luna-define-method elmo-message-fetch ((folder elmo-folder)
					number strategy
					&optional
					section
					outbuf
					unread)
  (if outbuf
      (with-current-buffer outbuf
	(erase-buffer)
	(elmo-message-fetch-with-cache-process folder number
					       strategy section unread))
    (with-temp-buffer
      (elmo-message-fetch-with-cache-process folder number
					     strategy section unread)
      (buffer-string))))

(luna-define-method elmo-message-fetch-with-cache-process ((folder elmo-folder)
							   number strategy
							   &optional
							   section unread)
  (let ((cache-path (elmo-fetch-strategy-cache-path strategy))
	(method-priorities
	 (cond ((eq (elmo-fetch-strategy-use-cache strategy) 'maybe)
		'(entity cache))
	       ((elmo-fetch-strategy-use-cache strategy)
		'(cache entity))
	       (t
		'(entity))))
	result err)
    (while (and method-priorities
		(null result))
      (setq result
	    (case (car method-priorities)
	      (cache
	       (elmo-file-cache-load cache-path section))
	      (entity
	       (when (and (condition-case error
			      (elmo-message-fetch-internal folder number
							   strategy
							   section
							   unread)
			    (error (setq err error) nil))
			  (> (buffer-size) 0))
		 (elmo-delete-cr-buffer)
		 (when (and (elmo-fetch-strategy-save-cache strategy)
			    cache-path)
		   (elmo-file-cache-save cache-path section))
		 t)))
	    method-priorities (cdr method-priorities)))
    (or result
	(and err (signal (car err) (cdr err))))))

(defun elmo-folder-kill-messages-before (folder msg)
  (elmo-folder-set-killed-list-internal
   folder
   (list (cons 1 msg))))

(defun elmo-folder-kill-messages (folder numbers)
  "Kill(hide) messages in the FOLDER with NUMBERS."
  (elmo-folder-set-killed-list-internal
   folder
   (elmo-number-set-append-list (elmo-folder-killed-list-internal
				 folder) numbers)))


(luna-define-method elmo-folder-clear ((folder elmo-folder)
				       &optional keep-killed)
  (unless keep-killed
    (elmo-folder-set-killed-list-internal folder nil))
  (elmo-folder-set-msgdb-internal folder (elmo-msgdb-clear)))

(luna-define-generic elmo-folder-synchronize (folder
					      &optional
					      disable-killed
					      ignore-msgdb
					      no-check)
  "Synchronize the folder data to the newest status.
FOLDER is the ELMO folder structure.

If optional DISABLE-KILLED is non-nil, killed messages are also synchronized.
If optional IGNORE-MSGDB is non-nil, current msgdb is thrown away except
flag status.
If NO-CHECK is non-nil, rechecking folder is skipped.
Return a list of a cross-posted message number.
If update process is interrupted, return nil.")

(luna-define-method elmo-folder-synchronize ((folder elmo-folder)
					     &optional
					     disable-killed
					     ignore-msgdb
					     no-check)
  (let ((killed-list (elmo-folder-killed-list-internal folder))
	(before-append t)
	number-alist
	old-msgdb diff diff-2 delete-list new-list new-msgdb mark
	flag-table crossed after-append)
    (setq old-msgdb (elmo-folder-msgdb folder))
    (setq flag-table (elmo-flag-table-load (elmo-folder-msgdb-path folder)))
    (when ignore-msgdb
      (elmo-msgdb-flag-table (elmo-folder-msgdb folder) flag-table)
      (elmo-folder-clear folder (not disable-killed)))
    (unless no-check (elmo-folder-check folder))
    (condition-case nil
	(progn
	  (message "Checking folder diff...")
	  (setq diff (elmo-list-diff (elmo-folder-list-messages
				      folder
				      (not disable-killed))
				     (elmo-folder-list-messages
				      folder
				      (not disable-killed)
				      'in-msgdb)))
	  (message "Checking folder diff...done")
	  (setq new-list (elmo-folder-confirm-appends (car diff)))
	  ;; Set killed list as ((1 . MAX-OF-DISAPPEARED))
	  (when (and (not (eq (length (car diff))
			      (length new-list)))
		     (setq diff-2 (elmo-list-diff (car diff) new-list)))
	    (elmo-folder-kill-messages-before folder
					      (nth (- (length (car diff-2)) 1)
						   (car diff-2))))
	  (setq delete-list (cadr diff))
	  (if (or (equal diff '(nil nil))
		  (equal diff '(nil))
		  (and (eq (length (car diff)) 0)
		       (eq (length (cadr diff)) 0)))
	      (progn
		(elmo-folder-update-number folder)
		(elmo-folder-process-crosspost folder)
		0 ; no updates.
		)
	    (when delete-list
	      (elmo-folder-detach-messages folder delete-list))
	    (when new-list
	      (elmo-msgdb-change-mark (elmo-folder-msgdb folder)
				      elmo-msgdb-new-mark
				      elmo-msgdb-unread-uncached-mark)
	      (setq new-msgdb (elmo-folder-msgdb-create
			       folder new-list flag-table))
	      ;; Clear flag-table
	      (if (elmo-folder-persistent-p folder)
		  (elmo-flag-table-save (elmo-folder-msgdb-path folder)
					nil))
	      (setq before-append nil)
	      (setq crossed (elmo-folder-append-msgdb folder new-msgdb))
	      ;; process crosspost.
	      ;; Return a cons cell of (NUMBER-CROSSPOSTS . NEW-MARK-ALIST).
	      (elmo-folder-process-crosspost folder)
	      (elmo-folder-set-message-modified-internal folder t)
	      (elmo-folder-set-mark-modified-internal folder t))
	    ;; return value.
	    (or crossed 0)))
      (quit
       ;; Resume to the original status.
       (if before-append (elmo-folder-set-msgdb-internal folder old-msgdb))
       (elmo-folder-set-killed-list-internal folder killed-list)
       nil))))

(luna-define-generic elmo-folder-detach-messages (folder numbers)
  "Remove messages with NUMBERS from MSGDB.")

(luna-define-method elmo-folder-detach-messages ((folder elmo-folder)
						 numbers)
  (elmo-msgdb-delete-msgs (elmo-folder-msgdb folder) numbers))

(luna-define-generic elmo-folder-length (folder)
  "Return number of messages in the FOLDER.")

(luna-define-method elmo-folder-length ((folder elmo-folder))
  (if (elmo-folder-msgdb-internal folder)
      (elmo-msgdb-length (elmo-folder-msgdb folder))
    0))

(defun elmo-msgdb-load (folder &optional silent)
  (unless silent
    (message "Loading msgdb for %s..." (elmo-folder-name-internal folder)))
  (let ((msgdb (elmo-load-msgdb (elmo-folder-msgdb-path folder))))
    (elmo-folder-set-info-max-by-numdb
     folder
     (elmo-msgdb-list-messages msgdb))
    (unless silent
      (message "Loading msgdb for %s...done"
	       (elmo-folder-name-internal folder)))
    msgdb))

(defun elmo-msgdb-delete-path (folder)
  (let ((path (elmo-folder-msgdb-path folder)))
    (if (file-directory-p path)
	(elmo-delete-directory path t))))

(defun elmo-msgdb-rename-path (old-folder new-folder)
  (let* ((old (directory-file-name (elmo-folder-msgdb-path old-folder)))
	 (new (directory-file-name (elmo-folder-msgdb-path new-folder)))
	 (new-dir (directory-file-name (file-name-directory new))))
    (if (not (file-directory-p old))
	()
      (if (file-exists-p new)
	  (error "Already exists directory: %s" new)
	(if (not (file-exists-p new-dir))
	    (elmo-make-directory new-dir))
	(rename-file old new)))))

(defun elmo-setup-subscribed-newsgroups (groups)
  "Setup subscribed newsgroups.
GROUPS is a list of newsgroup name string.
Return a hashtable for newsgroups."
  (let ((hashtb (or elmo-newsgroups-hashtb
		    (setq elmo-newsgroups-hashtb
			  (elmo-make-hash (length groups))))))
    (dolist (group groups)
      (or (elmo-get-hash-val group hashtb)
	  (elmo-set-hash-val group nil hashtb)))
    (setq elmo-newsgroups-hashtb hashtb)))

(defvar elmo-crosspost-message-alist-modified nil)
(defun elmo-crosspost-message-alist-load ()
  "Load crosspost message alist."
  (setq elmo-crosspost-message-alist (elmo-crosspost-alist-load))
  (setq elmo-crosspost-message-alist-modified nil))

(defun elmo-crosspost-message-alist-save ()
  "Save crosspost message alist."
  (when elmo-crosspost-message-alist-modified
    (let ((alist elmo-crosspost-message-alist)
	  newsgroups)
      (while alist
	(setq newsgroups
	      (elmo-delete-if
	       '(lambda (x)
		  (not (intern-soft x elmo-newsgroups-hashtb)))
	       (nth 1 (car alist))))
	(if newsgroups
	    (setcar (cdar alist) newsgroups)
	  (setq elmo-crosspost-message-alist
		(delete (car alist) elmo-crosspost-message-alist)))
	(setq alist (cdr alist)))
      (elmo-crosspost-alist-save elmo-crosspost-message-alist)
      (setq elmo-crosspost-message-alist-modified nil))))

(defun elmo-folder-make-temporary-directory (folder)
  ;; Make a temporary directory for FOLDER.
  (let ((temp-dir (make-temp-name
		   (concat
		    (file-name-as-directory (elmo-folder-msgdb-path folder))
		    "elmo"))))
    (elmo-make-directory temp-dir)
    temp-dir))

(defun elmo-init ()
  "Initialize ELMO module."
  (elmo-crosspost-message-alist-load)
  (elmo-resque-obsolete-variables)
  (elmo-dop-queue-load))

(defun elmo-quit ()
  "Quit and cleanup ELMO."
  (elmo-crosspost-message-alist-save)
  (elmo-dop-queue-save)
  ;; Not implemented yet.
  (let ((types elmo-folder-type-alist)
	class)
    (while types
      (setq class
	    (luna-find-class
	     (intern (format "elmo-%s-folder" (symbol-name (cdr (car types)))))))
      ;; Call all folder's `elmo-quit' method.
      (if class
	  (dolist (func (luna-class-find-functions class 'elmo-quit))
	    (funcall func nil)))
      (setq types (cdr types)))))

(luna-define-method elmo-folder-rename-internal ((folder elmo-folder)
						 new-folder)
  (error "Cannot rename %s folder"
	 (symbol-name (elmo-folder-type-internal folder))))


;;; Define folders.
(elmo-define-folder ?% 'imap4)
(elmo-define-folder ?-  'nntp)
(elmo-define-folder ?\+ 'localdir)
(elmo-define-folder ?\* 'multi)
(elmo-define-folder ?\/ 'filter)
(elmo-define-folder ?\$ 'archive)
(elmo-define-folder ?&  'pop3)
(elmo-define-folder ?=  'localnews)
(elmo-define-folder ?|  'pipe)
(elmo-define-folder ?.  'maildir)
(elmo-define-folder ?'  'internal)
(elmo-define-folder ?\[  'nmz)
(elmo-define-folder ?@  'shimbun)

;;; Obsolete variables.
(elmo-define-obsolete-variable 'elmo-default-imap4-mailbox
			       'elmo-imap4-default-mailbox)
(elmo-define-obsolete-variable 'elmo-default-imap4-server
			       'elmo-imap4-default-server)
(elmo-define-obsolete-variable 'elmo-default-imap4-authenticate-type
			       'elmo-imap4-default-authenticate-type)
(elmo-define-obsolete-variable 'elmo-default-imap4-user
			       'elmo-imap4-default-user)
(elmo-define-obsolete-variable 'elmo-default-imap4-port
			       'elmo-imap4-default-port)
(elmo-define-obsolete-variable 'elmo-default-imap4-stream-type
			       'elmo-imap4-default-stream-type)
(elmo-define-obsolete-variable 'elmo-default-nntp-server
			       'elmo-nntp-default-server)
(elmo-define-obsolete-variable 'elmo-default-nntp-user
			       'elmo-nntp-default-user)
(elmo-define-obsolete-variable 'elmo-default-nntp-port
			       'elmo-nntp-default-port)
(elmo-define-obsolete-variable 'elmo-default-nntp-stream-type
			       'elmo-nntp-default-stream-type)
(elmo-define-obsolete-variable 'elmo-default-pop3-server
			       'elmo-pop3-default-server)
(elmo-define-obsolete-variable 'elmo-default-pop3-user
			       'elmo-pop3-default-user)
(elmo-define-obsolete-variable 'elmo-default-pop3-authenticate-type
			       'elmo-pop3-default-authenticate-type)
(elmo-define-obsolete-variable 'elmo-default-pop3-port
			       'elmo-pop3-default-port)
(elmo-define-obsolete-variable 'elmo-default-pop3-stream-type
			       'elmo-pop3-default-stream-type)
(elmo-define-obsolete-variable 'elmo-cache-dirname
			       'elmo-cache-directory)
(elmo-define-obsolete-variable 'elmo-msgdb-dir
			       'elmo-msgdb-directory)

;; Obsolete functions.
;; 2001-12-11: *-dir -> *-directory
(defalias 'elmo-folder-make-temp-dir 'elmo-folder-make-temporary-directory)
(make-obsolete 'elmo-folder-make-temp-dir
	       'elmo-folder-make-temporary-directory)


;; autoloads
(autoload 'elmo-dop-queue-flush "elmo-dop")
(autoload 'elmo-nntp-post "elmo-nntp")

(require 'product)
(product-provide (provide 'elmo) (require 'elmo-version))

;;; elmo.el ends here
