;;; elmo.el -- Elisp Library for Message Orchestration

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
(elmo-define-error 'elmo-imap4-bye-error "IMAP4 BYE response" 'elmo-open-error)

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
If optional argument NON-PERSISTENT is non-nil, folder is treated as
 non-persistent."
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
				   :type   type
				   :prefix prefix
				   :name original
				   :persistent (not non-persistent)))
    (save-match-data
      (elmo-folder-send folder 'elmo-folder-initialize name))))

(defmacro elmo-folder-msgdb (folder)
  "Return the msgdb of FOLDER (on-demand loading)."
  (` (or (elmo-folder-msgdb-internal (, folder))
	 (elmo-folder-set-msgdb-internal (, folder)
					 (elmo-msgdb-load (, folder))))))

(luna-define-generic elmo-folder-open (folder &optional load-msgdb)
  "Open and setup (load saved status) FOLDER.
If optional LOAD-MSGDB is non-nil, msgdb is loaded.
(otherwise, msgdb is loaded on-demand)")

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

(luna-define-generic elmo-folder-diff (folder &optional numbers)
  "Get diff of FOLDER.
If optional NUMBERS is set, it is used as current NUMBERS.
Otherwise, saved status for folder is used for comparison.
Return value is a cons cell of NEWS and MESSAGES.")

(luna-define-generic elmo-folder-status (folder)
  "Returns a cons cell of (MAX-NUMBER . MESSAGES) in the FOLDER.")

(luna-define-generic elmo-folder-reserve-status-p (folder)
  "If non-nil, the folder should not close folder after `elmo-folder-status'.")

(defun elmo-folder-list-messages (folder &optional visible-only)
  "Return a list of message numbers contained in FOLDER.
If optional VISIBLE-ONLY is non-nil, killed messages are not listed."
  (let ((list (elmo-folder-list-messages-internal folder visible-only))
	(killed (elmo-folder-killed-list-internal folder))
	numbers)
    (setq numbers
	  (if (listp list)
	      list
	    ;; Not available, use current list.
	    (mapcar
	     'car
	     (elmo-msgdb-get-number-alist (elmo-folder-msgdb folder)))))
    (elmo-living-messages numbers killed)))

(defun elmo-folder-list-unreads (folder unread-marks)
  "Return a list of unread message numbers contained in FOLDER.
UNREAD-MARKS is the unread marks."
  (let ((list (elmo-folder-list-unreads-internal folder
						 unread-marks)))
    (if (listp list)
	list
      ;; Not available, use current mark.
      (delq nil
	    (mapcar
	     (function
	      (lambda (x)
		(if (member (cadr x) unread-marks)
		    (car x))))
	     (elmo-msgdb-get-mark-alist (elmo-folder-msgdb folder)))))))

(defun elmo-folder-list-importants (folder important-mark)
  "Returns a list of important message numbers contained in FOLDER.
IMPORTANT-MARK is the important mark."
  (let ((importants (elmo-folder-list-importants-internal folder important-mark))
	(number-alist (elmo-msgdb-get-number-alist
		       (elmo-folder-msgdb folder)))
	num-pair result)
    (dolist (mark-pair (or elmo-msgdb-global-mark-alist
			   (setq elmo-msgdb-global-mark-alist
				 (elmo-object-load
				  (expand-file-name
				   elmo-msgdb-global-mark-filename
				   elmo-msgdb-dir)))))
      (if (and (string= important-mark (cdr mark-pair))
	       (setq num-pair (rassoc (car mark-pair) number-alist)))
	  (setq result (cons (car num-pair) result))))
    (if (listp importants)
	(elmo-uniq-list (nconc result importants))
      result)))

(luna-define-generic elmo-folder-list-messages-internal (folder &optional
								visible-only)
  ;; Return a list of message numbers contained in FOLDER.
  ;; Return t if the message list is not available.
  )

(luna-define-generic elmo-folder-list-unreads-internal (folder
							unread-marks
							&optional mark-alist)
  ;; Return a list of unread message numbers contained in FOLDER.
  ;; If optional MARK-ALIST is set, it is used as mark-alist.
  ;; Return t if this feature is not available.
  )

(luna-define-generic elmo-folder-list-importants-internal (folder
							   important-mark)
  ;; Return a list of important message numbers contained in FOLDER.
  ;; Return t if this feature is not available.
  )

(luna-define-generic elmo-folder-list-subfolders (folder &optional one-level)
  "Returns a list of subfolders contained in FOLDER.
If optional argument ONE-LEVEL is non-nil, only children of FOLDER is returned.
(a folder which have children is returned as a list)
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
CONDITION is a condition string for searching.
If optional argument NUMBERS is specified and is a list of message numbers,
messages are searched from the list.")

(luna-define-generic elmo-folder-msgdb-create
  (folder numbers new-mark already-mark seen-mark important-mark seen-list)
  "Create a message database (implemented in each backends).
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to create msgdb.
NEW-MARK, ALREADY-MARK, SEEN-MARK, and IMPORTANT-MARK are mark string for
new message, unread but cached message, read message and important message.
SEEN-LIST is a list of message-id string which should be treated as read.")

(luna-define-generic elmo-folder-unmark-important (folder numbers)
  "Un-mark messages as important.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be processed.")

(luna-define-generic elmo-folder-mark-as-important (folder numbers)
  "Mark messages as important.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be processed.")

(luna-define-generic elmo-folder-unmark-read (folder numbers)
  "Un-mark messages as read.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be processed.")

(luna-define-generic elmo-folder-mark-as-read (folder numbers)
  "Mark messages as read.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be processed.")

(luna-define-generic elmo-folder-append-buffer (folder unread &optional number)
  "Append current buffer as a new message.
FOLDER is the destination folder(ELMO folder structure).
If UNREAD is non-nil, message is appended as unread.
If optional argument NUMBER is specified, the new message number is set
(if possible).")

(luna-define-generic elmo-folder-append-messages (folder
						  src-folder
						  numbers
						  unread-marks
						  &optional
						  same-number)
  "Append messages from folder.
FOLDER is the ELMO folder structure.
Caller should make sure FOLDER is `writable'.
(Can be checked with `elmo-folder-writable-p').
SRC-FOLDER is the source ELMO folder structure.
NUMBERS is the message numbers to be appended in the SRC-FOLDER.
UNREAD-MARKS is a list of unread mark string.
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

(luna-define-method elmo-folder-list-unreads-internal
  ((folder elmo-folder) unread-marks &optional mark-alist)
  t)

(luna-define-method elmo-folder-list-importants-internal
  ((folder elmo-folder) important-mark)
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

(luna-define-generic elmo-folder-process-crosspost (folder
						    &optional
						    number-alist)
  "Process crosspost for FOLDER.
If NUMBER-ALIST is set, it is used as number-alist.
Return a cons cell of (NUMBER-CROSSPOSTS . NEW-MARK-ALIST).")

(luna-define-generic elmo-folder-append-msgdb (folder append-msgdb)
  "Append  APPEND-MSGDB to the current msgdb of the folder.")

(luna-define-generic elmo-folder-newsgroups (folder)
  "Return list of newsgroup name of FOLDER.")

(luna-define-method elmo-folder-newsgroups ((folder elmo-folder))
  nil)

(luna-define-method elmo-folder-open ((folder elmo-folder)
				      &optional load-msgdb)
  (elmo-generic-folder-open folder load-msgdb))

(defun elmo-generic-folder-open (folder load-msgdb)
  (let ((inhibit-quit t))
    (if load-msgdb
	(elmo-folder-set-msgdb-internal folder (elmo-msgdb-load folder)))
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
       (elmo-msgdb-get-number-alist
	(elmo-folder-msgdb folder)))
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
  t) ; default is creatable.

(luna-define-method elmo-folder-writable-p ((folder elmo-folder))
  nil) ; default is not writable.

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

(defun elmo-folder-set-info-max-by-numdb (folder msgdb-number)
  "Set FOLDER info by MSGDB-NUMBER in msgdb."
  (let ((num-db (sort (mapcar 'car msgdb-number) '<)))
    (elmo-folder-set-info-hashtb
     folder
     (or (nth (max 0 (1- (length num-db))) num-db) 0)
     nil ;;(length num-db)
     )))

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
  (when (consp (cdr diff))
    (car diff)))

(defsubst elmo-diff-unread (diff)
  (if (consp (cdr diff))
      (nth 1 diff)
    (car diff)))

(defsubst elmo-diff-all (diff)
  (if (consp (cdr diff))
      (nth 2 diff)
    (cdr diff)))

(defsubst elmo-strict-folder-diff (folder)
  "Return folder diff information strictly from FOLDER."
  (let* ((dir (elmo-folder-msgdb-path folder))
	 (nalist (elmo-msgdb-get-number-alist (elmo-folder-msgdb folder)))
	 (in-db (sort (mapcar 'car nalist) '<))
	 (in-folder  (elmo-folder-list-messages folder))
	 append-list delete-list diff)
    (cons (if (equal in-folder in-db)
	      0
	    (setq diff (elmo-list-diff
			in-folder in-db
			nil
			))
	    (setq append-list (car diff))
	    (setq delete-list (cadr diff))
	    (if append-list
		(length append-list)
	      (if delete-list
		  (- 0 (length delete-list))
		0)))
	  (length in-folder))))

(luna-define-method elmo-folder-diff ((folder elmo-folder)
				      &optional numbers)
  (elmo-generic-folder-diff folder numbers))

(defun elmo-generic-folder-diff (folder numbers)
  (if (elmo-string-match-member (elmo-folder-name-internal folder)
				elmo-strict-diff-folder-list)
      (elmo-strict-folder-diff folder)
    (let ((cached-in-db-max (elmo-folder-get-info-max folder))
	  (in-folder (elmo-folder-status folder))
	  (in-db t)
	  unsync messages
	  in-db-max)
      (if numbers
	  (setq in-db-max (or (nth (max 0 (1- (length numbers))) numbers)
			      0))
	(if (not cached-in-db-max)
	    (let ((number-list (mapcar 'car
				       (elmo-msgdb-number-load
					(elmo-folder-msgdb-path folder)))))
	      ;; No info-cache.
	      (setq in-db (sort number-list '<))
	      (setq in-db-max (or (nth (max 0 (1- (length in-db))) in-db)
				  0))
	      (elmo-folder-set-info-hashtb folder in-db-max nil))
	  (setq in-db-max cached-in-db-max)))
      (setq unsync (if (and in-db
			    (car in-folder))
		       (- (car in-folder) in-db-max)
		     (if (and in-folder
			      (null in-db))
			 (cdr in-folder)
		       (if (null (car in-folder))
			   nil))))
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
						 unread-marks
						 &optional
						 same-number)
  (elmo-generic-folder-append-messages folder src-folder numbers
				       unread-marks same-number))

(defun elmo-generic-folder-append-messages (folder src-folder numbers
						   unread-marks same-number)
  (let (unseen seen-list succeed-numbers failure cache)
    (with-temp-buffer
      (while numbers
	(setq failure nil)
	(condition-case nil
	    (progn
	      (elmo-message-fetch
	       src-folder (car numbers)
	       (if (and (not (elmo-folder-plugged-p src-folder))
			elmo-enable-disconnected-operation
			(setq cache (elmo-file-cache-get
				     (elmo-message-field
				      src-folder (car numbers)
				      'message-id)))
			(eq (elmo-file-cache-status cache) 'entire))
		   (elmo-make-fetch-strategy
		    'entire t nil (elmo-file-cache-path cache))
		 (elmo-make-fetch-strategy 'entire t))
	       nil (current-buffer)
	       'unread)
	      (unless (eq (buffer-size) 0)
		(setq failure (not
			       (elmo-folder-append-buffer
				folder
				(setq unseen (member (elmo-message-mark
						      src-folder (car numbers))
						     unread-marks))
				(if same-number (car numbers)))))))
	  (error (setq failure t)))
	;; FETCH & APPEND finished
	(unless failure
	  (unless unseen
	    (setq seen-list (cons (elmo-message-field
				   src-folder (car numbers)
				   'message-id)
				  seen-list)))
	  (setq succeed-numbers (cons (car numbers) succeed-numbers)))
	(elmo-progress-notify 'elmo-folder-move-messages)
	(setq numbers (cdr numbers)))
      (if (and seen-list (elmo-folder-persistent-p folder))
	  (elmo-msgdb-seen-save (elmo-folder-msgdb-path folder)
				(nconc (elmo-msgdb-seen-load
					(elmo-folder-msgdb-path folder))
				       seen-list)))
      succeed-numbers)))

;; Arguments should be reduced.
(defun elmo-folder-move-messages (src-folder msgs dst-folder
					     &optional msgdb
					     no-delete-info
					     no-delete
					     same-number
					     unread-marks
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
							      unread-marks
							      same-number))
	    (error "move: append message to %s failed"
		   (elmo-folder-name-internal dst-folder)))
	  (elmo-folder-close dst-folder))
	(when (and (elmo-folder-persistent-p dst-folder)
		   save-unread)
	  ;; Save to seen list.
	  (let* ((dir (elmo-folder-msgdb-path dst-folder))
		 (seen-list (elmo-msgdb-seen-load dir)))
	    (setq seen-list
		  (elmo-msgdb-add-msgs-to-seen-list
		   msgs (elmo-folder-msgdb src-folder)
		   unread-marks seen-list))
	    (elmo-msgdb-seen-save dir seen-list))))
      (if (and (not no-delete) succeeds)
	  (progn
	    (if (not no-delete-info)
		(message "Cleaning up src folder..."))
	    (if (and (elmo-folder-delete-messages src-folder succeeds)
		     (elmo-msgdb-delete-msgs
		      (elmo-folder-msgdb src-folder) succeeds))
		(setq result t)
	      (message "move: delete messages from %s failed."
		       (elmo-folder-name-internal src-folder))
	      (setq result nil))
	    (if (and result
		     (not no-delete-info))
		(message "Cleaning up src folder...done"))
	    result)
	(if no-delete
	    (progn
	      (message "Copying messages...done")
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

(defun elmo-message-mark (folder number)
  "Get mark of the message.
FOLDER is the ELMO folder structure.
NUMBER is a number of the message."
  (cadr (assq number (elmo-msgdb-get-mark-alist (elmo-folder-msgdb folder)))))

(defun elmo-folder-list-messages-mark-match (folder mark-regexp)
  "List messages in the FOLDER which have a mark that matches MARK-REGEXP"
  (let ((case-fold-search nil)
	matched)
    (if mark-regexp
	(dolist (elem (elmo-msgdb-get-mark-alist (elmo-folder-msgdb folder)))
	  (if (string-match mark-regexp (cadr elem))
	      (setq matched (cons (car elem) matched)))))
    matched))

(defun elmo-message-field (folder number field)
  "Get message field value in the msgdb.
FOLDER is the ELMO folder structure.
NUMBER is a number of the message.
FIELD is a symbol of the field."
  (case field
    (message-id (elmo-msgdb-overview-entity-get-id
		 (elmo-msgdb-overview-get-entity
		  number (elmo-folder-msgdb folder))))
    (subject (elmo-msgdb-overview-entity-get-subject
	      (elmo-msgdb-overview-get-entity
	       number (elmo-folder-msgdb folder))))
    (size (elmo-msgdb-overview-entity-get-size
	   (elmo-msgdb-overview-get-entity
	    number (elmo-folder-msgdb folder))))
    (date (elmo-msgdb-overview-entity-get-date
	   (elmo-msgdb-overview-get-entity
	    number (elmo-folder-msgdb folder))))
    (to (elmo-msgdb-overview-entity-get-to
	 (elmo-msgdb-overview-get-entity
	  number (elmo-folder-msgdb folder))))
    (cc (elmo-msgdb-overview-entity-get-cc
	 (elmo-msgdb-overview-get-entity
	  number (elmo-folder-msgdb folder))))))

(defun elmo-message-set-mark (folder number mark)
  "Set mark for the message in the FOLDER with NUMBER as MARK."
  (elmo-msgdb-set-mark-alist
   (elmo-folder-msgdb folder)
   (elmo-msgdb-mark-set
    (elmo-msgdb-get-mark-alist (elmo-folder-msgdb folder))
    number mark)))

(luna-define-method elmo-message-use-cache-p ((folder elmo-folder) number)
  nil) ; default is not use cache.

(luna-define-method elmo-message-folder ((folder elmo-folder) number)
  folder) ; default is folder

(luna-define-method elmo-folder-unmark-important ((folder elmo-folder) numbers)
  t)

(luna-define-method elmo-folder-mark-as-important ((folder elmo-folder)
						   numbers)
  t)

(luna-define-method elmo-folder-unmark-read ((folder elmo-folder) numbers)
  t)

(luna-define-method elmo-folder-mark-as-read ((folder elmo-folder) numbers)
  t)

(luna-define-method elmo-folder-process-crosspost ((folder elmo-folder)
						   &optional
						   number-alist)
  ;; Do nothing.
  )

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
	(elmo-folder-set-msgdb-internal folder
					(elmo-msgdb-append
					 (elmo-folder-msgdb folder)
					 append-msgdb t))
	(length to-be-deleted))
    0))

(luna-define-method elmo-folder-append-msgdb ((folder elmo-folder)
					      append-msgdb)
  (elmo-generic-folder-append-msgdb folder append-msgdb))

(defun elmo-folder-confirm-appends (appends)
  (let ((len (length appends))
	in)
    (if (and (> len elmo-folder-update-threshold)
	     elmo-folder-update-confirm)
	(if (y-or-n-p (format "Too many messages(%d).  Continue? " len))
	    appends
	  (setq in elmo-folder-update-threshold)
	  (catch 'end
	    (while t
	      (setq in (read-from-minibuffer "Update number: "
					     (int-to-string in))
		    in (string-to-int in))
	      (if (< len in)
		  (throw 'end len))
	      (if (y-or-n-p (format "%d messages are disappeared.  OK? "
				    (max (- len in) 0)))
		  (throw 'end in))))
	  (nthcdr (max (- len in) 0) appends))
      (if (and (> len elmo-folder-update-threshold)
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
					       strategy section unread)
	t)
    (with-temp-buffer
      (elmo-message-fetch-with-cache-process folder number
					     strategy section unread)
      (buffer-string))))

(luna-define-method elmo-message-fetch-with-cache-process ((folder elmo-folder)
							   number strategy
							   &optional
							   section unread)
  (let (cache-path cache-file)
    (if (and (elmo-fetch-strategy-use-cache strategy)
	     (setq cache-path (elmo-fetch-strategy-cache-path strategy))
	     (setq cache-file (elmo-file-cache-expand-path
			       cache-path
			       section))
	     (file-exists-p cache-file)
	     (or (not (elmo-cache-path-section-p cache-file))
		 (not (eq (elmo-fetch-strategy-entireness strategy) 'entire))))
	(insert-file-contents-as-binary cache-file)
      (elmo-message-fetch-internal folder number strategy section unread)
      (elmo-delete-cr-buffer)
      (when (and (> (buffer-size) 0)
		 (elmo-fetch-strategy-save-cache strategy)
		 (elmo-fetch-strategy-cache-path strategy))
	(elmo-file-cache-save
	 (elmo-fetch-strategy-cache-path strategy)
	 section)))))

(luna-define-method elmo-folder-clear ((folder elmo-folder)
				       &optional keep-killed)
  (unless keep-killed
    (elmo-folder-set-killed-list-internal folder nil))
  (elmo-folder-set-msgdb-internal folder (elmo-msgdb-clear)))

(defun elmo-folder-synchronize (folder
				new-mark             ;"N"
				unread-uncached-mark ;"U"
				unread-cached-mark   ;"!"
				read-uncached-mark   ;"u"
				important-mark       ;"$"
				&optional ignore-msgdb
				no-check)
  "Synchronize the folder data to the newest status.
FOLDER is the ELMO folder structure.
NEW-MARK, UNREAD-CACHED-MARK, READ-UNCACHED-MARK, and IMPORTANT-MARK
are mark strings for new messages, unread but cached messages,
read but not cached messages, and important messages.
If optional IGNORE-MSGDB is non-nil, current msgdb is thrown away except
read mark status. If IGNORE-MSGDB is 'visible-only, only visible messages
\(the messages which are not in the killed-list\) are thrown away and
synchronized.
If NO-CHECK is non-nil, rechecking folder is skipped.

Return a list of
\(NEW-MSGDB DELETE-LIST CROSSED\)
NEW-MSGDB is the newly appended msgdb.
DELETE-LIST is a list of deleted message number.
CROSSED is cross-posted message number.
If update process is interrupted, return nil."
  (let ((killed-list (elmo-folder-killed-list-internal folder))
	(before-append t)
	number-alist mark-alist
	old-msgdb diff diff-2 delete-list new-list new-msgdb mark
	seen-list crossed after-append)
    (setq old-msgdb (elmo-folder-msgdb folder))
    ;; Load seen-list.
    (setq seen-list (elmo-msgdb-seen-load (elmo-folder-msgdb-path folder)))
    (setq number-alist (elmo-msgdb-get-number-alist
			(elmo-folder-msgdb folder)))
    (setq mark-alist (elmo-msgdb-get-mark-alist
		      (elmo-folder-msgdb folder)))
    (if ignore-msgdb
	(progn
	  (setq seen-list (nconc
			   (elmo-msgdb-mark-alist-to-seen-list
			    number-alist mark-alist
			    (concat important-mark read-uncached-mark))
			   seen-list))
	  (elmo-folder-clear folder (eq ignore-msgdb 'visible-only))))
    (unless no-check (elmo-folder-check folder))
    (condition-case nil
	(progn
	  (message "Checking folder diff...")
	  ;; TODO: killed list is loaded in elmo-folder-open and
	  ;; list-messages use internal killed-list-folder.
	  (setq diff (elmo-list-diff (elmo-folder-list-messages
				      folder
				      (eq 'visible-only ignore-msgdb))
				     (unless ignore-msgdb
				       (sort (mapcar
					      'car
					      number-alist)
					     '<))))
	  (message "Checking folder diff...done")
	  (setq new-list (elmo-folder-confirm-appends (car diff)))
	  ;; Set killed list.
	  (when (and (not (eq (length (car diff))
			      (length new-list)))
		     (setq diff-2 (elmo-list-diff (car diff) new-list)))
	    (elmo-msgdb-append-to-killed-list folder (car diff-2)))
	  ;; Don't delete important marked messages.
	  (setq delete-list
		(if (eq (elmo-folder-type-internal folder) 'mark)
		    (cadr diff)
		  (elmo-delete-if
		   (lambda (x)
		     (and (setq mark (cadr (assq x mark-alist)))
			  (string= mark important-mark)))
		   ;; delete message list
		   (cadr diff))))
	  (if (or (equal diff '(nil nil))
		  (equal diff '(nil))
		  (and (eq (length (car diff)) 0)
		       (eq (length (cadr diff)) 0)))
	      (progn
		(elmo-folder-update-number folder)
		(elmo-folder-process-crosspost folder)
		(list nil nil nil) ; no updates.
		)
	    (if delete-list (elmo-msgdb-delete-msgs
			     (elmo-folder-msgdb folder) delete-list))
	    (when new-list
	      (setq new-msgdb (elmo-folder-msgdb-create
			       folder
			       new-list
			       new-mark unread-cached-mark
			       read-uncached-mark important-mark
			       seen-list))
	      (elmo-msgdb-change-mark (elmo-folder-msgdb folder)
				      new-mark unread-uncached-mark)
	      ;; Clear seen-list.
	      (if (elmo-folder-persistent-p folder)
		  (setq seen-list (elmo-msgdb-seen-save
				   (elmo-folder-msgdb-path folder) nil)))
	      (setq before-append nil)
	      (setq crossed (elmo-folder-append-msgdb folder new-msgdb))
	      ;; process crosspost.
	      ;; Return a cons cell of (NUMBER-CROSSPOSTS . NEW-MARK-ALIST).
	      (elmo-folder-process-crosspost folder)
	      (elmo-folder-set-message-modified-internal folder t)
	      (elmo-folder-set-mark-modified-internal folder t))
	    ;; return value.
	    (list new-msgdb delete-list crossed)))
      (quit
       ;; Resume to the original status.
       (if before-append
	   (elmo-folder-set-msgdb-internal folder old-msgdb))
       (elmo-folder-set-killed-list-internal folder killed-list)
       nil))))

(defun elmo-folder-messages (folder)
  "Return number of messages in the FOLDER."
  (length
   (elmo-msgdb-get-number-alist
    (elmo-folder-msgdb folder))))

;;;
(defun elmo-msgdb-search (folder condition msgdb)
  "Search messages which satisfy CONDITION from FOLDER with MSGDB."
  (let* ((condition (car (elmo-parse-search-condition condition)))
	 (overview (elmo-msgdb-get-overview msgdb))
	 (number-alist (elmo-msgdb-get-number-alist msgdb))
	 (number-list (mapcar 'car number-alist))
	 (length (length overview))
	 (i 0)
	 result)
    (if (not (elmo-condition-in-msgdb-p condition))
	(elmo-folder-search folder condition number-list)
      (while overview
	(if (elmo-msgdb-search-internal condition (car overview)
					number-list)
	    (setq result
		  (cons
		   (elmo-msgdb-overview-entity-get-number (car overview))
		   result)))
	(setq i (1+ i))
	(elmo-display-progress
	 'elmo-msgdb-search "Searching..." (/ (* i 100) length))
	(setq overview (cdr overview)))
      (nreverse result))))

(defun elmo-msgdb-load (folder)
  (message "Loading msgdb for %s..." (elmo-folder-name-internal folder))
  (let* ((path (elmo-folder-msgdb-path folder))
	 (overview (elmo-msgdb-overview-load path))
	 (msgdb (list overview
		      (elmo-msgdb-number-load path)
		      (elmo-msgdb-mark-load path)
		      (elmo-msgdb-make-overview-hashtb overview))))
    (message "Loading msgdb for %s...done" (elmo-folder-name-internal folder))
    (elmo-folder-set-info-max-by-numdb folder
				       (elmo-msgdb-get-number-alist msgdb))
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

(defun elmo-folder-make-temp-dir (folder)
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
(elmo-define-obsolete-variable 'elmo-default-nntp-server
			       'elmo-nntp-default-server)
(elmo-define-obsolete-variable 'elmo-default-nntp-user
			       'elmo-nntp-default-user)
(elmo-define-obsolete-variable 'elmo-default-nntp-port
			       'elmo-nntp-default-port)
(elmo-define-obsolete-variable 'elmo-default-pop3-server
			       'elmo-pop3-default-server)
(elmo-define-obsolete-variable 'elmo-default-pop3-user
			       'elmo-pop3-default-user)
(elmo-define-obsolete-variable 'elmo-default-pop3-authenticate-type
			       'elmo-pop3-default-authenticate-type)
(elmo-define-obsolete-variable 'elmo-default-pop3-port
			       'elmo-pop3-default-port)

;; autoloads
(autoload 'elmo-dop-queue-flush "elmo-dop")

(require 'product)
(product-provide (provide 'elmo) (require 'elmo-version))

;;; elmo.el ends here
