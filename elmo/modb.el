;;; modb.el --- Message Orchestration DataBase.

;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
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

(eval-when-compile (require 'cl))

(require 'luna)

(eval-and-compile
  (luna-define-class modb-generic () (location         ; location for save.
				      message-modified ; message is modified.
				      flag-modified    ; flag is modified.
				      ))
  (luna-define-internal-accessors 'modb-generic))

(luna-define-generic elmo-msgdb-load (msgdb)
  "Load the MSGDB.")

(luna-define-generic elmo-msgdb-save (msgdb)
  "Save the MSGDB.")

(luna-define-generic elmo-msgdb-location (msgdb)
  "Return the location of MSGDB.")

(luna-define-generic elmo-msgdb-message-modified-p (msgdb)
  "Return non-nil if message is modified.")

(luna-define-generic elmo-msgdb-flag-modified-p (msgdb)
  "Return non-nil if flag is modified.")

(luna-define-generic elmo-msgdb-append (msgdb msgdb-append)
  "Append the MSGDB-APPEND to the MSGDB.
Return a list of messages which have duplicated message-id.")

(luna-define-generic elmo-msgdb-clear (msgdb)
  "Clear the MSGDB structure.")

(luna-define-generic elmo-msgdb-length (msgdb)
  "Return number of messages in the MSGDB")

(luna-define-generic elmo-msgdb-flags (msgdb number)
  "Return a list of flag which corresponds to the message with NUMBER.")

(luna-define-generic elmo-msgdb-set-flag (msgdb number flag)
  "Set message flag.
MSGDB is the ELMO msgdb.
NUMBER is a message number to set flag.
FLAG is a symbol which is one of the following:
`new'       ... Message which is new.
`read'      ... Message which is already read.
`important' ... Message which is important.
`answered'  ... Message which is answered.
`cached'    ... Message which is cached.")

(luna-define-generic elmo-msgdb-unset-flag (msgdb number flag)
  "Unset message flag.
MSGDB is the ELMO msgdb.
NUMBER is a message number to set flag.
FLAG is a symbol which is one of the following:
`new'       ... Message which is new.
`read'      ... Message which is already read.
`important' ... Message which is important.
`answered'  ... Message which is answered.
`cached'    ... Message which is cached.")

(luna-define-generic elmo-msgdb-list-messages (msgdb)
  "Return a list of message numbers in the MSGDB.")

(luna-define-generic elmo-msgdb-list-flagged (msgdb flag)
  "Return a list of message numbers which is set FLAG in the MSGDB.")

;;; (luna-define-generic elmo-msgdb-search (msgdb condition &optional numbers)
;;;   "Search and return list of message numbers.
;;; MSGDB is the ELMO msgdb structure.
;;; CONDITION is a condition structure for searching.
;;; If optional argument NUMBERS is specified and is a list of message numbers,
;;; messages are searched from the list.")

(luna-define-generic elmo-msgdb-append-entity (msgdb entity &optional flags)
  "Append a ENTITY with FLAGS into the MSGDB.
Return non-nil if message-id of entity is duplicated.")

(luna-define-generic elmo-msgdb-delete-messages (msgdb numbers)
  "Delete messages which are contained NUMBERS from MSGDB.")

(luna-define-generic elmo-msgdb-sort-entities (msgdb predicate
						     &optional app-data)
  "Sort entities of MSGDB, comparing with PREDICATE.
PREDICATE is called with two entities and APP-DATA.
Should return non-nil if the first entity is \"less\" than the second.")

(luna-define-generic elmo-msgdb-message-entity (msgdb key)
  "Return the message-entity structure which matches to the KEY.
KEY is a number or a string.
A number is for message number in the MSGDB.
A string is for message-id of the message.")

;; Message entity handling.
(defvar modb-cache-internal nil)
(defun elmo-message-entity-db (entity)
  "Get modb instance which corresponds to the ENTITY."
  (if (or (null (car entity))
	  (stringp (car entity)))
      ;; Transitional implementation for modb-legacy.
      (or modb-cache-internal
	  (progn
	    (require 'modb-legacy)
	    (setq modb-cache-internal (luna-make-entity 'modb-legacy))))
    ;; XXX Next generation entity structure...not decided yet.
    (car entity)))

(luna-define-generic elmo-msgdb-make-message-entity (msgdb
						     &rest args)
  "Make a message entity for MSGDB.")

(luna-define-generic elmo-msgdb-message-entity-number (msgdb entity)
  "Number of the ENTITY.")

(luna-define-generic elmo-msgdb-message-entity-set-number (msgdb entity number)
  "Set number of the ENTITY.")

(luna-define-generic elmo-msgdb-message-entity-field (msgdb
						      entity field
						      &optional decode)
  "Retrieve field value of the message entity.
MSGDB is the msgdb structure.
ENTITY is the message entity structure.
FIELD is a symbol of the field.
If optional DECODE is no-nil, the field value is decoded.")

(luna-define-generic elmo-msgdb-message-entity-set-field (msgdb
							  entity field value)
  "Set the field value of the message entity.
MSGDB is the msgdb structure.
ENTITY is the message entity structure.
FIELD is a symbol of the field.
VALUE is the field value to set.")

(luna-define-generic elmo-msgdb-copy-message-entity (msgdb entity)
  "Copy message entity.
MSGDB is the msgdb structure.
ENTITY is the message entity structure.")

(luna-define-generic elmo-msgdb-create-message-entity-from-file (msgdb number
								       file)
  "Create message entity from file.
MSGDB is the msgdb structure.
NUMBER is the number of the newly created message entity.
FILE is the message file.")

(luna-define-generic elmo-msgdb-create-message-entity-from-buffer (msgdb
								   number
								   &rest args)
  "Create message entity from current buffer.
NUMBER is the number of the newly created message entity.
Rest of the ARGS is a plist of message entity field for initial value.
Header region is supposed to be narrowed.")

;; Transitional interface.
(luna-define-generic elmo-msgdb-match-condition-internal (msgdb
							  condition
							  entity
							  flags
							  numbers)
  "Return non-nil when the entity matches the condition.")

;;; generic implement
;;
(luna-define-method elmo-msgdb-load ((msgdb modb-generic))
  t)

(luna-define-method elmo-msgdb-location ((msgdb modb-generic))
  (modb-generic-location-internal msgdb))

(luna-define-method elmo-msgdb-message-modified-p ((msgdb modb-generic))
  (modb-generic-message-modified-internal msgdb))

(luna-define-method elmo-msgdb-flag-modified-p ((msgdb modb-generic))
  (modb-generic-flag-modified-internal msgdb))

(luna-define-method elmo-msgdb-append ((msgdb modb-generic) msgdb-append)
  (let (duplicates)
    (dolist (number (elmo-msgdb-list-messages msgdb-append))
      (when (elmo-msgdb-append-entity
	     msgdb
	     (elmo-msgdb-message-entity msgdb-append number)
	     (elmo-msgdb-flags msgdb-append number))
	(setq duplicates (cons number duplicates))))
    duplicates))

(luna-define-method elmo-msgdb-clear ((msgdb modb-generic))
  (modb-generic-set-message-modified-internal msgdb nil)
  (modb-generic-set-flag-modified-internal msgdb nil))

(luna-define-method elmo-msgdb-length ((msgdb modb-generic))
  0)

(luna-define-method elmo-msgdb-make-message-entity ((msgdb modb-generic)
						    args)
  (cons msgdb args))

(luna-define-method elmo-msgdb-message-entity-field ((msgdb modb-generic)
						     entity field
						     &optional decode)
  (plist-get (cdr entity) (intern (concat ":" (symbol-name field)))))

(luna-define-method elmo-msgdb-message-entity-number ((msgdb modb-generic)
						      entity)
  (plist-get (cdr entity) :number))

;; for on demand loading
(provide 'modb-generic)

(require 'product)
(product-provide (provide 'modb) (require 'elmo-version))

;;; modb.el ends here
