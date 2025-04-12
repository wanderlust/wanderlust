;;; wl-mailto.el -- some mailto support for wanderlust  -*- lexical-binding: t -*-

;;; Copyright (C) 1999 Sen Nagata

;; Author: Sen Nagata <sen@eccosys.com>
;; Version: 0.6
;; License: GPL 2
;; Warning: not optimized at all

;; This file is not a part of GNU Emacs.

;;; Commentary:
;;
;; required elisp packages:
;;
;;   -wl (>= 0.9.6?)
;;
;;   -rfc6068.el or rfc2368.el
;;   -thingatpt.el or browse-url.el
;;
;; installation:
;;
;;   -put this file (and rfc2368.el) in an appropriate directory (so Emacs
;;    can find it)
;;
;;   <necessary>
;;   -put:
;;
;;     (add-hook 'wl-init-hook (lambda () (require 'wl-mailto)))
;;
;;    in .emacs or .wl
;;
;; details:
;;
;;   this package provides a number of interactive functions
;; (commands) for the user.  each of the commands ultimately creates a
;; draft message based on some information.  the names of the commands
;; and brief descriptions are:
;;
;;     1) wl-mailto-compose-message-from-mailto-url
;;            make a draft message from a user-specified mailto: url
;;
;;     2) wl-mailto-compose-message-from-mailto-url-at-point
;;            make a draft message from a mailto: url at point
;;
;; usage:
;;
;;   -invoke wl
;;   -try out the commands mentioned above in 'details'

;;; History:
;;
;; 0.6
;;
;;   Support rfc6068.el
;;
;; 0.5
;;
;;   wl-user-agent functionality merged into wl-draft.el, so removed
;;    dependency
;;
;; 1999-06-24:
;;
;;   incorporated a patch from Kenichi OKADA for
;;     wl-mailto-compose-message-from-mailto-url-at-point
;;
;; 1999-06-11:
;;
;;   fixed a typo
;;
;; 0.4
;;
;; 1999-06-01:
;;
;;   checkdoc checking
;;   xemacs compatibility
;;
;; 1999-05-31:
;;
;;   rewrote to use rfc2368.el and wl-user-agent.el

;;; Code:
(defconst wl-mailto-version "wl-mailto.el 0.6")

(require 'elmo-util)
(require 'wl-draft)

;; use rfc6068 or rfc2368 support -- should be usable for things other
;; than wl too
(unless (require 'rfc6068 nil t)
  (require 'rfc2368))

(declare-function rfc6068-parse-mailto-url "rfc6068" (mailto-url))
(declare-function rfc2368-parse-mailto-url "rfc2368" (mailto-url))

(defvar wl-mailto-parse-function (if (featurep 'rfc6068)
				     #'rfc6068-parse-mailto-url
				   #'rfc2368-parse-mailto-url))

(defun wl-mailto-compose-message-from-mailto-url (url &optional _dummy)
  "Compose a message from URL (RFC 2368).
The optional second argument, DUMMY, exists to match the interface
provided by `browse-url-mail' (w3) -- DUMMY does not do anything."
  (interactive "sURL: ")
  (let ((result (ignore-errors
		  ;; rfc2368-parse-mailto-url signals error when url
		  ;; is not a mailto: URL.
		  (funcall wl-mailto-parse-function url))))
    (if result
	(let ((to (cdr (elmo-assoc-ignore-case "to" result)))
	      (subject (cdr (elmo-assoc-ignore-case "subject" result))))
	  (wl-user-agent-compose to subject result))
      (message "Not a mailto: url"))))

;; prepare a message from a mailto: url at point
(defun wl-mailto-compose-message-from-mailto-url-at-point ()
  "Draft a new message based on URL (RFC 2368) at point."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (if url
	(wl-mailto-compose-message-from-mailto-url url)
      ;; tell the user that we didn't find a mailto: url at point
      (message "No url detected at point"))))

;; since this will be used via 'require'...
(provide 'wl-mailto)

;;; wl-mailto.el ends here
