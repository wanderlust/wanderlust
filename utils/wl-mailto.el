;;; wl-mailto.el -- some mailto support for wanderlust

;;; Copyright (C) 1999 Sen Nagata

;; Author: Sen Nagata <sen@eccosys.com>
;; Version: 0.5
;; License: GPL 2
;; Warning: not optimized at all

;; This file is not a part of GNU Emacs.

;;; Commentary:
;;
;; required elisp packages:
;;
;;   -wl (>= 0.9.6?)
;;
;;   -rfc2368.el
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
(defconst wl-mailto-version "wl-mailto.el 0.5")

;; how should we handle the dependecy on wl?
;; will this work?
(eval-when-compile
  (require 'wl)
  (defun wl-mailto-url-at-point ()))


;; use rfc2368 support -- should be usable for things other than wl too
(require 'rfc2368)

;; yucky compatibility stuff -- someone help me w/ this, please...
(if (and (string-match "^XEmacs \\([0-9.]+\\)" (emacs-version))
	 (< (string-to-int (match-string 1 (emacs-version))) 21))
    ;; for xemacs versions less than 21, use browse-url.el
    (progn
      (require 'browse-url)
      (fset 'wl-mailto-url-at-point
	    'browse-url-url-at-point))
  ;; for everything else, use thingatpt.el
  (progn
    (require 'thingatpt)
    (fset 'wl-mailto-url-at-point
	  (lambda ()
	    (thing-at-point 'url)))))

(defun wl-mailto-compose-message-from-mailto-url (url &optional dummy)
  "Compose a message from URL (RFC 2368).
The optional second argument, DUMMY, exists to match the interface
provided by `browse-url-mail' (w3) -- DUMMY does not do anything."
  (interactive "sURL: ")
  (if (string-match rfc2368-mailto-regexp url)
      (let* ((other-headers (rfc2368-parse-mailto-url url))
	     (to (cdr (assoc-ignore-case "to" other-headers)))
	     (subject (cdr (assoc-ignore-case "subject" other-headers))))

	(wl-user-agent-compose to subject other-headers))
    (message "Not a mailto: url.")))

;; prepare a message from a mailto: url at point
(defun wl-mailto-compose-message-from-mailto-url-at-point ()
  "Draft a new message based on URL (RFC 2368) at point."
  (interactive)
  (let ((url (wl-mailto-url-at-point)))
    (if (and url (string-match rfc2368-mailto-regexp url))
	(wl-mailto-compose-message-from-mailto-url url)
      ;; tell the user that we didn't find a mailto: url at point
      (message "No mailto: url detected at point."))))

;; since this will be used via 'require'...
(provide 'wl-mailto)

;;; wl-mailto.el ends here
