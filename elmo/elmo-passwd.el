;;; elmo-passwd.el --- ELMO password manager.

;; Copyright (C) 2017 Kazuhiro Ito <kzhr@d1.dion.ne.jp>

;; Author: Kazuhiro Ito <kzhr@d1.dion.ne.jp>
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
(require 'elmo-vars)
(require 'elmo-util)


(defgroup elmo-passwd nil
  "ELMO password manager settings."
  :prefix "elmo-passwd-"
  :group 'elmo)

(defcustom elmo-passwd-life-time nil
  "*Duration of ELMO Password in seconds.  nil means infinity."
  :type '(choice (const :tag "Infinity" nil)
		 number)
  :group 'elmo-passwd)

(defcustom elmo-passwd-confirm-before-clear nil
  "Confirm whether save passwords before clear password storage."
  :type 'boolean
  :group 'elmo-passwd)

(eval-and-compile
  (luna-define-class elmo-passwd () (timers))
  (luna-define-internal-accessors 'elmo-passwd))

(luna-define-generic elmo-passwd-load (elmo-passwd)
  "Load password storage.")

(luna-define-generic elmo-passwd-clear (elmo-passwd)
  "Clear password storage.")

(luna-define-generic elmo-passwd-save (elmo-passwd)
  "Save passwords into storage.")

(luna-define-generic elmo-passwd-get (elmo-passwd key)
  "Get password for KEY.  KEY is a list of protocol-name, username, authentication mechanism, host name and port number.")

(luna-define-generic elmo-passwd-remove (elmo-passwd key)
  "Remove password for KEY from cache.")

(luna-define-generic elmo-passwd-modified-p (elmo-passwd)
  "Return non-nil if storage is modified.")

(luna-define-generic elmo-passwd-add-timer (elmo-passwd key)
  "Add removing timer for KEY.
When `elmo-passwd-life-time' is nil, do nothing.")

(luna-define-generic elmo-passwd-remove-timer (elmo-passwd key)
  "Remove removing timer for KEY.
It is used to remove executed timer function.")

(luna-define-generic elmo-passwd-clear-timers (elmo-passwd)
  "Remove all removing timers for ELMO-PASSWD.")

(luna-define-method elmo-passwd-clear ((passwd elmo-passwd))
  (elmo-passwd-clear-timers passwd)
  (when (and elmo-passwd-confirm-before-clear
	     (elmo-passwd-modified-p passwd)
	     (y-or-n-p "Save password storage? "))
    (elmo-passwd-save passwd)))

(luna-define-method elmo-passwd-modified-p ((passwd elmo-passwd))
  nil)

(luna-define-method elmo-passwd-remove-timer ((passwd elmo-passwd) key)
  (let* ((timers (elmo-passwd-timers-internal passwd))
	 (elt (assoc key timers)))
    (elmo-passwd-set-timers-internal passwd (delq elt timers))))

(luna-define-method elmo-passwd-add-timer ((passwd elmo-passwd)
					   key)
  (when elmo-passwd-life-time
    (elmo-passwd-set-timers-internal
     passwd
     (cons
      (cons key (run-with-timer elmo-passwd-life-time nil
				#'(lambda (passwd key)
				    (elmo-passwd-remove passwd key)
				    (elmo-passwd-remove-timer passwd key))
				passwd key))
      (elmo-passwd-timers-internal passwd)))))


(luna-define-method elmo-passwd-clear-timers ((passwd elmo-passwd))
  (dolist (elt (elmo-passwd-timers-internal passwd))
    (cancel-timer (cdr elt)))
  (elmo-passwd-set-timers-internal passwd nil))


;; elmo built-in password storage.
(defcustom elmo-passwd-alist-file-name "passwd"
  "*ELMO Password filename."
  :type 'string
  :group 'elmo-passwd)

(defcustom elmo-passwd-alist-ignore-smtp-port t
  "When non-nil, share SMTP password between the same host with different ports."
  :type 'boolean
  :group 'elmo-passwd)

(eval-and-compile
  (luna-define-class elmo-passwd-alist (elmo-passwd) (filename
						      ignore-smtp-port
						      modified
						      alist))
  (luna-define-internal-accessors 'elmo-passwd-alist))

(defun elmo-passwd-alist-generate-key (passwd key)
  (if (and (elmo-passwd-alist-ignore-smtp-port-internal passwd)
	   (string= (car key) "SMTP"))
      (apply 'format "%s:%s/%s@%s" (nreverse (cdr (reverse key))))
    (apply 'format "%s:%s/%s@%s:%d" key)))

(defun elmo-passwd-alist-load-internal (passwd)
  (let ((filename (elmo-passwd-alist-filename-internal passwd)))
    (elmo-passwd-alist-set-alist-internal
     passwd
     (when (file-readable-p filename)
       (with-temp-buffer
	 (let (insert-file-contents-pre-hook ; To avoid autoconv-xmas...
	       insert-file-contents-post-hook)
	   (insert-file-contents filename)
	   (goto-char (point-min))
	   (ignore-errors
	     (read (current-buffer)))))))))

(luna-define-method initialize-instance :after ((passwd elmo-passwd-alist)
						&rest init-args)
  (elmo-passwd-alist-set-filename-internal
   passwd (expand-file-name elmo-passwd-alist-file-name elmo-msgdb-directory))
  (elmo-passwd-alist-set-ignore-smtp-port-internal
   passwd elmo-passwd-alist-ignore-smtp-port)
  (elmo-passwd-alist-load-internal passwd)
  passwd)

(luna-define-method elmo-passwd-load ((passwd elmo-passwd-alist))
  (elmo-passwd-alist-load-internal passwd)
  (elmo-passwd-alist-set-modified-internal passwd nil))

(luna-define-method elmo-passwd-clear :after ((passwd elmo-passwd-alist))
  (let ((alist (elmo-passwd-alist-alist-internal passwd)))
    (dolist (pair alist)
      (when (stringp (cdr-safe pair))
	(elmo-clear-string (cdr pair)))))
  (elmo-passwd-alist-set-alist-internal passwd nil)
  (elmo-passwd-alist-set-modified-internal passwd nil))

(luna-define-method elmo-passwd-save ((passwd elmo-passwd-alist))
  (with-temp-buffer
    (let ((filename (elmo-passwd-alist-filename-internal passwd))
	  print-length print-level)
      (prin1 (elmo-passwd-alist-alist-internal passwd) (current-buffer))
      (princ "\n" (current-buffer))
      (if (file-writable-p filename)
	  (progn
	    (write-region (point-min) (point-max) filename nil 'no-msg)
	    (set-file-modes filename 384))
	(message "%s is not writable." filename))))
  (elmo-passwd-alist-set-modified-internal passwd nil))

(luna-define-method elmo-passwd-get ((passwd elmo-passwd-alist)
				     key)
  ;; protocol-name, user, auth-method, server, port
  (let ((key-string (elmo-passwd-alist-generate-key passwd key))
	(alist (elmo-passwd-alist-alist-internal passwd))
	pair pass)
    (unless alist
      (elmo-passwd-load passwd)
      (setq alist (elmo-passwd-alist-alist-internal passwd)))
    (setq pair (assoc key-string alist))
    (if pair
	(elmo-base64-decode-string (cdr pair))
      (setq pass (read-passwd (format "Password for %s: " key-string)))
      ;; put key and passwd at the front of the alist
      (elmo-passwd-alist-set-alist-internal
       passwd (cons (cons key-string (elmo-base64-encode-string pass))
		    alist))
      (elmo-passwd-alist-set-modified-internal passwd t)
      (elmo-passwd-add-timer passwd key)
      pass)))

(luna-define-method elmo-passwd-remove ((passwd elmo-passwd-alist)
					key)
  ;; protocol-name, user, auth-method, server, port
  (let ((key (elmo-passwd-alist-generate-key passwd key))
	(alist (elmo-passwd-alist-alist-internal passwd))
	pass-cons)
    (while (setq pass-cons (assoc key alist))
      (unwind-protect
	  (elmo-clear-string (cdr pass-cons))
	(elmo-passwd-alist-set-alist-internal
	 passwd (setq alist (delete pass-cons alist)))
	(elmo-passwd-alist-set-modified-internal passwd t)))))

(luna-define-method elmo-passwd-modified-p ((passwd elmo-passwd-alist))
  (elmo-passwd-alist-modified-internal passwd))

;; auth-source
(eval-when-compile
  (require 'auth-source nil t))

(eval-and-compile
  (luna-define-class elmo-passwd-auth-source (elmo-passwd) (savers))
  (luna-define-internal-accessors 'elmo-passwd-auth-source))

(luna-define-method initialize-instance
  :after ((passwd elmo-passwd-auth-source) &rest init-args)
  (require 'auth-source)
  passwd)

(luna-define-method elmo-passwd-clear :after ((passwd elmo-passwd-auth-source))
  (elmo-passwd-auth-source-set-savers-internal passwd nil))

(luna-define-method elmo-passwd-save ((passwd elmo-passwd-auth-source))
  (dolist (elt (nreverse
		(elmo-passwd-auth-source-savers-internal passwd)))
    (when (functionp (cdr elt))
      (condition-case nil
	  (funcall (cdr elt)))))
  (elmo-passwd-auth-source-set-savers-internal passwd nil))

(luna-define-method elmo-passwd-get ((passwd elmo-passwd-auth-source)
				     key)
  (let* ((auth-source-creation-prompts
	  `((secret . ,(format "%s password for %%u@%%h:%%p? " (car key)))))
	 source secret)
    (setq source
	  (car (auth-source-search :host (nth 3 key)
				   :port (nth 4 key)
				   :user (nth 1 key)
				   :require '(:secret)
				   :create t)))
    (when (functionp (plist-get source :save-function))
      (elmo-passwd-auth-source-set-savers-internal
       passwd (cons (cons key (plist-get source :save-function))
		    (elmo-passwd-auth-source-savers-internal passwd)))
      (elmo-passwd-add-timer passwd key))
    (setq secret (plist-get source :secret))
    (if (functionp secret)
	(funcall secret)
      secret)))

(luna-define-method elmo-passwd-remove ((passwd elmo-passwd-auth-source)
					key)
  (auth-source-forget+ :host (nth 3 key) :port (nth 4 key) :user (nth 1 key))
  (let ((savers (elmo-passwd-auth-source-savers-internal passwd)))
    (elmo-passwd-auth-source-set-savers-internal
     passwd (delq (assoc key savers) savers))))

(luna-define-method elmo-passwd-modified-p ((passwd elmo-passwd-auth-source))
  (elmo-passwd-auth-source-savers-internal passwd))

(provide 'elmo-passwd)

;;; elmo-passwd.el ends here
