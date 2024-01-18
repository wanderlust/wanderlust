;;; wl-util.el --- Utility modules for Wanderlust.    -*- lexical-binding: t -*-

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000 A. SAGATA <sagata@nttvdt.hil.ntt.co.jp>
;; Copyright (C) 2000 Katsumi Yamaoka <yamaoka@jpl.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	A. SAGATA <sagata@nttvdt.hil.ntt.co.jp>
;;	Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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
(require 'bytecomp)
(require 'elmo-util)
(require 'elmo-flag)
(require 'wl-vars)
(require 'cl-lib)

(require 'pp nil t)

(provide 'wl-util)
 
(defalias 'wl-set-work-buf 'elmo-set-work-buf)
(make-obsolete 'wl-set-work-buf 'elmo-set-work-buf "03 Apr 2000 at latest")

(defmacro wl-append (val func)
  (list 'if val
      (list 'nconc val func)
    (list 'setq val func)))

(defalias 'wl-parse 'elmo-parse)
(make-obsolete 'wl-parse 'elmo-parse "20 Feb 2001")

(require 'wl-address)

(defun wl-delete-duplicates (list &optional all hack-addresses)
  "Delete duplicate equivalent strings from the LIST.
If ALL is t, then if there is more than one occurrence of a string in the LIST,
 then all occurrences of it are removed instead of just the subsequent ones.
If HACK-ADDRESSES is t, then the strings are considered to be mail addresses,
 and only the address part is compared (so that \"Name <foo>\" and \"foo\"
 would be considered to be equivalent.)"
 (let ((table (make-hash-table :test #'equal))
	key key-list)
    (while list
      (when (setq key (if hack-addresses
			  (wl-address-header-extract-address (car list))
			(car list)))
	(if (elmo-has-hash-val key table)
	    (when all (puthash key nil table))
	  (setq key-list (cons key key-list))
	  (puthash key (car list) table))
	(setq list (cdr list))))
    (delq nil (mapcar (lambda (elt) (gethash elt table))
		      (nreverse key-list)))))

;; string utils.
(defalias 'wl-string-member 'elmo-string-member)
(defalias 'wl-string-match-member 'elmo-string-match-member)
(defalias 'wl-string-delete-match 'elmo-string-delete-match)
(defalias 'wl-string-match-assoc 'elmo-string-match-assoc)
(defalias 'wl-string-assoc 'elmo-string-assoc)
(defalias 'wl-string-rassoc 'elmo-string-rassoc)

(defalias 'wl-parse-addresses 'elmo-parse-addresses)

(make-obsolete 'wl-string-member 'elmo-string-member "21 Sep 2000 at latest")
(make-obsolete
 'wl-string-match-member 'elmo-string-match-member "21 Sep 2000 at latest")
(make-obsolete
 'wl-string-delete-match 'elmo-string-delete-match "21 Sep 2000 at latest")
(make-obsolete
 'wl-string-match-assoc 'elmo-string-match-assoc "21 Sep 2000 at latest")
(make-obsolete 'wl-string-assoc 'elmo-string-assoc "21 Sep 2000 at latest")
(make-obsolete 'wl-string-rassoc 'elmo-string-rassoc "21 Sep 2000 at latest")

(make-obsolete 'wl-parse-addresses 'elmo-parse-addresses "20 Mar 2005")

(defun wl-append-element (list element)
  (if element
      (append list (list element))
    list))

(defmacro wl-push (v l)
  "Insert V at the head of the list stored in L."
  (list 'setq l (list 'cons v l)))

(defmacro wl-pop (l)
  "Remove the head of the list stored in L."
  (list 'car (list 'prog1 l (list 'setq l (list 'cdr l)))))

(defun wl-read-event-char (&optional prompt)
  "Get the next event."
  (let ((event (read-event prompt)))
    (cons (and (numberp event) event) event)))

(defun wl-ask-folder (func mes-string)
  (let* (key keve
	     (cmd (string-to-char (format "%s" (this-command-keys)))))
    (message "%s" mes-string)
    (setq key (car (setq keve (wl-read-event-char))))
    (if (or (equal key ?\s)
	    (and cmd
		 (equal key cmd)))
	(progn
	  (message "")
	  (funcall func))
      (wl-push (cdr keve) unread-command-events))))

(defun wl-require-update-all-folder-p (name)
  "Return non-nil if NAME is draft or queue folder."
  (or (string= name wl-draft-folder)
      (string= name wl-queue-folder)))

;;;(defalias 'wl-make-hash 'elmo-make-hash)
;;;(make-obsolete 'wl-make-hash 'elmo-make-hash)

;;;(defalias 'wl-get-hash-val 'elmo-get-hash-val)
;;;(make-obsolete 'wl-get-hash-val 'elmo-get-hash-val)

;;;(defalias 'wl-set-hash-val 'elmo-set-hash-val)
;;;(make-obsolete 'wl-set-hash-val 'elmo-set-hash-val)

(defsubst wl-set-string-width (width string &optional padding ignore-invalid)
  "Make a new string which have specified WIDTH and content of STRING.
`wl-invalid-character-message' is used when invalid character is contained.
If WIDTH is negative number, padding chars are added to the head and
otherwise, padding chars are added to the tail of the string.
The optional 3rd arg PADDING, if non-nil, specifies a padding character
to add the result instead of white space.
If optional 4th argument is non-nil, don't use `wl-invalid-character-message'
even when invalid character is contained."
  (if (> (string-width string) (abs width))
      (setq string (truncate-string-to-width string (abs width))))
  (if (= (string-width string) (abs width))
      string
    (when (and (not ignore-invalid)
	       (< (abs width) (string-width string)))
      (setq string
	    (truncate-string-to-width wl-invalid-character-message
				      (abs width))))
    (let ((paddings (make-string
		     (max 0 (- (abs width) (string-width string)))
		     (or padding ?\s))))
      (if (< width 0)
	  (concat paddings string)
	(concat string paddings)))))

(defun wl-mode-line-buffer-identification (&optional id)
  (let ((priorities '(biff plug title)))
    (let ((items (reverse wl-mode-line-display-priority-list))
	  item)
      (while items
	(setq item (car items)
	      items (cdr items))
	(unless (memq item '(biff plug))
	  (setq item 'title))
	(setq priorities (cons item (delq item priorities)))))
    (let (priority result)
      (while priorities
	(setq priority (car priorities)
	      priorities (cdr priorities))
	(cond
	 ((eq 'biff priority)
	  (when wl-biff-check-folder-list
	    (setq result (append result '((wl-modeline-biff-status
					   wl-modeline-biff-state-on
					   wl-modeline-biff-state-off))))))
	 ((eq 'plug priority)
	  (when wl-show-plug-status-on-modeline
	    (setq result (append result '((wl-modeline-plug-status
					   wl-modeline-plug-state-on
					   wl-modeline-plug-state-off))))))
	 (t
	  (setq result (append result (or id '("Wanderlust: %12b")))))))
      (prog1
	  (setq mode-line-buffer-identification (if (stringp (car result))
						    result
						  (cons "" result)))
	(force-mode-line-update t)))))

(defalias 'wl-display-error 'elmo-display-error)
(make-obsolete 'wl-display-error 'elmo-display-error "03 Apr 2000 at latest")

(defun wl-get-assoc-list-value (assoc-list folder &optional match)
  (catch 'found
    (let ((alist assoc-list)
	  value pair)
      (while alist
	(setq pair (car alist))
	(if (and (eq match 'function)
		 (functionp (car pair)))
	    (when (funcall (car pair) folder)
	      (throw 'found (cdr pair)))
	  (if (string-match (car pair) folder)
	      (cond ((eq match 'all)
		     (setq value (append value (list (cdr pair)))))
		    ((eq match 'all-list)
		     (setq value (append value (cdr pair))))
		    ((or (not match) (eq match 'function))
		     (throw 'found (cdr pair))))))
	(setq alist (cdr alist)))
      value)))

(make-obsolete 'wl-match-string 'match-string"11 Dec 2017")

(make-obsolete 'wl-match-buffer 'elmo-match-buffer "11 Dec 2017")

(put 'wl-as-coding-system 'lisp-indent-function 1)
(put 'wl-as-mime-charset 'lisp-indent-function 1)

(eval-and-compile
  (defmacro wl-as-coding-system (coding-system &rest body)
    `(let ((coding-system-for-read ,coding-system)
	   (coding-system-for-write ,coding-system))
       ,@body)))

(defmacro wl-as-mime-charset (mime-charset &rest body)
  `(wl-as-coding-system (mime-charset-to-coding-system ,mime-charset)
     ,@body))

(defalias 'wl-string 'elmo-string)
(make-obsolete 'wl-string 'elmo-string "03 Apr 2000 at latest")

(defsubst wl-repeat-string (str times)
  (apply #'concat (make-list times str)))

(defun wl-append-assoc-list (item value alist)
  "make assoc list \\='((item1 value1-1 value1-2 ...)) (item2 value2-1 ...)))"
  (let ((entry (assoc item alist)))
    (if entry
	(progn
	  (when (not (member value (cdr entry)))
	    (nconc entry (list value)))
	  alist)
      (append alist
	      (list (list item value))))))

(defun wl-delete-alist (key alist)
  "Delete by side effect any entries specified with KEY from ALIST.
Return the modified ALIST.  Key comparison is done with `assq'.
Write `(setq foo (wl-delete-alist key foo))' to be sure of changing
the value of `foo'."
  (let (entry)
    (while (setq entry (assq key alist))
      (setq alist (delq entry alist)))
    alist))

(defun wl-delete-associations (keys alist)
  "Delete by side effect any entries specified with KEYS from ALIST.
Return the modified ALIST.  KEYS must be a list of keys for ALIST.
Deletion is done with `wl-delete-alist'.
Write `(setq foo (wl-delete-associations keys foo))' to be sure of
changing the value of `foo'."
  (while keys
    (setq alist (wl-delete-alist (car keys) alist))
    (setq keys (cdr keys)))
  alist)

(defun wl-filter-associations (keys alist)
  (let (entry result)
    (while keys
      (when (setq entry (assq (car keys) alist))
	(setq result (cons entry result)))
      (setq keys (cdr keys)))
    result))

(defun wl-inverse-alist (keys alist)
  "Inverse ALIST, copying.
Return an association list represents the inverse mapping of ALIST,
from objects to KEYS.
The objects mapped (cdrs of elements of the ALIST) are shared."
  (let (x y tmp result)
    (while keys
      (setq x (car keys))
      (setq y (cdr (assq x alist)))
      (if y
	  (if (setq tmp (assoc y result))
	      (setq result (cons (append tmp (list x))
				 (delete tmp result)))
	    (setq result (cons (list y x) result))))
      (setq keys (cdr keys)))
    result))

(defsubst wl-get-date-iso8601 (date)
  (or (get-text-property 0 'wl-date date)
      (let* ((d1 (timezone-fix-time date nil nil))
	     (time (format "%04d%02d%02dT%02d%02d%02d"
			   (aref d1 0) (aref d1 1) (aref d1 2)
			   (aref d1 3) (aref d1 4) (aref d1 5))))
	(put-text-property 0 1 'wl-date time date)
	time)))

(defun wl-make-date-string (&optional time)
  (let ((system-time-locale "C"))
    (format-time-string "%a, %d %b %Y %T %z" time)))

(defun wl-date-iso8601 (date)
  "Convert the DATE to YYMMDDTHHMMSS."
  (condition-case ()
      (wl-get-date-iso8601 date)
    (error "")))

(defun wl-url-news (url &rest _args)
  (interactive "sURL: ")
  (if (string-match "^news:\\(.*\\)$" url)
      (wl-summary-goto-folder-subr
       (concat "-" (match-string 1 url)) nil nil nil t)
    (message "Not a news: url.")))

(defun wl-url-nntp (url &rest _args)
  (interactive "sURL: ")
  (let (folder fld-name server port msg)
    (if (string-match
	 "^nntp://\\([^:/]*\\):?\\([0-9]*\\)/\\([^/]*\\)/\\([0-9]*\\).*$" url)
	(progn
	  (if (zerop (length (setq fld-name (match-string 3 url))))
	      (setq fld-name nil))
	  (if (zerop (length (setq port (match-string 2 url))))
	      (setq port (number-to-string elmo-nntp-default-port)))
	  (if (zerop (length (setq server (match-string 1 url))))
	      (setq server elmo-nntp-default-server))
	  (setq folder (concat "-" fld-name "@" server ":" port))
	  (if (zerop (length (setq msg (match-string 4 url))))
	      (wl-summary-goto-folder-subr
	       folder nil nil nil t)
	    (wl-summary-goto-folder-subr
	     folder 'update nil nil t)
	    (wl-summary-jump-to-msg (string-to-number msg))
	    (wl-summary-redisplay)))
      (message "Not a nntp: url."))))

(defmacro wl-concat-list (list separator)
  `(mapconcat 'identity (delete "" (delq nil ,list)) ,separator))

(require 'wl-message)
(require 'wl-summary)

(defun wl-current-message-buffer ()
  (when (buffer-live-p wl-current-summary-buffer)
    (with-current-buffer wl-current-summary-buffer
      (or wl-message-buffer
	  (and (wl-summary-message-number)
	       (wl-message-buffer-display
		wl-summary-buffer-elmo-folder
		(wl-summary-message-number)
		wl-summary-buffer-display-mime-mode
		nil nil))))))

(defun wl-kill-buffers (regexp)
  (mapc
   (lambda (x)
     (if (and (buffer-name x)
	      (string-match regexp (buffer-name x)))
	 (and (get-buffer x)
	      (kill-buffer x))))
   (buffer-list)))

(defun wl-collect-summary ()
  (let (result)
    (mapc
     (lambda (x)
       (if (and (string-match "^Summary"
			      (buffer-name x))
		(with-current-buffer x
		  (eq major-mode 'wl-summary-mode)))
	   (setq result (nconc result (list x)))))
     (buffer-list))
    result))

(defun wl-collect-draft ()
  (let ((draft-regexp (concat "^" (regexp-quote (if (memq 'modeline wl-use-folder-petname)
                                                    (wl-folder-get-petname wl-draft-folder)
                                                  wl-draft-folder))))
	result)
    (dolist (buffer (buffer-list))
      (when (with-current-buffer buffer
	      (and (eq major-mode 'wl-draft-mode)
		   (buffer-name)
		   (string-match draft-regexp (buffer-name))))
	(setq result (cons buffer result))))
    (nreverse result)))

(defvar wl-inhibit-save-drafts nil)
(defvar wl-disable-auto-save nil)
(make-variable-buffer-local 'wl-disable-auto-save)

(defun wl-save-drafts ()
  "Save all drafts. Return nil if there is no draft buffer."
  (if wl-inhibit-save-drafts
      'inhibited
    (let ((wl-inhibit-save-drafts t)
	  (msg (current-message))
	  (buffers (wl-collect-draft)))
      (save-excursion
	(dolist (buffer buffers)
	  (set-buffer buffer)
	  (when (and (not wl-disable-auto-save)
		     (buffer-modified-p))
	    (wl-draft-save))))
      (message "%s" (or msg ""))
      buffers)))

(defun wl-read-directory-name (prompt dir)
  (read-directory-name prompt dir dir))

;; local variable check.
(defalias 'wl-local-variable-p 'local-variable-p)
(make-obsolete 'wl-local-variable-p 'local-variable-p "24 May 2020")

(defun wl-number-base36 (num len)
  (if (if (< len 0)
	  (<= num 0)
	(= len 0))
      ""
    (concat (wl-number-base36 (/ num 36) (1- len))
	    (list (aref "zyxwvutsrqponmlkjihgfedcba9876543210" (% num 36))))))

(defvar wl-unique-id-char nil)

(defun wl-unique-id ()
  ;; Don't use microseconds from (current-time), they may be unsupported.
  ;; Instead we use this randomly inited counter.
  (setq wl-unique-id-char
	(% (1+ (or wl-unique-id-char (logand (random t) (1- (ash 1 20)))))
	   ;; (current-time) returns 16-bit ints,
	   ;; and 2^16*25 just fits into 4 digits i base 36.
	   (* 25 25)))
  (let ((integer (elmo-time-integer)))
    (concat
     (if (memq system-type '(ms-dos emx vax-vms))
	 (let ((user (downcase (user-login-name))))
	   (while (string-match "[^a-z0-9_]" user)
	     (aset user (match-beginning 0) ?_))
	   user)
       (wl-number-base36 (user-uid) -1))
     (wl-number-base36 (+ (/ integer 65536)
			  (ash (% wl-unique-id-char 25) 16)) 4)
     (wl-number-base36 (+ (% integer 65536)
			  (ash (/ wl-unique-id-char 25) 16)) 4)
     ;; Append the name of the message interface, because while the
     ;; generated ID is unique to this newsreader, other newsreaders
     ;; might otherwise generate the same ID via another algorithm.
     wl-unique-id-suffix)))

(defcustom wl-draft-make-message-id-from-address-delimiter "-"
  "A string between unique and addr-spec of Message-ID built from
e-mail address.  It should be consist of atext (described in RFC
5322)."
  :type 'string
  :group 'wl-draft)

(defun wl-draft-make-message-id-from-address (string)
  (when (and (stringp string)
	     (string-match "\\`\\(.+\\)@\\([^@]+\\)\\'" string))
    (let ((local (match-string 1 string))
	  (domain (match-string 2 string)))
      (format "<%s%s%s@%s>"
	      (wl-unique-id)
	      wl-draft-make-message-id-from-address-delimiter
	      (if wl-message-id-hash-function
		  (concat wl-draft-make-message-id-from-address-delimiter
			  (funcall wl-message-id-hash-function local))
		local)
	      domain))))

(defvar wl-message-id-function 'wl-draft-make-message-id-string)
(defun wl-draft-make-message-id-string ()
  "Return Message-ID field value."
  (or (and wl-message-id-use-message-from
	   (catch :done
	     (mapc (lambda (string)
		     (when (setq string
				 (wl-draft-make-message-id-from-address
				  (std11-address-string
				   (car (std11-parse-address-string string)))))
		       (throw :done string)))
		   (list (or (std11-fetch-field "from") "") wl-from))
	     nil))
      (format "<%s@%s>"
	      (wl-unique-id)
	      (or wl-message-id-domain
		  (if wl-local-domain
		      (concat (system-name) "." wl-local-domain)
		    (system-name))))))

;;; Profile loading.
(defvar wl-load-profile-function 'wl-local-load-profile)
(defun wl-local-load-profile ()
  "Load `wl-init-file'."
  (message "Initializing...")
  (load wl-init-file 'noerror 'nomessage))

(defun wl-load-profile ()
  "Call `wl-load-profile-function' function."
  (funcall wl-load-profile-function))

;;;

(defsubst wl-count-lines ()
  (count-lines 1 (point-at-bol)))

(defun wl-horizontal-recenter ()
  "Recenter the current buffer horizontally."
  (beginning-of-line)
  (re-search-forward "[[<]" (point-at-eol) t)
  (if (< (current-column) (/ (window-width) 2))
      (set-window-hscroll (get-buffer-window (current-buffer) t) 0)
    (let* ((orig (point))
	   (end (window-end (get-buffer-window (current-buffer) t)))
	   (max 0))
      (when end
	;; Find the longest line currently displayed in the window.
	(goto-char (window-start))
	(while (and (not (eobp))
		    (< (point) end))
	  (end-of-line)
	  (setq max (max max (current-column)))
	  (forward-line 1))
	(goto-char orig)
	;; Scroll horizontally to center (sort of) the point.
	(if (> max (window-width))
	    (set-window-hscroll
	     (get-buffer-window (current-buffer) t)
	     (min (- (current-column) (/ (window-width) 3))
		  (+ 2 (- max (window-width)))))
	  (set-window-hscroll (get-buffer-window (current-buffer) t) 0))
	max))))

;; Draft auto-save
(defun wl-auto-save-drafts ()
  (unless (wl-save-drafts)
    (wl-stop-save-drafts)))

(defun wl-start-save-drafts ()
  (when (numberp wl-auto-save-drafts-interval)
    (let ((timer (get 'wl-save-drafts 'timer)))
      (if timer
	  (progn
	    (timer-set-idle-time timer wl-auto-save-drafts-interval t)
	    (cancel-timer timer)
	    (timer-activate-when-idle timer))
	(put 'wl-save-drafts 'timer
	     (run-with-idle-timer
	      wl-auto-save-drafts-interval t 'wl-auto-save-drafts))))))

(defun wl-stop-save-drafts ()
  (when (get 'wl-save-drafts 'timer)
    (cancel-timer (get 'wl-save-drafts 'timer))))

(defun wl-set-auto-save-draft (&optional arg)
  (interactive "P")
  (unless (setq wl-disable-auto-save
		(cond
		 ((null arg) (not wl-disable-auto-save))
		 ((< (prefix-numeric-value arg) 0) t)
		 (t nil)))
    (wl-start-save-drafts))
  (when (called-interactively-p 'interactive)
    (message "Auto save is %s (in this buffer)"
	     (if wl-disable-auto-save "disabled" "enabled"))))

;; Biff

;; Internal variable.
(defvar wl-biff-check-folders-running nil)

(defun wl-biff-stop ()
  (mapc (lambda (elt)
	  (when (timerp elt) (cancel-timer elt)))
	(get 'wl-biff 'timers))
  (put 'wl-biff 'timers nil))

(defun wl-biff-start ()
  (if wl-biff-check-folder-list
      ;; If biff timer already started, do nothing.
      (unless (get 'wl-biff 'timers)
	(put 'wl-biff 'timers
	     (list (if wl-biff-use-idle-timer
		       (run-with-idle-timer
			wl-biff-check-interval t 'wl-biff-event-handler)
		     (run-at-time t wl-biff-check-interval
				  'wl-biff-launch-handler)))))
    (message "No folder is specified for biff")))

(defun wl-biff-launch-handler ()
  (let ((timers (get 'wl-biff 'timers)))
    (unless (or wl-biff-check-folders-running
		(cdr timers))
      (put 'wl-biff 'timers
	   (cons (run-with-idle-timer
		  wl-biff-check-delay nil 'wl-biff-event-handler)
		 timers)))))

(defun wl-biff-event-handler ()
  ;; PAKURing from FSF:time.el
  (unwind-protect
      (progn
	(condition-case signal
	    (wl-biff-check-folders)
	  (error
	   (message "wl-biff: %s (%s)" (car signal) (cdr signal))))
	;; Do redisplay right now, if no input pending.
	(sit-for 0))
    (wl-biff-start)
    (put 'wl-biff 'timers
	 (let ((timers (get 'wl-biff 'timers)))
	   ;; Cancel existing extra idle timer (normaly only 1 at most).
	   (while (cdr timers)
	     (when (timerp (car timers)) (cancel-timer (car timers)))
	     (setq timers (cdr timers)))
	   (if wl-biff-use-idle-timer
	       ;; Run extra idle timer for the case Emacs keeps idle.
	       (cons (run-with-idle-timer
		      (+ wl-biff-check-interval
			 (float-time (current-idle-time)))
		      nil 'wl-biff-event-handler)
		     timers)
	     timers)))))

(defsubst wl-biff-notify (new-mails notify-minibuf)
  (if (> new-mails 0)
      (progn
	(unless wl-modeline-biff-status
	  (run-hooks 'wl-biff-notify-hook))
	(run-hook-with-args 'wl-biff-new-mail-hook new-mails))
    (when (and wl-modeline-biff-status (eq new-mails 0))
      (run-hooks 'wl-biff-unnotify-hook)))
  (setq wl-modeline-biff-status (> new-mails 0))
  (force-mode-line-update t)
  (when notify-minibuf
    (cond ((zerop new-mails) (message "No mail."))
	  ((= 1 new-mails) (message "You have a new mail."))
	  (t (message "You have %d new mails." new-mails)))))

(defun wl-biff-check-folders ()
  (interactive)
  (if wl-biff-check-folders-running
      (when (called-interactively-p 'interactive)
	(message "Biff process is running."))
    (setq wl-biff-check-folders-running t)
    (when (called-interactively-p 'interactive)
      (message "Checking new mails..."))
    (let ((new-mails 0)
	  (flist (or wl-biff-check-folder-list (list wl-default-folder)))
	  folder)
      (if (and (eq (length flist) 1)
	       (eq (elmo-folder-type-internal
		    (setq folder (wl-folder-get-elmo-folder (car flist) 'biff))) 'imap4))
	  (wl-biff-check-folder-async folder (called-interactively-p 'interactive))
	(unwind-protect
	    (while flist
	      (setq folder (wl-folder-get-elmo-folder (car flist))
		    flist (cdr flist))
	      (elmo-folder-set-biff-internal folder t)
	      (when (and (elmo-folder-plugged-p folder)
			 (elmo-folder-exists-p folder))
		(setq new-mails
		      (+ new-mails
			 (nth 0 (wl-biff-check-folder folder))))))
	  (setq wl-biff-check-folders-running nil)
	  (wl-biff-notify new-mails (called-interactively-p 'interactive)))))))

(autoload 'elmo-pop3-get-session "elmo-pop3")

(defun wl-biff-check-folder (folder)
  (if (eq (elmo-folder-type-internal folder) 'pop3)
      (if (elmo-pop3-get-session folder 'any-exists)
	  (make-list 3 0)
	(wl-folder-check-one-entity (elmo-folder-name-internal folder)
				    'biff))
    (wl-folder-check-one-entity (elmo-folder-name-internal folder)
				'biff)))

(require 'wl-folder)
(defun wl-biff-check-folder-async-callback (diff data)
  (if (nth 1 data)
      (with-current-buffer (nth 1 data)
	(wl-folder-entity-hashtb-set wl-folder-entity-hashtb
				     (nth 0 data)
				     (list (nth 0 diff)
					   (- (nth 1 diff) (nth 0 diff))
					   (nth 2 diff))
				     (current-buffer))))
  (setq wl-folder-info-alist-modified t)
  (setq wl-biff-check-folders-running nil)
  (sit-for 0)
  (wl-biff-notify (car diff) (nth 2 data)))

(defun wl-biff-check-folder-async (folder notify-minibuf)
  (if (and (elmo-folder-plugged-p folder)
	   (wl-folder-entity-exists-p (elmo-folder-name-internal folder)))
      (progn
	(elmo-folder-set-biff-internal folder t)
	(if (and (eq (elmo-folder-type-internal folder) 'imap4)
		 (elmo-folder-use-flag-p folder))
	    ;; Check asynchronously only when IMAP4 and use server diff.
	    (progn
	      (setq elmo-folder-diff-async-callback
		    'wl-biff-check-folder-async-callback)
	      (setq elmo-folder-diff-async-callback-data
		    (list (elmo-folder-name-internal folder)
			  (get-buffer wl-folder-buffer-name)
			  notify-minibuf))
	      (elmo-folder-diff-async folder))
	  (unwind-protect
	      (wl-biff-notify (car (wl-biff-check-folder folder))
			      notify-minibuf)
	    (setq wl-biff-check-folders-running nil))))
    (setq wl-biff-check-folders-running nil)))

(make-obsolete 'wl-expand-newtext 'elmo-expand-newtext "22 Sep 2016")

(make-obsolete 'wl-regexp-opt 'elmo-regexp-opt "22 Sep 2016")

(defun wl-region-exists-p ()
  "Return non-nil if a region exists on current buffer."
  (and transient-mark-mode mark-active))

(defun wl-deactivate-region ()
  "Deactivate region on current buffer"
  (setq mark-active nil))

(defvar wl-line-string)
(defun wl-line-parse-format (format spec-alist)
  "Make a formatter from FORMAT and SPEC-ALIST."
  (let (f spec specs stack)
    (setq f
	  (with-temp-buffer
	    (insert format)
	    (goto-char (point-min))
	    (while (search-forward "%" nil t)
	      (cond
	       ((looking-at "%")
		(goto-char (match-end 0)))
	       ((looking-at "\\(-?\\(0?\\)[0-9]*\\)\\([^0-9]\\)")
		(cond
		 ((string= (match-string 3) "(")
		  (if (zerop (length (match-string 1)))
		      (error "No number specification for %%( line format"))
		  (push (list
			 (match-beginning 0) ; start
			 (match-end 0)       ; start-content
			 (string-to-number
			  (match-string 1))  ; width
			 specs) ; specs
			stack)
		  (setq specs nil))
		 ((string= (match-string 3) ")")
		  (let ((entry (pop stack))
			form)
		    (unless entry
		      (error
		       "No matching %%( parenthesis in summary line format"))
		    (goto-char (car entry)) ; start
		    (setq form (buffer-substring (nth 1 entry) ; start-content
						 (- (match-beginning 0) 1)))
		    (delete-region (car entry) (match-end 0))
		    (insert "s")
		    (setq specs
			  (append
			   (nth 3 entry)
			   (list (list 'wl-set-string-width (nth 2 entry)
				       (append
					(list 'format form)
					specs)))))))
		 (t
		  (setq spec
			(if (setq spec (assq (string-to-char (match-string 3))
					     spec-alist))
			    (nth 1 spec)
			  (match-string 3)))
		  (unless (string= "" (match-string 1))
		    (setq spec (list 'wl-set-string-width
				     (string-to-number (match-string 1))
				     spec
				     (unless (string= "" (match-string 2))
				       (string-to-char (match-string 2))))))
		  (replace-match "s" 'fixed)
		  (setq specs (append specs
				      (list
				       (list
					'setq 'wl-line-string
					spec)))))))))
	    (buffer-string)))
    (append (list 'format f) specs)))

(defmacro wl-line-formatter-setup (formatter format alist)
  `(let (byte-compile-warnings)
     (setq ,formatter
	   (byte-compile
	    (list 'lambda ()
		  (wl-line-parse-format ,format ,alist))))
     (when (get-buffer "*Compile-Log*")
       (bury-buffer "*Compile-Log*"))
     (when (get-buffer "*Compile-Log-Show*")
       (bury-buffer "*Compile-Log-Show*"))))

(defsubst wl-copy-local-variables (src dst local-variables)
  "Copy value of LOCAL-VARIABLES from SRC buffer to DST buffer."
  (with-current-buffer dst
    (dolist (variable local-variables)
      (set (make-local-variable variable)
	   (with-current-buffer src
	     (symbol-value variable))))))

(defsubst wl-window-deletable-p ()
  "Return t if selected window can be safely deleted from its frame."
      (eq (window-deletable-p) t))
      
;;; Search Condition
(defun wl-search-condition-fields ()
  (let ((denial-fields
	 (nconc (mapcar 'capitalize elmo-msgdb-extra-fields)
		(mapcar 'capitalize wl-additional-search-condition-fields)
		'("Flag" "Since" "Before"
		  "From" "Subject" "To" "Cc" "Body" "Raw-Body" "ToCc"
		  "Larger" "Smaller"))))
    (append '("Last" "First")
	    denial-fields
	    (mapcar (lambda (f) (concat "!" f))
		    denial-fields))))

(defun wl-read-search-condition (default)
  "Read search condition string interactively."
  (wl-read-search-condition-internal "Search by" default))

(defun wl-read-search-condition-internal (prompt default &optional paren)
  (let* ((completion-ignore-case t)
	 (field (completing-read
		 (format "%s (%s): " prompt default)
		 (mapcar #'list
			 (append '("AND" "OR") (wl-search-condition-fields)))
		 nil nil nil nil default))
	 value)
    (cond
     ((or (string= field "AND") (string= field "OR"))
      (concat (if paren "(" "")
	      (wl-read-search-condition-internal
	       (concat field "(1) Search by") default 'paren)
	      (if (string= field "AND") "&" "|")
	      (wl-read-search-condition-internal
	       (concat field "(2) Search by") default 'paren)
	      (if paren ")" "")))
     ((string-match "Since\\|Before" field)
      (let ((default (format-time-string "%Y-%m-%d")))
	(setq value (completing-read
		     (format "Value for '%s' [%s]: " field default)
		     (mapcar
		      (lambda (x)
			(list (format "%s" (car x))))
		      elmo-date-descriptions)
		     nil nil nil nil default))
	(concat (downcase field) ":" value)))
     ((string-match "!?Flag" field)
      (while (null value)
	(setq value (downcase
		     (completing-read
		      (format "Value for '%s': " field)
		      (mapcar (lambda (f) (list (capitalize (symbol-name f))))
			      (elmo-uniq-list
			       (append
				'(unread answered forwarded digest any)
				(copy-sequence elmo-global-flags))
			       #'delq)))))
	(unless (elmo-flag-valid-p value)
	  (message "Invalid char in `%s'" value)
	  (setq value nil)
	  (sit-for 1)))
      (unless (string-match (concat "^" elmo-condition-atom-regexp "$")
			    value)
	(setq value (prin1-to-string value)))
      (concat (downcase field) ":" value))
     (t
      (setq value (read-from-minibuffer (format "Value for '%s': " field)))
      (unless (string-match (concat "^" elmo-condition-atom-regexp "$")
			    value)
	(setq value (prin1-to-string value)))
      (concat (downcase field) ":" value)))))

(defun wl-y-or-n-p-with-scroll (prompt &optional scroll-by-SPC)
  (let ((prompt (concat prompt (if scroll-by-SPC
				   "<y/n/SPC(down)/BS(up)> "
				 "<y/n/j(down)/k(up)> "))))
    (catch 'done
      (while t
	(discard-input)
	(let ((key (let ((cursor-in-echo-area t))
		     (cdr (wl-read-event-char prompt)))))
	  (cond
	   ((memq key '(?y ?Y))
	    (throw 'done t))
	   ((eq key ?\s)
	    (if scroll-by-SPC
		(ignore-errors (scroll-up))
	      (throw 'done t)))
	   ((memq key '(?v ?j ?J next))
	    (ignore-errors (scroll-up)))
	   ((memq key '(?^ ?k ?K prior backspace))
	    (ignore-errors (scroll-down)))
	   ((memq key '(?n ?N))
	    (throw 'done nil))))))))

(defun wl-find-region (beg-regexp end-regexp)
  (if (or (re-search-forward end-regexp nil t)
	  (re-search-backward end-regexp nil t))
      (let ((end (match-end 0))
	    (beg (re-search-backward beg-regexp nil t)))
	(if beg
	    (cons beg end)))))

(defun wl-simple-display-progress (_label action current total)
  (when (> total 0)
    (let ((progress (* (/ current (float total)) 100)))
      (if (< total 10000)
	  (message "%s... %d%%"
		   action (floor progress))
	(message (format "%%s... %%.%df%%%%"
			 (- (length (number-to-string total)) 4))
		 action
		 progress)))))

(when (fboundp 'progress-feedback-with-label)
  (defun wl-display-progress-with-gauge (label action current total)
    (progress-feedback-with-label
     label
     "%s..."
     (if (> total 0) (floor (* (/ current (float total)) 100)) 0)
     action)))

(defvar wl-progress-next-update-time nil)

(defun wl-progress-set-next-update-time ()
  (setq wl-progress-next-update-time
	(time-add (current-time) wl-progress-update-interval)))

(defun wl-progress-callback-function (label action current total)
  (cl-case current
    (query
     (let ((threshold (if (consp wl-display-progress-threshold)
			  (cdr (or (assq label wl-display-progress-threshold)
				   (assq t wl-display-progress-threshold)))
			wl-display-progress-threshold)))
       (and threshold
	    (>= total threshold))))
    (start
     (wl-progress-set-next-update-time)
     (message "%s..." action))
    (done
     (wl-progress-set-next-update-time)
     (message "%s...done" action))
    (t
     (when (and wl-display-progress-function
		(or (null wl-progress-next-update-time)
		    (time-less-p wl-progress-next-update-time
				 (current-time))))
       (wl-progress-set-next-update-time)
       (funcall wl-display-progress-function label action current total)))))

;; read multiple strings with completion
(defun wl-completing-read-multiple-1 (prompt
				      table
				      &optional predicate
				      _require-match initial-input
				      hist def inherit-input-method)
    "Read multiple strings in the minibuffer"
    (split-string
     (completing-read prompt table predicate nil
		      initial-input hist def inherit-input-method)
     ","))

(eval-when-compile
  (require 'crm))
(defun wl-completing-read-multiple-2 (prompt
				      table
				      &optional predicate
				      require-match initial-input
				      hist def inherit-input-method)
  "Read multiple strings in the minibuffer"
  (let ((ret (completing-read-multiple prompt table predicate
				       require-match initial-input
				       hist def inherit-input-method)))
    (if (and def (equal ret '("")))
	(split-string def crm-separator)
      ret)))

(defalias 'wl-completing-read-multiple 'completing-read-multiple)
(make-obsolete
 'wl-completing-read-multiple 'completing-read-multiple "24 May 2020")


(cond
 ((fboundp 'shell-command-read-minibuffer)
  (defun wl-read-shell-command (prompt &optional
				       initial-contents keymap read hist)
    (shell-command-read-minibuffer prompt default-directory
				   initial-contents keymap read hist)))
 (t
  (defalias 'wl-read-shell-command 'read-from-minibuffer)))

(defun wl-read-buffer (prompt &optional def require-match)
  (read-buffer prompt def require-match))

(require 'product)
(product-provide (provide 'wl-util) (require 'wl-version))

;;; wl-util.el ends here
