;;; wl-util.el --- Utility modules for Wanderlust.

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
(eval-when-compile
  (require 'elmo-util))

(condition-case nil (require 'pp) (error nil))

(eval-when-compile
  (require 'time-stamp)
  (defalias-maybe 'next-command-event 'ignore)
  (defalias-maybe 'event-to-character 'ignore)
  (defalias-maybe 'key-press-event-p 'ignore)
  (defalias-maybe 'button-press-event-p 'ignore)
  (defalias-maybe 'set-process-kanji-code 'ignore)
  (defalias-maybe 'set-process-coding-system 'ignore)
  (defalias-maybe 'dispatch-event 'ignore))

(defalias 'wl-set-work-buf 'elmo-set-work-buf)
(make-obsolete 'wl-set-work-buf 'elmo-set-work-buf)

(defmacro wl-append (val func)
  (list 'if val
      (list 'nconc val func)
    (list 'setq val func)))

(defalias 'wl-parse 'elmo-parse)
(make-obsolete 'wl-parse 'elmo-parse)

(defun wl-delete-duplicates (list &optional all hack-addresses)
  "Delete duplicate equivalent strings from the LIST.
If ALL is t, then if there is more than one occurrence of a string in the LIST,
 then all occurrences of it are removed instead of just the subsequent ones.
If HACK-ADDRESSES is t, then the strings are considered to be mail addresses,
 and only the address part is compared (so that \"Name <foo>\" and \"foo\"
 would be considered to be equivalent.)"
  (let ((hashtable (make-vector 29 0))
	(new-list nil)
	sym-string sym)
    (fillarray hashtable 0)
    (while list
      (setq sym-string
	    (if hack-addresses
		(wl-address-header-extract-address (car list))
	      (car list))
	    sym-string (or sym-string "-unparseable-garbage-")
	    sym (intern sym-string hashtable))
      (if (boundp sym)
	  (and all (setcar (symbol-value sym) nil))
	(setq new-list (cons (car list) new-list))
	(set sym new-list))
      (setq list (cdr list)))
    (delq nil (nreverse new-list))))

;; string utils.
(defalias 'wl-string-member 'elmo-string-member)
(defalias 'wl-string-match-member 'elmo-string-match-member)
(defalias 'wl-string-delete-match 'elmo-string-delete-match)
(defalias 'wl-string-match-assoc 'elmo-string-match-assoc)
(defalias 'wl-string-assoc 'elmo-string-assoc)
(defalias 'wl-string-rassoc 'elmo-string-rassoc)

(defun wl-parse-addresses (string)
  (if (null string)
      ()
    (elmo-set-work-buf
     ;;(unwind-protect
     (let (list start s char)
       (insert string)
       (goto-char (point-min))
       (skip-chars-forward "\t\f\n\r ")
       (setq start (point))
       (while (not (eobp))
	 (skip-chars-forward "^\"\\,(")
	 (setq char (following-char))
	 (cond ((= char ?\\)
		(forward-char 1)
		(if (not (eobp))
		    (forward-char 1)))
	       ((= char ?,)
		(setq s (buffer-substring start (point)))
		(if (or (null (string-match "^[\t\f\n\r ]+$" s))
			(not (string= s "")))
		    (setq list (cons s list)))
		(skip-chars-forward ",\t\f\n\r ")
		(setq start (point)))
	       ((= char ?\")
		(re-search-forward "[^\\]\"" nil 0))
	       ((= char ?\()
		(let ((parens 1))
		  (forward-char 1)
		  (while (and (not (eobp)) (not (zerop parens)))
		    (re-search-forward "[()]" nil 0)
		    (cond ((or (eobp)
			       (= (char-after (- (point) 2)) ?\\)))
			  ((= (preceding-char) ?\()
			   (setq parens (1+ parens)))
			  (t
			   (setq parens (1- parens)))))))))
       (setq s (buffer-substring start (point)))
       (if (and (null (string-match "^[\t\f\n\r ]+$" s))
		(not (string= s "")))
	   (setq list (cons s list)))
       (nreverse list)) ; jwz: fixed order
     )))

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

(defun wl-ask-folder (func mes-string)
  (let* (key keve
	     (cmd (if (featurep 'xemacs)
		      (event-to-character last-command-event)
		    (string-to-char (format "%s" (this-command-keys))))))
    (message mes-string)
    (setq key (car (setq keve (wl-read-event-char))))
    (if (or (equal key ?\ )
	    (and cmd
		 (equal key cmd)))
	(progn
	  (message "")
	  (funcall func))
      (wl-push (cdr keve) unread-command-events))))

;(defalias 'wl-make-hash 'elmo-make-hash)
;;(make-obsolete 'wl-make-hash 'elmo-make-hash)

;;(defalias 'wl-get-hash-val 'elmo-get-hash-val)
;;(make-obsolete 'wl-get-hash-val 'elmo-get-hash-val)

;;(defalias 'wl-set-hash-val 'elmo-set-hash-val)
;;(make-obsolete 'wl-set-hash-val 'elmo-set-hash-val)

(defsubst wl-set-string-width (width string &optional padding ignore-invalid)
  "Make a new string which have specified WIDTH and content of STRING.
`wl-invalid-character-message' is used when invalid character is contained.
If WIDTH is negative number, padding chars are added to the head and
otherwise, padding chars are added to the tail of the string.
The optional 3rd arg PADDING, if non-nil, specifies a padding character
to add the result instead of white space.
If optional 4th argument is non-nil, don't use `wl-invalid-character-message'
even when invalid character is contained."
  (static-cond
   ((and (fboundp 'string-width) (fboundp 'truncate-string-to-width)
	 (not (featurep 'xemacs)))
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
		       (or padding ?\ ))))
	(if (< width 0)
	    (concat paddings string)
	  (concat string paddings)))))
   (t
    (elmo-set-work-buf
     (elmo-set-buffer-multibyte default-enable-multibyte-characters)
     (insert string)
     (when (> (current-column) (abs width))
       (when (> (move-to-column (abs width)) (abs width))
	 (condition-case nil ; ignore error
	     (backward-char 1)
	   (error)))
       (setq string (buffer-substring (point-min) (point))))
     (if (= (current-column) (abs width))
	 string
       (let ((paddings (make-string (- (abs width) (current-column))
				    (or padding ?\ ))))
	 (if (< width 0)
	     (concat paddings string)
	   (concat string paddings))))))))

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
(make-obsolete 'wl-display-error 'elmo-display-error)

(defun wl-get-assoc-list-value (assoc-list folder &optional match)
  (catch 'found
    (let ((alist assoc-list)
	  value pair)
      (while alist
	(setq pair (car alist))
	(if (string-match (car pair) folder)
	    (cond ((eq match 'all)
		   (setq value (append value (list (cdr pair)))))
		  ((eq match 'all-list)
		   (setq value (append value (cdr pair))))
		  ((not match)
		   (throw 'found (cdr pair)))))
	(setq alist (cdr alist)))
      value)))

(defmacro wl-match-string (pos string)
  "Substring POSth matched STRING."
  (` (substring (, string) (match-beginning (, pos)) (match-end (, pos)))))

(defmacro wl-match-buffer (pos)
  "Substring POSth matched from the current buffer."
  (` (buffer-substring-no-properties
      (match-beginning (, pos)) (match-end (, pos)))))

(put 'wl-as-coding-system 'lisp-indent-function 1)
(put 'wl-as-mime-charset 'lisp-indent-function 1)

(eval-and-compile
  (if wl-on-mule3
      (defmacro wl-as-coding-system (coding-system &rest body)
	(` (let ((coding-system-for-read (, coding-system))
		 (coding-system-for-write (, coding-system)))
	     (,@ body))))
    (if wl-on-mule
	(defmacro wl-as-coding-system (coding-system &rest body)
	  (` (let ((file-coding-system-for-read (, coding-system))
		   (file-coding-system (, coding-system)))
	       (,@ body)))))))

(defmacro wl-as-mime-charset (mime-charset &rest body)
  (` (wl-as-coding-system (mime-charset-to-coding-system (, mime-charset))
       (,@ body))))

(defalias 'wl-string 'elmo-string)
(make-obsolete 'wl-string 'elmo-string)

;; Check if active region exists or not.
(if (boundp 'mark-active)
    (defmacro wl-region-exists-p ()
      'mark-active)
  (if (fboundp 'region-exists-p)
      (defmacro wl-region-exists-p ()
	(list 'region-exists-p))))

(if (not (fboundp 'overlays-in))
    (defun overlays-in (beg end)
      "Return a list of the overlays that overlap the region BEG ... END.
Overlap means that at least one character is contained within the overlay
and also contained within the specified region.
Empty overlays are included in the result if they are located at BEG
or between BEG and END."
      (let ((ovls (overlay-lists))
	    tmp retval)
	(if (< end beg)
	    (setq tmp end
		  end beg
		  beg tmp))
	(setq ovls (nconc (car ovls) (cdr ovls)))
	(while ovls
	  (setq tmp (car ovls)
		ovls (cdr ovls))
	  (if (or (and (<= (overlay-start tmp) end)
		       (>= (overlay-start tmp) beg))
		  (and (<= (overlay-end tmp) end)
		       (>= (overlay-end tmp) beg)))
	      (setq retval (cons tmp retval))))
	retval)))

(defsubst wl-repeat-string (str times)
  (let ((loop times)
	ret-val)
    (while (> loop 0)
      (setq ret-val (concat ret-val str))
      (setq loop (- loop 1)))
    ret-val))

(defun wl-list-diff (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2."
  (let ((list1 (copy-sequence list1)))
    (while list2
      (setq list1 (delq (car list2) list1))
      (setq list2 (cdr list2)))
    list1))

(defun wl-append-assoc-list (item value alist)
  "make assoc list '((item1 value1-1 value1-2 ...)) (item2 value2-1 ...)))"
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

(eval-when-compile
  (require 'static))
(static-unless (fboundp 'pp)
  (defvar pp-escape-newlines t)
  (defun pp (object &optional stream)
    "Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see)."
    (princ (pp-to-string object) (or stream standard-output)))

  (defun pp-to-string (object)
    "Return a string containing the pretty-printed representation of OBJECT,
any Lisp object.  Quoting characters are used when needed to make output
that `read' can handle, whenever this is possible."
    (save-excursion
      (set-buffer (generate-new-buffer " pp-to-string"))
      (unwind-protect
	  (progn
	    (lisp-mode-variables t)
	    (let ((print-escape-newlines pp-escape-newlines))
	      (prin1 object (current-buffer)))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (cond
	       ((looking-at "\\s(\\|#\\s(")
		(while (looking-at "\\s(\\|#\\s(")
		  (forward-char 1)))
	       ((and (looking-at "\\(quote[ \t]+\\)\\([^.)]\\)")
		     (> (match-beginning 1) 1)
		     (= ?\( (char-after (1- (match-beginning 1))))
		     ;; Make sure this is a two-element list.
		     (save-excursion
		       (goto-char (match-beginning 2))
		       (forward-sexp)
		       ;; Avoid mucking with match-data; does this test work?
		       (char-equal ?\) (char-after (point)))))
		;; -1 gets the paren preceding the quote as well.
		(delete-region (1- (match-beginning 1)) (match-end 1))
		(insert "'")
		(forward-sexp 1)
		(if (looking-at "[ \t]*\)")
		    (delete-region (match-beginning 0) (match-end 0))
		  (error "Malformed quote"))
		(backward-sexp 1))
	       ((condition-case err-var
		    (prog1 t (down-list 1))
		  (error nil))
		(backward-char 1)
		(skip-chars-backward " \t")
		(delete-region
		 (point)
		 (progn (skip-chars-forward " \t") (point)))
		(if (not (char-equal ?' (char-after (1- (point)))))
		    (insert ?\n)))
	       ((condition-case err-var
		    (prog1 t (up-list 1))
		  (error nil))
		(while (looking-at "\\s)")
		  (forward-char 1))
		(skip-chars-backward " \t")
		(delete-region
		 (point)
		 (progn (skip-chars-forward " \t") (point)))
		(if (not (char-equal ?' (char-after (1- (point)))))
		    (insert ?\n)))
	       (t (goto-char (point-max)))))
	    (goto-char (point-min))
	    (indent-sexp)
	    (buffer-string))
	(kill-buffer (current-buffer))))))

(defsubst wl-get-date-iso8601 (date)
  (or (get-text-property 0 'wl-date date)
      (let* ((d1 (timezone-fix-time date nil nil))
	     (time (format "%04d%02d%02dT%02d%02d%02d"
			   (aref d1 0) (aref d1 1) (aref d1 2)
			   (aref d1 3) (aref d1 4) (aref d1 5))))
	(put-text-property 0 1 'wl-date time date)
	time)))

(defun wl-make-date-string ()
  (let ((s (current-time-string)))
    (string-match "\\`\\([A-Z][a-z][a-z]\\) +[A-Z][a-z][a-z] +[0-9][0-9]? *[0-9][0-9]?:[0-9][0-9]:[0-9][0-9] *[0-9]?[0-9]?[0-9][0-9]"
		  s)
    (concat (wl-match-string 1 s) ", "
	    (timezone-make-date-arpa-standard s (current-time-zone)))))

(defun wl-date-iso8601 (date)
  "Convert the DATE to YYMMDDTHHMMSS."
  (condition-case ()
      (wl-get-date-iso8601 date)
    (error "")))

(defun wl-day-number (date)
  (let ((dat (mapcar '(lambda (s) (and s (string-to-int s)) )
		     (timezone-parse-date date))))
    (timezone-absolute-from-gregorian
     (nth 1 dat) (nth 2 dat) (car dat))))

(defun wl-url-news (url &rest args)
  (interactive "sURL: ")
  (if (string-match "^news:\\(.*\\)$" url)
      (wl-summary-goto-folder-subr
       (concat "-" (elmo-match-string 1 url)) nil nil nil t)
    (message "Not a news: url.")))

(defun wl-url-nntp (url &rest args)
  (interactive "sURL: ")
  (let (folder fld-name server port msg)
    (if (string-match
	 "^nntp://\\([^:/]*\\):?\\([0-9]*\\)/\\([^/]*\\)/\\([0-9]*\\).*$" url)
	(progn
	  (if (eq (length (setq fld-name
				(elmo-match-string 3 url))) 0)
	      (setq fld-name nil))
	  (if (eq (length (setq port
				(elmo-match-string 2 url))) 0)
	      (setq port (int-to-string elmo-nntp-default-port)))
	  (if (eq (length (setq server
				(elmo-match-string 1 url))) 0)
	      (setq server elmo-nntp-default-server))
	  (setq folder (concat "-" fld-name "@" server ":" port))
	  (if (eq (length (setq msg
				(elmo-match-string 4 url))) 0)
	      (wl-summary-goto-folder-subr
	       folder nil nil nil t)
	    (wl-summary-goto-folder-subr
	     folder 'update nil nil t)
	    (goto-char (point-min))
	    (re-search-forward (concat "^ *" msg) nil t)
	    (wl-summary-redisplay)))
      (message "Not a nntp: url."))))

(defmacro wl-concat-list (list separator)
  (` (mapconcat 'identity (delete "" (delq nil (, list))) (, separator))))

(defmacro wl-current-message-buffer ()
  (` (save-excursion
       (if (buffer-live-p wl-current-summary-buffer)
	   (set-buffer wl-current-summary-buffer))
       wl-message-buffer)))

(defmacro wl-kill-buffers (regexp)
  (` (mapcar (function
	      (lambda (x)
		(if (and (buffer-name x)
			 (string-match (, regexp) (buffer-name x)))
		    (and (get-buffer x)
			 (kill-buffer x)))))
	     (buffer-list))))

(defun wl-collect-summary ()
  (let (result)
    (mapcar
     (function (lambda (x)
		 (if (and (string-match "^Summary"
					(buffer-name x))
			  (save-excursion
			    (set-buffer x)
			    (equal major-mode 'wl-summary-mode)))
		     (setq result (nconc result (list x))))))
     (buffer-list))
    result))

(defun wl-collect-draft ()
  (let ((draft-regexp (concat
		       "^" (regexp-quote
			    (elmo-localdir-folder-directory-internal
			     (wl-folder-get-elmo-folder wl-draft-folder)))))
	result buf)
    (mapcar
     (function (lambda (x)
		 (if (and
		      (setq buf (with-current-buffer x
				  wl-draft-buffer-file-name))
		      (string-match draft-regexp buf))
		     (setq result (nconc result (list x))))))
     (buffer-list))
    result))

(static-if (fboundp 'read-directory-name)
    (defun wl-read-directory-name (prompt dir)
      (read-directory-name prompt dir dir))
  (defun wl-read-directory-name (prompt dir)
    (let ((dir (read-file-name prompt dir)))
      (unless (file-directory-p dir)
	(error "%s is not directory" dir))
      dir)))

;; local variable check.
(static-if (fboundp 'local-variable-p)
    (defalias 'wl-local-variable-p 'local-variable-p)
  (defmacro wl-local-variable-p (symbol &optional buffer)
    (` (if (assq (, symbol) (buffer-local-variables (, buffer)))
	   t))))

(defun wl-number-base36 (num len)
  (if (if (< len 0)
	  (<= num 0)
	(= len 0))
      ""
    (concat (wl-number-base36 (/ num 36) (1- len))
	    (char-to-string (aref "zyxwvutsrqponmlkjihgfedcba9876543210"
				  (% num 36))))))

(defvar wl-unique-id-char nil)

(defun wl-unique-id ()
  ;; Don't use microseconds from (current-time), they may be unsupported.
  ;; Instead we use this randomly inited counter.
  (setq wl-unique-id-char
	(% (1+ (or wl-unique-id-char (logand (random t) (1- (lsh 1 20)))))
	   ;; (current-time) returns 16-bit ints,
	   ;; and 2^16*25 just fits into 4 digits i base 36.
	   (* 25 25)))
  (let ((tm (static-if (fboundp 'current-time)
		(current-time)
	      (let* ((cts (split-string (current-time-string) "[ :]"))
		     (m (cdr (assoc (nth 1 cts)
				    '(("Jan" . "01") ("Feb" . "02")
				      ("Mar" . "03") ("Apr" . "04")
				      ("May" . "05") ("Jun" . "06")
				      ("Jul" . "07") ("Aug" . "08")
				      ("Sep" . "09") ("Oct" . "10")
				      ("Nov" . "11") ("Dec" . "12"))))))
		(list (string-to-int (concat (nth 6 cts) m
					     (substring (nth 2 cts) 0 1)))
		      (string-to-int (concat (substring (nth 2 cts) 1)
					     (nth 4 cts) (nth 5 cts)
					     (nth 6 cts))))))))
    (concat
     (if (memq system-type '(ms-dos emx vax-vms))
	 (let ((user (downcase (user-login-name))))
	   (while (string-match "[^a-z0-9_]" user)
	     (aset user (match-beginning 0) ?_))
	   user)
       (wl-number-base36 (user-uid) -1))
     (wl-number-base36 (+ (car   tm)
			  (lsh (% wl-unique-id-char 25) 16)) 4)
     (wl-number-base36 (+ (nth 1 tm)
			  (lsh (/ wl-unique-id-char 25) 16)) 4)
     ;; Append the name of the message interface, because while the
     ;; generated ID is unique to this newsreader, other newsreaders
     ;; might otherwise generate the same ID via another algorithm.
     wl-unique-id-suffix)))

(defvar wl-message-id-function 'wl-draft-make-message-id-string)
(defun wl-draft-make-message-id-string ()
  "Return Message-ID field value."
  (concat "<" (wl-unique-id)
	  (let (from user domain)
	    (if (and wl-message-id-use-wl-from
		     (progn
		       (setq from (wl-address-header-extract-address wl-from))
		       (and (string-match "^\\(.*\\)@\\(.*\\)$" from)
			    (setq user   (match-string 1 from))
			    (setq domain (match-string 2 from)))))
		(format "%%%s@%s>" user domain)
	      (format "@%s>"
		      (or wl-message-id-domain
			  (if wl-local-domain
			      (concat (system-name) "." wl-local-domain)
			    (system-name))))))))

;;; Profile loading.
(defvar wl-load-profile-function 'wl-local-load-profile)
(defun wl-local-load-profile ()
  "Load `wl-init-file'."
  (message "Initializing ...")
  (load wl-init-file 'noerror 'nomessage))

(defun wl-load-profile ()
  "Call `wl-load-profile-function' function."
  (funcall wl-load-profile-function))

;;;

(defmacro wl-count-lines ()
  (` (save-excursion
       (beginning-of-line)
       (count-lines 1 (point)))))

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

;; Biff
(static-cond
 (wl-on-xemacs
  (defvar wl-biff-timer-name "wl-biff")

  (defun wl-biff-stop ()
    (when (get-itimer wl-biff-timer-name)
      (delete-itimer wl-biff-timer-name)))

  (defun wl-biff-start ()
    (wl-biff-stop)
    (when wl-biff-check-folder-list
      (wl-biff-check-folders)
      (start-itimer wl-biff-timer-name 'wl-biff-check-folders
		    wl-biff-check-interval wl-biff-check-interval))))

 ((and (condition-case nil (require 'timer) (error nil));; FSFmacs 19+
       (fboundp 'timer-activate))

  (defun wl-biff-stop ()
    (when (get 'wl-biff 'timer)
      (cancel-timer (get 'wl-biff 'timer))))

  (defun wl-biff-start ()
    (require 'timer)
    (when wl-biff-check-folder-list
      (wl-biff-check-folders)
      (if (get 'wl-biff 'timer)
	  (timer-activate (get 'wl-biff 'timer))
	(put 'wl-biff 'timer (run-at-time
			      (timer-next-integral-multiple-of-time
			       (current-time) wl-biff-check-interval)
			      wl-biff-check-interval
			      'wl-biff-event-handler)))))

  (defun-maybe timer-next-integral-multiple-of-time (time secs)
    "Yield the next value after TIME that is an integral multiple of SECS.
More precisely, the next value, after TIME, that is an integral multiple
of SECS seconds since the epoch.  SECS may be a fraction.
This function is imported from Emacs 20.7."
    (let ((time-base (ash 1 16)))
      (if (fboundp 'atan)
	  ;; Use floating point, taking care to not lose precision.
	  (let* ((float-time-base (float time-base))
		 (million 1000000.0)
		 (time-usec (+ (* million
				  (+ (* float-time-base (nth 0 time))
				     (nth 1 time)))
			       (nth 2 time)))
		 (secs-usec (* million secs))
		 (mod-usec (mod time-usec secs-usec))
		 (next-usec (+ (- time-usec mod-usec) secs-usec))
		 (time-base-million (* float-time-base million)))
	    (list (floor next-usec time-base-million)
		  (floor (mod next-usec time-base-million) million)
		  (floor (mod next-usec million))))
	;; Floating point is not supported.
	;; Use integer arithmetic, avoiding overflow if possible.
	(let* ((mod-sec (mod (+ (* (mod time-base secs)
				   (mod (nth 0 time) secs))
				(nth 1 time))
			     secs))
	       (next-1-sec (+ (- (nth 1 time) mod-sec) secs)))
	  (list (+ (nth 0 time) (floor next-1-sec time-base))
		(mod next-1-sec time-base)
		0)))))

  (defun wl-biff-event-handler ()
    ;; PAKURing from FSF:time.el
    (wl-biff-check-folders)
    ;; Do redisplay right now, if no input pending.
    (sit-for 0)
    (let* ((current (current-time))
	   (timer (get 'wl-biff 'timer))
	   ;; Compute the time when this timer will run again, next.
	   (next-time (timer-relative-time
		       (list (aref timer 1) (aref timer 2) (aref timer 3))
		       (* 5 (aref timer 4)) 0)))
      ;; If the activation time is far in the past,
      ;; skip executions until we reach a time in the future.
      ;; This avoids a long pause if Emacs has been suspended for hours.
      (or (> (nth 0 next-time) (nth 0 current))
	  (and (= (nth 0 next-time) (nth 0 current))
	       (> (nth 1 next-time) (nth 1 current)))
	  (and (= (nth 0 next-time) (nth 0 current))
	       (= (nth 1 next-time) (nth 1 current))
	       (> (nth 2 next-time) (nth 2 current)))
	  (progn
	    (timer-set-time timer (timer-next-integral-multiple-of-time
				   current wl-biff-check-interval)
			    wl-biff-check-interval)
	    (timer-activate timer))))))
 (t
  (fset 'wl-biff-stop 'ignore)
  (fset 'wl-biff-start 'ignore)))

(defsubst wl-biff-notify (new-mails notify-minibuf)
  (when (and (not wl-modeline-biff-status) (> new-mails 0))
    (run-hooks 'wl-biff-notify-hook))
  (when (and wl-modeline-biff-status (eq new-mails 0))
    (run-hooks 'wl-biff-unnotify-hook))
  (setq wl-modeline-biff-status (> new-mails 0))
  (force-mode-line-update t)
  (when notify-minibuf
    (cond ((zerop new-mails) (message "No mail."))
	  ((= 1 new-mails) (message "You have a new mail."))
	  (t (message "You have %d new mails." new-mails)))))

;; Internal variable.
(defvar wl-biff-check-folders-running nil)

(defun wl-biff-check-folders ()
  (interactive)
  (if wl-biff-check-folders-running
      (when (interactive-p)
	(message "Biff process is running."))
    (setq wl-biff-check-folders-running t)
    (when (interactive-p)
      (message "Checking new mails..."))
    (let ((new-mails 0)
	  (flist (or wl-biff-check-folder-list (list wl-default-folder)))
	  folder)
      (if (eq (length flist) 1)
	  (wl-biff-check-folder-async (wl-folder-get-elmo-folder
				       (car flist) 'biff) (interactive-p))
	(unwind-protect
	    (while flist
	      (setq folder (wl-folder-get-elmo-folder (car flist))
		    flist (cdr flist))
	      (when (elmo-folder-plugged-p folder)
		(setq new-mails
		      (+ new-mails
			 (nth 0 (wl-biff-check-folder folder))))))
	  (setq wl-biff-check-folders-running nil)
	  (wl-biff-notify new-mails (interactive-p)))))))

(defun wl-biff-check-folder (folder)
  (if (eq (elmo-folder-type-internal folder) 'pop3)
      (unless (elmo-pop3-get-session folder 'if-exists)
	(wl-folder-check-one-entity (elmo-folder-name-internal folder)
				    'biff))
    (wl-folder-check-one-entity (elmo-folder-name-internal folder)
				'biff)))

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
  (when (elmo-folder-plugged-p folder)
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
	(setq wl-biff-check-folders-running nil)))))

(if (and (fboundp 'regexp-opt)
	 (not (featurep 'xemacs)))
    (defalias 'wl-regexp-opt 'regexp-opt)
  (defun wl-regexp-opt (strings &optional paren)
    "Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct."
    (let ((open-paren (if paren "\\(" "")) (close-paren (if paren "\\)" "")))
      (concat open-paren (mapconcat 'regexp-quote strings "\\|")
	      close-paren))))

(defalias 'wl-expand-newtext 'elmo-expand-newtext)

(defun wl-region-exists-p ()
  "Return non-nil if a region exists on current buffer."
  (static-if (featurep 'xemacs)
      (and zmacs-regions zmacs-region-active-p)
    (and transient-mark-mode mark-active)))

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
  (` (let (byte-compile-warnings)
       (setq (, formatter)
	     (byte-compile
	      (list 'lambda ()
		    (wl-line-parse-format (, format) (, alist)))))
       (when (get-buffer "*Compile-Log*")
	 (bury-buffer "*Compile-Log*"))
       (when (get-buffer "*Compile-Log-Show*")
	 (bury-buffer "*Compile-Log-Show*")))))

(require 'product)
(product-provide (provide 'wl-util) (require 'wl-version))

;;; wl-util.el ends here
