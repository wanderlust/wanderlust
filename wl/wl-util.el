;;; wl-util.el -- Utility modules for Wanderlust.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
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

(provide 'wl-util)
(eval-when-compile
  (require 'elmo-util))

(condition-case ()
    (require 'tm-edit)
  (error))
(condition-case ()
    (require 'pp)
  (error))
(eval-when-compile
  (mapcar
   (function
    (lambda (symbol)
      (unless (boundp symbol)
	(set (make-local-variable symbol) nil))))
   '(mule-version
     nemacs-version
     emacs-beta-version
     xemacs-codename
     mime-edit-insert-user-agent-field
     mime-edit-user-agent-value
     mime-editor/version
     mime-editor/codename))
  (require 'time-stamp)
  (defun-maybe read-event ())
  (defun-maybe next-command-event ())
  (defun-maybe event-to-character (a))
  (defun-maybe key-press-event-p (a))
  (defun-maybe button-press-event-p (a))
  (defun-maybe set-process-kanji-code (a b))
  (defun-maybe set-process-coding-system (a b c))
  (defun-maybe dispatch-event (a)))

(defalias 'wl-set-work-buf 'elmo-set-work-buf)
(make-obsolete 'wl-set-work-buf 'elmo-set-work-buf)

(defmacro wl-append (val func)
  (list 'if val
      (list 'nconc val func)
    (list 'setq val func)))

(defun wl-parse (string regexp &optional matchn)
  (or matchn (setq matchn 1))
  (let (list)
    (store-match-data nil)
    (while (string-match regexp string (match-end 0))
      (setq list (cons (substring string (match-beginning matchn)
                                  (match-end matchn)) list)))
    (nreverse list)))

(defun wl-delete-duplicates (list &optional all hack-addresses)
  "Delete duplicate equivalent strings from the list.
If ALL is t, then if there is more than one occurrence of a string in the list,
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

(defun wl-version (&optional with-codename)
  (format "%s %s%s" wl-appname wl-version
	  (if with-codename
	      (format " - \"%s\"" wl-codename) "")))

(defun wl-version-show ()
  (interactive)
  (message "%s" (wl-version t)))

;; from gnus
(defun wl-extended-emacs-version (&optional with-codename)
  "Stringified Emacs version"
  (interactive)
  (cond
   ((string-match "^\\([0-9]+\\.[0-9]+\\)\\.[.0-9]+$" emacs-version)
    (concat "Emacs " (wl-match-string 1 emacs-version)
	    (and (boundp 'mule-version)(concat "/Mule " mule-version))))
   ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		  emacs-version)
    (concat (wl-match-string 1 emacs-version)
	    (format " %d.%d" emacs-major-version emacs-minor-version)
	    (if (and (boundp 'emacs-beta-version)
		     emacs-beta-version)
		(format "b%d" emacs-beta-version))
	    (if with-codename
		(if (boundp 'xemacs-codename)
		    (concat " - \"" xemacs-codename "\"")))))
   (t emacs-version)))

(defun wl-extended-emacs-version2 (&optional delimiter with-codename)
  "Stringified Emacs version"
  (interactive)
  (cond
   ((and (boundp 'mule-version)
	 mule-version
	 (string-match "\\([0-9]+\.[0-9]+\\)\\(.*$\\)" mule-version))
    (format "Mule%s%s@%d.%d%s"
	    (or delimiter " ")
	    (wl-match-string 1 mule-version)
	    emacs-major-version
	    emacs-minor-version
	    (if with-codename
		(wl-match-string 2 mule-version)
	      "")))
   ((string-match "^\\([0-9]+\\.[0-9]+\\)\\.[.0-9]+$" emacs-version)
    (if (boundp 'nemacs-version)
	(concat "Nemacs" (or delimiter " ")
		nemacs-version
		"@"
		(substring emacs-version
			   (match-beginning 1)
			   (match-end 1)))
      (concat "Emacs" (or delimiter " ")
	      (wl-match-string 1 emacs-version))))
   ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		  emacs-version)
    (concat (wl-match-string 1 emacs-version)
	    (or delimiter " ")
	    (format "%d.%d" emacs-major-version emacs-minor-version)
	    (if (and (boundp 'emacs-beta-version)
		     emacs-beta-version)
		(format "b%d" emacs-beta-version))
	    (if (and with-codename
		     (boundp 'xemacs-codename)
		     xemacs-codename)
		(format " (%s)" xemacs-codename))))
   (t emacs-version)))

(defun wl-extended-emacs-version3 (&optional delimiter with-codename)
  "Stringified Emacs version"
  (interactive)
  (cond
   ((and (boundp 'mule-version)
	 mule-version
	 (string-match "\\([0-9]+\.[0-9]+\\)\\(.*$\\)" mule-version))
    (format "Emacs%s%d.%d Mule%s%s%s"
	    (or delimiter " ")
	    emacs-major-version
	    emacs-minor-version
	    (or delimiter " ")
	    (wl-match-string 1 mule-version)
	    (if with-codename
		(wl-match-string 2 mule-version)
	      "")))
   ((string-match "^\\([0-9]+\\.[0-9]+\\)\\.[.0-9]+$" emacs-version)
    (if (boundp 'nemacs-version)
	(let ((nemacs-codename-assoc '(("3.3.2" . " (FUJIMUSUME)")
				       ("3.3.1" . " (HINAMATSURI)")
				       ("3.2.3" . " (YUMENO-AWAYUKI)"))))
	  (format "Emacs%s%s Nemacs%s%s%s"
		  (or delimiter " ")
		  (wl-match-string 1 emacs-version)
		  (or delimiter " ")
		  nemacs-version
		  (or (and with-codename
			   (cdr (assoc nemacs-version
				       nemacs-codename-assoc)))
		      "")))
      (concat "Emacs" (or delimiter " ")
	      (wl-match-string 1 emacs-version))))
   ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		  emacs-version)
    (concat (wl-match-string 1 emacs-version)
	    (or delimiter " ")
	    (format "%d.%d" emacs-major-version emacs-minor-version)
	    (if (and (boundp 'emacs-beta-version)
		     emacs-beta-version)
		(format "b%d" emacs-beta-version))
	    (if (and with-codename
		     (boundp 'xemacs-codename)
		     xemacs-codename)
		(format " (%s)" xemacs-codename))))
   (t emacs-version)))

(defun wl-append-element (list element)
  (if element
      (append list (list element))
    list))

(defun wl-read-event-char ()
  "Get the next event."
  (let ((event (read-event)))
    ;; should be gnus-characterp, but this can't be called in XEmacs anyway
    (cons (and (numberp event) event) event)))

(defun wl-xmas-read-event-char ()
  "Get the next event."
  (let ((event (next-command-event)))
    (sit-for 0)
    ;; We junk all non-key events.  Is this naughty?
    (while (not (or (key-press-event-p event)
		    (button-press-event-p event)))
      (dispatch-event event)
      (setq event (next-command-event)))
    (cons (and (key-press-event-p event)
	       (event-to-character event))
	  event)))

(if running-xemacs
    (fset 'wl-read-event-char 'wl-xmas-read-event-char))

(defmacro wl-push (v l)
  (list 'setq l (list 'cons v l)))

(defmacro wl-pop (l)
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
;(make-obsolete 'wl-make-hash 'elmo-make-hash)

;(defalias 'wl-get-hash-val 'elmo-get-hash-val)
;(make-obsolete 'wl-get-hash-val 'elmo-get-hash-val)

;(defalias 'wl-set-hash-val 'elmo-set-hash-val)
;(make-obsolete 'wl-set-hash-val 'elmo-set-hash-val)

(defsubst wl-set-string-width (width string)
  (elmo-set-work-buf
   (elmo-set-buffer-multibyte default-enable-multibyte-characters)
   (insert string)
   (if (> (current-column) width)
       (if (> (move-to-column width) width)
	   (progn
	     (condition-case nil ; ignore error
		 (backward-char 1)
	       (error))
	     (concat (buffer-substring (point-min) (point)) " "))
	 (buffer-substring (point-min) (point)))
     (if (= (current-column) width)
	 string
       (concat string
	       (format (format "%%%ds"
			       (- width (current-column)))
		       " "))))))

(defun wl-display-bytes (num)
  (let (result remain)
    (cond
     ((> (setq result (/ num 1000000)) 0)
      (setq remain (% num 1000000))
      (if (> remain 400000)
	  (setq result (+ 1 result)))
      (format "%dM" result))
     ((> (setq result (/ num 1000)) 0)
      (setq remain (% num 1000))
      (if (> remain 400)
	  (setq result (+ 1 result)))
      (format "%dK" result))
     (t (format "%dB" result)))))

(defun wl-generate-user-agent-string ()
  "A candidate of wl-generate-mailer-string-func.
Insert User-Agent field instead of X-Mailer field."
  (let ((mime-user-agent (and (boundp 'mime-edit-insert-user-agent-field)
			      mime-edit-insert-user-agent-field
			      mime-edit-user-agent-value)))
    (if mime-user-agent
	(concat "User-Agent: "
		wl-appname "/" wl-version
		" (" wl-codename ") "
		mime-user-agent)
      (if (and (boundp 'mime-editor/version)
	       mime-editor/version)
	  (concat "User-Agent: "
		  wl-appname "/" wl-version
		  " (" wl-codename ") "
		  "tm/" mime-editor/version
		  (if (and (boundp 'mime-editor/codename)
			   mime-editor/codename)
		      (concat " (" mime-editor/codename ")"))
		  (if (and (boundp 'mime-library-product)
			   mime-library-product)
		      (concat " " (aref mime-library-product 0)
			      "/"
			      (mapconcat 'int-to-string
					 (aref mime-library-product 1)
					 ".")
			      " (" (aref mime-library-product 2) ")"))
		  (condition-case nil
		      (progn
			(require 'apel-ver)
			(concat " " (apel-version)))
		    (file-error nil))
		  " " (wl-extended-emacs-version3 "/" t))
	(concat "User-Agent: " wl-appname "/" wl-version " (" wl-codename ") "
		(wl-extended-emacs-version3 "/" t))))))

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
	(cond ((eq 'biff priority)
	       (when wl-biff-check-folder-list
		 (setq result (append result '(wl-biff-state-indicator)))))
	      ((eq 'plug priority)
	       (when wl-show-plug-status-on-modeline
		 (setq result (append result '(wl-plug-state-indicator)))))
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
  "Substring POSth matched string."
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
	       (,@ body))))
      (if wl-on-nemacs
	  (defmacro wl-as-coding-system (coding-system &rest body)
	    (` (let ((default-kanji-fileio-code (, coding-system))
		     (kanji-fileio-code (, coding-system))
		     kanji-expected-code)
		 (,@ body))))))))

(defmacro wl-as-mime-charset (mime-charset &rest body)
  (` (wl-as-coding-system (mime-charset-to-coding-system (, mime-charset))
       (,@ body))))

(defalias 'wl-string 'elmo-string)
(make-obsolete 'wl-string 'elmo-string)

(defun wl-parse-newsgroups (string &optional subscribe-only)
  (let* ((nglist (wl-parse string "[ \t\f\r\n,]*\\([^ \t\f\r\n,]+\\)"))
	 spec ret-val)
    (if (not subscribe-only)
	nglist
      (while nglist
	(if (intern-soft (car nglist) wl-folder-newsgroups-hashtb)
	    (wl-append ret-val (list (car nglist))))
	(setq nglist (cdr nglist)))
      ret-val)))

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
  "Inverse ALIST, copying.  Return an association list represents
the inverse mapping of ALIST, from objects to KEYS.
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
              (setq port (int-to-string elmo-default-nntp-port)))
	  (if (eq (length (setq server
                                (elmo-match-string 1 url))) 0)
              (setq server elmo-default-nntp-server))
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
       wl-message-buf-name)))

(defmacro wl-kill-buffers (regexp)
  (` (mapcar (function
	      (lambda (x)
		(if (and (buffer-name x)
			 (string-match (, regexp) (buffer-name x)))
		    (and (get-buffer x)
			 (kill-buffer x)))))
	     (buffer-list))))

(defun wl-sendlog-time ()
  (static-if (fboundp 'format-time-string)
      (format-time-string "%Y/%m/%d %T")
    (let ((date (current-time-string)))
      (format "%s/%02d/%02d %s"
	      (substring date -4)
	      (cdr (assoc (upcase (substring date 4 7))
			  timezone-months-assoc))
	      (string-to-int (substring date 8 10))
	      (substring date 11 19)))))

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

(static-if (fboundp 'read-directory-name)
    (defalias 'wl-read-directory-name 'read-directory-name)
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
     ".wl")))

(defun wl-draft-make-message-id-string ()
  (concat "<" (wl-unique-id) "@"
	  (or wl-message-id-domain
	      (if wl-local-domain
		  (concat (system-name) "." wl-local-domain)
		(system-name)))
	  ">"))

;;; Profile loading.
(defvar wl-load-profile-func 'wl-local-load-profile)
(defun wl-local-load-profile ()
  (message "Initializing ...")
  (load wl-init-file 'noerror 'nomessage))

(defun wl-load-profile ()
  (funcall wl-load-profile-func))

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

 ((condition-case nil (require 'timer) (error nil));; FSFmacs 19+
  (autoload 'run-at-time "timer")

  (defun wl-biff-stop ()
    (put 'wl-biff 'timer nil))

  (defun wl-biff-start ()
    (when wl-biff-check-folder-list
      (wl-biff-check-folders)
      (put 'wl-biff 'timer (run-at-time t wl-biff-check-interval
					'wl-biff-event-handler))))

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

(defun wl-biff-check-folders ()
  (interactive)
  (when (interactive-p)
    (message "Checking new mails..."))
  (let ((new-mails 0)
	(flist (or wl-biff-check-folder-list '("%inbox")))
	folder)
    (while flist
      (setq folder (car flist)
	    flist (cdr flist))
      (when (elmo-folder-plugged-p folder)
	(setq new-mails (+ new-mails
			   (nth 0 (wl-folder-check-one-entity folder))))))
    (setq wl-biff-state-indicator (if (zerop new-mails)
				      'wl-biff-state-indicator-off
				    'wl-biff-state-indicator-on))
    (force-mode-line-update t)
    (when (interactive-p)
      (cond ((zerop new-mails) (message "No mail."))
	    ((eq 1 new-mails) (message "You have a new mail."))
	    (t (message "You have %d new mails." new-mails))))))

;;; wl-util.el ends here
