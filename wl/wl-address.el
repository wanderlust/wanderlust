;;; wl-address.el -- Tiny address management for Wanderlust.

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

(require 'wl-util)
(require 'wl-vars)
(require 'std11)

(defvar wl-address-complete-header-regexp "^\\(To\\|From\\|Cc\\|Bcc\\|Mail-Followup-To\\|Reply-To\\|Return-Receipt-To\\):")
(defvar wl-newsgroups-complete-header-regexp "^\\(Newsgroups\\|Followup-To\\):")
(defvar wl-folder-complete-header-regexp "^\\(FCC\\):")
(defvar wl-address-list nil)
(defvar wl-address-completion-list nil)
(defvar wl-address-petname-hash nil)

(defvar wl-address-ldap-search-hash nil)

(eval-when-compile (require 'pldap))

(defvar wl-ldap-alias-dn-level nil
"Level of dn data to make alias postfix.
Valid value is nit, t, 1 or larget integer.

If this value nil, minimum alias postfix is made depends on uniqness
with other candidates. In this implementation, it's same to 1.  If t,
always append all dn data. If number, always append spcified level of
data but maybe appended more uniqness.  If invalid value, treat as
nil.

For example, following dn data is exsist, alias of each level is shown
bellow.

Match: Goto
dn: CN=Shun-ichi GOTO,OU=Mew,OU=Emacs,OU=Lisper,O=Programmers Inc.
  nil => Goto/Shun-ichi_GOTO
    1 => Goto/Shun-ichi_GOTO
    2 => Goto/Shun-ichi_GOTO/Mew
    3 => Goto/Shun-ichi_GOTO/Mew/Emacs
    4 => Goto/Shun-ichi_GOTO/Mew/Emacs/Lisper
    5 => Goto/Shun-ichi_GOTO/Mew/Emacs/Lisper/Programmers_Inc_
    6 => Goto/Shun-ichi_GOTO/Mew/Emacs/Lisper/Programmers_Inc_
    t => Goto/Shun-ichi_GOTO/Mew/Emacs/Lisper/Programmers_Inc_

If level 3 is required for uniqness with other candidates,
  nil => Goto/Shun-ichi_GOTO/Mew/Emacs    ... appended more
    1 => Goto/Shun-ichi_GOTO/Mew/Emacs    ... appended more
    2 => Goto/Shun-ichi_GOTO/Mew/Emacs    ... appended more
    3 => Goto/Shun-ichi_GOTO/Mew/Emacs
    4 => Goto/Shun-ichi_GOTO/Mew/Emacs/Lisper
    (so on...)
")

(defconst wl-ldap-alias-sep "/")

(defconst wl-ldap-search-attribute-type-list
  '("sn" "cn" "mail"))

(defun wl-ldap-get-value (type entry)
  ""
  (let* ((values (cdr (assoc type entry)))
	 (ret (car values)))
    (if (and ret (not ldap-ignore-attribute-codings))
	(while values
	  (if (not (string-match "^[\000-\177]*$" (car values)))
	      (setq ret (car values)
		    values nil)
	    (setq values (cdr values)))))
    ret))
	  
(defun wl-ldap-get-value-list (type entry)
  ""
  (cdr (assoc type entry)))

(defun wl-ldap-make-filter (pat type-list)
  "Make RFC1558 quiery filter for PAT from ATTR-LIST.
Each are \"OR\" combination, and PAT is beginning-match."
  (concat "(&(objectclass=person)(|"
	  (mapconcat (lambda (x) (format "(%s=%s*)" x pat)) ; fixed format
		     type-list
		     "")
	  "))"))

(defun wl-ldap-make-matched-value-list (regexp type-list entry)
  "Correct matching WORD with value of TYPE-LIST in ENTRY.
Returns matched uniq string list."
  (let (type val values result)
    ;; collect matching value
    (while entry
      (setq type (car (car entry))
	    values (mapcar (function wl-ldap-alias-safe-string)
			   (cdr (car entry)))
	    entry (cdr entry))
      (if (string-match "::?$" type)
	  (setq type (substring type 0 (match-beginning 0))))
      (if (member type type-list)
	  (while values
	    (setq val (car values)
		  values (cdr values))
	    (if (and (string-match regexp val)
		     (not (member val result)))
		(setq result (cons val result))))))
    result))

(defun wl-ldap-alias-safe-string (str)
  "Modify STR for alias.
Replace space/tab in STR into '_' char.
And remove domain part of mail addr."
  (while (string-match "[^_a-zA-Z0-9+@%.!\\-/]+" str)
    (setq str (concat (substring str 0 (match-beginning 0))
		      "_"
		      (substring str (match-end 0)))))
  (if (string-match "@[^/@]+" str)
      (setq str (concat (substring str 0 (match-beginning 0))
			(substring str (match-end 0)))))
  str)

(defun wl-ldap-register-dn-string (hash dn &optional str dn-list)
  ""
  (let (sym dnsym value level)
    (setq dnsym (intern (upcase dn) hash))
    (if (and (null str) (boundp dnsym))
	()					; already processed
      ;; make dn-list in fisrt time
      (if (null dn-list)
	  (let ((case-fold-search t))
	    (setq dn-list (mapcar (lambda (str)
				    (if (string-match "[a-z]+=\\(.*\\)" str)
					(wl-ldap-alias-safe-string
					 (wl-match-string 1 str))))
				  (split-string dn ",")))))
      ;; prepare candidate for uniq str
      (if str 
	  (setq str (concat str wl-ldap-alias-sep (car dn-list))
		dn-list (cdr dn-list))
	;; first entry, pre-build with given level
	(cond 
	 ((null wl-ldap-alias-dn-level) (setq level 1))
	 ((eq t wl-ldap-alias-dn-level) (setq level 1000)) ; xxx, big enough
	 ((numberp wl-ldap-alias-dn-level)
	  (if (< 0 wl-ldap-alias-dn-level)
	      (setq level  wl-ldap-alias-dn-level)
	    (setq level 1)))
	 (t
	  (setq level 1)))
	(while (and (< 0 level) dn-list)
	  (if (null str)
	      (setq str (car dn-list))
	    (setq str (concat str wl-ldap-alias-sep (car dn-list))))
	  (setq level (1- level)
		dn-list (cdr dn-list))))
      (setq sym (intern (upcase str) hash))
      (if (not (boundp sym))
	  ;; good
	  (progn (set sym (list dn str dn-list))
		 (set dnsym str))
	;; conflict
	(if (not (eq (setq value (symbol-value sym)) t))
	    ;; move away deeper
	    (progn (set sym t)
		   (apply (function wl-ldap-register-dn-string) hash value)))
	(wl-ldap-register-dn-string hash dn str dn-list)))))

(defun wl-address-ldap-search (pattern cl)
  "Make address completion-list matched for PATTERN by LDAP search.
Matched address lists are append to CL."
  (require 'pldap)
  (unless wl-address-ldap-search-hash
    (setq wl-address-ldap-search-hash (elmo-make-hash 7)))
  (let ((pat (if (string-match wl-ldap-alias-sep pattern)
		 (substring pattern 0 (match-beginning 0))
	       pattern))
	(ldap-default-host wl-ldap-server)
	(ldap-default-port (or wl-ldap-port 389))
	(ldap-default-base wl-ldap-base)
	(dnhash (elmo-make-hash))
	cache len sym tmpl regexp entries ent values dn dnstr alias
	result cn mails)
    ;; check cache
    (mapatoms (lambda (atom)
		(if (and (string-match 
			  (concat "^" (symbol-name atom) ".*") pat)
			 (or (null cache)
			     (< (car cache) 
				(setq len (length (symbol-name atom))))))
		    (setq cache (cons
				 (or len (length (symbol-name atom)))
				 (symbol-value atom)))))
	      wl-address-ldap-search-hash)
    ;; get matched entries
    (if cache
	(setq entries (cdr cache))
      (condition-case nil 
	  (progn
	    (message "Searching in LDAP...")
	    (setq entries (ldap-search-entries
			   (wl-ldap-make-filter 
			    (concat pat "*")
			    wl-ldap-search-attribute-type-list)
			   nil wl-ldap-search-attribute-type-list nil t))
	    (message "Searching in LDAP...done")
	    (elmo-set-hash-val pattern entries wl-address-ldap-search-hash))
	(error (message ""))))			; ignore error: No such object
    ;;
    (setq tmpl entries)
    (while tmpl
      (wl-ldap-register-dn-string dnhash (car (car tmpl))) ; car is 'dn'.
      (setq tmpl (cdr tmpl)))
    ;;
    (setq regexp (concat "^" pat))
    (while entries
      (setq ent (cdar entries)
	    values (wl-ldap-make-matched-value-list
		    regexp '("mail" "sn" "cn") ent)
	    mails (wl-ldap-get-value-list "mail" ent)
	    cn (wl-ldap-get-value "cn" ent)
	    dn (car (car entries))
	    dnstr (elmo-get-hash-val (upcase dn) dnhash))
      ;; make alias list generated from LDAP data.
      (while (and mails values)
	;; make alias like MATCHED/DN-STRING
	(if (not (string-match (concat "^" (regexp-quote (car values))) dnstr))
	    (setq alias (concat (car values) wl-ldap-alias-sep dnstr))
	  ;; use DN-STRING if DN-STRING begin with MATCHED
	  (setq alias dnstr))
	;; check uniqness then add to list
	(setq sym (intern (downcase alias) dnhash))
	(when (not (boundp sym))
	  (set sym alias)
	  (setq result (cons (cons alias
				   (concat cn " <" (car mails) ">"))
			     result)))
	(setq values (cdr values)))
      ;; make mail addrses list
      (while mails
	(if (null (assoc (car mails) cl)); Not already in cl.
	    ;; (string-match regexp (car mails))
	    ;; add mail address itself to completion list
	    (setq result (cons (cons (car mails)
				     (concat cn " <" (car mails) ">"))
			       result)))
	(setq mails (cdr mails)))
      (setq entries (cdr entries)))
    (append result cl)))

(defun wl-complete-field-to ()
  (interactive)
  (let ((cl wl-address-completion-list))
    (if cl
	(completing-read "To: " cl)
      (read-string "To: "))))

(defun wl-address-quote-specials (word)
  "Make quoted string of WORD if needed."
  (if (assq 'specials (std11-lexical-analyze word))
      (prin1-to-string word)
    word))

(defun wl-address-make-completion-list (address-list)
  (let (addr-tuple cl)
    (while address-list
      (setq addr-tuple (car address-list))
      (setq cl
	     (cons
	      (cons (nth 0 addr-tuple)
		    (concat
		     (wl-address-quote-specials
		      (nth 2 addr-tuple)) " <"(nth 0 addr-tuple)">"))
	      cl))
      ;; nickname completion.
      (unless (or (equal (nth 1 addr-tuple) (nth 0 addr-tuple))
		  ;; already exists
		  (assoc (nth 1 addr-tuple) cl))
	(setq cl
	      (cons
	       (cons (nth 1 addr-tuple)
		     (concat 
		      (wl-address-quote-specials
		       (nth 2 addr-tuple)) " <"(nth 0 addr-tuple)">"))
	       cl)))
      (setq address-list (cdr address-list)))
    cl))

(defun wl-complete-field-body-or-tab ()
  (interactive)
  (let ((case-fold-search t)
	epand-char skip-chars
	(use-ldap nil)
	completion-list)
    (if (wl-draft-on-field-p)
	(wl-complete-field)
      (if (and
	   (< (point)
	      (save-excursion
		(goto-char (point-min))
		(search-forward (concat "\n" mail-header-separator "\n") nil 0)
		(point)))
	   (save-excursion
	     (beginning-of-line)
	     (setq use-ldap nil)
	     (while (and (looking-at "^[ \t]")
			 (not (= (point) (point-min))))
	       (forward-line -1))
	     (cond ((looking-at wl-address-complete-header-regexp)
		    (setq completion-list wl-address-completion-list)
		    (if wl-use-ldap
			(setq use-ldap t))
		    (setq epand-char ?@))
		   ((looking-at wl-folder-complete-header-regexp)
		    (setq completion-list wl-folder-entity-hashtb)
		    (setq skip-chars "^, "))
		   ((looking-at wl-newsgroups-complete-header-regexp)
		    (setq completion-list wl-folder-newsgroups-hashtb)))))
	  (wl-complete-field-body completion-list 
				  epand-char skip-chars use-ldap)
	(indent-for-tab-command)))))

(defvar wl-completion-buf-name "*Completions*")

(defvar wl-complete-candidates nil)

(defun wl-complete-window-show (all)
  (if (and (get-buffer-window wl-completion-buf-name)
	   (equal wl-complete-candidates all))
      (let ((win (get-buffer-window wl-completion-buf-name)))
	(save-excursion
	  (set-buffer wl-completion-buf-name)
	  (if (pos-visible-in-window-p (point-max) win)
	      (set-window-start win 1)
	    (scroll-other-window))))
    (message "Making completion list...")
    (setq wl-complete-candidates all)
    (with-output-to-temp-buffer
	wl-completion-buf-name
      (display-completion-list all))
    (message "Making completion list... done")))

(defun wl-complete-window-delete ()
  (let (comp-buf comp-win)
    (if (setq comp-buf (get-buffer wl-completion-buf-name))
	(if (setq comp-win (get-buffer-window comp-buf))
	    (delete-window comp-win)))))

(defun wl-complete-field ()
  (interactive)
  (let* ((end (point))
	 (start (save-excursion
		  (skip-chars-backward "_a-zA-Z0-9+@%.!\\-")
		  (point)))
	 (completion)
	 (pattern (buffer-substring start end))
	 (cl wl-draft-field-completion-list))
    (if (null cl)
	nil
      (setq completion 
            (let ((completion-ignore-case t))
              (try-completion pattern cl)))
      (cond ((eq completion t)
	     (let ((alias (assoc pattern cl)))
	       (if alias
		   (progn
		     (delete-region start end)
		     (insert (cdr alias))
		;     (wl-highlight-message (point-min)(point-max) t)
		     )))
	     (wl-complete-window-delete))
	    ((null completion)
	     (message "Can't find completion for \"%s\"" pattern)
	     (ding))
	    ((not (string= pattern completion))
	     (delete-region start end)
	     (insert completion)
	     (wl-complete-window-delete)
	     (wl-highlight-message (point-min)(point-max) t))
	    (t
	     (let ((list (all-completions pattern cl)))
	       (wl-complete-window-show list)))))))

(defun wl-complete-insert (start end pattern completion-list)
  (let ((alias (and (consp completion-list)
		    (assoc pattern completion-list)))
	comp-buf comp-win)
    (if alias
	(progn
	  (delete-region start end)
	  (insert (cdr alias))
	  (if (setq comp-buf (get-buffer wl-completion-buf-name))
	      (if (setq comp-win (get-buffer-window comp-buf))
		  (delete-window comp-win)))))))

(defun wl-complete-field-body (completion-list
			       &optional epand-char skip-chars use-ldap)
  (interactive)
  (let* ((end (point))
	 (start (save-excursion
		  (skip-chars-backward (or skip-chars "^:,>\n"))
		  (skip-chars-forward " \t")
		  (point)))
	 (completion)
	 (pattern (buffer-substring start end))
	 (len (length pattern))
	 (cl completion-list))
    (when use-ldap
      (setq cl (wl-address-ldap-search pattern cl)))    
    (if (null cl)
	nil
      (setq completion (try-completion pattern cl))
      (cond ((eq completion t)
	     (if use-ldap (setq wl-address-ldap-search-hash nil))
	     (wl-complete-insert start end pattern cl)
	     (wl-complete-window-delete)
	     (message "Sole completion"))
	    ((and epand-char
		  (> len 0)
		  (char-equal (aref pattern (1- len)) epand-char)
		  (assoc (substring pattern 0 (1- len)) cl))
	     (wl-complete-insert
	      start end
	      (substring pattern 0 (1- len))
	      cl))
	    ((null completion)
	     (message "Can't find completion for \"%s\"" pattern)
	     (ding))
	    ((not (string= pattern completion))
	     (delete-region start end)
	     (insert completion))
	    (t
	     (let ((list (sort (all-completions pattern cl) 'string<)))
	       (wl-complete-window-show list)))))))

(defvar wl-address-init-func 'wl-local-address-init)

(defun wl-address-init ()
  (funcall wl-address-init-func))

(defun wl-local-address-init ()
  (message "Updating addresses...")
  (setq wl-address-list 
	(wl-address-make-address-list wl-address-file))
  (setq wl-address-completion-list 
	(wl-address-make-completion-list wl-address-list))
  (if (file-readable-p wl-alias-file)
      (setq wl-address-completion-list 
	    (append wl-address-completion-list 
		    (wl-address-make-alist-from-alias-file wl-alias-file))))
  (setq wl-address-petname-hash (elmo-make-hash))
  (mapcar 
   (function
	(lambda (x)
	  (elmo-set-hash-val (downcase (car x))
						 (cadr x)
						 wl-address-petname-hash)))
   wl-address-list)
  (message "Updating addresses...done."))


(defun wl-address-expand-aliases (alist nest-count)
  (when (< nest-count 5)
    (let (expn-str new-expn-str expn new-expn(n 0) (expanded nil))
      (while (setq expn-str (cdr (nth n alist)))
        (setq new-expn-str nil)
        (while (string-match "^[ \t]*\\([^,]+\\)" expn-str)
          (setq expn (elmo-match-string 1 expn-str))
	  (setq expn-str (wl-string-delete-match expn-str 0))
          (if (string-match "^[ \t,]+" expn-str)
	      (setq expn-str (wl-string-delete-match expn-str 0)))
          (if (string-match "[ \t,]+$" expn)
	      (setq expn (wl-string-delete-match expn 0)))
          (setq new-expn (cdr (assoc expn alist)))
          (if new-expn
              (setq expanded t))
          (setq new-expn-str (concat new-expn-str (and new-expn-str ", ")
                                     (or new-expn expn))))
        (when new-expn-str
          (setcdr (nth n alist) new-expn-str))
        (setq n (1+ n)))
      (and expanded
           (wl-address-expand-aliases alist (1+ nest-count))))))

(defun wl-address-make-alist-from-alias-file (file)
  (elmo-set-work-buf
    (let ((case-fold-search t)
	  alias expn alist)
      (insert-file-contents file)
      (while (re-search-forward ",$" nil t)
	(end-of-line)
	(forward-char 1)
 	(delete-backward-char 1))
      (goto-char (point-min))
      (while (re-search-forward "^\\([^#;\n][^:]+\\):[ \t]*\\(.*\\)$" nil t)
	(setq alias (wl-match-buffer 1)
	      expn (wl-match-buffer 2))
	(setq alist (cons (cons alias expn) alist)))
      (wl-address-expand-aliases alist 0)
      (nreverse alist) ; return value
      )))
	
(defun wl-address-make-address-list (path)
  (if (and path (file-readable-p path))
      (elmo-set-work-buf
	(let (ret
	      (coding-system-for-read wl-cs-autoconv))
	  (insert-file-contents path)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (looking-at 
 "^\\([^#\n][^ \t\n]+\\)[ \t]+\"\\(.*\\)\"[ \t]+\"\\(.*\\)\"[ \t]*.*$")
		(setq ret 
		      (wl-append-element 
		       ret
		       (list (wl-match-buffer 1)
			     (wl-match-buffer 2)
			     (wl-match-buffer 3)))))
	    (forward-line))
	  ret))))

(defsubst wl-address-get-petname (string)
  (let ((address (downcase (wl-address-header-extract-address string))))
    (elmo-get-hash-val address wl-address-petname-hash)))

(defsubst wl-address-user-mail-address-p (address)
  "Judge whether ADDRESS is user's or not."
  (member (downcase (wl-address-header-extract-address address))
	  (or (mapcar 'downcase wl-user-mail-address-list)
	      (list (downcase 
		     (wl-address-header-extract-address
		      wl-from))))))

(defsubst wl-address-header-extract-address (str)
  "Extracts a real e-mail address from STR and returns it.
e.g. \"Mine Sakurai <m-sakura@ccs.mt.nec.co.jp>\"
  ->  \"m-sakura@ccs.mt.nec.co.jp\".
e.g. \"m-sakura@ccs.mt.nec.co.jp (Mine Sakurai)\"
  ->  \"m-sakura@ccs.mt.nec.co.jp\"."
  (cond ((string-match ".*<\\([^>]*\\)>" str) ; .* to extract last <>
         (wl-match-string 1 str))
        ((string-match "\\([^ \t\n]*@[^ \t\n]*\\)" str)
	 (wl-match-string 1 str))
        (t str)))

(defsubst wl-address-header-extract-realname (str)
  "Extracts a real name from STR and returns it.
e.g. \"Mr. bar <hoge@foo.com>\"
  ->  \"Mr. bar\"."
  (cond ((string-match "\\(.*[^ \t]\\)[ \t]*<[^>]*>" str)
         (wl-match-string 1 str))
        (t "")))

(defmacro wl-address-concat-token (string token)
  (` (cond
      ((eq 'quoted-string (car (, token)))
       (concat (, string) "\"" (cdr (, token)) "\""))
      ((eq 'comment (car (, token)))
       (concat (, string) "(" (cdr (, token)) ")"))
      (t 
       (concat (, string) (cdr (, token)))))))

(defun wl-address-string-without-group-list-contents (sequence)
  "Return address string from lexical analyzed list SEQUENCE.
Group list contents is not included."
  (let (address-string route-addr-end token seq)
  (while sequence
    (setq token (car sequence))
    (cond 
     ;;   group       =  phrase ":" [#mailbox] ";"
     ((and (eq 'specials (car token))
	   (string= (cdr token) ":"))
      (setq address-string (concat address-string (cdr token))) ; ':'
      (setq seq (cdr sequence))
      (setq token (car seq))
      (while (not (and (eq 'specials (car token))
		       (string= (cdr token) ";")))
	(setq token (car seq))
	(setq seq (cdr seq)))
      (setq address-string (concat address-string (cdr token))) ; ';'
      (setq sequence seq))
     ;;   route-addr  =  "<" [route] addr-spec ">"
     ;;   route       =  1#("@" domain) ":"           ; path-relative
     ((and (eq 'specials (car token))
	   (string= (cdr token) "<"))
      (setq seq (std11-parse-route-addr sequence))
      (setq route-addr-end (car (cdr seq)))
      (while (not (eq (car sequence) route-addr-end))
	(setq address-string (wl-address-concat-token address-string
						      (car sequence)))
	(setq sequence (cdr sequence))))
     (t 
      (setq address-string (wl-address-concat-token address-string token))
      (setq sequence (cdr sequence)))))
  address-string))

(defun wl-address-petname-delete (the-email)
  "Delete petname in wl-address-file."
  (let* ( (tmp-buf (get-buffer-create " *wl-petname-tmp*"))
	  (output-coding-system 
	   (mime-charset-to-coding-system wl-mime-charset)))
    (set-buffer tmp-buf)
    (message "Deleting Petname...")
    (erase-buffer)
    (insert-file-contents wl-address-file)
    (delete-matching-lines (concat "^[ \t]*" the-email))
    (write-region (point-min) (point-max)
		  wl-address-file nil 'no-msg)
    (message "Deleting Petname...done")
    (kill-buffer tmp-buf)))


(defun wl-address-petname-add-or-change (the-email 
					 default-petname 
					 default-realname 
					 &optional change-petname)
  "Add petname to wl-address-file, if not registerd.
If already registerd, change it."
  (let (the-realname the-petname)

    ;; setup output "petname"
    ;; if null petname'd, let default-petname be the petname.
    (setq the-petname
	  (read-from-minibuffer (format "Petname: ") default-petname))
    (if (string= the-petname "")
	(setq the-petname (or default-petname the-email)))

    ;; setup output "realname"
    (setq the-realname
	(read-from-minibuffer (format "Real Name: ") default-realname))
;;	(if (string= the-realname "")
;;	    (setq the-realname default-petname))

    ;; writing to ~/.address
    (let ( (tmp-buf (get-buffer-create " *wl-petname-tmp*"))
	   (output-coding-system (mime-charset-to-coding-system wl-mime-charset)))
      (set-buffer tmp-buf)
      (message "Adding Petname...")
      (erase-buffer)
      (if (file-exists-p wl-address-file)
	  (insert-file-contents wl-address-file))
      (if (not change-petname)
	  ;; if only add
	  (progn
	    (goto-char (point-max))
	    (if (and (> (buffer-size) 0)
		     (not (eq (char-after (1- (point-max))) ?\n)))
		(insert "\n")))
	;; if change
	(if (re-search-forward (concat "^[ \t]*" the-email) nil t)
	    (delete-region (save-excursion (beginning-of-line)
					   (point))
			   (save-excursion (end-of-line)
					   (+ 1 (point))))))
      (insert (format "%s\t\"%s\"\t\"%s\"\n"
		      the-email the-petname the-realname))
      (write-region (point-min) (point-max)
		    wl-address-file nil 'no-msg)
      (message "Adding Petname...done")
      (kill-buffer tmp-buf))))

(provide 'wl-address)

;;; wl-address.el ends here

