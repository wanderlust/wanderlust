;; wl-addrbook.el --- Aliases and personal information

;; Author:  Masahiro MURATA <muse@ba2.so-net.ne.jp>
;;	Kazu Yamamoto <Kazu@Mew.org>
;; Keywords: mail, net news

;;; Commentary:

;;  Insert the following lines in your ~/.wl
;;
;; (require 'wl-addrbook)
;; (wl-addrbook-setup)

;; Original code: Kazu Yamamoto <Kazu@Mew.org>
;;	mew-addrbook.el (Mew developing team)

;;; Code:

(require 'wl-util)

(defvar wl-addrbook-file "~/.im/Addrbook"
  "*Addrbook file for completion")
(defvar wl-addrbook-expand-max-depth 5
  "*A value to limit alias(addrbook) expansion loop.")
(defvar wl-addrbook-comment-regexp "^;.*$\\|#.*$"
  "*Regular expression for \".im/Addrbook\".")
(defvar wl-addrbook-override-by-newone t
  "If non-nil, the 'user' entry in 'wl-alias-auto-alist'
is override by a new entry of (user different-address). 
This means that addresses in To: and Cc: in Draft mode are
always learned with an exception 'user' is defined in Addrbook.
If nil,  the old 'user' entry remains.")

;;(defvar wl-anonymous-recipients ":;")

(defvar wl-addrbook-hashtb nil)

(defvar wl-addrbook-strip-domainpart t
  "*If *non-nil*, a shortname is created by stripping its domain part.")

(defvar wl-addrbook-alist nil
  "(key addr) or (key (addr1, addr2) nickname name)")
(defvar wl-alias-auto-alist nil
  "(key addr)")
(defvar wl-alias-auto-file-name "auto-alias")

(defvar wl-summary-use-addrbook-from-func t)

;;; utils

(defun wl-uniq-alist (alst)
  "Distractively uniqfy elements of ALST."
  (let ((tmp alst))
    (while tmp (setq tmp (setcdr tmp (wl-delete-alist2 (car (car tmp)) (cdr tmp))))))
  alst)

(defun wl-delete-alist2 (key alist)
  "Destructively delete elements whose first member is equal to key"
  (if (null key)
      alist
    (let (ret)
      (while (equal (car (nth 0 alist)) key)
	(setq alist (cdr alist)))
      (setq ret alist)
      (while alist
	(if (equal (car (nth 1 alist)) key)
	    (setcdr alist (cdr (cdr alist)))
	  (setq alist (cdr alist))))
      ret)))

(defun wl-get-next (LIST MEM)
  (let (frst next crnt)
    (setq frst (car LIST))
    (setq LIST (cdr LIST))
    (setq next (car LIST))
    (if (equal frst MEM)
	(if next next frst)
    (catch 'loop
      (while LIST
	(setq crnt next)
	(setq LIST (cdr LIST))
	(setq next (car LIST))
	(if (equal crnt MEM)
	    (throw 'loop (if next next frst))))))))

(defun wl-address-extract-user (addr)
  "Extracts username from ADDR"
  (if (string-match "@.*:" addr) ;; xxx what's this?
      (setq addr (substring addr (match-end 0) nil))
    (setq addr (elmo-replace-in-string addr " " "_"))
    (setq addr (substring addr 0 (string-match "%" addr)))
    (setq addr (substring addr 0 (string-match "@" addr)))
    ;; just for refile:  "To: recipients:;" -> recipients
    ;;(setq addr (substring addr 0 (string-match wl-anonymous-recipients addr)))
    ;; removing Notes domain
    (setq addr (substring addr 0 (string-match "/" addr)))))

(defun wl-address-parse-address-list (addrs)
  (mapcar 'wl-address-header-extract-address (wl-parse-addresses addrs)))

;; hash table for wl-addrbook-alist
(defmacro wl-addrbook-hashtb ()
  (` (or wl-addrbook-hashtb
	 (setq wl-addrbook-hashtb (elmo-make-hash 1021)))))

(defsubst wl-addrbook-get-record-by-addr (addr &optional alist)
  (elmo-get-hash-val (downcase addr) (wl-addrbook-hashtb)))

(defsubst wl-addrbook-get-record-by-alias (alias &optional alist)
  (elmo-get-hash-val (format "#%s" (downcase alias)) (wl-addrbook-hashtb)))

(defun wl-addrbook-make-hashtb ()
  (let ((ht (wl-addrbook-hashtb))
	(alist wl-addrbook-alist)
	list addrs addr)
    (while alist
      (setq list (car alist)
	    alist (cdr alist))
      ;; key is alias
      (if (car list)
	  (elmo-set-hash-val (format "#%s" (downcase (car list))) list ht))
      (when (listp (setq addrs (nth 1 list)))
	(while addrs
	  (setq addr (car addrs)
		addrs (cdr addrs))
	  ;; key is address
	  (elmo-set-hash-val (downcase addr) list ht))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Address book
;;;

(defun wl-addrbook-setup ()
  (require 'wl-complete)
  ;; replace wl-address-init function.
  (setq wl-address-init-function 'wl-addrbook-init)
  ;;
  (when wl-summary-use-addrbook-from-func
    (setq wl-summary-from-function 'wl-summary-addrbook-from))
  (define-key wl-summary-mode-map "\C-c\C-a" 'wl-summary-addrbook-add)
  (define-key wl-draft-mode-map "\C-i"     'wl-draft-addrbook-header-comp-or-tab)
  (define-key wl-draft-mode-map "\e\t"     'wl-draft-addrbook-expand)
  (define-key wl-draft-mode-map "\C-c\t"   'wl-draft-circular-comp)
  (add-hook 'mail-send-hook 'wl-draft-learn-alias))

(defun wl-addrbook-init ()
  (message "Updating addrbook...")
  (or wl-alias-auto-alist
      (if wl-alias-auto-file-name
	  (setq wl-alias-auto-alist
		(elmo-object-load (expand-file-name
				   wl-alias-auto-file-name
				   elmo-msgdb-directory)))))
  (setq wl-addrbook-alist (wl-addrbook-make-alist))
  ;; wl-alias-auto-alist is used independently so must use copy-alist
  (if wl-addrbook-alist
      (nconc wl-addrbook-alist (copy-alist wl-alias-auto-alist))
    (setq wl-addrbook-alist (copy-alist wl-alias-auto-alist)))
;;   (if wl-addrbook-alist
;;       (nconc wl-addrbook-alist (wl-petname-make-alist))
;;     (setq wl-addrbook-version (wl-petname-make-alist)))
  (setq wl-addrbook-alist (wl-uniq-alist wl-addrbook-alist))
  (wl-addrbook-make-hashtb)
  (add-hook 'kill-emacs-hook (function wl-addrbook-clean-up))
  (add-hook 'wl-exit-hook (function wl-addrbook-clean-up))
  (message "Updating addrbook...done"))

(defun wl-addrbook-clean-up ()
  (remove-hook 'kill-emacs-hook (function wl-addrbook-clean-up))
  (remove-hook 'wl-exit-hook (function wl-addrbook-clean-up))
  (when wl-alias-auto-file-name
    (elmo-object-save (expand-file-name
		       wl-alias-auto-file-name
		       elmo-msgdb-directory)
		      wl-alias-auto-alist)
    (setq wl-alias-auto-alist nil)
    (setq wl-addrbook-hashtb nil)))

;;

(defmacro wl-alias-get (key)
  (` (wl-addrbook-alias-get (, key) wl-addrbook-alist)))

(defmacro wl-alias-next (key)
  (` (wl-addrbook-alias-next (, key) wl-addrbook-alist)))

(defalias 'wl-addrbook-alias-hit 'wl-addrbook-get-record-by-alias)

(defun wl-addrbook-alias-get (key alist)
  (let ((addrs (wl-addrbook-alias-get1 key alist 0)))
    (cond
     ((stringp addrs) addrs)
     ((listp addrs)
      (mapconcat (lambda (x) x) (nreverse addrs) ", "))
     (t key))))

(defun wl-addrbook-alias-get1 (key alist n)
  "Expand KEY to addresses according ALIST.
If addresses is a list, that follows one-of convention and
return the first member of the list.
If addresses is a string, expands it recursively."
  (let* ((crnt (nth 1 (wl-addrbook-alias-hit key alist)))
	 (keys (and (stringp crnt)
		    (elmo-parse crnt "\\([^, \t]+\\)")))
	 ret tmp)
    (cond
     ((> n wl-addrbook-expand-max-depth) key)
     ((null crnt) key)
     ((listp crnt) (car crnt))
     (t
      (while keys
	(setq tmp (wl-addrbook-alias-get1 (car keys) alist (1+ n)))
	(if (listp tmp)
	    (setq ret (nconc tmp ret))
	  (setq ret (cons tmp ret)))
	(setq keys (cdr keys)))
      ret))))

(defun wl-addrbook-alias-next (key alist)
  (let* ((addrs (nth 1 (wl-addrbook-get-record-by-addr key alist))))
    (if (and addrs (listp addrs))
	(wl-get-next addrs key))))

(defun wl-addrbook-alias-add (addr)
  (if (and (stringp addr) (string-match "@" addr))
      (let* ((user (wl-address-extract-user addr))
	     (match-auto (assoc user wl-alias-auto-alist))
	     (match-adbk (assoc user wl-addrbook-alist)))
	(cond
	 (match-auto
	  (cond
	   ((equal addr (nth 1 match-auto))
	    ;; move the entry to the top for the recent-used-first.
	    (setq wl-alias-auto-alist
		  (cons match-auto (delete match-auto wl-alias-auto-alist))))
	   (wl-addrbook-override-by-newone
	    ;; override match-auto by (user addr)
	    (setq wl-addrbook-alist
		  (cons (list user addr)
			(delete match-auto wl-addrbook-alist)))
	    (setq wl-alias-auto-alist
		  (cons (list user addr)
			(delete match-auto wl-alias-auto-alist))))
	   (t 
	    ;; the old entry remains
	    )))
	 (match-adbk
	  ;; do nothing
	  )
	 (t
	  (setq wl-addrbook-alist (cons (list user addr) wl-addrbook-alist))
	  (setq wl-alias-auto-alist
		(cons (list user addr) wl-alias-auto-alist)))))))

(defun wl-addrbook-alias-delete (addr)
  (if (and (stringp addr) (string-match "@" addr))
      (let* ((user (wl-address-extract-user addr))
	     (ent (assoc user wl-addrbook-alist)))
	(if (and ent (equal (cdr ent) addr))
	    (progn
	      (setq wl-addrbook-alist (delete ent wl-addrbook-alist))
	      (setq wl-alias-auto-alist (delete ent wl-alias-auto-alist)))))))

;;

(defun wl-addrbook-shortname-get (addr)
  (nth 0 (wl-addrbook-get-record-by-addr addr)))

(defun wl-addrbook-nickname-get (addr)
  (nth 2 (wl-addrbook-get-record-by-addr addr)))

(defun wl-addrbook-name-get (addr)
  (nth 3 (wl-addrbook-get-record-by-addr addr)))
;;

(defun wl-addrbook-insert-file (file cregexp &optional unquote)
  (let* ((case-fold-search t)
	 (coding-system-for-read wl-cs-autoconv)
	 (pars (elmo-parse file "\\([^, ]+\\)")) ;; parents
	 (files pars) ;; included
	 par chr path beg qchar)
    ;; include parents files
    (while pars
      (setq par (car pars))
      (setq pars (cdr pars))
      (if (not (file-readable-p par))
	  ()
	(insert-file-contents par)
	(setq path (file-name-directory par))
	;; include children files
	(while (re-search-forward "^\<[ \t]*\\([^ \t\n]+\\).*$" nil t)
	  (setq chr (expand-file-name (wl-match-buffer 1) path))
	  (delete-region (match-beginning 0) (match-end 0))
	  (if (and (file-readable-p chr) (not (member chr files)))
	      (progn
		(insert-file-contents chr)
		(setq files (cons chr files)))))
	(goto-char (point-max))))
    ;; remove commets
    (goto-char (point-min))
    (while (re-search-forward cregexp nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; concat continuation lines
    (goto-char (point-min))
    (while (re-search-forward "\\\\\n" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; concat separated lines by comma
    (goto-char (point-min))
    (while (re-search-forward ",[ \t]*$" nil t)
      (end-of-line)
      (forward-char 1)
      (delete-backward-char 1)
      (delete-horizontal-space))
    ;; unquote, replace white spaces to "\0".
    (if unquote
	(catch 'quote
	  (goto-char (point-min))
	  (while (re-search-forward "[\"']" nil t)
	    (setq qchar (char-before (point)))
	    ;; (point) is for backward compatibility
	    (backward-delete-char 1) ;; delete quote
	    (setq beg (point))
	    (if (not (re-search-forward (char-to-string qchar) nil t))
		(throw 'quote nil) ;; error
	      (backward-delete-char 1) ;; delete quote
	      (save-restriction
		(narrow-to-region beg (point))
		(goto-char (point-min))
		(while (re-search-forward "[ \t]+" nil t)
		  (replace-match "\0"))
		(goto-char (point-max))))))) ;; just in case
    ;; remove optional white spaces
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+" nil t)
      (replace-match " "))))

(defun wl-addrbook-strsafe (var)
  (if (or (string-equal "" var) (string-equal "*" var))
      nil
    (save-match-data
      (elmo-replace-in-string var (char-to-string 0) " "))))

(defun wl-addrbook-make-alist ()
  (let (alias colon addrs nick name alist)
    (wl-set-work-buf
     (wl-addrbook-insert-file
      wl-addrbook-file wl-addrbook-comment-regexp 'unquote)
     (goto-char (point-min))
     (while (re-search-forward "^ ?\\([^ \n:]+\\) ?\\(:?\\) ?\\([^ \n]+\\)" nil t)
       (setq alias (wl-addrbook-strsafe (wl-match-buffer 1)))
       (setq colon (wl-match-buffer 2))
       (setq addrs (wl-addrbook-strsafe (wl-match-buffer 3)))
       (if (equal colon ":")
	   (setq alist (cons (list alias addrs) alist))
	 (and addrs (setq addrs (elmo-parse addrs "\\([^, \t\r\n]+\\)")))
	 (if (looking-at " ?\\([^ \n]*\\) ?\\([^ \n]*\\)")
	     (progn
	       (setq nick (wl-addrbook-strsafe (wl-match-buffer 1)))
	       (setq name (wl-addrbook-strsafe (wl-match-buffer 2))))
	   (setq nick nil)
	   (setq name nil))
	 (setq alist (cons (list alias addrs nick name) alist))))
     (nreverse alist))))

(defun wl-draft-learn-alias ()
  (interactive)
  (let ((recipients (mapconcat 'identity 
			       (delq nil (std11-field-bodies '("To" "Cc")))
			       ",")))
    (mapcar '(lambda (addr)
	       (wl-addrbook-alias-add
		(wl-address-header-extract-address addr)))
	    (wl-parse-addresses recipients))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addrbook mode
;;;

(defvar wl-addrbook-mode-map nil)

(if wl-addrbook-mode-map
    ()
  ;;(setq wl-addrbook-mode-map (make-sparse-keymap))
  ;;(set-keymap-parent wl-addrbook-mode-map text-mode-map)
  (setq wl-addrbook-mode-map (copy-keymap text-mode-map))
  (define-key wl-addrbook-mode-map "\C-c\C-c" 'wl-addrbook-register)
  (define-key wl-addrbook-mode-map "\C-c\C-q" 'wl-addrbook-kill))

(defvar wl-addrbook-mode-alias "Alias")
(defvar wl-addrbook-mode-personalinfo "Personal Info")
(defconst wl-addrbook-buffer-name "*WL Addrbook*")

(defun wl-summary-addrbook-add (&optional personalinfo)
  "Adding the value of From: or To: in Message mode to Addrbook. When
executed with '\\[universal-argument]', it will add personal information.  Otherwise,
it will add an alias."
  (interactive "P")
  (wl-summary-redisplay)
  (let ((buf wl-message-buffer)
	from shortname address addrs name)
    (save-excursion
      (set-buffer buf)
      (setq address (std11-field-body "From"))
      (if (wl-address-user-mail-address-p address)
	  (setq address (std11-field-body "To")))
      (if (null address)
	  (message "No address to be registered")
	(setq addrs (wl-address-header-extract-address address))
	(if wl-addrbook-strip-domainpart
	    (setq shortname (wl-address-extract-user addrs))
	  (setq shortname addrs))
	(if (string-match "\\(.*\\)<.*>" address)
	    (progn
	      (setq name (wl-match-string 1 address))
	      (setq name (elmo-replace-in-string name "[ \t]$" ""))))
	(wl-addrbook-prepare-template personalinfo shortname addrs nil name)))))

(defun wl-addrbook-prepare-template (personalinfop shortname addrs &optional nickname name)
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create wl-addrbook-buffer-name))
  (erase-buffer)
  (insert "#If you want to register this entry, type "
	  (substitute-command-keys
	   "'\\<wl-addrbook-mode-map>\\[wl-addrbook-register]'.\n")
	  "#If you want to NOT register this entry, type "
	  (substitute-command-keys
	   "'\\<wl-addrbook-mode-map>\\[wl-addrbook-kill]'.\n"))
  (wl-addrbook-insert-template "Shortname" shortname)
  (wl-addrbook-insert-template "Addresses" addrs)
  (cond
   (personalinfop
    (wl-addrbook-insert-template "Nickname" nickname)
    (wl-addrbook-insert-template "Name" name)
    (wl-addrbook-mode wl-addrbook-mode-personalinfo))
   (t
    (wl-addrbook-mode wl-addrbook-mode-alias)))
  (wl-addrbook-insert-template "Comments" nil)
  (goto-char (point-min))
  (search-forward ": " nil t))

(defun wl-addrbook-insert-template (key val)
  (let ((buffer-read-only nil)
	(inhibit-read-only t)
	(beg (point)))
    (insert key ": ")
    (put-text-property beg (point) 'read-only t)
    (put-text-property (1- (point)) (point)
		       (if wl-on-xemacs 'end-open 'rear-nonsticky)
		       t)
    (and val (insert val))
    (insert "\n")))

(defun wl-addrbook-mode (mname)
  "\\<wl-addrbook-mode-map>
Mew Addrbook mode:: major mode to resistor Addrbook.
The keys that are defined for this mode are:

\\[wl-addrbook-register]	Register information in Addrbook mode to Addrbook.
\\[wl-addrbook-kill]	Kill Addrbook mode.
"
  (interactive)
  (setq major-mode 'wl-addrbook-mode)
  (setq mode-name mname)
  (setq mode-line-buffer-identification
	(format "Wanderlust: %s" mname))
  (use-local-map wl-addrbook-mode-map)
  (run-hooks 'wl-addrbook-mode-hook)
  (setq buffer-undo-list nil))

(defun wl-addrbook-register ()
  "Register information in Addrbook mode to Addrbook."
  (interactive)
  (let ((shortname (std11-field-body "Shortname"))
	(addrs     (std11-field-body "Addresses"))
	(nickname  (std11-field-body "Nickname"))
	(name      (std11-field-body "Name"))
	(comments  (std11-field-body "Comments"))
	(mode mode-name)
	buf addrsl errmsg not-uniq)
     (cond
      ((equal mode wl-addrbook-mode-alias)
       (cond
	((and (null shortname) (null addrs))
	 (setq errmsg "Must fill both Shortname and Addresses."))
	((null shortname)
	 (setq errmsg "Must fill Shortname."))
	((null addrs)
	 (setq errmsg "Must fill Addresses."))))
      (t
       (cond
	((null addrs)
	 (setq errmsg "Must fill Addresses."))
	((and (null shortname) (null nickname) (null name))
	 (setq errmsg "Must fill Shortname or Nickname or Name."))
	((and name (string-match "^\"[^\"]*[^\000-\177]" name))
	 (setq errmsg "Remove quote around non-ASCII Name.")))))
     (if errmsg
	 (message errmsg)
       (save-excursion
	 (setq buf (find-file-noselect wl-addrbook-file))
	 (set-buffer buf)
	 (goto-char (point-min))
	 (if (and shortname
		  (re-search-forward 
		   (concat "^" (regexp-quote shortname) "[ \t]*:?[ \t]+") nil t))
	     (setq not-uniq t))
	 (if not-uniq
	     () ;; see later
	   ;; All errors are checked.
	   (goto-char (point-max))
	   (if (not (bolp)) (insert "\n"))
	   (cond
	    ((equal mode wl-addrbook-mode-alias)
	     (setq wl-addrbook-alist
		   (cons (list shortname addrs) wl-addrbook-alist))
	     (insert shortname ":\t" addrs))
	    (t
	     (setq addrsl (wl-address-parse-address-list addrs))
	     (setq wl-addrbook-alist
		   (cons (list shortname addrsl nickname name) wl-addrbook-alist))
	     (if (null shortname) (setq shortname "*"))
	     (if (and nickname (string-match "^[^\" \t]+[ \t]+.*$" nickname))
		 (setq nickname (concat "\"" nickname "\"")))
	     (if (and name (string-match "^[^\" \t]+[ \t]+.*$" name))
		 (setq name (concat "\"" name "\"")))
	     (if name
		 (insert shortname "\t" addrs "\t" (or nickname "*") "\t" name)
	       (if nickname
		   (insert shortname "\t" addrs "\t" nickname)
		 (insert shortname "\t" addrs)))))
	   (if comments
	       (insert "\t#" comments "\n")
	     (insert "\n"))
	   (save-buffer)))
       (wl-addrbook-make-hashtb)
       ;; Addrbook buffer
       (kill-buffer buf)
       (if not-uniq
	   (message "Shortname is already used. Change Shortname.")
	 (wl-addrbook-kill 'no-msg)
	 (message "Registered to Addrbook.")))))

(defun wl-addrbook-kill (&optional no-msg)
  "Kill Addrbook mode."
  (interactive "P")
  (kill-buffer (current-buffer))
  (or no-msg (message "Not registered.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Show nick name of Addrbook in summary.
;;;

(defsubst wl-addrbook-get-names (names)
  (let (addrs)
    (mapconcat
     (function
      (lambda (name)
	(or (wl-addrbook-nickname-get
	     (wl-address-header-extract-address name))
	    (and (setq addrs (std11-extract-address-components name))
		 (or (car addrs) (cadr addrs))))))
     (wl-parse-addresses names)
     ",")))

(eval-when-compile (defvar-maybe entity nil)) ; silence byte compiler.
(defun wl-summary-addrbook-from (from)
  "A candidate for wl-summary-from-function.
Show destination in summary matched by `wl-summary-show-dest-folder-regexp'.
And use Addrbook for get user name."
  (let ((fromaddr (wl-address-header-extract-address from))
	dest)
    (or
     (and (eq major-mode 'wl-summary-mode)
	  (string-match wl-summary-showto-folder-regexp
			wl-summary-buffer-folder-name)
	  (wl-address-user-mail-address-p fromaddr)
	  (cond ((setq dest (elmo-message-entity-field entity 'to))
		 (concat "To:" (eword-decode-string (wl-addrbook-get-names dest))))
		((setq dest (elmo-message-entity-field entity 'newsgroups))
		 (concat "Ng:" dest))))
     (wl-addrbook-nickname-get fromaddr)
     from)))

(provide 'wl-addrbook)

;;; Copyright Notice:

;; Copyright (C) 1999-2001 Mew developing team.
;; Copyright (C) 2001 Masahiro Murata <muse@ba2.so-net.ne.jp>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; wl-addrbook.el ends here
