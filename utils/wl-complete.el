;;; wl-complete.el --- Completion magic for Wanderlust

;; Author:  Masahiro MURATA <muse@ba2.so-net.ne.jp>
;;	Kazu Yamamoto <Kazu@Mew.org>
;; Keywords: mail, net news

;;; Commentary:

;;  Insert the following lines in your ~/.wl
;;
;; (require 'wl-addrbook)
;; (wl-addrbook-setup)

;; Original code: Kazu Yamamoto <Kazu@Mew.org>
;;	mew-complete.el (Mew developing team)

;;; Code:

(require 'wl-util)
(require 'wl-addrbook)

(defvar wl-mail-domain-list nil)
(defvar wl-from-list nil)

(defvar wl-complete-lwsp "^[ \t]")
(defvar wl-complete-address-separator ":, \t\n")

(defvar wl-field-completion-switch
  '(("To:"       . wl-addrbook-complete-address)
    ("Cc:"       . wl-addrbook-complete-address)
    ("Dcc:"      . wl-addrbook-complete-address)
    ("Bcc:"      . wl-addrbook-complete-address)
    ("Reply-To:" . wl-addrbook-complete-address)
    ("Mail-Reply-To:" . wl-addrbook-complete-address)
    ("Return-Receipt-To:" . wl-addrbook-complete-address)
    ("Newsgroups:" . wl-complete-newsgroups)
    ("Followup-To:" . wl-complete-newsgroups)
    ("Fcc:"      . wl-complete-folder)
    )
  "*Completion function alist concerned with the key.")

(defvar wl-field-circular-completion-switch
  '(("To:"       . wl-circular-complete-domain)
    ("Cc:"       . wl-circular-complete-domain)
    ("Dcc:"      . wl-circular-complete-domain)
    ("Bcc:"      . wl-circular-complete-domain)
    ("Reply-To:" . wl-circular-complete-domain)
    ("From:"     . wl-circular-complete-from))
  "*Circular completion function alist concerned with the key.")

(defvar wl-field-expansion-switch
  '(("To:"       . wl-addrbook-expand-address)
    ("Cc:"       . wl-addrbook-expand-address)
    ("Dcc:"      . wl-addrbook-expand-address)
    ("Bcc:"      . wl-addrbook-expand-address)
    ("Reply-To:" . wl-addrbook-expand-address))
  "*expansion function alist concerned with the key.")

;;; Code:

(defun wl-string-match-assoc (key alist &optional case-ignore)
  (let (a
	(case-fold-search case-ignore))
    (catch 'loop
      (while alist
	(setq a (car alist))
	(if (and (consp a)
		 (stringp (car a))
		 (string-match key (car a)))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low level functions
;;;

(defsubst wl-draft-on-header-p ()
  (< (point)
     (save-excursion
       (goto-char (point-min))
       (search-forward (concat "\n" mail-header-separator "\n") nil 0)
       (point))))

(defun wl-draft-on-value-p (switch)
  (if (wl-draft-on-header-p)
      (save-excursion
	(beginning-of-line)
	(while (and (< (point-min) (point)) (looking-at wl-complete-lwsp))
	  (forward-line -1))
	(if (looking-at "\\([^:]*:\\)")
	    (wl-string-match-assoc (wl-match-buffer 1) switch t)
	  nil)))) ;; what a case reachs here?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion function: C-i
;;;

(defun wl-draft-addrbook-header-comp-or-tab (force)
  (interactive "P")
  (let ((case-fold-search t)
	func)
    (if (wl-draft-on-field-p)
	(wl-complete-field)
      (if (and
	   (wl-draft-on-header-p)
	   (setq func (wl-draft-on-value-p wl-field-completion-switch)))
	  (funcall (cdr func))
	(indent-for-tab-command)))))

(defun wl-complete-newsgroups ()
  (interactive)
  (wl-complete-field-body wl-folder-newsgroups-hashtb))
  ;;(wl-address-complete-address wl-folder-newsgroups-hashtb))

(defun wl-complete-folder ()
  "Folder complete function for Fcc:."
  (interactive)
  (let ((word (wl-delete-backward-char)))
    (if (null word)
	(wl-complete-window-show (list "+" "%"))
      (wl-complete word wl-folder-entity-hashtb "folder" nil))))

(defun wl-addrbook-complete-address ()
  "Complete and expand address aliases. 
First alias key is completed. When completed solely or the @ character
is inserted before the cursor, the alias key is expanded to its value."
  (interactive)
  (let ((word (wl-delete-backward-char)))
    (if (null word)
	(tab-to-tab-stop)
      (if (string-match "@." word)
	  (insert (or (wl-alias-next word) word))
	(wl-complete
	 word wl-addrbook-alist "alias" ?@ nil nil
	 (function wl-addrbook-alias-get) 
	 (function wl-addrbook-alias-hit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Circular completion: C-cC-i
;;;

(defun wl-draft-circular-comp ()
  "Switch function for circular complete functions."
  (interactive)
  (let ((func (wl-draft-on-value-p wl-field-circular-completion-switch)))
    (if func
	(funcall (cdr func))
      (message "No circular completion here"))))

(defun wl-circular-complete-domain ()
  "Circular completion of domains for To:, Cc:, etc.
If the @ character does not exist, the first value of
wl-mail-domain-list is inserted. If exists, the next value of 
wl-mail-domain-list concerned with the string between @ and 
the cursor is inserted."
  (interactive)
  (let ((word (wl-delete-backward-char "@")))
    (cond
     ((equal word nil) ;; @ doesn't exist.
      (if (null wl-mail-domain-list)
	  (message "For domain circular completion, set wl-mail-domain-list")
	(insert "@")
	(insert (car wl-mail-domain-list))
	(wl-complete-window-delete)))
     ((equal word t) ;; just after @
      (if (null wl-mail-domain-list)
	  (message "For domain circular completion, set wl-mail-domain-list")
	(insert (car wl-mail-domain-list))
	(wl-complete-window-delete)))
     (t
      ;; can't use wl-get-next since completion is necessary sometime.
      (wl-complete
       word
       (wl-slide-pair wl-mail-domain-list)
       "domain"
       t)) ;; use cdr
     )))

(defun wl-circular-complete (msg clist cname &optional here)
  "General circular complete function to call wl-complete."
  (interactive)
  (let ((str (wl-delete-value here)))
    (if (null str)
	(if (car clist)
	    (insert (car clist))
	  (message "For circular completion, set %s" cname))
      (wl-complete
       str
       (wl-slide-pair clist)
       msg
       t)))) ;; use cdr

(defun wl-circular-complete-from ()
  "Circular complete function for From:."
  (interactive)
  (wl-circular-complete "from" wl-from-list "wl-from-list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expansion : M-C-i
;;;

(defun wl-draft-addrbook-expand ()
  "Switch function for expand functions."
  (interactive)
  (let ((func (wl-draft-on-value-p wl-field-expansion-switch)))
    (if func
	(funcall (cdr func))
      (message "No expansion here"))))

(defun wl-addrbook-expand-address ()
  "Address expansion fuction for To:, Cc:, etc.
\"user@domain\" will be expands \"name <user@domain>\" if
the name exists."
  (interactive)
  (let ((word (wl-delete-backward-char)) name)
    (if (null word)
	(message "No address here")
      (setq name (wl-addrbook-name-get word))
      (insert
       (if name (format "%s <%s>" name word) word)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hart function for completions
;;;

(defun-maybe characterp (form)
  (numberp form))

(eval-and-compile
  (fset 'wl-complete-hit (symbol-function 'assoc)))

(defun wl-complete-get (key alist)
  (cdr (wl-complete-hit key alist)))

(defun wl-complete (WORD ALIST MSG EXPAND-CHAR &optional TRY ALL GET HIT)
  (let* ((ftry (or TRY (function try-completion)))
	 (fall (or ALL (function all-completions)))
	 (fget (or GET (function wl-complete-get)))
	 (fhit (or HIT (function wl-complete-hit)))
	 (cmp (funcall ftry WORD ALIST))
	 (all (funcall fall WORD ALIST))
	 (len (length WORD))
	 subkey)
    (cond
     ;; already completed
     ((eq cmp t)
      (if EXPAND-CHAR ;; may be "t"
	  (insert (funcall fget WORD ALIST)) ;; use cdr
	(insert WORD)) ;; use car
      (wl-complete-window-delete))
     ;; EXPAND
     ((and (characterp EXPAND-CHAR)
	   (char-equal (aref WORD (1- len)) EXPAND-CHAR)
	   (setq subkey (substring WORD 0 (1- len)))
	   (funcall fhit subkey ALIST))
      (insert (funcall fget subkey ALIST)) ;; use cdr
      (wl-complete-window-delete))
     ;; just one candidate
     ((equal 1 (length all))
      (insert cmp)
      (wl-complete-window-delete)
      (if (window-minibuffer-p (get-buffer-window (current-buffer)))
	  (wl-complete-temp-minibuffer-message " [Sole completion]")
	(message "Sole completion")))
     ;; two or more candidates
     ((stringp cmp) ;; (length all) > 1
      (insert cmp)
      (wl-complete-window-show all)
      (if (and EXPAND-CHAR (funcall fhit cmp ALIST))
	  (message
	   (substitute-command-keys
	    "To expand %s, type %c then '\\<wl-draft-mode-map>\\[wl-draft-addrbook-header-comp-or-tab]'.")
	   cmp EXPAND-CHAR)))
     ;; no candidate
     (t
      (insert WORD)
      (if (window-minibuffer-p (get-buffer-window (current-buffer)))
	  (wl-complete-temp-minibuffer-message (concat " No matching " MSG))
	(message "No matching %s" MSG))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Minibuf magic
;;;

(defun wl-complete-temp-minibuffer-message (m)
  (let ((savemax (point-max)))
    (save-excursion
      (goto-char (point-max))
      (insert m))
    (let ((inhibit-quit t))
      (sit-for 2)
      (delete-region savemax (point-max))
      (if quit-flag (setq quit-flag nil	unread-command-events 7)))))

;;
;; Extracting completion key
;;

(defun wl-delete-backward-char (&optional here)
  "Delete appropriate preceeding word and return it."
  (interactive)
  (let ((case-fold-search t)
        (start nil)
        (end (point))
        (regex (concat "[^" wl-complete-address-separator "]")))
    (save-excursion
      (while (and (not (bobp))
                  (string-match regex (buffer-substring-no-properties
                                       (1- (point)) (point))))
        (forward-char -1))
      (if (and here (not (re-search-forward (regexp-quote here) end t)))
          nil ;; "here" doesn't exist.
          (setq start (point))
          (if (= start end)
              (if here t nil) ;; just after "here",  just after separator
            (prog1
                (buffer-substring-no-properties start end)
              (delete-region start end)))))))

(defun wl-delete-value (&optional here)
  (beginning-of-line)
  (if (not (looking-at "[^:]+:"))
      ()
    (goto-char (match-end 0))
    (if (looking-at "[ \t]")
	(forward-char 1)
      (insert " "))
    (if (eolp)
	nil
      (let ((start (point)) ret)
	(end-of-line)
	(if (and here (re-search-backward (regexp-quote here) start t))
	    (progn
	      (setq start (1+ (point)))
	      (end-of-line)))
	(setq ret (buffer-substring-no-properties start (point)))
	(delete-region start (point))
	ret))))

;;
;; Making alist
;;

(defun wl-slide-pair (x)
  (let ((ret nil)
	(first (car x)))
    (cond 
     ((eq x 0) nil)
     ((eq x 1) (cons first first))
     (t
      (while (cdr x)
	(setq ret (cons (cons (nth 0 x) (nth 1 x)) ret))
	(setq x (cdr x)))
      (setq ret (cons (cons (car x) first) ret))
      (nreverse ret)))))

(provide 'wl-complete)

;;; Copyright Notice:

;; Copyright (C) 1997-2001 Mew developing team.
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

;;; wl-complete.el ends here
