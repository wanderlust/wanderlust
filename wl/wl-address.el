;;; wl-address.el -- Tiny address management for Wanderlust.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news
;; Time-stamp: <2000-03-03 00:59:01 teranisi>

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
(require 'std11)

(defvar wl-address-complete-header-regexp "^\\(To\\|From\\|Cc\\|Bcc\\|Mail-Followup-To\\|Reply-To\\|Return-Receipt-To\\):")
(defvar wl-newsgroups-complete-header-regexp "^\\(Newsgroups\\|Followup-To\\):")
(defvar wl-folder-complete-header-regexp "^\\(FCC\\):")
(defvar wl-address-list nil)
(defvar wl-address-completion-list nil)
(defvar wl-address-petname-hash nil)

(defun wl-complete-field-to ()
  (interactive)
  (let ((cl wl-address-completion-list))
    (if cl
	(completing-read "To: " cl)
      (read-string "To: "))))

(defun wl-complete-field-body-or-tab ()
  (interactive)
  (let ((case-fold-search t)
	epand-char skip-chars
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
	     (while (and (looking-at "^[ \t]")
			 (not (= (point) (point-min))))
	       (forward-line -1))
	     (cond ((looking-at wl-address-complete-header-regexp)
		    (setq completion-list wl-address-completion-list)
		    (setq epand-char ?@))
		   ((looking-at wl-folder-complete-header-regexp)
		    (setq completion-list wl-folder-entity-hashtb)
		    (setq skip-chars "^, "))
		   ((looking-at wl-newsgroups-complete-header-regexp)
		    (setq completion-list wl-folder-newsgroups-hashtb)))))
	  (wl-complete-field-body completion-list 
				  epand-char skip-chars)
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

(defun wl-complete-field-body (completion-list &optional epand-char skip-chars)
  (interactive)
  (let* ((end (point))
	 (start (save-excursion
;		  (skip-chars-backward "_a-zA-Z0-9+@%.!\\-")
		  (skip-chars-backward (or skip-chars 
					   "_a-zA-Z0-9+@%.!\\-/"))
		  (point)))
	 (completion)
	 (pattern (buffer-substring start end))
	 (len (length pattern))
	 (cl completion-list))
    (if (null cl)
	nil
      (setq completion (try-completion pattern cl))
      (cond ((eq completion t)
	     (wl-complete-insert start end pattern completion-list)
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

(defsubst wl-address-get-petname (str)
  (let ((addr (downcase (wl-address-header-extract-address str))))
    (or (elmo-get-hash-val addr wl-address-petname-hash)
	str)))

(defsubst wl-address-make-completion-list (address-list)
  (mapcar '(lambda (entity)
	     (cons (nth 0 entity)
		   (concat (nth 2 entity) " <"(nth 0 entity)">"))) address-list))

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
	(setq the-petname default-petname))

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
