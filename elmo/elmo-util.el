;;; elmo-util.el -- Utilities for Elmo.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
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

(eval-when-compile (require 'cl))
(require 'elmo-vars)
(require 'elmo-date)
(require 'std11)
(require 'eword-decode)
(require 'utf7)

(defmacro elmo-set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG."
  (cond ((boundp 'MULE)
         (list 'setq 'mc-flag flag))
        ((featurep 'xemacs)
         flag)
        ((and (boundp 'emacs-major-version) (>= emacs-major-version 20))
         (list 'set-buffer-multibyte flag))
        (t
         flag)))

(defvar elmo-work-buf-name " *elmo work*")
(defvar elmo-temp-buf-name " *elmo temp*")

(or (boundp 'default-enable-multibyte-characters)
    (defvar default-enable-multibyte-characters (featurep 'mule)
      "The mock variable except for Emacs 20."))

(defun elmo-base64-encode-string (string &optional no-line-break))
(defun elmo-base64-decode-string (string))

;; base64 encoding/decoding
(require 'mel)
(fset 'elmo-base64-encode-string
      (mel-find-function 'mime-encode-string "base64"))
(fset 'elmo-base64-decode-string
      (mel-find-function 'mime-decode-string "base64"))

;; Any Emacsen may have add-name-to-file(), because loadup.el requires it. :-p
;; Check make-symbolic-link() instead.  -- 981002 by Fuji
(if (fboundp 'make-symbolic-link)  ;; xxx
    (defalias 'elmo-add-name-to-file 'add-name-to-file)
  (defun elmo-add-name-to-file
    (filename newname &optional ok-if-already-exists)
    (copy-file filename newname ok-if-already-exists t)))

;; Nemacs's `read' is different.
(static-if (fboundp 'nemacs-version)
    (defun elmo-read (obj)
      (prog1 (read obj)
	(if (bufferp obj)
	    (or (bobp) (forward-char -1)))))
  (defalias 'elmo-read 'read))

(defmacro elmo-set-work-buf (&rest body)
  "Execute BODY on work buffer.  Work buffer remains."
  (` (save-excursion
       (set-buffer (get-buffer-create elmo-work-buf-name))
       (elmo-set-buffer-multibyte default-enable-multibyte-characters)
       (erase-buffer)
       (,@ body))))

(defmacro elmo-bind-directory (dir &rest body)
  "Set current directory DIR and execute BODY."
  (` (let ((default-directory (file-name-as-directory (, dir))))
       (,@ body))))

(defun elmo-object-load (filename &optional mime-charset no-err)
  "Load OBJECT from the file specified by FILENAME.
File content is decoded with MIME-CHARSET."
    (if (not (file-readable-p filename))
	nil
      (elmo-set-work-buf
       (as-binary-input-file
	(insert-file-contents filename))
       (when mime-charset
	 (elmo-set-buffer-multibyte default-enable-multibyte-characters)
	 (decode-mime-charset-region (point-min) (point-max) mime-charset))
       (condition-case nil
	   (read (current-buffer))
	 (error (unless no-err
		  (message "Warning: Loading object from %s failed."
			   filename)
		  (elmo-object-save filename nil))
		nil)))))

(defsubst elmo-save-buffer (filename &optional mime-charset)
  "Save current buffer to the file specified by FILENAME.
Directory of the file is created if it doesn't exist.
File content is encoded with MIME-CHARSET."
  (let ((dir (directory-file-name (file-name-directory filename))))
    (if (file-directory-p dir)
	() ; ok.
      (unless (file-exists-p dir)
	(elmo-make-directory dir)))
    (if (file-writable-p filename)
	(progn
	  (when mime-charset
;;;	    (elmo-set-buffer-multibyte default-enable-multibyte-characters)
	    (encode-mime-charset-region (point-min) (point-max) mime-charset))
	  (as-binary-output-file
	   (write-region (point-min) (point-max) filename nil 'no-msg)))
      (message (format "%s is not writable." filename)))))

(defun elmo-object-save (filename object &optional mime-charset)
  "Save OBJECT to the file specified by FILENAME.
Directory of the file is created if it doesn't exist.
File content is encoded with MIME-CHARSET."
  (elmo-set-work-buf
   (prin1 object (current-buffer))
;;;(princ "\n" (current-buffer))
   (elmo-save-buffer filename mime-charset)))

(defun elmo-get-network-stream-type (stream-type stream-type-alist)
  (catch 'found
    (while stream-type-alist
      (if (eq (nth 1 (car stream-type-alist)) stream-type)
	  (throw 'found (car stream-type-alist)))
      (setq stream-type-alist (cdr stream-type-alist)))))

;;; Search Condition

(defconst elmo-condition-atom-regexp "[^/ \")|&]*")

(defun elmo-read-search-condition (default)
  "Read search condition string interactively."
  (elmo-read-search-condition-internal "Search by" default))

(defun elmo-read-search-condition-internal (prompt default)
  (let* ((completion-ignore-case t)
	 (field (completing-read
		 (format "%s (%s): " prompt default)
		 (mapcar 'list
			 (append '("AND" "OR"
				   "Last" "First"
				   "From" "Subject" "To" "Cc" "Body"
				   "Since" "Before" "ToCc"
				   "!From" "!Subject" "!To" "!Cc" "!Body"
				   "!Since" "!Before" "!ToCc")
				 elmo-msgdb-extra-fields)) nil t))
	 value)
    (setq field (if (string= field "")
		    (setq field default)
		  field))
    (cond
     ((or (string= field "AND") (string= field "OR"))
      (concat "("
	      (elmo-read-search-condition-internal
	       (concat field "(1) Search by") default)
	      (if (string= field "AND") "&" "|")
	      (elmo-read-search-condition-internal
	       (concat field "(2) Search by") default)
	      ")"))
     ((string-match "Since\\|Before" field)
      (concat (downcase field) ":"
	      (completing-read (format "Value for '%s': " field)
			       (mapcar (function
					(lambda (x)
					  (list (format "%s" (car x)))))
				       elmo-date-descriptions))))
     (t
      (setq value (read-from-minibuffer (format "Value for '%s': " field)))
      (unless (string-match (concat "^" elmo-condition-atom-regexp "$")
			    value)
	(setq value (prin1-to-string value)))
      (concat (downcase field) ":" value)))))

(defsubst elmo-condition-parse-error ()
  (error "Syntax error in '%s'" (buffer-string)))

(defun elmo-parse-search-condition (condition)
  "Parse CONDITION.
Return value is a cons cell of (STRUCTURE . REST)"
  (with-temp-buffer
    (insert condition)
    (goto-char (point-min))
    (cons (elmo-condition-parse) (buffer-substring (point) (point-max)))))

;; condition    ::= or-expr
(defun elmo-condition-parse ()
  (or (elmo-condition-parse-or-expr)
      (elmo-condition-parse-error)))

;; or-expr      ::= and-expr /
;; 	            and-expr "|" or-expr
(defun elmo-condition-parse-or-expr ()
  (let ((left (elmo-condition-parse-and-expr)))
    (if (looking-at "| *")
	(progn
	  (goto-char (match-end 0))
	  (list 'or left (elmo-condition-parse-or-expr)))
      left)))

;; and-expr     ::= primitive /
;;                  primitive "&" and-expr
(defun elmo-condition-parse-and-expr ()
  (let ((left (elmo-condition-parse-primitive)))
    (if (looking-at "& *")
	(progn
	  (goto-char (match-end 0))
	  (list 'and left (elmo-condition-parse-and-expr)))
      left)))

;; primitive    ::= "(" expr ")" /
;;                  ["!"] search-key SPACE* ":" SPACE* search-value
(defun elmo-condition-parse-primitive ()
  (cond
   ((looking-at "( *")
    (goto-char (match-end 0))
    (prog1 (elmo-condition-parse)
      (unless (looking-at ") *")
	(elmo-condition-parse-error))
      (goto-char (match-end 0))))
;; search-key   ::= [A-Za-z-]+
;;                 ;; "since" / "before" / "last" / "first" /
;;                 ;; "body" / field-name
   ((looking-at "\\(!\\)? *\\([A-Za-z-]+\\) *: *")
    (goto-char (match-end 0))
    (let ((search-key (vector
		       (if (match-beginning 1) 'unmatch 'match)
		       (elmo-match-buffer 2)
		       (elmo-condition-parse-search-value))))
      ;; syntax sugar.
      (if (string= (aref search-key 1) "tocc")
	  (if (eq (aref search-key 0) 'match)
	      (list 'or
		    (vector 'match "to" (aref search-key 2))
		    (vector 'match "cc" (aref search-key 2)))
	    (list 'and
		  (vector 'unmatch "to" (aref search-key 2))
		  (vector 'unmatch "cc" (aref search-key 2))))
	search-key)))))

;; search-value ::= quoted / time / number / atom
;; quoted       ::= <elisp string expression>
;; time         ::= "yesterday" / "lastweek" / "lastmonth" / "lastyear" /
;;                   number SPACE* "daysago" /
;;                   number "-" month "-" number  ; ex. 10-May-2000
;; number       ::= [0-9]+
;; month        ::= "Jan" / "Feb" / "Mar" / "Apr" / "May" / "Jun" /
;;                  "Jul" / "Aug" / "Sep" / "Oct" / "Nov" / "Dec"
;; atom         ::= ATOM_CHARS*
;; SPACE        ::= <ascii space character, 0x20>
;; ATOM_CHARS   ::= <any character except specials>
;; specials     ::= SPACE / <"> / </> / <)> / <|> / <&>
;;                  ;; These characters should be quoted.
(defun elmo-condition-parse-search-value ()
  (cond
   ((looking-at "\"")
    (elmo-read (current-buffer)))
   ((or (looking-at "yesterday") (looking-at "lastweek")
	(looking-at "lastmonth") (looking-at "lastyear")
	(looking-at "[0-9]+ *daysago")
	(looking-at "[0-9]+-[A-Za-z]+-[0-9]+")
	(looking-at "[0-9]+")
	(looking-at elmo-condition-atom-regexp))
    (prog1 (elmo-match-buffer 0)
      (goto-char (match-end 0))))
   (t (error "Syntax error '%s'" (buffer-string)))))

;;;
(defsubst elmo-buffer-replace (regexp &optional newtext)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match (or newtext ""))))

(defsubst elmo-delete-char (char string &optional unibyte)
  (save-match-data
    (elmo-set-work-buf
     (let ((coding-system-for-read 'no-conversion)
	   (coding-system-for-write 'no-conversion))
       (if unibyte (elmo-set-buffer-multibyte nil))
       (insert string)
       (goto-char (point-min))
       (while (search-forward (char-to-string char) nil t)
	 (replace-match ""))
       (buffer-string)))))

(defsubst elmo-delete-cr-buffer ()
  "Delete CR from buffer."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n")) ))

(defsubst elmo-delete-cr-get-content-type ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n"))
    (goto-char (point-min))
    (or (std11-field-body "content-type")
	t)))

(defun elmo-delete-cr (string)
  (save-match-data
    (elmo-set-work-buf
     (insert string)
     (goto-char (point-min))
     (while (search-forward "\r\n" nil t)
       (replace-match "\n"))
     (buffer-string))))

(defun elmo-uniq-list (lst)
  "Distractively uniqfy elements of LST."
  (let ((tmp lst))
    (while tmp (setq tmp
		     (setcdr tmp
			     (and (cdr tmp)
				  (delete (car tmp)
					  (cdr tmp)))))))
  lst)

(defun elmo-string-partial-p (string)
  (and (stringp string) (string-match "message/partial" string)))

(defun elmo-get-file-string (filename &optional remove-final-newline)
  (elmo-set-work-buf
   (let (insert-file-contents-pre-hook   ; To avoid autoconv-xmas...
	 insert-file-contents-post-hook)
     (when (file-exists-p filename)
       (if filename
	   (as-binary-input-file (insert-file-contents filename)))
       (when (and remove-final-newline
		  (> (buffer-size) 0)
		  (= (char-after (1- (point-max))) ?\n))
	 (goto-char (point-max))
	 (delete-backward-char 1))
       (buffer-string)))))

(defun elmo-save-string (string filename)
  (if string
      (elmo-set-work-buf
       (as-binary-output-file
	(insert string)
	(write-region (point-min) (point-max)
		      filename nil 'no-msg))
       )))

(defun elmo-max-of-list (nlist)
  (let ((l nlist)
	(max-num 0))
    (while l
      (if (< max-num (car l))
	  (setq max-num (car l)))
      (setq l (cdr l)))
    max-num))

(defun elmo-concat-path (path filename)
  (if (not (string= path ""))
      (if (string= elmo-path-sep (substring path (- (length path) 1)))
	  (concat path filename)
	(concat path elmo-path-sep filename))
    filename))

(defvar elmo-passwd-alist nil)

(defun elmo-passwd-alist-load ()
  (save-excursion
    (let ((filename (expand-file-name elmo-passwd-alist-file-name
                                      elmo-msgdb-dir))
          (tmp-buffer (get-buffer-create " *elmo-passwd-alist-tmp*"))
	  insert-file-contents-pre-hook   ; To avoid autoconv-xmas...
          insert-file-contents-post-hook
          ret-val)
      (if (not (file-readable-p filename))
          ()
        (set-buffer tmp-buffer)
        (insert-file-contents filename)
        (setq ret-val
              (condition-case nil
                  (read (current-buffer))
                (error nil nil))))
      (kill-buffer tmp-buffer)
      ret-val)))

(defun elmo-passwd-alist-clear ()
  "Clear password cache."
  (interactive)
  (setq elmo-passwd-alist nil))
  
(defun elmo-passwd-alist-save ()
  "Save password into file."
  (interactive)
  (save-excursion
    (let ((filename (expand-file-name elmo-passwd-alist-file-name
                                      elmo-msgdb-dir))
          (tmp-buffer (get-buffer-create " *elmo-passwd-alist-tmp*")))
      (set-buffer tmp-buffer)
      (erase-buffer)
      (prin1 elmo-passwd-alist tmp-buffer)
      (princ "\n" tmp-buffer)
;;;   (if (and (file-exists-p filename)
;;;	       (not (equal 384 (file-modes filename))))
;;;	  (error "%s is not safe.chmod 600 %s!" filename filename))
      (if (file-writable-p filename)
         (progn
           (write-region (point-min) (point-max)
                         filename nil 'no-msg)
           (set-file-modes filename 384))
        (message (format "%s is not writable." filename)))
      (kill-buffer tmp-buffer))))

(defun elmo-get-passwd (key)
  "Get password from password pool."
  (let (pair pass)
    (if (not elmo-passwd-alist)
	(setq elmo-passwd-alist (elmo-passwd-alist-load)))
    (setq pair (assoc key elmo-passwd-alist))
    (if pair
	(elmo-base64-decode-string (cdr pair))
      (setq pass (elmo-read-passwd (format "Password for %s: "
					   key) t))
      (setq elmo-passwd-alist
	    (append elmo-passwd-alist
		    (list (cons key
				(elmo-base64-encode-string pass)))))
      (if elmo-passwd-life-time
	  (run-with-timer elmo-passwd-life-time nil
			  (` (lambda () (elmo-remove-passwd (, key))))))
      pass)))

(defun elmo-remove-passwd (key)
  "Remove password from password pool (for failure)."
  (let (pass-cons)
    (if (setq pass-cons (assoc key elmo-passwd-alist))
	(progn
	  (unwind-protect
	      (fillarray (cdr pass-cons) 0))
	  (setq elmo-passwd-alist
		(delete pass-cons elmo-passwd-alist))))))

(defmacro elmo-read-char-exclusive ()
  (cond ((featurep 'xemacs)
         '(let ((table (quote ((backspace . ?\C-h) (delete . ?\C-?)
                               (left . ?\C-h))))
                event key)
            (while (not
                    (and
                     (key-press-event-p (setq event (next-command-event)))
                     (setq key (or (event-to-character event)
                                   (cdr (assq (event-key event) table)))))))
            key))
        ((fboundp 'read-char-exclusive)
         '(read-char-exclusive))
        (t
         '(read-char))))

(defun elmo-read-passwd (prompt &optional stars)
  "Read a single line of text from user without echoing, and return it."
  (let ((ans "")
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t)
	(log-message-max-size 0)
	message-log-max	done msg truncate)
    (while (not done)
      (if (or (not stars) (string= "" ans))
	  (setq msg prompt)
	(setq msg (concat prompt (make-string (length ans) ?.)))
	(setq truncate
	      (1+ (- (length msg) (window-width (minibuffer-window)))))
	(and (> truncate 0)
	     (setq msg (concat "$" (substring msg (1+ truncate))))))
      (message "%s" msg)
      (setq c (elmo-read-char-exclusive))
      (cond ((= c ?\C-g)
	     (setq quit-flag t
		   done t))
	    ((or (= c ?\r) (= c ?\n) (= c ?\e))
	     (setq done t))
	    ((= c ?\C-u)
	     (setq ans ""))
	    ((and (/= c ?\b) (/= c ?\177))
	     (setq ans (concat ans (char-to-string c))))
	    ((> (length ans) 0)
	     (setq ans (substring ans 0 -1)))))
    (if quit-flag
	(prog1
	    (setq quit-flag nil)
	  (message "Quit")
	  (beep t))
      (message "")
      ans)))

(defun elmo-string-to-list (string)
  (elmo-set-work-buf
   (insert string)
   (goto-char (point-min))
   (insert "(")
   (goto-char (point-max))
   (insert ")")
   (goto-char (point-min))
   (read (current-buffer))))

(defun elmo-list-to-string (list)
  (let ((tlist list)
	str)
    (if (listp tlist)
	(progn
	  (setq str "(")
	  (while (car tlist)
	    (setq str
		  (concat str
			  (if (symbolp (car tlist))
			      (symbol-name (car tlist))
			    (car tlist))))
	    (if (cdr tlist)
		(setq str
		      (concat str " ")))
	    (setq tlist (cdr tlist)))
	  (setq str
		(concat str ")")))
      (setq str 
	    (if (symbolp tlist)
		(symbol-name tlist)
	      tlist)))
    str))
 

(defun elmo-plug-on-by-servers (alist &optional servers)
  (let ((server-list (or servers elmo-plug-on-servers)))
    (catch 'plugged
      (while server-list
	(if (elmo-plugged-p (car server-list))
	    (throw 'plugged t))
	(setq server-list (cdr server-list))))))

(defun elmo-plug-on-by-exclude-servers (alist &optional servers)
  (let ((server-list (or servers elmo-plug-on-exclude-servers))
	server other-servers)
    (while alist
      (when (and (not (member (setq server (caaar alist)) server-list))
		 (not (member server other-servers)))
	(push server other-servers))
      (setq alist (cdr alist)))
    (elmo-plug-on-by-servers alist other-servers)))

(defun elmo-plugged-p (&optional server port stream-type alist label-exp)
  (let ((alist (or alist elmo-plugged-alist))
	plugged-info)
    (cond ((and (not port) (not server))
	   (cond ((eq elmo-plugged-condition 'one)
		  (if alist
		      (catch 'plugged
			(while alist
			  (if (nth 2 (car alist))
			      (throw 'plugged t))
			  (setq alist (cdr alist))))
		    elmo-plugged))
		 ((eq elmo-plugged-condition 'all)
		  (if alist
		      (catch 'plugged
			(while alist
			  (if (not (nth 2 (car alist)))
			      (throw 'plugged nil))
			  (setq alist (cdr alist)))
			t)
		    elmo-plugged))
		 ((functionp elmo-plugged-condition)
		  (funcall elmo-plugged-condition alist))
		 (t ;; independent
		  elmo-plugged)))
	  ((not port) ;; server
	   (catch 'plugged
	     (while alist
	       (when (string= server (caaar alist))
		 (if (nth 2 (car alist))
		     (throw 'plugged t)))
	       (setq alist (cdr alist)))))
	  (t
	   (setq plugged-info (assoc (list server port stream-type) alist))
	   (if (not plugged-info)
	       ;; add elmo-plugged-alist automatically
	       (progn
		 (elmo-set-plugged elmo-plugged server port stream-type
				   nil nil nil label-exp)
		 elmo-plugged)
	     (if (and elmo-auto-change-plugged
		      (> elmo-auto-change-plugged 0)
		      (nth 3 plugged-info)  ;; time
		      (elmo-time-expire (nth 3 plugged-info)
					elmo-auto-change-plugged))
		 t
	       (nth 2 plugged-info)))))))

(defun elmo-set-plugged (plugged &optional server port stream-type time
				 alist label-exp add)
  (let ((alist (or alist elmo-plugged-alist))
	label plugged-info)
    (cond ((and (not port) (not server))
	   (setq elmo-plugged plugged)
	   ;; set plugged all element of elmo-plugged-alist.
	   (while alist
	     (setcdr (cdar alist) (list plugged time))
	     (setq alist (cdr alist))))
	  ((not port)
	   ;; set plugged all port of server
	   (while alist
	     (when (string= server (caaar alist))
	       (setcdr (cdar alist) (list plugged time)))
	     (setq alist (cdr alist))))
	  (t
	   ;; set plugged one port of server
	   (setq plugged-info (assoc (list server port stream-type) alist))
	   (setq label (if label-exp
			   (eval label-exp)
			 (nth 1 plugged-info)))
	   (if plugged-info
	       ;; if add is non-nil, don't reset plug state.
	       (unless add
		 (setcdr plugged-info (list label plugged time)))
	     (setq alist
		   (setq elmo-plugged-alist
			 (nconc
			  elmo-plugged-alist
			  (list
			   (list (list server port stream-type)
				 label plugged time))))))))
    alist))

(defun elmo-delete-plugged (&optional server port alist)
  (let* ((alist (or alist elmo-plugged-alist))
	 (alist2 alist))
    (cond ((and (not port) (not server))
	   (setq alist nil))
	  ((not port)
	   ;; delete plugged all port of server
	   (while alist2
	     (when (string= server (caaar alist2))
	       (setq alist (delete (car alist2) alist)))
	     (setq alist2 (cdr alist2))))
	  (t
	   ;; delete plugged one port of server
	   (setq alist
		 (delete (assoc (cons server port) alist) alist))))
    alist))

(defun elmo-disk-usage (path)
  "Get disk usage (bytes) in PATH."
  (let ((file-attr
	 (condition-case () (file-attributes path) (error nil))))
    (if file-attr
	(if (nth 0 file-attr) ; directory
	    (let ((files (condition-case ()
			     (directory-files path t "^[^\\.]")
			   (error nil)))
		  (result 0.0))
	      ;; (result (nth 7 file-attr))) ... directory size
	      (while files
		(setq result (+ result (or (elmo-disk-usage (car files)) 0)))
		(setq files (cdr files)))
	      result)
	  (float (nth 7 file-attr))))))

(defun elmo-get-last-accessed-time (path &optional dir)
  "Return the last accessed time of PATH."
  (let ((last-accessed (nth 4 (file-attributes (or (and dir
							(expand-file-name
							 path dir))
						   path)))))
    (if last-accessed
	(setq last-accessed (+ (* (nth 0 last-accessed)
				  (float 65536)) (nth 1 last-accessed)))
      0)))

(defun elmo-get-last-modification-time (path &optional dir)
  "Return the last accessed time of PATH."
  (let ((last-modified (nth 5 (file-attributes (or (and dir
							(expand-file-name
							 path dir))
						   path)))))
    (setq last-modified (+ (* (nth 0 last-modified)
			      (float 65536)) (nth 1 last-modified)))))

(defun elmo-make-directory (path)
  "Create directory recursively."
  (let ((parent (directory-file-name (file-name-directory path))))
    (if (null (file-directory-p parent))
	(elmo-make-directory parent))
    (make-directory path)
    (if (string= path (expand-file-name elmo-msgdb-dir))
	(set-file-modes path (+ (* 64 7) (* 8 0) 0))))) ; chmod 0700

(defun elmo-delete-directory (path &optional no-hierarchy)
  "Delete directory recursively."
  (if (stringp path) ; nil is not permitted.
  (let ((dirent (directory-files path))
	relpath abspath hierarchy)
    (while dirent
      (setq relpath (car dirent)
	    dirent (cdr dirent)
	    abspath (expand-file-name relpath path))
      (when (not (string-match "^\\.\\.?$" relpath))
	(if (eq (nth 0 (file-attributes abspath)) t)
	    (if no-hierarchy
		(setq hierarchy t)
	      (elmo-delete-directory abspath no-hierarchy))
	  (delete-file abspath))))
    (unless hierarchy
      (delete-directory path)))))

(defun elmo-list-filter (l1 l2)
  "L1 is filter."
  (if (eq l1 t)
      ;; t means filter all.
      nil
    (if l1
	(elmo-delete-if (lambda (x) (not (memq x l1))) l2)
      ;; filter is nil
      l2)))

(defsubst elmo-list-delete-if-smaller (list number)
  (let ((ret-val (copy-sequence list)))
    (while list
      (if (< (car list) number)
	  (setq ret-val (delq (car list) ret-val)))
      (setq list (cdr list)))
    ret-val))

(defun elmo-list-diff (list1 list2 &optional mes)
  (if mes
      (message mes))
  (let ((clist1 (copy-sequence list1))
	(clist2 (copy-sequence list2)))
    (while list2
      (setq clist1 (delq (car list2) clist1))
      (setq list2 (cdr list2)))
    (while list1
      (setq clist2 (delq (car list1) clist2))
      (setq list1 (cdr list1)))
    (if mes
	(message (concat mes "done.")))
    (list clist1 clist2)))

(defun elmo-list-bigger-diff (list1 list2 &optional mes)
  "Returns a list (- +). + is bigger than max of LIST1, in LIST2."
  (if (null list2)
      (cons list1  nil)
    (let* ((l1 list1)
	   (l2 list2)
	   (max-of-l2 (or (nth (max 0 (1- (length l2))) l2) 0))
	   diff1 num i percent
	   )
      (setq i 0)
      (setq num (+ (length l1)))
      (while l1
	(if (memq (car l1) l2)
	    (if (eq (car l1) (car l2))
		(setq l2 (cdr l2))
	      (delq (car l1) l2))
	  (if (> (car l1) max-of-l2)
	      (setq diff1 (nconc diff1 (list (car l1))))))
	(if mes
	    (progn
	      (setq i (+ i 1))
	      (setq percent (/ (* i 100) num))
	      (if (eq (% percent 5) 0)
		  (elmo-display-progress
		   'elmo-list-bigger-diff "%s%d%%" percent mes))))
	(setq l1 (cdr l1)))
      (cons diff1 (list l2)))))

(defmacro elmo-filter-type (filter)
  (` (aref (, filter) 0)))

(defmacro elmo-filter-key (filter)
  (` (aref (, filter) 1)))

(defmacro elmo-filter-value (filter)
  (` (aref (, filter) 2)))

(defsubst elmo-buffer-field-primitive-condition-match (condition
						       number
						       number-list)
  (let (result)
    (goto-char (point-min))
    (cond
     ((string= (elmo-filter-key condition) "last")
      (setq result (<= (length (memq number number-list))
		       (string-to-int (elmo-filter-value condition)))))
     ((string= (elmo-filter-key condition) "first")
      (setq result (< (- (length number-list)
			 (length (memq number number-list)))
		      (string-to-int (elmo-filter-value condition)))))
     ((string= (elmo-filter-key condition) "since")
      (let ((date (elmo-date-get-datevec (elmo-filter-value condition))))
	(setq result
	      (string<
	       (timezone-make-sortable-date (aref date 0)
					    (aref date 1)
					    (aref date 2)
					    (timezone-make-time-string
					     (aref date 3)
					     (aref date 4)
					     (aref date 5)))
	       (timezone-make-date-sortable (std11-field-body "date"))))))
     ((string= (elmo-filter-key condition) "before")
      (let ((date (elmo-date-get-datevec (elmo-filter-value condition))))
	(setq result
	      (string<
	       (timezone-make-date-sortable (std11-field-body "date"))
	       (timezone-make-sortable-date (aref date 0)
					    (aref date 1)
					    (aref date 2)
					    (timezone-make-time-string
					     (aref date 3)
					     (aref date 4)
					     (aref date 5)))))))
     ((string= (elmo-filter-key condition) "body")
      (and (re-search-forward "^$" nil t)	   ; goto body
	   (setq result (search-forward (elmo-filter-value condition)
					nil t))))
     (t
      (let ((fval (std11-field-body (elmo-filter-key condition))))
	(if (eq (length fval) 0) (setq fval nil))
	(if fval (setq fval (eword-decode-string fval)))
	(setq result (and fval (string-match
				(elmo-filter-value condition) fval))))))
    (if (eq (elmo-filter-type condition) 'unmatch)
	(setq result (not result)))
    result))

(defun elmo-condition-find-key-internal (condition key)
  (cond
   ((vectorp condition)
    (if (string= (elmo-filter-key condition) key)
	(throw 'found t)))
   ((or (eq (car condition) 'and)
	(eq (car condition) 'or))
    (elmo-condition-find-key-internal (nth 1 condition) key)
    (elmo-condition-find-key-internal (nth 2 condition) key))))

(defun elmo-condition-find-key (condition key)
  (catch 'found
    (elmo-condition-find-key-internal condition key)))

(defun elmo-buffer-field-condition-match (condition number number-list)
  (cond
   ((vectorp condition)
    (elmo-buffer-field-primitive-condition-match
     condition number number-list))
   ((eq (car condition) 'and)
    (and (elmo-buffer-field-condition-match
	  (nth 1 condition) number number-list)
	 (elmo-buffer-field-condition-match
	  (nth 2 condition) number number-list)))
   ((eq (car condition) 'or)
    (or (elmo-buffer-field-condition-match
	 (nth 1 condition) number number-list)
	(elmo-buffer-field-condition-match
	 (nth 2 condition) number number-list)))))

(defsubst elmo-file-field-condition-match (file condition number number-list)
  (elmo-set-work-buf
   (as-binary-input-file (insert-file-contents file))
   (elmo-set-buffer-multibyte default-enable-multibyte-characters)
   ;; Should consider charset?
   (decode-mime-charset-region (point-min)(point-max) elmo-mime-charset)
   (elmo-buffer-field-condition-match condition number number-list)))

(defmacro elmo-get-hash-val (string hashtable)
  (let ((sym (list 'intern-soft string hashtable)))
    (list 'if (list 'boundp sym)
       (list 'symbol-value sym))))

(defmacro elmo-set-hash-val (string value hashtable)
  (list 'set (list 'intern string hashtable) value))

(defmacro elmo-clear-hash-val (string hashtable)
  (static-if (fboundp 'unintern)
      (list 'unintern string hashtable)
    (list 'makunbound (list 'intern string hashtable))))

(defmacro elmo-unintern (string)
  "`unintern' symbol named STRING,  When can use `unintern'.
Emacs 19.28 or earlier does not have `unintern'."
  (static-if (fboundp 'unintern)
      (list 'unintern string)))

;; Make a hash table (default and minimum size is 1024).
(defun elmo-make-hash (&optional hashsize)
  (make-vector
   (if hashsize (max (min (elmo-create-hash-size hashsize)
			  elmo-hash-maximum-size) 1024) 1024) 0))

(defsubst elmo-mime-string (string)
  "Normalize MIME encoded STRING."
    (and string
	 (let (str)
	   (elmo-set-work-buf
	    (elmo-set-buffer-multibyte default-enable-multibyte-characters)
	    (setq str (eword-decode-string
		       (decode-mime-charset-string string elmo-mime-charset)))
	    (setq str (encode-mime-charset-string str elmo-mime-charset))
	    (elmo-set-buffer-multibyte nil)
	    str))))

(defsubst elmo-collect-field (beg end downcase-field-name)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((regexp (concat "\\(" std11-field-head-regexp "\\)[ \t]*"))
	    dest name body)
	(while (re-search-forward regexp nil t)
	  (setq name (buffer-substring-no-properties
		      (match-beginning 1)(1- (match-end 1))))
	  (if downcase-field-name
	      (setq name (downcase name)))
	  (setq body (buffer-substring-no-properties
		      (match-end 0) (std11-field-end)))
	  (or (assoc name dest)
	      (setq dest (cons (cons name body) dest))))
	dest))))

(defsubst elmo-collect-field-from-string (string downcase-field-name)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((regexp (concat "\\(" std11-field-head-regexp "\\)[ \t]*"))
	  dest name body)
      (while (re-search-forward regexp nil t)
	(setq name (buffer-substring-no-properties
		    (match-beginning 1)(1- (match-end 1))))
	(if downcase-field-name
	    (setq name (downcase name)))
	(setq body (buffer-substring-no-properties
		    (match-end 0) (std11-field-end)))
	(or (assoc name dest)
	    (setq dest (cons (cons name body) dest))))
      dest)))

(defun elmo-create-hash-size (min)
  (let ((i 1))
    (while (< i min)
      (setq i (* 2 i)))
    i))

(defun elmo-safe-filename (folder)
  (elmo-replace-in-string
   (elmo-replace-in-string
    (elmo-replace-in-string folder "/" " ")
    ":" "__")
   "|" "_or_"))

(defvar elmo-filename-replace-chars nil)

(defsubst elmo-replace-string-as-filename (msgid)
  "Replace string as filename."
  (setq msgid (elmo-replace-in-string msgid " " "  "))
  (if (null elmo-filename-replace-chars)
      (setq elmo-filename-replace-chars
	    (regexp-quote (mapconcat
			   'car elmo-filename-replace-string-alist ""))))
  (while (string-match (concat "[" elmo-filename-replace-chars "]")
		       msgid)
    (setq msgid (concat
		 (substring msgid 0 (match-beginning 0))
		 (cdr (assoc
		       (substring msgid
				  (match-beginning 0) (match-end 0))
		       elmo-filename-replace-string-alist))
		 (substring msgid (match-end 0)))))
  msgid)

(defsubst elmo-recover-string-from-filename (filename)
  "Recover string from FILENAME."
  (let (tmp result)
    (while (string-match " " filename)
      (setq tmp (substring filename
			   (match-beginning 0)
			   (+ (match-end 0) 1)))
      (if (string= tmp "  ")
	  (setq tmp " ")
	(setq tmp (car (rassoc tmp
			       elmo-filename-replace-string-alist))))
      (setq result
	    (concat result
		    (substring filename 0 (match-beginning 0))
		    tmp))
      (setq filename (substring filename (+ (match-end 0) 1))))
    (concat result filename)))

(defsubst elmo-copy-file (src dst)
  (condition-case err
      (elmo-add-name-to-file src dst t)
    (error (copy-file src dst t))))

(defsubst elmo-buffer-exists-p (buffer)
  (if (bufferp buffer)
      (buffer-live-p buffer)
    (get-buffer buffer)))

(defsubst elmo-kill-buffer (buffer)
  (when (elmo-buffer-exists-p buffer)
    (kill-buffer buffer)))

(defun elmo-delete-if (pred lst)
  "Return new list contain items which don't satisfy PRED in LST."
  (let (result)
    (while lst
      (unless (funcall pred (car lst))
	(setq result (nconc result (list (car lst)))))
      (setq lst (cdr lst)))
    result))

(defun elmo-list-delete (list1 list2)
  "Delete by side effect any occurrences equal to elements of LIST1 from LIST2.
Return the modified LIST2.  Deletion is done with `delete'.
Write `(setq foo (elmo-list-delete bar foo))' to be sure of changing
the value of `foo'."
  (while list1
    (setq list2 (delete (car list1) list2))
    (setq list1 (cdr list1)))
  list2)

(defun elmo-list-member (list1 list2)
  "If any element of LIST1 is member of LIST2, return t."
  (catch 'done
    (while list1
      (if (member (car list1) list2)
	  (throw 'done t))
      (setq list1 (cdr list1)))))

(defun elmo-count-matches (regexp beg end)
  (let ((count 0))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp end t)
	(setq count (1+ count)))
      count)))

(if (fboundp 'display-error)
    (defalias 'elmo-display-error 'display-error)
  (defun elmo-display-error (error-object stream)
    "A tiny function to display ERROR-OBJECT to the STREAM."
    (let ((first t)
	  (errobj error-object)
	  err-mes)
      (while errobj
	(setq err-mes (concat err-mes (format
				       (if (stringp (car errobj))
					   "%s"
					 (if (boundp 'nemacs-version)
					     "%s"
					   "%S")) (car errobj))))
	(setq errobj (cdr errobj))
	(if errobj (setq err-mes (concat err-mes (if first ": " ", "))))
	(setq first nil))
      (princ err-mes stream))))

(if (fboundp 'define-error)
    (defalias 'elmo-define-error 'define-error)
  (defun elmo-define-error (error doc &optional parents)
    (or parents
	(setq parents 'error))
    (let ((conds (get parents 'error-conditions)))
      (or conds
	  (error "Not an error symbol: %s" error))
      (setplist error
		(list 'error-message doc
		      'error-conditions (cons error conds))))))

(cond ((fboundp 'lprogress-display)
       (defalias 'elmo-display-progress 'lprogress-display))
      ((fboundp 'progress-feedback-with-label)
       (defalias 'elmo-display-progress 'progress-feedback-with-label))
      (t
       (defun elmo-display-progress (label format &optional value &rest args)
	 "Print a progress message."
	 (if (and (null format) (null args))
	     (message nil)
	   (apply (function message) (concat format " %d%%")
		  (nconc args (list value)))))))

(defun elmo-time-expire (before-time diff-time)
  (let* ((current (current-time))
	 (rest (when (< (nth 1 current) (nth 1 before-time))
		 (expt 2 16)))
	 diff)
    (setq diff
	  (list (- (+ (car current) (if rest -1 0)) (car before-time))
		(- (+ (or rest 0) (nth 1 current)) (nth 1 before-time))))
    (and (eq (car diff) 0)
	 (< diff-time (nth 1 diff)))))

(if (fboundp 'std11-fetch-field)
    (defalias 'elmo-field-body 'std11-fetch-field) ;;no narrow-to-region
  (defalias 'elmo-field-body 'std11-field-body))

(defmacro elmo-string (string)
  "STRING without text property."
  (` (let ((obj (copy-sequence (, string))))
       (set-text-properties 0 (length obj) nil obj)
       obj)))

(defun elmo-flatten (list-of-list)
  "Flatten LIST-OF-LIST."
  (unless (null list-of-list)
    (append (if (and (car list-of-list)
		     (listp (car list-of-list)))
		(car list-of-list)
	      (list (car list-of-list)))
	    (elmo-flatten (cdr list-of-list)))))

(defun elmo-y-or-n-p (prompt &optional auto default)
  "Same as `y-or-n-p'.
But if optional argument AUTO is non-nil, DEFAULT is returned."
  (if auto
      default
    (y-or-n-p prompt)))

(defun elmo-string-member (string slist)
  "Return t if STRING is a member of the SLIST."
  (catch 'found
    (while slist
      (if (and (stringp (car slist))
	       (string= string (car slist)))
	  (throw 'found t))
      (setq slist (cdr slist)))))

(defun elmo-string-match-member (str list &optional case-ignore)
  (let ((case-fold-search case-ignore))
    (catch 'member
      (while list
	(if (string-match (car list) str)
	    (throw 'member (car list)))
	(setq list (cdr list))))))

(defun elmo-string-matched-member (str list &optional case-ignore)
  (let ((case-fold-search case-ignore))
    (catch 'member
      (while list
	(if (string-match str (car list))
	    (throw 'member (car list)))
	(setq list (cdr list))))))

(defsubst elmo-string-delete-match (string pos)
  (concat (substring string
		     0 (match-beginning pos))
	  (substring string
		     (match-end pos)
		     (length string))))

(defun elmo-string-match-assoc (key alist &optional case-ignore)
  (let ((case-fold-search case-ignore)
	a)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(if (and (consp a)
		 (stringp (car a))
		 (string-match key (car a)))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun elmo-string-matched-assoc (key alist &optional case-ignore)
  (let ((case-fold-search case-ignore)
	a)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(if (and (consp a)
		 (stringp (car a))
		 (string-match (car a) key))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun elmo-string-assoc (key alist)
  (let (a)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(if (and (consp a)
		 (stringp (car a))
		 (string= key (car a)))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun elmo-string-rassoc (key alist)
  (let (a)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(if (and (consp a)
		 (stringp (cdr a))
		 (string= key (cdr a)))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun elmo-string-rassoc-all (key alist)
  (let (matches)
    (while alist
      (if (string= key (cdr (car alist)))
	  (setq matches
		(cons (car alist)
		      matches)))
      (setq alist (cdr alist)))
    matches))

;;; Number set defined by OKAZAKI Tetsurou <okazaki@be.to>
;; 
;; number          ::= [0-9]+
;; beg             ::= number
;; end             ::= number
;; number-range    ::= "(" beg " . " end ")"      ;; cons cell
;; number-set-elem ::= number / number-range
;; number-set      ::= "(" *number-set-elem ")"   ;; list

(defun elmo-number-set-member (number number-set)
  "Return non-nil if NUMBER is an element of NUMBER-SET.
The value is actually the tail of NUMBER-RANGE whose car contains NUMBER."
  (or (memq number number-set)
      (let (found)
	(while (and number-set (not found))
	  (if (and (consp (car number-set))
		   (and (<= (car (car number-set)) number)
			(<= number (cdr (car number-set)))))
	      (setq found t)
	    (setq number-set (cdr number-set))))
	number-set)))

(defun elmo-number-set-append-list (number-set list)
  "Append LIST of numbers to the NUMBER-SET.
NUMBER-SET is altered."
  (let ((appended number-set))
    (while list
      (setq appended (elmo-number-set-append appended (car list)))
      (setq list (cdr list)))
    appended))

(defun elmo-number-set-append (number-set number)
  "Append NUMBER to the NUMBER-SET.
NUMBER-SET is altered."
  (let ((number-set-1 number-set)
	found elem)
    (while (and number-set (not found))
      (setq elem (car number-set))
      (cond
       ((and (consp elem)
	     (eq (+ 1 (cdr elem)) number))
	(setcdr elem number)
	(setq found t))
       ((and (integerp elem)
	     (eq (+ 1 elem) number))
	(setcar number-set (cons elem number))
	(setq found t))
       ((or (and (integerp elem) (eq elem number))
	    (and (consp elem)
		 (<= (car elem) number)
		 (<= number (cdr elem))))
	(setq found t)))
      (setq number-set (cdr number-set)))
    (if (not found)
	(setq number-set-1 (nconc number-set-1 (list number))))
    number-set-1))

(defun elmo-number-set-to-number-list (number-set)
  "Return a number list which corresponds to NUMBER-SET."
  (let (number-list elem i)
    (while number-set
      (setq elem (car number-set))
      (cond
       ((consp elem)
	(setq i (car elem))
	(while (<= i (cdr elem))
	  (setq number-list (cons i number-list))
	  (incf i)))
       ((integerp elem)
	(setq number-list (cons elem number-list))))
      (setq number-set (cdr number-set)))
    (nreverse number-list)))

(defcustom elmo-list-subdirectories-ignore-regexp "^\\(\\.\\.?\\|[0-9]+\\)$"
  "*Regexp to filter subfolders."
  :type 'regexp
  :group 'elmo)

(defun elmo-list-subdirectories (directory file one-level)
  (let ((root (zerop (length file)))
	(w32-get-true-file-link-count t) ; for Meadow
	files attr dirs dir)
    (setq files (directory-files (setq dir (expand-file-name file directory))))
    (while files
      (if (and (not (string-match elmo-list-subdirectories-ignore-regexp
				  (car files)))
	       (car (setq attr (file-attributes (expand-file-name 
						 (car files) dir)))))
	  (if (and (not one-level)
		   (and elmo-have-link-count (< 2 (nth 1 attr))))
	      (setq dirs
		    (nconc dirs
			   (elmo-list-subdirectories
			    directory
			    (concat file
				    (and (not root) elmo-path-sep)
				    (car files))
			    one-level)))
	    (setq dirs (nconc dirs
			      (list
			       (concat file
				       (and (not root) elmo-path-sep)
				       (car files)))))))
      (setq files (cdr files)))
    (nconc (and (not root) (list file)) dirs)))

(require 'product)
(product-provide (provide 'elmo-util) (require 'elmo-version))

;;; elmo-util.el ends here
