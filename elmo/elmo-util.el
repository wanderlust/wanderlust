;;; elmo-util.el -- Utilities for Elmo.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news
;; Time-stamp: <2000-03-29 09:42:41 teranisi>

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

(require 'elmo-vars)
(require 'elmo-date)
(eval-when-compile (require 'cl))
(require 'std11)
(require 'eword-decode)
(require 'utf7)

(eval-when-compile 
  (condition-case nil 
      (progn
	(require 'ssl)
	(require 'starttls))
    (error))
  (defun-maybe starttls-negotiate (a))
  (defun-maybe starttls-open-stream (a b c d))
  (defun-maybe open-ssl-stream (a b c d)))

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

(require 'broken)
(broken-facility timezone-y2k
  "timezone.el does not clear Y2K."
  (or (not (featurep 'timezone))
      (string= (aref (timezone-parse-date "Sat, 1 Jan 00 07:00:00 JST") 0) 
	       "2000")))

(when-broken timezone-y2k
  (defun timezone-parse-date (date)
    "Parse DATE and return a vector [YEAR MONTH DAY TIME TIMEZONE].
19 is prepended to year if necessary.  Timezone may be nil if nothing.
Understands the following styles:
 (1) 14 Apr 89 03:20[:12] [GMT]
 (2) Fri, 17 Mar 89 4:01[:33] [GMT]
 (3) Mon Jan 16 16:12[:37] [GMT] 1989
 (4) 6 May 1992 1641-JST (Wednesday)
 (5) 22-AUG-1993 10:59:12.82
 (6) Thu, 11 Apr 16:17:12 91 [MET]
 (7) Mon, 6  Jul 16:47:20 T 1992 [MET]"
    (condition-case nil
	(progn
	  ;; Get rid of any text properties.
	  (and (stringp date)
	       (or (text-properties-at 0 date)
		   (next-property-change 0 date))
	       (setq date (copy-sequence date))
	       (set-text-properties 0 (length date) nil date))
	  (let ((date (or date ""))
		(year nil)
		(month nil)
		(day nil)
		(time nil)
		(zone nil))			;This may be nil.
	    (cond ((string-match
		    "\\([^ \t,]+\\),[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\(T[ \t]+\\|\\)\\([0-9]+\\)[ \t]*\\'" date)
		   ;; Styles: (6) and (7) without timezone
		   (setq year 6 month 3 day 2 time 4 zone nil))
		  ((string-match
		    "\\([^ \t,]+\\),[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\(T[ \t]+\\|\\)\\([0-9]+\\)[ \t]*\\([-+a-zA-Z0-9]+\\)" date)
		   ;; Styles: (6) and (7) with timezone and buggy timezone
		   (setq year 6 month 3 day 2 time 4 zone 7))
		  ((string-match
		    "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]*\\'" date)
		   ;; Styles: (1) and (2) without timezone
		   (setq year 3 month 2 day 1 time 4 zone nil))
		  ((string-match
		    "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]*\\([-+a-zA-Z0-9]+\\)" date)
		   ;; Styles: (1) and (2) with timezone and buggy timezone
		   (setq year 3 month 2 day 1 time 4 zone 5))
		  ((string-match
		    "\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\([0-9]+\\)" date)
		   ;; Styles: (3) without timezone
		   (setq year 4 month 1 day 2 time 3 zone nil))
		  ((string-match
		    "\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\([-+a-zA-Z0-9]+\\)[ \t]+\\([0-9]+\\)" date)
		   ;; Styles: (3) with timezone
		   (setq year 5 month 1 day 2 time 3 zone 4))
		  ((string-match
		    "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]*\\([-+a-zA-Z0-9]+\\)" date)
		   ;; Styles: (4) with timezone
		   (setq year 3 month 2 day 1 time 4 zone 5))
		  ((string-match
		    "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)\\.[0-9]+" date)
		   ;; Styles: (5) without timezone.
		   (setq year 3 month 2 day 1 time 4 zone nil))
		  )
	    (if year
		(progn
		  (setq year
			(substring date (match-beginning year) 
				   (match-end year)))
		  (if (< (length year) 4)
		      (let ((yr (string-to-int year)))
			(when (>= yr 100)
			  (setq yr (- yr 100)))
			(setq year (format "%d%02d"
					   (if (< yr 70)
					       20
					     19)
					   yr))))
		  (let ((string (substring date
					   (match-beginning month)
					   (+ (match-beginning month) 3))))
		    (setq month
			  (int-to-string
			   (cdr (assoc (upcase string) 
				       timezone-months-assoc)))))
		  (setq day
			(substring date (match-beginning day) (match-end day)))
		  (setq time
			(substring date (match-beginning time) 
				   (match-end time)))))
	    (if zone
		(setq zone
		      (substring date (match-beginning zone) 
				 (match-end zone))))
	    (if year
		(vector year month day time zone)
	      (vector "0" "0" "0" "0" nil))
	    )
	  )
      (t (signal 'invalid-date (list date))))))

(defsubst elmo-call-func (folder func-name &rest args)
  (let* ((spec (if (stringp folder)
		   (elmo-folder-get-spec folder)
		 folder))
	 (type (symbol-name (car spec)))
	 (backend-str (concat "elmo-" type))
	 (backend-sym (intern backend-str)))
    (unless (featurep backend-sym)
      (require backend-sym))
    (apply (intern (format "%s-%s" backend-str func-name))
	   spec
	   args)))

(defmacro elmo-set-work-buf (&rest body)
  "Execute BODY on work buffer. Work buffer remains."
  (` (save-excursion
       (set-buffer (get-buffer-create elmo-work-buf-name))
       (elmo-set-buffer-multibyte default-enable-multibyte-characters)
       (erase-buffer)
       (,@ body))))

(defmacro elmo-match-substring (pos string from)
  "Substring of POSth matched string of STRING. "
  (` (substring (, string) 
		(+ (match-beginning (, pos)) (, from))
		(match-end (, pos)))))

(defmacro elmo-match-string (pos string)
  "Substring POSth matched string."
  (` (substring (, string) (match-beginning (, pos)) (match-end (, pos)))))

(defmacro elmo-match-buffer (pos)
  "Substring POSth matched from the current buffer."
  (` (buffer-substring-no-properties
      (match-beginning (, pos)) (match-end (, pos)))))

(defmacro elmo-bind-directory (dir &rest body)
  "Set current directory DIR and execute BODY."
  (` (let ((default-directory (file-name-as-directory (, dir))))
       (,@ body))))

(defmacro elmo-folder-get-type (folder)
  "Get type of FOLDER."
  (` (and (stringp (, folder))
	  (cdr (assoc (string-to-char (, folder)) elmo-spec-alist)))))

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
	    ;;(elmo-set-buffer-multibyte default-enable-multibyte-characters)
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
   ;;(princ "\n" (current-buffer))
   (elmo-save-buffer filename mime-charset)))

(defsubst elmo-imap4-decode-folder-string (string)
  (if elmo-imap4-use-modified-utf7
      (utf7-decode-string string 'imap)
    string))

(defsubst elmo-imap4-encode-folder-string (string)
  (if elmo-imap4-use-modified-utf7
      (utf7-encode-string string 'imap)
    string))

(defun elmo-network-get-spec (folder default-server default-port default-tls)
  (let (server port tls)
    (if (string-match "\\(@[^@:/!]+\\)?\\(:[0-9]+\\)?\\(!*\\)$" folder)
	(progn
	  (if (match-beginning 1)
	      (setq server (elmo-match-substring 1 folder 1))
	    (setq server default-server))
	  (if (match-beginning 2)
	      (setq port 
		    (string-to-int (elmo-match-substring 2 folder 1)))
	    (setq port default-port))
	  (setq tls (elmo-match-string 3 folder))
	  (if (and (match-beginning 3)
		   (> (length tls) 0))
	      (setq tls (if (= 2 (length tls)) 'starttls
			  (string= tls "!")))
	    (setq tls default-tls))
	  (setq folder (substring folder 0 (match-beginning 0))))
      (setq server default-server
	    port   default-port
	    tls    default-tls))
    (cons folder (list server port tls))))

(defun elmo-imap4-get-spec (folder)
  (let ((default-user    elmo-default-imap4-user)
	(default-server  elmo-default-imap4-server)
	(default-port    elmo-default-imap4-port)
	(default-tls     elmo-default-imap4-ssl)
	spec mailbox user auth)
    (when (string-match "\\(.*\\)@\\(.*\\)" default-server)
      ;; case: default-imap4-server is specified like 
      ;; "hoge%imap.server@gateway".
      (setq default-user (elmo-match-string 1 default-server))
      (setq default-server (elmo-match-string 2 default-server)))
    (setq spec (elmo-network-get-spec 
		folder default-server default-port default-tls))
    (setq folder (car spec))
    (when (string-match
	   "^\\(%\\)\\([^:@!]*\\)\\(:[^/!]+\\)?\\(/[^/:@!]+\\)?"
	   folder)
      (progn
	(setq mailbox (if (match-beginning 2) 
			  (elmo-match-string 2 folder)
			elmo-default-imap4-mailbox))
	(setq user (if (match-beginning 3)
		       (elmo-match-substring 3 folder 1)
		     default-user))
	(setq auth (if (match-beginning 4)
		       (elmo-match-substring 4 folder 1)
		     elmo-default-imap4-authenticate-type))
	(append (list 'imap4 
		      (elmo-imap4-encode-folder-string mailbox)
		      user auth)
		(cdr spec))))))

(defsubst elmo-imap4-spec-mailbox (spec)
  (nth 1 spec))

(defsubst elmo-imap4-spec-username (spec)
  (nth 2 spec))

(defsubst elmo-imap4-spec-auth (spec)
  (nth 3 spec))

(defsubst elmo-imap4-spec-hostname (spec)
  (nth 4 spec))

(defsubst elmo-imap4-spec-port (spec)
  (nth 5 spec))

(defsubst elmo-imap4-spec-ssl (spec)
  (nth 6 spec))

(defsubst elmo-imap4-spec-folder (spec) ;; obsolete
  (nth 1 spec))

(defsubst elmo-imap4-connection-get-process (conn)
  (nth 1 conn))

(defsubst elmo-imap4-connection-get-buffer (conn)
  (nth 0 conn))

(defsubst elmo-imap4-connection-get-cwf (conn)
  (nth 2 conn))

(defun elmo-nntp-get-spec (folder)
  (let (spec group user)
    (setq spec (elmo-network-get-spec folder
				      elmo-default-nntp-server
				      elmo-default-nntp-port
				      elmo-default-nntp-ssl))
    (setq folder (car spec))
    (when (string-match
	   "^\\(-\\)\\([^:@!]*\\)\\(:[^/!]+\\)?\\(/[^/:@!]+\\)?"
	   folder)
      (setq group 
	    (if (match-beginning 2)
		(elmo-match-string 2 folder)))
      (setq user 
	    (if (match-beginning 3) 
		(elmo-match-substring 3 folder 1)
	      elmo-default-nntp-user))
      (append (list 'nntp group user)
	      (cdr spec)))))

(defsubst elmo-nntp-spec-group (spec)
  (nth 1 spec))

(defsubst elmo-nntp-spec-username (spec)  
  (nth 2 spec))

;; future use?
;; (defsubst elmo-nntp-spec-auth (spec))

(defsubst elmo-nntp-spec-hostname (spec)
  (nth 3 spec))

(defsubst elmo-nntp-spec-port (spec)
  (nth 4 spec))

(defsubst elmo-nntp-spec-ssl (spec)
  (nth 5 spec))

(defun elmo-localdir-get-spec (folder)
  (let (fld-name path)
    (when (string-match
	   "^\\(\\+\\)\\(.*\\)$"
	   folder)
      (if (eq (length (setq fld-name
			    (elmo-match-string 2 folder))) 0)
	  (setq fld-name "")
	)
      (if (file-name-absolute-p fld-name)
	  (setq path (expand-file-name fld-name))
	(setq path fld-name))
	;(setq path (expand-file-name fld-name
	;elmo-localdir-folder-path)))
      (list (if (elmo-folder-maildir-p folder)
		'maildir
	      'localdir) path))))

(defun elmo-maildir-get-spec (folder)
  (let (fld-name path)
    (when (string-match
	   "^\\(\\.\\)\\(.*\\)$"
	   folder)
      (if (eq (length (setq fld-name
			    (elmo-match-string 2 folder))) 0)
	  (setq fld-name ""))
      (if (file-name-absolute-p fld-name)
	  (setq path (expand-file-name fld-name))
	(setq path fld-name))
      (list 'maildir path))))

(defun elmo-folder-maildir-p (folder)
  (catch 'found
    (let ((li elmo-maildir-list))
      (while li
	(if (string-match (car li) folder)
	    (throw 'found t))
	(setq li (cdr li))))))

(defun elmo-localnews-get-spec (folder)
  (let (fld-name)
    (when (string-match
	 "^\\(=\\)\\(.*\\)$"
	 folder)
      (if (eq (length (setq fld-name
			    (elmo-match-string 2 folder))) 0)
	  (setq fld-name "")
	)
      (list 'localnews 
	    (elmo-replace-in-string fld-name "\\." "/")))))

(defun elmo-cache-get-spec (folder)
  (let (fld-name)
    (when (string-match
	 "^\\(!\\)\\(.*\\)$"
	 folder)
      (if (eq (length (setq fld-name
			    (elmo-match-string 2 folder))) 0)
	  (setq fld-name "")
	)
      (list 'cache
	    (elmo-replace-in-string fld-name "\\." "/")))))

;; Archive interface by OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
(defun elmo-archive-get-spec (folder)
  (require 'elmo-archive)
  (let (fld-name type prefix)
    (when (string-match
	   "^\\(\\$\\)\\([^;]*\\);?\\([^;]*\\);?\\([^;]*\\)$"
	   folder)
      ;; Drive letter is OK!
      (if (eq (length (setq fld-name
			    (elmo-match-string 2 folder))) 0)
	  (setq fld-name "")
	)
      (if (eq (length (setq type
			    (elmo-match-string 3 folder))) 0)
	  (setq type (symbol-name elmo-archive-default-type)))
      (if (eq (length (setq prefix
			    (elmo-match-string 4 folder))) 0)
	  (setq prefix ""))
      (list 'archive fld-name (intern-soft type) prefix))))

(defun elmo-pop3-get-spec (folder)
  (let (spec user auth)
    (setq spec (elmo-network-get-spec folder
				      elmo-default-pop3-server
				      elmo-default-pop3-port
				      elmo-default-pop3-ssl))
    (setq folder (car spec))
    (when (string-match
	   "^\\(&\\)\\([^:/!]*\\)\\(/[^/:@!]+\\)?"
	   folder)
      (setq user (if (match-beginning 2)
		     (elmo-match-string 2 folder)))
      (if (eq (length user) 0)
	  (setq user elmo-default-pop3-user))
      (setq auth (if (match-beginning 3)
		     (elmo-match-substring 3 folder 1)
		   elmo-default-pop3-authenticate-type))
      (append (list 'pop3 user auth)
	      (cdr spec)))))

(defsubst elmo-pop3-spec-username (spec)
  (nth 1 spec))

(defsubst elmo-pop3-spec-auth (spec)
  (nth 2 spec))

(defsubst elmo-pop3-spec-hostname (spec)
  (nth 3 spec))

(defsubst elmo-pop3-spec-port (spec)
  (nth 4 spec))

(defsubst elmo-pop3-spec-ssl (spec)
  (nth 5 spec))

(defun elmo-internal-get-spec (folder)
  (if (string-match "\\('\\)\\([^/]*\\)/?\\(.*\\)$" folder)
      (let* ((item (downcase (elmo-match-string 2 folder)))
	     (sym (and (> (length item) 0) (intern item))))
	(cond ((or (null sym)
		   (eq sym 'mark))
	       (list 'internal sym (elmo-match-string 3 folder)))
	      ((eq sym 'cache)
	       (list 'cache (elmo-match-string 3 folder)))
	      (t (error "Invalid internal folder spec"))))))

(defun elmo-multi-get-spec (folder)
  (save-match-data
    (when (string-match
	   "^\\(\\*\\)\\(.*\\)$"
	   folder)
      (append (list 'multi)
	      (split-string
	       (elmo-match-string 2 folder)
	       ",")))))

(defun elmo-filter-get-spec (folder)
  (save-match-data
    (when (string-match
	   "^\\(/\\)\\(.*\\)$"
	   folder)
      (let ((spec (elmo-match-string 2 folder))
	    filter)
	(when (string-match "\\([^/]+\\)/" spec)
	  (setq filter (elmo-match-string 1 spec))
	  (setq spec (substring spec (match-end 0))))
	(cond
	 ((string-match "^\\(last\\|first\\):\\(.*\\)$" filter) ; partial
	  (setq filter (vector 'partial
			       (elmo-match-string 1 filter)
			       (elmo-match-string 2 filter))))
	 (t
	  (setq filter (elmo-parse-search-condition filter))))
	(list 'filter filter spec)))))

(defun elmo-pipe-get-spec (folder)
  (when (string-match "^\\(|\\)\\([^|]*\\)|\\(.*\\)$" folder)
    (list 'pipe
	  (elmo-match-string 2 folder)
	  (elmo-match-string 3 folder))))

(defun elmo-folder-get-spec (folder)
  "return spec of folder"
  (let ((type (elmo-folder-get-type folder)))
    (if type
	(funcall (intern (concat "elmo-" (symbol-name type) "-get-spec"))
		 folder)
      (error "%s is not supported folder type" folder))))

(defun elmo-parse-search-condition (condition)
  (let ((terms (split-string condition "|")) ; split by OR
	term ret-val)
    (while terms
      (setq term (car terms))
      (cond 
       ((string-match "^\\([a-zA-Z\\-]+\\)=\\(.*\\)$" term)
	(if (save-match-data
	      (string-match "tocc" (elmo-match-string 1 term))) ;; syntax sugar
	    (setq ret-val (nconc
			   ret-val
			   (list (vector 'match "to"
					 (elmo-match-string 2 term))
				 (vector 'match "cc"
					 (elmo-match-string 2 term)))))
	  (setq ret-val (cons (vector 'match 
				      (elmo-match-string 1 term)
				      (elmo-match-string 2 term))
			      ret-val))))
       ((string-match "^\\([a-zA-Z\\-]+\\)!=\\(.*\\)$" term)
	(if (save-match-data
	      (string-match "tocc" (elmo-match-string 1 term))) ;; syntax sugar
	    (setq ret-val (nconc
			   ret-val
			   (list (vector 'unmatch "to"
					 (elmo-match-string 2 term))
				 (vector 'unmatch "cc"
					 (elmo-match-string 2 term)))))
	  (setq ret-val (cons (vector 'unmatch 
				      (elmo-match-string 1 term)
				      (elmo-match-string 2 term))
			      ret-val))))
       ((string-match "^\\(since\\|before\\):\\(.*\\)$" term)
	(setq ret-val (cons (vector 'date
				    (elmo-match-string 1 term)
				    (elmo-match-string 2 term))
			    ret-val))))
      (setq terms (cdr terms)))
    ret-val))

(defun elmo-multi-get-real-folder-number (folder number)
  (let* ((spec (elmo-folder-get-spec folder))
	 (flds (cdr spec))
	 (num number)
	 (fld (nth (- (/ num elmo-multi-divide-number) 1) flds)))
    (cons fld (% num elmo-multi-divide-number))))

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
    (while tmp (setq tmp (setcdr tmp (and (cdr tmp) (delete (car tmp) (cdr tmp)))))))
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
;      (if (and (file-exists-p filename)
;             (not (equal 384 (file-modes filename))))
;        (error "%s is not safe.chmod 600 %s!" filename filename))
      (if (file-writable-p filename)
         (progn
           (write-region (point-min) (point-max) 
                         filename nil 'no-msg)
           (set-file-modes filename 384))
        (message (format "%s is not writable." filename)))
      (kill-buffer tmp-buffer))))

(defun elmo-get-passwd (user-at-host)
  "Get password from password pool."
  (let (data pass)
    (if (not elmo-passwd-alist)
	(setq elmo-passwd-alist (elmo-passwd-alist-load)))
    (setq data (assoc user-at-host elmo-passwd-alist))
    (if data
	(elmo-base64-decode-string (cdr data))
      (setq pass (elmo-read-passwd (format "Password for %s: " 
					   user-at-host) t))
      (setq elmo-passwd-alist
	    (append elmo-passwd-alist
		    (list (cons user-at-host 
				(elmo-base64-encode-string pass)))))
      (if elmo-passwd-life-time
	  (run-with-timer elmo-passwd-life-time nil
			  `(lambda () (elmo-remove-passwd ,user-at-host))))
      pass)))

(defun elmo-remove-passwd (user-at-host)
  "Remove password from password pool (for failure)."
  (let (pass-cons)
    (if (setq pass-cons (assoc user-at-host elmo-passwd-alist))
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

;; from subr.el
(defun elmo-replace-in-string (str regexp newtext &optional literal)
  "Replaces all matches in STR for REGEXP with NEWTEXT string,
 and returns the new string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\."
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	      rtn-str
	      (substring str prev-start match)
	      (cond (literal newtext)
		    (t (mapconcat
			(function
			 (lambda (c)
			   (if special
			       (progn
				 (setq special nil)
				 (cond ((eq c ?\\) "\\")
				       ((eq c ?&)
					(elmo-match-string 0 str))
				       ((and (>= c ?0) (<= c ?9))
					(if (> c (+ ?0 (length
							(match-data))))
					; Invalid match num
					    (error "Invalid match num: %c" c)
					  (setq c (- c ?0))
					  (elmo-match-string c str)))
				       (t (char-to-string c))))
			     (if (eq c ?\\) (progn (setq special t) nil)
			       (char-to-string c)))))
			newtext ""))))))
    (concat rtn-str (substring str start))))

(defun elmo-string-to-list (string)
  (elmo-set-work-buf
   (insert string)
   (goto-char (point-min))
   (insert "(")
   (goto-char (point-max))
   (insert ")")
   (goto-char (point-min))
   (read (current-buffer))))

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

(defun elmo-plugged-p (&optional server port alist label-exp)
  (let ((alist (or alist elmo-plugged-alist))
	plugged-info)
    (cond ((and (not port) (not server))
	   (cond ((eq elmo-plugged-condition 'one)
		  (catch 'plugged
		    (while alist
		      (if (nth 2 (car alist))
			  (throw 'plugged t))
		      (setq alist (cdr alist)))))
		 ((eq elmo-plugged-condition 'all)
		  (catch 'plugged
		    (while alist
		      (if (not (nth 2 (car alist)))
			  (throw 'plugged nil))
		      (setq alist (cdr alist)))
		    t))
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
	   (setq plugged-info (assoc (cons server port) alist))
	   (if (not plugged-info)
	       ;; add elmo-plugged-alist automatically
	       (progn
		 (elmo-set-plugged elmo-plugged server port nil nil label-exp)
		 elmo-plugged)
	     (if (and elmo-auto-change-plugged
		      (> elmo-auto-change-plugged 0)
		      (nth 3 plugged-info)  ;; time
		      (elmo-time-expire (nth 3 plugged-info)
					elmo-auto-change-plugged))
		 t
	       (nth 2 plugged-info)))))))

(defun elmo-set-plugged (plugged &optional server port time
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
	   (setq plugged-info (assoc (cons server port) alist))
	   (setq label (if label-exp
			   (eval label-exp)
			 (nth 1 plugged-info)))
	   (if plugged-info
	       ;; if add is non-nil, don't reset plug state.
	       (unless add
		 (setcdr plugged-info (list label plugged time)))
	     (setq alist
		   (setq elmo-plugged-alist
			 (nconc elmo-plugged-alist
				(list
				 (list (cons server port) label plugged time))))))))
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
		 (delete (assoc (cons server port) alist)) alist)))
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
  "Returns last accessed time."
  (let ((last-accessed (nth 4 (file-attributes (or (and dir
							(expand-file-name
							 path dir))
						   path)))))
    (if last-accessed
	(setq last-accessed (+ (* (nth 0 last-accessed)
				  (float 65536)) (nth 1 last-accessed)))
      0)))

(defun elmo-get-last-modification-time (path &optional dir)
  "Returns last accessed time."
  (let ((last-modified (nth 5 (file-attributes (or (and dir
							(expand-file-name
							 path dir))
						   path)))))
    (setq last-modified (+ (* (nth 0 last-modified)
			      (float 65536)) (nth 1 last-modified)))))

(defun elmo-make-directory (path)
  "create directory recursively."
  (let ((parent (directory-file-name (file-name-directory path))))
    (if (null (file-directory-p parent))
	(elmo-make-directory parent))
    (make-directory path)
    (if (string= path (expand-file-name elmo-msgdb-dir))
	(set-file-modes path 448) ; 700
      )))

(defun elmo-delete-directory (path &optional no-hierarchy)
  "delete directory recursively."
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
      (delete-directory path))))

(defun elmo-list-filter (l1 l2)
  "L1 is filter."
  (if (eq l1 t)
      ;; t means filter all.
      nil
    (if l1
	(elmo-delete-if (lambda (x) (not (memq x l1))) l2)
      ;; filter is nil
      l2)))

(defun elmo-folder-local-p (folder)
  "Return whether FOLDER is a local folder or not."
  (let ((type (elmo-folder-get-type folder)))
    (memq type '(localdir localnews archive maildir internal cache))))

(defun elmo-folder-writable-p (folder)
  (let ((type (elmo-folder-get-type folder)))
    (memq type '(imap4 localdir archive))))

(defun elmo-multi-get-intlist-list (numlist &optional as-is)
  (let ((numbers (sort numlist '<))
	(cur-number 0)
	one-list int-list-list)
    (while numbers
      (setq cur-number (+ cur-number 1))
      (setq one-list nil)
      (while (and numbers 
		  (eq 0
		      (/ (- (car numbers)
			    (* elmo-multi-divide-number cur-number))
			 elmo-multi-divide-number)))
	(setq one-list (nconc
			one-list 
			(list 
			 (if as-is
			     (car numbers)
			   (% (car numbers)
			      (* elmo-multi-divide-number cur-number))))))
	(setq numbers (cdr numbers)))
      (setq int-list-list (nconc int-list-list (list one-list))))
    int-list-list))

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
  "Returns a list (- +). + is bigger than max of LIST1, in LIST2"
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

(defun elmo-multi-list-bigger-diff (list1 list2 &optional mes)
  (let ((list1-list (elmo-multi-get-intlist-list list1 t))
	(list2-list (elmo-multi-get-intlist-list list2 t))
	result
	dels news)
    (while (or list1-list list2-list)
      (setq result (elmo-list-bigger-diff (car list1-list) (car list2-list) 
					  mes))
      (setq dels (append dels (car result)))
      (setq news (append news (cadr result)))
      (setq list1-list (cdr list1-list))
      (setq list2-list (cdr list2-list)))
    (cons dels (list news))))

(defvar elmo-imap4-name-space-regexp-list nil)
(defun elmo-imap4-identical-name-space-p (fld1 fld2)
  ;; only on UW?
  (if (or (eq (string-to-char fld1) ?#)
	  (eq (string-to-char fld2) ?#))
      (string= (car (split-string fld1 "/"))
	       (car (split-string fld2 "/")))
    t))

(defun elmo-folder-identical-system-p (folder1 folder2)
  "folder1 and folder2 should be real folder (not virtual)."
  (cond ((eq (elmo-folder-get-type folder1) 'imap4)
	 (let ((spec1 (elmo-folder-get-spec folder1))
	       (spec2 (elmo-folder-get-spec folder2)))
	   (and (elmo-imap4-identical-name-space-p
		 (nth 1 spec1) (nth 1 spec2))
		(string= (elmo-imap4-spec-hostname spec1)
			 (elmo-imap4-spec-hostname spec2))    ; hostname
		(string= (elmo-imap4-spec-username spec1)
			 (elmo-imap4-spec-username spec2))))) ; username
	(t
	 (elmo-folder-direct-copy-p folder1 folder2))))

(defconst elmo-folder-direct-copy-alist
  '((localdir  . (localdir localnews archive))
    (maildir   . (maildir  localdir localnews archive))
    (localnews . (localdir localnews archive))
    (archive   . (localdir localnews archive))
    (cache     . (localdir localnews archive))))

(defun elmo-folder-direct-copy-p (src-folder dst-folder)
  (let ((src-type (car (elmo-folder-get-spec src-folder)))
	(dst-type (car (elmo-folder-get-spec dst-folder)))
	dst-copy-type)
    (and (setq dst-copy-type
	       (cdr (assq src-type elmo-folder-direct-copy-alist)))
	 (memq dst-type dst-copy-type))))

(defmacro elmo-filter-type (filter)
  (` (aref (, filter) 0)))

(defmacro elmo-filter-key (filter)
  (` (aref (, filter) 1)))

(defmacro elmo-filter-value (filter)
  (` (aref (, filter) 2)))

(defsubst elmo-buffer-field-condition-match (condition)
  (let (term)
    (catch 'done
      (while condition
	(goto-char (point-min))
	(setq term (car condition))
	(cond 
	 ((and (eq (elmo-filter-type term) 'date)
	       (string= (elmo-filter-key term) "since"))
	  (let ((date (elmo-date-get-datevec (elmo-filter-value term))))
	    (if (string<
		 (timezone-make-sortable-date (aref date 0) 
					      (aref date 1)
					      (aref date 2)
					      (timezone-make-time-string
					       (aref date 3) 
					       (aref date 4) 
					       (aref date 5)))
		 (timezone-make-date-sortable (std11-field-body "date")))
		(throw 'done t))))
	 ((and (eq (elmo-filter-type term) 'date)
	       (string= (elmo-filter-key term) "before"))
	  (let ((date (elmo-date-get-datevec (elmo-filter-value term))))
	    (if (string<
		 (timezone-make-date-sortable (std11-field-body "date"))
		 (timezone-make-sortable-date (aref date 0) 
					      (aref date 1)
					      (aref date 2)
					      (timezone-make-time-string
					       (aref date 3) 
					       (aref date 4) 
					       (aref date 5))))
		(throw 'done t))))
	 ((eq (elmo-filter-type term) 'match)
	  (if (string= "body" (elmo-filter-key term))
	      (progn
		(re-search-forward "^$" nil t)	   ; goto body
		(if (search-forward (elmo-filter-value term) nil t)
		    (throw 'done t)))
	    (let ((fval (eword-decode-string
			 (or (std11-field-body (elmo-filter-key term)) ""))))
	      (if (and fval (string-match (elmo-filter-value term)
					  fval))
		  (throw 'done t)))))
	 ((eq (elmo-filter-type term) 'unmatch)
	  (if (string= "body" (elmo-filter-key term))
	      (progn
		(re-search-forward "^$" nil t)	   ; goto body
		(if (not (search-forward (elmo-filter-value term) nil t))
		    (throw 'done t)))
	    (let ((fval (eword-decode-string
			 (or (std11-field-body (elmo-filter-key term)) ""))))
	      (if fval
		  (if (not (string-match (elmo-filter-value term)
					 fval))
		      (throw 'done t))
		(throw 'done t)))))) ; OK?
	(setq condition (cdr condition)))
      (throw 'done nil))))

(defsubst elmo-file-field-condition-match (file condition)
  (elmo-set-work-buf
   (as-binary-input-file
    (insert-file-contents file))
   (elmo-set-buffer-multibyte default-enable-multibyte-characters)
   (decode-mime-charset-region (point-min)(point-max) elmo-mime-charset)
   (elmo-buffer-field-condition-match condition)))

(defun elmo-cross-device-link-error-p (err)
  (let ((errobj err)
	cur)
    (catch 'done
      (while errobj
	(if (and (stringp (setq cur (car errobj)))
		 (or (string-match "cross-device" cur)
		     (string-match "operation not supported" cur)))
	    (throw 'done t))
	(setq errobj (cdr errobj)))
      nil)))

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

;; Make a hash table (default and minimum size is 1024).
(defun elmo-make-hash (&optional hashsize)
  (make-vector
   (if hashsize (max (min (elmo-create-hash-size hashsize)
			  elmo-hash-maximum-size) 1024) 1024) 0))

(defsubst elmo-mime-string (string)
  "Normalize MIME encoded string."
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

(defvar elmo-msgid-replace-chars nil)

(defsubst elmo-replace-msgid-as-filename (msgid)
  "Replace message-id string as filename." 
  (setq msgid (elmo-replace-in-string msgid " " "  "))
  (if (null elmo-msgid-replace-chars)
      (setq elmo-msgid-replace-chars 
	    (regexp-quote (mapconcat 
			   'car elmo-msgid-replace-string-alist ""))))
  (while (string-match (concat "[" elmo-msgid-replace-chars "]")
		       msgid)
    (setq msgid (concat 
		 (substring msgid 0 (match-beginning 0))
		 (cdr (assoc 
		       (substring msgid 
				  (match-beginning 0) (match-end 0))
		       elmo-msgid-replace-string-alist))
		 (substring msgid (match-end 0)))))
  msgid)

(defsubst elmo-recover-msgid-from-filename (filename)
  "Recover Message-ID from filename."
  (let (tmp result)
    (while (string-match " " filename)
      (setq tmp (substring filename 
			   (match-beginning 0)
			   (+ (match-end 0) 1)))
      (if (string= tmp "  ")
	  (setq tmp " ")
	(setq tmp (car (rassoc tmp 
			       elmo-msgid-replace-string-alist))))
      (setq result
	    (concat result 
		    (substring filename 0 (match-beginning 0))
		    tmp))
      (setq filename (substring filename (+ (match-end 0) 1))))
    (concat result filename)))

(defsubst elmo-copy-file (src dst)
  (condition-case err
      (elmo-add-name-to-file src dst t)
    (error (if (elmo-cross-device-link-error-p err)
	       (copy-file src dst t)
	     (error "copy file failed")))))

(defmacro elmo-buffer-exists-p (buffer)
  (` (when (, buffer)
       (funcall (if (stringp (, buffer)) 'get-buffer 'buffer-name)
		(, buffer)))))

(defmacro elmo-kill-buffer (buffer)
  (` (when (elmo-buffer-exists-p (, buffer))
       (kill-buffer (, buffer)))))

(defun elmo-delete-lists (keys list)
  "Delete all entries in LIST that equal to KEYS."
  (while keys
    (setq list (delete (car keys) list))
    (setq keys (cdr keys)))
  list)

(defun elmo-delete-if (pred lst)
  "Returns new list contains items which don't satisfy PRED in LST."
  (let (result)
    (while lst
      (unless (funcall pred (car lst))
	(setq result (nconc result (list (car lst)))))
      (setq lst (cdr lst)))
    result))

(defun elmo-list-delete (list1 list2)
  "Any element of list1 is deleted from list2."
  (while list1
    (setq list2 (delete (car list1) list2))
    (setq list1 (cdr list1)))  
  list2)

(defun elmo-list-member (list1 list2)
  "If any element of list1 is member of list2, returns t."
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
    "a tiny function to display error-object to the stream."
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

(if (fboundp 'lprogress-display)
    (defalias 'elmo-display-progress 'lprogress-display)
  (defun elmo-display-progress (label format &optional value &rest args)
    "Print a progress message."
    (if (and (null format) (null args))
	(message nil)
      (apply (function message) (concat format " %d%%")
	     (nconc args (list value))))))

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

(defun elmo-open-network-stream (name buffer host service ssl)
  (let ((auto-plugged (and elmo-auto-change-plugged
			   (> elmo-auto-change-plugged 0)))
	process)
    (if (eq ssl 'starttls)
	(require 'starttls)
      (if ssl (require 'ssl)))
    (condition-case err
 	(let (process-connection-type)
	  (setq process
		(if (eq ssl 'starttls)
		    (starttls-open-stream name buffer host service)
		  (if ssl
		      (open-ssl-stream name buffer host service)
		    (open-network-stream name buffer host service)))))
      (error
       (when auto-plugged
	 (elmo-set-plugged nil host service (current-time))
	 (message "Auto plugged off at %s:%d" host service)
	 (sit-for 1))
       (signal (car err) (cdr err))))
    (when process
      (process-kill-without-query process)
      (when auto-plugged
	(elmo-set-plugged t host service))
      process)))

(if (fboundp 'std11-fetch-field)
    (defalias 'elmo-field-body 'std11-fetch-field) ;;no narrow-to-region
  (defalias 'elmo-field-body 'std11-field-body))

(defmacro elmo-string (string)
  "String without text property"
  (` (let ((obj (copy-sequence (, string))))
       (set-text-properties 0 (length obj) nil obj)
       obj)))

(defun elmo-y-or-n-p (prompt &optional auto default)
  "Same as `y-or-n-p'.
But if optional argument AUTO is non-nil, DEFAULT is returned."
  (if auto
      default
    (y-or-n-p prompt)))

(defun elmo-string-member (string slist)
  "Returns t if STRING is a member of the SLIST."
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

(provide 'elmo-util)

;;; elmo-util.el ends here
