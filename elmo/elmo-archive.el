;;; elmo-archive.el -- Archive folder of ELMO.

;; Copyright 1998,1999,2000 OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;;                          Yuuichi Teranishi <teranisi@gohome.org>

;; Author:  OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;; Keywords: mail, net news
;; Created: Sep 13, 1998
;; Revised: Dec 15, 1998

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
;; TODO:
;; [ボソ] append-msgs() が欲しい（けど multi-refile 不可）。
;; Info-Zip 専用エージェントを用いた日本語検索（OS/2 専用）。

;;; Code:
;; 

(require 'elmo-msgdb)
(require 'emu)
(require 'std11)
(eval-when-compile (require 'elmo-localdir))

;;; Const
(defconst elmo-archive-version "v0.18 [990729/alpha]")

;;; User vars.
(defvar elmo-archive-lha-dos-compatible
  (memq system-type '(OS/2 emx windows-nt))
  "*If non-nil, regard your LHA as compatible to DOS version.")

(defvar elmo-archive-use-izip-agent (memq system-type '(OS/2 emx))
  "*If non-nil, use the special agent in fetching headers.")

(defvar elmo-archive-folder-path "~/Mail"
  "*Base directory for archive folders.")

(defvar elmo-archive-basename "elmo-archive"
  "*Common basename of archive folder file, w/o suffix.")

(defvar elmo-archive-cmdstr-max-length 8000 ; SASAKI Osamu's suggestion
  "*Command line string limitation under OS/2, exactly 8190 bytes.")

(defvar elmo-archive-fetch-headers-volume 50
  "*Quantity of article headers to fetch per once.")

(defvar elmo-archive-dummy-file ".elmo-archive"
  "*Name of dummy file that will be appended when the folder is null.")

(defvar elmo-archive-check-existance-strict t
  "*Check existance of archive contents if non-nil.")

(defvar elmo-archive-load-hook nil
  "*Hook called after loading elmo-archive.el.")

(defvar elmo-archive-treat-file nil
  "*Treat archive folder as a file if non-nil.")

;;; MMDF parser -- info-zip agent w/ REXX
(defvar elmo-mmdf-delimiter "^\01\01\01\01$"
  "*Regular expression of MMDF delimiter.")

(defvar elmo-unixmail-delimiter "^From \\([^ \t]+\\) \\(.+\\)"
  "*Regular expression of UNIX Mail delimiter.")

(defvar elmo-archive-header-regexp "^[ \t]*[-=][-=][-=][-=]"
  "*Common regexp of the delimiter in listing archive.") ;; marche

(defvar elmo-archive-file-regexp-alist
  (append
   (if elmo-archive-lha-dos-compatible
       '((lha . "^%s\\([0-9]+\\)$"))		; OS/2,DOS w/  "-x"
     '((lha . "^.*[ \t]%s\\([0-9]+\\)$")))
   '((zip . "^.*[ \t]%s\\([0-9]+\\)$")
     (zoo . "^.*[ \t]%s\\([0-9]+\\)$")
     (tar . "^%s\\([0-9]+\\)$") ; ok
     (tgz . "^%s\\([0-9]+\\)$") ; ok
     (rar . "^[ \t]%s\\([0-9]+\\)$"))))

(defvar elmo-archive-suffix-alist
   '((lha . ".lzh")  ; default
;    (lha . ".lzs")
     (zip . ".zip")
     (zoo . ".zoo")
;    (arc . ".arc")
;    (arj . ".arj")
     (rar . ".rar")
     (tar . ".tar")
     (tgz . ".tar.gz")))

;;; lha
(defvar elmo-archive-lha-method-alist
  (if elmo-archive-lha-dos-compatible
      ;; OS/2
      '((cp  . ("lha" "u" "-x"))
	(mv  . ("lha" "m" "-x"))
	(rm  . ("lha" "d"))
	(ls  . ("lha" "l" "-x"))
	(cat . ("lha" "p" "-n"))
	(ext . ("lha" "x")) ; "-x"
	)
    ;; some UN|X
    '((cp  . ("lha" "u"))
      (mv  . ("lha" "m"))
      (rm  . ("lha" "d"))
      (ls  . ("lha" "l"))
      (cat . ("lha" "pq"))
      (ext . ("lha" "x")))))

;;; info-zip/unzip
(defvar elmo-archive-zip-method-alist
  '((cp       . ("zip" "-9q"))
    (cp-pipe  . ("zip" "-9q@"))
    (mv       . ("zip" "-mDq9"))
    (mv-pipe  . ("zip" "-mDq9@"))
    (rm       . ("zip" "-dq"))
    (rm-pipe  . ("zip" "-dq@"))
    (ls       . ("unzip" "-lq"))
    (cat      . ("unzip" "-pq"))
    (ext      . ("unzip"))
    (cat-headers . ("izwlagent" "--cat"))))

;;; zoo
(defvar elmo-archive-zoo-method-alist
  '((cp       . ("zoo" "aq"))
    (cp-pipe  . ("zoo" "aqI"))
    (mv       . ("zoo" "aMq"))
    (mv-pipe  . ("zoo" "aMqI"))
    (rm       . ("zoo" "Dq"))
    (ls       . ("zoo" "l"))  ; normal
    (cat      . ("zoo" "xpq"))
    (ext      . ("zoo" "xq"))))

;;; rar
(defvar elmo-archive-rar-method-alist
  '((cp       . ("rar" "u" "-m5"))
    (mv       . ("rar" "m" "-m5"))
    (rm       . ("rar" "d"))
    (ls       . ("rar" "v"))
    (cat      . ("rar" "p" "-inul"))
    (ext      . ("rar" "x"))))

;;; GNU tar (*.tar)
(defvar elmo-archive-tar-method-alist
  (if elmo-archive-lha-dos-compatible
      '((ls   . ("gtar" "-tf"))
	(cat  . ("gtar" "--posix Oxf"))
	(ext  . ("gtar" "-xf"))
	;;(rm   . ("gtar" "--posix" "--delete" "-f")) ;; well not work
	)
  '((ls    . ("gtar" "-tf"))
    (cat   . ("gtar" "-Oxf"))
    (ext   . ("gtar" "-xf"))
    ;;(rm    . ("gtar" "--delete" "-f")) ;; well not work
    )))

;;; GNU tar (*.tar.gz, *.tar.Z, *.tar.bz2)
(defvar elmo-archive-tgz-method-alist
  '((ls         . ("gtar" "-ztf"))
    (cat        . ("gtar" "-Ozxf"))
    (create     . ("gtar" "-zcf"))
    ;;(rm       . elmo-archive-tgz-rm-func)
    (cp         . elmo-archive-tgz-cp-func)
    (mv         . elmo-archive-tgz-mv-func)
    (ext        . ("gtar" "-zxf"))
    ;; tgz special method
    (decompress . ("gzip" "-d"))
    (compress   . ("gzip"))
    (append     . ("gtar" "-uf"))
    ;;(delete     . ("gtar" "--delete" "-f")) ;; well not work
    ))

(defvar elmo-archive-method-list
  '(elmo-archive-lha-method-alist
    elmo-archive-zip-method-alist
    elmo-archive-zoo-method-alist
;   elmo-archive-tar-method-alist
    elmo-archive-tgz-method-alist
;   elmo-archive-arc-method-alist
;   elmo-archive-arj-method-alist
    elmo-archive-rar-method-alist))

;;; Internal vars.
(defvar elmo-archive-method-alist nil)
(defvar elmo-archive-suffixes nil)


;;; Macro
(defmacro elmo-archive-get-method (type action)
  (` (cdr (assq (, action) (cdr (assq (, type)
				      elmo-archive-method-alist))))))

(defmacro elmo-archive-get-suffix (type)
  (` (cdr (assq (, type)
		elmo-archive-suffix-alist))))

(defmacro elmo-archive-get-regexp (type)
  (` (cdr (assq (, type)
		elmo-archive-file-regexp-alist))))

(defsubst elmo-archive-call-process (prog args &optional output)
  (= (apply 'call-process prog nil output nil args) 0))

(defsubst elmo-archive-call-method (method args &optional output)
  (cond
   ((functionp method)
    (funcall method args output))
   (t
    (elmo-archive-call-process
     (car method) (append (cdr method) args) output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scan Folder

(defsubst elmo-archive-list-folder-subr (spec &optional nonsort)
  "*Returns list of number-file(int, not string) in archive FILE.
TYPE specifies the archiver's symbol."
  (let* ((type (nth 2 spec))
	 (prefix (nth 3 spec))
         (file (elmo-archive-get-archive-name (nth 1 spec) type spec))
	 (method (elmo-archive-get-method type 'ls))
	 (args (list file))
	 (file-regexp (format (elmo-archive-get-regexp type)
			      (elmo-concat-path (regexp-quote prefix) "")))
	 (killed (and elmo-use-killed-list
		      (elmo-msgdb-killed-list-load
		       (elmo-msgdb-expand-path spec))))
	 numbers buf file-list header-end)
    (when (file-exists-p file)
      (save-excursion
	(set-buffer (setq buf (get-buffer-create " *ELMO ARCHIVE ls*")))
	(unless (elmo-archive-call-method method args t)
	  (error "%s exited abnormally!" method))
	(goto-char (point-min))
	(when (re-search-forward elmo-archive-header-regexp nil t)
	  (forward-line 1)
	  (setq header-end (point))
	  (when (re-search-forward elmo-archive-header-regexp nil t)
	      (beginning-of-line)
	      (narrow-to-region header-end (point))
	      (goto-char (point-min))))
	(while (and (re-search-forward file-regexp nil t)
		    (not (eobp)))  ; for GNU tar 981010
	  (setq file-list (nconc file-list (list (string-to-int
						  (match-string 1))))))
	(kill-buffer buf)))
    (if nonsort
	(cons (or (elmo-max-of-list file-list) 0)
	      (if killed
		  (- (length file-list) (length killed))
		(length file-list)))
      (setq numbers (sort file-list '<))
      (elmo-living-messages numbers killed))))

(defun elmo-archive-list-folder (spec)
  (elmo-archive-list-folder-subr spec))

(defun elmo-archive-max-of-folder (spec)
  (elmo-archive-list-folder-subr spec t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Folder related functions

(defsubst elmo-archive-get-archive-directory (name)
  ;; allow fullpath. return format is "/foo/bar/".
  (if (file-name-absolute-p name)
      (if (find-file-name-handler name 'copy-file)
	  name
	(expand-file-name name))
    (expand-file-name name elmo-archive-folder-path)))

(defun elmo-archive-get-archive-name (folder type &optional spec)
  (let ((dir (elmo-archive-get-archive-directory folder))
        (suffix (elmo-archive-get-suffix type))
	filename dbdir)
    (if elmo-archive-treat-file
	(if (string-match (concat (regexp-quote suffix) "$") folder)
	    (expand-file-name
	     folder
	     elmo-archive-folder-path)
	  (expand-file-name
	   (concat folder suffix)
	   elmo-archive-folder-path))
      (if (and (let ((handler (find-file-name-handler dir 'copy-file))) ; dir is local.
		 (or (not handler)
		     (if (featurep 'xemacs)
			 (eq handler 'dired-handler-fn))))
	       (or (not (file-exists-p dir))
		   (file-directory-p dir)))
	  (expand-file-name
	   (concat elmo-archive-basename suffix)
	   dir)
	;; for full-path specification.
	(if (and (find-file-name-handler dir 'copy-file) ; ange-ftp, efs
		 spec)
	    (progn
	      (setq filename (expand-file-name
			      (concat elmo-archive-basename suffix)
			      (setq dbdir (elmo-msgdb-expand-path spec))))
	      (if (file-directory-p dbdir)
		  (); ok.
		(if (file-exists-p dbdir)
		    (error "File %s already exists" dbdir)
		  (elmo-make-directory dbdir)))
	      (if (not (file-exists-p filename))
		  (copy-file
		   (if (file-directory-p dir)
		       (expand-file-name
			(concat elmo-archive-basename suffix)
			dir)
		     dir)
		   filename))
	      filename)
	  dir)))))

(defun elmo-archive-folder-exists-p (spec)
  (file-exists-p
   (elmo-archive-get-archive-name (nth 1 spec) (nth 2 spec) spec)))

(defun elmo-archive-folder-creatable-p (spec)
  t)

(defun elmo-archive-create-folder (spec)
  (let* ((dir (directory-file-name ;; remove tail slash.
	       (elmo-archive-get-archive-directory (nth 1 spec))))
         (type (nth 2 spec))
         (arc (elmo-archive-get-archive-name (nth 1 spec) type)))
    (if elmo-archive-treat-file
	(setq dir (directory-file-name (file-name-directory dir))))
    (cond ((and (file-exists-p dir)
		(not (file-directory-p dir)))
           ;; file exists
           (error "Create folder failed; File \"%s\" exists" dir))
          ((file-directory-p dir)
           (if (file-exists-p arc)
               t  ; return value
	     (elmo-archive-create-file arc type spec)))
          (t
	   (elmo-make-directory dir)
	   (elmo-archive-create-file arc type spec)
	   t))))

(defun elmo-archive-create-file (archive type spec)
  (save-excursion
    (let* ((tmp-dir (directory-file-name
		     (elmo-msgdb-expand-path spec)))
           (dummy elmo-archive-dummy-file)
           (method (or (elmo-archive-get-method type 'create)
		       (elmo-archive-get-method type 'mv)))
	   (args (list archive dummy)))
      (when (null method)
	(ding)
	(error "WARNING: read-only mode: %s (method undefined)" type))
      (cond
       ((file-directory-p tmp-dir)
	()) ;nop
       ((file-exists-p tmp-dir)
	;; file exists
	(error "Create directory failed; File \"%s\" exists" tmp-dir))
       (t
	(elmo-make-directory tmp-dir)))
      (elmo-bind-directory
       tmp-dir
       (write-region (point) (point) dummy nil 'no-msg)
       (prog1
	   (elmo-archive-call-method method args)
	 (if (file-exists-p dummy)
	     (delete-file dummy)))
       ))))

(defun elmo-archive-delete-folder (spec)
  (let* ((arc (elmo-archive-get-archive-name (nth 1 spec) (nth 2 spec))))
    (if (not (file-exists-p arc))
	(error "no such file: %s" arc)
      (delete-file arc)
      t)))

(defun elmo-archive-rename-folder (old-spec new-spec)
  (let* ((old-arc (elmo-archive-get-archive-name
		   (nth 1 old-spec) (nth 2 old-spec)))
	 (new-arc (elmo-archive-get-archive-name
		   (nth 1 new-spec) (nth 2 new-spec))))
    (unless (and (eq (nth 2 old-spec) (nth 2 new-spec))
		 (equal (nth 3 old-spec) (nth 3 new-spec)))
      (error "not same archive type and prefix"))
    (if (not (file-exists-p old-arc))
	(error "no such file: %s" old-arc)
      (if (file-exists-p new-arc)
	  (error "already exists: %s" new-arc)
	(rename-file old-arc new-arc)
	t))))

(defun elmo-archive-list-folders (spec &optional hierarchy)
  (let ((folder (concat "$" (nth 1 spec)))
	(elmo-localdir-folder-path elmo-archive-folder-path))
    (if elmo-archive-treat-file
	(let* ((path (elmo-localdir-get-folder-directory spec))
	       (base-folder (or (nth 1 spec) ""))
	       (suffix (nth 2 spec))
	       (prefix (if (string= (nth 3 spec) "")
			   "" (concat ";" (nth 3 spec))))
	       (dir (if (file-directory-p path)
			path (file-name-directory path)))
	       (name (if (file-directory-p path)
			 "" (file-name-nondirectory path)))
	       (flist (and (file-directory-p dir)
			   (directory-files dir nil name nil)))
	       (regexp (format "^\\(.*\\)\\(%s\\)$"
			       (mapconcat
				'(lambda (x) (regexp-quote (cdr x)))
				elmo-archive-suffix-alist
				"\\|"))))
	  (if (string-match "\\(.*\\)/$" base-folder) ; ends with '/'.
	      (setq base-folder (elmo-match-string 1 base-folder))
	    (unless (file-directory-p path)
	      (setq base-folder (or (file-name-directory base-folder)
				    base-folder))))
	  (delq
	   nil
	   (mapcar
	    '(lambda (x)
	       (when (and (string-match regexp x)
			  (eq suffix
			      (car
			       (rassoc (elmo-match-string 2 x)
				       elmo-archive-suffix-alist))))
		 (format "$%s;%s%s"
			 (elmo-concat-path base-folder (elmo-match-string 1 x))
			 suffix prefix)))
	    flist)))
      (elmo-localdir-list-folders-subr folder hierarchy))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Article file related functions
;;; read(extract) / append(move) / delete(delete) / query(list)

(defun elmo-archive-read-msg (spec number outbuf)
  (save-excursion
    (let* ((type (nth 2 spec))
	   (arc (elmo-archive-get-archive-name (nth 1 spec) type spec))
	   (prefix (nth 3 spec))
	   (method (elmo-archive-get-method type 'cat))
	   (args (list arc (elmo-concat-path
			    prefix (int-to-string number)))))
      (set-buffer outbuf)
      (erase-buffer)
      (when (file-exists-p arc)
	(and
	 (as-binary-process
	  (elmo-archive-call-method method args t))
	 (elmo-delete-cr-get-content-type))))))

(defun elmo-archive-append-msg (spec string &optional msg no-see) ;;; verrrrrry slow!!
  (let* ((type (nth 2 spec))
	 (prefix (nth 3 spec))
	 (arc (elmo-archive-get-archive-name (nth 1 spec) type))
	 (method (elmo-archive-get-method type 'mv))
	 (tmp-buffer (get-buffer-create " *ELMO ARCHIVE mv*"))
	 (next-num (or msg
		       (1+ (if (file-exists-p arc)
			       (car (elmo-archive-max-of-folder spec)) 0))))
	 (tmp-dir (elmo-msgdb-expand-path spec))
	 newfile)
    (when (null method)
      (ding)
      (error "WARNING: read-only mode: %s (method undefined)" type))
    (save-excursion
      (set-buffer tmp-buffer)
      (erase-buffer)
      (let ((tmp-dir (expand-file-name prefix tmp-dir)))
	(when (not (file-directory-p tmp-dir))
	  (elmo-make-directory (directory-file-name tmp-dir))))
      (setq newfile (elmo-concat-path
		     prefix
		     (int-to-string next-num)))
      (unwind-protect
	  (elmo-bind-directory
	   tmp-dir
	   (if (and (or (functionp method) (car method))
		    (file-writable-p newfile))
	       (progn
		 (insert string)
		 (as-binary-output-file
		  (write-region (point-min) (point-max) newfile nil 'no-msg))
		 (elmo-archive-call-method method (list arc newfile)))
	     nil))
	(kill-buffer tmp-buffer)))))

;;; (localdir, maildir, localnews, archive) -> archive
(defun elmo-archive-copy-msgs (dst-spec msgs src-spec
					&optional loc-alist same-number)
  (let* ((dst-type (nth 2 dst-spec))
	 (arc (elmo-archive-get-archive-name (nth 1 dst-spec) dst-type))
	 (prefix (nth 3 dst-spec))
	 (p-method (elmo-archive-get-method dst-type 'mv-pipe))
	 (n-method (elmo-archive-get-method dst-type 'mv))
	 (new (unless same-number
		(1+ (car (elmo-archive-max-of-folder dst-spec)))))
	 (src-dir (elmo-localdir-get-folder-directory src-spec))
	 (tmp-dir
	  (file-name-as-directory (elmo-msgdb-expand-path dst-spec)))
	 (do-link t)
	 src tmp newfile tmp-msgs)
    (when (not (elmo-archive-folder-exists-p dst-spec))
      (elmo-archive-create-folder dst-spec))
    (when (null (or p-method n-method))
      (ding)
      (error "WARNING: read-only mode: %s (method undefined)" dst-type))
    (when (and same-number
	       (not (eq (car src-spec) 'maildir))
	       (string-match (concat prefix "$") src-dir)
	       (or
		(elmo-archive-get-method dst-type 'cp-pipe)
		(elmo-archive-get-method dst-type 'cp)))
      (setq tmp-dir (substring src-dir 0 (match-beginning 0)))
      (setq p-method (elmo-archive-get-method dst-type 'cp-pipe)
	    n-method (elmo-archive-get-method dst-type 'cp))
      (setq tmp-msgs (mapcar '(lambda (x)
				(elmo-concat-path prefix (int-to-string x)))
			     msgs))
      (setq do-link nil))
    (when do-link
      (let ((tmp-dir (expand-file-name prefix tmp-dir)))
	(when (not (file-directory-p tmp-dir))
	  (elmo-make-directory (directory-file-name tmp-dir))))
      (while msgs
	(setq newfile (elmo-concat-path prefix (int-to-string
						(if same-number
						    (car msgs)
						  new))))
	(setq tmp-msgs (nconc tmp-msgs (list newfile)))
	(elmo-copy-file
	 ;; src file
	 (elmo-call-func src-spec "get-msg-filename" (car msgs) loc-alist)
	 ;; tmp file
	 (expand-file-name newfile tmp-dir))
	(setq msgs (cdr msgs))
	(unless same-number (setq new (1+ new)))))
    (save-excursion
      (elmo-bind-directory
       tmp-dir
       (cond
	((functionp n-method)
	 (funcall n-method (cons arc tmp-msgs)))
	(p-method
	 (let ((p-prog (car p-method))
	       (p-prog-arg (cdr p-method)))
	   (elmo-archive-exec-msgs-subr1
	    p-prog (append p-prog-arg (list arc)) tmp-msgs)))
	(t
	 (let ((n-prog (car n-method))
	       (n-prog-arg (cdr n-method)))
	   (elmo-archive-exec-msgs-subr2
	    n-prog (append n-prog-arg (list arc)) tmp-msgs (length arc)))))))))

;;; archive -> (localdir, localnews, archive)
(defun elmo-archive-copy-msgs-froms (dst-spec msgs src-spec
					      &optional loc-alist same-number)
  (let* ((src-type (nth 2 src-spec))
	 (arc (elmo-archive-get-archive-name (nth 1 src-spec) src-type))
	 (prefix (nth 3 src-spec))
	 (p-method (elmo-archive-get-method src-type 'ext-pipe))
	 (n-method (elmo-archive-get-method src-type 'ext))
	 (tmp-dir
	  (file-name-as-directory (elmo-msgdb-expand-path src-spec)))
	 (tmp-msgs (mapcar '(lambda (x) (elmo-concat-path
					 prefix
					 (int-to-string x)))
			   msgs))
	 result)
    (unwind-protect
	(setq result
	      (and
	       ;; extract messages
	       (save-excursion
		 (elmo-bind-directory
		  tmp-dir
		  (cond
		   ((functionp n-method)
		    (funcall n-method (cons arc tmp-msgs)))
		   (p-method
		    (let ((p-prog (car p-method))
			  (p-prog-arg (cdr p-method)))
		      (elmo-archive-exec-msgs-subr1
		       p-prog (append p-prog-arg (list arc)) tmp-msgs)))
		   (t
		    (let ((n-prog (car n-method))
			  (n-prog-arg (cdr n-method)))
		      (elmo-archive-exec-msgs-subr2
		       n-prog (append n-prog-arg (list arc)) tmp-msgs (length arc)))))))
	       ;; call elmo-*-copy-msgs of destination folder
	       (elmo-call-func dst-spec "copy-msgs"
			       msgs src-spec loc-alist same-number)))
      ;; clean up tmp-dir
      (elmo-bind-directory
       tmp-dir
       (while tmp-msgs
	 (if (file-exists-p (car tmp-msgs))
	     (delete-file (car tmp-msgs)))
	 (setq tmp-msgs (cdr tmp-msgs))))
      result)))

(defun elmo-archive-delete-msgs (spec msgs)
  (save-excursion
    (let* ((type (nth 2 spec))
	   (prefix (nth 3 spec))
	   (arc (elmo-archive-get-archive-name (nth 1 spec) type))
	   (p-method (elmo-archive-get-method type 'rm-pipe))
	   (n-method (elmo-archive-get-method type 'rm))
	   (msgs (mapcar '(lambda (x) (elmo-concat-path
				       prefix
				       (int-to-string x)))
			 msgs)))
      (cond ((functionp n-method)
	     (funcall n-method (cons arc msgs)))
            (p-method
	     (let ((p-prog (car p-method))
		   (p-prog-arg (cdr p-method)))
	       (elmo-archive-exec-msgs-subr1
		p-prog (append p-prog-arg (list arc)) msgs)))
            (n-method
	     (let ((n-prog (car n-method))
		   (n-prog-arg (cdr n-method)))
	       (elmo-archive-exec-msgs-subr2
		n-prog (append n-prog-arg (list arc)) msgs (length arc))))
	    (t
	     (ding)
	     (error "WARNING: not delete: %s (method undefined)" type))) )))

(defun elmo-archive-exec-msgs-subr1 (prog args msgs)
  (let ((buf (get-buffer-create " *ELMO ARCHIVE exec*")))
    (set-buffer buf)
    (insert (mapconcat 'concat msgs "\n")) ;string
    (unwind-protect
	(= 0
	   (apply 'call-process-region (point-min) (point-max)
		  prog nil nil nil args))
      (kill-buffer buf))))

(defun elmo-archive-exec-msgs-subr2 (prog args msgs arc-length)
  (let ((max-len (- elmo-archive-cmdstr-max-length arc-length))
	(n (length msgs))
	rest i sum)
    (setq rest msgs) ;string
    (setq i 1)
    (setq sum 0)
    (catch 'done
      (while (and rest (<= i n))
	(mapcar '(lambda (x)
		   (let* ((len (length x))
			  (files (member x (reverse rest))))
		     ;; total(previous) + current + white space
		     (if (<= max-len (+ sum len 1))
			 (progn
			   (unless
			       (elmo-archive-call-process
				prog (append args files))
			     (throw 'done nil))
			   (setq sum 0) ;; reset
			   (setq rest (nthcdr i rest)))
		       (setq sum (+ sum len 1)))
		     (setq i (1+ i)))) msgs))
      (throw 'done
	     (or (not rest)
		 (elmo-archive-call-process prog (append args rest))))
      )))

(defsubst elmo-archive-article-exists-p (arc msg type)
  (if (not elmo-archive-check-existance-strict)
      t  ; nop
    (save-excursion ;; added 980915
      (let* ((method (elmo-archive-get-method type 'ls))
	     (args (list arc msg))
	     (buf (get-buffer-create " *ELMO ARCHIVE query*"))
	     (error-msg "\\(no file\\|0 files\\)")
	     ret-val)
	(set-buffer buf)
	(erase-buffer)
	(elmo-archive-call-method method args t)
	;; pointer: point-max
	(setq ret-val (not (re-search-backward error-msg nil t)))
	(kill-buffer buf)
	ret-val))))

(defun elmo-archive-tgz-common-func (args exec-type &optional copy)
  (let* ((arc (car args))
	 (tmp-msgs (cdr args))
	 (decompress (elmo-archive-get-method 'tgz 'decompress))
	 (compress (elmo-archive-get-method 'tgz 'compress))
	 (exec (elmo-archive-get-method 'tgz exec-type))
	 (suffix (elmo-archive-get-suffix 'tgz))
	 (tar-suffix (elmo-archive-get-suffix 'tar))
	 arc-tar ret-val
	 )
    (when (null (and decompress compress exec))
      (ding)
      (error "WARNING: special method undefined: %s of %s"
	     (or (if (null decompress) 'decompress)
		 (if (null compress) 'compress)
		 (if (null exec) exec-type))
	     'tgz))
    (unless tar-suffix
      (ding)
      (error "WARNING: `tar' suffix undefined"))
    (if (string-match (concat (regexp-quote suffix) "$") arc)
	(setq arc-tar
	      (concat (substring arc 0 (match-beginning 0)) tar-suffix))
      (error "%s: not match suffix [%s]" arc suffix))
    (and
     ;; decompress
     (elmo-archive-call-process
      (car decompress) (append (cdr decompress) (list arc)))
     ;; append (or delete)
     (elmo-archive-exec-msgs-subr2
      (car exec) (append (cdr exec) (list arc-tar)) tmp-msgs (length arc-tar))
     ;; compress
     (setq ret-val
	   (elmo-archive-call-process
	    (car compress) (append (cdr compress) (list arc-tar)))))
    ;; delete tmporary messages
    (if (and (not copy)
	     (eq exec-type 'append))
	(while tmp-msgs
	  (if (file-exists-p (car tmp-msgs))
	      (delete-file (car tmp-msgs)))
	  (setq tmp-msgs (cdr tmp-msgs))))
    ret-val))

(defun elmo-archive-tgz-cp-func (args &optional output)
  (elmo-archive-tgz-common-func args 'append t))

(defun elmo-archive-tgz-mv-func (args &optional output)
  (elmo-archive-tgz-common-func args 'append))

(defun elmo-archive-tgz-rm-func (args &optional output)
  (elmo-archive-tgz-common-func args 'delete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MessageDB functions (from elmo-localdir.el)

(defsubst elmo-archive-msgdb-create-entity-subr (number)
  (let (header-end)
    (elmo-set-buffer-multibyte default-enable-multibyte-characters)
    (goto-char (point-min))
    (if (re-search-forward "\\(^--.*$\\)\\|\\(\n\n\\)" nil t)
	(setq header-end (point))
      (setq header-end (point-max)))
    (narrow-to-region (point-min) header-end)
    (elmo-msgdb-create-overview-from-buffer number)))

(defsubst elmo-archive-msgdb-create-entity (method archive number type &optional prefix) ;; verrrry slow!!
  (let* ((msg (elmo-concat-path prefix (int-to-string number)))
	 (arg-list (list archive msg)))
    (when (elmo-archive-article-exists-p archive msg type)
      ;; insert article.
      (as-binary-process
       (elmo-archive-call-method method arg-list t))
      (elmo-archive-msgdb-create-entity-subr number))))

(defun elmo-archive-msgdb-create-as-numlist (spec numlist new-mark
						  already-mark seen-mark
						  important-mark seen-list)
  (when numlist
    (save-excursion ;; 981005
      (if (and elmo-archive-use-izip-agent
	       (elmo-archive-get-method (nth 2 spec) 'cat-headers))
	  (elmo-archive-msgdb-create-as-numlist-subr2
           spec numlist new-mark already-mark seen-mark important-mark
	   seen-list)
	(elmo-archive-msgdb-create-as-numlist-subr1
         spec numlist new-mark already-mark seen-mark important-mark
	 seen-list)))))

(defalias 'elmo-archive-msgdb-create 'elmo-archive-msgdb-create-as-numlist)


(defun elmo-archive-msgdb-create-as-numlist-subr1 (spec numlist new-mark
							already-mark seen-mark
							important-mark
							seen-list)
  (let* ((type (nth 2 spec))
	 (file (elmo-archive-get-archive-name (nth 1 spec) type spec))
	 (method (elmo-archive-get-method type 'cat))
	 (tmp-buf (get-buffer-create " *ELMO ARCHIVE msgdb*"))
	 overview number-alist mark-alist entity
	 i percent num message-id seen gmark)
    (save-excursion
      (set-buffer tmp-buf)
      (setq num (length numlist))
      (setq i 0)
      (message "Creating msgdb...")
      (while numlist
	(erase-buffer)
	(setq entity
	      (elmo-archive-msgdb-create-entity
	       method file (car numlist) type (nth 3 spec)))
	(when entity
	  (setq overview
		(elmo-msgdb-append-element
		 overview entity))
	  (setq number-alist
		(elmo-msgdb-number-add
		 number-alist
		 (elmo-msgdb-overview-entity-get-number entity)
		 (car entity)))
	  (setq message-id (car entity))
	  (setq seen (member message-id seen-list))
	  (if (setq gmark
		    (or (elmo-msgdb-global-mark-get message-id)
			(if (elmo-cache-exists-p message-id) ; XXX
			    (if seen
				nil
			      already-mark)
			  (if seen
			      seen-mark
			    new-mark))))
	      (setq mark-alist
		    (elmo-msgdb-mark-append
		     mark-alist
		     (elmo-msgdb-overview-entity-get-number entity)
		     gmark))))
	(when (> num elmo-display-progress-threshold)
	  (setq i (1+ i))
	  (setq percent (/ (* i 100) num))
	  (elmo-display-progress
	   'elmo-archive-msgdb-create-as-numlist-subr1 "Creating msgdb..."
	   percent))
	(setq numlist (cdr numlist)))
      (kill-buffer tmp-buf)
      (message "Creating msgdb...done.")
      (list overview number-alist mark-alist)) ))

;;; info-zip agent
(defun elmo-archive-msgdb-create-as-numlist-subr2 (spec numlist new-mark
							already-mark seen-mark
							important-mark
							seen-list)
  (let* ((buf (get-buffer-create " *ELMO ARCHIVE headers*"))
	 (delim1 elmo-mmdf-delimiter)		;; MMDF
	 (delim2 elmo-unixmail-delimiter)	;; UNIX Mail
	 (type (nth 2 spec))
	 (prefix (nth 3 spec))
	 (method (elmo-archive-get-method type 'cat-headers))
	 (prog (car method))
	 (args (cdr method))
	 (arc (elmo-archive-get-archive-name (nth 1 spec) type))
	 n i percent num result overview number-alist mark-alist
	 msgs case-fold-search)
    (set-buffer buf)
    (setq num (length numlist))
    (setq i 0)
    (message "Creating msgdb...")
    (while numlist
      (setq n (min (1- elmo-archive-fetch-headers-volume)
		   (1- (length numlist))))
      (setq msgs (reverse (memq (nth n numlist) (reverse numlist))))
      (setq numlist (nthcdr (1+ n) numlist))
      (erase-buffer)
      (insert
       (mapconcat
	'concat
	(mapcar '(lambda (x) (elmo-concat-path prefix (int-to-string x))) msgs)
	"\n"))
      (message "Fetching headers...")
      (as-binary-process (apply 'call-process-region
				(point-min) (point-max)
				prog t t nil (append args (list arc))))
      (goto-char (point-min))
      (cond
       ((looking-at delim1)	;; MMDF
	(setq result (elmo-archive-parse-mmdf msgs
					      new-mark
					      already-mark seen-mark
					      seen-list))
	(setq overview (append overview (nth 0 result)))
	(setq number-alist (append number-alist (nth 1 result)))
	(setq mark-alist (append mark-alist (nth 2 result))))
;      ((looking-at delim2)	;; UNIX MAIL
;	(setq result (elmo-archive-parse-unixmail msgs))
;	(setq overview (append overview (nth 0 result)))
;	(setq number-alist (append number-alist (nth 1 result)))
;	(setq mark-alist (append mark-alist (nth 2 result))))
       (t			;; unknown format
	(error "unknown format!")))
      (when (> num elmo-display-progress-threshold)
	(setq i (+ n i))
	(setq percent (/ (* i 100) num))
	(elmo-display-progress
	 'elmo-archive-msgdb-create-as-numlist-subr2 "Creating msgdb..."
	 percent)))
    (kill-buffer buf)
    (list overview number-alist mark-alist)) )

(defun elmo-archive-parse-mmdf (msgs new-mark
				     already-mark
				     seen-mark
				     seen-list)
  (let ((delim elmo-mmdf-delimiter)
	number sp ep rest entity overview number-alist mark-alist ret-val
	message-id seen gmark)
    (goto-char (point-min))
    (setq rest msgs)
    (while (and rest (re-search-forward delim nil t)
                (not (eobp)))
      (setq number (car rest))
      (setq sp (1+ (point)))
      (setq ep (prog2 (re-search-forward delim)
		   (1+ (- (point) (length delim)))))
      (if (>= sp ep) ; no article!
	  ()  ; nop
        (save-excursion
          (narrow-to-region sp ep)
          (setq entity (elmo-archive-msgdb-create-entity-subr number))
	  (setq overview
		(elmo-msgdb-append-element
		 overview entity))
	  (setq number-alist
		(elmo-msgdb-number-add
		 number-alist
		 (elmo-msgdb-overview-entity-get-number entity)
		 (car entity)))
	  (setq message-id (car entity))
	  (setq seen (member message-id seen-list))
	  (if (setq gmark
		    (or (elmo-msgdb-global-mark-get message-id)
			(if (elmo-cache-exists-p message-id) ; XXX
			    (if seen
				nil
			      already-mark)
			  (if seen
			      seen-mark
			    new-mark))))
	      (setq mark-alist
		    (elmo-msgdb-mark-append
		     mark-alist
		     (elmo-msgdb-overview-entity-get-number entity)
		     gmark)))
          (setq ret-val (append ret-val (list overview number-alist mark-alist)))
	  (widen)))
      (forward-line 1)
      (setq rest (cdr rest)))
    ret-val))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search functions

(defsubst elmo-archive-field-condition-match (spec number number-list
						   condition prefix)
  (save-excursion
    (let* ((type (nth 2 spec))
	   (arc (elmo-archive-get-archive-name (nth 1 spec) type spec))
	   (method (elmo-archive-get-method type 'cat))
	   (args (list arc (elmo-concat-path prefix (int-to-string number)))))
      (elmo-set-work-buf
       (when (file-exists-p arc)
	 (as-binary-process
	  (elmo-archive-call-method method args t))
	 (elmo-set-buffer-multibyte default-enable-multibyte-characters)
	 (decode-mime-charset-region (point-min)(point-max) elmo-mime-charset)
	 (elmo-buffer-field-condition-match condition number number-list))))))

(defun elmo-archive-search (spec condition &optional from-msgs)
  (let* (;;(args (elmo-string-to-list key))
	 ;; XXX: I don't know whether `elmo-archive-list-folder'
	 ;;      updates match-data.
	 ;; (msgs (or from-msgs (elmo-archive-list-folder spec)))
	 (msgs (or from-msgs (elmo-archive-list-folder spec)))
	 (num (length msgs))
	 (i 0)
	 (case-fold-search nil)
	 number-list ret-val)
    (setq number-list msgs)
    (while msgs
      (if (elmo-archive-field-condition-match spec (car msgs) number-list
					      condition
					      (nth 3 spec))
	  (setq ret-val (cons (car msgs) ret-val)))
      (when (> num elmo-display-progress-threshold)
	(setq i (1+ i))
	(elmo-display-progress
	 'elmo-archive-search "Searching..."
	 (/ (* i 100) num)))
      (setq msgs (cdr msgs)))
    (nreverse ret-val)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc functions

(defun elmo-archive-check-validity (spec validity-file)
  t) ; ok.

(defun elmo-archive-sync-validity (spec validity-file)
  t) ; ok.


;;; method(alist)
(if (null elmo-archive-method-alist)
    (let ((mlist elmo-archive-method-list) ; from mew-highlight.el
	  method type str)
      (while mlist
	(setq method (car mlist))
	(setq mlist (cdr mlist))
	(setq str (symbol-name method))
	(string-match "elmo-archive-\\([^-].*\\)-method-alist$" str)
	(setq type (intern-soft
		    (elmo-match-string 1 str)))
	(setq elmo-archive-method-alist
	      (cons (cons type
			  (symbol-value method))
		    elmo-archive-method-alist)))))

;;; valid suffix(list)
(if (null elmo-archive-suffixes)
    (let ((slist elmo-archive-suffix-alist)
	  tmp)
      (while slist
	(setq tmp (car slist))
	(setq elmo-archive-suffixes
	      (nconc elmo-archive-suffixes (list (cdr tmp))))
	(setq slist (cdr slist)))))

(defun elmo-archive-use-cache-p (spec number)
  elmo-archive-use-cache)

(defun elmo-archive-local-file-p (spec number)
  nil)

(defun elmo-archive-get-msg-filename (spec number &optional loc-alist)
  (let ((tmp-dir (file-name-as-directory (elmo-msgdb-expand-path spec)))
	(prefix (nth 3 spec)))
    (expand-file-name
     (elmo-concat-path prefix (int-to-string number))
     tmp-dir)))

(defalias 'elmo-archive-sync-number-alist
  'elmo-generic-sync-number-alist)
(defalias 'elmo-archive-list-folder-unread
  'elmo-generic-list-folder-unread)
(defalias 'elmo-archive-list-folder-important
  'elmo-generic-list-folder-important)
(defalias 'elmo-archive-commit 'elmo-generic-commit)
(defalias 'elmo-archive-folder-diff 'elmo-generic-folder-diff)

;;; End
(run-hooks 'elmo-archive-load-hook)

(require 'product)
(product-provide (provide 'elmo-archive) (require 'elmo-version))

;;; elmo-archive.el ends here
