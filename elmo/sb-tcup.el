;;; sb-tcup.el --- shimbun backend for www.tcup.com.

;; Author: Yuuichi Teranishi <teranisi@gohome.org>

;; Keywords: news

;;; Copyright:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Original was http://homepage2.nifty.com/strlcat/nnshimbun-tcup.el

;;; Code:

(require 'shimbun)

(eval-and-compile
  (luna-define-class shimbun-tcup (shimbun) (content-hash))
  (luna-define-internal-accessors 'shimbun-tcup))

(defconst shimbun-tcup-group-alist
  '(("yutopia" "http://www66.tcup.com/6629/yutopia.html")))

(defvar shimbun-tcup-url "http://www.tcup.com/")
(defvar shimbun-tcup-groups (mapcar 'car shimbun-tcup-group-alist))
(defvar shimbun-tcup-coding-system (static-if (boundp 'MULE) 
				       '*sjis* 'shift_jis))
(defvar shimbun-tcup-content-hash-length 31)

(luna-define-method initialize-instance :after ((shimbun shimbun-tcup)
						&rest init-args)
  (shimbun-tcup-set-content-hash-internal
   shimbun
   (make-vector shimbun-tcup-content-hash-length 0))
  shimbun)

(luna-define-method shimbun-index-url ((shimbun shimbun-tcup))
  (cadr (assoc (shimbun-current-group-internal shimbun)
	       shimbun-tcup-group-alist)))

(defun shimbun-tcup-get-group-key (group)
  (let ((url (cadr (assoc group 
			  shimbun-tcup-group-alist)))
	(n 3)
	keys)
    (string-match "www\\([0-9]+\\)[^/]+/\\([0-9]+\\)/\\(.+\\)\\.html" url)
    (while (> n 0)
      (push (substring url (match-beginning n) (match-end n)) keys)
      (setq n (1- n)))
    keys))

(defun shimbun-tcup-stime-to-time (stime)
  (let (a b c)
    (setq a (length stime))
    (setq b (- (string-to-number (substring stime 0 (- a 4))) 9))
    (setq c (+ (string-to-number (substring stime (- a 4) a))
	       (* (% b 4096) 10000)
	       (- 90000 (car (current-time-zone)))))
    (list (+ (* (/ b 4096) 625) (/ c 65536)) (% c 65536))))

(defun shimbun-tcup-make-time ()
  (let (yr mon day hr min sec dow tm)
    (looking-at
     "\\([0-9]+\\)月\\([0-9]+\\)日(\\(.\\))\\([0-9]+\\)時\\([0-9]+\\)分\\([0-9]+\\)秒")
    (setq mon (string-to-number (match-string 1))
	  day (string-to-number (match-string 2))
	  dow (match-string 3)
	  hr  (string-to-number (match-string 4))
	  min (string-to-number (match-string 5))
	  sec (string-to-number (match-string 6)))
    (setq dow (string-match dow "日月火水木金土"))
    (setq yr (nth 5 (decode-time (current-time))))
    (setq tm (encode-time sec min hr day mon yr))
    (while (not (eq dow (nth 6 (decode-time tm))))
      (setq yr (1- yr))
      (setq tm (encode-time sec min hr day mon yr)))
    tm))

(defun shimbun-tcup-make-id (stime group)
  (let ((keys (shimbun-tcup-get-group-key group)))
    (format "<%s.%s.%s@www%s.tcup.com>" 
	    stime (nth 2 keys) (nth 1 keys) (nth 0 keys))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-tcup))
  (let ((case-fold-search t)
	headers from subject date id url stime st body)
    (decode-coding-region (point-min) (point-max)
			  (shimbun-coding-system-internal shimbun))
    (goto-char (point-min))
    (while (re-search-forward "<b>\\([^<]+\\)</b></font>　投稿者：" nil t)
      (setq subject (match-string 1))
      (setq from
	    (cond 
	     ((looking-at "<b><a href=\"mailto:\\([^\"]+\\)\">\\([^<]+\\)<")
	      (concat (match-string 2) " <" (match-string 1) ">"))
	     ((looking-at "<[^>]+><b>\\([^<]+\\)<")
	      (match-string 1))
	     (t "(none)")))
      (re-search-forward "投稿日：" nil t)
      (setq stime
	    (cond 
	     ((looking-at "[^,]+, Time: \\([^ ]+\\) ")
	      (shimbun-tcup-stime-to-time (match-string 1)))
	     ((looking-at "\\([^ ]+\\) <")
	      (shimbun-tcup-make-time))
	     (t (current-time))))
      (setq date (format-time-string "%d %b %Y %T %z" stime))
      (setq stime (format "%05d%05d" (car stime) (cadr stime)))
      (setq id (shimbun-tcup-make-id
		stime
		(shimbun-current-group-internal shimbun)))
      (search-forward "<tt><font size=\"3\">")
      (setq st (match-end 0))
      (re-search-forward "\\(<!-- form[^>]+>\\)?</font></tt><p>")
      (setq body (buffer-substring st (match-beginning 0)))
      (forward-line 1)
      (setq url 
	    (if (looking-at "<a[^>]+>[^<]+</a>") 
		(concat (match-string 0) "\n<p>\n")
	      ""))
      (set (intern stime (shimbun-tcup-content-hash-internal shimbun))
	   (concat body "<p>\n" url))
      (push (shimbun-make-header
	     0
	     (shimbun-mime-encode-string subject)
	     (shimbun-mime-encode-string from)
	     date id "" 0 0 stime)
	    headers))
    headers))

(luna-define-method shimbun-article ((shimbun shimbun-tcup) id
				     &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (let* ((header (shimbun-header shimbun id))
	   (xref (shimbun-header-xref header)))
      (with-current-buffer (or outbuf (current-buffer))
	(insert
	 (with-temp-buffer
	   (let ((sym (intern-soft (shimbun-header-xref header)
				   (shimbun-tcup-content-hash-internal
				    shimbun))))
	     (if (boundp sym)
		 (insert (symbol-value sym)))
	     (goto-char (point-min))
	     (shimbun-header-insert header)
	     (insert "Content-Type: " "text/html"
		     "; charset=ISO-2022-JP\n"
		     "MIME-Version: 1.0\n")
	     (insert "\n")
	     (encode-coding-string
	      (buffer-string)
	      (mime-charset-to-coding-system "ISO-2022-JP")))))))))

(provide 'sb-tcup)

;;; sb-tcup.el ends here
