;;; sb-yomiuri.el --- shimbun backend for www.yomiuri.co.jp

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;         Akihiro Arisawa    <ari@atesoft.advantest.co.jp>
;;         Yuuichi Teranishi <teranisi@gohome.org>

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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>.

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-yomiuri (shimbun shimbun-text) ())

(defvar shimbun-yomiuri-url "http://www.yomiuri.co.jp/")
(defvar shimbun-yomiuri-groups '("shakai" "sports" "seiji" "keizai"
				 "kokusai" "fuho"))
(defvar shimbun-yomiuri-coding-system  (static-if (boundp 'MULE)
					   '*sjis* 'shift_jis))
(defvar shimbun-yomiuri-from-address  "webmaster@www.yomiuri.co.jp")
(defvar shimbun-yomiuri-content-start "\n<!--  honbun start  -->\n")
(defvar shimbun-yomiuri-content-end  "\n<!--  honbun end  -->\n")

(defvar shimbun-yomiuri-group-path-alist
  '(("shakai" . "04")
    ("sports" . "06")
    ("seiji"  . "01")
    ("keizai" . "02")
    ("kokusai" . "05")
    ("fuho"    . "zz")))

(luna-define-method shimbun-index-url ((shimbun shimbun-yomiuri))
  (concat (shimbun-url-internal shimbun)
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-yomiuri-group-path-alist))
	  "/index.htm"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yomiuri))
  (let ((case-fold-search t)
	start headers)
    (goto-char (point-min))
    (when (and (search-forward
		(format "\n<!-- /news/list start -->\n"
			(shimbun-current-group-internal shimbun)) nil t)
	       (setq start (point))
	       (search-forward
		(format "\n<!-- /news/list end -->\n"
			(shimbun-current-group-internal shimbun)) nil t))
      (forward-line -1)
      (save-restriction
	(narrow-to-region start (point))
	(goto-char start)
	(while (re-search-forward
		"<a href=\"/\\([0-9]+\\)/\\(\\(\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[A-z0-9]+\\)\\.htm\\)\"[^>]*>"
		nil t)
	  (let ((url   (concat (match-string 1) "/"
			       (match-string 2)))
		(id    (format "<%s%s%%%s>"
			       (match-string 1)
			       (match-string 3)
			       (shimbun-current-group-internal shimbun)))
		(year  (string-to-number (match-string 4)))
		(month (string-to-number (match-string 5)))
		(day   (string-to-number (match-string 6)))
		(subject (mapconcat
			  'identity
			  (split-string
			   (buffer-substring
			    (match-end 0)
			    (progn (search-forward "<br>" nil t) (point)))
			   "<[^>]+>")
			  ""))
		date)
	    (when (string-match "^◆" subject)
	      (setq subject (substring subject (match-end 0))))
	    (if (string-match "(\\([0-9][0-9]:[0-9][0-9]\\))$" subject)
		(setq date (shimbun-make-date-string
			    year month day (match-string 1 subject))
		      subject (substring subject 0 (match-beginning 0)))
	      (setq date (shimbun-make-date-string year month day)))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   (shimbun-from-address-internal shimbun)
		   date id "" 0 0 (concat
				   (shimbun-url-internal shimbun)
				   url))
		  headers)))))
    headers))

(provide 'sb-yomiuri)

;;; sb-yomiuri.el ends here
