;;; sb-sponichi.el --- shimbun backend for www.sponichi.co.jp

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

(luna-define-class shimbun-sponichi (shimbun shimbun-text) ())

(defvar shimbun-sponichi-url "http://www.sponichi.co.jp/")
(defvar shimbun-sponichi-groups '("baseball" "soccer" "usa" "others"
				  "society" "entertainment" "horseracing"))
(defvar shimbun-sponichi-coding-system (static-if (boundp 'MULE)
					   '*sjis* 'shift_jis))
(defvar shimbun-sponichi-from-address "webmaster@www.sponichi.co.jp")
(defvar shimbun-sponichi-content-start "\n<span class=\"text\">　")
(defvar shimbun-sponichi-content-end "\n")

(luna-define-method shimbun-index-url ((shimbun shimbun-sponichi))
  (format "%s%s/index.html"
	  (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-sponichi))
  (when (search-forward "ニュースインデックス" nil t)
    (delete-region (point-min) (point))
    (when (search-forward "アドタグ" nil t)
      (forward-line 2)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (let ((case-fold-search t)
	    headers)
	(while (re-search-forward
		"^<a href=\"/\\(\\([A-z]*\\)/kiji/\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)\\.html\\)\">"
		nil t)
	  (let ((url (match-string 1))
		(id (format "<%s%s%s%s%%%s>"
			    (match-string 3)
			    (match-string 4)
			    (match-string 5)
			    (match-string 6)
			    (shimbun-current-group-internal shimbun)))
		(date (shimbun-make-date-string
		       (string-to-number (match-string 3))
		       (string-to-number (match-string 4))
		       (string-to-number (match-string 5)))))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string
		    (mapconcat 'identity
			       (split-string
				(buffer-substring
				 (match-end 0)
				 (progn (search-forward "<br>" nil t) (point)))
				"<[^>]+>")
			       ""))
		   (shimbun-from-address-internal shimbun)
		   date id "" 0 0 (concat (shimbun-url-internal shimbun)
					  url))
		  headers)))
	headers))))

(provide 'sb-sponichi)

;;; sb-sponichi.el ends here
