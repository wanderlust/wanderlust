;;; sb-zdnet.el --- shimbun backend for Zdnet Japan

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
(luna-define-class shimbun-zdnet (shimbun) ())

(defvar shimbun-zdnet-url "http://www.zdnet.co.jp/news/")
(defvar shimbun-zdnet-groups '("comp"))
(defvar shimbun-zdnet-coding-system (static-if (boundp 'MULE)
					'*sjis* 'shift_jis))
(defvar shimbun-zdnet-from-address "zdnn@softbank.co.jp")
(defvar shimbun-zdnet-content-start "\\(<!--BODY-->\\|<!--DATE-->\\)")
(defvar shimbun-zdnet-content-end "\\(<!--BODYEND-->\\|<!--BYLINEEND-->\\)")

(luna-define-method shimbun-get-headers ((shimbun shimbun-zdnet))
  (let ((case-fold-search t) headers)
    (goto-char (point-min))
    (let (start)
      (while (and (search-forward "<!--" nil t)
		  (setq start (- (point) 4))
		  (search-forward "-->" nil t))
	(delete-region start (point))))
    (goto-char (point-min))
    (while (re-search-forward
	    "<a href=\"\\(/news/\\)?\\(\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([^\\.]+\\).html\\)\"><font size=\"4\"><strong>"
	    nil t)
      (let ((year  (+ 2000 (string-to-number (match-string 3))))
	    (month (string-to-number (match-string 4)))
	    (day   (string-to-number (match-string 5)))
	    (id    (format "<%s%s%s%s%%%s>"
			   (match-string 3)
			   (match-string 4)
			   (match-string 5)
			   (match-string 6)
			   (shimbun-current-group-internal shimbun)))
	    (url (match-string 2)))
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string
		(mapconcat 'identity
			   (split-string
			    (buffer-substring
			     (match-end 0)
			     (progn (search-forward "</a>" nil t) (point)))
			    "<[^>]+>")
			   ""))
	       (shimbun-from-address-internal shimbun)
	       (shimbun-make-date-string year month day)
	       id  "" 0 0 (concat (shimbun-url-internal shimbun) url))
	      headers)))
    (nreverse headers)))

(provide 'sb-zdnet)

;;; sb-zdnet.el ends here
