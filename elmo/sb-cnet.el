;;; sb-cnet.el --- shimbun backend for cnet

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

(luna-define-class shimbun-cnet (shimbun) ())

(defvar shimbun-cnet-url "http://cnet.sphere.ne.jp/")
(defvar shimbun-cnet-groups '("comp"))
(defvar shimbun-cnet-coding-system  (static-if (boundp 'MULE)
					'*sjis* 'shift_jis))
(defvar shimbun-cnet-from-address  "cnet@sphere.ad.jp")
(defvar shimbun-cnet-content-start "\n<!--KIJI-->\n")
(defvar shimbun-cnet-content-end "\n<!--/KIJI-->\n")

(luna-define-method shimbun-index-url ((shimbun shimbun-cnet))
  (format "%s/News/Oneweek/" (shimbun-url-internal shimbun)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-cnet))
  (let ((case-fold-search t) headers)
    (while (search-forward "\n<!--*****見出し*****-->\n" nil t)
      (let ((subject (buffer-substring (point) (point-at-eol)))
	    (point (point)))
	(forward-line -2)
	(when (looking-at "<a href=\"/\\(News/\\([0-9][0-9][0-9][0-9]\\)/Item/\\([0-9][0-9]\\([0-9][0-9]\\)\\([0-9][0-9]\\)-[0-9]+\\).html\\)\">")
	  (let ((url (match-string 1))
		(id  (format "<%s%s%%%s>"
			     (match-string 2)
			     (match-string 3)
			     (shimbun-current-group-internal shimbun)))
		(date (shimbun-make-date-string
		       (string-to-number (match-string 2))
		       (string-to-number (match-string 4))
		       (string-to-number (match-string 5)))))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   (shimbun-from-address-internal shimbun)
		   date id "" 0 0 (concat (shimbun-url-internal shimbun) url))
		  headers)))
	(goto-char point)))
    headers))

(provide 'sb-cnet)

;;; sb-cnet.el ends here
