;;; sb-wired.el --- shimbun backend for Wired Japan

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
(require 'sb-lump)

(luna-define-class shimbun-wired (shimbun-lump) ())

(defvar shimbun-wired-url "http://www.hotwired.co.jp/")
(defvar shimbun-wired-groups '("business" "culture" "technology"))
(defvar shimbun-wired-coding-system (static-if (boundp 'MULE)
					'*euc-japan* 'euc-jp))
(defvar shimbun-wired-from-address "webmaster@www.hotwired.co.jp")
(defvar shimbun-wired-content-start
  "<FONT color=\"#ff0000\" size=\"-1\">.*</FONT>\n")
(defvar shimbun-wired-content-end "<DIV ALIGN=\"RIGHT\">\\[")

(luna-define-method shimbun-get-group-header-alist ((shimbun shimbun-wired))
  (let ((group-header-alist (mapcar (lambda (g) (cons g nil))
				    (shimbun-groups-internal shimbun)))
	(case-fold-search t)
	(regexp (format
		 "<a href=\"\\(%s\\|/\\)\\(news/news/\\(%s\\)/story/\\(\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[0-9]+\\)\\.html\\)[^>]*\">"
		 (regexp-quote (shimbun-url-internal shimbun))
		 (shimbun-regexp-opt (shimbun-groups-internal shimbun)))))
      (dolist (xover (list (concat (shimbun-url-internal shimbun)
				   "news/news/index.html")
			   (concat (shimbun-url-internal shimbun)
				   "news/news/last_seven.html")))
	(erase-buffer)
	(shimbun-retrieve-url shimbun xover t)
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (let* ((url   (concat (shimbun-url-internal shimbun)
				(match-string 2)))
		 (group (downcase (match-string 3)))
		 (id    (format "<%s%%%s>" (match-string 4) group))
		 (date  (shimbun-make-date-string
			 (string-to-number (match-string 5))
			 (string-to-number (match-string 6))
			 (string-to-number (match-string 7))))
		 (header (shimbun-make-header
			  0
			  (shimbun-mime-encode-string
			   (mapconcat 'identity
				      (split-string
				       (buffer-substring
					(match-end 0)
					(progn (search-forward "</b>" nil t) (point)))
				       "<[^>]+>")
				      ""))
			  (shimbun-from-address-internal shimbun)
			  date id "" 0 0 url))
		 (x (assoc group group-header-alist)))
	    (setcdr x (cons header (cdr x))))))
      group-header-alist))

(provide 'sb-wired)

;;; sb-wired.el ends here
