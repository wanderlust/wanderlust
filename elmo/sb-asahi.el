;;; sb-asahi.el --- shimbun backend for asahi.com

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
(luna-define-class shimbun-asahi (shimbun shimbun-text) ())

(defvar shimbun-asahi-url "http://spin.asahi.com/")
(defvar shimbun-asahi-groups '("national" "business" "politics"
			       "international" "sports"))
(defvar shimbun-asahi-coding-system (static-if (boundp 'MULE) '*sjis*
				      'shift_jis))
(defvar shimbun-asahi-from-address "webmaster@www.asahi.com")

(defvar shimbun-asahi-content-start "\n<!-- Start of kiji -->\n")
(defvar shimbun-asahi-content-end "\n<!-- End of kiji -->\n")

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi))
  (format "%s%s/update/list.html"
	  (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi))
  (when (search-forward "\n<!-- Start of past -->\n" nil t)
    (delete-region (point-min) (point))
    (when (search-forward "\n<!-- End of past -->\n" nil t)
      (forward-line -1)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (let ((case-fold-search t)
	    headers)
	(while (re-search-forward
		"<a href=\"\\(\\([0-9][0-9][0-9][0-9]\\)/\\([0-9]+\\)\\.html\\)\"> *"
		nil t)
	  (let ((id (format "<%s%s%%%s>"
			    (match-string 2)
			    (match-string 3)
			    (shimbun-current-group-internal shimbun)))
		(url (match-string 1)))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string
		    (mapconcat 'identity
			       (split-string
				(buffer-substring
				 (match-end 0)
				 (progn (search-forward "<br>" nil t) (point)))
				"\\(<[^>]+>\\|\r\\)")
			       ""))
		   (shimbun-from-address-internal shimbun)
		   "" id "" 0 0 (format "%s%s/update/%s"
					(shimbun-url-internal shimbun)
					(shimbun-current-group-internal
					 shimbun)
					url))
		  headers)))
	(setq headers (nreverse headers))
	(let ((i 0))
	  (while (and (nth i headers)
		      (re-search-forward
		       "^(\\([0-9][0-9]\\)/\\([0-9][0-9]\\) \\([0-9][0-9]:[0-9][0-9]\\))"
		       nil t))
	    (let ((month (string-to-number (match-string 1)))
		  (date (decode-time (current-time))))
	      (shimbun-header-set-date
	       (nth i headers)
	       (shimbun-make-date-string
		(if (and (eq 12 month) (eq 1 (nth 4 date)))
		    (1- (nth 5 date))
		  (nth 5 date))
		month
		(string-to-number (match-string 2))
		(match-string 3))))
	    (setq i (1+ i))))
	(nreverse headers)))))

(provide 'sb-asahi)

;;; sb-asahi.el ends here
