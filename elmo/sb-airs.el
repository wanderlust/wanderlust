;;; sb-airs.el --- shimbun backend for lists.airs.net

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

;; Original was nnshimbun-airs.el on http://homepage2.nifty.com/strlcat/

;;; Code:

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-airs (shimbun-mhonarc) ())

(defconst shimbun-airs-group-path-alist
  '(("semi-gnus-ja" . "semi-gnus/archive")
    ("wl" . "wl/archive")))

(defvar shimbun-airs-url "http://lists.airs.net/")
(defvar shimbun-airs-groups (mapcar 'car shimbun-airs-group-path-alist))
(defvar shimbun-airs-coding-system (static-if (boundp 'MULE)
				       '*euc-japan* 'euc-jp))

(defmacro shimbun-airs-concat-url (shimbun url)
  (` (concat (shimbun-url-internal (, shimbun))
	     (cdr (assoc (shimbun-current-group-internal (, shimbun))
			 shimbun-airs-group-path-alist))
	     "/"
	     (, url))))

(luna-define-method shimbun-index-url ((shimbun shimbun-airs))
  (shimbun-airs-concat-url shimbun "index.html"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-airs))
  (let ((case-fold-search t) headers months)
    (goto-char (point-min))
    ;; Only first month...
    (if (re-search-forward "<A HREF=\"\\([0-9]+\\)/\">" nil t)
	(push (match-string 1) months))
    (setq months (nreverse months))
    (dolist (month months)
      (erase-buffer)
      (shimbun-retrieve-url
       shimbun
       (shimbun-airs-concat-url shimbun (concat month "/index.html"))
       t)
      (let (id url subject)
	(goto-char (point-max))
	(while (re-search-backward
		"<A[^>]*HREF=\"\\(msg\\([0-9]+\\)\\.html\\)\">\\([^<]+\\)</A>"
		nil t)
	  (setq url (shimbun-airs-concat-url
		     shimbun
		     (concat month "/" (match-string 1)))
		id (format "<%s%05d%%%s>"
			   month
			   (string-to-number (match-string 2))
			   (shimbun-current-group-internal shimbun))
		subject (match-string 3))
	  (save-excursion
	    (goto-char (match-end 0))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   (if (looking-at "</STRONG> *<EM>\\([^<]+\\)<")
		       (shimbun-mime-encode-string (match-string 1))
		     "")
		   "" id "" 0 0 url)
		  headers)))))
    headers))

(provide 'sb-airs)

;;; sb-airs.el ends here
