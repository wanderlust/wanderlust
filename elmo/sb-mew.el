;;; sb-mew.el --- shimbun backend for mew.org

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
(require 'sb-mhonarc)
(luna-define-class shimbun-mew (shimbun-mhonarc) ())

(defconst shimbun-mew-groups
  '(("meadow-develop" "meadow-develop" nil t)
    ("meadow-users-jp" "meadow-users-jp")
    ("mule-win32" "mule-win32")
    ("mew-win32" "mew-win32")
    ("mew-dist" "mew-dist/3300" t)
    ("mgp-users-jp" "mgp-users-jp/A" t t)))

(luna-define-method initialize-instance :after ((shimbun shimbun-mew)
						&rest init-args)
  (shimbun-set-url-internal shimbun "http://www.mew.org/archive/")
  (shimbun-set-groups-internal shimbun
			       (mapcar 'car shimbun-mew-groups))
  (shimbun-set-coding-system-internal shimbun
				      (static-if (boundp 'MULE)
					  '*iso-2022-jp* 'iso-2022-jp))
  shimbun)

(defmacro shimbun-mew-concat-url (shimbun url)
  (` (concat (shimbun-url-internal (, shimbun))
	     (nth 1 (assoc
		     (shimbun-current-group-internal (, shimbun))
		     shimbun-mew-groups))
	     "/"
	     (, url))))

(defmacro shimbun-mew-reverse-order-p (shimbun)
  (` (nth 2 (assoc (shimbun-current-group-internal (, shimbun))
		   shimbun-mew-groups))))

(defmacro shimbun-mew-spew-p (shimbun)
  (` (nth 3 (assoc (shimbun-current-group-internal (, shimbun))
		   shimbun-mew-groups))))

(defsubst shimbun-mew-retrieve-xover (shimbun aux)
  (erase-buffer)
  (shimbun-retrieve-url
   shimbun
   (shimbun-mew-concat-url
    shimbun
    (if (= aux 1) "index.html" (format "mail%d.html" aux)))
   t))

(defconst shimbun-mew-regexp "<A[^>]*HREF=\"\\(msg\\([0-9]+\\).html\\)\">\\([^<]+\\)<")

(defsubst shimbun-mew-extract-header-values (shimbun)
  (let (url id subject)
    (setq url (shimbun-mew-concat-url shimbun (match-string 1))
	  id (format "<%05d%%%s>"
		     (1- (string-to-number (match-string 2)))
		     (shimbun-current-group-internal shimbun))
	  subject (match-string 3))
    (forward-line 1)
    (shimbun-make-header
     0
     (shimbun-mime-encode-string subject)
     (if (looking-at "<EM>\\([^<]+\\)<")
	 (shimbun-mime-encode-string (match-string 1))
       "")
     "" id "" 0 0 url)))

(luna-define-method shimbun-index-url ((shimbun shimbun-mew))
  (shimbun-mew-concat-url shimbun "index.html"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-mew))
  (shimbun-mew-get-headers shimbun))

(defun shimbun-mew-get-headers (shimbun)
  (let ((case-fold-search t)
	headers)
    (goto-char (point-min))
    (when (re-search-forward
	   "<A[^>]*href=\"mail\\([0-9]+\\)\\.html\">\\[?Last Page\\]?</A>"
	   nil t)
      (let ((limit 1));(string-to-number (match-string 1))))
	(catch 'stop
	  (if (shimbun-mew-reverse-order-p shimbun)
	      (let ((aux 1))
		(while (let (id url subject)
			 (while (re-search-forward shimbun-mew-regexp nil t)
			   (push (shimbun-mew-extract-header-values shimbun)
				 headers))
			 (< aux limit))
		  (shimbun-mew-retrieve-xover shimbun (setq aux (1+ aux)))))
	    (while (> limit 0)
	      (shimbun-mew-retrieve-xover shimbun limit)
	      (setq limit (1- limit))
	      (let (id url subject)
		(goto-char (point-max))
		(while (re-search-backward shimbun-mew-regexp nil t)
		  (push (shimbun-mew-extract-header-values shimbun)
			headers)
		  (forward-line -2)))))
	  headers)))))

(provide 'sb-mew)

;;; sb-mew.el ends here
