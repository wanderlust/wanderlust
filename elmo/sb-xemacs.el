;;; sb-xemacs.el --- shimbun backend for xemacs.org

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

(luna-define-class shimbun-xemacs (shimbun-mhonarc) ())

(defvar shimbun-xemacs-url "http://list-archives.xemacs.org/")
(defvar shimbun-xemacs-groups '("xemacs-announce"
				"xemacs-beta-ja" "xemacs-beta"
				"xemacs-build-reports" "xemacs-cvs"
				"xemacs-mule" "xemacs-nt" "xemacs-patches"
				"xemacs-users-ja" "xemacs"))
(defvar shimbun-xemacs-coding-system (static-if (boundp 'MULE)
					 '*euc-japan* 'euc-jp))

(defmacro shimbun-xemacs-concat-url (shimbun url)
  (` (concat (shimbun-url-internal shimbun)
	     (shimbun-current-group-internal shimbun) "/" (, url))))

(luna-define-method shimbun-index-url ((shimbun shimbun-xemacs))
  (shimbun-xemacs-concat-url shimbun nil))

(luna-define-method shimbun-get-headers ((shimbun shimbun-xemacs))
  (let ((case-fold-search t)
	headers auxs aux)
    (catch 'stop
      ;; Only latest month.
      (if (re-search-forward
	   (concat "<A HREF=\"/" (shimbun-current-group-internal shimbun)
		   "/\\([12][0-9][0-9][0-9][0-1][0-9]\\)/\">\\[Index\\]")
	   nil t)
	  (setq auxs (append auxs (list (match-string 1)))))
      (while auxs
	(erase-buffer)
	(shimbun-retrieve-url
	 shimbun
	 (shimbun-xemacs-concat-url shimbun
				    (concat (setq aux (car auxs)) "/")))
	(let ((case-fold-search t)
	      id url subject)
	  (goto-char (point-max))
	  (while (re-search-backward
		  "<A[^>]*HREF=\"\\(msg\\([0-9]+\\).html\\)\">\\([^<]+\\)<"
		  nil t)
	    (setq url (shimbun-xemacs-concat-url
		       shimbun
		       (concat aux "/" (match-string 1)))
		  id (format "<%s%05d%%%s>"
			     aux
			     (string-to-number (match-string 2))
			     (shimbun-current-group-internal shimbun))
		  subject (match-string 3))
	    (forward-line 1)
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   (if (looking-at "<td><em>\\([^<]+\\)<")
		       (match-string 1)
		     "")
		   "" id "" 0 0 url)
		  headers)
	    ;; (message "%s" id)
	    (forward-line -2)))
	(setq auxs (cdr auxs))))
    headers))

(provide 'sb-xemacs)

;;; sb-xemacs.el ends here
