;;; sb-mhonarc.el --- shimbun backend class for mhonarc

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
(luna-define-class shimbun-mhonarc (shimbun) ())

(luna-define-method shimbun-make-contents ((shimbun shimbun-mhonarc)
					   header)
  (if (search-forward "<!--X-Head-End-->" nil t)
      (progn
	(forward-line 0)
	;; Processing headers.
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (shimbun-decode-entities)
	  (goto-char (point-min))
	  (while (search-forward "\n<!--X-" nil t)
	    (replace-match "\n"))
	  (goto-char (point-min))
	  (while (search-forward " -->\n" nil t)
	    (replace-match "\n"))
	  (goto-char (point-min))
	  (while (search-forward "\t" nil t)
	    (replace-match " "))
	  (goto-char (point-min))
	  (let (buf refs)
	    (while (not (eobp))
	      (cond
	       ((looking-at "<!--")
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Subject: +")
		(shimbun-header-set-subject header
					    (shimbun-header-field-value))
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "From: +")
		(shimbun-header-set-from header (shimbun-header-field-value))
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Date: +")
		(shimbun-header-set-date header (shimbun-header-field-value))
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Message-Id: +")
		(shimbun-header-set-id header
		 (concat "<" (shimbun-header-field-value) ">"))
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Reference: +")
		(push (concat "<" (shimbun-header-field-value) ">") refs)
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Content-Type: ")
		(unless (search-forward "charset" (point-at-eol) t)
		  (end-of-line)
		  (insert "; charset=ISO-2022-JP"))
		(forward-line 1))
	       (t (forward-line 1))))
	    (insert "MIME-Version: 1.0\n")
	    (if refs
		(shimbun-header-set-references header
					       (mapconcat 'identity refs " ")))
	    (insert "\n")
	    (goto-char (point-min))
	    (shimbun-header-insert header))
	  (goto-char (point-max)))
	;; Processing body.
	(save-restriction
	  (narrow-to-region (point) (point-max))
	  (delete-region
	   (point)
	   (progn
	     (search-forward "\n<!--X-Body-of-Message-->\n" nil t)
	     (point)))
	  (when (search-forward "\n<!--X-Body-of-Message-End-->\n" nil t)
	    (forward-line -1)
	    (delete-region (point) (point-max)))
	  (shimbun-remove-markup)
	  (shimbun-decode-entities)))
    (goto-char (point-min))
    (shimbun-header-insert header)
    (insert
     "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n\n"))
  (encode-coding-string (buffer-string)
			(mime-charset-to-coding-system "ISO-2022-JP")))

(provide 'sb-mhonarc)

;;; sb-mhonarc.el ends here
