;;; sb-lump.el --- shimbun backend class to check all groups at once

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
(defvar shimbun-lump-check-interval 300)

(eval-and-compile
  (luna-define-class shimbun-lump (shimbun) (group-header-alist last-check))
  (luna-define-internal-accessors 'shimbun-lump))

(defun shimbun-lump-lapse-seconds (time)
  (let ((now (current-time)))
    (+ (* (- (car now) (car time)) 65536)
       (- (nth 1 now) (nth 1 time)))))

(defun shimbun-lump-check-p (shimbun)
  (or (null (shimbun-lump-last-check-internal shimbun))
      (and (shimbun-lump-last-check-internal shimbun)
	   (< (shimbun-lump-lapse-seconds
	       (shimbun-lump-last-check-internal shimbun))
	      shimbun-lump-check-interval))))

(defun shimbun-lump-checked (shimbun)
  (shimbun-lump-set-last-check-internal shimbun (current-time)))

(luna-define-generic shimbun-get-group-header-alist (shimbun)
  "Return an alist of group and header list.")

(luna-define-method shimbun-get-headers ((shimbun shimbun-lump))
  (when (shimbun-lump-check-p shimbun)
    (shimbun-lump-set-group-header-alist-internal
     shimbun (shimbun-get-group-header-alist shimbun))
    (shimbun-lump-checked shimbun))
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      (shimbun-lump-group-header-alist-internal shimbun))))

(luna-define-method shimbun-close :after ((shimbun shimbun-lump))
  (shimbun-lump-set-group-header-alist-internal shimbun nil)
  (shimbun-lump-set-last-check-internal shimbun nil))

(provide 'sb-lump)

;;; sb-lump.el ends here
