;;; sb-bbdb-ml.el --- shimbun backend for bbdb-ml

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
(require 'sb-fml)

(luna-define-class shimbun-bbdb-ml (shimbun-fml) ())

(defvar shimbun-bbdb-ml-url "http://www.rc.tutrp.tut.ac.jp/bbdb-ml/")
(defvar shimbun-bbdb-ml-groups '("bbdb-ml"))
(defvar shimbun-bbdb-ml-coding-system (static-if (boundp 'MULE)
					  '*iso-2022-jp* 'iso-2022-jp))

(provide 'sb-bbdb-ml)

;;; sb-bbdb-ml.el ends here
