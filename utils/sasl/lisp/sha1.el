;;; sha1.el --- SHA1 Secure Hash Algorithm.

;; Copyright (C) 1999 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;;	Kenichi OKADA <okada@opaopa.org>
;; Maintainer: Kenichi OKADA <okada@opaopa.org>
;; Keywords: SHA1, FIPS 180-1

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Examples from FIPS PUB 180-1.
;; <URL:http://www.itl.nist.gov/div897/pubs/fip180-1.htm>
;;
;; (sha1 "abc")
;; => a9993e364706816aba3e25717850c26c9cd0d89d
;;
;; (sha1 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
;; => 84983e441c3bd26ebaae4aa1f95129e5e54670f1
;;
;; (sha1 (make-string 1000000 ?a))
;; => 34aa973cd4c4daa4f61eeb2bdbad27316534016f

;;; Code:

(require 'hex-util)

(eval-when-compile
  (or (fboundp 'sha1-string)
      (defun sha1-string (a))))

(defvar sha1-dl-module
  (if (and (fboundp 'sha1-string)
	   (subrp (symbol-function 'sha1-string)))
      nil
    (if (fboundp 'dynamic-link)
	(let ((path (expand-file-name "sha1.so" exec-directory)))
	  (and (file-exists-p path)
	       path)))))

(cond
 (sha1-dl-module
  ;; Emacs with DL patch.
  (require 'sha1-dl))
 (t
  (require 'sha1-el)))

;; compatibility for another sha1.el by Keiichi Suzuki.
(defun sha1-encode (string)
  (decode-hex-string 
   (sha1-string string)))
(defun sha1-encode-binary (string)
  (decode-hex-string
   (sha1-string string)))

(make-obsolete 'sha1-encode "It's old API.")
(make-obsolete 'sha1-encode-binary "It's old API.")

(provide 'sha1)

;;; sha1.el ends here
