;;; hmac-md5.el --- Compute HMAC-MD5.

;; Copyright (C) 1999 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;;	Kenichi OKADA <okada@opaopa.org>
;; Maintainer: Kenichi OKADA <okada@opaopa.org>
;; Keywords: HMAC, RFC 2104, HMAC-MD5, MD5, KEYED-MD5, CRAM-MD5

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

;; Test cases from RFC 2202, "Test Cases for HMAC-MD5 and HMAC-SHA-1".
;;
;; (encode-hex-string (hmac-md5 "Hi There" (make-string 16 ?\x0b)))
;;  => "9294727a3638bb1c13f48ef8158bfc9d"
;;
;; (encode-hex-string (hmac-md5 "what do ya want for nothing?" "Jefe"))
;;  => "750c783e6ab0b503eaa86e310a5db738"
;;
;; (encode-hex-string (hmac-md5 (make-string 50 ?\xdd) (make-string 16 ?\xaa)))
;;  => "56be34521d144c88dbb8c733f0e8b3f6"
;;
;; (encode-hex-string
;;  (hmac-md5
;;   (make-string 50 ?\xcd)
;;   (decode-hex-string "0102030405060708090a0b0c0d0e0f10111213141516171819")))
;;  => "697eaf0aca3a3aea3a75164746ffaa79"
;;
;; (encode-hex-string
;;  (hmac-md5 "Test With Truncation" (make-string 16 ?\x0c)))
;;  => "56461ef2342edc00f9bab995690efd4c"
;; (encode-hex-string
;;  (hmac-md5-96 "Test With Truncation" (make-string 16 ?\x0c)))
;;  => "56461ef2342edc00f9bab995"
;;
;; (encode-hex-string
;;  (hmac-md5
;;   "Test Using Larger Than Block-Size Key - Hash Key First"
;;   (make-string 80 ?\xaa)))
;;  => "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd"
;;
;; (encode-hex-string
;;  (hmac-md5
;;   "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data"
;;   (make-string 80 ?\xaa)))
;;  => "6f630fad67cda0ee1fb1f562db3aa53e"

;;; Code:

(eval-when-compile (require 'hmac-def))
(require 'hex-util)			; (decode-hex-string STRING)
(require 'md5)				; expects (md5 STRING)

;; We cannot define this function in md5.el because recent XEmacs provides
;; built-in md5 function and provides feature 'md5 at startup.
(if (and (featurep 'xemacs)
	 (fboundp 'md5)
	 (subrp (symbol-function 'md5))
	 (condition-case nil
	     ;; `md5' of XEmacs 21 takes 4th arg CODING (and 5th arg NOERROR).
	     (md5 "" nil nil 'binary)	; => "fb5d2156096fa1f254352f3cc3fada7e"
	   (error nil)))
    ;; XEmacs 21.
    (defun md5-binary (string &optional start end)
      "Return the MD5 of STRING in binary form."
      (decode-hex-string (md5 string start end 'binary)))
  ;; not XEmacs 21 and not DL.
  (if (not (fboundp 'md5-binary))
      (defun md5-binary (string)
	"Return the MD5 of STRING in binary form."
	(decode-hex-string (md5 string)))))

(define-hmac-function hmac-md5 md5-binary 64 16) ; => (hmac-md5 TEXT KEY)
;; (define-hmac-function hmac-md5-96 md5-binary 64 16 96)

(provide 'hmac-md5)

;;; hmac-md5.el ends here
