;;; hmac-sha1.el --- Compute HMAC-SHA1.

;; Copyright (C) 1999 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: HMAC, RFC 2104, HMAC-SHA1, SHA1, Cancel-Lock

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
;; (encode-hex-string (hmac-sha1 "Hi There" (make-string 20 ?\x0b)))
;;  => "b617318655057264e28bc0b6fb378c8ef146be00"
;;
;; (encode-hex-string (hmac-sha1 "what do ya want for nothing?" "Jefe"))
;;  => "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"
;;
;; (encode-hex-string (hmac-sha1 (make-string 50 ?\xdd) (make-string 20 ?\xaa)))
;;  => "125d7342b9ac11cd91a39af48aa17b4f63f175d3"
;;
;; (encode-hex-string
;;  (hmac-sha1
;;   (make-string 50 ?\xcd)
;;   (decode-hex-string "0102030405060708090a0b0c0d0e0f10111213141516171819")))
;;  => "4c9007f4026250c6bc8414f9bf50c86c2d7235da"
;;
;; (encode-hex-string
;;  (hmac-sha1 "Test With Truncation" (make-string 20 ?\x0c)))
;;  => "4c1a03424b55e07fe7f27be1d58bb9324a9a5a04"
;; (encode-hex-string
;;  (hmac-sha1-96 "Test With Truncation" (make-string 20 ?\x0c)))
;;  => "4c1a03424b55e07fe7f27be1"
;;
;; (encode-hex-string
;;  (hmac-sha1
;;   "Test Using Larger Than Block-Size Key - Hash Key First"
;;   (make-string 80 ?\xaa)))
;;  => "aa4ae5e15272d00e95705637ce8a3b55ed402112"
;;
;; (encode-hex-string
;;  (hmac-sha1
;;   "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data"
;;   (make-string 80 ?\xaa)))
;;  => "e8e99d0f45237d786d6bbaa7965c7808bbff1a91"

;;; Code:

(eval-when-compile (require 'hmac-def))
(require 'hex-util)			; (decode-hex-string STRING)
(require 'sha1)				; expects (sha1 STRING)

;;; For consintency with hmac-md5.el, we define this function here.
(or (fboundp 'sha1-binary)
    (defun sha1-binary (string)
      "Return the SHA1 of STRING in binary form."
      (decode-hex-string (sha1 string))))

(define-hmac-function hmac-sha1 sha1-binary 64 20) ; => (hmac-sha1 TEXT KEY)
;; (define-hmac-function hmac-sha1-96 sha1-binary 64 20 96)

(provide 'hmac-sha1)

;;; hmac-sha1.el ends here
