;;; md5.el --- MD5 Message Digest Algorithm.

;; Copyright (C) 1999 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: MD5, RFC 1321

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

;; Examples from RFC 1321.
;;
;; (md5 "")
;; => d41d8cd98f00b204e9800998ecf8427e
;;
;; (md5 "a")
;; => 0cc175b9c0f1b6a831c399e269772661
;;
;; (md5 "abc")
;; => 900150983cd24fb0d6963f7d28e17f72
;;
;; (md5 "message digest")
;; => f96b697d7cb7938d525a2f31aaf161d0
;;
;; (md5 "abcdefghijklmnopqrstuvwxyz")
;; => c3fcd3d76192e4007dfb496cca67e13b
;;
;; (md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
;; => d174ab98d277d9f5a5611c2c9f419d9f
;;
;; (md5 "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
;; => 57edf4a22be3c955ac49da2e2107b67a

;;; Code:

(cond
 ((and (fboundp 'md5)
       (subrp (symbol-function 'md5)))
  ;; recent XEmacs has `md5' as a built-in function.
  ;; (and 'md5 is already provided.)
  )
 ((and (fboundp 'dynamic-link)
       (file-exists-p (expand-file-name "md5.so" exec-directory)))
  ;; Emacs with DL patch.
  (require 'md5 "md5-dl"))
 (t
  (require 'md5 "md5-el")))

;;; md5.el ends here.
