;;; sha1-dl.el --- SHA1 Secure Hash Algorithm using DL module.

;; Copyright (C) 1999 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
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

;;; Code:

(provide 'sha1-dl)			; beware of circular dependency.
(eval-when-compile (require 'sha1))	; sha1-dl-module.

(defvar sha1-dl-handle
  (and (stringp sha1-dl-module)
       (file-exists-p sha1-dl-module)
       (dynamic-link sha1-dl-module)))

;;; sha1-dl-module provides `sha1-string' and `sha1-binary'.
(dynamic-call "emacs_sha1_init" sha1-dl-handle)

(defun sha1-region (beg end)
  (sha1-string (buffer-substring-no-properties beg end)))

(defun sha1 (object &optional beg end)
  "Return the SHA1 (Secure Hash Algorithm) of an object.
OBJECT is either a string or a buffer.
Optional arguments BEG and END denote buffer positions for computing the
hash of a portion of OBJECT."
  (if (stringp object)
      (sha1-string object)
    (save-excursion
      (set-buffer object)
      (sha1-region (or beg (point-min)) (or end (point-max))))))

(provide 'sha1-dl)

;;; sha1-dl.el ends here
