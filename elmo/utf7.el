;;; utf7.el --- UTF-7 encoding/decoding for Emacs   -*-coding: iso-8859-1;-*-
;; Copyright (C) 1999, 2000, 2003 Free Software Foundation, Inc.

;; Author: Jon K Hellan <hellan@acm.org>
;; Maintainer: bugs@gnus.org
;; Keywords: mail

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; UTF-7 - A Mail-Safe Transformation Format of Unicode - RFC 2152
;; This is a transformation format of Unicode that contains only 7-bit
;; ASCII octets and is intended to be readable by humans in the limiting
;; case that the document consists of characters from the US-ASCII
;; repertoire.
;; In short, runs of characters outside US-ASCII are encoded as base64
;; inside delimiters.
;; A variation of UTF-7 is specified in IMAP 4rev1 (RFC 2060) as the way
;; to represent characters outside US-ASCII in mailbox names in IMAP.
;; This library supports both variants, but the IMAP variation was the
;; reason I wrote it.
;; The routines convert UTF-7 -> UTF-16 (16 bit encoding of Unicode)
;; -> current character set, and vice versa.
;; However, until Emacs supports Unicode, the only Emacs character set
;; supported here is ISO-8859.1, which can trivially be converted to/from
;; Unicode.
;; When decoding results in a character outside the Emacs character set,
;; an error is thrown.  It is up to the application to recover.

;; UTF-7 should be done by providing a coding system.  Mule-UCS does
;; already, but I don't know if it does the IMAP version and it's not
;; clear whether that should really be a coding system.  The UTF-16
;; part of the conversion can be done with coding systems available
;; with Mule-UCS or some versions of Emacs.  Unfortunately these were
;; done wrongly (regarding handling of byte-order marks and how the
;; variants were named), so we don't have a consistent name for the
;; necessary coding system.  The code below doesn't seem to DTRT
;; generally.  E.g.:
;;
;; (utf7-encode "a+£")
;;   => "a+ACsAow-"
;;
;; $ echo "a+£"|iconv -f iso-8859-1 -t utf-7
;; a+-+AKM
;;
;;  -- fx

;; Modified 5 May 2004 by Yuuichi Teranishi so that it can run with APEL/FLIM
;; instead of mm-util in Gnus.
;;
;; * Use find-coding-system instead of mm-coding-system-p.
;; * Use mel.el instead of base64.el.
;; * Don't use mm-with-unibyte-current-buffer etc.
;; * Do nothing if utf-16 coding system is not found.

;; Modified 31 Aug 2004 by Yuuichi Teranishi so that it can avoid the bug of
;; Emacs 21.3 release version.

;;; Code:

(eval-when-compile (require 'cl))
(require 'pces)
(require 'mel)

;; base64 encoding/decoding
(defun utf7-base64-encode-region (beg end &optional no-line-break))
(defun utf7-base64-decode-region (beg end))
(fset 'utf7-base64-encode-region
      (mel-find-function 'mime-encode-region "base64"))
(fset 'utf7-base64-decode-region
      (mel-find-function 'mime-decode-region "base64"))

(defconst utf7-direct-encoding-chars " -%'-*,-[]-}"
  "Character ranges which do not need escaping in UTF-7.")

(defconst utf7-imap-direct-encoding-chars
  (concat utf7-direct-encoding-chars "+\\~")
  "Character ranges which do not need escaping in the IMAP variant of UTF-7.")

(defconst utf7-utf-16-coding-system (and (fboundp 'find-coding-system)
					 (find-coding-system 'utf-16-be))
  "Coding system which encodes big endian UTF-16.")

(defsubst utf7-imap-get-pad-length (len modulus)
  "Return required length of padding for IMAP modified base64 fragment."
  (mod (- len) modulus))

(defun utf7-encode-internal (&optional for-imap)
  "Encode text in (temporary) buffer as UTF-7.
Use IMAP modification if FOR-IMAP is non-nil."
  (let ((start (point-min))
	(end (point-max)))
    (narrow-to-region start end)
    (goto-char start)
    (let* ((esc-char (if for-imap ?& ?+))
	   (direct-encoding-chars
	    (if for-imap utf7-imap-direct-encoding-chars
	      utf7-direct-encoding-chars))
	   (not-direct-encoding-chars (concat "^" direct-encoding-chars)))
      (while (not (eobp))
	(skip-chars-forward direct-encoding-chars)
	(unless (eobp)
	  (insert esc-char)
	  (let ((p (point))
		(fc (following-char))
		(run-length
		 (skip-chars-forward not-direct-encoding-chars)))
	    (if (and (= fc esc-char)
		     (= run-length 1))	; Lone esc-char?
		(delete-backward-char 1) ; Now there's one too many
	      (utf7-fragment-encode p (point) for-imap))
	    (insert "-")))))))

(defun utf7-fragment-encode (start end &optional for-imap)
  "Encode text from START to END in buffer as UTF-7 escape fragment.
Use IMAP modification if FOR-IMAP is non-nil."
  (let ((converter (utf7-get-u16char-converter 'to-utf-16))
	(str (buffer-substring start end))
	pm)
    (when converter
      (delete-region start end)
      (goto-char start)
      (insert
       (with-temp-buffer
	 (insert str)
	 (funcall converter)
	 (set-buffer-multibyte nil)
	 (utf7-base64-encode-region (point-min) (point-max))
	 (goto-char (point-min))
	 (setq pm (point-max))
	 (when for-imap
	   (while (search-forward "/" nil t)
	     (replace-match ",")))
	 (skip-chars-forward "^= \t\n" pm)
	 (delete-region (point) pm)
	 (buffer-string))))))

(defun utf7-decode-internal (&optional for-imap)
  "Decode UTF-7 text in (temporary) buffer.
Use IMAP modification if FOR-IMAP is non-nil."
  (let ((start (point-min))
	(end (point-max)))
    (goto-char start)
    (let* ((esc-pattern (concat "^" (char-to-string (if for-imap ?& ?+))))
	   (base64-chars (concat "A-Za-z0-9+"
				 (char-to-string (if for-imap ?, ?/)))))
      (while (not (eobp))
	(skip-chars-forward esc-pattern)
	(unless (eobp)
	  (forward-char)
	  (let ((p (point))
		(run-length (skip-chars-forward base64-chars)))
	    (when (and (not (eobp)) (= (following-char) ?-))
	      (delete-char 1))
	    (unless (= run-length 0)	; Encoded lone esc-char?
	      (save-excursion
		(utf7-fragment-decode p (point) for-imap)
		(goto-char p)
		(delete-backward-char 1)))))))))

(defun utf7-fragment-decode (start end &optional for-imap)
  "Decode base64 encoded fragment from START to END of UTF-7 text in buffer.
Use IMAP modification if FOR-IMAP is non-nil."
  (save-restriction
    (narrow-to-region start end)
    (let ((converter (utf7-get-u16char-converter 'from-utf-16)))
      (when converter
	(when for-imap
	  (goto-char start)
	  (while (search-forward "," nil 'move-to-end) (replace-match "/")))
	(let ((pl (utf7-imap-get-pad-length (- end start) 4)))
	  (insert (make-string pl ?=))
	  (utf7-base64-decode-region start (+ end pl)))
	(funcall converter)))))

(defun utf7-get-u16char-converter (which-way)
  "Return a function to convert between UTF-16 and current character set."
  (if utf7-utf-16-coding-system
      (if (eq which-way 'to-utf-16)
	  (lambda ()
	    (encode-coding-region (point-min)(point-max)
				  utf7-utf-16-coding-system)
	    (set-buffer-multibyte nil)
	    (goto-char (point-min))
	    ;; Remove BOM (Big-endian UTF-16 FE FF)
	    (while (re-search-forward "\376\377" nil t)
	      (delete-region (match-beginning 0)(match-end 0))))
	(lambda ()
	  (goto-char (point-min))
	  ;; Add BOM (Big-endian UTF-16 FE FF)
	  (insert "\376\377")
	  (decode-coding-region (point-min) (point-max)
				utf7-utf-16-coding-system)))))

(defun utf7-encode (string &optional for-imap)
  "Encode UTF-7 STRING.  Use IMAP modification if FOR-IMAP is non-nil."
  (let ((default-enable-multibyte-characters t))
    (with-temp-buffer
      (insert string)
      (utf7-encode-internal for-imap)
      (buffer-string))))

(defun utf7-decode (string &optional for-imap)
  "Decode UTF-7 STRING.  Use IMAP modification if FOR-IMAP is non-nil."
  (let ((default-enable-multibyte-characters nil))
    (with-temp-buffer
      (insert string)
      (utf7-decode-internal for-imap)
      (set-buffer-multibyte t)
      (buffer-string))))

(defalias 'utf7-encode-string 'utf7-encode)
(defalias 'utf7-decode-string 'utf7-decode)

(provide 'utf7)

;;; utf7.el ends here
