;;; utf7.el --- UTF-7 encoding/decoding for Emacs
;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Jon K Hellan <hellan@item.ntnu.no>
;; Keywords: mail

;; This file is part of GNU Emacs, but the same permissions apply

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
;;; UTF-7 - A Mail-Safe Transformation Format of Unicode - RFC 2152
;;; This is a transformation format of Unicode that contains only 7-bit
;;; ASCII octets and is intended to be readable by humans in the limiting 
;;; case that the document consists of characters from the US-ASCII
;;; repertoire.
;;; In short, runs of characters outside US-ASCII are encoded as base64 
;;; inside delimiters.
;;; A variation of UTF-7 is specified in IMAP 4rev1 (RFC 2060) as the way
;;; to represent characters outside US-ASCII in mailbox names in IMAP.
;;; This library supports both variants, but the IMAP variation was the
;;; reason I wrote it. 
;;; The routines convert UTF-7 -> UTF-16 (16 bit encoding of Unicode) 
;;; -> current character set, and vice versa. 
;;; However, until Emacs supports Unicode, the only Emacs character set
;;; supported here is ISO-8859.1, which can trivially be converted to/from
;;; Unicode.
;;; When decoding results in a character outside the Emacs character set,
;;; an error is thrown. It is up to the application to recover.

;;; Modified 8 December 1999 by Yuuichi Teranishi so that it will work under
;;; Emacs+Mule-UCS and XEmacs.

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'pces))
(eval-when-compile (require 'static))

;; base64 stuff.
;;(static-if (and (fboundp 'base64-decode-region)
;;		(subrp (symbol-function 'base64-decode-region)))
;;    (eval-and-compile (fset 'utf7-base64-decode-region 'base64-decode-region))
;;  (require 'mel)
;;  (defun utf7-base64-decode-region (start end)
;;    (fset 'utf7-base64-decode-string
;;	  (symbol-function (mel-find-function 'mime-decode-region "base64")))
;;    (utf7-base64-decode-region start end)))

;;(static-if (and (fboundp 'base64-encode-region)
;;		(subrp (symbol-function 'base64-encode-region)))
;;    (eval-and-compile (fset 'utf7-base64-encode-region 'base64-encode-region))
;;  (defun utf7-base64-encode-string (start end)
;;    (fset 'utf7-base64-encode-region
;;	  (symbol-function (mel-find-function 'mime-encode-region "base64")))
;;    (utf7-base64-encode-region start end)))

;; On XEmacs which does not support UTF-16 have to use u7tou8 and u8tou7. 
;; These programs are included in
;; ftp://ftp.ifcss.org/pub/software/unix/convert/utf7.tar.gz
(defvar utf7-utf7-to-utf8-program "u7tou8"
  "Program to convert utf7 to utf8.")
(defvar utf7-utf8-to-utf7-program "u8tou7"
  "Program to convert utf8 to utf7.")

(defvar utf7-direct-encoding-chars " -%'-*,-[]-} \t\n\r"
  "Characters ranges which do not need escaping in UTF-7")
(defvar utf7-imap-direct-encoding-chars 
  (concat utf7-direct-encoding-chars "+\\\\~")
  "Characters ranges which do not need escaping in the IMAP modified variant of UTF-7")

(defsubst utf7-imap-get-pad-length (len modulus)	  
  "Return required length of padding for IMAP modified base64 fragment."
  (mod (- len) modulus))

(cond
 ((or (and (fboundp 'find-coding-system)
	   (find-coding-system 'utf-7))
      (module-installed-p 'un-define))
  (defun utf7-fragment-decode (start end &optional imap)
    "Decode base64 encoded fragment from START to END of UTF-7 text in buffer.
Use IMAP modification if IMAP is non-nil."
    (require 'un-define)
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert "+")
      (when imap
	(goto-char start)
	(while (search-forward "," nil 'move-to-end) (replace-match "/")))
      (decode-coding-region (point-min) (point-max) 'utf-7)))

  (defun utf7-fragment-encode (start end &optional imap)
    "Encode text from START to END in buffer as UTF-7 escape fragment.
Use IMAP modification if IMAP is non-nil."
    (require 'un-define)
    (let ((buffer (current-buffer))
	  encoded-string)
      (setq encoded-string
	    (with-temp-buffer
	      (insert-buffer-substring buffer start end)
	      (encode-coding-region (point-min) 
				    (point-max) 'utf-7)
	      (goto-char (point-min))
	      (when imap
		(skip-chars-forward "+")
		(delete-region (point-min) (point))
		(insert "&")
		(while (search-forward "/" nil t)
		  (replace-match ",")))
	      (skip-chars-forward "^= \t\n" (point-max))
	      (delete-region (point) (point-max))
	      (buffer-string)))
      (delete-region start end)
      (insert encoded-string))))
 ((and (featurep 'xemacs) 
       (or (and (fboundp 'find-coding-system)
		(find-coding-system 'utf-8))
	   (module-installed-p 'xemacs-ucs)))
  (defun utf7-fragment-decode (start end &optional imap)
    "Decode base64 encoded fragment from START to END of UTF-7 text in buffer.
Use IMAP modification if IMAP is non-nil."  
    (require 'xemacs-ucs)
    (save-restriction 
      (narrow-to-region start end)
      (when imap
	(goto-char start)
	(while (search-forward "," nil 'move-to-end) (replace-match "/")))
      (goto-char (point-min))
      (insert "+")
      (as-binary-process
       (call-process-region (point-min) (point-max)
			    utf7-utf7-to-utf8-program
			    t (current-buffer)))
      (decode-coding-region (point-min) (point-max) 'utf-8)))
  
  (defun utf7-fragment-encode (start end &optional imap)
    "Decode base64 encoded fragment from START to END of UTF-7 text in buffer.
Use IMAP modification if IMAP is non-nil."  
    (require 'xemacs-ucs)
    (let ((buffer (current-buffer))
	  encoded-string)
      (setq encoded-string
	    (with-temp-buffer
	      (insert-buffer-substring buffer start end)
	      (save-excursion 
		(goto-char (point-max))
		(insert "\n"))
	      (encode-coding-region (point-min) (point-max) 'utf-8)
	      (as-binary-process
	       (call-process-region (point-min) (point-max)
				    utf7-utf8-to-utf7-program
				    t (current-buffer)))
	      (goto-char (point-min))
	      (when imap
		(skip-chars-forward "+")
		(delete-region (point-min) (point))
		(insert "&")
		(while (search-forward "/" nil t)
		  (replace-match ",")))
	      (goto-char (point-max))
	      (delete-backward-char 1)
	      (insert "-")
	      (buffer-string)))
      (delete-region start end)
      (insert encoded-string))))
 (t
  (defun utf7-fragment-decode (start end &optional imap)
    "Encode text from START to END in buffer as UTF-7 escape fragment.
Use IMAP modification if IMAP is non-nil."
    ;; Define as a null function.
    )

  (defun utf7-fragment-encode (start end &optional imap)
    "Encode text from START to END in buffer as UTF-7 escape fragment.
Use IMAP modification if IMAP is non-nil."
    ;; Define as a null function.
    )))

(defun utf7-encode-region (start end &optional imap)
  "Encode text in region as UTF-7.
Use IMAP modification if IMAP is non-nil."
  (interactive "r")
  (save-restriction
    ;;(set-buffer-multibyte default-enable-multibyte-characters)
    (narrow-to-region start end)
    (goto-char start)
    (let ((esc-char (if imap ?& ?+))
	  (direct-encoding-chars 
	   (if imap utf7-imap-direct-encoding-chars
	     utf7-direct-encoding-chars)))
      (while (not (eobp))
	(skip-chars-forward direct-encoding-chars)
	(unless (eobp)
	  (let ((p (point))
		(fc (following-char))
		(run-length 
		 (skip-chars-forward (concat "^" direct-encoding-chars))))
	    (if (and (= fc esc-char)
		     (= run-length 1))	; Lone esc-char?
		(delete-backward-char 1) ; Now there's one too many
	      (utf7-fragment-encode p (point) imap))))))))

(defun utf7-decode-region (start end &optional imap)
  "Decode UTF-7 text in region.
Use IMAP modification if IMAP is non-nil."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let* ((esc-pattern (concat "^" (char-to-string (if imap ?& ?+))))
	   (base64-chars (concat "A-Za-z0-9+" 
				 (char-to-string (if imap ?, ?/)))))
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
		(utf7-fragment-decode p (point) imap)
		(goto-char p)
		(delete-backward-char 1)))))))
    ;;(set-buffer-multibyte default-enable-multibyte-characters)
    ))

(defun utf7-encode-string (string &optional imap)
  "Encode UTF-7 string. Use IMAP modification if IMAP is non-nil."
  (with-temp-buffer
    (insert string)
    (utf7-encode-region (point-min) (point-max) imap)
    (buffer-string)))

(defun utf7-decode-string (string &optional imap)
  "Decode UTF-7 string. Use IMAP modification if IMAP is non-nil."
  (with-temp-buffer
    (insert string)
    (utf7-decode-region (point-min) (point-max) imap)
    (buffer-string)))

;; For compatibility.
(defalias 'utf7-decode 'utf7-decode-string)
(defalias 'utf7-encode 'utf7-encode-string)

(provide 'utf7)

;;; utf7.el ends here
