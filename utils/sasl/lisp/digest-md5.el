;;; digest-md5.el --- Compute DIGEST-MD5.

;; Copyright (C) 1999 Kenichi OKADA

;; Author: Kenichi OKADA <okada@opaopa.org>
;;	Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
;; Keywords: DIGEST-MD5, HMAC-MD5, SASL, IMAP, POP, ACAP

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

;; This program is implemented from draft-leach-digest-sasl-05.txt.
;;
;; It is caller's responsibility to base64-decode challenges and
;; base64-encode responses in IMAP4 AUTHENTICATE command.
;;
;; Passphrase should be longer than 16 bytes. (See RFC 2195)

;; Examples.
;;
;; (digest-md5-parse-digest-challenge 
;;   "realm=\"elwood.innosoft.com\",nonce=\"OA6MG9tEQGm2hh\",qop=\"auth\",algorithm=md5-sess,charset=utf-8")
;; => (realm "elwood.innosoft.com" nonce "OA6MG9tEQGm2hh" qop "auth" algorithm md5-sess charset utf-8)

;; (digest-md5-build-response-value
;;   "chris" "elwood.innosoft.com" "secret" "OA6MG9tEQGm2hh"
;;   "OA6MHXh6VqTrRk" 1 "imap/elwood.innosoft.com" "auth")
;; => "d388dad90d4bbd760a152321f2143af7"

;;; Code:

(require 'hmac-md5)
(require 'unique-id)

(defvar digest-md5-challenge nil)

(defvar digest-md5-parse-digest-challenge-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?, "." table)
    table)
  "A syntax table for parsing digest-challenge attributes.")

;;;###autoload
(defun digest-md5-parse-digest-challenge (digest-challenge)
  ;; return a property list of 
  ;; (realm nonce qop-options stale maxbuf charset 
  ;; algorithm cipher-opts auth-param).
  (with-temp-buffer
    (set-syntax-table digest-md5-parse-digest-challenge-syntax-table)
    (insert digest-challenge)
    (goto-char (point-min))
    (insert "(")
    (while (progn (forward-sexp) (not (eobp)))
      (delete-char 1)
      (insert " "))
    (insert ")")
    (condition-case nil
	(setplist 'digest-md5-challenge (read (point-min-marker)))
      (end-of-file
       (error "Parse error in digest-challenge.")))))

(defun digest-md5-digest-uri (serv-type host &optional serv-name)
  (concat serv-type "/" host
	  (if (and serv-name
		   (null (string= host serv-name)))
	      (concat "/" serv-name))))

(defmacro digest-md5-cnonce ()
  ;; It is RECOMMENDED that it 
  ;; contain at least 64 bits of entropy.
  '(concat (unique-id-m "") (unique-id-m "")))

(defmacro digest-md5-challenge (prop)
  (list 'get ''digest-md5-challenge prop))

(defmacro digest-md5-build-response-value
  (username realm passwd nonce cnonce nonce-count digest-uri qop)
  `(encode-hex-string
    (md5-binary
     (concat
      (encode-hex-string
       (md5-binary (concat (md5-binary 
			    (concat ,username 
				    ":" ,realm
				    ":" ,passwd))
			   ":" ,nonce
			   ":" ,cnonce
			   (let ((authzid (digest-md5-challenge 'authzid)))
			     (if authzid (concat ":" authzid) nil)))))
      ":" ,nonce
      ":" (format "%08x" ,nonce-count) ":" ,cnonce ":" ,qop ":"
      (encode-hex-string
       (md5-binary
	(concat "AUTHENTICATE:" ,digest-uri
		(if (string-equal "auth-int" ,qop)
		    ":00000000000000000000000000000000"
		  nil))))))))

;;;###autoload
(defun digest-md5-digest-response
  (username realm passwd nonce cnonce nonce-count digest-uri
	    &optional charset qop maxbuf cipher authzid)
  (concat
   "username=\"" username "\","
   "realm=\"" realm "\","
   "nonce=\"" nonce "\","
   (format "nc=%08x," nonce-count)
   "cnonce=\"" cnonce "\","
   "digest-uri=\"" digest-uri "\","
   "response=" 
   (digest-md5-build-response-value
    username realm passwd nonce cnonce nonce-count digest-uri
    (or qop "auth"))
   ","
   (mapconcat 
    #'identity
    (delq nil 
	  (mapcar (lambda (prop)
		    (if (digest-md5-challenge prop)
			(format "%s=%s"
				prop (digest-md5-challenge prop))))
		  '(charset qop maxbuf cipher authzid)))
    ",")))
  
(provide 'digest-md5)

;;; digest-md5.el ends here
