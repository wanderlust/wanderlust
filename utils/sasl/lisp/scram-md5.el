;;; scram-md5.el --- Compute SCRAM-MD5.

;; Copyright (C) 1999 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;;	Kenichi OKADA <okada@opaopa.org>
;; Keywords: SCRAM-MD5, HMAC-MD5, SASL, IMAP, POP, ACAP

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

;; This program is implemented from draft-newman-auth-scram-03.txt.
;;
;; It is caller's responsibility to base64-decode challenges and
;; base64-encode responses in IMAP4 AUTHENTICATE command.
;;
;; Passphrase should be longer than 16 bytes. (See RFC 2195)

;; Examples.
;;
;; (scram-make-security-info nil t 0)
;; => "^A^@^@^@"

;;; Code:

(require 'hmac-md5)
(require 'unique-id)

(defmacro scram-security-info-no-security-layer (security-info)
  `(eq (logand (aref ,security-info 0) 1) 1))
(defmacro scram-security-info-integrity-protection-layer (security-info)
  `(eq (logand (aref ,security-info 0) 2) 2))
(defmacro scram-security-info-buffer-size (security-info)
  `(let ((ssecinfo ,security-info))
     (+ (lsh (aref ssecinfo 1) 16)
	(lsh (aref ssecinfo 2) 8)
	(aref ssecinfo 3))))

(defun scram-make-security-info (integrity-protection-layer
				 no-security-layer buffer-size)
  (let ((csecinfo (make-string 4 0)))
    (when integrity-protection-layer
      (aset csecinfo 0 2))
    (if no-security-layer
	(aset csecinfo 0 (logior (aref csecinfo 0) 1))
      (aset csecinfo 1
	    (lsh (logand buffer-size (lsh 255 16)) -16))
      (aset csecinfo 2
	    (lsh (logand buffer-size (lsh 255 8)) -8))
      (aset csecinfo 3 (logand buffer-size 255)))
    csecinfo))

(defun scram-make-unique-nonce ()	; 8*OCTET, globally unique.
  ;; For example, concatenated string of process-identifier, system-clock,
  ;; sequence-number, random-number, and domain-name.
  (let (id)
    (unwind-protect
	(concat "<" 
		(setq id (unique-id-m ".sasl"))
		"@" (system-name) ">")
      (fillarray id 0))))

(defun scram-xor-string (str1 str2)
  ;; (length str1) == (length str2) == (length dst) == 16 (in SCRAM-MD5)
  (let* ((len (length str1))
         (dst (make-string len 0))
         (pos 0))
    (while (< pos len)
      (aset dst pos (logxor (aref str1 pos) (aref str2 pos)))
      (setq pos (1+ pos)))
    dst))

(defun scram-md5-make-client-msg-1 (authenticate-id &optional authorize-id)
  "Make an initial client message from AUTHENTICATE-ID and AUTHORIZE-ID.
If AUTHORIZE-ID is the same as AUTHENTICATE-ID, it may be omitted."
  (let (nonce)
    (unwind-protect
	(concat authorize-id "\0" authenticate-id "\0" 
		(setq nonce (scram-make-unique-nonce)))
      (fillarray nonce 0))))

(defun scram-md5-parse-server-msg-1 (server-msg-1)
  "Parse SERVER-MSG-1 and return a list of (SALT SECURITY-INFO SERVICE-ID)."
  (when (and (> (length server-msg-1) 16)
	     (eq (string-match "[^@]+@[^\0]+\0" server-msg-1 12) 12))
    (list (substring server-msg-1 0 8)	; salt
	  (substring server-msg-1 8 12)	; server-security-info
	  (substring server-msg-1	; service-id
		     12 (1- (match-end 0))))))

(defun scram-md5-make-salted-pass (passphrase salt)
  (hmac-md5 salt passphrase))

(defun scram-md5-make-client-key (salted-pass)
  (md5-binary salted-pass))

(defun scram-md5-make-client-verifier (client-key)
  (md5-binary client-key))

(defun scram-md5-make-shared-key (server-msg-1
				  client-msg-1
				  client-security-info
				  client-verifier)
  (let (buff)
    (unwind-protect
	(hmac-md5
	 (setq buff
	       (concat server-msg-1 client-msg-1 client-security-info))
	 client-verifier)
      (fillarray buff 0))))

(defun scram-md5-make-client-proof (client-key shared-key)
  (scram-xor-string client-key shared-key))

(defun scram-md5-make-client-msg-2 (client-security-info client-proof)
  (concat client-security-info client-proof))

(defun scram-md5-make-server-msg-2 (server-msg-1
				    client-msg-1
				    client-security-info
				    salt salted-pass)
  (let (buff server-salt)
    (setq server-salt
	  (hmac-md5 salt salted-pass))
    (unwind-protect
	(hmac-md5
	 (setq buff
	       (concat
		client-msg-1
		server-msg-1
		client-security-info))
	 server-salt)
      (fillarray server-salt 0)
      (fillarray buff 0))))

(provide 'scram-md5)

;;; scram-md5.el ends here
