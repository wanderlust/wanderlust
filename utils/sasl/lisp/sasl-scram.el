;;; sasl-scram.el --- Compute SCRAM-MD5.

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
;; (sasl-scram-md5-make-security-info nil t 0)
;; => "^A^@^@^@"
;;
;; (base64-encode-string
;;  (sasl-scram-md5-make-client-msg-2
;;   (base64-decode-string "dGVzdHNhbHQBAAAAaW1hcEBlbGVhbm9yLmlubm9zb2Z0LmNvbQBqaGNOWmxSdVBiemlGcCt2TFYrTkN3")
;;   (base64-decode-string "AGNocmlzADx0NG40UGFiOUhCMEFtL1FMWEI3MmVnQGVsZWFub3IuaW5ub3NvZnQuY29tPg==")
;;   (sasl-scram-md5-make-salted-pass
;;    "secret stuff" "testsalt")
;;   (sasl-scram-md5-make-security-info nil t 0)))
;; => "AQAAAMg9jU8CeB4KOfk7sUhSQPs="
;;
;; (base64-encode-string
;;  (sasl-scram-md5-make-server-msg-2
;;   (base64-decode-string "dGVzdHNhbHQBAAAAaW1hcEBlbGVhbm9yLmlubm9zb2Z0LmNvbQBqaGNOWmxSdVBiemlGcCt2TFYrTkN3")
;;   (base64-decode-string "AGNocmlzADx0NG40UGFiOUhCMEFtL1FMWEI3MmVnQGVsZWFub3IuaW5ub3NvZnQuY29tPg==")
;;   (sasl-scram-md5-make-security-info nil t 0)
;;   "testsalt"
;;   (sasl-scram-md5-make-salted-pass
;;    "secret stuff" "testsalt")))
;; => "U0odqYw3B7XIIW0oSz65OQ=="

;;; Code:

(require 'sasl)
(require 'hmac-md5)

(defvar sasl-scram-md5-unique-id-function
  sasl-unique-id-function)

(defconst sasl-scram-md5-steps
  '(ignore				;no initial response
    sasl-scram-md5-response-1
    sasl-scram-md5-response-2
    sasl-scram-md5-authenticate-server))

(defmacro sasl-scram-md5-security-info-no-security-layer (security-info)
  `(eq (logand (aref ,security-info 0) 1) 1))
(defmacro sasl-scram-md5-security-info-integrity-protection-layer (security-info)
  `(eq (logand (aref ,security-info 0) 2) 2))
(defmacro sasl-scram-md5-security-info-buffer-size (security-info)
  `(let ((ssecinfo ,security-info))
     (+ (lsh (aref ssecinfo 1) 16)
	(lsh (aref ssecinfo 2) 8)
	(aref ssecinfo 3))))

(defun sasl-scram-md5-make-security-info (integrity-protection-layer
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

(defun sasl-scram-md5-make-unique-nonce ()	; 8*OCTET, globally unique.
  ;; For example, concatenated string of process-identifier, system-clock,
  ;; sequence-number, random-number, and domain-name.
  (let* ((sasl-unique-id-function sasl-scram-md5-unique-id-function)
	 (id (sasl-unique-id)))
    (unwind-protect
	(concat "<" id "@" (system-name) ">")
      (fillarray id 0))))

(defun sasl-scram-md5-xor-string (str1 str2)
  ;; (length str1) == (length str2) == (length dst) == 16 (in SCRAM-MD5)
  (let* ((len (length str1))
         (dst (make-string len 0))
         (pos 0))
    (while (< pos len)
      (aset dst pos (logxor (aref str1 pos) (aref str2 pos)))
      (setq pos (1+ pos)))
    dst))

(defun sasl-scram-md5-make-client-msg-1 (authenticate-id &optional authorize-id nonce)
  "Make an initial client message from AUTHENTICATE-ID and AUTHORIZE-ID.
If AUTHORIZE-ID is the same as AUTHENTICATE-ID, it may be omitted."
  (concat authorize-id "\0" authenticate-id "\0"
	  (or nonce
	      (sasl-scram-md5-make-unique-nonce))))

(defun sasl-scram-md5-parse-server-msg-1 (server-msg-1)
  "Parse SERVER-MSG-1 and return a list of (SALT SECURITY-INFO SERVICE-ID)."
  (if (and (> (length server-msg-1) 16)
	   (eq (string-match "[^@]+@[^\0]+\0" server-msg-1 12) 12))
      (list (substring server-msg-1 0 8)	; salt
	    (substring server-msg-1 8 12)	; server-security-info
	    (substring server-msg-1	; service-id
		       12 (1- (match-end 0))))
    (sasl-error (format "Unexpected response: %s" server-msg-1))))

(defun sasl-scram-md5-server-salt (server-msg-1)
  (car (sasl-scram-md5-parse-server-msg-1 server-msg-1)))

(defun sasl-scram-md5-make-salted-pass (passphrase salt)
  (hmac-md5 salt passphrase))

(defun sasl-scram-md5-make-client-key (salted-pass)
  (md5-binary salted-pass))

(defun sasl-scram-md5-make-client-verifier (client-key)
  (md5-binary client-key))

(defun sasl-scram-md5-make-shared-key (server-msg-1
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

(defun sasl-scram-md5-make-client-proof (client-key shared-key)
  (sasl-scram-md5-xor-string client-key shared-key))

(defun sasl-scram-md5-make-client-msg-2 (server-msg-1
					 client-msg-1
					 salted-pass
					 client-security-info)
  (let (client-proof client-key shared-key client-verifier)
    (setq client-key
          (sasl-scram-md5-make-client-key salted-pass))
    (setq client-verifier
          (sasl-scram-md5-make-client-verifier client-key))
    (setq shared-key
          (unwind-protect
              (sasl-scram-md5-make-shared-key
               server-msg-1
               client-msg-1
               client-security-info
               client-verifier)
            (fillarray client-verifier 0)))
    (setq client-proof
          (unwind-protect
              (sasl-scram-md5-make-client-proof
               client-key shared-key)
            (fillarray client-key 0)
            (fillarray shared-key 0)))
    (unwind-protect
        (concat
         client-security-info
         client-proof)
      (fillarray client-proof 0))))

(defun sasl-scram-md5-make-server-msg-2 (server-msg-1
					 client-msg-1
					 client-security-info
					 salt salted-pass)
  (let ((server-salt
	(hmac-md5 salt salted-pass))
	buff)
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

(defun sasl-scram-md5-response-1 (client step)
  (sasl-client-set-property
   client 'client-msg-1
   (sasl-scram-md5-make-client-msg-1
    (sasl-client-name client)
    (sasl-client-property client 'authorize-id)
    (sasl-client-property client 'nonce))))

(defun sasl-scram-md5-response-2 (client step)
  (let* ((server-msg-1
	  (sasl-client-set-property
	   client 'server-msg-1
	   (sasl-step-data step)))
	 (salted-pass
	  (sasl-client-set-property
	   client 'salted-pass
	   (sasl-scram-md5-make-salted-pass
	    (sasl-read-passphrase
	     (format "SCRAM-MD5 passphrase for %s: "
		     (sasl-client-name client)))
	    (sasl-scram-md5-server-salt server-msg-1)))))
    (sasl-client-set-property
     client 'client-msg-2
     (sasl-scram-md5-make-client-msg-2
      server-msg-1
      (sasl-client-property client 'client-msg-1)
      salted-pass
      (or (sasl-client-property client 'client-security-info)
	  (sasl-scram-md5-make-security-info nil t 0))))))

(defun sasl-scram-md5-authenticate-server (client step)
  (let ((server-msg-2
	 (sasl-client-set-property
	  client 'server-msg-2
	  (sasl-step-data step)))
	(server-msg-1
	 (sasl-client-property client 'server-msg-1)))
    (if (string= server-msg-2
		     (sasl-scram-md5-make-server-msg-2
		      server-msg-1
		      (sasl-client-property client 'client-msg-1)
		      (or (sasl-client-property client 'client-security-info)
			  (sasl-scram-md5-make-security-info nil t 0))
		      (sasl-scram-md5-server-salt server-msg-1)
		      (sasl-client-property client 'salted-pass)))
	" "
      (sasl-error "SCRAM-MD5:  authenticate server failed."))))

(put 'sasl-scram 'sasl-mechanism
     (sasl-make-mechanism "SCRAM-MD5" sasl-scram-md5-steps))

(provide 'sasl-scram)

;;; sasl-scram.el ends here
