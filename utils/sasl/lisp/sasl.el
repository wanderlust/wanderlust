;;; sasl.el --- basic functions for SASL

;; Copyright (C) 1995, 1996, 1998, 1999 Free Software Foundation, Inc.

;; Author: Kenichi OKADA <okada@opaopa.org>
;; Keywords: SMTP, SASL, RFC2222

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Example.
;;
;; (base64-encode-string
;;  (sasl-scram-md5-client-msg-2
;;   (base64-decode-string "dGVzdHNhbHQBAAAAaW1hcEBlbGVhbm9yLmlubm9zb2Z0LmNvbQBqaGNOWmxSdVBiemlGcCt2TFYrTkN3")
;;   (base64-decode-string "AGNocmlzADx0NG40UGFiOUhCMEFtL1FMWEI3MmVnQGVsZWFub3IuaW5ub3NvZnQuY29tPg==")
;;   (scram-md5-make-salted-pass
;;    "secret stuff" "testsalt")))
;; => "AQAAAMg9jU8CeB4KOfk7sUhSQPs="
;;
;; (base64-encode-string
;;  (scram-md5-make-server-msg-2
;;   (base64-decode-string "dGVzdHNhbHQBAAAAaW1hcEBlbGVhbm9yLmlubm9zb2Z0LmNvbQBqaGNOWmxSdVBiemlGcCt2TFYrTkN3")
;;   (base64-decode-string "AGNocmlzADx0NG40UGFiOUhCMEFtL1FMWEI3MmVnQGVsZWFub3IuaW5ub3NvZnQuY29tPg==")
;;   (scram-make-security-info nil t 0)
;;   "testsalt"
;;   (scram-md5-make-salted-pass
;;    "secret stuff" "testsalt")))
;; => "U0odqYw3B7XIIW0oSz65OQ=="

;;; Code:

(require 'hmac-md5)

(eval-when-compile
  (require 'scram-md5)
  (require 'digest-md5))

(eval-and-compile
  (autoload 'open-ssl-stream "ssl")
  (autoload 'base64-decode-string "base64")
  (autoload 'base64-encode-string "base64")
  (autoload 'starttls-open-stream "starttls")
  (autoload 'starttls-negotiate "starttls")
  (autoload 'digest-md5-parse-digest-challenge "digest-md5")
  (autoload 'digest-md5-digest-response "digest-md5")
  (autoload 'scram-md5-make-salted-pass "scram-md5")
  (autoload 'scram-md5-parse-server-msg-1 "scram-md5")
  (autoload 'scram-md5-make-client-msg-1 "scram-md5"))

;;; CRAM-MD5
(defun sasl-cram-md5 (username passphrase challenge)
  (let ((secure-word (copy-sequence passphrase)))
    (setq secure-word (unwind-protect
			  (hmac-md5 challenge secure-word)
			(fillarray secure-word 0))
	  secure-word (unwind-protect
			  (encode-hex-string secure-word)
			(fillarray secure-word 0))
	  secure-word (unwind-protect
			  (concat username " " secure-word)
			(fillarray secure-word 0)))))

;;; PLAIN
(defun sasl-plain (authorid authenid passphrase)
  (concat authorid "\0" authenid "\0" passphrase))

;;; SCRAM-MD5
(eval-when-compile
  (defvar sasl-scram-md5-client-security-info
    (scram-make-security-info nil t 0)))

(defun sasl-scram-md5-make-salted-pass (server-msg-1 passphrase)
  (scram-md5-make-salted-pass
   passphrase
   (car
    (scram-md5-parse-server-msg-1 server-msg-1))))

(defun sasl-scram-md5-client-msg-1 (authenticate-id &optional authorize-id)
  (scram-md5-make-client-msg-1 authenticate-id authorize-id))

(defun sasl-scram-md5-client-msg-2 (server-msg-1 client-msg-1 salted-pass)
  (let (client-proof client-key shared-key client-verifier)
    (setq client-key
	  (scram-md5-make-client-key salted-pass))
    (setq client-verifier
	  (scram-md5-make-client-verifier client-key))
    (setq shared-key
	  (unwind-protect
	      (scram-md5-make-shared-key
	       server-msg-1
	       client-msg-1
	       sasl-scram-md5-client-security-info
	       client-verifier)
	    (fillarray client-verifier 0)))
    (setq client-proof
	  (unwind-protect
	      (scram-md5-make-client-proof
	       client-key shared-key)
	    (fillarray client-key 0)
	    (fillarray shared-key 0)))
    (unwind-protect
	(scram-md5-make-client-msg-2
	 sasl-scram-md5-client-security-info
	 client-proof)
      (fillarray client-proof 0))))
	     
(defun sasl-scram-md5-authenticate-server (server-msg-1 
					   server-msg-2
					   client-msg-1
					   salted-pass)
  (string= server-msg-2
	   (scram-md5-make-server-msg-2
	    server-msg-1
	    client-msg-1
	    sasl-scram-md5-client-security-info
	    (car
	     (scram-md5-parse-server-msg-1 server-msg-1))
	    salted-pass)))

;;; DIGEST-MD5

(defvar sasl-digest-md5-nonce-count 1)

(defun sasl-digest-md5-digest-response (digest-challenge username passwd
							 serv-type host &optional realm)
  (digest-md5-parse-digest-challenge digest-challenge)
  (digest-md5-digest-response
   username
   (or realm (digest-md5-challenge 'realm)) ;; need to check.
   passwd
   (digest-md5-challenge 'nonce)
   (digest-md5-cnonce)
   sasl-digest-md5-nonce-count
   (digest-md5-digest-uri serv-type host) ;; MX host
   ))

(provide 'sasl)

;;; sasl.el ends here