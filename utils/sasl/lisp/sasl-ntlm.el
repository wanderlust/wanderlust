;;; sasl-ntlm.el --- NTLM (NT Lan Manager) module for the SASL client framework

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Taro Kawagishi <tarok@transpulse.org>
;; Keywords: SASL, NTLM
;; Version: 1.00
;; Created: February 2001

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a SASL interface layer for NTLM authentication message
;; generation by ntlm.el

;;; Code:

(require 'sasl)
(require 'ntlm)

(defconst sasl-ntlm-steps
  '(ignore				;nothing to do before making
    sasl-ntlm-request			;authentication request
    sasl-ntlm-response)			;response to challenge
  "A list of functions to be called in sequnece for the NTLM
authentication steps.  Ther are called by 'sasl-next-step.")

(defun sasl-ntlm-request (client step)
  "SASL step function to generate a NTLM authentication request to the server.
Called from 'sasl-next-step.
CLIENT is a vector [mechanism user service server sasl-client-properties]
STEP is a vector [<previous step function> <result of previous step function>]"
  (let ((user (sasl-client-name client)))
    (ntlm-build-auth-request user)))

(defun sasl-ntlm-response (client step)
  "SASL step function to generate a NTLM response against the server
challenge stored in the 2nd element of STEP.  Called from 'sasl-next-step."
  (let* ((user (sasl-client-name client))
	 (passphrase
	  (sasl-read-passphrase (format "NTLM passphrase for %s: " user)))
	 (challenge (sasl-step-data step)))
    (ntlm-build-auth-response challenge user
			      (ntlm-get-password-hashes passphrase))))

(put 'sasl-ntlm 'sasl-mechanism
     (sasl-make-mechanism "NTLM" sasl-ntlm-steps))

(provide 'sasl-ntlm)

;;; sasl-ntlm.el ends here
