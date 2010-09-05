;;; ssl.el,v --- ssl functions for emacsen without them builtin
;; Author: wmperry
;; Created: 1999/10/14 12:44:18
;; Version: 1.2
;; Keywords: comm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1995, 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl))
(require 'base64)

(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      `(devar ,var ,value ,doc))))

(defgroup ssl nil
  "Support for `Secure Sockets Layer' encryption."
  :group 'comm)

(defcustom ssl-certificate-directory "~/.w3/certs/"
  "*Directory to store CA certificates in"
  :group 'ssl
  :type 'directory)

(defcustom ssl-rehash-program-name "c_rehash"
  "*Program to run after adding a cert to a directory .
Run with one argument, the directory name."
  :group 'ssl
  :type 'string)

(defcustom ssl-view-certificate-program-name "x509"
  "*The program to run to provide a human-readable view of a certificate."
  :group 'ssl
  :type 'string)

(defcustom ssl-view-certificate-program-arguments '("-text" "-inform" "DER")
  "*Arguments that should be passed to the certificate viewing program.
The certificate is piped to it.
Maybe a way of passing a file should be implemented"
  :group 'ssl
  :type 'list)

(defcustom ssl-certificate-directory-style 'ssleay
  "*Style of cert database to use, the only valid value right now is `ssleay'.
This means a directory of pem encoded certificates with hash symlinks."
  :group 'ssl
  :type '(choice (const :tag "SSLeay" :value ssleay)
		 (const :tag "OpenSSL" :value openssl)))

(defcustom ssl-certificate-verification-policy 0
  "*How far up the certificate chain we should verify."
  :group 'ssl
  :type '(choice (const :tag "No verification" :value 0)
		 (const :tag "Verification required" :value 1)
		 (const :tag "Reject connection if verification fails" :value 3)
		 (const :tag "SSL_VERIFY_CLIENT_ONCE" :value 5)))

(defcustom ssl-program-name "openssl"
  "*The program to run in a subprocess to open an SSL connection."
  :group 'ssl
  :type 'string)

(defcustom ssl-program-arguments
  '("s_client"
    "-quiet"
    "-host" host
    "-port" service
    "-verify" (int-to-string ssl-certificate-verification-policy)
    "-CApath" ssl-certificate-directory
    )
  "*Arguments that should be passed to the program `ssl-program-name'.
This should be used if your SSL program needs command line switches to
specify any behaviour (certificate file locations, etc).
The special symbols 'host and 'port may be used in the list of arguments
and will be replaced with the hostname and service/port that will be connected
to."
  :group 'ssl
  :type 'list)

(defun ssl-certificate-information (der)
  "Return an assoc list of information about a certificate in DER format."
  (let ((certificate (concat "-----BEGIN CERTIFICATE-----\n"
			     (base64-encode-string der)
			     "\n-----END CERTIFICATE-----\n"))
	(exit-code 0))
    (save-excursion
      (set-buffer (get-buffer-create " *openssl*"))
      (erase-buffer)
      (insert certificate)
      (setq exit-code (condition-case ()
			  (call-process-region (point-min) (point-max)
					       ssl-program-name
					       t (list (current-buffer) nil) t
					       "x509"
					       "-subject" ; Print the subject DN
					       "-issuer" ; Print the issuer DN
					       "-dates" ; Both before and after dates
					       "-serial" ; print out serial number
					       "-noout" ; Don't spit out the certificate
					       )
			(error -1)))
      (if (/= exit-code 0)
	  nil
	(let ((vals nil))
	  (goto-char (point-min))
	  (while (re-search-forward "^\\([^=\n\r]+\\)\\s *=\\s *\\(.*\\)" nil t)
	    (push (cons (match-string 1) (match-string 2)) vals))
	  vals)))))

(defun ssl-accept-ca-certificate ()
  "Ask if the user is willing to accept a new CA certificate. The buffer-name
should be the intended name of the certificate, and the buffer should probably
be in DER encoding"
  ;; TODO, check if it is really new or if we already know it
  (let* ((process-connection-type nil)
	 (tmpbuf (generate-new-buffer "X509 CA Certificate Information"))
	 (response (save-excursion
		     (and (eq 0
			      (apply 'call-process-region
				     (point-min) (point-max)
				     ssl-view-certificate-program-name
				     nil tmpbuf t
				     ssl-view-certificate-program-arguments))
			  (switch-to-buffer tmpbuf)
			  (goto-char (point-min))
			  (or (recenter) t)
			  (yes-or-no-p
			   "Accept this CA to vouch for secure server identities? ")
			  (kill-buffer tmpbuf)))))
    (if (not response)
	nil
      (if (not (file-directory-p ssl-certificate-directory))
	  (make-directory ssl-certificate-directory))
      (case ssl-certificate-directory-style
	(ssleay
	 (base64-encode-region (point-min) (point-max))
	 (goto-char (point-min))
	 (insert "-----BEGIN CERTIFICATE-----\n")
	 (goto-char (point-max))
	 (insert "-----END CERTIFICATE-----\n")
	 (let ((f (expand-file-name
		   (concat (file-name-sans-extension (buffer-name)) ".pem")
		   ssl-certificate-directory)))
	   (write-file f)
	   (call-process ssl-rehash-program-name
			 nil nil nil
			 (expand-file-name ssl-certificate-directory))))))))

(defun open-ssl-stream (name buffer host service)
  "Open a SSL connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
specifying a port number to connect to."
  (if (integerp service) (setq service (int-to-string service)))
  (let* ((process-connection-type nil)
	 (port service)
	 (proc (eval
		`(start-process name buffer ssl-program-name
				,@ssl-program-arguments))))
    (process-kill-without-query proc)
    proc))

(provide 'ssl)
