;;; elmo-net.el -- Network module for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;; 

(require 'luna)
(require 'elmo-util)
(require 'elmo-vars)

(eval-and-compile
  (autoload 'starttls-negotiate "starttls")
  (autoload 'sasl-find-mechanism "sasl")
  (autoload 'sasl-make-client "sasl")
  (autoload 'sasl-mechanism-name "sasl")
  (autoload 'sasl-next-step "sasl")
  (autoload 'sasl-step-data "sasl")
  (autoload 'sasl-step-set-data "sasl"))

(defvar sasl-mechanisms)

;;; Code:
;;
(eval-and-compile
  (luna-define-class elmo-network-session () (name
					      host
					      port
					      user
					      auth
					      stream-type
					      process
					      greeting))
  (luna-define-internal-accessors 'elmo-network-session))

(luna-define-generic elmo-network-initialize-session (session)
  "Initialize SESSION (Called before authentication).")

(luna-define-generic elmo-network-initialize-session-buffer (session buffer)
  "Initialize SESSION's BUFFER.")

(luna-define-generic elmo-network-authenticate-session (session)
  "Authenticate SESSION.")

(luna-define-generic elmo-network-setup-session (session)
  "Setup SESSION. (Called after authentication).")

(luna-define-generic elmo-network-close-session (session)
  "Close SESSION.")

(luna-define-method
  elmo-network-initialize-session-buffer ((session
					   elmo-network-session) buffer)
  (with-current-buffer buffer
    (elmo-set-buffer-multibyte nil)
    (buffer-disable-undo (current-buffer))))

(luna-define-method elmo-network-close-session ((session elmo-network-session))
  (when (elmo-network-session-process-internal session)
;;; (memq (process-status (elmo-network-session-process-internal session))
;;;	  '(open run))
    (kill-buffer (process-buffer
		  (elmo-network-session-process-internal session)))
    (delete-process (elmo-network-session-process-internal session))))

(defmacro elmo-network-stream-type-spec-string (stream-type)
  (` (nth 0 (, stream-type))))

(defmacro elmo-network-stream-type-symbol (stream-type)
  (` (nth 1 (, stream-type))))

(defmacro elmo-network-stream-type-feature (stream-type)
  (` (nth 2 (, stream-type))))

(defmacro elmo-network-stream-type-function (stream-type)
  (` (nth 3 (, stream-type))))

(defsubst elmo-network-session-password-key (session)
  (format "%s:%s/%s@%s:%d"
	  (elmo-network-session-name-internal session)
	  (elmo-network-session-user-internal session)
	  (symbol-name (or (elmo-network-session-auth-internal session)
			   'plain))
	  (elmo-network-session-host-internal session)
	  (elmo-network-session-port-internal session)))

(defvar elmo-network-session-cache nil)
(defvar elmo-network-session-name-prefix nil)

(defsubst elmo-network-session-cache-key (name host port user auth stream-type)
  "Returns session cache key."
  (format "%s:%s/%s@%s:%d%s"
	  (concat elmo-network-session-name-prefix name)
	  user auth host port (or stream-type "")))

(defun elmo-network-clear-session-cache ()
  "Clear session cache."
  (interactive)
  (mapcar (lambda (pair)
	    (elmo-network-close-session (cdr pair)))
	  elmo-network-session-cache)
  (setq elmo-network-session-cache nil))

(defmacro elmo-network-session-buffer (session)
  "Get buffer for SESSION."
  (` (process-buffer (elmo-network-session-process-internal
		      (, session)))))

(defun elmo-network-get-session (class name host port user auth stream-type
				       &optional if-exists)
  "Get network session from session cache or a new network session.
CLASS is the class name of the session.
NAME is the name of the process.
HOST is the name of the server host.
PORT is the port number of the service.
USER is the user-id for the authenticate.
AUTH is the authenticate method name (symbol).
STREAM-TYPE is the stream type (See also `elmo-network-stream-type-alist').
Returns a `elmo-network-session' instance.
If optional argument IF-EXISTS is non-nil, it does not return session
if there is no session cache.
if making session failed, returns nil."
  (let (pair session key)
    (if (not (elmo-plugged-p host port))
	(error "Unplugged"))
    (setq pair (assoc (setq key (elmo-network-session-cache-key
				 name host port user auth stream-type))
		      elmo-network-session-cache))
    (when (and pair
	       (not (memq (process-status
			   (elmo-network-session-process-internal
			    (cdr pair)))
			  '(open run))))
      (setq elmo-network-session-cache
	    (delq pair elmo-network-session-cache))
      (elmo-network-close-session (cdr pair))
      (setq pair nil))
    (if pair
	(cdr pair)			; connection cache exists.
      (unless if-exists
	(setq session
	      (elmo-network-open-session class name
					 host port user auth stream-type))
	(setq elmo-network-session-cache
	      (cons (cons key session)
		    elmo-network-session-cache))
	session))))

(defun elmo-network-open-session (class name host port user auth
					stream-type)
  "Open an authenticated network session.
CLASS is the class name of the session.
NAME is the name of the process.
HOST is the name of the server host.
PORT is the port number of the service.
USER is the user-id for the authenticate.
AUTH is the authenticate method name (symbol).
STREAM-TYPE is the stream type (See also `elmo-network-stream-type-alist').
Returns a process object.  if making session failed, returns nil."
  (let ((session
	 (luna-make-entity class
			   :name name
			   :host host
			   :port port
			   :user user
			   :auth auth
			   :stream-type stream-type
			   :process nil
			   :greeting nil))
	(buffer (format " *%s session for %s@%s:%d%s"
			(concat elmo-network-session-name-prefix name)
			user
			host
			port
			(or (elmo-network-stream-type-spec-string stream-type)
			    "")))
	process)
    (condition-case error
	(progn
	  (if (get-buffer buffer) (kill-buffer buffer))
	  (setq buffer (get-buffer-create buffer))
	  (elmo-network-initialize-session-buffer session buffer)
	  (elmo-network-session-set-process-internal
	   session
	   (setq process (elmo-open-network-stream
			  (elmo-network-session-name-internal session)
			  buffer host port stream-type)))
	  (when process
	    (elmo-network-initialize-session session)
	    (elmo-network-authenticate-session session)
	    (elmo-network-setup-session session)))
      (error
       (when (eq (car error) 'elmo-authenticate-error)
	 (elmo-remove-passwd (elmo-network-session-password-key session)))
       (elmo-network-close-session session)
       (signal (car error)(cdr error))))
    session))

(defun elmo-open-network-stream (name buffer host service stream-type)
  (let ((auto-plugged (and elmo-auto-change-plugged
			   (> elmo-auto-change-plugged 0)))
	process)
    (if (and stream-type
	     (elmo-network-stream-type-feature stream-type))
	(require (elmo-network-stream-type-feature stream-type)))
    (condition-case err
 	(let (process-connection-type)
	  (as-binary-process
	   (setq process
		 (if stream-type
		     (funcall (elmo-network-stream-type-function stream-type)
			      name buffer host service)
		   (open-network-stream name buffer host service)))))
      (error
       (when auto-plugged
	 (elmo-set-plugged nil host service (current-time))
	 (message "Auto plugged off at %s:%d" host service)
	 (sit-for 1))
       (signal (car err) (cdr err))))
    (when process
      (process-kill-without-query process)
      (when auto-plugged
	(elmo-set-plugged t host service))
      process)))

(require 'product)
(product-provide (provide 'elmo-net) (require 'elmo-version))

;;; elmo-net.el ends here
