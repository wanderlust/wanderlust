;;; mmelmo-1.el -- mm-backend (for FLIM 1.12.x) by ELMO.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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

;;; Code:
;; 

(require 'mime)
(require 'mime-parse)
(eval-when-compile
  (require 'std11))

(require 'mmelmo)

(defvar mmelmo-force-reload nil)
(defvar mmelmo-sort-field-list nil)

;;; mmelmo: Only the initialization method is different from mmbuffer.
(mm-define-backend elmo (buffer))

(mm-define-method initialize-instance ((entity elmo))
  (mime-entity-set-buffer-internal 
   entity
   (get-buffer-create (concat mmelmo-entity-buffer-name "0")))
    (save-excursion
      (set-buffer (mime-entity-buffer-internal entity))
      (mmelmo-original-mode)
      (let ((buffer-read-only nil)
	    (location (mime-entity-location-internal entity))
	    header-start header-end body-start body-end)
	(erase-buffer)
	(setq mime-message-structure entity)
	(elmo-read-msg-with-buffer-cache (nth 0 location)
					 (nth 1 location)
					 (current-buffer)
					 (nth 2 location)
					 mmelmo-force-reload)
	(setq header-start (point-min))
	(setq body-end (point-max))
	(goto-char header-start)
	(if (re-search-forward 
	     (concat "^" (regexp-quote mail-header-separator) "$\\|^$" )
	     nil t)
	    (setq header-end (match-beginning 0)
		  body-start (if (= header-end body-end)
				 body-end
			       (1+ (match-end 0))))
	  (setq header-end (point-min)
		body-start (point-min)))
	(save-restriction
	  (narrow-to-region header-start header-end)
	  (mime-entity-set-content-type-internal
	   entity
	   (let ((str (std11-fetch-field "Content-Type")))
	     (if str
		 (mime-parse-Content-Type str)
	       )))
	  )
	(mime-entity-set-header-start-internal entity header-start)
	(mime-entity-set-header-end-internal entity header-end)
	(mime-entity-set-body-start-internal entity body-start)
	(mime-entity-set-body-end-internal entity body-end)
	)))

(mm-define-method insert-header ((entity elmo)
				 &optional invisible-fields visible-fields)
  (mmelmo-insert-sorted-header-from-buffer
   (mime-entity-buffer entity)
   (mime-entity-header-start-internal entity)
   (mime-entity-header-end-internal entity)
   invisible-fields visible-fields))

(mm-define-method insert-text-content ((entity elmo))
  (insert
   (decode-mime-charset-string (mime-entity-content entity)
                               (or (mime-content-type-parameter
                                    (mime-entity-content-type entity)
                                    "charset")
                                   default-mime-charset)
                               'CRLF))
  (run-hooks 'mmelmo-entity-content-inserted-hook))

(provide 'mmelmo-1)

;;; mmelmo-1.el ends here
