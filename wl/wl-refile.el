;;; wl-refile.el -- Refile modules for Wanderlust.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news
;; Time-stamp: <00/03/23 19:07:28 teranisi>

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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

(require 'wl-vars)
(require 'wl-util)
(provide 'wl-refile)


(defvar wl-refile-alist nil)
(defvar wl-refile-alist-file-name "refile-alist")
;; should be renamed to "refile-from-alist"
(defvar wl-refile-msgid-alist nil)
(defvar wl-refile-msgid-alist-file-name "refile-msgid-alist")

(defvar wl-refile-alist-max-length 1000)

(defun wl-refile-alist-setup ()
  (setq wl-refile-alist
	(elmo-object-load
	 (expand-file-name wl-refile-alist-file-name
			   elmo-msgdb-dir)))
  (setq wl-refile-msgid-alist
	(elmo-object-load
	 (expand-file-name wl-refile-msgid-alist-file-name
			   elmo-msgdb-dir))))

(defun wl-refile-alist-save (file-name alist)
  (save-excursion
    (let ((filename (expand-file-name file-name
				      elmo-msgdb-dir))
	  (tmp-buffer (get-buffer-create " *wl-refile-alist-tmp*")))
      (set-buffer tmp-buffer)
      (erase-buffer)
      (if (> (length alist) wl-refile-alist-max-length)
	  (setcdr (nthcdr (1- wl-refile-alist-max-length) alist) nil))
      (prin1 alist tmp-buffer)
      (princ "\n" tmp-buffer)
      (if (file-writable-p filename)
	  (write-region (point-min) (point-max) 
			filename nil 'no-msg)
	(message (format "%s is not writable." filename)))
      (kill-buffer tmp-buffer))))

(defun wl-refile-learn (entity dst)
  (let (tocc-list from key hit ml)
    (setq dst (elmo-string dst))
    (setq tocc-list 
	  (mapcar (function
		   (lambda (entity) 
		     (downcase (wl-address-header-extract-address entity))))
		  (wl-parse-addresses 
		   (concat
		    (elmo-msgdb-overview-entity-get-to entity) ","
		    (elmo-msgdb-overview-entity-get-cc entity)))))
    (while tocc-list
      (if (wl-string-member 
	   (car tocc-list) 
	   (mapcar (function downcase) wl-subscribed-mailing-list))
	  (setq ml (car tocc-list)
		tocc-list nil)
	(setq tocc-list (cdr tocc-list))))
    (if ml
	(setq key ml) ; subscribed entity!!
      (or (wl-address-user-mail-address-p
	   (setq from 
		 (downcase 
		  (wl-address-header-extract-address
		   (elmo-msgdb-overview-entity-get-from 
		    entity)))))
	  (setq key from)))
    (if (not ml)
	(wl-refile-msgid-learn entity dst))
    (if key
	(if (setq hit (assoc key wl-refile-alist))
	    (setcdr hit dst)
	  (setq wl-refile-alist
		(nconc wl-refile-alist (list (cons key dst))))))))

(defun wl-refile-msgid-learn (entity dst)
  (let ((key (elmo-msgdb-overview-entity-get-id entity))
	hit)
    (setq dst (elmo-string dst))
    (if key
	(if (setq hit (assoc key wl-refile-msgid-alist))
	    (setcdr hit dst)
	  (setq wl-refile-msgid-alist (cons (cons key dst)
					    wl-refile-msgid-alist))))))

;;
;; refile guess
;;
(defvar wl-refile-guess-func-list
  '(wl-refile-guess-by-rule
    wl-refile-guess-by-msgid
    wl-refile-guess-by-history)
  "*Functions in this list are used for guessing refile destination folder.")

(defun wl-refile-guess (entity)
  (let ((flist wl-refile-guess-func-list) guess)
    (while flist
      (if (setq guess (funcall (car flist) entity))
	  (setq flist nil)
	(setq flist (cdr flist))))
    guess))

(defun wl-refile-guess-by-rule (entity)
  (let ((rules wl-refile-rule-alist)
	(rule-set) (field) (field-cont))
    (catch 'found
      (while rules
	(setq rule-set (cdr (car rules))
	      field (car (car rules)))
	(cond ((string-match field "From")
	       (setq field-cont
		     (elmo-msgdb-overview-entity-get-from entity)))
	      ((string-match field "Subject")
	       (setq field-cont
		     (elmo-msgdb-overview-entity-get-subject entity)))
	      ((string-match field "To")
	       (setq field-cont
		     (elmo-msgdb-overview-entity-get-to entity)))
	      ((string-match field "Cc")
	       (setq field-cont
		     (elmo-msgdb-overview-entity-get-cc entity)))
	      (t
	       (setq field-cont
		     (elmo-msgdb-overview-entity-get-extra-field
		      entity (downcase field)))))
	(if field-cont
	    (while rule-set
	      (if (string-match (car (car rule-set)) field-cont)
		  (throw 'found (cdr (car rule-set)))
		(setq rule-set (cdr rule-set)))))
	(setq rules (cdr rules))))))

(defun wl-refile-guess-by-history (entity)
  (let ((tocc-list 
	 (mapcar (function
		  (lambda (entity)
		    (downcase (wl-address-header-extract-address entity))))
		 (wl-parse-addresses 
		  (concat
		   (elmo-msgdb-overview-entity-get-to entity) ","
		   (elmo-msgdb-overview-entity-get-cc entity)))))
	ret-val)
    (setq tocc-list (elmo-list-delete
		     (or wl-user-mail-address-list
			 (list (wl-address-header-extract-address wl-from)))
		     tocc-list))
    (while tocc-list
      (if (setq ret-val (cdr (assoc (car tocc-list) wl-refile-alist)))
	  (setq tocc-list nil)
	(setq tocc-list (cdr tocc-list))))
    (or ret-val
	(wl-refile-guess-by-from entity))))

(defun wl-refile-get-account-part-from-address (address)
  (if (string-match "\\([^@]+\\)@[^@]+" address)
      (wl-match-string 1 address)
    address))
		 
(defun wl-refile-guess-by-from (entity)
  (let ((from
	 (downcase (wl-address-header-extract-address
		    (elmo-msgdb-overview-entity-get-from entity)))))
    ;; search from alist
    (or (cdr (assoc from wl-refile-alist))
	(format "%s/%s" wl-refile-default-from-folder 
		(wl-refile-get-account-part-from-address from)))))
  
(defun wl-refile-guess-by-msgid (entity)
  (cdr (assoc (elmo-msgdb-overview-entity-get-references entity)
	      wl-refile-msgid-alist)))

;;; wl-refile.el ends here
