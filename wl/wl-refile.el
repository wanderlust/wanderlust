;;; wl-refile.el -- Refile modules for Wanderlust.

;; Copyright 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

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
(defvar wl-refile-subject-alist nil)
(defvar wl-refile-subject-alist-file-name "refile-subject-alist")

(defvar wl-refile-alist-max-length 1000)

(defun wl-refile-alist-setup ()
  (let ((flist wl-refile-guess-func-list))
    (while flist
      (cond
       ((eq (car flist) 'wl-refile-guess-by-history)
	(setq wl-refile-alist
	      (elmo-object-load
	       (expand-file-name wl-refile-alist-file-name
				 elmo-msgdb-dir) elmo-mime-charset)))
       ((eq (car flist) 'wl-refile-guess-by-msgid)
	(setq wl-refile-msgid-alist
	      (elmo-object-load
	       (expand-file-name wl-refile-msgid-alist-file-name
				 elmo-msgdb-dir) elmo-mime-charset)))
       ((eq (car flist) 'wl-refile-guess-by-subject)
	(setq wl-refile-subject-alist
	      (elmo-object-load
	       (expand-file-name wl-refile-subject-alist-file-name
				 elmo-msgdb-dir) elmo-mime-charset))))
      (setq flist (cdr flist)))))

(defun wl-refile-alist-save ()
  (if wl-refile-alist
      (wl-refile-alist-save-file
       wl-refile-alist-file-name wl-refile-alist))
  (if wl-refile-msgid-alist
      (wl-refile-alist-save-file
       wl-refile-msgid-alist-file-name wl-refile-msgid-alist))
  (if wl-refile-subject-alist
      (wl-refile-alist-save-file
       wl-refile-subject-alist-file-name wl-refile-subject-alist)))

(defun wl-refile-alist-save-file (file-name alist)
  (if (> (length alist) wl-refile-alist-max-length)
      (setcdr (nthcdr (1- wl-refile-alist-max-length) alist) nil))
  (elmo-object-save (expand-file-name file-name elmo-msgdb-dir)
		    alist elmo-mime-charset))

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
	(progn
	  (if (or wl-refile-msgid-alist
		  (member 'wl-refile-guess-by-msgid
			  wl-refile-guess-func-list))
	      (wl-refile-msgid-learn entity dst))
	  (if (or wl-refile-subject-alist
		  (member 'wl-refile-guess-by-subject
			  wl-refile-guess-func-list))
	      (wl-refile-subject-learn entity dst))))
    (when key
      (if (setq hit (assoc key wl-refile-alist))
          (setq wl-refile-alist (delq hit wl-refile-alist)))
      (add-to-list 'wl-refile-alist (cons key dst)))))

(defun wl-refile-msgid-learn (entity dst)
  (let ((key (elmo-msgdb-overview-entity-get-id entity))
	hit)
    (setq dst (elmo-string dst))
    (if key
	(if (setq hit (assoc key wl-refile-msgid-alist))
	    (setcdr hit dst)
	  (setq wl-refile-msgid-alist (cons (cons key dst)
					    wl-refile-msgid-alist))))))

(defun wl-refile-subject-learn (entity dst)
  (let ((subject (wl-summary-subject-filter-func-internal
		  (elmo-msgdb-overview-entity-get-subject entity)))
	hit)
    (setq dst (elmo-string dst))
    (if (and subject (not (string= subject "")))
	(if (setq hit (assoc subject wl-refile-subject-alist))
	    (setcdr hit dst)
	  (setq wl-refile-subject-alist (cons (cons subject dst)
					    wl-refile-subject-alist))))))

;;
;; refile guess
;;
(defvar wl-refile-guess-func-list
  '(wl-refile-guess-by-rule
    wl-refile-guess-by-msgid
    wl-refile-guess-by-subject
    wl-refile-guess-by-history)
  "*Functions in this list are used for guessing refile destination folder.")

(defun wl-refile-guess (entity)
  (let ((flist wl-refile-guess-func-list) guess)
    (while flist
      (if (setq guess (funcall (car flist) entity))
	  (setq flist nil)
	(setq flist (cdr flist))))
    guess))

(defun wl-refile-evaluate-rule (rule entity)
  "Returns folder string if RULE is matched to ENTITY.
If RULE does not match ENTITY, returns nil."
  (let ((case-fold-search t)
	fields guess pairs value)
    (cond 
     ((stringp rule) rule)
     ((listp (car rule))
      (setq fields (car rule))
      (while fields
	(if (setq guess (wl-refile-evaluate-rule (append (list (car fields))
							 (cdr rule))
						 entity))
	    (setq fields nil)
	  (setq fields (cdr fields))))
      guess)
     ((stringp (car rule))
      (setq pairs (cdr rule))
      (setq value (wl-refile-get-field-value entity (car rule)))
      (while pairs
	(if (and (stringp value)
		 (string-match
		  (car (car pairs))
		  value)
		 (setq guess (wl-refile-expand-newtext
			      (wl-refile-evaluate-rule (cdr (car pairs))
						       entity)
			      value)))
	    (setq pairs nil)
	  (setq pairs (cdr pairs))))
      guess)
     (t (error "Invalid structure for wl-refile-rule-alist")))))

(defun wl-refile-get-field-value (entity field)
  "Get FIELD value from ENTITY."
  (let ((field (downcase field))
	(fixed-fields '("from" "subject" "to" "cc")))
    (if (member field fixed-fields)
	(funcall (symbol-function
		  (intern (concat
			   "elmo-msgdb-overview-entity-get-"
			   field)))
		 entity)
      (elmo-msgdb-overview-entity-get-extra-field entity field))))

(defun wl-refile-expand-newtext (newtext original)
  (let ((len (length newtext))
	(pos 0)
	c expanded beg N did-expand)
    (while (< pos len)
      (setq beg pos)
      (while (and (< pos len)
		  (not (= (aref newtext pos) ?\\)))
	(setq pos (1+ pos)))
      (unless (= beg pos)
	(push (substring newtext beg pos) expanded))
      (when (< pos len)
	;; We hit a \; expand it.
	(setq did-expand t
	      pos (1+ pos)
	      c (aref newtext pos))
	(if (not (or (= c ?\&)
		     (and (>= c ?1)
			  (<= c ?9))))
	    ;; \ followed by some character we don't expand.
	    (push (char-to-string c) expanded)
	  ;; \& or \N
	  (if (= c ?\&)
	      (setq N 0)
	    (setq N (- c ?0)))
	  (when (match-beginning N)
	    (push (substring original (match-beginning N) (match-end N))
		  expanded))))
      (setq pos (1+ pos)))
    (if did-expand
	(apply (function concat) (nreverse expanded))
      newtext)))

(defun wl-refile-guess-by-rule (entity)
  (let ((rules wl-refile-rule-alist)
	guess)
    (while rules
      (if (setq guess (wl-refile-evaluate-rule (car rules) entity))
	  (setq rules nil)
	(setq rules (cdr rules))))
    guess))

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

(defun wl-refile-guess-by-subject (entity)
  (cdr (assoc (wl-summary-subject-filter-func-internal
	       (elmo-msgdb-overview-entity-get-subject entity))
	      wl-refile-subject-alist)))

;;; wl-refile.el ends here
