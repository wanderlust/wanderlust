;;; mmelmo.el -- mm-backend by ELMO.

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

(require 'elmo-vars)
(require 'elmo-util)
(require 'mime-parse)
(require 'mmbuffer)

(defvar mmelmo-header-max-column fill-column
  "*Inserted header is folded with this value.
If function is specified, its return value is used.")

(defvar mmelmo-header-inserted-hook nil
  "*A hook called when header is inserted.")

(defvar mmelmo-entity-content-inserted-hook nil
  "*A hook called when entity-content is inserted.")

(defun mmelmo-get-original-buffer ()
  (let ((ret-val (get-buffer (concat mmelmo-entity-buffer-name "0"))))
    (if (not ret-val)
	(save-excursion
	  (set-buffer (setq ret-val
			    (get-buffer-create 
			     (concat mmelmo-entity-buffer-name "0"))))
	  (mmelmo-original-mode)))
    ret-val))

(defun mmelmo-cleanup-entity-buffers ()
  "Cleanup entity buffers of mmelmo."
  (mapcar (lambda (x)
	    (if (string-match mmelmo-entity-buffer-name x)
		(kill-buffer x)))
	  (mapcar 'buffer-name (buffer-list))))

(defun mmelmo-insert-sorted-header-from-buffer (buffer 
						start end
						&optional invisible-fields
						visible-fields
						sorted-fields)
  (let ((the-buf (current-buffer))
	(mode-obj (mime-find-field-presentation-method 'wide))
	field-decoder
	f-b p f-e field-name field field-body
        vf-alist (sl sorted-fields))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (re-search-forward std11-field-head-regexp nil t)
	  (setq f-b (match-beginning 0)
		p (match-end 0)
		field-name (buffer-substring f-b p)
		f-e (std11-field-end))
	  (when (mime-visible-field-p field-name
				      visible-fields invisible-fields)
	    (setq field (intern
			 (capitalize (buffer-substring f-b (1- p))))
		  field-body (buffer-substring p f-e)
		  field-decoder (inline (mime-find-field-decoder-internal
					 field mode-obj)))
            (setq vf-alist (append (list
                                    (cons field-name
                                          (list field-body field-decoder)))
                                   vf-alist))))
        (and vf-alist
             (setq vf-alist
                   (sort vf-alist
                         (function (lambda (s d)
                                     (let ((n 0) re
                                           (sf (car s))
                                           (df (car d)))
                                       (catch 'done
                                         (while (setq re (nth n sl))
                                           (setq n (1+ n))
                                           (and (string-match re sf)
                                                (throw 'done t))
                                           (and (string-match re df)
                                                (throw 'done nil)))
                                         t)))))))
        (with-current-buffer the-buf
          (while vf-alist
            (let* ((vf (car vf-alist))
                   (field-name (car vf))
                   (field-body (car (cdr vf)))
                   (field-decoder (car (cdr (cdr vf)))))
              (insert field-name)
	      (insert (if field-decoder
			  (funcall field-decoder field-body
                                   (string-width field-name)
				   (if (functionp mmelmo-header-max-column)
				       (funcall mmelmo-header-max-column)
				     mmelmo-header-max-column))
			;; Don't decode
			field-body))
              (insert "\n"))
            (setq vf-alist (cdr vf-alist)))
	  (run-hooks 'mmelmo-header-inserted-hook))))))

(defun mmelmo-original-mode ()
  (setq major-mode 'mmelmo-original-mode)
  (setq buffer-read-only t)
  (elmo-set-buffer-multibyte nil)
  (setq mode-name "MMELMO-Original"))

;; For FLIMs without rfc2231 feature .
(if (not (fboundp 'mime-parse-parameters-from-list))
    (defun mime-parse-parameters-from-list (attrlist)
      (let (ret-val)
	(if (not (eq (% (length attrlist) 2) 0))
	    (message "Invalid attributes."))
	(while attrlist
	  (setq ret-val (append ret-val
				(list (cons (downcase (car attrlist))
					    (downcase (car (cdr attrlist)))))))
	  (setq attrlist (cdr (cdr attrlist))))
	ret-val)))

(provide 'mmelmo) ; for circular dependency.
(static-if (fboundp 'luna-define-method)
    ;; FLIM 1.13 or later
    (require 'mmelmo-2)
  ;; FLIM 1.12
  (require 'mmelmo-1))


;;; mmelmo.el ends here
