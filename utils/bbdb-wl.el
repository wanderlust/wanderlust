;;; bbdb-wl.el -- BBDB interface to Wanderlust

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, news, database

;;; Commentary:
;;
;;  Insert the following lines in your ~/.wl
;; 
;;  (require 'bbdb-wl)
;;  (bbdb-wl-setup)

;;; Code:
;;

;; bbdb setup.
(eval-when-compile
  (require 'mime-setup)
  (require 'elmo-vars)
  (require 'elmo-util)
  (require 'wl-summary)
  (require 'wl-message)
  (require 'wl-draft)
  (require 'wl-address)
  (defvar bbdb-pop-up-elided-display nil))
;;  (or (fboundp 'bbdb-wl-extract-field-value-internal)
;;      (defun bbdb-wl-extract-field-value-internal (field))))
(require 'bbdb)

(defvar bbdb-wl-get-update-record-hook nil)

(defun bbdb-wl-setup ()
  (add-hook 'wl-message-redisplay-hook 'bbdb-wl-get-update-record)
  (add-hook 'wl-summary-exit-hook 'bbdb-wl-hide-bbdb-buffer)
  (add-hook 'wl-message-window-deleted-hook 'bbdb-wl-hide-bbdb-buffer)
  (add-hook 'wl-exit-hook 'bbdb-wl-exit)
  (add-hook 'wl-summary-toggle-disp-off-hook 'bbdb-wl-hide-bbdb-buffer)
  (add-hook 'wl-summary-toggle-disp-folder-on-hook 'bbdb-wl-hide-bbdb-buffer)
  (add-hook 'wl-summary-toggle-disp-folder-off-hook 'bbdb-wl-hide-bbdb-buffer)
  (add-hook 'wl-summary-toggle-disp-folder-message-resumed-hook
	    'bbdb-wl-show-bbdb-buffer)
  (add-hook 'wl-summary-mode-hook
	    (function
	     (lambda ()
	       (define-key (current-local-map) ":" 'bbdb-wl-show-sender)
	       (define-key (current-local-map) ";" 'bbdb-wl-edit-notes))))
  (add-hook 'wl-summary-exit-hook 'bbdb-flush-all-caches)
  (add-hook 'wl-summary-exec-hook 'bbdb-flush-all-caches)
  (add-hook 'wl-mail-setup-hook
	    (function
	     (lambda ()
;;;	       (local-set-key "\M-\t" 'bbdb-complete-name)
	       (define-key (current-local-map) "\M-\t" 'bbdb-complete-name)
	       ))))

(defun bbdb-wl-exit ()
  (let (bbdb-buf)
    (if (setq bbdb-buf (get-buffer bbdb-buffer-name))
	(kill-buffer bbdb-buf)))
  (bbdb-save-db t))

(defun bbdb-wl-get-update-record ()
  (set-buffer (wl-message-get-original-buffer))
  (bbdb-wl-update-record)
  (run-hooks 'bbdb-wl-get-update-record-hook))

(defun bbdb-wl-hide-bbdb-buffer ()
  (let (bbdb-buf bbdb-win)
    (if (setq bbdb-buf (get-buffer bbdb-buffer-name))
	(if (setq bbdb-win (get-buffer-window bbdb-buf))
	    (delete-window bbdb-win)))))

(defun bbdb-wl-show-bbdb-buffer ()
  (save-selected-window
    (if (get-buffer-window bbdb-buffer-name)
	nil
      (let ((mes-win (get-buffer-window
		      (save-excursion
			(if (buffer-live-p  wl-current-summary-buffer)
			    (set-buffer wl-current-summary-buffer))
			wl-message-buf-name)))
	    (cur-win (selected-window))
	    (b (current-buffer)))
	(and mes-win (select-window mes-win))
	(let ((size (min
		     (- (window-height mes-win)
			window-min-height 1)
		     (- (window-height mes-win)
			(max window-min-height
			     (1+ bbdb-pop-up-target-lines))))))
	  (split-window mes-win (if (> size 0) size window-min-height)))
	;; goto the bottom of the two...
	(select-window (next-window))
	;; make it display *BBDB*...
	(let ((pop-up-windows nil))
	  (switch-to-buffer (get-buffer-create bbdb-buffer-name)))))))

(defun bbdb-wl-get-petname (from)
  "For `wl-summary-get-petname-func'."
  (let* ((address (wl-address-header-extract-address from))
	 (record (bbdb-search-simple nil address)))
    (and record
	 (or (bbdb-record-name record)
	     (car (bbdb-record-name record))))))

(defun bbdb-wl-from-func (string)
  "A candidate From field STRING.  For `wl-summary-from-func'."
  (let ((hit (bbdb-search-simple nil (wl-address-header-extract-address
				      string)))
	first-name last-name from-str)
    (if hit
	(progn
	  (setq first-name (aref hit 0))
	  (setq last-name (aref hit 1))
	  (cond ((and (null first-name)
		      (null last-name))
		 (setq from-str string))
		((and first-name last-name)
		 (setq from-str (concat first-name " " last-name)))
		((or first-name last-name)
		 (setq from-str (or first-name last-name))))
	  from-str)
      string)))

(defun bbdb-wl-update-record (&optional offer-to-create)
  "Returns the record corresponding to the current WL message,
creating or modifying it as necessary.  A record will be created if
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation."
  (save-excursion
    (if bbdb-use-pop-up
	(bbdb-wl-pop-up-bbdb-buffer offer-to-create)
      (let ((key
	     (save-excursion
	       (set-buffer
		(save-excursion
		  (if (buffer-live-p wl-current-summary-buffer)
		      (set-buffer wl-current-summary-buffer))
		  wl-message-buf-name))
	       (intern (format
			"%s-%d"
			wl-current-summary-buffer
			wl-message-buffer-cur-number)))))
	(or (bbdb-message-cache-lookup key nil)
	    (and key
		 (let* ((from (or (std11-field-body "From") ""))
			(addr (and from
				   (nth 1 (std11-extract-address-components
					   from)))))
		   (if (or (null from)
			   (null addr)
			   (string-match (bbdb-user-mail-names) addr))
		       (setq from (or (std11-field-body "To") from)))
		   (with-temp-buffer ; to keep raw buffer unibyte.
		     (elmo-set-buffer-multibyte
		      default-enable-multibyte-characters)
		     (setq from (eword-decode-string
				 (decode-mime-charset-string
				  from
				  wl-mime-charset))))
		   (if from
		       (bbdb-encache-message
			key
			(bbdb-annotate-message-sender
			 from t
			 (or (bbdb-invoke-hook-for-value
			      bbdb/mail-auto-create-p)
			     offer-to-create)
			 offer-to-create))))))))))

(defun bbdb-wl-annotate-sender (string)
  "Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message."
  (interactive (list (if bbdb-readonly-p
			 (error "The Insidious Big Brother Database is read-only")
		       (read-string "Comments: "))))
  (set-buffer (wl-message-get-original-buffer))
  (bbdb-annotate-notes (bbdb-wl-update-record t) string))

(defun bbdb-wl-edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (wl-summary-redisplay)
  (set-buffer (wl-message-get-original-buffer))
  (let ((record (or (bbdb-wl-update-record t) (error ""))))
    (bbdb-display-records (list record))
    (if arg
	(bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))

(defun bbdb-wl-show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in `bbdb-mode', with associated keybindings."
  (interactive)
  (wl-summary-redisplay)
  (set-buffer (wl-message-get-original-buffer))
  (let ((record (bbdb-wl-update-record t))
	bbdb-win)
    (if record
	(progn
	  (bbdb-wl-pop-up-bbdb-buffer)
	  (bbdb-display-records (list record)))
      (error "Unperson"))
    (setq bbdb-win (get-buffer-window (get-buffer bbdb-buffer-name)))
    (and bbdb-win
	 (select-window bbdb-win))))


(defun bbdb-wl-pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the WL window(s),
displaying the record corresponding to the sender of the current message."
  (if (get-buffer-window bbdb-buffer-name)
      nil
    (let ((mes-win (get-buffer-window
		    (save-excursion
		      (if (buffer-live-p  wl-current-summary-buffer)
			  (set-buffer wl-current-summary-buffer))
		      wl-message-buf-name)))
	  (cur-win (selected-window))
	  (b (current-buffer)))
      (and mes-win
	   (select-window mes-win))
      (let ((size (min
		   (- (window-height mes-win)
		      window-min-height 1)
		   (- (window-height mes-win)
		      (max window-min-height
			   (1+ bbdb-pop-up-target-lines))))))
	(split-window mes-win (if (> size 0) size window-min-height)))
      ;; goto the bottom of the two...
      (select-window (next-window))
      ;; make it display *BBDB*...
      (let ((pop-up-windows nil))
	(switch-to-buffer (get-buffer-create bbdb-buffer-name)))
      ;; select the original window we were in...
      (select-window cur-win)
      ;; and make sure the current buffer is correct as well.
      (set-buffer b)))
  (let ((bbdb-gag-messages t)
	(bbdb-use-pop-up nil)
	(bbdb-electric-p nil))
    (let ((record (bbdb-wl-update-record offer-to-create))
	  (bbdb-elided-display (bbdb-pop-up-elided-display))
	  (b (current-buffer)))
      (bbdb-display-records (if record (list record) nil))
      (set-buffer b)
      record)))

(defun bbdb-wl-send-mail-internal (&optional to subj records)
  (unwind-protect
      (wl-draft (wl-address-header-extract-address to) "" (or subj ""))
    (condition-case nil (delete-other-windows) (error))))

;;; @ bbdb-extract-field-value -- stolen from tm-bbdb.
;;;
(and (not (fboundp 'bbdb-wl-extract-field-value-internal))
;;;  (not (fboundp 'PLEASE_REPLACE_WITH_SEMI-BASED_MIME-BBDB)) ;; mime-bbdb
    (progn
      (if (and (string< bbdb-version "1.58")
	       ;; (not (fboundp 'bbdb-extract-field-value) ; defined as autoload
	       (not (fboundp 'bbdb-header-start)))
	  (load "bbdb-hooks")
	(require 'bbdb-hooks))
      (fset 'bbdb-wl-extract-field-value-internal
	    (cond
	     ((fboundp 'tm:bbdb-extract-field-value)
	      (symbol-function 'tm:bbdb-extract-field-value))
	     (t (symbol-function 'bbdb-extract-field-value))))
      (defun bbdb-extract-field-value (field)
	(let ((value (bbdb-wl-extract-field-value-internal field)))
	  (with-temp-buffer ; to keep raw buffer unibyte.
	    (elmo-set-buffer-multibyte
	     default-enable-multibyte-characters)
	    (and value
		 (eword-decode-string value)))))
      ))


(provide 'bbdb-wl)

;;; bbdb-wl.el ends here
