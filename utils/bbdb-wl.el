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
  (require 'bbdb-com)
  (defvar bbdb-pop-up-elided-display nil))

(require 'bbdb)

(defvar bbdb-wl-get-update-record-hook nil)
(defvar bbdb-wl-folder-regexp nil)
(defvar bbdb-wl-ignore-folder-regexp nil)

(defvar bbdb-wl-canonicalize-full-name-function
  #'bbdb-wl-canonicalize-spaces-and-dots
  "Way to canonicalize full name.")

(defun bbdb-wl-canonicalize-spaces-and-dots (string)
  (while (and string (string-match "  +\\|[\f\t\n\r\v]+\\|\\." string))
    (setq string (replace-match " " nil t string)))
  (and string (string-match "^ " string)
       (setq string (replace-match "" nil t string)))
  string)

;;;###autoload
(defun bbdb-wl-setup ()
  (add-hook 'wl-message-redisplay-hook 'bbdb-wl-get-update-record)
  (add-hook 'wl-summary-exit-hook 'bbdb-wl-hide-bbdb-buffer)
  (add-hook 'wl-message-window-deleted-hook 'bbdb-wl-hide-bbdb-buffer)
  (add-hook 'wl-exit-hook 'bbdb-wl-exit)
  (add-hook 'wl-save-hook 'bbdb-offer-save)
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
	       (define-key (current-local-map) "\M-\t" 'bbdb-complete-name))))
  (require 'bbdb)
  (bbdb-initialize)

  (if (not (boundp 'bbdb-get-addresses-from-headers))
      (defvar bbdb-get-addresses-from-headers
	'("From" "Resent-From" "Reply-To")))

  (if (not (boundp 'bbdb-get-addresses-to-headers))
      (defvar bbdb-get-addresses-to-headers
	'("Resent-To" "Resent-CC" "To" "CC" "BCC")))

  (if (not (boundp 'bbdb-get-addresses-headers))
      (defvar bbdb-get-addresses-headers
	(append bbdb-get-addresses-from-headers
		bbdb-get-addresses-to-headers))))

(defun bbdb-wl-exit ()
  (let (bbdb-buf)
    (if (setq bbdb-buf (get-buffer bbdb-buffer-name))
	(kill-buffer bbdb-buf)))
  (bbdb-offer-save))

(defun bbdb-wl-get-update-record ()
  (let ((folder-name (with-current-buffer
			 wl-message-buffer-cur-summary-buffer
		       (wl-summary-buffer-folder-name))))
    (if (and (or (null bbdb-wl-folder-regexp)
		 (string-match bbdb-wl-folder-regexp folder-name))
	     (not (and bbdb-wl-ignore-folder-regexp
		       (string-match bbdb-wl-ignore-folder-regexp
				     folder-name))))
	(with-current-buffer (wl-message-get-original-buffer)
	  (bbdb-wl-update-record)
	  (run-hooks 'bbdb-wl-get-update-record-hook)))))

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
			wl-message-buffer)))
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
  "For `wl-summary-get-petname-function'."
  (let* ((address (wl-address-header-extract-address from))
	 (record (bbdb-search-simple nil address)))
    (and record
	 (or (bbdb-record-name record)
	     (car (bbdb-record-name record))))))

(defun bbdb-wl-from-func (string)
  "A candidate From field STRING.  For `wl-summary-from-function'."
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

(defun bbdb-wl-get-addresses-1 (&optional only-first-address)
  "Return real name and email address of sender respectively recipients.
If an address matches `bbdb-user-mail-names' it will be ignored.
The headers to search can be configured by `bbdb-get-addresses-headers'.
For BBDB 2.33 or earlier."
  (save-excursion
    (save-restriction
      (std11-narrow-to-header)
      (let ((headers bbdb-get-addresses-headers)
	    (uninteresting-senders bbdb-user-mail-names)
	    addrlist header structures structure fn ad)
	(while headers
	  (setq header (std11-fetch-field (car headers)))
	  (when header
	    (setq structures (std11-parse-addresses-string
			      (std11-unfold-string header)))
	    (while (and (setq structure (car structures))
			(eq (car structure) 'mailbox))
	      (setq fn (std11-full-name-string structure)
		    fn (and fn
			    (with-temp-buffer ; to keep raw buffer unibyte.
			      (set-buffer-multibyte
			       default-enable-multibyte-characters)
			      (eword-decode-string
			       (decode-mime-charset-string
				fn wl-mime-charset))))
                    fn (funcall bbdb-wl-canonicalize-full-name-function fn)
		    ad (std11-address-string structure))
	      ;; ignore uninteresting addresses, this is kinda gross!
	      (when (or (not (stringp uninteresting-senders))
			(not
			 (or
			  (and fn (string-match uninteresting-senders fn))
			  (and ad (string-match uninteresting-senders ad)))))
		(add-to-list 'addrlist (list fn ad)))
	      (if (and only-first-address addrlist)
		  (setq structures nil headers nil)
		(setq structures (cdr structures)))))
	  (setq headers (cdr headers)))
	(nreverse addrlist)))))

(defun bbdb-wl-get-addresses-2 (&optional only-first-address)
  "Return real name and email address of sender respectively recipients.
If an address matches `bbdb-user-mail-names' it will be ignored.
The headers to search can be configured by `bbdb-get-addresses-headers'.
For BBDB 2.34 or later."
  (save-excursion
    (save-restriction
      (std11-narrow-to-header)
      (let ((headers bbdb-get-addresses-headers)
	    (uninteresting-senders bbdb-user-mail-names)
	    addrlist header structures structure fn ad
	    header-type header-fields header-content)
	(while headers
	  (setq header-type (caar headers)
		header-fields (cdar headers))
	  (while header-fields
	    (setq header-content (std11-fetch-field (car header-fields)))
	    (when header-content
	      (setq structures (std11-parse-addresses-string
				(std11-unfold-string header-content)))
	      (while (and (setq structure (car structures))
			  (eq (car structure) 'mailbox))
                (setq fn (std11-full-name-string structure)
		      fn (and fn
			      (with-temp-buffer ; to keep raw buffer unibyte.
				(set-buffer-multibyte
				 default-enable-multibyte-characters)
				(eword-decode-string
				 (decode-mime-charset-string
				  fn wl-mime-charset))))
		      fn (funcall bbdb-wl-canonicalize-full-name-function fn)
		      ad (std11-address-string structure))
		;; ignore uninteresting addresses, this is kinda gross!
		(when (or (not (stringp uninteresting-senders))
			  (not
			   (or
			    (and fn
				 (string-match uninteresting-senders fn))
			    (and ad
				 (string-match uninteresting-senders ad)))))
		  (add-to-list 'addrlist (list header-type
					       (car header-fields)
					       (list fn ad))))
		(if (and only-first-address addrlist)
		    (setq structures nil headers nil)
		  (setq structures (cdr structures)))))
	    (setq header-fields (cdr header-fields)))
	  (setq headers (cdr headers)))
	(nreverse addrlist)))))

(defun bbdb-wl-get-addresses (&optional only-first-address)
  "Return real name and email address of sender respectively recipients.
If an address matches `bbdb-user-mail-names' it will be ignored.
The headers to search can be configured by `bbdb-get-addresses-headers'."
  (if (string< bbdb-version "2.34")
      (bbdb-wl-get-addresses-1)
    (bbdb-wl-get-addresses-2)))

(defun bbdb-wl-update-record (&optional offer-to-create)
  "Returns the record corresponding to the current WL message,
creating or modifying it as necessary.  A record will be created if
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation."
  (let* ((bbdb-get-only-first-address-p t)
	 (records (bbdb-wl-update-records offer-to-create)))
    (if (and records (listp records))
	(car records)
      records)))

(defun bbdb-wl-update-records (&optional offer-to-create)
  "Returns the records corresponding to the current WL message,
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
		  wl-message-buffer))
	       (intern (format
			"%s-%d"
			wl-current-summary-buffer
			wl-message-buffer-cur-number))))
	    record)
	(or (progn (setq record (bbdb-message-cache-lookup key))
		   (if (listp record) (nth 1 record) record))
	    (static-if (not (fboundp 'bbdb-update-records))
		(let* ((from (or (std11-field-body "From") ""))
		       (addr (and from
				  (nth 1 (std11-extract-address-components
					  from)))))
		  (if (or (null from)
			  (null addr)
			  (string-match (bbdb-user-mail-names) addr))
		      (setq from (or (std11-field-body "To") from)))
		  (with-temp-buffer ; to keep raw buffer unibyte.
		    (set-buffer-multibyte
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
			offer-to-create))))
	      (bbdb-encache-message
	       key
	       (bbdb-update-records (bbdb-wl-get-addresses
				     bbdb-get-only-first-address-p)
				    (or (bbdb-invoke-hook-for-value
					 bbdb/mail-auto-create-p)
					offer-to-create)
				    offer-to-create))))))))

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
  (wl-summary-set-message-buffer-or-redisplay)
  (set-buffer (wl-message-get-original-buffer))
  (let ((record (or (bbdb-wl-update-record t) (error ""))))
    (bbdb-display-records (list record))
    (if arg
	(bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))

(defun bbdb-wl-show-records (&optional headers)
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in `bbdb-mode', with associated keybindings."
  (interactive)
  (wl-summary-set-message-buffer-or-redisplay)
  (set-buffer (wl-message-get-original-buffer))
  (let ((bbdb-get-addresses-headers (or headers bbdb-get-addresses-headers))
	(bbdb-update-records-mode 'annotating)
	(bbdb-message-cache nil)
	(bbdb-user-mail-names nil)
	records bbdb-win)
    (setq records (bbdb-wl-update-records t))
    (if records
	(progn
	  (bbdb-wl-pop-up-bbdb-buffer)
	  (bbdb-display-records (if (listp records) records
				  (list records))))
      (bbdb-undisplay-records))
    (setq bbdb-win (get-buffer-window (get-buffer bbdb-buffer-name)))
    (and bbdb-win
	 (select-window bbdb-win))
    records))

(defun bbdb-wl-address-headers-spec (address-class)
  "Return address headers structure for ADDRESS-CLASS."
  (if (string< bbdb-version "2.34")
      (cond
       ((eq address-class 'recipients)
	bbdb-get-addresses-to-headers)
       ((eq address-class 'authors)
	bbdb-get-addresses-from-headers)
       (t
	(append bbdb-get-addresses-to-headers
		bbdb-get-addresses-from-headers)))
    (list (assoc address-class bbdb-get-addresses-headers))))

(defun bbdb-wl-show-all-recipients ()
  "Show all recipients of this message. Counterpart to `bbdb/vm-show-sender'."
  (interactive)
  (bbdb-wl-show-records (bbdb-wl-address-headers-spec 'recipients)))

(defun bbdb-wl-show-sender (&optional show-recipients)
  "Display the contents of the BBDB for the senders of this message.
With a prefix argument show the recipients instead,
with two prefix arguments show all records.
This buffer will be in `bbdb-mode', with associated keybindings."
  (interactive "p")
  (cond ((= 4 show-recipients)
	 (bbdb-wl-show-all-recipients))
	((= 16 show-recipients)
	 (bbdb-wl-show-records))
	(t
	 (if (null (bbdb-wl-show-records
		    (bbdb-wl-address-headers-spec 'authors)))
	     (bbdb-wl-show-all-recipients)))))

(defun bbdb-wl-pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the WL window(s),
displaying the record corresponding to the sender of the current message."
  (if (get-buffer-window bbdb-buffer-name)
      nil
    (let ((mes-win (get-buffer-window
		    (save-excursion
		      (if (buffer-live-p  wl-current-summary-buffer)
			  (set-buffer wl-current-summary-buffer))
		      wl-message-buffer)))
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
    (let* ((records (static-if (fboundp 'bbdb-update-records)
			(bbdb-wl-update-records offer-to-create)
		      (bbdb-wl-update-record offer-to-create)))
	   ;; BBDB versions v2.33 and later.
	   (bbdb-display-layout
	    (cond ((boundp 'bbdb-pop-up-display-layout)
		   (symbol-value 'bbdb-pop-up-display-layout))
		  ((boundp 'bbdb-pop-up-elided-display)
		   (symbol-value 'bbdb-pop-up-elided-display))))
	   ;; BBDB versions prior to v2.33,
	   (bbdb-elided-display bbdb-display-layout)
	   (b (current-buffer)))
      (bbdb-display-records (if (listp records) records
			      (list records)))
      (set-buffer b)
      records)))

(defun bbdb-wl-send-mail-internal (&optional to subj records)
  (unwind-protect
      (wl-draft (wl-address-header-extract-address to) "" (or subj ""))
    (condition-case nil (delete-other-windows) (error))))

;;; @ bbdb-extract-field-value -- stolen from tm-bbdb.
;;;
(eval-and-compile
  (if (fboundp 'bbdb-wl-extract-field-value-internal)
;;(if (fboundp 'PLEASE_REPLACE_WITH_SEMI-BASED_MIME-BBDB)) ;; mime-bbdb
      nil
    (if (and (string< bbdb-version "1.58")
	     ;;(not (fboundp 'bbdb-extract-field-value) ;; defined as autoload
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
	  (set-buffer-multibyte
	   default-enable-multibyte-characters)
	  (and value
	       (eword-decode-string value)))))
    ))


(provide 'bbdb-wl)

;;; bbdb-wl.el ends here
