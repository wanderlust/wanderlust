(require 'lunit)
(require 'wl)

(luna-define-class check-modules (lunit-test-case))

;;;
;;; environment test for user (not for developer)
;;;

;; APEL
(luna-define-method test-apel-version ((case check-modules))
  (require 'apel-ver)
  (lunit-assert
   (product-version>= (product-find 'apel-ver) '(10 3))))

(luna-define-method test-modules-charsets-mime-charset-alist ((case check-modules))
  (require 'mcharset)
  (lunit-assert
   (not (memq nil charsets-mime-charset-alist))))

;; X-Face on XEmacs
(luna-define-method test-x-face-xmas ((case check-modules))
  (when (and (locate-library "x-face") (featurep 'xemacs))
    (lunit-assert
     (check-modules-x-face-xmas-wl-display-x-face-was-argument-required))))

(defun check-modules-x-face-xmas-wl-display-x-face-was-argument-required ()
  "When `x-face-xmas-wl-display-x-face' has non-optional argument, return nil."
  (require 'x-face-xmas)
  (condition-case nil
      (with-temp-buffer
	(x-face-xmas-wl-display-x-face)
	t)
    (wrong-number-of-arguments)))

;; x-face-mule
(luna-define-method test-x-face-mule ((case check-modules))
  (when (and (locate-library "x-face-mule")
	     (not (featurep 'xemacs))
	     (not (locate-library "x-face-e21")))
    (require 'x-face-mule)
    (lunit-assert (fboundp 'x-face-decode-message-header))
    (lunit-assert (check-modules-x-face-decode-message-header))))

(defun check-modules-x-face-decode-message-header ()
  "When `x-face-decode-message-header' has non-optional argument, return nil."
  (require 'x-face-mule)
  (condition-case nil
      (with-temp-buffer
	(x-face-decode-message-header)
	t)
    (wrong-number-of-arguments)))

;; MIME entity (FLIM API Version 1.14 Draft Release 3)
(luna-define-method test-flim-mime-entity ((case check-modules))
  (require 'mime)
  (lunit-assert (fboundp 'mime-open-entity)) ; [Required]<Suggest>
  (lunit-assert (fboundp 'mime-entity-children)) ; [Required]<Suggest>
  (lunit-assert (fboundp 'mime-entity-fetch-field)) ; [Required]<Suggest>
  (lunit-assert (fboundp 'mime-insert-text-content)) ; [Required]
  (lunit-assert (boundp 'default-mime-charset)) ; [Required]
  (lunit-assert (fboundp 'mime-entity-content)) ; [Required]
  (lunit-assert (fboundp 'mime-write-entity-content)) ; [Required]
  (lunit-assert (fboundp 'mime-insert-entity)) ; [Required]
  (lunit-assert (fboundp 'mime-write-entity)) ; [Required]
  (lunit-assert (fboundp 'mime-insert-entity-body)) ; [Required]
  (lunit-assert (fboundp 'mime-write-entity-body))) ; [Required]

;; MIME content information (FLIM API Version 1.14 Draft Release 3)
(luna-define-method test-flim-mime-content-information ((case check-modules))
  (require 'mime)
  (lunit-assert (fboundp 'mime-content-type-primary-type)) ; [Required]
  (lunit-assert (fboundp 'mime-content-type-subtype)) ; [Required]
  (lunit-assert (fboundp 'mime-content-type-parameter)) ; [Required]
  (lunit-assert (fboundp 'mime-content-disposition-type)) ; [Required]
  (lunit-assert (fboundp 'mime-content-disposition-parameter))) ; [Required]

;; encoded-word (FLIM API Version 1.14 Draft Release 3)
(luna-define-method test-flim-encoded-word ((case check-modules))
  (require 'mime)
  (lunit-assert (fboundp 'mime-decode-field-body)) ; [Required]<Suggest>
  (lunit-assert (fboundp 'mime-encode-field-body))) ; [Required]<Suggest>

;; Content-Transfer-Encoding (FLIM API Version 1.14 Draft Release 3)
(luna-define-method test-flim-content-transfer-encoding ((case check-modules))
  (require 'mel)
  ;; mime-decode-string (string encoding) [Required]<Suggest>
  (lunit-assert (fboundp 'mime-decode-string))
  (lunit-assert (fboundp (mel-find-function 'mime-decode-string "base64")))
  ;; mime-encode-string (string encoding)  [Required]<Suggest>
  (lunit-assert (fboundp (mel-find-function 'mime-encode-string "base64")))

  (lunit-assert (fboundp 'base64-decode-string)) ; [Required]
  (lunit-assert (fboundp 'base64-encode-string)) ; [Required]
  (lunit-assert (fboundp 'mime-write-decoded-region)) ; [Required]<Suggest>
  (lunit-assert (fboundp 'mime-insert-encoded-file)) ; [Required]<Suggest>
  (lunit-assert (fboundp 'binary-write-decoded-region)) ; [Required]
  (lunit-assert (fboundp 'binary-insert-encoded-file))) ; [Required]

;; Mailcap (FLIM API Version 1.14 Draft Release 3)
(luna-define-method test-flim-mailcap ((case check-modules))
  (require 'mime-conf)
  (lunit-assert (fboundp 'mime-parse-mailcap-buffer)) ; [Required]<Suggest>
  (lunit-assert (boundp 'mime-mailcap-file)) ; [Required]<Suggest>
  (lunit-assert (fboundp 'mime-parse-mailcap-file)) ; [Required]<Suggest>
  (lunit-assert (fboundp 'mime-format-mailcap-command))) ; [Required]<Suggest>

;; STD 11 (FLIM API Version 1.14 Draft Release 3)
(luna-define-method test-flim-std11 ((case check-modules))
  (require 'std11)
  (lunit-assert (fboundp 'std11-narrow-to-header)) ; [Required]
  (lunit-assert (fboundp 'std11-fetch-field)) ; [Required]
  (lunit-assert (fboundp 'std11-field-body)) ; [Required]
  (lunit-assert (fboundp 'std11-unfold-string))) ; [Required]

;; SMTP (FLIM API Version 1.14 Draft Release 3)
(luna-define-method test-flim-smtp ((case check-modules))
  (require 'smtp)
  (lunit-assert (fboundp 'smtp-send-buffer))) ; [Suggest]


;; SEMI
(luna-define-method test-semi-mime-edit ((case check-modules))
  (require 'mime-edit)
  (lunit-assert (fboundp 'mime-find-file-type))
  (lunit-assert (fboundp 'mime-edit-content-beginning))
  (lunit-assert (fboundp 'mime-edit-content-end))
  (lunit-assert (fboundp 'mime-edit-preview-message))
  (lunit-assert (fboundp 'mime-create-tag))
  (lunit-assert (fboundp 'mime-make-text-tag)))

(luna-define-method test-semi-mime-view ((case check-modules))
  (require 'mime-view)
  (lunit-assert (fboundp 'mime-display-message))
  (lunit-assert (fboundp 'mime-maybe-hide-echo-buffer))
  (lunit-assert (fboundp 'mime-preview-original-major-mode))
  (lunit-assert (fboundp 'mime-preview-follow-current-entity))
  (lunit-assert (fboundp 'mime-view-mode))
  (lunit-assert (fboundp 'mime-display-text/plain))
  (lunit-assert (fboundp 'mime-entity-situation)))

(luna-define-method test-semi-mime-play ((case check-modules))
  (require 'mime-play)
  (lunit-assert (fboundp 'mime-store-message/partial-piece)))

;; emacs-w3m >= 1.3.4
(luna-define-method test-shimbun ((case check-modules))
  (when (locate-library "shimbun")
    (require 'shimbun)
    (lunit-assert (fboundp 'shimbun-server))))

;; spamfilter.el >= 0.10
(luna-define-method test-spamfilter ((case check-modules))
  (when (locate-library "spamfilter")
    (require 'spamfilter)
    (lunit-assert (fboundp 'spamf-corpus-name))))
