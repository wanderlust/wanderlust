(require 'lunit)
(require 'utf7)

;; Emacs 21.3.50 or later
(if (boundp 'utf-translate-cjk-mode)
    (utf-translate-cjk-mode 1)
  ;; Use Mule-UCS if installed
  (ignore-errors (require 'un-define)))

(luna-define-class test-utf7 (lunit-test-case))

(luna-define-method test-utf7-encode-string ((case test-utf7))
  (lunit-assert
   (string=
    "+ZeVnLIqe-"
    (utf7-encode-string "日本語"))))	; FIXME!!: don't care coding system

(luna-define-method test-utf7-decode-string ((case test-utf7))
  (lunit-assert
   (string=
    "日本語"				; FIXME!!: don't care coding system
    (utf7-decode-string "+ZeVnLIqe-"))))
