(require 'lunit)
(require 'utf7)

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
