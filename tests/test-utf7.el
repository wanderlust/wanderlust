;; -*- coding: iso-2022-jp -*-
(require 'lunit)

;; Emacs 21
(unless (and (fboundp 'find-coding-system) (find-coding-system 'utf-16))
  (ignore-errors (require 'un-define)))
(require 'utf7)

;; Emacs 21.3.50 to 22
(when (fboundp 'utf-translate-cjk-mode)
  (utf-translate-cjk-mode 1))

(luna-define-class test-utf7 (lunit-test-case))

(luna-define-method test-utf7-encode-string ((case test-utf7))
  (lunit-assert
   (string=
    "+ZeVnLIqe-"
    (utf7-encode-string "日本語"))))	; FIXME!!: don't care coding system

(luna-define-method test-utf7-encode-string-plus ((case test-utf7))
  (lunit-assert
   (string= "+-" (utf7-encode-string "+"))))

(luna-define-method test-utf7-encode-string-noconv ((case test-utf7))
  (lunit-assert
   (string= "" (utf7-encode-string "")))
  (lunit-assert
   (string= "a" (utf7-encode-string "a")))
  (lunit-assert
   (string= "-" (utf7-encode-string "-")))
  (lunit-assert
   (string= "=" (utf7-encode-string "="))))


(luna-define-method test-utf7-decode-string ((case test-utf7))
  (lunit-assert
   (string=
    "日本語"				; FIXME!!: don't care coding system
    (utf7-decode-string "+ZeVnLIqe-"))))

(luna-define-method test-utf7-decode-string-plus ((case test-utf7))
  (lunit-assert
   (string= "+" (utf7-decode-string "+-")))
  (lunit-assert
   (string= "++" (utf7-decode-string "+-+-")))
  (lunit-assert
   (string= "+++" (utf7-decode-string "+-+-+-")))
  (lunit-assert
   (string= "++++" (utf7-decode-string "+-+-+-+-"))))

(luna-define-method test-utf7-decode-string-noconv ((case test-utf7))
  (lunit-assert
   (string= "" (utf7-decode-string "")))
  (lunit-assert
   (string= "a" (utf7-decode-string "a")))
  (lunit-assert
   (string= "-" (utf7-decode-string "-")))
  (lunit-assert
   (string= "=" (utf7-encode-string "="))))
