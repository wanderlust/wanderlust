;; -*- lexical-binding: t -*-
(require 'lunit)

;; Emacs 21
(unless (and (fboundp 'find-coding-system) (find-coding-system 'utf-16))
  (ignore-errors (require 'un-define)))
(require 'utf7)

;; Emacs 21.3.50 to 22
(when (fboundp 'utf-translate-cjk-mode)
  (utf-translate-cjk-mode 1))

(luna-define-class test-utf7 (lunit-test-case))

(luna-define-method test-utf7-encode-string-nihongo ((case test-utf7))
  (lunit-assert
   (string=
    "+ZeVnLIqe-"
    (utf7-encode-string
     (string (make-char 'japanese-jisx0208 70 124)
	     (make-char 'japanese-jisx0208 75 92)
	     (make-char 'japanese-jisx0208 56 108))))))

(luna-define-method test-utf7-encode-string-smiling-face ((case test-utf7))
  (lunit-assert
   (string=
    "Hi Mom -+Jjo--!"
    (utf7-encode-string
     (concat "Hi Mom -"
	     (string (make-char 'mule-unicode-2500-33ff 35 58))
	     "-!")))))

(luna-define-method test-utf7-encode-string-alpha ((case test-utf7))
  (lunit-assert
   (string=
    "A+ImIDkQ-."
    (utf7-encode-string
     (concat "A"
	     (string (make-char 'mule-unicode-0100-24ff 121 34)
		     (make-char 'mule-unicode-0100-24ff 38 113))
	     ".")))))

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


(luna-define-method test-utf7-decode-string-nihongo ((case test-utf7))
  (lunit-assert
   (string=
    (string (make-char 'japanese-jisx0208 70 124)
	    (make-char 'japanese-jisx0208 75 92)
	    (make-char 'japanese-jisx0208 56 108))
    (utf7-decode-string "+ZeVnLIqe-"))))

(luna-define-method test-utf7-decode-string-smiling-face ((case test-utf7))
  (lunit-assert
   (string=
    (concat "Hi Mom -"
	    (string (make-char 'mule-unicode-2500-33ff 35 58))
	    "-!")
    (utf7-decode-string "Hi Mom -+Jjo--!"))))

(luna-define-method test-utf7-decode-string-alpha ((case test-utf7))
  (lunit-assert
   (string=
    (concat "A"
	    (string (make-char 'mule-unicode-0100-24ff 121 34)
		    (make-char 'mule-unicode-0100-24ff 38 113))
	    ".")
    (utf7-decode-string "A+ImIDkQ.")))	; omit `-'
  ;;
  (lunit-assert
   (string=
    (concat "A"
	    (string (make-char 'mule-unicode-0100-24ff 121 34)
		    (make-char 'mule-unicode-0100-24ff 38 113))
	    ".")
    (utf7-decode-string "A+ImIDkQ-."))))

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
