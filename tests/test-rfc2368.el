(require 'lunit)
(require 'rfc2368)

(luna-define-class test-rfc2368 (lunit-test-case))

(luna-define-method test-rfc2368-parse-mailto-url-1 ((case test-rfc2368))
  "To field only."
  (lunit-assert
   (equal
    '(("To" . "chris@example.com"))
    (rfc2368-parse-mailto-url "mailto:chris@example.com"))))

(luna-define-method test-rfc2368-parse-mailto-url-2 ((case test-rfc2368))
  "RFC2368 example in 2. Syntax of a mailto URL "
  (lunit-assert
   (equal
    (rfc2368-parse-mailto-url "mailto:addr1%2C%20addr2")
    (rfc2368-parse-mailto-url "mailto:?to=addr1%2C%20addr2")))
  (lunit-assert
   (equal
    (rfc2368-parse-mailto-url "mailto:?to=addr1%2C%20addr2")
    (rfc2368-parse-mailto-url "mailto:addr1?to=addr2"))))

(luna-define-method test-rfc2368-parse-mailto-url-3 ((case test-rfc2368))
  "With Subject field."
  (lunit-assert
   (equal
    '(("To" . "infobot@example.com")
      ("Subject" . "current-issue"))
    (rfc2368-parse-mailto-url
     "mailto:infobot@example.com?subject=current-issue"))))

(luna-define-method test-rfc2368-parse-mailto-url-4 ((case test-rfc2368))
  "Space in Subject field."
  (lunit-assert
   (equal
    '(("To" . "infobot@example.com")
      ("Body" . "send current-issue"))
    (rfc2368-parse-mailto-url
     "mailto:infobot@example.com?body=send%20current-issue"))))

(luna-define-method test-rfc2368-parse-mailto-url-5 ((case test-rfc2368))
  "CRLF in body."
  (lunit-assert
   (equal
    '(("To" . "infobot@example.com")
      ("Body" . "send current-issue\nsend index"))
    (rfc2368-parse-mailto-url
     (concat "mailto:infobot@example.com?body=send%20current-\n"
	     "issue%0D%0Asend%20index")))))
