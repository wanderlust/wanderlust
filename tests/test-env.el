(require 'lunit)
(require 'wl)

(luna-define-class test-env (lunit-test-case))

;;;
;;; environment test for user (not for developer)
;;;

;; APEL
(luna-define-method test-apel-version ((case test-env))
  (require 'apel-ver)
  (lunit-assert
   (product-version>= (product-find 'apel-ver) '(10 2))))


;; mel-b-ccl on XEmacs 21.1
;;  <http://lists.airs.net/wl/archive/200101/msg00075.html>
(luna-define-method test-base64-encode-1 ((case test-env))
  (require 'elmo-util)
  (lunit-assert
   (string=
    "QQ=="
    (elmo-base64-encode-string "A"))))

(luna-define-method test-base64-encode-2 ((case test-env))
  (require 'elmo-util)
  (lunit-assert
   (string=
    "QUE="
    (elmo-base64-encode-string "AA"))))

(luna-define-method test-base64-encode-3 ((case test-env))
  (require 'elmo-util)
  (lunit-assert
   (string=
    "QUFB"
    (elmo-base64-encode-string "AAA"))))
