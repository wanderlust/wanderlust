(require 'lunit)
(require 'wl)

(luna-define-class check-modules (lunit-test-case))

;;;
;;; environment test for user (not for developer)
;;;

;; APEL
(luna-define-method check-modules-apel-version ((case check-modules))
  (require 'apel-ver)
  (lunit-assert
   (product-version>= (product-find 'apel-ver) '(10 2))))
