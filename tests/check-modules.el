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

;; X-Face on XEmacs
(luna-define-method check-modules-x-face-xmas ((case check-modules))
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
