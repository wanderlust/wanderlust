(require 'lunit)

(luna-define-class check-base64 (lunit-test-case))

;; mel-b-ccl on XEmacs 21.1
;;  <http://lists.airs.net/wl/archive/200101/msg00075.html>
(luna-define-method test-base64-encode-1 ((case check-base64))
  (require 'elmo-util)
  (lunit-assert
   (string=
    "QQ=="
    (elmo-base64-encode-string "A"))))

(luna-define-method test-base64-encode-2 ((case check-base64))
  (require 'elmo-util)
  (lunit-assert
   (string=
    "QUE="
    (elmo-base64-encode-string "AA"))))

(luna-define-method test-base64-encode-3 ((case check-base64))
  (require 'elmo-util)
  (lunit-assert
   (string=
    "QUFB"
    (elmo-base64-encode-string "AAA"))))

;; Old base64 module on XEmacs 21.1
;;  <http://lists.airs.net/wl/archive/200104/msg00150.html>
(defun check-base64-encode-string-has-no-line-break-argument ()
  (require 'elmo-util)
  (condition-case nil
      (elmo-base64-encode-string "" t)
    (wrong-number-of-arguments)))

(luna-define-method test-base64-encode-4 ((case check-base64))
  (lunit-assert
   (check-base64-encode-string-has-no-line-break-argument)))

;;  <http://lists.airs.net/wl/archive/200107/msg00121.html>

(defun check-base64-encode-region-has-no-line-break-argument ()
  (require 'mel)
  (condition-case nil
      (with-temp-buffer
	(funcall (mel-find-function 'mime-encode-region "base64")
		 (point-min) (point-max) t)
	t)
    (wrong-number-of-arguments)))

(luna-define-method test-base64-encode-5 ((case check-base64))
  (lunit-assert
   (check-base64-encode-region-has-no-line-break-argument)))
