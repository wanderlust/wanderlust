(require 'lunit)
(require 'wl-util)

(luna-define-class test-wl-util (lunit-test-case))

(luna-define-method test-wl-parse-addresses-1 ((case test-wl-util))
  (lunit-assert
   (equal
    '("foo@example.com" "bar@example.com")
    (wl-parse-addresses "foo@example.com, bar@example.com"))))


;; Message-ID
(luna-define-method test-wl-unique-id ((case test-wl-util))
  (lunit-assert
   (not
    (string= (wl-unique-id)
	     (progn (sleep-for 1) (wl-unique-id))))))
