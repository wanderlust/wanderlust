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

(luna-define-method test-wl-unique-id-by-user ((case test-wl-util))
  (lunit-assert
   (let (user-login-name)
     (not
      (string= (progn (setq user-login-name "_alice") (wl-unique-id))
	       (progn (setq user-login-name "_bob") (wl-unique-id)))))))


(luna-define-method test-wl-inverse-alist ((case test-wl-util))
  (lunit-assert
   (equal
    '((cc c) (aa a))
    (wl-inverse-alist '(a c) '((a . aa) (a . bb) (c . cc))))))

(luna-define-method test-wl-delete-duplicates-1 ((case test-wl-util))
  (lunit-assert
   (equal '("foo@example.com" "bar@example.com" "foo <foo@example.com>")
	  (let ((list '("foo@example.com"
			"bar@example.com"
			"foo <foo@example.com>"
			"foo@example.com"
			"bar@example.com")))
	    (wl-delete-duplicates list nil nil)))))

(luna-define-method test-wl-delete-duplicates-2 ((case test-wl-util))
  (lunit-assert
   (equal '("foo@example.com" "bar@example.com")
	  (let ((list '("foo@example.com"
			"bar@example.com"
			"foo <foo@example.com>"
			"foo@example.com"
			"bar@example.com")))
	    (wl-delete-duplicates list nil t)))))

(luna-define-method test-wl-delete-duplicates-3 ((case test-wl-util))
  (lunit-assert
   (equal '("foo <foo@example.com>")
	  (let ((list '("foo@example.com"
			"bar@example.com"
			"foo <foo@example.com>"
			"foo@example.com"
			"bar@example.com")))
	    (wl-delete-duplicates list t nil)))))

(luna-define-method test-wl-delete-duplicates-4 ((case test-wl-util))
  (lunit-assert
   (equal '("bar@example.com")
	  (let ((list '("foo@example.com"
			"bar@example.com"
			"foo <foo@example.com>"
			"foo@example.com")))
	    (wl-delete-duplicates list t t)))))
