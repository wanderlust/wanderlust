(require 'lunit)
(require 'elmo-util)

(luna-define-class test-elmo-util (lunit-test-case))

(luna-define-method test-elmo-replace-string-as-filename-1 ((case test-elmo-util))
  (lunit-assert
   (let ((str "/foo//./../bar/"))
     (string= str
	      (elmo-recover-string-from-filename
	       (elmo-replace-string-as-filename str))))))
