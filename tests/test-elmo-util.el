(require 'lunit)
(require 'elmo-util)


(luna-define-class test-elmo-util (lunit-test-case))

;; setup & teardown
(defvar test-elmo-temoporary-file)

(luna-define-method lunit-test-case-setup ((case test-elmo-util))
  (setq test-elmo-temoporary-file
	(make-temp-file temporary-file-directory)))

(luna-define-method lunit-test-case-teardown ((case test-elmo-util))
  (delete-file test-elmo-temoporary-file))


(luna-define-method test-elmo-replace-string-as-filename-1 ((case test-elmo-util))
  (lunit-assert
   (let ((str "/foo//./../bar/"))
     (string= str
	      (elmo-recover-string-from-filename
	       (elmo-replace-string-as-filename str))))))


(luna-define-method test-elmo-object-save-1 ((case test-elmo-util))
  (let ((list '(1 2 3 4 5 6 7 8 9 10 11 12))
	(print-length 1)
	(print-level 1))
    (elmo-object-save test-elmo-temoporary-file list)
    (lunit-assert
     (equal list
	    (elmo-object-load test-elmo-temoporary-file)))))
