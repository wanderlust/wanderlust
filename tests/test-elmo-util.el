(require 'lunit)
(require 'elmo-util)


(luna-define-class test-elmo-util (lunit-test-case))

;; setup & teardown
(defvar test-elmo-temoporary-file)

(luna-define-method lunit-test-case-setup ((case test-elmo-util))
  (setq print-length 1
	print-level 1)
  (setq case-fold-search nil)
  (setq test-elmo-temoporary-file
	(make-temp-file temporary-file-directory)))

(luna-define-method lunit-test-case-teardown ((case test-elmo-util))
  (setq print-length nil
	print-level nil)
  (when (file-exists-p test-elmo-temoporary-file)
    (delete-file test-elmo-temoporary-file)))


(luna-define-method test-elmo-replace-string-as-filename-1 ((case test-elmo-util))
  (lunit-assert
   (let ((str "/foo//./../bar/"))
     (string= str
	      (elmo-recover-string-from-filename
	       (elmo-replace-string-as-filename str))))))


;; object save & load
(luna-define-method test-elmo-object-save-1 ((case test-elmo-util))
  (let ((list '(1 2 3 4 5 6 7 8 9 10 11 12)))
    (elmo-object-save test-elmo-temoporary-file list)
    (lunit-assert
     (equal list
	    (elmo-object-load test-elmo-temoporary-file)))))

(luna-define-method test-elmo-object-save-2 ((case test-elmo-util))
  (let ((list '(1 (2 :foo (nil . :bar)))))
    (elmo-object-save test-elmo-temoporary-file list)
    (lunit-assert
     (equal list
	    (elmo-object-load test-elmo-temoporary-file)))))

(luna-define-method test-elmo-save-string-1 ((case test-elmo-util))
  )

;; list functions
(luna-define-method test-elmo-uniq-list-1 ((case test-elmo-util))
  (lunit-assert
   (eq nil (elmo-uniq-list nil)))
  (lunit-assert
   (equal '(1) (elmo-uniq-list '(1))))
  (lunit-assert
   (equal '(1) (elmo-uniq-list '(1 1))))
  (lunit-assert
   (equal '(1) (elmo-uniq-list '(1 1 1)))))

(luna-define-method test-elmo-uniq-list-2 ((case test-elmo-util))
  (lunit-assert
   (equal '(1 2 3 4 5 6 nil)
	  (elmo-uniq-list '(1 2 3 4 1 5 6 nil nil 1 1 2))))
  (lunit-assert
   (equal '("foo") (elmo-uniq-list '("foo" "foo")))))

(luna-define-method test-elmo-uniq-list-delq ((case test-elmo-util))
  (lunit-assert
   (equal '("foo" "foo") (elmo-uniq-list '("foo" "foo") #'delq)))
  (lunit-assert
   (equal '(:foo) (elmo-uniq-list '(:foo :foo) #'delq))))

(luna-define-method test-elmo-list-insert-1 ((case test-elmo-util))
  (lunit-assert
   (equal '(1 2 3 4 5)
	  (elmo-list-insert '(1 2 3 5) 4 3)))
  (lunit-assert
   (equal '(1 2 3 5 9)
	  (elmo-list-insert '(1 2 3 5) 9 :notfound)))
;;; memq vs. member
;;;   (equal '(1 2 "3" 5 4)
;;;	  (elmo-list-insert '(1 2 "3" 5) 4 "3"))
   )

(luna-define-method test-elmo-list-insert-2 ((case test-elmo-util))
  (let* ((list1 '(1 2 3 4 5))
	 (list2 list1))
    (elmo-list-insert list1 4 3)
    (lunit-assert
     (eq list1 list2))))
;;; memq vs. member
;;;   (equal '(1 2 "3" 5 4)
;;;	  (elmo-list-insert '(1 2 "3" 5) 4 "3"))

(luna-define-method test-elmo-delete-char-1 ((case test-elmo-util))
  (lunit-assert
   (string= "f" (elmo-delete-char ?o "foo")))
  (lunit-assert
   (string= "f\nf" (elmo-delete-char ?o "foo\nfoo")))
  (lunit-assert
   (string= "" (elmo-delete-char ?o  "oo")))
  (lunit-assert
   (string= "" (elmo-delete-char ?o  ""))))

(luna-define-method test-elmo-concat-path-1 ((case test-elmo-util))
  (lunit-assert
   (string=
    "/home/foo"
    (elmo-concat-path "/home" "foo")))
  (lunit-assert
   (string=
    (elmo-concat-path "/home/" "foo")
    (elmo-concat-path "/home//" "foo"))))


(luna-define-method test-elmo-remove-passwd-1 ((case test-elmo-util))
  (let* ((password "cGFzc3dk")
	 (elmo-passwd-alist (list (cons "key" password))))
    (elmo-remove-passwd "key")
    (lunit-assert
     (string= "\0\0\0\0\0\0\0\0" password))
    (lunit-assert
     (null elmo-passwd-alist))))
