(require 'lunit)
(require 'elmo-util)

(luna-define-class test-elmo-util (lunit-test-case))

;; setup & teardown
(defvar test-elmo-temoporary-file)

(luna-define-method lunit-test-case-setup ((case test-elmo-util))
  (setq test-elmo-temoporary-file
	(make-temp-file temporary-file-directory)))

(luna-define-method lunit-test-case-teardown ((case test-elmo-util))
  (when (file-exists-p test-elmo-temoporary-file)
    (delete-file test-elmo-temoporary-file)))


(luna-define-method test-elmo-replace-string-as-filename-1 ((case test-elmo-util))
  "Revert checking replace and recover."
  (lunit-assert
   (let ((str "/foo//./../bar/"))
     (string= str
	      (elmo-recover-string-from-filename
	       (elmo-replace-string-as-filename str))))))


;; object save & load
(luna-define-method test-elmo-object-save-1 ((case test-elmo-util))
  "Check `print-length' let bindings."
  (let ((list '(1 2 3 4 5 6 7 8 9 10 11 12))
	(print-length 1))
    (elmo-object-save test-elmo-temoporary-file list)
    (lunit-assert
     (equal list
	    (elmo-object-load test-elmo-temoporary-file)))))

(luna-define-method test-elmo-object-save-2 ((case test-elmo-util))
  "Check `print-level' let bindings."
  (let ((list '(1 (2 :foo (nil . :bar))))
	(print-level 1))
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

(luna-define-method test-elmo-uniq-list-3 ((case test-elmo-util))
  "Check using DELETE-FUNCTION"
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
  "Check not copied"
  (let* ((list1 '(1 2 3 4 5))
	 (list2 list1))
    (elmo-list-insert list1 4 3)
    (lunit-assert
     (eq list1 list2))))
;;; memq vs. member
;;;   (equal '(1 2 "3" 5 4)
;;;	  (elmo-list-insert '(1 2 "3" 5) 4 "3"))

(defun test-elmo-same-list-p (list1 list2)
  (let ((clist1 (copy-sequence list1))
	(clist2 (copy-sequence list2)))
    (while list2
      (setq clist1 (delq (car list2) clist1))
      (setq list2 (cdr list2)))
    (while list1
      (setq clist2 (delq (car list1) clist2))
      (setq list1 (cdr list1)))
    (equal (list clist1 clist2) '(nil nil))))

(defun test-elmo-same-diff-p (diff1 diff2)
  (and (test-elmo-same-list-p (car diff1)
			      (car diff2))
       (test-elmo-same-list-p (nth 1 diff1)
			      (nth 1 diff2))))

(luna-define-method test-elmo-list-diff ((case test-elmo-util))
  (let ((list1 '(1 2 3))
	(list2 '(1 2 3 4))
	(list3 '(1 2 4))
	(list4 '(4 5 6))
	(list5 '(3 4 5 6)))
    (lunit-assert
     (test-elmo-same-diff-p '(nil nil)
			    (elmo-list-diff nil nil)))
    (lunit-assert
     (test-elmo-same-diff-p '(nil (1 2 3))
			    (elmo-list-diff nil list1)))
    (lunit-assert
     (test-elmo-same-diff-p '((1 2 3) nil)
			    (elmo-list-diff list1 nil)))
    (lunit-assert
     (test-elmo-same-diff-p '(nil nil)
			    (elmo-list-diff list1 list1)))
    (lunit-assert
     (test-elmo-same-diff-p '(nil (4))
			    (elmo-list-diff list1 list2)))
    (lunit-assert
     (test-elmo-same-diff-p '((3) (4))
			    (elmo-list-diff list1 list3)))
    (lunit-assert
     (test-elmo-same-diff-p '((1 2 3) (4 5 6))
			    (elmo-list-diff list1 list4)))
    (lunit-assert
     (test-elmo-same-diff-p '((1 2) (5 6))
			    (elmo-list-diff list3 list4)))
    (lunit-assert
     (test-elmo-same-diff-p '((1 2) (3 5 6))
			    (elmo-list-diff list3 list5)))))

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
  "Check shred password."
  (let* ((password "cGFzc3dk")
	 (elmo-passwd-alist (list (cons "key" password))))
    (elmo-remove-passwd "key")
    (lunit-assert
     (string= "\0\0\0\0\0\0\0\0" password))))

(luna-define-method test-elmo-remove-passwd-2 ((case test-elmo-util))
  "Check remove target pair only.  Not rassoc."
  (let ((password "cGFzc3dk")
	(elmo-passwd-alist '(("foo" . "key")
			     ("key" . "ok")
			     ("bar" . "baz"))))
    (elmo-remove-passwd "key")
    (lunit-assert
     (equal '(("foo" . "key")
	      ("bar" . "baz"))
	    elmo-passwd-alist))))

(luna-define-method test-elmo-remove-passwd-3 ((case test-elmo-util))
  "Multiple same key."
  (let ((password "cGFzc3dk")
	(elmo-passwd-alist '(("foo" . "key")
			     ("key" . "ok")
			     ("key" . "ok2")
			     ("bar" . "baz"))))
    (elmo-remove-passwd "key")
    (lunit-assert
     (equal '(("foo" . "key")
	      ("bar" . "baz"))
	    elmo-passwd-alist))))

(luna-define-method test-elmo-passwd-alist-clear-1 ((case test-elmo-util))
  "Check shred ALL password."
  (let* ((password1 "cGFzc3dk")
	 (password2 (copy-sequence password1))
	 (elmo-passwd-alist (list (cons "key1" password1)
				  (cons "key2" password2))))
    (elmo-passwd-alist-clear)
    (lunit-assert
     (string= "\0\0\0\0\0\0\0\0" password1))
    (lunit-assert
     (string= "\0\0\0\0\0\0\0\0" password2))))

(luna-define-method test-elmo-address-quote-specials-1 ((case test-elmo-util))
  ""
  (lunit-assert
   (string= "\"dot.atom.text\""
	    (elmo-address-quote-specials "dot.atom.text")))
  (lunit-assert
   (string= "\"...\""
	    (elmo-address-quote-specials "..."))))

(luna-define-method test-elmo-address-quote-specials-2 ((case test-elmo-util))
  ""
  (lunit-assert
   (string=
    "atext!#$%&'*+-/=?^_`{|}~"
    (elmo-address-quote-specials "atext!#$%&'*+-/=?^_`{|}~"))))

(luna-define-method test-elmo-elmo-flatten-1 ((case test-elmo-util))
  ""
  (lunit-assert
   (equal
    '(1 2 3 4 5)
    (elmo-flatten '(1 2 (3 4 5))))))

(luna-define-method test-elmo-number-set-member-1 ((case test-elmo-util))
  "Check edge."
  (lunit-assert
   (equal '((1 . 99))
	  (elmo-number-set-member 1 '((1 . 99)))))
  (lunit-assert
   (equal '((1 . 99))
	  (elmo-number-set-member 99 '((1 . 99))))))

(luna-define-method test-elmo-number-set-member-2 ((case test-elmo-util))
  "Check edge."
  (lunit-assert
   (null (elmo-number-set-member 0 '((1 . 99)))))
  (lunit-assert
   (null (elmo-number-set-member 100 '((1 . 99))))))

(luna-define-method test-elmo-number-set-append-list-1 ((case test-elmo-util))
  "Simple testcase."
  (lunit-assert
   (equal '((1 . 100))
	  (elmo-number-set-append-list '((1 . 99)) '(100)))))

(luna-define-method test-elmo-number-set-append-1 ((case test-elmo-util))
  "Simple testcase."
  (lunit-assert
   (equal '((1 . 100))
	  (elmo-number-set-append '((1 . 99)) 100))))

(luna-define-method test-elmo-number-set-delete-1 ((case test-elmo-util))
  "Check edge."
  (lunit-assert
   (equal '(2 3)
	  (elmo-number-set-delete '(1 2 3) 1))))

(luna-define-method test-elmo-number-set-delete-2 ((case test-elmo-util))
  "Normal case."
  (lunit-assert
   (equal '(1 3)
	  (elmo-number-set-delete '(1 2 3) 2))))

(luna-define-method test-elmo-number-set-delete-3 ((case test-elmo-util))
  "Check edge."
  (lunit-assert
   (equal '(1 2)
	  (elmo-number-set-delete '(1 2 3) 3))))

(luna-define-method test-elmo-number-set-delete-4 ((case test-elmo-util))
  "Check edge."
  (lunit-assert
   (equal '((2 . 100))
	  (elmo-number-set-delete '((1 . 100)) 1))))

(luna-define-method test-elmo-number-set-delete-5 ((case test-elmo-util))
  "Check edge."
  (lunit-assert
   (equal '(1 (3 . 100))
	  (elmo-number-set-delete '((1 . 100)) 2))))

(luna-define-method test-elmo-number-set-delete-6 ((case test-elmo-util))
  "Normal case."
  (lunit-assert
   (equal '((1 . 49) (51 . 100))
	  (elmo-number-set-delete '((1 . 100)) 50))))

(luna-define-method test-elmo-number-set-delete-7 ((case test-elmo-util))
  "Check edge."
  (lunit-assert
   (equal '((1 . 98) 100)
	  (elmo-number-set-delete '((1 . 100)) 99))))

(luna-define-method test-elmo-number-set-delete-8 ((case test-elmo-util))
  "Check edge."
  (lunit-assert
   (equal '((1 . 99))
	  (elmo-number-set-delete '((1 . 100)) 100))))

(luna-define-method test-elmo-number-set-delete-list-1 ((case test-elmo-util))
  "Simple testcase."
  (lunit-assert
   (equal '((1 . 99))
	  (elmo-number-set-delete-list '((1 . 100)) '(100)))))

(luna-define-method test-elmo-number-set-to-number-list-1 ((case test-elmo-util))
  "Simple testcase."
  (lunit-assert
   (equal '(1 2 3 4 5 6 7 8 9)
	  (elmo-number-set-to-number-list '((1 . 9)))))
  (lunit-assert
   (equal '(1)
	  (elmo-number-set-to-number-list '(1)))))
