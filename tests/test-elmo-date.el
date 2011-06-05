;; -*- lexical-binding: t -*-
(require 'lunit)
(require 'elmo-date)

(luna-define-class test-elmo-date (lunit-test-case))

(luna-define-method test-elmo-date-get-week ((case test-elmo-date))
  "Check around singularity date. leap year and 2038-01-19."
  (let ((elmo-lang "en"))
    (lunit-assert
     (string= "Fri" (elmo-date-get-week 1582 10 15)))
    (lunit-assert
     (string= "Tue" (elmo-date-get-week 2000 2 29)))
    (lunit-assert
     (string= "Tue" (elmo-date-get-week 2038 1 19)))
    (lunit-assert
     (string= "Wed" (elmo-date-get-week 2038 1 20)))
    (lunit-assert
     (string= "Sun" (elmo-date-get-week 2100 2 28)))
    (lunit-assert
     (string= "Mon" (elmo-date-get-week 2100 3 1)))))

(luna-define-method test-elmo-time-parse-date-string-1 ((case test-elmo-date))
  ""
  (lunit-assert
   ;; [RFC5322] Appendix A.1.1.
   (equal '(13429 44762)
	  (elmo-time-parse-date-string
	   "Date: Fri, 21 Nov 1997 09:55:06 -0600")))
  (lunit-assert
   ;; [RFC5322] Appendix A.1.2.
   (equal '(16129 19413)
	  (elmo-time-parse-date-string
	   "Date: Tue, 1 Jul 2003 10:52:37 +0200")))
  ;; (lunit-assert
  ;;  ;; leapsec
  ;;  (elmo-time-parse-date-string
  ;;   "Date: Thu, 1 Jan 2009 08:59:60 +0900"))


  (lunit-assert
   ;; [RFC5322] Appendix A.1.3.
   (equal '(-424 63838)
	  (elmo-time-parse-date-string
	   "Date: Thu, 13 Feb 1969 23:32:54 -0330")))
)

(luna-define-method test-elmo-time-parse-date-string-2 ((case test-elmo-date))
  "Obsolete Date: format"
  (lunit-assert
   ;; [RFC5322] Appendix A.5.
   (equal '(14403 4992)
	  (elmo-time-parse-date-string
	   (concat
	    "Date: Thu,\n"
	    "      13\n"
	    "        Feb\n"
	    "          1969\n"
	    "        23:32\n"
	    "                 -0330 (Newfoundland Time)"))))
  (lunit-assert
   ;; [RFC5322] Appendix A.6.2.
   (equal '(13429 23162)
	  (elmo-time-parse-date-string
	   "Date: 21 Nov 97 09:55:06 GMT")))
  (lunit-assert
   ;; [RFC5322] Appendix A.6.3.
   (equal '(13428 52452)
	  (elmo-time-parse-date-string
	   "Date  : Fri, 21 Nov 1997 09(comment):   55  :  06 -0600"))))
