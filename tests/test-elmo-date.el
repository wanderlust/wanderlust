(require 'lunit)
(require 'elmo-date)

(luna-define-class test-elmo-date (lunit-test-case))

(luna-define-method test-elmo-date-get-week ((case test-elmo-date))
  "Check around singularity date. leap year and 2038-01-19."
  (let ((elmo-lang "en"))
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
