(require 'lunit)
(require 'elmo-localdir)

(luna-define-class test-elmo-localdir (lunit-test-case))

(luna-define-method test-elmo-folder-expand-msgdb-path-1 ((case test-elmo-localdir))
  (lunit-assert
   (string=
    (elmo-folder-expand-msgdb-path
     (wl-folder-get-elmo-folder
      (concat "+" (expand-file-name "~/Mail/inbox"))))
    (elmo-folder-expand-msgdb-path
     (wl-folder-get-elmo-folder "+~/Mail/inbox")))))

(luna-define-method test-elmo-folder-expand-msgdb-path-2 ((case test-elmo-localdir))
  (lunit-assert
   (string=
    (elmo-folder-expand-msgdb-path (wl-folder-get-elmo-folder "+inbox"))
    (expand-file-name "localdir/inbox" elmo-msgdb-directory))))


(luna-define-method test-elmo-folder-expand-msgdb-path-3 ((case test-elmo-localdir))
  (lunit-assert
   (not
    (string=
     (elmo-folder-expand-msgdb-path (wl-folder-get-elmo-folder "+/inbox"))
     (elmo-folder-expand-msgdb-path (wl-folder-get-elmo-folder "+inbox"))))))
