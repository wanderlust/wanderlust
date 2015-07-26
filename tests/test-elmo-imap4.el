(require 'ert)
(require 'elmo-imap4)

(ert-deftest test-elmo-imap4-parse-greeting/ok ()
  (should (eq 'nonauth (with-temp-buffer
                         (let (case-fold-search)
                           (insert-string "* OK [CAPABILITY IMAP4 IMAP4rev1 LITERAL+ ID"
                                          " LOGINDISABLED AUTH=DIGEST-MD5 AUTH=CRAM-MD5 SASL-IR]"
                                          " mail.example.org Cyrus IMAP v2.3.13 server ready\n")
                           (goto-char (point-min))
                           (elmo-imap4-parse-greeting))))))

(ert-deftest test-elmo-imap4-parse-greeting/preauth ()
  (should (eq 'auth (with-temp-buffer
                      (let (case-fold-search)
                        (insert-string "* PREAUTH IMAP4rev1 server logged in as Smith\n")
                        (goto-char (point-min))
                        (elmo-imap4-parse-greeting))))))

(ert-deftest test-elmo-imap4-parse-greeting/bye ()
  (should (eq 'closed (with-temp-buffer
                        (let (case-fold-search)
                          (insert-string "* BYE LOGOUT received\n")
                          (goto-char (point-min))
                          (elmo-imap4-parse-greeting))))))

(ert-deftest test-elmo-imap4-flatten-command/unknown-token-type ()
  (should
   (string-match-p
    "^Invalid token type:"
    (error-message-string
     (should-error (elmo-imap4-flatten-command '((invalid))))))))

(ert-deftest test-elmo-imap4-flatten-command/unknown-command-token ()
  (should
   (string-match-p
    "^Invalid command token:"
    (error-message-string
     (should-error (elmo-imap4-flatten-command '(3)))))))

(ert-deftest test-elmo-imap4-flatten-command/ok ()
  (let ((cmd (elmo-imap4-flatten-command (list "a" (list 'atom "b") (list 'quoted "c") (list 'literal "d") "e f"))))
    (should (string= (nth 0 cmd) "a b \"c\""))
    (should (listp (nth 1 cmd)))
    (should (string= (nth 2 cmd) "e f"))))

(ert-deftest test-elmo-imap4-flatten-command/parens ()
  (should (string= "a (b c) d" (nth 0 (elmo-imap4-flatten-command (list "a" "(" "b" "c" ")" "d")))))
  (should (string= "a (" (nth 0 (elmo-imap4-flatten-command (list "a" "(" (list 'literal "b") ")" "c")))))
  (should (string= ") c" (nth 2 (elmo-imap4-flatten-command (list "a" "(" (list 'literal "b") ")" "c"))))))
