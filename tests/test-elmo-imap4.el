;; -*- lexical-binding: t -*-
(require 'lunit)
(require 'elmo-imap4)

(luna-define-class test-elmo-imap4 (lunit-test-case))

(luna-define-method test-elmo-imap4-parse-greeting-ok ((case test-elmo-imap4))
  (with-temp-buffer
    (setq case-fold-search nil)
    (let (elmo-imap4-status)
      (insert-string "* OK [CAPABILITY IMAP4 IMAP4rev1 LITERAL+ ID"
		     " LOGINDISABLED AUTH=DIGEST-MD5 AUTH=CRAM-MD5 SASL-IR]"
		     " mail.example.org Cyrus IMAP v2.3.13 server ready\n")
      (goto-char (point-min))
      (lunit-assert
       (eq 'nonauth (elmo-imap4-parse-greeting))))))

(luna-define-method test-elmo-imap4-parse-greeting-preauth ((case test-elmo-imap4))
  (with-temp-buffer
    (setq case-fold-search nil)
    (let (elmo-imap4-status)
      (insert-string "* PREAUTH IMAP4rev1 server logged in as Smith\n")
      (goto-char (point-min))
      (lunit-assert
       (eq 'auth (elmo-imap4-parse-greeting))))))

(luna-define-method test-elmo-imap4-parse-greeting-bye ((case test-elmo-imap4))
  (with-temp-buffer
    (setq case-fold-search nil)
    (let (elmo-imap4-status)
      (insert-string "* BYE LOGOUT received\n")
      (goto-char (point-min))
      (lunit-assert
       (eq 'closed (elmo-imap4-parse-greeting))))))
