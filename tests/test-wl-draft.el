;; -*- lexical-binding: t -*-
(require 'lunit)
(require 'wl-draft)

(luna-define-class test-wl-draft (lunit-test-case))

(luna-define-method test-wl-draft-deduce-address-list-1 ((case test-wl-draft))
  (lunit-assert
   (equal
    '("foo@example.com" "bar@example.com")
    (with-temp-buffer
      (insert "To: foo@example.com, bar@example.com
")
      (wl-draft-deduce-address-list (current-buffer) (point-min) (point-max))))))

(luna-define-method test-wl-draft-deduce-address-list-2 ((case test-wl-draft))
  (lunit-assert
   (equal
    '("foo@example.com" "bar@example.com")
    (with-temp-buffer
      (insert "To: foo@example.com
Cc: bar@example.com
")
      (wl-draft-deduce-address-list (current-buffer) (point-min) (point-max))))))

;; RFC 2822 A.1.2. Different types of mailboxes
(luna-define-method test-wl-draft-deduce-address-list-rfc2822-example-1 ((case test-wl-draft))
  (lunit-assert
   (equal
    '("mary@x.test" "jdoe@example.org" "one@y.test" "boss@nil.test" "sysservices@example.net")
    (with-temp-buffer
      (insert "From: \"Joe Q. Public\" <john.q.public@example.com>
To: Mary Smith <mary@x.test>, jdoe@example.org, Who? <one@y.test>
Cc: <boss@nil.test>, \"Giant; \\\"Big\\\" Box\" <sysservices@example.net>
Date: Tue, 1 Jul 2003 10:52:37 +0200
Message-ID: <5678.21-Nov-1997@example.com>
")
      (wl-draft-deduce-address-list (current-buffer) (point-min) (point-max))))))

;; RFC 2822 A.1.3. Group addresses
(luna-define-method test-wl-draft-deduce-address-list-rfc2822-example-2 ((case test-wl-draft))
  (lunit-assert
   (equal
    '("c@a.test" "joe@where.test" "jdoe@one.test")
    (with-temp-buffer
      (insert "From: Pete <pete@silly.example>
To: A Group:Chris Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;
Cc: Undisclosed recipients:;
Date: Thu, 13 Feb 1969 23:32:54 -0330
Message-ID: <testabcd.1234@silly.example>
")
      (wl-draft-deduce-address-list (current-buffer) (point-min) (point-max))))))

;; RFC 2822 A.3. Resent messages
(luna-define-method test-wl-draft-deduce-address-list-rfc2822-example-3 ((case test-wl-draft))
  (lunit-assert
   (equal
    '("j-brown@other.example")
    (with-temp-buffer
      (insert "Resent-From: Mary Smith <mary@example.net>
Resent-To: Jane Brown <j-brown@other.example>
Resent-Date: Mon, 24 Nov 1997 14:22:01 -0800
Resent-Message-ID: <78910@example.net>
From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: Fri, 21 Nov 1997 09:55:06 -0600
Message-ID: <1234@local.machine.example>
")
      (wl-draft-deduce-address-list (current-buffer) (point-min) (point-max))))))

;; obs-phrase
(luna-define-method test-wl-draft-deduce-address-list-rfc2822-obs-1 ((case test-wl-draft))
  (lunit-assert
   (equal
    '("foo@example.com")
    (with-temp-buffer
      (insert "To: foo.bar <foo@example.com>
")
      (wl-draft-deduce-address-list (current-buffer) (point-min) (point-max))))))

;; multiple occurrences of destination address fields
(luna-define-method test-wl-draft-deduce-address-list-rfc2822-obs-2 ((case test-wl-draft))
  (lunit-assert
   (equal
    '("foo@example.com" "bar@example.com")
    (with-temp-buffer
      (insert "To: foo@example.com
To: bar@example.com
")
      (wl-draft-deduce-address-list (current-buffer) (point-min) (point-max))))))
