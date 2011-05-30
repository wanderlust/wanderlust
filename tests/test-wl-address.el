;; -*- lexical-binding: t -*-
(require 'lunit)
(require 'wl-address)

(luna-define-class test-wl-address (lunit-test-case))

(luna-define-method test-wl-address-header-extract-address-1 ((case test-wl-address))
  (lunit-assert
   (string=
    "m-sakura@example.org"
    (wl-address-header-extract-address "Mine Sakurai <m-sakura@example.org>")
    )))

(luna-define-method test-wl-address-header-extract-address-2 ((case test-wl-address))
  (lunit-assert
   (string=
    "m-sakura@example.org"
    (wl-address-header-extract-address "m-sakura@example.org (Mine Sakurai)")
    )))
