;; -*- lexical-binding: t -*-
(require 'lunit)
(require 'wl)
(require 'cl)				; mapc

(luna-define-class test-dist (lunit-test-case))

;; WL-MODULES
(defvar test-dist-wl-lost-module-list nil)
(luna-define-method test-wl-modules-exists ((case test-dist))
  (setq test-dist-wl-lost-module nil)
  (mapc
   (lambda (module)
     (let ((filename (concat (symbol-name module) ".el")))
       (unless (file-exists-p (expand-file-name filename WLDIR))
	 (add-to-list 'test-dist-wl-lost-module-list filename))))
   WL-MODULES)
  (lunit-assert (null test-dist-wl-lost-module-list)))

(defvar test-dist-wl-bad-module-list nil)
(luna-define-method test-wl-modules-trailing-whitespace ((case test-dist))
  (setq test-dist-wl-bad-module-list nil)
  (mapc
   (lambda (module)
     (let ((filename (format "%s.el%s" (symbol-name module)
			     (if (eq 'wl-news module) ".in" ""))))
       (with-temp-buffer
	 (insert-file-contents (expand-file-name filename WLDIR))
	 (when (re-search-forward "[ \t]$" nil t)
	   (add-to-list 'test-dist-wl-bad-module-list filename)))))
   WL-MODULES)
  (lunit-assert (null test-dist-wl-bad-module-list)))


;; ELMO-MODULES
(defvar test-dist-elmo-lost-module-list nil)
(luna-define-method test-elmo-modules-exists ((case test-dist))
  (setq test-dist-elmo-lost-module-list nil)
  (mapc
   (lambda (module)
     (let ((filename (concat (symbol-name module) ".el")))
       (unless (file-exists-p (expand-file-name filename ELMODIR))
	 (add-to-list 'test-dist-elmo-lost-module-list filename))))
   ELMO-MODULES)
  (lunit-assert (null test-dist-elmo-lost-module-list)))

(defvar test-dist-elmo-bad-module-list nil)
(luna-define-method test-elmo-modules-trailing-whitespace ((case test-dist))
  (setq test-dist-elmo-bad-module-list nil)
  (mapc
   (lambda (module)
     (let ((filename (format "%s.el" (symbol-name module))))
       (with-temp-buffer
	 (insert-file-contents (expand-file-name filename ELMODIR))
	 (when (re-search-forward "[ \t]$" nil t)
	   (add-to-list 'test-dist-elmo-bad-module-list filename)))))
   ELMO-MODULES)
  (lunit-assert (null test-dist-elmo-bad-module-list)))


;; UTILS-MODULES
(defvar test-dist-util-lost-module-list nil)
(luna-define-method test-util-modules-exists ((case test-dist))
  (setq test-dist-util-lost-module-list nil)
  (mapc
   (lambda (module)
     (let ((filename (concat (symbol-name module) ".el")))
       (unless (file-exists-p (expand-file-name filename UTILSDIR))
	 (add-to-list 'test-dist-util-lost-module-list symbol))))
   UTILS-MODULES)
  (lunit-assert (null test-dist-util-lost-module-list)))

(defvar test-dist-util-bad-module-list nil)
(luna-define-method test-util-modules-trailing-whitespace ((case test-dist))
  (setq test-dist-util-bad-module-list nil)
  (mapc
   (lambda (module)
     (let ((filename (format "%s.el" (symbol-name module))))
       (with-temp-buffer
	 (insert-file-contents (expand-file-name filename UTILSDIR))
	 (when (re-search-forward "[ \t]$" nil t)
	   (add-to-list 'test-dist-util-bad-module-list filename)))))
   UTILS-MODULES)
  (lunit-assert (null test-dist-util-bad-module-list)))


;; Icons
(defvar test-dist-wl-lost-icon-list nil)
(luna-define-method test-wl-icon-exists ((case test-dist))
  (setq test-dist-wl-lost-icon-list nil)
  (mapatoms
   (lambda (symbol)
     (let ((name (symbol-name symbol))
	   (value (and (boundp symbol) (symbol-value symbol))))
       (when (and (string-match "^wl-.*-icon$" name)
		  (stringp value)
		  (string-match "xpm$" value))
	 (unless (file-exists-p (expand-file-name value ICONDIR))
	   (add-to-list 'test-dist-wl-lost-icon-list symbol))))))
  (lunit-assert (null test-dist-wl-lost-icon-list)))

(luna-define-method test-version-status-icon-xpm ((case test-dist))
  (require 'wl-demo)
  (lunit-assert
   (file-exists-p
    (expand-file-name (concat (wl-demo-icon-name) ".xpm") ICONDIR))))

(luna-define-method test-version-status-icon-xbm ((case test-dist))
  (require 'wl-demo)
  (lunit-assert
   (file-exists-p
    (expand-file-name (concat (wl-demo-icon-name) ".xbm") ICONDIR))))

;; verstion.texi
(luna-define-method test-texi-version ((case test-dist))
  (require 'wl-version)
  (lunit-assert
   (string=
    (product-version-string (product-find 'wl-version))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "version.texi" DOCDIR))
      (re-search-forward "^@set VERSION \\([0-9\.]+\\)$")
      (match-string 1)))))

;; version.tex
(luna-define-method test-refcard-version ((case test-dist))
  (require 'wl-version)
  (lunit-assert
   (string=
    (product-version-string (product-find 'wl-version))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "version.tex" DOCDIR))
      (re-search-forward "^\\\\def\\\\versionnumber{\\([0-9\.]+\\)}$")
      (match-string 1)))))

;; wl/ChangeLog
(luna-define-method test-version-wl-changelog ((case test-dist))
  (require 'wl-version)
  (lunit-assert
   (string=
    (product-version-string (product-find 'wl-version))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "ChangeLog" WLDIR))
      (re-search-forward
       "^\t\\* Version number is increased to \\([0-9\\.]+[0-9]\\).$")
      (match-string 1)))))

;; elmo/ChangeLog
(luna-define-method test-version-elmo-changelog ((case test-dist))
  (require 'elmo-version)
  (lunit-assert
   (string=
    (product-version-string (product-find 'elmo-version))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "ChangeLog" ELMODIR))
      (re-search-forward
       "^\t\\* elmo-version.el (elmo-version): Up to \\([0-9\\.]+[0-9]\\).$")
      (match-string 1)))))

;; ChangeLog (toplevel)
(luna-define-method test-version-toplevel-changelog ((case test-dist))
  (require 'wl-version)
  (when (and (string= (wl-version-status) "stable")
	     ;; pre release version don't check.
	     (not (string-match
		   "pre"
		   (product-code-name (product-find 'wl-version)))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "ChangeLog" "./"))
      (re-search-forward
       "^\t\\* \\([0-9\\.]+\\) - \"\\([^\"]+\\)\"$")
      (lunit-assert
       (string=
	(product-version-string (product-find 'wl-version))
	(match-string 1)))
      (lunit-assert
       (string=
	(product-code-name (product-find 'wl-version))
	(match-string 2))))))

;; README, README.ja (toplevel)
(luna-define-method test-version-readme ((case test-dist))
  (require 'wl-version)
  (when (string= (wl-version-status) "stable")
    (mapc
     (lambda (file)
       (with-temp-buffer
	 (insert-file-contents (expand-file-name file "./"))
	 (re-search-forward "checkout -r wl-\\([0-9]+\\)_\\([0-9]+\\) wanderlust")
	 (lunit-assert
	  (= (string-to-number (match-string 1))
	     (nth 0 (product-version (product-find 'wl-version)))))
	 (lunit-assert
	  (= (string-to-number (match-string 2))
	     (nth 1 (product-version (product-find 'wl-version)))))))
     '("README" "README.ja"))))

;; copyright notice
(luna-define-method test-wl-demo-copyright-notice ((case test-dist))
  (require 'wl-demo)
  (lunit-assert
   (string-match
    (format-time-string "%Y" (current-time))
    wl-demo-copyright-notice)))
