(require 'lunit)
(require 'wl)
(require 'cl)				; mapc

(luna-define-class test-dist (lunit-test-case))

;; WL-MODULES
(luna-define-method test-wl-modules-exists ((case test-dist))
  (lunit-assert
   (null
    (let (filename lost)
      (mapc
       (lambda (module)
	 (setq filename (concat (symbol-name module) ".el"))
	 (unless (file-exists-p (expand-file-name filename WLDIR))
	   (add-to-list 'lost symbol)))
       WL-MODULES)
      lost))))

;; ELMO-MODULES
(luna-define-method test-elmo-modules-exists ((case test-dist))
  (lunit-assert
   (null
    (let (filename lost)
      (mapc
       (lambda (module)
	 (setq filename (concat (symbol-name module) ".el"))
	 (unless (file-exists-p (expand-file-name filename ELMODIR))
	   (add-to-list 'lost symbol)))
       ELMO-MODULES)
      lost))))

;; UTILS-MODULES
(luna-define-method test-util-modules-exists ((case test-dist))
  (lunit-assert
   (null
    (let (filename lost)
      (mapc
       (lambda (module)
	 (setq filename (concat (symbol-name module) ".el"))
	 (unless (file-exists-p (expand-file-name filename UTILSDIR))
	   (add-to-list 'lost symbol)))
       UTILS-MODULES)
      lost))))

;; Icons
(luna-define-method test-wl-icon-exists ((case test-dist))
  (lunit-assert
   (null
    (let (name value lost)
      (mapatoms
       (lambda (symbol)
	 (setq name (symbol-name symbol))
	 (setq value (and (boundp symbol) (symbol-value symbol)))
	 (when (and (string-match "^wl-.*-icon$" name)
		    (stringp value)
		    (string-match "xpm$" value))
	   (unless (file-exists-p (expand-file-name value ICONDIR))
	     (add-to-list 'lost symbol)))))
      lost))))

(luna-define-method test-version-status-icon-xpm ((case test-dist))
  (require 'wl-demo)
  (lunit-assert
   (file-exists-p
    (expand-file-name (concat wl-demo-icon-name ".xpm") ICONDIR))))

(luna-define-method test-version-status-icon-xbm ((case test-dist))
  (require 'wl-demo)
  (lunit-assert
   (file-exists-p
    (expand-file-name (concat wl-demo-icon-name ".xbm") ICONDIR))))

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

;; wl-refcard.tex
(luna-define-method test-refcard-version ((case test-dist))
  (require 'wl-version)
  (lunit-assert
   (string=
    (product-version-string (product-find 'wl-version))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "wl-refcard.tex" DOCDIR))
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
       "\\* \\([0-9\\.]+\\) - \"\\([^\"]+\\)\".$")
      (lunit-assert
       (string=
	(product-version-string (product-find 'wl-version))
	(match-string 1)))
      (lunit-assert
       (string=
	(product-code-name (product-find 'wl-version))
	(match-string 2))))))
