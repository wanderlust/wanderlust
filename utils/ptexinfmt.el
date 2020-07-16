;;; ptexinfmt.el --- portable Texinfo formatter  -*- lexical-binding: t -*-

;; Copyright (C) 1985, 1986, 1988, 1990, 1991, 1992, 1993,
;;               1994, 1995, 1996, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1999 Yoshiki Hayashi <yoshiki@xemacs.org>
;; Copyright (C) 2000 TAKAHASHI Kaoru <kaoru@kaisei.org>

;; Author: TAKAHASHI Kaoru <kaoru@kaisei.org>
;;	Yoshiki Hayashi <yoshiki@xemacs.org>
;;	Katsumi Yamaoka <yamaoka@jpl.org>
;; Maintainer: TAKAHASHI Kaoru <kaoru@kaisei.org>
;; Created: 7 Jul 2000
;; Keywords: maint, tex, docs, emulation, compatibility

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Original code: Yoshiki Hayashi <yoshiki@xemacs.org>
;;	makeinfo.el (gnujdoc project)

;; Support texinfmt.el 2.32 or later.

;; Modified by Yamaoka not to use APEL functions.

;; Unimplemented command:
;;  @abbr{ABBREVIATION}
;;  @float ... @end float, @caption{TEXT}, @shortcaption{TEXT}, @listoffloats
;;  @deftypecv[x]
;;  @headitem
;;  @quotation (optional arguments)
;;  @acronym{ACRONYM[, MEANING]} (optional argument)
;;  @dofirstparagraphindent
;;  @indent
;;  @verbatiminclude FILENAME
;;  @\
;;  @definfoenclose phoo,//,\\
;;  @deftypeivar CLASS DATA-TYPE VARIABLE-NAME
;;  @deftypeop CATEGORY CLASS DATA-TYPE NAME ARGUMENTS...
;;  @allowcodebreaks false
;;  @thischapternum
;;  @quotedblleft @quotedblright
;;  @quoteleft @quoteright  @quotedblbase @quotesinglbase
;;  @guillemetleft @guillemetright @guilsinglleft @guilsinglright.
;;  @clicksequence, @click, @clickstyle, @arrow

;;; Code:

(require 'texinfmt)

;; Work around a problem that double-quotes at bol disappear:
;; @dfn{FOO} => FOO", ``BAR'' => BAR", \BAZ/ => BAZ/
(modify-syntax-entry ?\" "w" texinfo-format-syntax-table)
(modify-syntax-entry ?\\ "w" texinfo-format-syntax-table)

;;; Broken
(defvar ptexinfmt-disable-broken-notice-flag t
  "If non-nil disable notice, when call `ptexinfmt-broken-facility'.
This is last argument in `ptexinfmt-broken-facility'.")

(put 'ptexinfmt-broken-facility 'lisp-indent-function 'defun)
(defmacro ptexinfmt-broken-facility (facility docstring assertion
					      &optional _dummy)
  "Declare a symbol FACILITY is broken if ASSERTION is nil.
DOCSTRING will be printed if ASSERTION is nil and
`ptexinfmt-disable-broken-notice-flag' is nil."
  `(let ((facility ',facility)
	 (docstring ,docstring)
	 (assertion (eval ',assertion)))
     (put facility 'broken (not assertion))
     (if assertion
	 nil
       (put facility 'broken-docstring docstring)
       (if ptexinfmt-disable-broken-notice-flag
	   nil
	 (message "BROKEN FACILITY DETECTED: %s" docstring)))))

(put 'ptexinfmt-defun-if-broken 'lisp-indent-function 'defun)
(defmacro ptexinfmt-defun-if-broken (&rest args)
  "Redefine a function just like `defun' if it is considered broken."
  (let ((name (list 'quote (car args))))
    (setq args (cdr args))
    `(prog1
	 ,name
       (if (get ,name 'broken)
	   (defalias ,name
	     (function (lambda ,@args)))))))

(put 'ptexinfmt-defun-if-void 'lisp-indent-function 'defun)
(defmacro ptexinfmt-defun-if-void (&rest args)
  "Define a function just like `defun' unless it is already defined."
  (let ((name (list 'quote (car args))))
    (setq args (cdr args))
    `(prog1
	 ,name
       (if (fboundp ,name)
	   nil
	 (defalias ,name
	   (function (lambda ,@args)))))))

(put 'ptexinfmt-defvar-if-void 'lisp-indent-function 'defun)
(defmacro ptexinfmt-defvar-if-void (&rest args)
  "Define a variable just like `defvar' unless it is already defined."
  (let ((name (car args)))
    (setq args (cdr args))
    `(prog1
	 (defvar ,name)
       (if (boundp ',name)
	   nil
	 (defvar ,name ,@args)))))

(defvar texinfo-enclosure-list)
(defvar texinfo-alias-list)

;; @uref{URL[, TEXT][, REPLACEMENT]}
(ptexinfmt-broken-facility texinfo-format-uref
  "Parse twice @uref argument."
  (condition-case nil
      (with-temp-buffer
	(let (texinfo-enclosure-list texinfo-alias-list)
	  (texinfo-mode)
	  (insert "@uref{mailto:foo@@noncommand.example.com}\n")
	  (texinfo-format-expand-region (point-min) (point-max))
	  t))
    (error nil)))

;;; Hardcopy and HTML (discard)
;; size
(put 'letterpaper 'texinfo-format 'texinfo-discard-line)
(put 'fonttextsize 'texinfo-format 'texinfo-discard-line-with-args)

;; flags
(put 'frenchspacing 'texinfo-format 'texinfo-discard-line-with-args)

;; @slanted{TEXT} (makeinfo 4.8 or later)
(put 'slanted 'texinfo-format 'texinfo-format-noop)

;; @sansserif{TEXT} (makeinfo 4.8 or later)
(put 'sansserif 'texinfo-format 'texinfo-format-noop)

;; @tie{} (makeinfo 4.3 or later)
(put 'tie 'texinfo-format 'texinfo-format-tie)
(ptexinfmt-defun-if-void texinfo-format-tie ()
  (texinfo-parse-arg-discard)
  (insert " "))


;;; Conditional
;; @ifnotdocbook ... @end ifnotdocbook (makeinfo 4.7 or later)
(put 'ifnotdocbook 'texinfo-format 'texinfo-discard-line)
(put 'ifnotdocbook 'texinfo-end 'texinfo-discard-command)

;; @docbook ... @end docbook (makeinfo 4.7 or later)
(put 'docbook 'texinfo-format 'texinfo-format-docbook)
(ptexinfmt-defun-if-void texinfo-format-docbook ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end docbook[ \t]*\n")
			(point))))

;; @ifdocbook ... @end ifdocbook (makeinfo 4.7 or later)
(put 'ifdocbook 'texinfo-format 'texinfo-format-ifdocbook)
(ptexinfmt-defun-if-void texinfo-format-ifdocbook ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end ifdocbook[ \t]*\n")
			(point))))


;;; Marking
;; @indicateurl{INDICATEURL}
(put 'indicateurl 'texinfo-format 'texinfo-format-code)


;; @LaTeX{}
(put 'LaTeX 'texinfo-format 'texinfo-format-LaTeX)
(ptexinfmt-defun-if-void texinfo-format-LaTeX ()
  (texinfo-parse-arg-discard)
  (insert "LaTeX"))

;; @registeredsymbol{}
(put 'registeredsymbol 'texinfo-format 'texinfo-format-registeredsymbol)
(ptexinfmt-defun-if-void texinfo-format-registeredsymbol ()
  (texinfo-parse-arg-discard)
  (insert "(R)"))

;;; Accents and Special characters
;; @euro{}	==>	Euro
(put 'euro 'texinfo-format 'texinfo-format-euro)
(ptexinfmt-defun-if-void texinfo-format-euro ()
  (texinfo-parse-arg-discard)
  (insert "Euro "))

;; @ordf{}	==>	a	Spanish feminine
(put 'ordf 'texinfo-format 'texinfo-format-ordf)
(ptexinfmt-defun-if-void texinfo-format-ordf ()
  (texinfo-parse-arg-discard)
  (insert "a"))

;; @ordm{}	==>	o	Spanish masculine
(put 'ordm 'texinfo-format 'texinfo-format-ordm)
(ptexinfmt-defun-if-void texinfo-format-ordm ()
  (texinfo-parse-arg-discard)
  (insert "o"))

;; @.
(put '\. 'texinfo-format 'texinfo-format-\.)
(ptexinfmt-defun-if-void texinfo-format-\. ()
  (texinfo-discard-command)
  (insert "."))

;; @:
(put '\: 'texinfo-format 'texinfo-format-\:)
(ptexinfmt-defun-if-void texinfo-format-\: ()
  (texinfo-discard-command))

;; @-
(put '\- 'texinfo-format 'texinfo-format-soft-hyphen)
(ptexinfmt-defun-if-void texinfo-format-soft-hyphen ()
  (texinfo-discard-command))

;; @/
(put '\/ 'texinfo-format 'texinfo-format-\/)
(ptexinfmt-defun-if-void texinfo-format-\/ ()
  (texinfo-discard-command))

;; @textdegree{}
(put 'textdegree 'texinfo-format 'texinfo-format-textdegree)
(ptexinfmt-defun-if-void texinfo-format-textdegree ()
  (insert "o" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @geq{}
(put 'geq 'texinfo-format 'texinfo-format-geq)
(ptexinfmt-defun-if-void texinfo-format-geq ()
  (insert ">=" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @leq{}
(put 'leq 'texinfo-format 'texinfo-format-leq)
(ptexinfmt-defun-if-void texinfo-format-leq ()
  (insert "<=" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @U{XXXX}
(put 'U 'texinfo-format 'texinfo-format-U)
(ptexinfmt-defun-if-void texinfo-format-U ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (insert (string-to-number (nth 0 args) 16))))


;;; Cross References
;; @ref{NODE-NAME, ...}
;; @xref{NODE-NAME, ...}
(put 'ref 'texinfo-format 'texinfo-format-xref)



;;; Indent
;; @exampleindent INDENT  (makeinfo 4.0 or later)

;; @paragraphindent INDENT  (makeinfo 4.0 or later)
;; INDENT: asis, 0, n

;; @firstparagraphindent WORD   (makeinfo 4.6 or later)
;; WORD: none, insert



;;; @multitable ... @end multitable

;; Workaround for @headitem
(unless (get 'headitem 'texinfo-format)
  (put 'headitem 'texinfo-format 'texinfo-item))


;; @comma
(if (fboundp 'texinfo-format-comma)
    nil
  (put 'comma 'texinfo-format 'texinfo-format-comma)
  (defun texinfo-format-comma ()
    (texinfo-parse-arg-discard)
    (insert ",")
    (put-text-property (1- (point)) (point) 'ignore t))

  ;; Redefine this function so as to work for @comma
  (defun texinfo-format-parse-args ()
    (let ((start (1- (point)))
	  next beg end
	  args)
      (search-forward "{")
      (save-excursion
	(texinfo-format-expand-region
	 (point)
	 (save-excursion (up-list 1) (1- (point)))))
      ;; The following does not handle cross references of the form:
      ;; `@xref{bullet, , @code{@@bullet}@{@}}.' because the
      ;; re-search-forward finds the first right brace after the second
      ;; comma.
      (while (/= (preceding-char) ?\})
	(skip-chars-forward " \t\n")
	(setq beg (point))
;;;	(re-search-forward "[},]")
	;; Ignore commas that are derived from @comma{}.
	(while (and (re-search-forward "[},]" nil t)
		    (get-text-property (match-beginning 0) 'ignore)))
;;;
	(setq next (point))
	(forward-char -1)
	(skip-chars-backward " \t\n")
	(setq end (point))
	(cond ((< beg end)
	       (goto-char beg)
	       (while (search-forward "\n" end t)
		 (replace-match " "))))
	(push (if (> end beg) (buffer-substring-no-properties beg end))
	      args)
	(goto-char next))
      ;;(if (eolp) (forward-char 1))
      (setq texinfo-command-end (point))
      (nreverse args))))

(provide 'ptexinfmt)

;;; ptexinfmt.el ends here
