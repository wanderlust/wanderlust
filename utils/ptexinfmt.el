;;; ptexinfmt.el -- portable Texinfo formatter.

;; Copyright (C) 2000 TAKAHASHI Kaoru <kaoru@kaisei.org>

;; Author: TAKAHASHI Kaoru <kaoru@kaisei.org>
;;	Yoshiki Hayashi <yoshiki@xemacs.org>
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Original code: Yoshiki Hayashi <yoshiki@xemacs.org>
;;	makeinfo.el (gnujdoc project)

;; Support texinfmt.el 2.32 or later.

;;; Code:
(require 'texinfmt)
(require 'poe)
(require 'broken)

(provide 'ptexinfmt)

;;; Fix broken facility
;; @var
(broken-facility texinfo-format-var
  "When @COMMAND included, don't perse it."
  (condition-case nil
      (with-temp-buffer
	(let (texinfo-enclosure-list texinfo-alias-list)
	  (texinfo-mode)
	  (insert "@var{@asis{foo}}\n")
	  (texinfo-format-expand-region (point-min) (point-max))
	  t))
    (error nil)))

(when-broken texinfo-format-var
  (defun texinfo-format-var ()
    (let ((arg (texinfo-parse-expanded-arg)))
      (texinfo-discard-command)
      (insert (upcase arg)))))

;; @xref
(broken-facility texinfo-format-xref
  "When 1st arg is nil, wrong-type-argument error."
  (condition-case nil
      (with-temp-buffer
	(let (texinfo-enclosure-list texinfo-alias-list)
	  (texinfo-mode)
	  (insert "@xref{, xref, , file}\n")
	  (texinfo-format-expand-region (point-min) (point-max))
	  t))
    (error nil)))

(when-broken texinfo-format-xref
  (defun texinfo-format-xref ()
    (let ((args (texinfo-format-parse-args)))
      (texinfo-discard-command)
      (insert "*Note ")
      (let ((fname (or (nth 1 args) (nth 2 args))))
	(if (null (or fname (nth 3 args)))
	    (insert (car args) "::")
	  (insert (or fname (car args)) ": ")
	  (if (nth 3 args)
	      (insert "(" (nth 3 args) ")"))
	  (and (car args) (insert (car args))))))))

;;; Directory File
;; @direcategory
(put 'dircategory 'texinfo-format 'texinfo-format-dircategory)
(defun-maybe texinfo-format-dircategory ()
  (let ((str (texinfo-parse-arg-discard)))
    (delete-region (point)
		   (progn
		     (skip-chars-forward " ")
		     (point)))
    (insert "INFO-DIR-SECTION " str "\n")))

;; @direntry
(put 'direntry 'texinfo-format 'texinfo-format-direntry)
(defun-maybe texinfo-format-direntry ()
  (texinfo-push-stack 'direntry nil)
  (texinfo-discard-line)
  (insert "START-INFO-DIR-ENTRY\n\n"))

(put 'direntry 'texinfo-end 'texinfo-end-direntry)
(defun-maybe texinfo-end-direntry ()
  (texinfo-discard-command)
  (insert "END-INFO-DIR-ENTRY\n\n")
  (texinfo-pop-stack 'direntry))


;;; Menus
;; @detailmenu ... @end detailmenu
(put 'detailmenu 'texinfo-format 'texinfo-discard-line)
(put 'detailmenu 'texinfo-end 'texinfo-discard-command)

;;; Block Enclosing
;; @smalldisplay ... @end smalldisplay
(put 'smalldisplay 'texinfo-format 'texinfo-format-example)
(put 'smalldisplay 'texinfo-end 'texinfo-end-example)

;; @smallformat ... @end smallformat
(put 'smallformat 'texinfo-format 'texinfo-format-flushleft)
(put 'smallformat 'texinfo-end 'texinfo-end-flushleft)


;;; Marking
;; @url, @env, @command
(put 'url 'texinfo-format 'texinfo-format-code)
(put 'command 'texinfo-format 'texinfo-format-code)
(put 'env 'texinfo-format 'texinfo-format-code)

;; @acronym
(put 'acronym 'texinfo-format 'texinfo-format-var)

;; @email
(put 'email 'texinfo-format 'texinfo-format-key)
(defun-maybe texinfo-format-key ()
  (insert (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @option
(put 'option 'texinfo-format 'texinfo-format-option)
(defun-maybe texinfo-format-option ()
  "Insert ` ... ' around arg unless inside a table; in that case, no quotes."
  ;; `looking-at-backward' not available in v. 18.57, 20.2
  ;; searched-for character is a control-H
  (if (not (search-backward "\010"
			    (save-excursion (beginning-of-line) (point))
			    t))
      (insert "`" (texinfo-parse-arg-discard) "'")
    (insert  (texinfo-parse-arg-discard)))
  (goto-char texinfo-command-start))


;;; Cross References
;; @ref
(put 'ref 'texinfo-format 'texinfo-format-xref)

;; @uref
(put 'uref 'texinfo-format 'texinfo-format-uref)
(defun-maybe texinfo-format-uref ()
  "Format URL and optional URL-TITLE.
Insert ` ... ' around URL if no URL-TITLE argument;
otherwise, insert URL-TITLE followed by URL in parentheses."
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    ;; if url-title
    (if (nth 1 args)
	(insert  (nth 1 args) " (" (nth 0 args) ")")
      (insert "`" (nth 0 args) "'"))
    (goto-char texinfo-command-start)))

;;; Conditional
;; @ifnottex ... @end ifnottex
(put 'ifnottex 'texinfo-format 'texinfo-discard-line)
(put 'ifnottex 'texinfo-end 'texinfo-discard-command)

;; @ifnothtml ... @end ifnothtml
(put 'ifnothtml 'texinfo-format 'texinfo-discard-line)
(put 'ifnothtml 'texinfo-end 'texinfo-discard-command)

;; @ifnotinfo ... @end ifnotinfo
(put 'ifnotinfo 'texinfo-format 'texinfo-format-ifnotinfo)
(put 'endifnotinfo 'texinfo-format 'texinfo-discard-line)
(defun-maybe texinfo-format-ifnotinfo ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end ifnotinfo[ \t]*\n")
			(point))))


;;; Defining New Texinfo Commands
;; @alias (maybe invalid)
(put 'alias 'texinfo-format 'texinfo-alias)
(defun-maybe texinfo-alias ()
  (let ((start (1- (point)))
	args)
    (skip-chars-forward " ")
    (save-excursion (end-of-line) (setq texinfo-command-end (point)))
    (if (not (looking-at "\\([^=]+\\)=\\(.*\\)"))
	(error "Invalid alias command")
      (setq texinfo-alias-list
	    (cons
	     (cons
	      (buffer-substring (match-beginning 1) (match-end 1))
	      (buffer-substring (match-beginning 2) (match-end 2)))
	     texinfo-alias-list))
      (texinfo-discard-command))))

;;; misc.
(put 'documentlanguage 'texinfo-format 'texinfo-discard-line-with-args)
(put 'documentencoding 'texinfo-format 'texinfo-discard-line-with-args)
(put 'smallbook 'texinfo-format 'texinfo-discard-line)
(put 'overfullrule 'texinfo-format 'texinfo-discard-line)
(put 'smallbreak 'texinfo-format 'texinfo-discard-line)
(put 'medbreak 'texinfo-format 'texinfo-discard-line)
(put 'bigbreak 'texinfo-format 'texinfo-discard-line)

(put 'setchapterstyle 'texinfo-format 'texinfo-discard-line-with-args)
(put 'novalidate 'texinfo-format 'texinfo-discard-line-with-args)
(put 'hyphenation 'texinfo-format 'texinfo-discard-command-and-arg)
(put 'pagesizes 'texinfo-format 'texinfo-discard-line-with-args)


;;; Accents and so on
;; @pounds{}	==>	#	Pounds Sterling
(put 'pounds 'texinfo-format 'texinfo-format-pounds)
(defun-maybe texinfo-format-pounds ()
  (texinfo-parse-arg-discard)
  (insert "#"))

;; @OE{}	==>	OE	French-OE-ligature
(put 'OE 'texinfo-format 'texinfo-format-French-OE-ligature)
(defun-maybe texinfo-format-French-OE-ligature ()
  (insert "OE" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @oe{}	==>	oe
(put 'oe 'texinfo-format 'texinfo-format-French-oe-ligature)
(defun-maybe texinfo-format-French-oe-ligature () ; lower case
  (insert "oe" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @AA{}	==>	AA	Scandinavian-A-with-circle
(put 'AA 'texinfo-format 'texinfo-format-Scandinavian-A-with-circle)
(defun-maybe texinfo-format-Scandinavian-A-with-circle ()
  (insert "AA" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @aa{}	==>	aa
(put 'aa 'texinfo-format 'texinfo-format-Scandinavian-a-with-circle)
(defun-maybe texinfo-format-Scandinavian-a-with-circle () ; lower case
  (insert "aa" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @AE{}	==>	AE	Latin-Scandinavian-AE
(put 'AE 'texinfo-format 'texinfo-format-Latin-Scandinavian-AE)
(defun-maybe texinfo-format-Latin-Scandinavian-AE ()
  (insert "AE" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @ae{}	==>	ae
(put 'ae 'texinfo-format 'texinfo-format-Latin-Scandinavian-ae)
(defun-maybe texinfo-format-Latin-Scandinavian-ae () ; lower case
  (insert "ae" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @ss{}	==>	ss	German-sharp-S
(put 'ss 'texinfo-format 'texinfo-format-German-sharp-S)
(defun-maybe texinfo-format-German-sharp-S ()
  (insert "ss" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @questiondown{}	==>	?	upside-down-question-mark
(put 'questiondown 'texinfo-format 'texinfo-format-upside-down-question-mark)
(defun-maybe texinfo-format-upside-down-question-mark ()
  (insert "?" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @exclamdown{}	==>	!	upside-down-exclamation-mark
(put 'exclamdown 'texinfo-format 'texinfo-format-upside-down-exclamation-mark)
(defun-maybe texinfo-format-upside-down-exclamation-mark ()
  (insert "!" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @L{}		==>	L/	Polish suppressed-L (Lslash)
(put 'L 'texinfo-format 'texinfo-format-Polish-suppressed-L)
(defun-maybe texinfo-format-Polish-suppressed-L ()
  (insert (texinfo-parse-arg-discard) "/L")
  (goto-char texinfo-command-start))

;; @l{}		==>	l/	Polish suppressed-L (Lslash) (lower case)
(put 'l 'texinfo-format 'texinfo-format-Polish-suppressed-l-lower-case)
(defun-maybe texinfo-format-Polish-suppressed-l-lower-case ()
  (insert (texinfo-parse-arg-discard) "/l")
  (goto-char texinfo-command-start))

;; @O{}		==>	O/	Scandinavian O-with-slash
(put 'O 'texinfo-format 'texinfo-format-Scandinavian-O-with-slash)
(defun-maybe texinfo-format-Scandinavian-O-with-slash ()
  (insert (texinfo-parse-arg-discard) "O/")
  (goto-char texinfo-command-start))

;; @o{}		==>	o/	Scandinavian O-with-slash (lower case)
(put 'o 'texinfo-format 'texinfo-format-Scandinavian-o-with-slash-lower-case)
(defun-maybe texinfo-format-Scandinavian-o-with-slash-lower-case ()
  (insert (texinfo-parse-arg-discard) "o/")
  (goto-char texinfo-command-start))

;; @,{c}	==>	c,	cedilla accent
(put ', 'texinfo-format 'texinfo-format-cedilla-accent)
(defun-maybe texinfo-format-cedilla-accent ()
  (insert (texinfo-parse-arg-discard) ",")
  (goto-char texinfo-command-start))


;; @dotaccent{o}	==>	.o	overdot-accent
(put 'dotaccent 'texinfo-format 'texinfo-format-overdot-accent)
(defun-maybe texinfo-format-overdot-accent ()
  (insert "." (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @ubaraccent{o}	==>	_o	underbar-accent
(put 'ubaraccent 'texinfo-format 'texinfo-format-underbar-accent)
(defun-maybe texinfo-format-underbar-accent ()
  (insert "_" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @udotaccent{o}	==>	o-.	underdot-accent
(put 'udotaccent 'texinfo-format 'texinfo-format-underdot-accent)
(defun-maybe texinfo-format-underdot-accent ()
  (insert (texinfo-parse-arg-discard) "-.")
  (goto-char texinfo-command-start))

;; @H{o}	==>	""o	long Hungarian umlaut
(put 'H 'texinfo-format 'texinfo-format-long-Hungarian-umlaut)
(defun-maybe texinfo-format-long-Hungarian-umlaut ()
  (insert "\"\"" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @ringaccent{o}	==>	*o	ring accent
(put 'ringaccent 'texinfo-format 'texinfo-format-ring-accent)
(defun-maybe texinfo-format-ring-accent ()
  (insert "*" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @tieaccent{oo}	==>	[oo	tie after accent
(put 'tieaccent 'texinfo-format 'texinfo-format-tie-after-accent)
(defun-maybe texinfo-format-tie-after-accent ()
  (insert "[" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @u{o}	==>	(o	breve accent
(put 'u 'texinfo-format 'texinfo-format-breve-accent)
(defun-maybe texinfo-format-breve-accent ()
  (insert "(" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @v{o}	==>	<o	hacek accent
(put 'v 'texinfo-format 'texinfo-format-hacek-accent)
(defun-maybe texinfo-format-hacek-accent ()
  (insert "<" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @dotless{i}	==>	i	dotless i and dotless j
(put 'dotless 'texinfo-format 'texinfo-format-dotless)
(defun-maybe texinfo-format-dotless ()
  (insert  (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;;; ptexinfmt.el ends here
