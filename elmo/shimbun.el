;;; shimbun.el --- interfacing with web newspapers -*- coding: junet; -*-

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;         Akihiro Arisawa    <ari@atesoft.advantest.co.jp>
;;         Yuuichi Teranishi <teranisi@gohome.org>

;; Keywords: news

;;; Copyright:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Original code was nnshimbun.el written by 
;; TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>.

;; Shimbun API:
;;
;; shimbun-open
;; shimbun-groups
;; shimbun-open-group
;; shimbun-close-group
;; shimbun-headers
;; shimbun-header
;; shimbun-article
;; shimbun-close

;; Shimbun Header API:
;;
;; shimbun-header-subject
;; shimbun-header-set-subject
;; shimbun-header-from
;; shimbun-header-set-from
;; shimbun-header-date
;; shimbun-header-set-date
;; shimbun-header-id
;; shimbun-header-set-id
;; shimbun-header-references
;; shimbun-header-set-references
;; shimbun-header-chars
;; shimbun-header-set-chars
;; shimbun-header-lines
;; shimbun-header-set-lines
;; shimbun-header-xref
;; shimbun-header-set-xref
;; shimbun-header-extra
;; shimbun-header-set-extra
;; shimbun-header-insert

(eval-when-compile (require 'cl))
(eval-when-compile (require 'static))

(require 'mcharset)
(require 'eword-encode)
(require 'luna)
(require 'std11)

(eval-and-compile
  (luna-define-class shimbun ()
		     (server current-group groups headers hash x-face
			     url coding-system from-address
			     content-start content-end))
  (luna-define-internal-accessors 'shimbun))

(defvar shimbun-x-face-alist
  '(("asahi" .
     (("default" .
       "X-Face:  +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|bIy3rr^<Q#lf&~ADU:X!t5t>
        gW5)Q]N{MmnL]suPpL|gFjV{S|]a-:)\\FR7GRf9uL:ue5_=;h{V%@()={u
        Td@l?eXBppF%`6W%;h`#]2q+f*81n$Bh|t")))
    ("cnet" .
     (("default" .
       "X-Face: 0p7.+XId>z%:!$ahe?x%+AEm37Abvn]n*GGh+>v=;[3`a{1l
        qO[$,~3C3xU_ri>[JwJ!9l0~Y`b*eXAQ:*q=bBI_=ro*?]4:
        |n>]ZiLZ2LEo^2nr('C<+`lO~/!R[lH'N'4X&%\\I}8T!wt")))
    ("wired" .
     (("default" .
       "X-Face: \"yhMDxMBowCFKt;5Q$s_Wx)/'L][0@c\"#n2BwH{7mg]5^w1D]\"K^R
        ]&fZ5xtt1Ynu6V;Cv(@BcZUf9IV$($6TZ`L)$,cegh`b:Uwy`8}#D
        b-kyCsr_UMRz=,U|>-:&`05lXB4(;h{[&~={Imb-az7&U5?|&X_8c
        ;#'L|f.P,]|\\50pgSVw_}byL+%m{TrS[\"Ew;dbskaBL[ipk2m4V")))
    ("zdnet" .
     (("default" .
       "X-Face: 88Zbg!1nj{i#[*WdSZNrn1$Cdfat,zsG`P)OLo=U05q:RM#72\\p;3XZ
        ~j|7T)QC7\"(A;~HrfP.D}o>Z.]=f)rOBz:A^G*M3Ea5JCB$a>BL/y!")))
    ("default" .
     (("default" .
       "X-Face: Ygq$6P.,%Xt$U)DS)cRY@k$VkW!7(X'X'?U{{osjjFG\"E]hND;SPJ-J?O?R|a?L
        g2$0rVng=O3Lt}?~IId8Jj&vP^3*o=LKUyk(`t%0c!;t6REk=JbpsEn9MrN7gZ%"))))
  "Alist of server vs. alist of group vs. X-Face field.  It looks like:

\((\"asahi\" . ((\"national\" . \"X-face: ***\")
	     (\"business\" . \"X-Face: ***\")
		;;
		;;
	     (\"default\" . \"X-face: ***\")))
 (\"sponichi\" . ((\"baseball\" . \"X-face: ***\")
		(\"soccer\" . \"X-Face: ***\")
		;;
		;;
		(\"default\" . \"X-face: ***\")))
		;;
 (\"default\" . ((\"default\" . \"X-face: ***\")))")

(defconst shimbun-meta-content-type-charset-regexp
  (eval-when-compile
    (concat "<meta[ \t]+http-equiv=\"?Content-type\"?[ \t]+content=\"\\([^;]+\\)"
	    ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
	    ">"))
  "Regexp used in parsing `<META HTTP-EQUIV=\"Content-Type\" content=\"...;charset=...\">
for a charset indication")

(defconst shimbun-meta-charset-content-type-regexp
  (eval-when-compile
    (concat "<meta[ \t]+content=\"\\([^;]+\\)"
	    ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
	    "[ \t]+http-equiv=\"?Content-type\"?>"))
  "Regexp used in parsing `<META content=\"...;charset=...\" HTTP-EQUIV=\"Content-Type\">
for a charset indication")

(defvar shimbun-hash-length 997
  "Length of header hashtable.")

(static-when (boundp 'MULE)
  (unless (coding-system-p 'euc-japan)
    (copy-coding-system '*euc-japan* 'euc-japan))
  (unless (coding-system-p 'shift_jis)
    (copy-coding-system '*sjis* 'shift_jis))
  (eval-and-compile
    (defalias-maybe 'coding-system-category 'get-code-mnemonic)))

(static-if (and (ignore-errors (require 'w3m))
		(fboundp 'w3m-retrieve))
(progn
(require 'w3m)
(defun shimbun-retrieve-url (shimbun url &optional no-cache)
  "Rertrieve URL contents and insert to current buffer."
  (when (w3m-retrieve url nil no-cache)
    (insert-buffer w3m-work-buffer-name))))
;; Otherwise.
(require 'url)
(defun shimbun-retrieve-url (shimbun url &optional no-cache)
  "Rertrieve URL contents and insert to current buffer."
  (let ((buf (current-buffer))
	(url-working-buffer url-working-buffer))
    (let ((old-asynch (default-value 'url-be-asynchronous))
	  (old-caching (default-value 'url-automatic-caching))
	  (old-mode (default-value 'url-standalone-mode)))
      (setq-default url-be-asynchronous nil)
      (when no-cache
	(setq-default url-automatic-caching nil)
	(setq-default url-standalone-mode nil))
      (unwind-protect
	  (let ((coding-system-for-read 'binary)
		(coding-system-for-write 'binary)
		(input-coding-system 'binary)
		(output-coding-system 'binary)
		(default-enable-multibyte-characters nil))
	    (set-buffer
	     (setq url-working-buffer
		   (cdr (url-retrieve url no-cache))))
	    (url-uncompress))
	(setq-default url-be-asynchronous old-asynch)
	(setq-default url-automatic-caching old-caching)
	(setq-default url-standalone-mode old-mode)))
    (let ((charset
	   (or (and (boundp 'url-current-mime-charset)
		    (symbol-value 'url-current-mime-charset))
	       (let ((case-fold-search t))
		 (goto-char (point-min))
		 (if (or (re-search-forward
			  shimbun-meta-content-type-charset-regexp nil t)
			 (re-search-forward
			  shimbun-meta-charset-content-type-regexp nil t))
		     (buffer-substring-no-properties (match-beginning 2)
						     (match-end 2)))))))
      (decode-coding-region
       (point-min) (point-max)
       (if charset
	   (let ((mime-charset-coding-system-alist
		  (append '((euc-jp . euc-japan)
			    (shift-jis . shift_jis)
			    (shift_jis . shift_jis)
			    (sjis . shift_jis)
			    (x-euc-jp . euc-japan)
			    (x-shift-jis . shift_jis)
			    (x-shift_jis . shift_jis)
			    (x-sjis . shift_jis))
			  mime-charset-coding-system-alist)))
	     (mime-charset-to-coding-system charset))
	 (let ((default (condition-case nil
			    (coding-system-category
			     (shimbun-coding-system-internal shimbun))
			  (error nil)))
	       (candidate (detect-coding-region (point-min) (point-max))))
	   (unless (listp candidate)
	     (setq candidate (list candidate)))
	   (catch 'coding
	     (dolist (coding candidate)
	       (if (eq default (coding-system-category coding))
		   (throw 'coding coding)))
	     (if (eq (coding-system-category 'binary)
		     (coding-system-category (car candidate)))
		 (shimbun-coding-system-internal shimbun)
	       (car candidate)))))))
    (set-buffer-multibyte t)
    (set-buffer buf)
    (insert-buffer url-working-buffer)
    (kill-buffer url-working-buffer)))
;; End of shimbun-retrieve-url definition
)

;;; Implementation of Header API.
(defun shimbun-make-header (&optional number subject from date id
				      references chars lines xref
				      extra)
  (vector number subject from date id references chars lines xref extra))

;;(defsubst shimbun-header-number (header)
;;  (aref header 0))

(defsubst shimbun-header-field-value ()
  (let ((pt (point)))
    (prog1
	(buffer-substring (match-end 0) (std11-field-end))
      (goto-char pt))))

(defsubst shimbun-header-subject (header)
  (aref header 1))

(defsubst shimbun-header-set-subject (header subject)
  (aset header 1 subject))

(defsubst shimbun-header-from (header)
  (aref header 2))

(defsubst shimbun-header-set-from (header from)
  (aset header 2 from))

(defsubst shimbun-header-date (header)
  (aref header 3))

(defsubst shimbun-header-set-date (header date)
  (aset header 3 date))

(defsubst shimbun-header-id (header)
  (aref header 4))

(defsubst shimbun-header-set-id (header id)
  (aset header 4 id))

(defsubst shimbun-header-references (header)
  (aref header 5))

(defsubst shimbun-header-set-references (header references)
  (aset header 5 references))

(defsubst shimbun-header-chars (header)
  (aref header 6))

(defsubst shimbun-header-set-chars (header chars)
  (aset header 6 chars))

(defsubst shimbun-header-lines (header)
  (aref header 7))

(defsubst shimbun-header-set-lines (header lines)
  (aset header 7 lines))

(defsubst shimbun-header-xref (header)
  (aref header 8))

(defsubst shimbun-header-set-xref (header xref)
  (aset header 8 xref))

(defsubst shimbun-header-extra (header)
  (aref header 9))

(defsubst shimbun-header-set-extra (header extra)
  (aset header 9 extra))

(defun shimbun-header-insert (header)
  (insert "Subject: " (or (shimbun-header-subject header) "(none)") "\n"
	  "From: " (or (shimbun-header-from header) "(nobody)") "\n"
	  "Date: " (or (shimbun-header-date header) "") "\n"
	  "Message-ID: " (shimbun-header-id header) "\n")
  (let ((refs (shimbun-header-references header)))
    (and refs
	 (string< "" refs)
	 (insert "References: " refs "\n")))
  (insert "Lines: " (number-to-string (or (shimbun-header-lines header) 0)) 
	  "\n"
	  "Xref: " (or (shimbun-header-xref header) "") "\n"))

;;; Implementation of Shimbun API.

(defvar shimbun-attributes
  '(url groups coding-system from-address content-start content-end))

(defun shimbun-open (server)
  "Open a shimbun for SERVER."
  (require (intern (concat "sb-" server)))
  (let (url groups coding-system from-address content-start content-end)
    (dolist (attr shimbun-attributes)
      (set attr
	   (symbol-value (intern-soft 
			  (concat "shimbun-" server "-" (symbol-name attr))))))
    (luna-make-entity (intern (concat "shimbun-" server))
		      :server server
		      :url url
		      :groups groups
		      :coding-system coding-system
		      :from-address from-address
		      :content-start content-start
		      :content-end content-end)))

(defun shimbun-groups (shimbun)
  "Return a list of groups which are available in the SHIMBUN."
  (shimbun-groups-internal shimbun))

(defun shimbun-open-group (shimbun group)
  "Open a SHIMBUN GROUP."
  (unless (shimbun-current-group-internal shimbun)
;    (condition-case nil
	(if (member group (shimbun-groups-internal shimbun))
	    (progn
	      (shimbun-set-current-group-internal shimbun group)
	      (let ((x-faces (cdr (or (assoc (shimbun-server-internal shimbun)
					     shimbun-x-face-alist)
				      (assoc "default" shimbun-x-face-alist)))))
		(shimbun-set-x-face-internal shimbun
					     (cdr (or (assoc group x-faces)
						      (assoc "default" x-faces)))))
	      (with-temp-buffer
		(shimbun-retrieve-url shimbun (shimbun-index-url shimbun))
		(shimbun-set-headers-internal shimbun
					      (shimbun-get-headers shimbun)))
	      (shimbun-set-hash-internal shimbun
					 (make-vector shimbun-hash-length 0))
	      (dolist (header (shimbun-headers-internal shimbun))
		(set (intern (shimbun-header-id header)
			     (shimbun-hash-internal shimbun))
		     header)))
	  (error "Cannot open group %s" group))))
;      (error (shimbun-set-current-group-internal shimbun nil)))))

(defun shimbun-close-group (shimbun)
  "Close opened group of SHIMBUN."
  (when (shimbun-current-group-internal shimbun)
    (shimbun-set-current-group-internal shimbun nil)
    (shimbun-set-headers-internal shimbun nil)
    (shimbun-set-hash-internal shimbun nil)))

(defun shimbun-headers (shimbun)
  "Return a SHIMBUN header list."
  (shimbun-headers-internal shimbun))

(defun shimbun-header (shimbun id)
  "Return a SHIMBUN header which corresponds to ID."
  (when (shimbun-current-group-internal shimbun)
    (let ((sym (intern-soft id (shimbun-hash-internal shimbun))))
      (if (boundp sym)
	  (symbol-value sym)))))

(luna-define-generic shimbun-article (shimbun id &optional outbuf)
  "Retrieve a SHIMBUN article which corresponds to ID to the OUTBUF.
If OUTBUF is not specified, article is retrieved to the current buffer.")

(luna-define-method shimbun-article ((shimbun shimbun) id &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (let* ((header (shimbun-header shimbun id))
	   (xref (shimbun-header-xref header)))
      (with-current-buffer (or outbuf (current-buffer))
	(insert
	 (or (with-temp-buffer
	       (shimbun-retrieve-url shimbun xref)
	       (message "shimbun: Make contents...")
	       (goto-char (point-min))
	       (prog1 (shimbun-make-contents shimbun header)
		 (message "shimbun: Make contents...done"))) 
	     ""))))))

(defsubst shimbun-make-html-contents (shimbun header)
  (let (start)
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (setq start (point))
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start))
    (goto-char (point-min))
    (shimbun-header-insert header)
    (insert "Content-Type: text/html; charset=ISO-2022-JP\n"
	    "MIME-Version: 1.0\n")
    (when (shimbun-x-face-internal shimbun)
      (insert (shimbun-x-face-internal shimbun))
      (unless (bolp)
	(insert "\n")))
    (insert "\n")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(luna-define-generic shimbun-make-contents (shimbun header)
  "Return a content string of SHIMBUN article using current buffer content.
HEADER is a header structure obtained via `shimbun-get-headers'.")

(luna-define-method shimbun-make-contents ((shimbun shimbun) header)
  (shimbun-make-html-contents shimbun header))

(luna-define-generic shimbun-index-url (shimbun)
  "Return a index URL of SHIMBUN.")

;; Default is same as base url.
(luna-define-method shimbun-index-url ((shimbun shimbun))
  (shimbun-url-internal shimbun))

(luna-define-generic shimbun-get-headers (shimbun)
  "Return a shimbun header list of SHIMBUN.")

(luna-define-generic shimbun-close (shimbun)
  "Close a SHIMBUN.")
  
(luna-define-method shimbun-close ((shimbun shimbun))
  (shimbun-close-group shimbun))

;;; Misc Functions
(defun shimbun-mime-encode-string (string)
  (mapconcat
   #'identity
   (split-string (eword-encode-string
		  (shimbun-decode-entities-string string)) "\n")
   ""))

(defun shimbun-make-date-string (year month day &optional time)
  (format "%02d %s %04d %s +0900"
	  day
	  (aref [nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
		     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
		month)
	  (cond ((< year 69)
		 (+ year 2000))
		((< year 100)
		 (+ year 1900))
		((< year 1000)	; possible 3-digit years.
		 (+ year 1900))
		(t year))
	  (or time "00:00")))

(if (fboundp 'regexp-opt)
    (defalias 'shimbun-regexp-opt 'regexp-opt)
  (defun shimbun-regexp-opt (strings &optional paren)
    "Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct."
    (let ((open-paren (if paren "\\(" "")) (close-paren (if paren "\\)" "")))
      (concat open-paren (mapconcat 'regexp-quote strings "\\|") close-paren))))
;; Fast fill-region function

(defvar shimbun-fill-column (min 80 (- (frame-width) 4)))

(defconst shimbun-kinsoku-bol-list
  (append "!)-_~}]:;',.?、。，．・：；？！゛゜´｀¨＾￣＿ヽヾゝゞ〃\
仝々〆〇ー―‐／＼〜‖｜…‥’”）〕］｝〉》」』】°′″℃ぁぃぅぇぉ\
っゃゅょゎァィゥェォッャュョヮヵヶ" nil))

(defconst shimbun-kinsoku-eol-list
  (append "({[`‘“（〔［｛〈《「『【°′″§" nil))

(defun shimbun-fill-line ()
  (forward-line 0)
  (let ((top (point)) chr)
    (while (if (>= (move-to-column shimbun-fill-column)
		   shimbun-fill-column)
	       (not (progn
		      (if (memq (preceding-char) shimbun-kinsoku-eol-list)
			  (progn
			    (backward-char)
			    (while (memq (preceding-char) shimbun-kinsoku-eol-list)
			      (backward-char))
			    (insert "\n"))
			(while (memq (setq chr (following-char)) shimbun-kinsoku-bol-list)
			  (forward-char))
			(if (looking-at "\\s-+")
			    (or (eolp) (delete-region (point) (match-end 0)))
			  (or (> (char-width chr) 1)
			      (re-search-backward "\\<" top t)
			      (end-of-line)))
			(or (eolp) (insert "\n"))))))
      (setq top (point))))
  (forward-line 1)
  (not (eobp)))

(defsubst shimbun-shallow-rendering ()
  (goto-char (point-min))
  (while (search-forward "<p>" nil t)
    (insert "\n\n"))
  (goto-char (point-min))
  (while (search-forward "<br>" nil t)
    (insert "\n"))
  (shimbun-remove-markup)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (while (shimbun-fill-line))
  (goto-char (point-min))
  (when (skip-chars-forward "\n")
    (delete-region (point-min) (point)))
  (while (search-forward "\n\n" nil t)
    (let ((p (point)))
      (when (skip-chars-forward "\n")
	(delete-region p (point)))))
  (goto-char (point-max))
  (when (skip-chars-backward "\n")
    (delete-region (point) (point-max)))
  (insert "\n"))

;;; entity decoding (stolen from w3m.el)
(eval-and-compile
  (defconst shimbun-entity-alist		; html character entities and values
    (eval-when-compile
      (let ((basic-entity-alist
	     '(("nbsp" . " ")
	       ("gt" . ">")
	       ("lt" . "<")
	       ("amp" . "&")
	       ("quot" . "\"")
	       ("apos" . "'")))
	    (latin1-entity
	     '(				;("nbsp" . 160)
	       ("iexcl" . 161) ("cent" . 162) ("pound" . 163)
	       ("curren" . 164) ("yen" . 165) ("brvbar" . 166) ("sect" . 167)
	       ("uml" . 168) ("copy" . 169) ("ordf" . 170) ("laquo" . 171)
	       ("not" . 172)  ("shy" . 173) ("reg" . 174) ("macr" . 175)
	       ("deg" . 176) ("plusmn" . 177) ("sup2" . 178) ("sup3" . 179)
	       ("acute" . 180) ("micro" . 181) ("para" . 182) ("middot" . 183)
	       ("cedil" . 184) ("sup1" . 185) ("ordm" . 186) ("raquo" . 187)
	       ("frac14" . 188) ("frac12" . 189) ("frac34" . 190) ("iquest" . 191)
	       ("Agrave" . 192) ("Aacute" . 193) ("Acirc" . 194) ("Atilde" . 195)
	       ("Auml" . 196) ("Aring" . 197) ("AElig" . 198) ("Ccedil" . 199)
	       ("Egrave" . 200) ("Eacute" . 201) ("Ecirc" . 202) ("Euml" . 203)
	       ("Igrave" . 204) ("Iacute" . 205) ("Icirc" . 206) ("Iuml" . 207)
	       ("ETH"  . 208) ("Ntilde" . 209) ("Ograve" . 210) ("Oacute" . 211)
	       ("Ocirc" . 212) ("Otilde" . 213) ("Ouml" . 214) ("times" . 215)
	       ("Oslash" . 216) ("Ugrave" . 217) ("Uacute" . 218) ("Ucirc" . 219)
	       ("Uuml" . 220) ("Yacute" . 221) ("THORN" . 222) ("szlig" . 223)
	       ("agrave" . 224) ("aacute" . 225) ("acirc" . 226) ("atilde" . 227)
	       ("auml" . 228) ("aring" . 229) ("aelig" . 230) ("ccedil" . 231)
	       ("egrave" . 232) ("eacute" . 233) ("ecirc" . 234) ("euml" . 235)
	       ("igrave" . 236) ("iacute" . 237) ("icirc" . 238) ("iuml" . 239)
	       ("eth" . 240) ("ntilde" . 241) ("ograve" . 242) ("oacute" . 243)
	       ("ocirc" . 244) ("otilde" . 245) ("ouml" . 246) ("divide" . 247)
	       ("oslash" . 248) ("ugrave" . 249) ("uacute" . 250) ("ucirc" . 251)
	       ("uuml" . 252) ("yacute" . 253) ("thorn" . 254) ("yuml" . 255))))
	(append basic-entity-alist
		(mapcar
		 (function
		  (lambda (entity)
		    (cons (car entity)
			  (char-to-string
			   (make-char
			    (static-if (boundp 'MULE) lc-ltn1 'latin-iso8859-1)
			    (cdr entity))))))
		 latin1-entity))))))

(defconst shimbun-entity-regexp
  (eval-when-compile
    (format "&\\(%s\\|#[0-9]+\\);?"
	    (if (fboundp 'regexp-opt)
		(let ((fn (function regexp-opt)))
		  ;; Don't funcall directly for avoiding compile warning.
		  (funcall fn (mapcar (function car)
				      shimbun-entity-alist)))
	      (mapconcat (lambda (s)
			   (regexp-quote (car s)))
			 shimbun-entity-alist
			 "\\|")))))

(defvar shimbun-entity-db nil)		; nil means un-initialized
(defconst shimbun-entity-db-size 13)	; size of obarray

(defun shimbun-entity-db-setup ()
  ;; initialise entity database (obarray)
  (setq shimbun-entity-db (make-vector shimbun-entity-db-size 0))
  (dolist (elem shimbun-entity-alist)
    (set (intern (car elem) shimbun-entity-db)
	 (cdr elem))))

(defsubst shimbun-entity-value (name)
  ;; initialise if need
  (if (null shimbun-entity-db)
      (shimbun-entity-db-setup))
  ;; return value of specified entity, or empty string for unknown entity.
  (or (symbol-value (intern-soft name shimbun-entity-db))
      (if (not (char-equal (string-to-char name) ?#))
	  (concat "&" name)		; unknown entity
	;; case of immediate character (accept only 0x20 .. 0x7e)
	(let ((char (string-to-int (substring name 1)))
	      sym)
	  ;; make character's representation with learning
	  (set (setq sym (intern name shimbun-entity-db))
	       (if (or (< char 32) (< 127 char))
		   "~"			; un-supported character
		 (char-to-string char)))))))

(defun shimbun-decode-entities ()
  "Decode entities in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward shimbun-entity-regexp nil t)
      (replace-match (shimbun-entity-value (match-string 1)) nil t))))

(defun shimbun-decode-entities-string (string)
  "Decode entities in the STRING."
  (with-temp-buffer
    (insert string)
    (shimbun-decode-entities)
    (buffer-string)))

(defun shimbun-remove-markup ()
  "Remove all HTML markup, leaving just plain text."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "<!--" nil t)
      (delete-region (match-beginning 0)
		     (or (search-forward "-->" nil t)
			 (point-max))))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match "" t t))))

(provide 'shimbun)
;;; shimbun.el ends here.
