;;; unique-id.el --- Compute DIGEST-MD5.

;; Copyright (C) 1999 Kenichi OKADA

;; Author: Katsumi Yamaoka  <yamaoka@jpl.org>

;; This file is part of FLIM (Faithful Library about Internet Message).

;;; Code:

;;; Gnus 5.8.3: message.el

(defvar unique-id-m-char nil)

;; If you ever change this function, make sure the new version
;; cannot generate IDs that the old version could.
;; You might for example insert a "." somewhere (not next to another dot
;; or string boundary), or modify the suffix string (default to "fsf").
(defun unique-id-m (&optional suffix)
  ;; Don't use microseconds from (current-time), they may be unsupported.
  ;; Instead we use this randomly inited counter.
  (setq unique-id-m-char
	(% (1+ (or unique-id-m-char (logand (random t) (1- (lsh 1 20)))))
	   ;; (current-time) returns 16-bit ints,
	   ;; and 2^16*25 just fits into 4 digits i base 36.
	   (* 25 25)))
  (let ((tm (current-time)))
    (concat
     (if (memq system-type '(ms-dos emx vax-vms))
	 (let ((user (downcase (user-login-name))))
	   (while (string-match "[^a-z0-9_]" user)
	     (aset user (match-beginning 0) ?_))
	   user)
       (unique-id-m-number-base36 (user-uid) -1))
     (unique-id-m-number-base36 (+ (car   tm)
				   (lsh (% unique-id-m-char 25) 16)) 4)
     (unique-id-m-number-base36 (+ (nth 1 tm)
				   (lsh (/ unique-id-m-char 25) 16)) 4)
     ;; Append the suffix, because while the generated ID is unique to
     ;; the application, other applications might otherwise generate
     ;; the same ID via another algorithm.
     (or suffix ".fsf"))))

(defun unique-id-m-number-base36 (num len)
  (if (if (< len 0)
	  (<= num 0)
	(= len 0))
      ""
    (concat (unique-id-m-number-base36 (/ num 36) (1- len))
	    (char-to-string (aref "zyxwvutsrqponmlkjihgfedcba9876543210"
				  (% num 36))))))


;;; Wanderlust 1.0.3: wl-draft.el, wl-mule.el

(defun unique-id-w-random-alphabet ()
  (let ((alphabet '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
		       ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z)))
    (nth (abs (% (random) 26)) alphabet)))

(defun unique-id-w ()
  (let ((time (current-time)))
    (format "%d.%d.%d.%d%c"
	    (car time) (nth 1 time) (nth 2 time)
	    (random 100000)
	    (unique-id-w-random-alphabet))))


;;; VM 6.75: vm-misc.el

(defun unique-id-v ()
  (let ((time (current-time)))
    (format "%d.%d.%d.%d"
	    (car time) (nth 1 time) (nth 2 time)
	    (random 1000000))))


;;; X-PGP-Sig 1.3.5.1

(defun unique-id-x (&optional length)
  (let ((i (or length 16))
	s)
    (while (> i 0)
      (setq i (1- i)
	    s (concat s (char-to-string (+ (/ (* 94 (% (abs (random)) 100))
					      100) 33)))))
    s))

(provide 'unique-id)

;;; unique-id.el ends here

