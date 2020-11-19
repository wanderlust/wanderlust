;;; elmo-date.el --- Date processing module for ELMO.  -*- lexical-binding: t -*-

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;


(require 'path-util)
(require 'timezone)
(require 'elmo-vars)
(require 'elmo-util)

(make-obsolete 'elmo-match-string 'match-string "26 Aug 2012")

(defalias 'elmo-match-buffer 'match-string-no-properties)
(make-obsolete 'elmo-match-buffer 'match-string-no-properties "24 May 2020")

(make-obsolete 'elmo-replace-in-string 'replace-regexp-in-string "17 Jun 2020")

(defvar elmo-date-descriptions
  '((yesterday . [0 0 1])
    (lastweek  . [0 0 7])
    (lastmonth . [0 1 0])
    (lastyear  . [1 0 0])))

(defun elmo-date-get-description (datevec)
  (format "%d-%s-%d"
	  (aref datevec 2)
	  (car (rassq (aref datevec 1)
		      timezone-months-assoc))
	  (aref datevec 0)))

(defun elmo-date-get-datevec (description)
  (cond
   ((not elmo-date-match)
    (error "Date match is not available"))
   ((string-match "^[ \t]*\\([0-9]+\\)?[ \t]*\\([a-zA-Z]+\\)$" description)
    (let ((today
	   (save-match-data
	     (timezone-fix-time (current-time-string) (current-time-zone)
				nil)))
	  (number
	   (string-to-number
	    (if (match-beginning 1)
		(match-string 1 description)
	      "0")))
	  (suffix (downcase (match-string 2 description)))
	  pair)
      (if (setq pair (assq (intern suffix) elmo-date-descriptions))
	  (elmo-datevec-substitute today (cdr pair))
	(if (string= "daysago" suffix)
	    (elmo-date-get-offset-datevec today number)
	  (error "%s is not supported yet" suffix)))))
   ((string-match "[0-9]+-[A-Za-z]+-[0-9]+" description)
    (timezone-fix-time
     (concat (elmo-replace-char-in-string ?- ?  description t) " 0:0")
     (current-time-zone) nil))
   ((string-match "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" description)
    (vector (string-to-number (match-string 1 description))
	    (string-to-number (match-string 2 description))
	    (string-to-number (match-string 3 description))
	    0 0 0
	    (current-time-zone)))))

(defun elmo-datevec-substitute (datevec1 datevec2)
  (if (/= (aref datevec2 2) 0)
      (elmo-date-get-offset-datevec datevec1 (aref datevec2 2))
    (let ((year (- (aref datevec1 0) (aref datevec2 0)))
	  (month (- (aref datevec1 1) (aref datevec2 1)))
	  (timezone (current-time-zone)))
      (while (<= month 0)
	(setq year (1- year)
	      month (+ 12 month)))
      (timezone-fix-time
       (format "%d %s %d 0:00 %s"
	       (aref datevec1 2)
	       (car (rassq month timezone-months-assoc))
	       year
	       (cadr timezone)) nil nil))))

(defun elmo-date-get-week (year month day)
  (let ((C (/ year 100))
	(Y (% year 100)))
    (aref (symbol-value (intern (concat "elmo-weekday-name-"
					elmo-lang)))
	  (% (+ day (/ (* 13 (1+ month)) 5) Y (/ Y 4)
		(* 5 C) (/ C 4) 6)
	     7))))

(defun elmo-date-get-offset-datevec (datevec offset &optional time)
  (let ((year  (aref datevec 0))
	(month (aref datevec 1))
	(day   (aref datevec 2))
	(hour     (aref datevec 3))
	(minute   (aref datevec 4))
	(second   (aref datevec 5))
	(timezone (aref datevec 6))
	day-number p
	day-of-month)
    (setq p 1)
    (setq day-number (- (timezone-day-number month day year)
			offset))
    (while (<= day-number 0)
      (setq year (1- year)
	    day-number (+ (timezone-day-number 12 31 year)
			  day-number)))
    (while (> day-number (setq day-of-month
			       (timezone-last-day-of-month p year)))
      (setq day-number (- day-number day-of-month))
      (setq p (1+ p)))
    (setq month p)
    (setq day day-number)
    (timezone-fix-time
     (format "%d %s %d %s %s"
	     day
	     (car (rassq month timezone-months-assoc))
	     year
	     (if time
		 (format "%d:%d:%d" hour minute second)
	       "0:00")
	     (cadr timezone)) nil nil)))

(defmacro elmo-date-make-sortable-string (datevec)
  "Make a sortable string from DATEVEC."
  `(timezone-make-sortable-date
    (aref ,datevec 0)
    (aref ,datevec 1)
    (aref ,datevec 2)
    (timezone-make-time-string
     (aref ,datevec 3)
     (aref ,datevec 4)
     (aref ,datevec 5))))

(defsubst elmo-datevec-to-time (datevec)
  (encode-time (aref datevec 5) (aref datevec 4) (aref datevec 3)
	       (aref datevec 2) (aref datevec 1) (aref datevec 0)
	       (aref datevec 6)))

(defun elmo-time-parse-date-string (date)
  (ignore-errors
   (elmo-datevec-to-time (timezone-fix-time date nil nil))))

(defun elmo-time-make-date-string (time)
  (let ((system-time-locale "C"))
    (format-time-string "%a, %d %b %Y %T %z" time)))

(defun elmo-time-make-imap-date-string (&optional time)
  (let ((system-time-locale "C"))
    (format-time-string "%d-%b-%Y %T %z" time)))

(defalias 'elmo-time-less-p 'time-less-p)
(make-obsolete 'elmo-time-less-p 'time-less-p "24 May 2020")
(defalias 'elmo-time< 'time-less-p)
(make-obsolete 'elmo-time< 'time-less-p "24 May 2020")

(defun elmo-time-to-days (time)
  (let ((date (decode-time time)))
    (timezone-absolute-from-gregorian
     (nth 4 date) (nth 3 date) (nth 5 date))))

;; from timezone-fix-time in `timezone.el'
(defun elmo-time-to-datevec (time &optional timezone)
  (when time
    (let* ((date   (decode-time time))
	   (year   (nth 5 date))
	   (month  (nth 4 date))
	   (day    (nth 3 date))
	   (hour   (nth 2 date))
	   (minute (nth 1 date))
	   (second (nth 0 date))
	   (local  (nth 8 date))
	   (timezone
	    (or timezone
		(timezone-time-zone-from-absolute
		 (timezone-absolute-from-gregorian month day year)
		 (+ second (* 60 (+ minute (* 60 hour)))))))
	   (diff   (- (timezone-zone-to-minute timezone) (/ local 60)))
	   (minute (+ minute diff))
	   (hour-fix (floor minute 60)))
      (setq hour (+ hour hour-fix))
      (setq minute (- minute (* 60 hour-fix)))
      ;; HOUR may be larger than 24 or smaller than 0.
      (cond ((<= 24 hour)			;24 -> 00
	     (setq hour (- hour 24))
	     (setq day  (1+ day))
	     (when (< (timezone-last-day-of-month month year) day)
	       (setq month (1+ month))
	       (setq day 1)
	       (when (< 12 month)
		 (setq month 1)
		 (setq year (1+ year)))))
	    ((> 0 hour)
	     (setq hour (+ hour 24))
	     (setq day  (1- day))
	     (when (> 1 day)
	       (setq month (1- month))
	       (when (> 1 month)
		 (setq month 12)
		 (setq year (1- year)))
	       (setq day (timezone-last-day-of-month month year)))))
      (vector year month day hour minute second timezone))))

(require 'product)
(product-provide (provide 'elmo-date) (require 'elmo-version))

;;; elmo-date.el ends here
