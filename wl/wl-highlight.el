;;; wl-highlight.el --- Hilight modules for Wanderlust.  -*- lexical-binding: t -*-

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004
;;  Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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

(require 'invisible)
(require 'wl-vars)
(require 'diff-mode)

(provide 'wl-highlight)

(defgroup wl-faces nil
  "Wanderlust, Faces."
  :prefix "wl-highlight-"
  :group 'wl-highlight
  :group 'wl)

(defgroup wl-summary-faces nil
  "Wanderlust, Faces of summary buffer."
  :prefix "wl-highlight-"
  :group 'wl-highlight
  :group 'wl-summary)

(defgroup wl-folder-faces nil
  "Wanderlust, Faces of folder buffer."
  :prefix "wl-highlight-"
  :group 'wl-highlight
  :group 'wl-folder)

(defgroup wl-message-faces nil
  "Wanderlust, Faces of message buffer."
  :prefix "wl-highlight-"
  :group 'wl-highlight)

;; for message header and signature

(defface wl-highlight-message-headers
  '((((type graphic)
      (background dark))
     (:foreground "gray" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "gray" :bold t))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-250" :bold t))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-85" :bold t))
    (((type tty)
      (background dark))
     (:foreground "white" :bold t))
    (((type graphic)
      (background light))
     (:foreground "gray50" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "gray50" :bold t))
    (((type tty)
      (min-colors 16)
      (background light))
     (:foreground "brightblack" :bold t))
    (((type tty)
      (background light))
     (:bold t)))
  "Face used for displaying header names."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-header-contents
  '((((type graphic)
      (background dark))
     (:foreground "LightSkyBlue" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "LightSkyBlue" :bold t))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-117" :bold t))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-43" :bold t))
    (((type tty)
      (background dark))
     (:foreground "cyan" :bold t))
    (((type graphic)
      (background light))
     (:foreground "purple" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "purple" :bold t))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-129" :bold t))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-35" :bold t))
    (((type tty)
      (background light))
     (:foreground "magenta" :bold t)))
  "Face used for displaying header content."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-important-header-contents
  '((((type graphic)
      (background dark))
     (:foreground "yellow" :bold t))
    (((type tty)
      (min-colors 16)
      (background dark))
     (:foreground "brightyellow" :bold t))
    (((type tty)
      (background dark))
     (:foreground "yellow" :bold t))
    (((type graphic)
      (background light))
     (:foreground "brown" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "brown" :bold t))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-124" :bold t))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-32" :bold t))
    (((type tty)
      (background light))
     (:foreground "red" :bold t)))
  "Face used for displaying contents of special headers."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-important-header-contents2
  '((((type graphic)
      (background dark))
     (:foreground "orange" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "orange" :bold t))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-214" :bold t))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-68" :bold t))
    (((type tty)
      (background dark))
     (:foreground "yellow" :bold t))
    (((type graphic)
      (background light))
     (:foreground "DarkSlateBlue" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "DarkSlateBlue" :bold t))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-60" :bold t))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-81" :bold t))
    (((type tty)
      (background light))
     (:foreground "blue" :bold t)))
  "Face used for displaying contents of special headers."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-citation-header
  '((((type graphic)
      (background dark))
     (:foreground "SkyBlue"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "SkyBlue"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-117"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-43"))
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((type graphic)
      (background light))
     (:foreground "DarkGreen"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "DarkGreen"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-22"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-20"))
    (((type tty)
      (background light))
     (:foreground "black")))
  "Face used for displaying header of quoted texts."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-unimportant-header-contents
  '((((type graphic)
      (background dark))
     (:foreground "GreenYellow" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "GreenYellow" :bold t))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-154" :bold t))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-154" :bold t))
    (((type tty)
      (background dark))
     (:foreground "green" :bold t))
    (((type graphic)
      (background light))
     (:foreground "DarkGreen" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "DarkGreen" :bold t))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-22" :bold t))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-20" :bold t))
    (((type tty)
      (background light))
     (:foreground "black" :bold t)))
  "Face used for displaying contents of unimportant headers."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-signature
  '((((type graphic)
      (background dark))
     (:foreground "khaki"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "khaki"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-222"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-77"))
    (((type tty)
      (background dark))
     (:foreground "white"))
    (((type graphic)
      (background light))
     (:foreground "DarkSlateBlue"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "DarkSlateBlue"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-60"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-81"))
    (((type tty)
      (background light))
     (:foreground "blue")))
  "Face used for displaying signature."
  :group 'wl-message-faces
  :group 'wl-faces)

;; for draft

(defface wl-highlight-header-separator-face
  '((((type graphic))
     (:foreground "Black" :background "DarkKhaki"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "Black" :background "DarkKhaki"))
    (((type tty)
      (min-colors 256))
     (:foreground "black" :background "color-143"))
    (((type tty)
      (min-colors 88))
     (:foreground "black" :background "color-57"))
    (((type tty)
      (background dark))
     (:foreground "black" :background "yellow")))
  "Face used for displaying header separator."
  :group 'wl-draft
  :group 'wl-faces)

;; important messages

(defface wl-highlight-summary-flagged-face
  '((((type graphic)
      (background dark))
     (:foreground "orange"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "orange"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-214"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-68"))
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((type graphic)
      (background light))
     (:foreground "purple"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "purple"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-129"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-35"))
    (((type tty)
      (background light))
     (:foreground "magenta")))
  "Face used for displaying flagged messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-new-face
  '((((type graphic))
     (:foreground "tomato"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "tomato"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-203"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-69"))
    (((type tty)
      (min-colors 16))
     (:foreground "brightred"))
    (((type tty))
     (:foreground "red")))
  "Face used for displaying new messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-killed-face
  '((((type graphic)
      (background dark))
     (:foreground "gray"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "gray"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-250"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-285"))
    (((type tty)
      (background dark))
     (:foreground "white"))
    (((type graphic)
      (background light))
     (:foreground "LightSlateGray"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "LightSlateGray"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-102"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-83"))
    (((type tty)
      (min-colors 16)
      (background light))
     (:foreground "brightblack"))
    (((type tty)
      (background light))
     (:foreground "cyan")))
  "Face used for displaying killed messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-displaying-face
  '((t
     (:underline t :bold t)))
  "Face used for displaying message."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-thread-indent-face
  '((((type graphic))
     (:foreground "gray40"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "gray40"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-241"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-81"))
    (((type tty)
      (min-colors 16))
     (:foreground "brightblack"))
    ;; (((type tty))
    ;;  (:foreground "black"))
    )
  "Face used for displaying indented thread."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; unimportant messages

(defface wl-highlight-summary-unread-face
  '((((type graphic)
      (background dark))
     (:foreground "LightSkyBlue"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "LightSkyBlue"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-117"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-43"))
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((type graphic)
      (background light))
     (:foreground "RoyalBlue"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "RoyalBlue"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-62"))
    (((type tty)
      (min-colors 16)
      (background light))
     (:foreground "brightblue"))
    (((type tty)
      (background light))
     (:foreground "cyan")))
  "Face used for displaying unread messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-disposed-face
  '((((type graphic)
      (background dark))
     (:foreground "gray"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "gray"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-250"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-285"))
    (((type tty)
      (background dark))
     (:foreground "white"))
    (((type graphic)
      (background light))
     (:foreground "DarkKhaki"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "DarkKhaki"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-143"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-57"))
    (((type tty)
      (background light))
     (:foreground "yellow")))
  "Face used for displaying messages mark as disposed."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-deleted-face
  '((((type graphic)
      (background dark))
     (:foreground "SteelBlue"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "SteelBlue"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-67"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-38"))
    (((type tty)
      (min-colors 16)
      (background dark))
     (:foreground "brightblack"))
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((type graphic)
      (background light))
     (:foreground "RoyalBlue4"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "RoyalBlue4"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-24"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-17"))
    (((type tty)
      (min-colors 16)
      (background light))
     (:foreground "brightblack"))
    (((type tty)
      (background dark))
     (:foreground "blue")))
  "Face used for displaying messages mark as deleted."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-prefetch-face
  '((((type graphic)
      (background dark))
     (:foreground "DeepSkyBlue"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "DeepSkyBlue"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-39"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-27"))
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((type graphic)
      (background light))
     (:foreground "brown"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "brown"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-124"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-32"))
    (((type tty)
      (background light))
     (:foreground "red")))
  "Face used for displaying messages mark as deleted."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-resend-face
  '((((type graphic))
     (:foreground "orange3"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "orange3"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-172"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-52"))
    (((type tty))
     (:foreground "yellow")))
  "Face used for displaying messages mark as resend."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-refiled-face
  '((((type graphic)
      (background dark))
     (:foreground "blue"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "blue"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-21"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-19"))
    (((type tty)
      (min-colors 16)
      (background dark))
     (:foreground "brightblue"))
    (((type tty)
      (background dark))
     ;; "blue" is too dark.
     (:foreground "magenta"))
    (((type graphic)
      (background light))
     (:foreground "firebrick"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "firebrick"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-124"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-48"))
    (((type tty)
      (background light))
     (:foreground "red")))
  "Face used for displaying messages mark as refiled."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-copied-face
  '((((type graphic)
      (background dark))
     (:foreground "cyan"))
    (((type tty)
      (min-colors 16)
      (background dark))
     (:foreground "brightcyan"))
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((type graphic)
      (background light))
     (:foreground "blue"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "blue"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-21"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-19"))
    (((type tty)
      (background light))
     (:foreground "blue")))
  "Face used for displaying messages mark as copied."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; answered
(defface wl-highlight-summary-answered-face
  '((((type graphic)
      (background dark))
     (:foreground "khaki"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "khaki"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-222"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-73"))
    (((type tty)
      (background dark))
     (:foreground "white"))
    (((type graphic)
      (background light))
     (:foreground "khaki4"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "khaki4"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-101"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-82"))
    (((type tty)
      (background light))
     (:foreground "yellow")))
  "Face used for displaying answered messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; forwarded
(defface wl-highlight-summary-forwarded-face
  '((((type graphic)
      (background dark))
     (:foreground "DarkOliveGreen2"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "DarkOliveGreen2"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-155"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-61"))
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((type graphic)
      (background light))
     (:foreground "DarkOliveGreen4"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "DarkOliveGreen4"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-65"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-81"))
    (((type tty)
      (background light))
     (:foreground "yellow")))
  "Face used for displaying forwarded messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-summary-persistent-mark-face
  '((((type graphic)
      (background dark))
     (:foreground "SeaGreen4"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "SeaGreen4"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-29"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-81"))
    (((type tty)
      (min-colors 16)
      (background dark))
     (:foreground "brightblack"))
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((type graphic)
      (background light))
     (:foreground "SeaGreen1"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "SeaGreen1"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-85"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-45"))
    (((type tty)
      (background light))
     (:foreground "cyan")))
  "Dafault face used for displaying messages with persistent mark."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; obsolete.
(defface wl-highlight-summary-temp-face
  '((((type graphic))
     (:foreground "HotPink1"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "HotPink1"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-205"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-70"))
    (((type tty))
     (:foreground "magenta")))
  "Face used for displaying messages mark as temp."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-target-face
  '((((type graphic))
     (:foreground "HotPink1"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "HotPink1"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-205"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-70"))
    (((type tty))
     (:foreground "magenta")))
  "Face used for displaying messages mark as target."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-low-read-face
  '((((type graphic)
      (background dark))
     (:foreground "PaleGreen" :italic t))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "PaleGreen" :italic t))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-120" :italic t))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-45" :italic t))
    (((type tty)
      (background dark))
     (:foreground "green" :italic t))
    (((type graphic)
      (background light))
     (:foreground "Green3" :italic t))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "Green3" :italic t))
    (((type tty)
      (background light))
     (:foreground "green" :italic t)))
  "Face used for displaying low interest read messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-high-read-face
  '((((type graphic)
      (background dark))
     (:foreground "PaleGreen" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "PaleGreen" :bold t))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-120" :bold t))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-45" :bold t))
    (((type tty)
      (background dark))
     (:foreground "green" :bold t))
    (((type graphic)
      (background light))
     (:foreground "SeaGreen" :bold t))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "SeaGreen" :bold t))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-29" :bold t))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-81" :bold t))
    (((type tty)
      (background light))
     (:foreground "green" :bold t)))
  "Face used for displaying high interest read messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-low-unread-face
  '((((type graphic)
      (background dark))
     (:foreground "LightSkyBlue" :italic t))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "LightSkyBlue" :italic t))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-117" :italic t))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-43" :italic t))
    (((type tty)
      (background dark))
     (:foreground "cyan" :italic t))
    (((type graphic)
      (background light))
     (:foreground "RoyalBlue" :italic t))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "RoyalBlue" :italic t))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-62" :italic t))
    (((type tty)
      (min-colors 16)
      (background light))
     (:foreground "brightblue" :italic t))
    (((type tty)
      (background light))
     (:foreground "cyan" :italic t)))
  "Face used for displaying low interest unread messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-high-unread-face
  '((((type graphic))
     (:foreground "tomato" :bold t))
    (((type tty)
      (min-colors 16777216))
     (:foreground "tomato" :bold t))
    (((type tty)
      (min-colors 256))
     (:foreground "color-203" :bold t))
    (((type tty)
      (min-colors 88))
     (:foreground "color-69" :bold t))
    (((type tty)
      (min-colors 16))
     (:foreground "brightred" :bold t))
    (((type tty))
     (:foreground "red" :bold t)))
  "Face used for displaying high interest unread messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; ordinary messages

(defface wl-highlight-summary-thread-top-face
  '((((type graphic)
      (background dark))
     (:foreground "GreenYellow"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "GreenYellow"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-154"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-60"))
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((type graphic)
      (background light))
     (:foreground "green4"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "green4"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-28"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-20"))
    (((type tty)
      (background light))
     (:foreground "green")))
  "Face used for displaying top thread message."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defface wl-highlight-summary-normal-face
  '((((type graphic)
      (background dark))
     (:foreground "PaleGreen"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "PaleGreen"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-120"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-45"))
    (((type tty)
      (background dark))
     (:foreground "white"))
    (((type graphic)
      (background light))
     (:foreground "SeaGreen"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "SeaGreen"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-29"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-81"))
    (((type tty)
      (background light))
     (:foreground "green")))
  "Face used for displaying normal message."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; folder

(defface wl-highlight-folder-unknown-face
  '((((type graphic)
      (background dark))
     (:foreground "pink"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "pink"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-218"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-74"))
    (((type tty)
      (background dark))
     (:foreground "white"))
    (((type graphic)
      (background light))
     (:foreground "RoyalBlue"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "RoyalBlue"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-62"))
    (((type tty)
      (min-colors 16)
      (background light))
     (:foreground "brightblue"))
    (((type tty)
      (background light))
     (:foreground "cyan")))
  "Face used for displaying unread folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(defface wl-highlight-folder-killed-face
  '((((type graphic))
     (:foreground "gray50"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "gray50"))
    (((type tty)
      (min-colors 16)
      (background dark))
     (:foreground "brightblack")))
  "Face used for displaying killed folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(defface wl-highlight-folder-zero-face
  '((((type graphic)
      (background dark))
     (:foreground "SkyBlue"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "SkyBlue"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-116"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-43"))
    (((type tty)
      (background dark))
     (:foreground "white"))
    (((type graphic)
      (background light))
     (:foreground "BlueViolet"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "BlueViolet"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-92"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-34"))
    (((type tty)
      (min-colors 16)
      (background light))
     (:foreground "brightblue"))
    (((type tty)
      (background light))
     (:foreground "magenta")))
  "Face used for displaying folder needs no sync."
  :group 'wl-folder-faces
  :group 'wl-faces)

(defface wl-highlight-folder-few-face
  '((((type graphic)
      (background dark))
     (:foreground "orange"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "orange"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-214"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-68"))
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((type graphic)
      (background light))
     (:foreground "OrangeRed3"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "OrangeRed3"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-166"))
    (((type tty)
      (background light))
     (:foreground "red")))
  "Face used for displaying folder contains few unsync messages."
  :group 'wl-folder-faces
  :group 'wl-faces)

(defface wl-highlight-folder-many-face
  '((((type graphic)
      (background dark))
     (:foreground "HotPink1"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "HotPink1"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-205"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-70"))
    (((type tty)
      (background dark))
     (:foreground "magenta"))
    (((type graphic)
      (background light))
     (:foreground "tomato"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "tomato"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-203"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-69"))
    (((type tty)
      (min-colors 16)
      (background light))
     (:foreground "brightred"))
    (((type tty)
      (background light))
     (:foreground "red")))
  "Face used for displaying folder contains many unsync messages."
  :group 'wl-folder-faces
  :group 'wl-faces)

(defface wl-highlight-folder-unread-face
  '((((type graphic)
      (background dark))
     (:foreground "gold"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "gold"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-220"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-72"))
    (((type tty)
      (min-colors 16)
      (background dark))
     (:foreground "brightyellow"))
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((type graphic)
      (background light))
     (:foreground "MediumVioletRed"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "MediumVioletRed"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-162"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-49"))
    (((type tty)
      (background light))
     (:foreground "magenta")))
  "Face used for displaying unread folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(defface wl-highlight-folder-opened-face
  '((((type graphic)
      (background dark))
     (:foreground "PaleGreen"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "PaleGreen"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-120"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-45"))
        (((type tty)
      (background dark))
     (:foreground "green"))
    (((type graphic)
      (background light))
     (:foreground "ForestGreen"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "ForestGreen"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-28"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-20"))
    (((type tty)
      (background light))
     (:foreground "green")))
  "Face used for displaying opened group folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(defface wl-highlight-folder-closed-face
  '((((type graphic)
      (background dark))
     (:foreground "GreenYellow"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "GreenYellow"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-154"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-60"))
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((type graphic)
      (background light))
     (:foreground "DarkOliveGreen4"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "DarkOliveGreen4"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-65"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-81"))
    (((type tty)
      (background light))
     (:foreground "yellow")))
  "Face used for displaying closed group folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(defface wl-highlight-folder-path-face
  '((t
     (:bold t :underline t)))
  "Face used for displaying path."
  :group 'wl-folder-faces
  :group 'wl-faces)

(defface wl-highlight-demo-face
  '((((type graphic)
      (background dark))
     (:foreground "#d9ffd9" :background "#004400"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "#d9ffd9" :background "#004400"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-194" :background "color-22"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-62" :background "color-16"))
    (((type tty)
      (background dark))
     (:foreground "white" :background "green"))
    (((type graphic)
      (background light))
     (:foreground "#006600" :background "#d9ffd9"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "#006600" :background "#d9ffd9"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-22" :background "color-194"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-20" :background "color-62"))
    (((type tty)
      (background light))
     (:foreground "black" :background "white")))
  "Face used for displaying demo."
  :group 'wl-faces)

(defface wl-highlight-logo-face
  '((((type graphic)
      (background dark))
     (:foreground "SkyBlue" :background "#004400"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "SkyBlue" :background "#004400"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-116" :background "color-22"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-43" :background "color-16"))
    (((type tty)
      (background dark))
     (:foreground "cyan" :background "black"))
    (((type graphic)
      (background light))
     (:foreground "SteelBlue" :background "#d9ffd9"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "SteelBlue" :background "#d9ffd9"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-67" :background "color-194"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-38" :background "color-62"))
    (((type tty)
      (background light))
     (:foreground "cyan" :background "white")))
  "Face used for displaying demo."
  :group 'wl-faces)

(defface wl-highlight-action-argument-face
  '((((type graphic)
      (background dark))
     (:foreground "pink"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "pink"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-218"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-74"))
    (((type tty)
      (background dark))
     (:foreground "magenta"))
    (((type graphic)
      (background light))
     (:foreground "red"))
    (((type tty)
      (min-colors 16)
      (background light))
     (:foreground "brightred"))
    (((type tty)
      (background light))
     (:foreground "red")))
  "Face used for displaying action argument."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; cited face

(defface wl-highlight-message-cited-text-1
  '((((type graphic)
      (background dark))
     (:foreground "HotPink1"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "HotPink1"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "color-205"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "color-70"))
    (((type tty)
      (background dark))
     (:foreground "magenta"))
    (((type graphic)
      (background light))
     (:foreground "ForestGreen"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "ForestGreen"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "color-28"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "color-20"))
    (((type tty)
      (background light))
     (:foreground "green")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-cited-text-2
  '((((type graphic))
     (:foreground "violet"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "violet"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-213"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-71"))
    (((type tty))
     (:foreground "red"))
    )
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-cited-text-3
  '((((type graphic))
     (:foreground "orchid3"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "orchid3"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-170"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-54"))
    (((type tty))
     (:foreground "magenta")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-cited-text-4
  '((((type graphic))
     (:foreground "purple1"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "purple1"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-99"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-35"))
    (((type tty)
      (min-colors 16))
     (:foreground "brightblue"))
    (((type tty))
     (:foreground "cyan")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-cited-text-5
  '((((type graphic))
     (:foreground "MediumPurple1"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "MediumPurple1"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-141"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-39"))
    (((type tty)
      (min-colors 16))
     (:foreground "brightblue"))
    ;; (((type tty))
    ;;  (:foreground "white"))
    )
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-cited-text-6
  '((((type graphic))
     (:foreground "PaleVioletRed"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "PaleVioletRed"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-168"))
    (((type tty))
     (:foreground "magenta")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-cited-text-7
  '((((type graphic))
     (:foreground "LightPink"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "LightPink"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-217"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-74"))
    ;; (((type tty))
    ;;  (:foreground "white"))
    )
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-cited-text-8
  '((((type graphic))
     (:foreground "salmon"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "salmon"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-209"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-69"))
    (((type tty))
     (:foreground "magenta")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-cited-text-9
  '((((type graphic))
     (:foreground "SandyBrown"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "SandyBrown"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-215"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-69"))
    (((type tty))
     (:foreground "yellow")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-highlight-message-cited-text-10
  '((((type graphic))
     (:foreground "wheat"))
    (((type tty)
      (min-colors 16777216))
     (:foreground "wheat"))
    (((type tty)
      (min-colors 256))
     (:foreground "color-223"))
    (((type tty)
      (min-colors 88))
     (:foreground "color-74"))
    ;; (((type tty))
    ;;  (:foreground "white"))
    )
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-message-header-narrowing-face
  '((((type graphic)
      (background dark))
     (:foreground "white" :background "darkgoldenrod"))
    (((type tty)
      (min-colors 16777216)
      (background dark))
     (:foreground "white" :background "darkgoldenrod"))
    (((type tty)
      (min-colors 256)
      (background dark))
     (:foreground "brightwhite" :background "color-136"))
    (((type tty)
      (min-colors 88)
      (background dark))
     (:foreground "brightwhite" :background "color-52"))
    (((type tty)
      (min-colors 16)
      (background dark))
     (:foreground "brightwhite" :background "yellow"))
    (((type tty)
      (background dark))
     (:foreground "cyan" :background "yellow"))
    (((type graphic)
      (background light))
     (:foreground "black" :background "darkkhaki"))
    (((type tty)
      (min-colors 16777216)
      (background light))
     (:foreground "black" :background "darkkhaki"))
    (((type tty)
      (min-colors 256)
      (background light))
     (:foreground "black" :background "color-143"))
    (((type tty)
      (min-colors 88)
      (background light))
     (:foreground "black" :background "color-57"))
    (((type tty)
      (background light))
     (:foreground "black" :background "yellow")))
  "Face used for header narrowing for the message."
  :group 'wl-message-faces
  :group 'wl-faces)

(defvar wl-highlight-folder-opened-regexp "^ *\\(\\[\\-\\]\\)")
(defvar wl-highlight-folder-closed-regexp "^ *\\(\\[\\+\\]\\)")
(defvar wl-highlight-folder-leaf-regexp "[ ]*\\([-%\\+]\\)\\(.*\\):.*$")

(defvar wl-highlight-citation-face-list
  '(wl-highlight-message-cited-text-1
    wl-highlight-message-cited-text-2
    wl-highlight-message-cited-text-3
    wl-highlight-message-cited-text-4
    wl-highlight-message-cited-text-5
    wl-highlight-message-cited-text-6
    wl-highlight-message-cited-text-7
    wl-highlight-message-cited-text-8
    wl-highlight-message-cited-text-9
    wl-highlight-message-cited-text-10))

(defun wl-delete-all-overlays ()
  "Delete all momentary overlays."
  (let ((overlays (overlays-in (point-min) (point-max)))
	overlay)
    (while (setq overlay (car overlays))
      (if (overlay-get overlay 'wl-momentary-overlay)
	  (delete-overlay overlay))
      (setq overlays (cdr overlays)))))

(defun wl-highlight-summary-displaying ()
  (interactive)
  (wl-delete-all-overlays)
  (let (bol eol ov)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (setq bol (point))
      (setq ov (make-overlay bol eol))
      (overlay-put ov 'face 'wl-highlight-summary-displaying-face)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'wl-momentary-overlay t))))

(defun wl-highlight-folder-group-line (numbers)
  (end-of-line)
  (let ((eol (point))
	bol)
    (beginning-of-line)
    (setq bol (point))
    (let ((text-face (cond ((looking-at wl-highlight-folder-opened-regexp)
			    'wl-highlight-folder-opened-face)
			   ((looking-at wl-highlight-folder-closed-regexp)
			    'wl-highlight-folder-closed-face))))
      (if (and wl-highlight-folder-by-numbers
	       (re-search-forward "[0-9-]+/[0-9-]+/[0-9-]+" eol t))
	  (let* ((unsync (nth 0 numbers))
		 (unread (nth 1 numbers))
		 (face (cond ((and unsync (zerop unsync))
			      (if (and unread (> unread 0))
				  'wl-highlight-folder-unread-face
				'wl-highlight-folder-zero-face))
			     ((and unsync
				   (>= unsync wl-folder-many-unsync-threshold))
			      'wl-highlight-folder-many-face)
			     (t
			      'wl-highlight-folder-few-face))))
	    (if (numberp wl-highlight-folder-by-numbers)
		(progn
		  (put-text-property bol (match-beginning 0) 'face text-face)
		  (put-text-property (match-beginning 0) (match-end 0)
				     'face face))
	      ;; Remove previous face.
	      (put-text-property bol (match-end 0) 'face nil)
	      (put-text-property bol (match-end 0) 'face face)))
	(put-text-property bol eol 'face text-face)))))

(defsubst wl-highlight-get-face-by-name (format &rest args)
  (let ((face (intern (apply #'format format args))))
    (and (facep face)
	 face)))

(defsubst wl-highlight-summary-line-face-spec (status temp-mark indent)
  "Return a cons cell of (face . argument)."
  (or (let (action)
	(and (setq action (assoc temp-mark wl-summary-mark-action-list))
	     (cons (nth 5 action) (nth 2 action))))
      (let ((flags (elmo-message-status-flags status)))
	(cond
	 ((and (string= temp-mark wl-summary-score-over-mark)
	       (or (memq 'new flags) (memq 'unread flags)))
	  '(wl-highlight-summary-high-unread-face))
	 ((and (string= temp-mark wl-summary-score-below-mark)
	       (or (memq 'new flags) (memq 'unread flags)))
	  '(wl-highlight-summary-low-unread-face))
	 ((let ((priorities wl-summary-persistent-mark-priority-list)
		(fl wl-summary-flag-alist)
		result global-flags)
	    (while (and (null result) priorities)
	      (cond
	       ((eq (car priorities) 'killed)
		(when (elmo-message-status-killed-p status)
		  (setq result '(wl-highlight-summary-killed-face))))
	       ((eq (car priorities) 'flag)
		(when (setq global-flags
			    (elmo-get-global-flags flags 'ignore-preserved))
		  (while fl
		    (when (memq (car (car fl)) global-flags)
		      (setq result
			    (list (or (wl-highlight-get-face-by-name
				       "wl-highlight-summary-%s-flag-face"
				       (car (car fl)))
				      'wl-highlight-summary-flagged-face))
			    fl nil))
		    (setq fl (cdr fl)))
		  (unless result
		    (setq result (list 'wl-highlight-summary-flagged-face)))))
	       ((memq (car priorities) flags)
		(setq result
		      (list (or (wl-highlight-get-face-by-name
				 "wl-highlight-summary-%s-face"
				 (car priorities))
				'wl-summary-persistent-mark-face)))))
	      (setq priorities (cdr priorities)))
	    result))
	 ((string= temp-mark wl-summary-score-below-mark)
	  '(wl-highlight-summary-low-read-face))
	 ((string= temp-mark wl-summary-score-over-mark)
	  '(wl-highlight-summary-high-read-face))
	 (t (if indent
		'(wl-highlight-summary-normal-face)
	      '(wl-highlight-summary-thread-top-face)))))))

(autoload 'elmo-flag-folder-referrer "elmo-flag")
(defun wl-highlight-flag-folder-help-echo (folder number)
  (let ((referer (elmo-flag-folder-referrer folder number)))
    (concat "The message exists in "
	    (mapconcat
	     (lambda (pair)
		   (concat (car pair) "/"
			   (number-to-string
			    (cdr pair))))
	     referer ","))))

(require 'wl-summary)
(defun wl-highlight-summary-line-help-echo (number beg end &optional string)
  (let ((type (elmo-folder-type-internal wl-summary-buffer-elmo-folder))
	message handler)
    (when (setq handler (cadr (assq type wl-highlight-summary-line-help-echo-alist)))
      (setq message
	    (funcall handler wl-summary-buffer-elmo-folder number))
      (if message
	  (put-text-property beg end 'help-echo
			     message
			     string)))))

(defun wl-highlight-summary-line-string (number line status temp-mark indent)
  (let ((fsymbol (car (wl-highlight-summary-line-face-spec
		       status
		       temp-mark
		       (> (length indent) 0)))))
    (put-text-property 0 (length line) 'face fsymbol line))
  (when wl-use-highlight-mouse-line
    (put-text-property 0 (length line) 'mouse-face 'highlight line))
  (when wl-highlight-summary-line-help-echo-alist
    (wl-highlight-summary-line-help-echo number 0 (length line) line)))

(defun wl-highlight-summary-current-line (&optional number status)
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  (case-fold-search nil)
	  (deactivate-mark nil)
	  (number (or number (wl-summary-message-number)))
	  bol eol spec)
      (when number
	(setq bol (line-beginning-position))
	(setq eol (line-end-position))
	(wl-summary-selective-display eol bol)
	(goto-char bol)
	(setq spec (wl-highlight-summary-line-face-spec
		    (or status (wl-summary-message-status number))
		    (wl-summary-temp-mark number)
		    (when (eq wl-summary-buffer-view 'thread)
		      (wl-thread-entity-get-parent-entity
		       (wl-thread-get-entity number)))))
	(when (car spec)
	  (put-text-property bol eol 'face (car spec)))
	(when (cdr spec)
	  (put-text-property (next-single-property-change
			      (next-single-property-change
			       bol 'wl-summary-action-argument
			       nil eol)
			      'wl-summary-action-argument nil eol)
			     eol
			     'face
			     'wl-highlight-action-argument-face))
	(when wl-use-highlight-mouse-line
	  (put-text-property bol eol 'mouse-face 'highlight))
	(when wl-highlight-summary-line-help-echo-alist
	  (wl-highlight-summary-line-help-echo number bol eol))))))

(defun wl-highlight-folder (start end)
  "Highlight folder between start and end.
Faces used:
  wl-highlight-folder-unknown-face      unread messages
  wl-highlight-folder-zero-face         folder needs no sync
  wl-highlight-folder-few-face          folder contains few unsync messages
  wl-highlight-folder-many-face         folder contains many unsync messages
  wl-highlight-folder-opened-face       opened group folder
  wl-highlight-folder-closed-face       closed group folder

Variables used:
  wl-highlight-folder-opened-regexp     matches opened group folder
  wl-highlight-folder-closed-regexp     matches closed group folder
"
  (interactive "r")
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region start end)
      (save-restriction
	(goto-char start)
	(while (not (eobp))
	  (wl-highlight-folder-current-line)
	  (forward-line))))))

(require 'wl-folder)
(defun wl-highlight-folder-path (folder-path)
  "Highlight current folder path...overlay"
  (save-excursion
    (wl-delete-all-overlays)
    (let ((fp folder-path) ov)
      (goto-char (point-min))
      (while (and fp
		  (not (eobp)))
	(beginning-of-line)
	(or (looking-at "^[ ]*\\[[\\+-]\\]\\(.+\\):.*\n")
	    (looking-at "^[ ]*\\([^ \\[].+\\):.*\n"))
	(when (equal
	       (get-text-property (point) 'wl-folder-entity-id)
	       (car fp))
	  (setq fp (cdr fp))
	  (setq ov (make-overlay
		    (match-beginning 1)
		    (match-end 1)))
	  (setq wl-folder-buffer-cur-point (point))
	  (overlay-put ov 'face 'wl-highlight-folder-path-face)
	  (overlay-put ov 'evaporate t)
	  (overlay-put ov 'wl-momentary-overlay t))
	(forward-line)))))

(defun wl-highlight-action-argument-string (string)
  (put-text-property 0 (length string) 'face
		     'wl-highlight-action-argument-face
		     string))

(defun wl-highlight-summary-all ()
  "For evaluation"
  (interactive)
  (wl-highlight-summary (point-min)(point-max)))

(defun wl-highlight-summary (start end &optional lazy)
  "Highlight summary between start and end.
Faces used:
  wl-highlight-summary-unread-face      unread messages
  wl-highlight-summary-deleted-face     messages mark as deleted
  wl-highlight-summary-refiled-face     messages mark as refiled
  wl-highlight-summary-copied-face      messages mark as copied
  wl-highlight-summary-new-face         new messages
  wl-highlight-summary-*-flag-face      flagged messages"
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (save-excursion
    (goto-char start)
    (while (and (not (eobp))
		(< (point) end))
      (if (or (not lazy)
	      (null (get-text-property (point) 'face)))
	  (wl-highlight-summary-current-line)
	(let ((inhibit-read-only t)
	      (deactivate-mark nil))
	  (wl-summary-selective-display (line-end-position))))
      (forward-line))
    (unless wl-summary-lazy-highlight
      (message "Highlighting...done"))))

(defun wl-highlight-summary-window (&optional win _beg)
  "Highlight summary window.
This function is defined for `window-scroll-functions'"
  (when wl-summary-highlight
    (with-current-buffer (window-buffer win)
      (when (eq major-mode 'wl-summary-mode)
	(let ((start (window-start win))
	      (end (window-end win t)))
	  (wl-highlight-summary start
				end
				'lazy))
	(set-buffer-modified-p nil)))))

(defun wl-highlight-headers (&optional for-draft)
  (let ((beg (point-min))
	(end (or (save-excursion (re-search-forward "^$" nil t)
				 (point))
		 (point-max))))
    (wl-highlight-message beg end nil)
    (unless for-draft
      (when wl-highlight-x-face-function
	(funcall wl-highlight-x-face-function)))
    (run-hooks 'wl-highlight-headers-hook)))

(defun wl-highlight-body ()
  (let ((beg (or (save-excursion (goto-char (point-min))
				 (re-search-forward "^$" nil t))
		 (point-min)))
	(end (point-max)))
    (wl-highlight-message beg end t)))

(defun wl-highlight-body-region (beg end)
  (wl-highlight-message beg end t t))

(defun wl-highlight-signature-search-simple (beg end)
  "Search signature area in the body message between BEG and END.
Returns start point of signature."
  (save-excursion
    (goto-char end)
    (if (re-search-backward "\n--+ *\n" beg t)
	(if (eq (following-char) ?\n)
	    (1+ (point))
	  (point))
      end)))

(defun wl-highlight-signature-search (beg end)
  "Search signature area in the body message between BEG and END.
Returns start point of signature."
  (save-excursion
    (goto-char end)
    (or
     ;; look for legal signature separator (check at first for fasten)
     (search-backward "\n-- \n" beg t)

     ;; look for dual separator
     (let ((pt (point))
	   separator)
       (prog1
	   (and (re-search-backward "^[^A-Za-z0-9> \t\n]+ *$" beg t)
		;; `10' is a magic number.
		(> (- (match-end 0) (match-beginning 0)) 10)
		(setq separator (buffer-substring (match-beginning 0)
						  (match-end 0)))
		;; We should not use `re-search-backward' for a long word
		;; since it is possible to crash XEmacs because of a bug.
		(if (search-backward (concat "\n" separator "\n") beg t)
		    (1+ (point))
		  (and (search-backward (concat separator "\n") beg t)
		       (bolp)
		       (point))))
	 (goto-char pt)))

     ;; look for user specified signature-separator
     (if (stringp wl-highlight-signature-separator)
	 (re-search-backward wl-highlight-signature-separator nil t);; case one string
       (let ((sep wl-highlight-signature-separator))		;; case list
	 (while (and sep
		     (not (re-search-backward (car sep) beg t)))
	   (setq sep (cdr sep)))
	 (point)))	;; if no separator found, returns end.
     )))

(defun wl-highlight-citation-prefix-index (prefix)
  "Return a face index for a given citation prefix"
  (apply '+ (mapcar (lambda (ch)
                      (cond
                        ((memq ch '(?> ?| ?: ?})) 1)
                        ((memq ch '(9 32)) 0)
                        (t ch)))
		    prefix)))

(defun wl-highlight-message (start end hack-sig &optional body-only)
  "Highlight message headers between start and end.
Faces used:
  wl-highlight-message-headers			  the part before the colon
  wl-highlight-message-header-contents		  the part after the colon
  wl-highlight-message-important-header-contents  contents of \"important\"
                                                  headers
  wl-highlight-message-important-header-contents2 contents of \"important\"
                                                  headers
  wl-highlight-message-unimportant-header-contents contents of unimportant
                                                   headers
  wl-highlight-message-cited-text-N	           quoted text from other
                                                   messages
  wl-highlight-message-citation-header             header of quoted texts
  wl-highlight-message-signature                   signature

Variables used:
  wl-highlight-message-header-alist             alist of header regexp with
                                                face for header contents
  wl-highlight-citation-prefix-regexp		matches lines of quoted text
  wl-highlight-force-citation-header-regexp	matches headers for quoted text
  wl-highlight-citation-header-regexp		matches headers for quoted text

If HACK-SIG is true,then we search backward from END for something that
looks like the beginning of a signature block, and don't consider that a
part of the message (this is because signatures are often incorrectly
interpreted as cited text.)"
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (let ((too-big (and wl-highlight-max-message-size
		      (> (- end start) wl-highlight-max-message-size)))
	(real-end end)
	current
	p hend
	wl-draft-real-time-highlight)
    (unless too-big
      (save-excursion
	(save-restriction
	  ;; take off signature
	  (when hack-sig
	    (setq end (funcall wl-highlight-signature-search-function
			       (- end wl-max-signature-size) end))
	    (when (not (eq end real-end))
	      (put-text-property end (point-max)
				 'face 'wl-highlight-message-signature)))
	  (narrow-to-region start end)
	  ;; narrow down to just the headers...
	  (goto-char start)
	  (unless body-only
	    (save-restriction
	      ;; If this search fails then the narrowing performed above
	      ;; is sufficient
	      (if (re-search-forward
		   (format "^\\(%s\\)?$" (regexp-quote mail-header-separator))
		   nil t)
		  (narrow-to-region (point-min) (match-beginning 0)))
	      ;; highlight only when header is not too-big.
	      (if  (and wl-highlight-max-header-size
			(>= (point) wl-highlight-max-header-size))
		  (goto-char (point-max))
		(goto-char start)
		(while (not (eobp))
		  (if (looking-at "^[^ \t\n:]+[ \t]*:[ \t]*")
		      (progn
			(setq p (match-end 0))
			(put-text-property (match-beginning 0) p
					   'face 'wl-highlight-message-headers)
			(setq hend (save-excursion (std11-field-end end)))
			(put-text-property
			 p hend 'face
			 (catch 'match
			   (let ((regexp-alist wl-highlight-message-header-alist))
			     (while regexp-alist
			       (when (looking-at (caar regexp-alist))
				 (throw 'match (cdar regexp-alist)))
			       (setq regexp-alist (cdr regexp-alist))))
			   'wl-highlight-message-header-contents))
			(goto-char hend))
		    ;; ignore non-header field name lines
		    (forward-line))))))
	  (when (looking-at
		 (format "^%s$" (regexp-quote mail-header-separator)))
	    (put-text-property (match-beginning 0) (match-end 0)
			       'face 'wl-highlight-header-separator-face)
	    (forward-line))
	  (let ((wl-highlight-text/diff
		 (wl-mime-enabled-major-mode-p wl-highlight-text/diff))
		prefix end)
	    (while (null (progn
			     ;; Skip invisible region.
			   (when (invisible-p (point))
			     (goto-char (next-visible-point (point))))
			   (eobp)))
	      (cond
	       ((and
		 wl-highlight-text/diff
		 (looking-at
		  "^--- .*\n\\+\\+\\+ .*\n\\([-+ ].*\n\\|@@ .* @@.*\n\\)+"))
		(save-restriction
		  (narrow-to-region (match-beginning 0) (match-end 0))
		  (let ((font-lock-defaults diff-font-lock-defaults))
		    (font-lock-ensure))
		  (goto-char (point-max)))
		(setq current nil))
	       ((and wl-highlight-force-citation-header-regexp
		     (looking-at wl-highlight-force-citation-header-regexp))
		(setq current 'wl-highlight-message-citation-header)
		(setq end (match-end 0)))
	       ((and wl-highlight-citation-prefix-regexp
		     (looking-at wl-highlight-citation-prefix-regexp))
		(setq prefix (buffer-substring (point)
					       (match-end 0)))
		(unless wl-highlight-highlight-citation-too
		  (goto-char (match-end 0)))
		(setq current
                      (nth (% (wl-highlight-citation-prefix-index prefix)
                              (length wl-highlight-citation-face-list))
                           wl-highlight-citation-face-list)))
	       ((and wl-highlight-citation-header-regexp
		     (looking-at wl-highlight-citation-header-regexp))
		(setq current 'wl-highlight-message-citation-header)
		(setq end (match-end 0)))
	       (t (setq current nil)))
	      (when current
		(setq p (point))
		(forward-line) ; this is to put the \n in the face too
		(put-text-property p (or end (point)) 'face current)
		(setq end nil)
		(backward-char))
	      (forward-line)))
	  (run-hooks 'wl-highlight-message-hook))))))

;; highlight-mouse-line for folder mode

(defun wl-highlight-folder-mouse-line ()
  (interactive)
  (let* ((end (save-excursion (end-of-line) (point)))
	 (beg (progn
		(re-search-forward "[^ ]" end t)
		(1- (point))))
	 (inhibit-read-only t))
    (put-text-property beg end 'mouse-face 'highlight)))


(require 'product)
(product-provide (provide 'wl-highlight) (require 'wl-version))

;;; wl-highlight.el ends here
