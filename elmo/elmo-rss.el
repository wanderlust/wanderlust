;;; elmo-rss --- RSS, Atom and OPML support for Wanderlust

;;; Copyright (c) 2014, 2015 Juliusz Chroboczek <jch@pps.univ-paris-diderot.fr>

;; Author: Juliusz Chroboczek <jch@pps.univ-paris-diderot.fr>
;; Keywords: Wanderlust
;; Version: 0.1

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;; Commentary:
;;
;; An Atom or RSS feed appears as a folder in Wanderlust:
;;
;;   m a rss:https://github.com/wanderlust/wanderlust/commits/master.atom
;;
;; In order to cache previously downloaded messages, use a pipe folder:
;;
;;   m a |rss:http://lwn.net/headlines/newrss|+lwn
;;
;; An Atom, RSS or OPML feed can be used for an access group:
;;
;;    m A rss:http://planet.gnome.org/atom.xml
;;
;;
;; We don't do XML namespaces right -- we simply compare the tag prefixes
;; against a well-known list, rather than doing the whole binding dance.
;; If you use inhabitual prefixes in your feed, you deserve what you get.

;;; Code:

(require 'elmo)
(require 'elmo-map)
(require 'url)
(require 'xml)

(eval-and-compile
  (luna-define-class elmo-rss-folder (elmo-map-folder)
                     (url downloaded entries children))
  (luna-define-internal-accessors 'elmo-rss-folder)
  )

(defcustom elmo-rss-use-raw-utf8-in-headers nil
  "*Whether to use raw UTF-8 in headers of RSS messages.
Setting this to true will annoy the pedants."
  :type 'boolean
  :group 'elmo)

(defun elmo-rss-id-to-message-id (id url)
  "Convert an Atom/RSS id into something suitable for use as a Message-ID."
  (let* ((host (or (ignore-errors (url-host (url-generic-parse-url url)))
                  "unknown"))
         ;; this should probably be improved in order to generate readable
         ;; IDs more often.
         (id* (if (string-match "\\`tag:\\(.*\\)\\'" id)
                  (match-string 1 id)
                  id))
         (msg-id (concat "<" id* "@" host ">")))
    (if (std11-parse-msg-id-string msg-id)
        msg-id
        (concat "<" (sha1 id) "@" host ">"))))

(defun elmo-rss-parse-iso-timeoffset (string)
  (cond
    ((null string) nil)
    ((equal string "Z") '(0 "utc"))
    ((string-match "\\`[+-]\\([0-9]+\\):\\([0-9]+\\)\\'" string)
     (list
      (*
       (if (eql (aref string 0) ?-) -1 1)
       (+ (* 3600 (string-to-number (match-string 1 string)))
          (* 60 (string-to-number (match-string 2 string)))))
      nil))
    (t nil)))

(defun elmo-rss-parse-iso-date (string)
  "Convert a date in ISO 8601 format into Internet Mail format."
  (and string
       (and (string-match
             ;; Ugh.
             "\\`\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)t\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)\.?[0-9]*\\([z+-][0-9:]*\\)?\\'"
             string)
            (timezone-make-arpa-date
             (string-to-number (match-string 1 string))
             (string-to-number (match-string 2 string))
             (string-to-number (match-string 3 string))
             (format "%s:%s:%s"
                     (match-string 4 string)
                     (match-string 5 string)
                     (match-string 6 string))
             (elmo-rss-parse-iso-timeoffset (match-string 7 string))))))

(defun elmo-rss-utf-8 (string)
  "Decode string into UTF-8.  String may be nil."
  (and string (decode-coding-string string 'utf-8 t)))

(defun elmo-rss-whitespace-p (string)
  (and (stringp string)
       (string-match "\\`[ \t\n]*\\'" string)))

;; Emacs' XML library preserves whitespace, which it is then unable to
;; format properly.  This partly works around the issue.

(defun elmo-rss-trim-xml (xml)
  (when (consp xml)
    (when (elmo-rss-whitespace-p (car xml))
      (setq xml (cdr xml)))
    (when (elmo-rss-whitespace-p (car (last xml)))
      (setq xml (butlast xml))))
  xml)

(defun elmo-rss-format-string (xml)
  (setq xml (elmo-rss-trim-xml xml))
  (let ((string
         (cond
           ((atom xml) xml)
           ((null (cdr xml)) (car xml)))))
    (if (stringp string)
        string
        ;; This should not happen, but let's be defensive against the case
        ;; when somebody puts XML nodes within HTML content.
        (format "%s" string))))

(defun elmo-rss-format-xml (xml)
  (setq xml (elmo-rss-trim-xml xml))
  (with-temp-buffer
    (condition-case nil
        (xml-print xml)
      (error
       (message "Unable to format XML parse tree")
       (insert (format "%s" xml))))
    (buffer-substring (point-min) (point-max))))

(defun elmo-rss-atom-link-interesting (node &optional kind)
  (and
   (consp node)
   (member (xml-node-name node) '(link atom10:link))
   (not (equal (xml-get-attribute-or-nil node 'href) ""))
   (member (xml-get-attribute-or-nil node 'type)
           '("application/atom+xml" "application/rss+xml"))
   (member (xml-get-attribute-or-nil node 'rel)
           (if kind (list kind) '("self" "related" "replies")))))

(defun elmo-rss-atom-link-web (node)
  (and
   (consp node)
   (member (xml-node-name node) '(link atom10:link))
   (not (equal (xml-get-attribute-or-nil node 'href) ""))
   (not (member (xml-get-attribute-or-nil node 'type)
                '("application/atom+xml" "application/rss+xml")))
   (member (xml-get-attribute-or-nil node 'rel)
           '(nil "alternate" "enclosure"))))


(defun elmo-rss-parse-atom-entry (body &optional url)
  "Parse one Atom entry."
  (let (id published updated author title in-reply-to links content summary
        urls)
    (dolist (node body)
      (when (consp node)
        (let ((name (xml-node-name node))
              (child (car (xml-node-children node))))
          (cond
            ((eq name 'id) (setq id child))
            ((eq name 'published) (setq published child))
            ((eq name 'updated) (setq updated child))
            ((eq name 'author) (setq author node))
            ((eq name 'title) (setq title child))
            ((eq name 'thr:in-reply-to)
             (let ((ref (xml-get-attribute-or-nil node 'ref)))
               (when ref (push ref in-reply-to))))
            ((eq name 'link)
             (cond
               ((elmo-rss-atom-link-interesting node)
                (push (xml-get-attribute node 'href) urls))
               ((elmo-rss-atom-link-web node)
                (push (xml-get-attribute node 'href) links))))
            ((eq name 'source)
             (dolist (child (xml-node-children node))
               (when (elmo-rss-atom-link-interesting child "self")
                 (push (xml-get-attribute child 'href) urls))))
            ((eq name 'content) (setq content node))
            ((eq name 'summary) (setq summary node))))))
    (let* ((date (or (elmo-rss-parse-iso-date updated)
                     (elmo-rss-parse-iso-date published)))
           (author-name
            (and author
                 (car (xml-node-children
                       (assoc 'name (xml-node-children author))))))
           (author-email
            (and author
                 (car (xml-node-children
                       (assoc 'email (xml-node-children author))))))
           (content* (or content summary))
           (content-type (and content*
                              (xml-get-attribute-or-nil content* 'type)))
           (mime-content-type
            (cond
              ((null content-type) "text/plain")
              ((member content-type '("html" "xhtml")) "text/html")
              ;; This is incorrect, we should be checking for XML and
              ;; doing the Base64 dance otherwise.
              ((string-match "/" content-type) content-type)
              (t "text/plain")))
           (content-body
            (if (equal "xhtml" content-type)
                (elmo-rss-format-xml (xml-node-children content*))
                (elmo-rss-format-string (xml-node-children content*)))))
      (list
       (list
        ;; id is compulsory in Atom, but I'm paranoid.
        (or id (sha1 (concat date "-" title "-" (car links))))
        date
        (cond
          ((and author-name author-email)
           (concat (elmo-rss-utf-8 author-name) " <" author-email ">"))
          (author-name (elmo-rss-utf-8 author-name))
          (author-email author-email))
        (elmo-rss-utf-8 title)
        (nreverse in-reply-to)
        (nreverse links)
       mime-content-type
       content-body)
       urls))))

(defun elmo-rss-parse-atom (feed &optional url)
  "Parse an Atom feed, return a list of entries."
  (let ((children (xml-node-children feed))
        (entries nil)
        (urls nil)
        (extra-urls nil))
    (dolist (node children)
      (when (listp node)
        (cond
          ((eql 'link (xml-node-name node))
           (when (elmo-rss-atom-link-interesting node)
             (push (xml-get-attribute node 'href) urls)))
          ((eql 'entry (xml-node-name node))
           (let ((parse (elmo-rss-parse-atom-entry
                         (xml-node-children node) url)))
             (when (car parse)
               (push (car parse) entries))
             (dolist (u (cadr parse))
               (push u extra-urls)))))))
    ;; feeds are in reverse chronological order, we've reversed them already
    (list entries (nconc (nreverse urls) extra-urls))))

(defun elmo-rss-parse-rss-entry (body &optional url)
  "Parse one RSS entry."
  (let (guid pubdate dc-date author dc-creator title links
           content-encoded description urls)
    (dolist (node body)
      (when (consp node)
        (let ((name (xml-node-name node))
              (child (car (xml-node-children node))))
          (cond
            ((eq name 'guid) (setq guid child))
            ((eq name 'pubDate) (setq pubdate child))
            ((eq name 'dc:date) (setq dc-date child))
            ((eq name 'author) (setq author child))
            ((eq name 'dc:creator) (setq dc-creator child))
            ((eq name 'title) (setq title child))
            ((eq name 'link) (push child links))
            ((eq name 'enclosure)
             (when (not (equal (xml-get-attribute node 'url) ""))
               (push (xml-get-attribute node 'url) links)))
            ((eq name 'source)
             (when (not (equal (xml-get-attribute node 'url) ""))
               (push (xml-get-attribute node 'url) urls)))
            ((eq name 'wfw:commentRss)
             (push child urls))
            ((eq name 'content:encoded) (setq content-encoded child))
            ((eq name 'description) (setq description child))))))
    (let ((date (or pubdate (elmo-rss-parse-iso-date dc-date))))
      (list
       (list
        ;; guid is optional in RSS.
        (or guid (sha1 (concat date "-" title "-" (car links))))
        date
        (elmo-rss-utf-8 (or author dc-creator))
        (elmo-rss-utf-8 title)
        nil
        (nreverse links)
        "text/html"
        (or content-encoded description))
       urls))))

(defun elmo-rss-parse-rss (channel &optional container url)
  "Parse an RSS feed, return a list of entries."
  (let ((children (append (xml-node-children channel)
                          (and container (xml-node-children container))))
        (entries nil)
        (urls nil)
        (extra-urls nil))
    (dolist (node children)
      (when (listp node)
        (cond
          ((eql 'atom10:link (xml-node-name node))
           (when (elmo-rss-atom-link-interesting node)
             (push (xml-get-attribute node 'href) urls)))
          ((eql 'item (xml-node-name node))
           (let ((parse (elmo-rss-parse-rss-entry
                         (xml-node-children node) url)))
             (when (car parse)
               (push (car parse) entries))
             (dolist (u (cadr parse))
               (push u extra-urls)))))))
    (list entries (nconc (nreverse urls) extra-urls))))

(defun elmo-rss-parse-opml (body &optional _url)
  (let ((children (xml-node-children body))
        (urls nil))
    (dolist (child children)
      (when (and (listp child) (eql 'outline (xml-node-name child)))
        (when (equal "rss" (xml-get-attribute-or-nil child 'type))
          (let ((url (xml-get-attribute-or-nil child 'xmlUrl)))
            (when url
              (push url urls))))
        (dolist (url* (cadr (elmo-rss-parse-opml child)))
          (push url* urls))))
    (list nil (nreverse urls))))

(defun elmo-rss-parse (body &optional url)
  "Parse a feed (Atom or RSS), return a list of entries."
  (let* ((feed (assoc 'feed body))
         (entry (assoc 'entry body))
         (rss (assoc 'rss body))
         (rdf (assoc 'rdf:RDF body))
         (channel (and (or rss rdf)
                       (assoc 'channel (xml-node-children (or rss rdf)))))
         (opml (assoc 'opml body))
         (opml-body (and opml (assoc 'body (xml-node-children opml)))))
    (cond
      ;; Atom feed
      (feed (elmo-rss-parse-atom feed url))
      ;; RSS feed
      ((and rss channel) (elmo-rss-parse-rss channel nil url))
      ((and rdf channel) (elmo-rss-parse-rss channel rdf url))
      ;; Single Atom entry
      (entry (elmo-rss-parse-atom (cons 'feed (cons nil (list entry)))))
      ;; OPML outline
      (opml-body (elmo-rss-parse-opml opml-body url))
      (t (error "Couldn't find Atom, RSS or OPML at %s." url)))))

(defun elmo-rss-download (folder)
  "Download the RSS feed and parse it."
  (let* ((url (elmo-rss-folder-url-internal folder))
         (buffer (or (url-retrieve-synchronously url)
                     (error "No data for feed %s" url)))
         (xml
          (prog1
              (with-current-buffer buffer
                (xml-parse-region (progn (goto-char (point-min))
					 (search-forward "\n\n"))
				  (point-max)))
            (kill-buffer buffer)))
         (parse (elmo-rss-parse xml url))
         (entries (car parse))
         (urls (cadr parse)))
    (elmo-rss-folder-set-downloaded-internal folder t)
    (elmo-rss-folder-set-entries-internal folder entries)
    (elmo-rss-folder-set-children-internal folder urls)))

(defun elmo-rss-maybe-download (folder)
  "Call elmo-rss-download if there is no ready data."
  (unless (elmo-rss-folder-downloaded-internal folder)
    (if (elmo-folder-plugged-p folder)
        (elmo-rss-download folder)
        (error "Unplugged"))))

(luna-define-method elmo-folder-initialize ((folder elmo-rss-folder) name)
    (elmo-rss-folder-set-url-internal folder name)
    (elmo-rss-folder-set-downloaded-internal folder nil)
    (elmo-rss-folder-set-entries-internal folder nil)
    (elmo-rss-folder-set-children-internal folder nil)
  folder)

(luna-define-method elmo-folder-local-p ((folder elmo-rss-folder))
  nil)

(luna-define-method elmo-message-use-cache-p ((folder elmo-rss-folder)
                                              number)
  t)

(luna-define-method elmo-folder-close-internal :after ((folder elmo-rss-folder))
  (elmo-rss-folder-set-downloaded-internal folder nil)
  (elmo-rss-folder-set-entries-internal folder nil)
  (elmo-rss-folder-set-children-internal folder nil))

(luna-define-method elmo-folder-exists-p ((folder elmo-rss-folder))
  t)

(luna-define-method elmo-folder-plugged-p ((folder elmo-rss-folder))
  (elmo-plugged-p))

(luna-define-method elmo-map-folder-list-message-locations
    ((folder elmo-rss-folder))
  (cond
    ((elmo-folder-plugged-p folder)
     ;; Re-download unconditionally, so that sync does the right thing.
     (elmo-rss-download folder))
    ((not (elmo-rss-folder-downloaded-internal folder))
     (error "Unplugged")))
  (mapcar #'car (elmo-rss-folder-entries-internal folder)))

(defun elmo-rss-encode-field-body (body)
  "Encode a header body depending on elmo-rss-use-raw-utf8-in-headers."
  (if elmo-rss-use-raw-utf8-in-headers
      (encode-coding-string body 'utf-8 t)
      ;; since From fields don't necessarily have the syntax of an e-mail
      ;; address, we always encode them as an unstructured field.
      (mime-encode-field-body body "Subject")))

(defun elmo-rss-format-message (entry url)
  "Format a parsed entry as a mail message."
  (let* ((id (car entry))
         (date (cadr entry))
         (author (nth 2 entry))
         (subject (nth 3 entry))
         (in-reply-to (nth 4 entry))
         (links (nth 5 entry))
         (content-type (nth 6 entry))
         (body (nth 7 entry))
         (time (current-time))
         (boundary (format "%d-%d" (cadr time) (nth 2 time))))
    (when author
      (insert "From: " (elmo-rss-encode-field-body author) "\n"))
    (when subject
      (insert "Subject: " (elmo-rss-encode-field-body subject) "\n"))
    (when date (insert "Date: " date "\n"))
    (insert "Message-Id: " (elmo-rss-id-to-message-id id url) "\n")
    (when in-reply-to
      (insert "References:")
      (dolist (r in-reply-to)
        (insert " " (elmo-rss-id-to-message-id r url)))
      (insert "\n"))
    (insert "Mime-Version: 1.0\n")
    (when (and body links)
      (insert "Content-type: multipart/mixed; boundary=\"" boundary "\"\n")
      (insert "\n--" boundary "\n"))
    (when body
      (insert "Content-Type: " content-type "; charset=utf-8\n")
      (insert "\n")
      (insert body)
      (unless (bolp) (insert "\n")))
    (when (and body links)
      (insert "--" boundary "\n"))
    (when links
      (insert "Content-Type: text/plain\n")
      (insert "\n")
      (dolist (l links)
        (insert "<" l ">\n")))
    (when (and body links)
      (insert "--" boundary "--\n"))
    (unless (bolp) (insert "\n"))))

(luna-define-method elmo-map-message-fetch ((folder elmo-rss-folder)
					    location _strategy
					    &optional _section _unseen)
  (elmo-rss-maybe-download folder)
  (let ((entry (assoc location
                      (elmo-rss-folder-entries-internal folder))))
    (set-buffer-multibyte nil)
    (elmo-rss-format-message entry (elmo-rss-folder-url-internal folder)))
  t)

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-rss-folder))
  (expand-file-name
   (elmo-replace-string-as-filename (elmo-folder-name-internal folder))
   (expand-file-name "rss" elmo-msgdb-directory)))

(defun elmo-rss-msgdb-create-entity (msgdb folder number)
  (let* ((url (elmo-rss-folder-url-internal folder))
         (location (elmo-map-message-location folder number))
         (entry (assoc location
                       (elmo-rss-folder-entries-internal folder))))
    (elmo-msgdb-make-message-entity
     (elmo-msgdb-message-entity-handler msgdb)
     :message-id (elmo-rss-id-to-message-id location url)
     :number number
     :date (cadr entry)
     :from (or (nth 2 entry) elmo-no-from)
     :subject (or (nth 3 entry) elmo-no-subject)
     :references (mapcar #'(lambda (ref) (elmo-rss-id-to-message-id ref url))
                         (nth 4 entry)))))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-rss-folder)
					      numlist _flag-table)
  (elmo-rss-maybe-download folder)
  (let ((new-msgdb (elmo-make-msgdb)))
    (elmo-with-progress-display (elmo-folder-msgdb-create (length numlist))
	"Creating msgdb"
      (dolist (number numlist)
        (let ((entity (elmo-rss-msgdb-create-entity new-msgdb folder number)))
          (when entity
            (elmo-msgdb-append-entity new-msgdb entity '(new unread))))
        (elmo-progress-notify 'elmo-folder-msgdb-create)))
    new-msgdb))

(luna-define-method elmo-folder-delete-messages ((folder elmo-rss-folder)
						 numbers)
  (elmo-folder-kill-messages folder numbers)
  t)

(luna-define-method elmo-folder-list-subfolders ((folder elmo-rss-folder)
                                                 &optional _one-level)
  (elmo-rss-maybe-download folder)
  (mapcar #'(lambda (url) (concat "rss:" url))
          (nconc
           (and (elmo-rss-folder-entries-internal folder)
                (not (member (elmo-rss-folder-url-internal folder)
                             (elmo-rss-folder-children-internal folder)))
                (list (elmo-rss-folder-url-internal folder)))
           (elmo-rss-folder-children-internal folder))))

(provide 'elmo-rss)

;;; elmo-rss.el ends here
