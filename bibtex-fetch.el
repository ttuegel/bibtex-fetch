;;; bibtex-fetch.el --- Fetch BibTeX entries and documents from common sources  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Thomas Tuegel

;; Author: Thomas Tuegel <ttuegel@duo>
;; Keywords: bib

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(require 'bibtex)
(require 'bibtex-print)
(require 'helm-utils)
(require 'org)
(require 'rx)
(require 'select)
(require 'url-util)
(require 'xml)

(require 'bibtex-fetch-acs)
(require 'bibtex-fetch-aip)
(require 'bibtex-fetch-aps)
(require 'bibtex-fetch-arxiv)
(require 'bibtex-fetch-doi)
(require 'bibtex-fetch-iop)
(require 'bibtex-fetch-jps)
(require 'bibtex-fetch-nature)
(require 'bibtex-fetch-nature-physics)
(require 'bibtex-fetch-sciencedirect)
(require 'bibtex-fetch-springer)

(defvar-local bibtex-fetch/document-path
  nil
  "The directory containing the documents associated with the bibliography.

Each bibliography entry may have an associated document. The filename of the
document is the same as the entry key, plus the appropriate extension.")

(defvar bibtex-fetch-entry-handlers
  (list (cons bibtex-fetch/arxiv-rx #'bibtex-fetch/arxiv-entry)
        (cons bibtex-fetch/doi-rx #'bibtex-fetch/doi-entry)
        (cons bibtex-fetch/aps-rx #'bibtex-fetch/aps-entry)
        (cons bibtex-fetch/acs-rx #'bibtex-fetch/acs-entry)
        (cons bibtex-fetch/iop-rx #'bibtex-fetch/iop-entry)
        (cons bibtex-fetch/sciencedirect-rx #'bibtex-fetch/sciencedirect-entry)
        (cons bibtex-fetch/springer-rx #'bibtex-fetch/springer-entry)
        (cons bibtex-fetch/jps-rx #'bibtex-fetch/jps-entry)
        (cons bibtex-fetch/nature-rx #'bibtex-fetch/nature-entry)
        (cons bibtex-fetch/nature-physics-rx #'bibtex-fetch/nature-physics-entry)
        (cons bibtex-fetch/aip-rx #'bibtex-fetch/aip-entry))
  "The list of handlers to use to fetch a BibTeX entry from a URL.

Each handler is a pair of a regular expression and a function that will be
called when the URL matches the regular expression. The function takes no
arguments, but it may assume that `match-data' is set.")

(defvar bibtex-fetch-document-handlers
  (list (cons bibtex-fetch/arxiv-rx #'bibtex-fetch/arxiv-document)
        (cons bibtex-fetch/doi-rx #'bibtex-fetch/doi-document)
        (cons bibtex-fetch/aps-rx #'bibtex-fetch/aps-document)
        (cons bibtex-fetch/acs-rx #'bibtex-fetch/acs-document)
        (cons bibtex-fetch/iop-rx #'bibtex-fetch/iop-document)
        (cons bibtex-fetch/springer-rx #'bibtex-fetch/springer-document)
        (cons bibtex-fetch/jps-rx #'bibtex-fetch/jps-document)
        (cons bibtex-fetch/nature-rx #'bibtex-fetch/nature-document)
        (cons bibtex-fetch/nature-physics-rx #'bibtex-fetch/nature-physics-document)
        (cons bibtex-fetch/aip-rx #'bibtex-fetch/aip-document))
  "The handlers used to fetch a document from a URL stored in a BibTeX entry.

Each handler is a pair of a regular expression and a function that will be
called when the URL matches the regular expression. The function takes two
arguments, the URL and the destination for the file.")

(defun bibtex-fetch/run-entry-handler (url handler)
  (let* ((handler-rx (car handler))
         (handler-fun (cdr handler)))
    (when (string-match handler-rx url)
      (funcall handler-fun url))))

(defun bibtex-fetch/from-url (url)
  "Fetch the BibTeX entry for the document at URL."
  (interactive "MURL: ")
  (let* ((handlers bibtex-fetch-entry-handlers) entry)
    (while (and (not entry) handlers)
      (setq entry (bibtex-fetch/run-entry-handler url (pop handlers))))
    (if (not entry)
        (error "No handler found to fetch entry")
      entry)))

(defun bibtex-fetch/entry-from-clipboard ()
  "Fetch the BibTeX entry for the URL on the system clipboard."
  (bibtex-fetch/from-url (url-unhex-string (gui-get-selection 'CLIPBOARD))))

(defun bibtex-fetch/run-document-handler (url dest handler)
  (let* ((handler-rx (car handler))
         (handler-fun (cdr handler))
         (final-dest (expand-file-name dest)))
    (when (string-match handler-rx url)
      (make-directory (file-name-directory final-dest) t)
      (funcall handler-fun url final-dest)
      t)))

(defun bibtex-fetch/document-file-name (key &optional document-path)
  "Determine the full path and file name of the document associated with KEY."
  (unless (or document-path bibtex-fetch/document-path)
    (error "Please set `bibtex-fetch/document-path'"))
  (expand-file-name
   (s-concat (or document-path bibtex-fetch/document-path) "/" key ".pdf")))

(defun bibtex-fetch/document-from-url (url dest)
  "Fetch the document corresponding to URL into DEST."
  (let* ((handlers bibtex-fetch-document-handlers) matched)
    (while (and (not matched) handlers)
      (setq matched
            (bibtex-fetch/run-document-handler url dest (pop handlers))))
    (when (not matched)
      (error "No handler found to fetch document"))))

(defun bibtex-fetch/document-from-entry (entry document-path)
  "Fetch the document corresponding to ENTRY and retrieve it into DOCUMENT-PATH."
  (let* ((url (bibtex-print/remove-delimiters (cdr (assoc "url" entry))))
         (url-redirect (bibtex-fetch/url-redirect url))
         (key (cdr (assoc "=key=" entry)))
         (file-name (bibtex-fetch/document-file-name key document-path)))
    (bibtex-fetch/document-from-url url-redirect file-name)))

(defun bibtex-fetch/org-insert-entry (entry)
  (org-insert-heading-respect-content)
  (when (> 2 (org-current-level)) (org-demote))
  (let* ((title (bibtex-print/remove-delimiters (cdr (assoc "title" entry))))
         (key (cdr (assoc "=key=" entry)))
         (document-path "./doc")
         (file-name (bibtex-fetch/document-file-name key document-path)))
    (insert title)
    (newline-and-indent)
    (insert ":PROPERTIES:") (newline-and-indent)
    (insert ":CUSTOM_ID: " key) (newline-and-indent)
    (insert ":END":) (newline)
    (newline)
    (insert "#+BEGIN_SRC bibtex") (newline)
    (insert "#+END_SRC") (newline)
    (forward-line -1)
    (org-babel-do-in-edit-buffer
     (bibtex-print/insert-entry entry)
     (bibtex-fetch/document-from-entry entry document-path))
    (forward-line 2)
    (insert "[[file:" file-name "]]")
    (newline)))

(defun bibtex-fetch/org-insert-entry-from-clipboard ()
  (interactive)
  (let* ((url (url-unhex-string (gui-get-selection 'CLIPBOARD)))
         (entry (bibtex-fetch/from-url url)))
    (unless entry (error "Could not fetch BibTeX entry"))
    (bibtex-fetch/org-insert-entry entry)))

(provide 'bibtex-fetch)
;;; bibtex-fetch.el ends here
