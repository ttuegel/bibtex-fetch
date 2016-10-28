;;; bibtex-fetch-doi.el --- Fetch bibliographic information and documents from a DOI URL  -*- lexical-binding: t; -*-

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

(require 'bibtex-fetch-parse)
(require 'url)

(defvar *bibtex-fetch/doi-waiting* t
  "Stores waiting state for url retrieval.")

(defvar *bibtex-fetch/doi-redirect* nil
  "Stores redirect url from a callback function.")

(defun bibtex-fetch/url-redirect-callback (status)
  (pcase (plist-get status :error)
    (`nil
     (pcase (plist-get status :redirect)
       (`nil nil)
       (url (setq *bibtex-fetch/doi-redirect* url))))
    (`(,err ,data) (signal err data)))
  (setq *bibtex-fetch/doi-waiting* nil))

(defun bibtex-fetch/url-redirect (url)
  "Determine where URL redirects to."
  (setq *bibtex-fetch/doi-waiting* t)
  (setq *bibtex-fetch/doi-redirect* url)
  (url-retrieve url #'bibtex-fetch/url-redirect-callback nil t)
  (while *bibtex-fetch/doi-waiting* (sleep-for 0.1))
  *bibtex-fetch/doi-redirect*)

(defun bibtex-fetch/retrieve-bibtex-1 (url)
  "Retrieve a BibTeX entry from URL."
  (with-current-buffer
      (url-retrieve-synchronously url t)
    (goto-char (point-min))
    (save-match-data (re-search-forward bibtex-entry-head))
    (bibtex-fetch/parse-entry)))

(defun bibtex-fetch/retrieve-bibtex (url)
  "Retrieve a BibTeX entry from URL."
  (let ((url-mime-accept-string "text/bibliography;style=bibtex, application/x-bibtex"))
    (bibtex-fetch/retrieve-bibtex-1 url)))

(defconst bibtex-fetch/doi-rx
  (rx string-start
      "http" (opt "s") "://" (opt "dx.") "doi.org/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))))
  "A regular expression to match the DOI from a URL.")

(defun bibtex-fetch/doi-query-url (doi)
  (s-concat "https://doi.org/" doi))

(defun bibtex-fetch/crossref-doi-query-url (doi)
  (format
   "http://crosscite.org/citeproc/format?doi=%s&style=bibtex&lang=en-US"
   doi))

(defun bibtex-fetch/doi-entry-1 (doi)
  "Fetch the BibTeX info from a DOI."
  (let* ((entry
          (or (bibtex-fetch/retrieve-bibtex (bibtex-fetch/doi-query-url doi))
              (bibtex-fetch/retrieve-bibtex
               (bibtex-fetch/crossref-doi-query-url doi))))
         (key-cell (assoc "=key=" entry))
         (new-key (bibtex-print/generate-key entry)))
    (setcdr key-cell new-key)
    entry))

(defun bibtex-fetch/doi-entry (url)
  "Fetch the BibTeX info from a DOI URL."
  (bibtex-fetch/doi-entry-1 (match-string 1 url)))

(defun bibtex-fetch/doi-document (url dest)
  "Fetch to DEST the document (PDF) corresponding to a DOI URL."
  (let ((provider-url (bibtex-fetch/url-redirect url)))
    (bibtex-fetch-document-1 provider-url dest)))

(provide 'bibtex-fetch-doi)
;;; bibtex-fetch-doi.el ends here
