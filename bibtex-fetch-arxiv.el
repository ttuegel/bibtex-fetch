;;; bibtex-fetch-arxiv.el --- Fetch bibliographic entries and documents from arXiv  -*- lexical-binding: t; -*-

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

(require 'bibtex-print)
(require 'rx)
(require 's)
(require 'url)
(require 'xml)

(defconst bibtex-fetch/arxiv-rx
  (rx string-start
      "http" (opt "s") "://arxiv.org/abs/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))))
  "A regular expression to match the arXiv identifier from a URL.")

(defun bibtex-fetch/arxiv-query-url (arxiv-id)
  "The URL to GET to fetch bibliographic data for an ARXIV-ID."
  (s-concat "http://export.arxiv.org/api/query?id_list=" arxiv-id))

(defun bibtex-fetch/beginning-of-xml ()
  "The point of the beginning of the XML document in the current buffer."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (search-forward "<?xml")
      (match-beginning 0))))

(defun bibtex-fetch/xml-get-child (node name)
  "Get the first child of NODE with NAME."
  (car (xml-get-children node name)))

(defun bibtex-fetch/year-of-date (date-string)
  "Parse DATE-STRING and return the year."
  (elt (timezone-parse-date date-string) 0))

(defun bibtex-fetch/arxiv-entry-title (entry)
  "Get the title of a parsed (XML) arXiv ENTRY."
  (let ((title (caddr (bibtex-fetch/xml-get-child entry 'title))))
    (s-concat "{" title "}")))

(defun bibtex-fetch/arxiv-entry-year (entry)
  "Get the year of a parsed (XML) arXiv ENTRY."
  (let ((year (bibtex-fetch/year-of-date
               (caddr (bibtex-fetch/xml-get-child entry 'published)))))
    (s-concat "{" year "}")))

(defun bibtex-fetch/arxiv-author-name (author)
  (caddr (bibtex-fetch/xml-get-child author 'name)))

(defun bibtex-fetch/arxiv-entry-authors (entry)
  "Get the authors of a parsed (XML) arXiv ENTRY."
  (let ((authors (mapcar #'bibtex-fetch/arxiv-author-name
                         (xml-get-children entry 'author))))
    (s-concat "{" (s-join " and " authors) "}")))

(defun bibtex-fetch/arxiv-entry-doi (entry)
  "Get the DOI of a parsed (XML) arXiv ENTRY."
  (caddr (bibtex-fetch/xml-get-child entry 'arxiv:doi)))

(defun bibtex-fetch/arxiv-entry (url)
  "Fetch the BibTeX info from an arXiv URL."
  (let* ((arxiv-id (match-string 1 url))
         (arxiv-query-url (bibtex-fetch/arxiv-query-url arxiv-id)))
    (with-current-buffer
        (url-retrieve-synchronously arxiv-query-url t)
      (let* ((feed (car (xml-parse-region)))
             (entry (bibtex-fetch/xml-get-child feed 'entry))
             (doi (bibtex-fetch/arxiv-entry-doi entry))
             (title (bibtex-fetch/arxiv-entry-title entry))
             (year (bibtex-fetch/arxiv-entry-year entry))
             (authors (bibtex-fetch/arxiv-entry-authors entry))
             (bib (list (cons "=type=" "article")
                        (cons "author" authors)
                        (cons "title" title)
                        (cons "year" year)
                        (cons "archiveprefix" "{arXiv}")
                        (cons "eprint" (s-concat "{" arxiv-id "}"))
                        (cons "url" (s-concat "{" url "}")))))
        (add-to-list 'bib (cons "=key=" (bibtex-print/generate-key bib)))))))

(defun bibtex-fetch/arxiv-document-url (id)
  "The URL of the document associated with arXiv identifier ID."
  (s-concat "https://arxiv.org/pdf/" id))

(defun bibtex-fetch/arxiv-document (url dest)
  "Fetch to DEST the document (PDF) corresponding to an arXiv URL."
  (let* ((arxiv-id (match-string 1 url))
         (arxiv-pdf-url (bibtex-fetch/arxiv-document-url arxiv-id)))
    (url-copy-file arxiv-pdf-url dest t)))

(provide 'bibtex-fetch-arxiv)
;;; bibtex-fetch-arxiv.el ends here
