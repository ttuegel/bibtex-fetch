;;; bibtex-fetch-aip.el --- Fetch bibliographic information and documents for AIP journals  -*- lexical-binding: t; -*-

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

(require 'rx)
(require 's)

(require 'bibtex-fetch-doi)

(defconst bibtex-fetch/aip-rx
  (rx string-start "http" (opt "s") "://scitation.aip.org/content/aip/journal/"
      (submatch (one-or-more (any "A-Z" "a-z")) "/" ; journal abbrev
                (one-or-more (any "0-9")) "/" ; volume
                (one-or-more (any "0-9")) "/" ; issue
                )
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))) ; DOI
      )
  "A regular expression to match AIP journal URLs.")

(defun bibtex-fetch/aip-entry (url)
  (let* ((doi (match-string 2 url))
         (entry (bibtex-fetch/doi-entry-1 doi)))
    (unless entry (error "Unable to fetch entry"))
    (setcdr (assoc "=key=" entry) (bibtex-print/generate-key entry))
    entry))

(defun bibtex-fetch/aip-document-url (journal doi)
  (s-concat "https://scitation.aip.org/deliver/fulltext/aip/"
            journal "/" doi ".pdf"))

(defun bibtex-fetch/aip-document (url dest)
  (let* ((journal (match-string 1 url))
         (doi (match-string 2 url))
         (document-url (bibtex-fetch/aip-document-url journal doi)))
    (message document-url)
    (url-copy-file document-url dest t)))

(provide 'bibtex-fetch-aip)
;;; bibtex-fetch-aip.el ends here
