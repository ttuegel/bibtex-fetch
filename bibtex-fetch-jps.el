;;; bibtex-fetch-jps.el --- Fetch bibliographic information and documents from JPS journals  -*- lexical-binding: t; -*-

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

(defconst bibtex-fetch/jps-rx
  (rx string-start "http" (opt "s") "://journals.jps.jp/doi/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))))
  "A regular expression to match JPS journal URLs.")

(defun bibtex-fetch/jps-entry (url)
  "Fetch the BibTeX info from an JPS URL."
  (bibtex-fetch/doi-entry-1 (match-string 1 url)))

(defun bibtex-fetch/jps-document-url (article-id)
  (s-concat "http://journals.jps.jp/doi/pdf/" article-id))

(defun bibtex-fetch/jps-document (url dest)
  "Fetch to DEST the document (PDF) corresponding to an APS journal URL."
  (let* ((doi (match-string 1 url))
         (document-url (bibtex-fetch/jps-document-url doi)))
    (url-copy-file document-url dest t)))

(provide 'bibtex-fetch-jps)
;;; bibtex-fetch-jps.el ends here
