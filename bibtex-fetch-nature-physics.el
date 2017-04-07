;;; bibtex-fetch-nature.el --- Fetch bibliographic information and documents from Nature journals  -*- lexical-binding: t; -*-

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
(require 'url)

(require 'bibtex-fetch-ris)

(defconst bibtex-fetch/nature-physics-rx
  (rx string-start "http" (opt "s") "://www.nature.com/nphys/journal/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))) ".html")
  "A regular expression to match Nature Physics URLs.")

(defun bibtex-fetch/nature-physics-entry-url (article-id)
  (s-concat "http://www.nature.com/nphys/journal/"
            (replace-regexp-in-string "/full/" "/ris/" article-id) ".ris"))

(defun bibtex-fetch/nature-physics-entry (url)
  "Fetch the BibTeX info from a Nature Physics URL."
  (let* ((article-id (match-string 1 url))
         (entry-url (bibtex-fetch/nature-physics-entry-url article-id)))
    (bibtex-fetch/ris-entry entry-url)))

(defun bibtex-fetch/nature-physics-document-url (article-id)
  (s-concat "http://www.nature.com/nphys/journal/"
            (replace-regexp-in-string "/full/" "/pdf/" article-id) ".pdf"))

(defun bibtex-fetch/nature-physics-document (url dest)
  "Fetch the document from a Nature Physics URL."
  (let* ((article-id (match-string 1 url))
         (document-url (bibtex-fetch/nature-physics-document-url article-id)))
    (url-copy-file document-url dest)))

(provide 'bibtex-fetch-nature-physics)
;;; bibtex-fetch-nature-physics.el ends here
