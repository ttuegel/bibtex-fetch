;;; bibtex-fetch-springer.el --- Fetch bibliographic information and documents from Springer journals  -*- lexical-binding: t; -*-

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

(require 'browse-url)
(require 'rx)
(require 'select)

(require 'bibtex-fetch-doi)

(defconst bibtex-fetch/springer-rx
  (rx string-start "http" (opt "s") "://link.springer.com/article/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" ".:/"))))
  "A regular expression to match Springer journal URLs.")

(defun bibtex-fetch/springer-entry (url)
  "Fetch the BibTeX info from a Springer URL."
  (bibtex-fetch/doi-entry-1 (match-string 1 url)))

(defun bibtex-fetch/springer-document (url dest)
  "Fetch to DEST the document (PDF) corresponding to Springer journal URL."
  (let ((document-url
         (s-concat
          (replace-regexp-in-string "/article/" "/content/pdf/" url)
          ".pdf")))
    (url-copy-file document-url dest)))


(provide 'bibtex-fetch-springer)
;;; bibtex-fetch-springer.el ends here
