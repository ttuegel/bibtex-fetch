;;; bibtex-fetch-aps.el --- Fetch bibliographic information and documents from APS journals  -*- lexical-binding: t; -*-

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

(defconst bibtex-fetch/aps-rx
  (rx string-start "http" (opt "s") "://journals.aps.org/"
      (one-or-more (any "A-Z" "a-z" "0-9")) "/abstract/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))))
  "A regular expression to match APS journal URLs.")

(defun bibtex-fetch/aps-entry (url)
  "Fetch the BibTeX info from an APS URL."
  (bibtex-fetch/doi-entry-1 (match-string 1 url)))

(defun bibtex-fetch/aps-document (url dest)
  "Fetch to DEST the document (PDF) corresponding to an APS journal URL."
  (let ((document-url (replace-regexp-in-string "/abstract/" "/pdf/" url)))
    (gui-set-selection 'CLIPBOARD dest)
    (browse-url document-url)))

(provide 'bibtex-fetch-aps)
;;; bibtex-fetch-aps.el ends here
