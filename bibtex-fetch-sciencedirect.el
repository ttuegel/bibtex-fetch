;;; bibtex-fetch-sciencedirect.el --- Fetch bibliographic information and documents from ScienceDirect  -*- lexical-binding: t; -*-

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
(require 'url)

(require 'bibtex-fetch-doi)

(defconst bibtex-fetch/sciencedirect-rx
  (rx string-start "http" (opt "s") "://www.sciencedirect.com"
      "/science/article/pii/"
      (one-or-more (any "A-Z" "a-z" "0-9")))
  "A regular expression to match SciencDirect journal URLs.")

(defvar bibtex-fetch/sciencedirect-doi-rx
  (rx "SDM.doi = '" (submatch (one-or-more not-newline)) "';")
  "A regular expression to extract the DOI from a ScienceDirect landing page.")

(defun bibtex-fetch/sciencedirect-entry (url)
  "Fetch the BibTeX info from an ScienceDirect URL."
  (bibtex-fetch/doi-entry-1
   (with-current-buffer (url-retrieve-synchronously url t)
     (goto-char (point-min))
     (if (re-search-forward bibtex-fetch/sciencedirect-doi-rx (point-max) t)
         (match-string 1)
       (error "Could not find a DOI")))))

(provide 'bibtex-fetch-sciencedirect)
;;; bibtex-fetch-sciencedirect.el ends here
