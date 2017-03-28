;;; bibtex-fetch-ris.el --- Fetch bibliographic information from RIS files  -*- lexical-binding: t; -*-

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

(defun bibtex-fetch/ris-entry-1 (url)
  (with-current-buffer
      (url-retrieve-synchronously url)
    (shell-command-on-region (point-min) (point-max) "ris2xml | xml2bib" t)
    (goto-char (point-min))
    (narrow-to-region (1- (search-forward "@")) (re-search-forward "^}"))
    (bibtex-mode)
    (bibtex-set-dialect 'BibTeX)
    (goto-char (point-min))
    (bibtex-fetch/parse-entry)))

(defun bibtex-fetch/ris-entry (url)
  "Fetch an RIS entry and reformat as BibTeX."
  (let* ((entry (bibtex-fetch/ris-entry-1 url)))
    (unless entry (error "Unable to fetch entry"))
    (setcdr (assoc "=key=" entry) (bibtex-print/generate-key entry))
    entry))

(provide 'bibtex-fetch-ris)
;;; bibtex-fetch-ris.el ends here
