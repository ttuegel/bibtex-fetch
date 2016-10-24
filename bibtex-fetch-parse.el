;;; bibtex-fetch-parse.el --- Parse the BibTeX entry around point  -*- lexical-binding: t; -*-

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

(defun bibtex-fetch/parse-entry ()
  "Parse the BibTeX entry at point.

If point is inside or at the beginning of an entry, parse and return that entry.
Restore point when finished."
  (save-excursion
    (bibtex-beginning-of-entry)
    (bibtex-parse-entry)))


(provide 'bibtex-fetch-parse)
;;; bibtex-fetch-parse.el ends here
