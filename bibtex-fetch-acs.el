;;; bibtex-fetch-acs.el --- Fetch bibliographic information and documents from ACS journals  -*- lexical-binding: t; -*-

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

(require 'bibtex-fetch-doi)

(defconst bibtex-fetch/acs-rx
  (rx string-start "http" (opt "s") "://pubs.acs.org/doi/abs/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))))
  "A regular expression to match ACS journal URLs.")

(defun bibtex-fetch/acs-entry (url)
  (let* ((doi (match-string 1 url))
         (entry (bibtex-fetch/retrieve-bibtex
                 (s-concat
                  "https://pubs.acs.org/action/downloadCitation"
                  "?direct=true"
                  "&doi=" (url-encode-url doi)
                  "&format=bibtex"
                  "&include=abs"
                  "&submit=Download+Citation"))))
    (setcdr (assoc "=key=" entry) (bibtex-print/generate-key entry))
    (setcdr (assoc "abstract" entry) nil)
    entry))

(defun bibtex-fetch/acs-document (url dest)
  "Fetch to DEST the document (PDF) corresponding to an APS journal URL."
  (let ((document-url (replace-regexp-in-string "/abs/" "/pdf/" url)))
    (url-copy-file document-url dest t)))

(provide 'bibtex-fetch-acs)
;;; bibtex-fetch-acs.el ends here
