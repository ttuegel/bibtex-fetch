;;; bibtex-fetch-iop.el --- Fetch bibliographic information and documents from IOP journals  -*- lexical-binding: t; -*-

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

(require 'bibtex-fetch-doi)

(defconst bibtex-fetch/iop-rx
  (rx string-start
      "http" (opt "s") "://iopscience.iop.org/article/"
      (one-or-more (not (any ?/))) ;; journal id
      "/"
      (submatch (one-or-more not-newline)))
  "A regular expression to match IOP URLs.")

(defun bibtex-fetch/iop-entry-url (article-id)
  (s-concat "http://iopscience.iop.org/export"
            "?articleId=" article-id
            "&exportFormat=iopexport_bib"
            "&exportType=abs"
            "&navsubmit=Export%2Babstract"))

(defconst bibtex-fetch/iop-entry-url-rx
  (rx "href=\"" (submatch (one-or-more (not (any ?\")))) ?\"
      " class=\"btn btn-primary wd-btn-cit-abs-bib\""))

(defun bibtex-fetch/iop-entry-url (url)
  (with-current-buffer (url-retrieve-synchronously url t)
    (goto-char (point-min))
    (re-search-forward bibtex-fetch/iop-entry-url-rx (point-max) t)
    (if (= (point-min) (point))
        (error "Could not extract BibTeX entry URL from page")
      (s-concat "http://iopscience.iop.org" (match-string 1)))))

(defun bibtex-fetch/iop-entry (url)
  "Fetch the BibTeX entry from an IOP URL."
  (let* ((entry-url (bibtex-fetch/iop-entry-url url))
         (entry (bibtex-fetch/retrieve-bibtex-1 entry-url)))
    (unless entry (error "Unable to fetch entry"))
    (setcdr (assoc "=key=" entry) (bibtex-print/generate-key entry))
    (setcdr (assoc "abstract" entry) nil)
    entry))

(defun bibtex-fetch/iop-document-url (article-id)
  (s-concat "http://iopscience.iop.org/article/" article-id "/pdf"))

(defconst bibtex-fetch/iop-document-rx
  (rx string-start "http" (opt "s") "://stacks.iop.org")
  "A regular expression to match IOP URLs.")

(defun bibtex-fetch/iop-document (url dest)
  "Fetch the document associated with an IOP URL."
  (let* ((article-url (bibtex-fetch/url-redirect url))
         (article-id (progn
                       (string-match bibtex-fetch/iop-rx article-url)
                       (match-string 1 article-url)))
         (document-url (bibtex-fetch/iop-document-url article-id)))
    (url-copy-file document-url dest t)))

(provide 'bibtex-fetch-iop)
;;; bibtex-fetch-iop.el ends here
