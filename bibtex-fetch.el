;;; bibtex-fetch.el --- Fetch BibTeX entries and documents from common sources  -*- lexical-binding: t; -*-

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
(require 'helm-utils)
(require 'rx)
(require 'select)
(require 'xml)

(defvar *bibtex-fetch/doi-waiting* t
  "Stores waiting state for url retrieval.")

(defvar *bibtex-fetch/doi-redirect* nil
  "Stores redirect url from a callback function.")

(defun bibtex-fetch/url-redirect-callback (status)
  (pcase (plist-get status :error)
    (`nil
     (pcase (plist-get status :redirect)
       (`nil nil)
       (url (setq *bibtex-fetch/doi-redirect* url))))
    (`(,err ,data) (signal err data)))
  (setq *bibtex-fetch/doi-waiting* nil))

(defun bibtex-fetch/url-redirect (url)
  "Determine where URL redirects to."
  (setq *bibtex-fetch/doi-waiting* t)
  (setq *bibtex-fetch/doi-redirect* url)
  (url-retrieve url #'bibtex-fetch/url-redirect-callback nil t)
  (while *bibtex-fetch/doi-waiting* (sleep-for 0.1))
  *bibtex-fetch/doi-redirect*)

(defconst bibtex-fetch/arxiv-rx
  (rx string-start
      "http" (opt "s") "://arxiv.org/abs/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))))
  "A regular expression to match the arXiv identifier from a URL.")

(defun bibtex-fetch/arxiv-query-url (arxiv-id)
  "The URL to GET to fetch bibliographic data for an ARXIV-ID."
  (s-concat "http://export.arxiv.org/api/query?id_list=" arxiv-id))

(defun bibtex-fetch/beginning-of-xml ()
  "The point of the beginning of the XML document in the current buffer."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (search-forward "<?xml")
      (match-beginning 0))))

(defun bibtex-fetch/xml-get-child (node name)
  (car (xml-get-children node name)))

(defun bibtex-fetch/year-of-date (date-string)
  "Parse DATE-STRING and return the year."
  (elt (timezone-parse-date date-string) 0))

(defun bibtex-fetch/arxiv-entry-title (entry)
  "Get the title of a parsed (XML) arXiv ENTRY."
  (let ((title (caddr (bibtex-fetch/xml-get-child entry 'title))))
    (s-concat "{" title "}")))

(defun bibtex-fetch/arxiv-entry-year (entry)
  "Get the year of a parsed (XML) arXiv ENTRY."
  (let ((year (bibtex-fetch/year-of-date
               (caddr (bibtex-fetch/xml-get-child entry 'published)))))
    (s-concat "{" year "}")))

(defun bibtex-fetch/arxiv-author-name (author)
  (caddr (bibtex-fetch/xml-get-child author 'name)))

(defun bibtex-fetch/arxiv-entry-authors (entry)
  "Get the authors of a parsed (XML) arXiv ENTRY."
  (let ((authors (mapcar #'bibtex-fetch/arxiv-author-name
                         (xml-get-children entry 'author))))
    (s-concat "{" (s-join " and " authors) "}")))

(defun bibtex-fetch/arxiv-entry-doi (entry)
  "Get the DOI of a parsed (XML) arXiv ENTRY."
  (caddr (bibtex-fetch/xml-get-child entry 'arxiv:doi)))

(defun bibtex-fetch/arxiv-entry (url)
  "Fetch the BibTeX info from an arXiv URL."
  (let* ((arxiv-id (match-string 1 url))
         (arxiv-query-url (bibtex-fetch/arxiv-query-url arxiv-id)))
    (with-current-buffer
        (url-retrieve-synchronously arxiv-query-url t)
      (let* ((feed (car (xml-parse-region)))
             (entry (bibtex-fetch/xml-get-child feed 'entry))
             (doi (bibtex-fetch/arxiv-entry-doi entry))
             (title (bibtex-fetch/arxiv-entry-title entry))
             (year (bibtex-fetch/arxiv-entry-year entry))
             (authors (bibtex-fetch/arxiv-entry-authors entry))
             (bib (list (cons "=type=" "article")
                        (cons "author" authors)
                        (cons "title" title)
                        (cons "year" year)
                        (cons "archiveprefix" "{arXiv}")
                        (cons "eprint" (s-concat "{" arxiv-id "}"))
                        (cons "url" (s-concat "{" url "}")))))
        (add-to-list 'bib (cons "=key=" (bibtex-print/generate-key bib)))))))

(defun bibtex-fetch/retrieve-bibtex (url)
  "Retrieve a BibTeX entry from URL."
  (let ((url-mime-accept-string "text/bibliography;style=bibtex, application/x-bibtex"))
    (with-current-buffer
        (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (save-match-data (re-search-forward bibtex-entry-head))
      (bibtex-fetch/parse-entry))))

(defconst bibtex-fetch/doi-rx
  (rx string-start
      "http" (opt "s") "://" (opt "dx.") "doi.org/"
      (submatch (one-or-more (any "A-Z" "a-z" "0-9" "./"))))
  "A regular expression to match the DOI from a URL.")

(defun bibtex-fetch/doi-query-url (doi)
  (s-concat "https://doi.org/" doi))

(defun bibtex-fetch/crossref-doi-query-url (doi)
  (format
   "http://crosscite.org/citeproc/format?doi=%s&style=bibtex&lang=en-US"
   doi))

(defun bibtex-fetch/doi-entry (url)
  "Fetch the BibTeX info from an DOI URL."
  (let* ((doi (match-string 1 url))
         (entry
          (or (bibtex-fetch/retrieve-bibtex (bibtex-fetch/doi-query-url doi))
              (bibtex-fetch/retrieve-bibtex
               (bibtex-fetch/crossref-doi-query-url doi))))
         (key-cell (assoc "=key=" entry))
         (new-key (bibtex-print/generate-key entry)))
    (setcdr key-cell new-key)
    entry))

(defun bibtex-fetch/url-entry (url)
  "Fetch the BibTeX info from a URL."
  (let* ((entry (bibtex-fetch/retrieve-bibtex url))
         (key-cell (assoc "=key=" entry))
         (new-key (bibtex-print/generate-key entry)))
    (setcdr key-cell new-key)
    entry))

(defvar bibtex-fetch-entry-handlers
  (list (cons bibtex-fetch/arxiv-rx #'bibtex-fetch/arxiv-entry)
        (cons bibtex-fetch/doi-rx #'bibtex-fetch/doi-entry)
        (cons bibtex-fetch/aps-rx #'bibtex-fetch/url-entry))
  "The list of handlers to use to fetch a BibTeX entry from a URL.

Each handler is a pair of a regular expression and a function that will be
called when the URL matches the regular expression. The function takes no
arguments, but it may assume that `match-data' is set.")

(defun bibtex-fetch/run-entry-handler (url handler)
  (let* ((handler-rx (car handler))
         (handler-fun (cdr handler)))
    (when (string-match handler-rx url)
      (funcall handler-fun url))))

(defun bibtex-fetch-entry-from-url (url)
  "Fetch the BibTeX entry for the document at URL."
  (interactive "MURL: ")
  (let* ((handlers bibtex-fetch-entry-handlers) handler entry)
    (while (and (not entry) (setq handler (pop handlers)))
      (setq entry (bibtex-fetch/run-entry-handler url handler)))
    (bibtex-print-entry entry)))

(defun bibtex-fetch-entry ()
  "Fetch the BibTeX entry for the URL on the system clipboard."
  (interactive)
  (bibtex-fetch-entry-from-url (gui-get-selection 'CLIPBOARD)))

(defun bibtex-fetch/arxiv-document-url (id)
  "The URL of the document associated with arXiv identifier ID."
  (s-concat "https://arxiv.org/pdf/" id))

(defun bibtex-fetch/arxiv-document-callback (dest)
  (write-file dest)
  (kill-buffer))

(defun bibtex-fetch/arxiv-document (url dest)
  "Fetch to DEST the document (PDF) corresponding to an arXiv URL."
  (let* ((arxiv-id (match-string 1 url))
         (arxiv-pdf-url (bibtex-fetch/arxiv-document-url arxiv-id)))
    (url-copy-file arxiv-pdf-url dest t)))

(defun bibtex-fetch/doi-document (url dest)
  "Fetch to DEST the document (PDF) corresponding to a DOI URL."
  (let ((provider-url (bibtex-fetch/url-redirect url)))
    (bibtex-fetch-document-1 provider-url dest)))

(defconst bibtex-fetch/aps-rx
  (rx string-start "http" (opt "s") "://journals.aps.org")
  "A regular expression to match APS journal URLs.")

(defun bibtex-fetch/aps-document (url dest)
  "Fetch to DEST the document (PDF) corresponding to an APS journal URL."
  (let ((document-url (replace-regexp-in-string "/abstract/" "/pdf/" url)))
    (gui-set-selection 'CLIPBOARD dest)
    (browse-url document-url)))

(defvar bibtex-fetch-document-handlers
  (list (cons bibtex-fetch/arxiv-rx #'bibtex-fetch/arxiv-document)
        (cons bibtex-fetch/doi-rx #'bibtex-fetch/doi-document)
        (cons bibtex-fetch/aps-rx #'bibtex-fetch/aps-document))
  "The handlers used to fetch a document from a URL stored in a BibTeX entry.

Each handler is a pair of a regular expression and a function that will be
called when the URL matches the regular expression. The function takes two
arguments, the URL and the destination for the file.")

(defun bibtex-fetch/run-document-handler (url dest handler)
  (let* ((handler-rx (car handler))
         (handler-fun (cdr handler))
         (final-dest (expand-file-name dest)))
    (when (string-match handler-rx url)
      (make-directory (file-name-directory final-dest) t)
      (funcall handler-fun url final-dest)
      t)))

(defun bibtex-fetch/parse-entry ()
  "Parse the BibTeX entry at point.

If point is inside or at the beginning of an entry, parse and return that entry.
Restore point when finished."
  (save-excursion
    (bibtex-beginning-of-entry)
    (bibtex-parse-entry)))

(defun bibtex-fetch-document-1 (url dest)
  "Fetch the document corresponding to the BibTeX entry at point."
  (let* ((handlers bibtex-fetch-document-handlers) matched)
    (while (and (not matched) handlers)
      (setq matched
            (bibtex-fetch/run-document-handler url dest (pop handlers))))))

(defun bibtex-fetch-document ()
  "Fetch the document corresponding to the BibTeX entry at point."
  (interactive)
  (let* ((entry (bibtex-fetch/parse-entry))
         (url (bibtex-print/remove-delimiters
               (cdr (assoc "url" entry))))
         (key (cdr (assoc "=key=" entry)))
         (dest (s-concat "doc/" key ".pdf")))
    (bibtex-fetch-document-1 url dest)))

(defun bibtex-capture ()
  (interactive)
  (progn
    (when (bibtex-fetch-entry)
      (bibtex-fetch-document))))

(defun bibtex-open-document ()
  "Open the document associated with the BibTeX entry at point."
  (interactive)
  (let* ((entry (bibtex-fetch/parse-entry))
         (key (cdr (assoc "=key=" entry)))
         (document (expand-file-name (s-concat "doc/" key ".pdf"))))
    (if (file-readable-p document)
        (helm-open-file-with-default-tool document)
      (message "Could not open %s" document))))

(defun bibtex-open-url ()
  "Open the URL associated with the BibTeX entry at point."
  (interactive
   (let* ((entry (bibtex-fetch/parse-entry))
          (url (bibtex-print/remove-delimiters
                (cdr (assoc "url" entry)))))
     (if url
         (browse-url url)
       (message "No URL for this entry.")))))

(bind-key "C-c o" #'bibtex-open-document bibtex-mode-map)
(bind-key "C-c M-o" #'bibtex-open-url bibtex-mode-map)
(bind-key "C-c C-c" #'bibtex-capture bibtex-mode-map)

(provide 'bibtex-fetch)
;;; bibtex-fetch.el ends here
