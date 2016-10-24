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
(require 'bibtex-print)
(require 'helm-utils)
(require 'rx)
(require 'select)
(require 'xml)

(require 'bibtex-fetch-acs)
(require 'bibtex-fetch-aps)
(require 'bibtex-fetch-arxiv)
(require 'bibtex-fetch-doi)
(require 'bibtex-fetch-springer)

(defvar bibtex-fetch-entry-handlers
  (list (cons bibtex-fetch/arxiv-rx #'bibtex-fetch/arxiv-entry)
        (cons bibtex-fetch/doi-rx #'bibtex-fetch/doi-entry)
        (cons bibtex-fetch/aps-rx #'bibtex-fetch/aps-entry)
        (cons bibtex-fetch/acs-rx #'bibtex-fetch/acs-entry)
        (cons bibtex-fetch/springer-rx #'bibtex-fetch/springer-entry))
  "The list of handlers to use to fetch a BibTeX entry from a URL.

Each handler is a pair of a regular expression and a function that will be
called when the URL matches the regular expression. The function takes no
arguments, but it may assume that `match-data' is set.")

(defvar bibtex-fetch-document-handlers
  (list (cons bibtex-fetch/arxiv-rx #'bibtex-fetch/arxiv-document)
        (cons bibtex-fetch/doi-rx #'bibtex-fetch/doi-document)
        (cons bibtex-fetch/aps-rx #'bibtex-fetch/aps-document)
        (cons bibtex-fetch/acs-rx #'bibtex-fetch/acs-document)
        (cons bibtex-fetch/springer-rx #'bibtex-fetch/springer-document))
  "The handlers used to fetch a document from a URL stored in a BibTeX entry.

Each handler is a pair of a regular expression and a function that will be
called when the URL matches the regular expression. The function takes two
arguments, the URL and the destination for the file.")

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

(defun bibtex-fetch/run-document-handler (url dest handler)
  (let* ((handler-rx (car handler))
         (handler-fun (cdr handler))
         (final-dest (expand-file-name dest)))
    (when (string-match handler-rx url)
      (make-directory (file-name-directory final-dest) t)
      (funcall handler-fun url final-dest)
      t)))

(defun bibtex-fetch-document-1 (url dest)
  "Fetch the document corresponding to URL into DEST."
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
