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
(require 'org)
(require 'rx)
(require 'select)
(require 'url-util)
(require 'xml)

(require 'bibtex-fetch-acs)
(require 'bibtex-fetch-aip)
(require 'bibtex-fetch-aps)
(require 'bibtex-fetch-arxiv)
(require 'bibtex-fetch-doi)
(require 'bibtex-fetch-iop)
(require 'bibtex-fetch-jps)
(require 'bibtex-fetch-nature)
(require 'bibtex-fetch-nature-physics)
(require 'bibtex-fetch-sciencedirect)
(require 'bibtex-fetch-springer)

(defvar-local bibtex-fetch-document-path
  nil
  "The directory containing the documents associated with the bibliography.

Each bibliography entry may have an associated document. The filename of the
document is the same as the entry key, plus the appropriate extension.")

(defvar bibtex-fetch-entry-handlers
  (list (cons bibtex-fetch/arxiv-rx #'bibtex-fetch/arxiv-entry)
        (cons bibtex-fetch/doi-rx #'bibtex-fetch/doi-entry)
        (cons bibtex-fetch/aps-rx #'bibtex-fetch/aps-entry)
        (cons bibtex-fetch/acs-rx #'bibtex-fetch/acs-entry)
        (cons bibtex-fetch/iop-rx #'bibtex-fetch/iop-entry)
        (cons bibtex-fetch/sciencedirect-rx #'bibtex-fetch/sciencedirect-entry)
        (cons bibtex-fetch/springer-rx #'bibtex-fetch/springer-entry)
        (cons bibtex-fetch/jps-rx #'bibtex-fetch/jps-entry)
        (cons bibtex-fetch/nature-rx #'bibtex-fetch/nature-entry)
        (cons bibtex-fetch/nature-physics-rx #'bibtex-fetch/nature-physics-entry)
        (cons bibtex-fetch/aip-rx #'bibtex-fetch/aip-entry))
  "The list of handlers to use to fetch a BibTeX entry from a URL.

Each handler is a pair of a regular expression and a function that will be
called when the URL matches the regular expression. The function takes no
arguments, but it may assume that `match-data' is set.")

(defvar bibtex-fetch-document-handlers
  (list (cons bibtex-fetch/arxiv-rx #'bibtex-fetch/arxiv-document)
        (cons bibtex-fetch/doi-rx #'bibtex-fetch/doi-document)
        (cons bibtex-fetch/aps-rx #'bibtex-fetch/aps-document)
        (cons bibtex-fetch/acs-rx #'bibtex-fetch/acs-document)
        (cons bibtex-fetch/iop-document-rx #'bibtex-fetch/iop-document)
        (cons bibtex-fetch/springer-rx #'bibtex-fetch/springer-document)
        (cons bibtex-fetch/jps-rx #'bibtex-fetch/jps-document)
        (cons bibtex-fetch/nature-rx #'bibtex-fetch/nature-document)
        (cons bibtex-fetch/nature-physics-rx #'bibtex-fetch/nature-physics-document)
        (cons bibtex-fetch/aip-rx #'bibtex-fetch/aip-document))
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
  (let* ((handlers bibtex-fetch-entry-handlers) entry)
    (while (and (not entry) handlers)
      (setq entry (bibtex-fetch/run-entry-handler url (pop handlers))))
    (if (not entry)
        (error "No handler found to fetch entry")
      (bibtex-print-entry entry))))

(defun bibtex-fetch-entry ()
  "Fetch the BibTeX entry for the URL on the system clipboard."
  (interactive)
  (bibtex-fetch-entry-from-url (url-unhex-string (gui-get-selection 'CLIPBOARD))))

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
            (bibtex-fetch/run-document-handler url dest (pop handlers))))
    (when (not matched)
      (error "No handler found to fetch document"))))

(defun bibtex-fetch/document-file-name (key)
  "Determine the full path and file name of the document associated with KEY."
  (unless bibtex-fetch-document-path
    (error "Please set `bibtex-fetch-document-path'"))
  (expand-file-name
   (s-concat bibtex-fetch-document-path "/" key ".pdf")))

(defun bibtex-fetch-document ()
  "Fetch the document corresponding to the BibTeX entry at point."
  (interactive)
  (let* ((entry (bibtex-fetch/parse-entry))
         (url (bibtex-fetch/url-redirect
               (bibtex-print/remove-delimiters
                (cdr (assoc "url" entry)))))
         (key (cdr (assoc "=key=" entry)))
         (dest (bibtex-fetch/document-file-name key)))
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
         (document (bibtex-fetch/document-file-name key)))
    (if (file-readable-p document)
        (helm-open-file-with-default-tool document)
      (error "Could not open %s" document))))

(defun bibtex-open-url ()
  "Open the URL associated with the BibTeX entry at point."
  (interactive
   (let* ((entry (bibtex-fetch/parse-entry))
          (url (bibtex-print/remove-delimiters
                (cdr (assoc "url" entry)))))
     (if url
         (browse-url url)
       (error "No URL for this entry")))))

(defun bibtex-associate ()
  "Copy the associated document path to the clipboard."
  (interactive)
  (let* ((entry (bibtex-fetch/parse-entry))
         (key (cdr (assoc "=key=" entry)))
         (document (bibtex-fetch/document-file-name key)))
    (gui-set-selection 'CLIPBOARD document)))

(defun org-bibtex-open-document ()
  "Run `bibtex-open-document' if point is in a BibTeX source block."
  (interactive)
  (pcase (org-babel-get-src-block-info)
    (`("bibtex" . ,_) (bibtex-open-document))
    (_ (error "No BibTeX block at point"))))

(defun org-bibtex-capture ()
  "Run `bibtex-capture' if point is in a BibTeX source block."
  (interactive)
  (pcase (org-babel-get-src-block-info)
    (`("bibtex" . ,_) (bibtex-capture))
    (_ (error "No BibTeX block at point"))))

(bind-key "C-c o" #'org-bibtex-open-document org-mode-map)
(bind-key "C-c c" #'org-bibtex-capture org-mode-map)

(define-minor-mode bibtex-fetch-mode
  "A minor mode for managing BibTeX citations and documents."
  :keymap (make-sparse-keymap))

(bind-key "C-c o" #'bibtex-open-document bibtex-fetch-mode-map)
(bind-key "C-c M-o" #'bibtex-open-url bibtex-fetch-mode-map)
(bind-key "C-c C-c" #'bibtex-capture bibtex-fetch-mode-map)
(bind-key "C-c a" #'bibtex-associate bibtex-fetch-mode-map)

(defun turn-on-bibtex-fetch-mode ()
  "Turn on `bibtex-fetch-mode'."
  (interactive)
  (bibtex-fetch-mode 1))

(defun turn-off-bibtex-fetch-mode ()
  "Turn off `bibtex-fetch-mode'."
  (interactive)
  (bibtex-fetch-mode -1))

(define-minor-mode org-bibtex-fetch-mode
  "A minor mode for managing BibTeX citations and documents in Org."
  :keymap (make-sparse-keymap))

(define-prefix-command 'org-bibtex-fetch-prefix-map)
(bind-key "C-c C-b" #'org-bibtex-fetch-prefix-map org-bibtex-fetch-mode-map)

(bind-key "o" #'bibtex-open-document org-bibtex-fetch-prefix-map)
(bind-key "M-o" #'bibtex-open-url org-bibtex-fetch-prefix-map)
(bind-key "C-c" #'bibtex-capture org-bibtex-fetch-prefix-map)
(bind-key "a" #'bibtex-associate org-bibtex-fetch-prefix-map)

(defun turn-on-org-bibtex-fetch-mode ()
  "Turn on `org-bibtex-fetch-mode'."
  (interactive)
  (org-bibtex-fetch-mode 1))

(defun turn-off-org-bibtex-fetch-mode ()
  "Turn off `org-bibtex-fetch-mode'."
  (interactive)
  (org-bibtex-fetch-mode -1))

(provide 'bibtex-fetch)
;;; bibtex-fetch.el ends here
