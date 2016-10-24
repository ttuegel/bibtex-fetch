;;; bibtex-print.el --- Pretty-printing BibTeX entries  -*- lexical-binding: t; -*-

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
(require 's)

(defun bibtex-print/texify-whitespace (string)
  "Replace extended whitespace as TeX would."
  (replace-regexp-in-string
   "^{[ \n\t]+" "{"
   (replace-regexp-in-string
    "[ \n\t]+}$" "}"
    (replace-regexp-in-string "[ \n\t]+" " " string))))

(defconst bibtex-print/header-fields-rx
  (rx "=" (or "type" "key") "="))

(defun bibtex-print/header-fields (name value)
  nil)

(defvar bibtex-print-field-handlers
  (list (cons bibtex-print/header-fields-rx #'bibtex-print/header-fields))
  "The list of handlers to use to print a BibTeX field.

Each handler is a pair of a regular expression and a function that will be
called when the field name matches that expression. The function takes two
arguments, the name and value of the field, and should insert the field at
point.")

(defun bibtex-print-default-field (field)
  (let ((name (car field))
        (value (cdr field)))
    (insert
     "  " (s-downcase name)
     " = " (s-trim (bibtex-print/texify-whitespace value)) ",\n")))

(defun bibtex-print/run-field-handler (field handler)
  (let* ((handler-rx (car handler))
         (handler-fun (cdr handler))
         (name (car field))
         (value (cdr field))
         (matched (string-match handler-rx name)))
    (when matched
      (funcall handler-fun name value)
      matched)))

(defun bibtex-print-field (field)
  (let* ((handlers bibtex-print-field-handlers) matched)
    (when (cdr field)
      (while (and (not matched) handlers)
        (setq matched (bibtex-print/run-field-handler field (pop handlers))))
      (unless matched (bibtex-print-default-field field)))))

(defun bibtex-print-entry (entry)
  "Pretty-print a parsed BibTeX entry."
  (let ((type (cdr (assoc "=type=" entry)))
        (key (cdr (assoc "=key=" entry))))
    (when (and type key)
      (insert "@" (s-downcase type) "{" key ",\n")
      (mapc #'bibtex-print-field entry)
      (insert "}\n")
      entry)))

(defun bibtex-print/remove-delimiters (s)
  "Remove the outer-most string delimiters around a BibTeX field."
  (s-chop-suffix "\""
  (s-chop-suffix "}"
  (s-chop-prefix "{"
  (s-chop-prefix "\"" s)))))

(defun bibtex-print/names (entry)
  "Get contents of the name field of ENTRY."
  (let ((names (bibtex-print/remove-delimiters
                (cdr (or (assoc "author" entry)
                         (assoc "editor" entry))))))
    ;; Some entries do not have a name field.
    (progn
      (dolist (pattern bibtex-autokey-name-change-strings)
        (setq names (replace-regexp-in-string (car pattern) (cdr pattern)
                                              names t)))
      (if (string= "" names)
          names
        (let* ((case-fold-search t)
               (name-list (mapcar 'bibtex-autokey-demangle-name
                                  (split-string names "[ \t\n]+and[ \t\n]+")))
               additional-names)
          (unless (or (not (numberp bibtex-autokey-names))
                      (<= (length name-list)
                          (+ bibtex-autokey-names
                             bibtex-autokey-names-stretch)))
            ;; Take `bibtex-autokey-names' elements from beginning of name-list
            (setq name-list (nreverse (nthcdr (- (length name-list)
                                                 bibtex-autokey-names)
                                              (nreverse name-list)))
                  additional-names bibtex-autokey-additional-names))
          (concat (mapconcat 'identity name-list
                             bibtex-autokey-name-separator)
                  additional-names))))))

(defun bibtex-print/year (entry)
  "Return year field of ENTRY."
  (let ((yearfield (bibtex-print/remove-delimiters
                    (cdr (assoc "year" entry))))
        (bibtex-autokey-year-length 2))
    (substring yearfield (max 0 (- (length yearfield)
                                   bibtex-autokey-year-length)))))

(defun bibtex-print/title (entry)
  "Get title of ENTRY up to a terminator."
  (let ((case-fold-search t)
        (titlestring (bibtex-print/remove-delimiters
                      (cdr (assoc "title" entry))))
        (bibtex-autokey-titleword-change-strings
         '(("\\\\aa" . "a")
           ("\\\\AA" . "A")
           ("\\\"a\\|\\\\\\\"a\\|\\\\ae" . "ae")
           ("\\\"A\\|\\\\\\\"A\\|\\\\AE" . "Ae")
           ("\\\\i" . "i")
           ("\\\\j" . "j")
           ("\\\\l" . "l")
           ("\\\\L" . "L")
           ("\\\"o\\|\\\\\\\"o\\|\\\\o\\|\\\\oe" . "oe")
           ("\\\"O\\|\\\\\\\"O\\|\\\\O\\|\\\\OE" . "Oe")
           ("\\\"s\\|\\\\\\\"s\\|\\\\3" . "ss")
           ("\\\"u\\|\\\\\\\"u" . "ue")
           ("\\\"U\\|\\\\\\\"U" . "Ue")
           ("\\\\`\\|\\\\'\\|\\\\\\^\\|\\\\~\\|\\\\=\\|\\\\\\.\\|\\\\u\\|\\\\v\\|\\\\H\\|\\\\t\\|\\\\c\\|\\\\d\\|\\\\b" . "")
           ("[`'\"{}#]" . "")
           ("\\\\-" . "")
           ("\\\\?[ 	\n]+\\|~" . " ")))
        (bibtex-autokey-title-terminators "[.!?:;]\\|--")
        (bibtex-autokey-titleword-ignore '("A" "An" "On" "The" "Eine?" "Der"
                                           "Die" "Das" "[^[:upper:]].*"
                                           ".*[^[:upper:][:lower:]0-9].*"))
        (bibtex-autokey-titlewords 5)
        (bibtex-autokey-titlewords-stretch 2)
        (bibtex-autokey-titleword-separator "_"))
    (progn
      (dolist (pattern bibtex-autokey-titleword-change-strings)
        (setq titlestring (replace-regexp-in-string (car pattern) (cdr pattern)
                                                    titlestring t)))
      ;; ignore everything past a terminator
      (if (string-match bibtex-autokey-title-terminators titlestring)
          (setq titlestring (substring titlestring 0 (match-beginning 0))))
      ;; gather words from titlestring into a list.  Ignore
      ;; specific words and use only a specific amount of words.
      (let ((counter 0)
            (ignore-re (concat "\\`\\(?:"
                               (mapconcat 'identity
                                          bibtex-autokey-titleword-ignore "\\|")
                               "\\)\\'"))
            titlewords titlewords-extra word)
        (while (and (or (not (numberp bibtex-autokey-titlewords))
                        (< counter (+ bibtex-autokey-titlewords
                                      bibtex-autokey-titlewords-stretch)))
                    (string-match "\\b\\w+" titlestring))
          (setq word (match-string 0 titlestring)
                titlestring (substring titlestring (match-end 0)))
          ;; Ignore words matched by one of the elements of
          ;; `bibtex-autokey-titleword-ignore'.  Case is significant.
          (unless (let (case-fold-search)
                    (string-match ignore-re word))
            (setq counter (1+ counter))
            (if (or (not (numberp bibtex-autokey-titlewords))
                    (<= counter bibtex-autokey-titlewords))
                (push word titlewords)
              (push word titlewords-extra))))
        ;; Obey `bibtex-autokey-titlewords-stretch':
        ;; If by now we have processed all words in titlestring, we include
        ;; titlewords-extra in titlewords.  Otherwise, we ignore titlewords-extra.
        (unless (string-match "\\b\\w+" titlestring)
          (setq titlewords (append titlewords-extra titlewords)))
        (mapconcat 'bibtex-autokey-demangle-title (nreverse titlewords)
                   bibtex-autokey-titleword-separator)))))

(defun bibtex-print/generate-key (entry)
  "Generate automatically a key for a BibTeX entry.
Use the author/editor, the year and the title field."
  (let* ((names (bibtex-print/names entry))
         (year (bibtex-print/year entry))
         (title (bibtex-print/title entry))
         (bibtex-autokey-prefix-string "")
         (bibtex-autokey-name-year-separator "")
         (bibtex-autokey-year-title-separator "_")
         (autokey (concat bibtex-autokey-prefix-string
                          names
                          (unless (or (equal names "")
                                      (equal year ""))
                            bibtex-autokey-name-year-separator)
                          year
                          (unless (or (and (equal names "")
                                           (equal year ""))
                                      (equal title ""))
                            bibtex-autokey-year-title-separator)
                          title)))
    (replace-regexp-in-string ":" "" autokey)))

(provide 'bibtex-print)
;;; bibtex-print.el ends here
