;;; fpa.el --- Open italian fattura PA format  -*- lexical-binding: t; -*-

;; Copyright 2024 Stefano Merlo

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;; Author: Stefano Merlo <trepreciso@gmail.com>
;; Created: 2024-02-13

;;; Commentary:

;; Convert the XML `fattura PA' (Fatturazione elettronica verso la
;; Pubblica amministrazione - Sistema di Interscambio) format to
;; readable format.
;;
;; Mapping is parsed from the spreadsheet-style map published on
;; https://www.fatturapa.gov.it/it/norme-e-regole/
;; documentazione-fattura-elettronica/formato-fatturapa/

;;; Code:

(require 'xml)

(defvar fpa-schema-file "~/org/projects/fpa-reader/fpa-schema.el"
  "File containing the schema copy-pasted from
`fatturapa.gov.it'. Schema file is a elisp-formatted list, where
nested levels are represented by nested lists. Automatic indent
helps with skimming the structure. Structure: (<identifier,
1.2.3> <label> <children>)")

(defvar fpa--output-file "~/org/projects/fpa-reader/out/out.csv")

(defun fpa--schema-from-file ()
  "Return schema from file `fpa-schema-file' as Lisp object."
  (with-temp-buffer
    (insert-file-contents fpa-schema-file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun fpa--get-level (id)
  "Return level, given ID format as `1.2.3'."
  (pcase id
    ("root" 0)
    ((rx string-start num (= 0 (group "." (one-or-more num))) string-end) 1)
    ((rx string-start num (= 1 (group "." (one-or-more num))) string-end) 2)
    ((rx string-start num (= 2 (group "." (one-or-more num))) string-end) 3)
    ((rx string-start num (= 3 (group "." (one-or-more num))) string-end) 4)
    ((rx string-start num (= 4 (group "." (one-or-more num))) string-end) 5)
    ((rx string-start num (= 5 (group "." (one-or-more num))) string-end) 6)))

(defvar fpa--root-header-prefixes '(ns0 n0 ns1 ns2 ns3 b p nil)
  "List of possible prefix headers for the top level.")

(defun fpa--root-keys ()
  "Return all possible keys for the root header, combining the root
from the schema file and the `fpa--root-header-prefixes'."
  (seq-map (lambda (p)
             (let ((rt "FatturaElettronica"))
               (if p (intern (concat (symbol-name p) ":" rt))
                 (intern rt)))) fpa--root-header-prefixes))

(defun fpa--xml-to-tree (file-name)
  "XML-parse FILE-NAME and return top node tree as xml tree."
  (let ((parsed-xml-region (xml-parse-file file-name))
        (tree))
    (cl-loop for key in (fpa--root-keys)
             for tree = (assq key parsed-xml-region)
             if tree return tree)))

;; TODO convert schema-keys to struct using cl-struct

(defun fpa--schema-key-get (what key)
  "Return WHAT from the KEY schema key."
  (pcase what
    ('import-flag (nth 1 key))
    ('id (nth 0 key))
    ('path (nth 2 key))
    ;; label: either is present, or last value of path
    ('label (or (nth 3 key)
                (symbol-name
                 (seq-first (reverse (fpa--schema-key-get 'path key))))))))

(defun fpa--tree-get-value-from-path (tree path)
  "Return raw xml value from TREE, parsing PATH using
`xml-get-children'. Path: (root child1 child2 leaf)"
  (cond ((not path) tree)
        (t (let ((children (xml-get-children tree (pop path))))
             (if (= 1 (length children))
                 (fpa--tree-get-value-from-path (car children) path)
               (cl-loop for child in children
                        collect
                        (fpa--tree-get-value-from-path child path)))))))

(defun fpa--tree-get-value-from-key (tree key)
  "Search KEY in TREE and return enriched value. Only if flagged
with non-nil `import' (see schema file specs). Return
list (label id (value-or-values))."
  (when (fpa--schema-key-get 'import-flag key)
    (let* ((path (fpa--schema-key-get 'path key))
           (value (fpa--tree-get-value-from-path tree path))
           (label (fpa--schema-key-get 'label key))
           (id (fpa--schema-key-get 'id key)))
      (list label id value))))

(defun fpa--tree-get-all-values (tree)
  "Return all values from TREE using `fpa-schema-file'. Result
 is an assoc list (`fpa-list') using `label' (see schema specs) as key."
  (let* ((keys (fpa--schema-from-file))
         (f (lambda (k) (fpa--tree-get-value-from-key tree k)))
         (raw-values (seq-map f keys))
         ;; remove all nils
         (cleaned-values (seq-filter #'identity raw-values)))
    cleaned-values))

(defconst fpa--invoice-id-identifier '2-1-1-4
  "Immutable identified for the invoice id, used to separate
 different invoices in the same file.")

(defconst fpa--lines-id-identifier '2-2-1-1
  "Immutable identified for the line id, used to separate
 different lines in the same file.")

(defun fpa--count-multi (what fpa-list)
  "Return number of WHAT in FPA-LIST."
  (let ((id (pcase what
              ('invoices fpa--invoice-id-identifier)
              ('lines fpa--lines-id-identifier))))
    (let* ((id--schema-key (assoc id (fpa--get-schema)))
           (id--schema-label (fpa--schema-key-get 'label id--schema-key))
           (fpa-ids-el (assoc id--schema-label fpa-list))
           (fpa-ids (caddr fpa-ids-el)))
      (if (and (symbolp (car   fpa-ids))
               (not     (cadr  fpa-ids))
               (stringp (caddr fpa-ids))) 1 ; element is not a list
        (length fpa-ids)))))

(defun fpa--invoices (fpa-list)
  "Return list of invoices (formatted as fpa-lists) from FPA-LIST."
  (let ((n (fpa--count-multi 'invoices fpa-list)))
    (cl-loop for invoice-idx to (- n 1)
             collect
             (cl-loop for element in fpa-list
                      for id = (cadr element)
                                        ; split here all elements starting with 2 (fatturabody)
                      for element-prefix = (substring (symbol-name id) 0 1)
                      for header-flag = (string= element-prefix "1")
                      collect (if header-flag element
                                (list (car element)
                                      (cadr element)
                                      (nth invoice-idx (caddr element))))))))

(defun fpa--reshape-for-multi-lines (fpa-list)
  "Reshape the FPA-LIST repeating headers for each line. Return list
of lines."
  (let ((n (fpa--count-multi 'lines fpa-list)))
    (if (= n 1) (list fpa-list) ; return unchanged
      (cl-loop for line-id below n
               collect
               (cl-loop for element in fpa-list
                        for id = (cadr element)
                        ;; split elements starting with 2-2-1 (dettagliolinee)
                        for element-prefix = (substring (symbol-name id) 0 5)
                        for header-flag = (not (string= element-prefix "2-2-1"))
                        collect (if header-flag element
                                  (list (car element)
                                        (cadr element)
                                        (nth line-id (caddr element)))))))))

(defun fpa--sanity-check-1 (fpa-list)
  "Validate each element has length == 1."
  (when (not (cl-loop for el in fpa-list
                      always (stringp (car el))
                      always (symbolp (cadr el))
                      always (listp (caddr el))
                      always (= (length (caddr el)) 3)
                      finally return t))
    (error "Sanity check #1: elements length <> 1.")))

(defun fpa-file-to-invoice (file-name)
  "Parse FILE-NAME and return list of lines by invoices."
  (let* ((tree (fpa--xml-to-tree file-name))
         (invoices (fpa--invoices (fpa--tree-get-all-values tree))))
    (cl-loop for invoice in invoices
             for lines = (fpa--reshape-for-multi-lines invoice)
             never (cl-loop for line in lines do (fpa--sanity-check-1 line))
             collect lines)))

(fpa-file-to-invoice "c:/Users/c740/OneDrive/org/projects/fpa-reader/test/IT01234567890_FPA03.xml")

(defconst fpa--separator ";" "Separator for export to string.")

(defconst fpa--invalid-regexps-in-string `("\n" "\257" ,fpa--separator)
  "Regexps to be removed from strings during conversion.")

(defconst fpa--string-max-length 1099 "Max length for parser.")

(defun fpa--sanitize-string (str)
  "Remove invalid characters from string STR, and truncate if too long."
  (cl-loop for rx in fpa--invalid-regexps-in-string
           for cleaned = (replace-regexp-in-string rx "" str) then
           (replace-regexp-in-string rx "" cleaned)
           for max-len = (min (length cleaned) fpa--string-max-length)
           for shortened = (substring cleaned 0 max-len)
           finally return shortened))

(defun fpa--to-strings (headers-and-lines what)
  "Return HEADERS-AND-LINES converted to list of strings. WHAT
select if return column names only, or list of rows only, or
both. Output is not ready for printing, as it's a list of
strings."
  (let* ((column-names
          (cl-loop for column in (car headers-and-lines)
                   for column-name = (car column)
                   ;; using `then' and format to avoid trailing separator
                   then (format "%s%s" fpa--separator (car column))
                   concat column-name))
         (column-values
          (cl-loop
           for row in headers-and-lines
           for row-s = (cl-loop
                        for col in row
                        for col-val-raw = (cadr col)
                        for col-val = (fpa--sanitize-string col-val-raw)
                        for col-str = col-val then
                        (format "%s%s" fpa--separator col-val)
                        concat col-str)
           collect row-s)))
    (pcase what
      ('column-names column-names)   ; return a string
      ('column-values column-values) ; return list of strings
      ('all (list column-names column-values)))))

(defun fpa--strings-to-buffer (strings &optional save-to-file)
  "Print STRINGS to buffer. Optionally, save to file 'fpa--output-file'."
  (let ((out-buffer "*FPA*"))
    (save-excursion
      (with-output-to-temp-buffer out-buffer
        (goto-char (point-min))
        ;; print header
        (princ (format "%s\n" (car strings)))
        ;; print lines
        (dolist (s (cadr strings))
          (princ (format "%s\n" s)))
        (pop-to-buffer out-buffer)
        (if save-to-file
            (when (file-writable-p fpa--output-file)
              (if (file-exists-p fpa--output-file)
                  (let ((backup-name
                         (concat (make-backup-file-name
                                  fpa--output-file) 
                                 (format-time-string "%Y%m%dT%H%M%S"))))
                    (copy-file fpa--output-file backup-name)))
              (write-region nil nil fpa--output-file)))))))

(defun fpa--valid-files (file-names)
  "Return only valid fpa files from FILE-NAMES."
  (let ((valids (seq-filter
                 (lambda (f)
                   (and (string-match "\\.xml" f)
                        (not (string-match "metaDato" f))
                        (not (string-match "p7m" f)))) file-names)))
    (if (not valids) (error "No valid file remained")) valids))

(defun fpa-file-to-buffer (file-name-or-names &optional save-to-file)
  "Convert FILE-NAME-OR-NAMES to buffer. Include all invoices in each
file. FILE-NAME-OR-NAMES is a file path, or a list of file paths."
  ;; convert single name to list
  (let* ((file-names-raw (if (listp file-name-or-names)
                             file-name-or-names (list file-name-or-names)))
         ;; filter for valid file names only
         (file-names (or (fpa--valid-files file-names-raw)
                         (error "Cannot continue. No files.")))
         ;; get header from first file (they are all the same)
         (header (fpa--to-strings (fpa--expand-flat-headers-and-lines
                                   (fpa--flatten-list
                                    (car (fpa--split-list-by-invoices
                                          (fpa--invoice-file-to-list
                                           (car file-names)))))) 'column-names))
         (header-with-file-name (format "%s%s%s%s%s" "FileName" fpa--separator
                                        "FilePath" fpa--separator header))
         ;; collect all invoices from all files
         (invoices-strings
          ;; iterate file names
          (cl-loop
           for file in file-names
           do (message (format "Working on %s" file))
           ;; create line prefix with file infos
           for file-info = (format "%s%s%s%s" (file-name-base file)
                                   fpa--separator file fpa--separator)
           ;; get a single file strings
           for file-s = (let*
                            ((fpa-list (fpa--invoice-file-to-list file))
                             (invoices (fpa--split-list-by-invoices fpa-list)))
                          ;; iterate invoices in a file
                          (cl-loop
                           for invoice in invoices
                           for flatten = (fpa--flatten-list invoice)
                           for h-and-l = (fpa--expand-flat-headers-and-lines
                                          flatten)
                           for lines = (fpa--to-strings h-and-l 'column-values)
                           ;; add file info to each row
                           for lines-f = (seq-map (lambda (l)
                                                    (format "%s%s" file-info l))
                                                  lines)
                           ;; append lines to single file
                           append lines-f))
           ;; append file strings to all invoices' lines
           append file-s)))
    ;; send everything to buffer
    ;; (header (invoice-1-line-1 invoice-1-line-2 invoice-2-line-1 etc))
    (fpa--strings-to-buffer
     (list header-with-file-name invoices-strings) save-to-file)))

