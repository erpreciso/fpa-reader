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

(defun fpa--get-schema ()
  "Return schema from file `fpa-schema-file' as Lisp object."
  (with-temp-buffer
    (insert-file-contents fpa-schema-file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun fpa--get-schema-root ()
  "Return root label of `fpa-schema-file'. It's used to parse the XML."
  (cadddr (fpa--get-schema)))

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
             (let ((rt (fpa--get-schema-root)))
               (if p (intern (concat (symbol-name p) ":" rt))
                 (intern rt)))) fpa--root-header-prefixes))
  
(defun fpa--xml-to-tree (file-name)
  "XML-parse FILE-NAME and return top node tree as xml tree."
  (let ((parsed-xml-region (xml-parse-file file-name))
        (tree))
    (cl-loop for key in (fpa--root-keys)
             for tree = (assq key parsed-xml-region)
             if tree return tree)))

(defun fpa--tree-to-list (schema tree prefix)
  "Convert fpa xml TREE to nested list. The algo walks in parallel
the nested schema and the nested tree."
  (cl-assert (= (length schema) 5))  ;; sanity check on schema
  (let* ((schema-name         (nth 3 schema))
         (schema-children     (nth 4 schema))
         (schema-id           (nth 0 schema))
         (schema-level   (fpa--get-level schema-id))
         (new-prefix (if (> schema-level 2)
                         ;; in prefix, include only levels below 2 to save ink
                         (format "%s|%s" prefix schema-name)
                       schema-name)))
    (cond ((not schema-children) ;; exit recursion and return leaf
           (list schema-id new-prefix (or (nth 2 tree) "empty")))
          (schema-children
           (cl-loop for schema-child in schema-children
                    for child-name = (intern (nth 3 schema-child))
                    for child-id = (nth 0 schema-child)
                    for child-trees = (xml-get-children tree child-name)
                    collect
                    (list child-id child-name
                          (if child-trees
                              ;; recurse for one or multiple children
                              ;; return always a list, even for single child
                              (cl-loop for child-tree in child-trees
                                       collect
                                       (fpa--tree-to-list schema-child
                                                          child-tree new-prefix))
                            ;; if no children, no value, then return empty leaf
                            (fpa--tree-to-list schema-child nil new-prefix))))))))

(defun fpa--invoice-file-to-list (file-name)
  "Convert invoice at FILE-NAME to nested list, calling the
recursive `fpa--tree-to-list' function on a root schema and a
root tree from file-name."
  (let ((schema (fpa--get-schema))
        (tree (fpa--xml-to-tree file-name)))
    (fpa--tree-to-list schema tree "root")))

(defun fpa--split-list-by-invoices (fpa-list)
  "Return list of fpa-lists, each list one invoice in original
FPA-LIST. It returns different fpa-lists repeating the header and
keeping only one invoice data."
  (let* ((header    (car fpa-list))
         (body      (cadr fpa-list))
         (body-id   (car body))
         (body-name (cadr body))
         (invoices  (caddr body)))
    (cl-loop for invoice in invoices
             collect (list header (list body-id body-name invoice)))))

(defun fpa--get-lines-count (fpa-list)
  "Return number of lines in FPA-LIST."
  (let* ((body (cadr fpa-list))
         (data1 (cadr body))
         (data2 (caddr data1))
         (lines (caddr (car (car data2)))))
    (length lines)))

(defun fpa--element-type (el)
  "Return type of ELement by checking types of element' elements,
and recurse in case of nested elements."
  (cond ((not (listp el)) (error "invalid element"))
        ((= (length el) 1) (fpa--element-type (car el)))
        ((seq-every-p #'stringp el)  'leaf)
        ((and (stringp (car el)) (symbolp (cadr el)))    'parent)
        ((seq-every-p (lambda (e) (eq (fpa--element-type e) 'parent)) el) 'parents)
        ((seq-every-p (lambda (e) (eq (fpa--element-type e) 'leaf)) el) 'leaves)
        ((seq-every-p (lambda (e) (eq (fpa--element-type e) 'parents)) el)
         'list-of-parents)))

(ert-deftest fpa--test-element-type ()
  (should (equal (fpa--element-type '("2.5" "Att" "e")) 'leaf))
  (should (equal (fpa--element-type '(("2.5" "Att" "e"))) 'leaf))
  (should (equal (fpa--element-type '(("1.11" "Ca" "L")
                                      ("1.11" "Ca" "S"))) 'leaves))
  (should (equal (fpa--element-type '((("1" IdP (("1" "dP" "IT")))
                                       ("2" Cod (("2" "dC" "01")))))) 'parents))
  (should (equal (fpa--element-type '("2" Tipo ("2" "Tipo" "e"))) 'parent))
  (should (equal (fpa--element-type '((("1.1" N (("1.1" "Num" "1")))
                                       ("1.2" Tip ("1.2" "Dat" "e")))
                                      (("1.1" N (("1.1" "Num" "2")))
                                       ("1.2" Tip ("1.2" "Dat" "e")))))
                 'list-of-parents)))

(defun fpa--flatten-list (fpa-list)
  "Return flattened single-invoice FPA-LIST. This list is not really
flat since it contains repetitions of multi-value fields such as
lines. Therefore, this function must be followed by other reshape
functions such as `fpa--extract-lines' and
`fpa--list-to-dataframe'. Note that it is mandatory to run this
function on a single-invoice list, otherwise there will be
missing data."
  (let ((out nil)) ;; list to collect <key value>
    (defun fpa--flatten-el (el)
      (if (and (listp el) (= (length el) 1))
          (fpa--flatten-el (car el)) ;; recurse into single-element list
        (pcase (fpa--element-type el)
          ('leaf (push (list (cadr el) (caddr el)) out)) ;; exit recursion
          ('leaves (seq-map (lambda (e) (fpa--flatten-el e)) el))
          ('parent (fpa--flatten-el (caddr el))) ;; recurse in nested elements
          ('parents (seq-map (lambda (e) (fpa--flatten-el (caddr e))) el))
          ;; below the case when elements repeat, such as Linee
          ('list-of-parents (seq-map (lambda (e) (fpa--flatten-el e)) el)))))
    (fpa--flatten-el fpa-list)
    (reverse out)))

(defun fpa--expand-flattened-headers-and-lines (flattened-list)
  "Return list of all common headers (elements that do not repeat)
and each elements of each line of FLATTENED-LIST. This function
relies on some hard-coded identifiers of where the first line is
in the flattened list, and the string to identify
it. (((headers) (line1)) ((headers) (line2)))"
  (let* ((f flattened-list)
         (first-line-id 153)
         (elements-per-line 22)
         (number-of-lines 
          ;; count number of lines
          (cl-loop for i from first-line-id to (length f) by elements-per-line
                   for name = (car (nth i f))
                   for name-match = (substring name (- (length name) 11))
                   if (string= name-match "NumeroLinea") 
                   sum 1))
         (last-line-element
          (+ first-line-id (* number-of-lines elements-per-line)))
         (lines
          ;; For each line ...
          (cl-loop for i to (- number-of-lines 1)
                   collect
                   ;; ... extract each line's elements
                   (cl-loop for j to (- elements-per-line 1)
                            for el-idx = (+ first-line-id
                                            (+ j (* elements-per-line i)))
                            collect (nth el-idx f))))
         (headers
          (append (cl-loop for i to (- first-line-id 1) collect (nth i f))
                  (cl-loop for i from last-line-element to (- (length f) 1)
                           collect (nth i f)))))
    (cl-loop for line in lines
             collect (append headers line))))

(defconst fpa--separator ";" "Separator for export to string.")

(defconst fpa--invalid-regexps-in-string `("\n" "\257" ,fpa--separator)
  "Regexps to be removed from strings during conversion.")

(defun fpa--clean-value (val)
  "Remove invalid characters from string VAL."
  (cl-loop for rx in fpa--invalid-regexps-in-string
           for cleaned = (replace-regexp-in-string rx "" val) then
           (replace-regexp-in-string rx "" cleaned)
           finally return cleaned))

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
                        for col-val = (fpa--clean-value col-val-raw)
                        for col-str = col-val then
                        (format "%s%s" fpa--separator col-val)
                        concat col-str)
           collect row-s)))
    (pcase what
      ('column-names column-names)   ; return a string
      ('column-values column-values) ; return list of strings
      ('all (list column-names column-values)))))

(defvar fpa--output-file "~/org/projects/fpa-reader/out/out.csv")

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

(defun fpa-file-to-buffer (file-name-or-names &optional save-to-file)
  "Convert FILE-NAME-OR-NAMES to buffer. Include all invoices in each
file. FILE-NAME-OR-NAMES is a file path, or a list of file paths."
  ;; convert single name to list
  (let* ((file-names (if (listp file-name-or-names)
                         file-name-or-names (list file-name-or-names)))
         ;; get header from first file (they are all the same)
         (header (fpa--to-strings (fpa--extract-header-and-lines
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
                           for h-and-l = (fpa--extract-header-and-lines flatten)
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

(fpa-file-to-buffer fpa-test-files t)
