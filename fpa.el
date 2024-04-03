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

;;;; convert xml

(require 'xml)

(defvar fpa--schema-file-name "~/org/projects/fpa-reader/fpa-schema.el"
  "File containing the schema to parse `FatturaPA' xml.

Schema file is a elisp-formatted list derived from
`fatturapa.gov.it'.  Each element contains the identifier, the
flag if the elements is to be imported, the 'path' to extract the
corresponding element from the XML, and a label.")

(defun fpa--schema ()
  "Return schema from file `fpa--schema-file-name' as Lisp object.

Schema is formatted as a list with this structure:
 (<identifier symbol 1-1-2>
 <import t or nil>
 <search path in xml (FatturaElettronicaHeader DatiTrasmissione ProgressivoInvio)
 <label string>)."
  (with-temp-buffer
    (insert-file-contents fpa--schema-file-name)
    (goto-char (point-min))
    (read (current-buffer))))

(defvar fpa--root-header-prefixes '(ns0 n0 ns1 ns2 ns3 b p nil)
  "List of possible prefix headers for the top level.")

(defun fpa--root-keys ()
  "Return all possible keys for the root header, combining the root
from the schema file and the `fpa--root-header-prefixes'."
  (seq-map (lambda (p)
             (let ((rt "FatturaElettronica"))
               (if p (intern (concat (symbol-name p) ":" rt))
                 (intern rt)))) fpa--root-header-prefixes))

(defvar fpa--bad-regexps '("‚" "‚è" "‚" "‚"
                           "‚¯" "‚_"
                           "‚‚" "‚" "‚–" "‚“"
                           "º" "‚º" "" "‚" "\202")
  "Regexps to replace with ''.

There are invalid characters in some examples of fatture,
especially the ones processed with digital signature, therefore
they are replaced with '' during parsing.")

(defun fpa--valid-files (file-names)
  "Return only valid fpa files from FILE-NAMES."
  (let ((valids (seq-filter
                 (lambda (f)
                   (and (not (string-match "metaDato" f))
                        ;; (not (string-match "p7m" f))
                        (string-match "\\.xml" f)))
                 file-names)))
    (if (not valids) (message "No valid file remained")) valids))

(defun fpa--xml-to-tree (file-name)
  "XML-parse FILE-NAME and return top node tree as xml tree.

This loader is heavily patched and is built incrementally with
more cases as soon as they manifest."
  (let ((parsed-xml-region
         (if (string= "p7m" (file-name-extension file-name))
             (with-temp-buffer
               (insert-file-contents file-name)
               ;; replace bad regexps
               (dolist (rxp fpa--bad-regexps)
                 (replace-regexp-in-region rxp "" (point-min)))
               ;; replace illegal headers
               (replace-regexp-in-region
                (rx (or "DataRiferimentoXTerminiPagamento"
                        "DataRiferimentoTerminiPaOgamento"))
                "DataRiferimentoTerminiPagamento" (point-min))
               ;; replace other strange characters
               (replace-regexp-in-region "<Al\4" "<Al" (point-min))
               (replace-regexp-in-region "<Al\3" "<Al" (point-min))
               (replace-regexp-in-region "ImpwortoPagamento"
                                         "ImportoPagamento" (point-min))
               (goto-char (point-min))
               (or
                ;; legal version
                (re-search-forward
                 (rx (group "<?xml version"
                            (one-or-more anychar)
                            "FatturaElettronica>")) nil t)
                ;; illegal version
                (progn
                  (replace-regexp
                   (rx "<FatturaElettronica" (one-or-more anychar)
                       "FatturaElettronica>")
                   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\\&")
                  (goto-char (point-min))
                  (re-search-forward
                   (rx (group "<?xml version"
                              (one-or-more anychar)
                              "FatturaElettronica>")) nil t))
                ;; base-64 encoded version
                (progn
                  (base64-decode-region (point-min) (point-max))
                  (goto-char (point-min))
                  (re-search-forward
                   (rx (group "<?xml version"
                              (one-or-more anychar)
                              "FatturaElettronica>")) nil t)))
               (xml-parse-region (match-beginning 0) (match-end 0)))
           (xml-parse-file file-name)))
        (tree))
    (cl-loop for key in (fpa--root-keys)
             for tree = (assq key parsed-xml-region)
             if tree return tree)))

(defun fpa--schema-key-get (what key)
  "Return WHAT from the KEY schema key.

WHAT can be a symbol: `import-flag', `id', `path', or `label'.
If `label' is not present, the last value of path is converted to
string and returned as label."
  (pcase what
    ('import-flag (nth 1 key))
    ('id (nth 0 key))
    ('path (nth 2 key))
    ;; label: either is present, or last value of path
    ('label (or (nth 3 key)
                (symbol-name
                 (seq-first (reverse (fpa--schema-key-get 'path key))))))))

(defun fpa--tree-get-value-from-path (tree path)
  "Return raw xml from TREE, parsing PATH w/ `xml-get-children'.

Path: (root child1 child2 .. childN leaf)"
  (cond ((not path) tree)
        (t (let ((children (xml-get-children tree (pop path))))
             (if (= 1 (length children))
                 (fpa--tree-get-value-from-path (car children) path)
               (cl-loop for child in children
                        collect
                        (fpa--tree-get-value-from-path child path)))))))

(defun fpa--tree-get-value-from-key (tree key)
  "Search KEY in TREE and return enriched value.

Only if flagged with non-nil `import' (see schema file
specs).  Return list (label id (value-or-values))."
  (when (fpa--schema-key-get 'import-flag key)
    (let* ((path (fpa--schema-key-get 'path key))
           (value (fpa--tree-get-value-from-path tree path))
           (label (fpa--schema-key-get 'label key))
           (id (fpa--schema-key-get 'id key)))
      (list label id value))))

(defun fpa--tree-get-all-values (tree)
  "Return all values from TREE using `fpa--schema-file-name'.

Result is an assoc list (`fpa-list' as input in following
 functions) using `label' (see schema specs) as key."
  (let* ((keys (fpa--schema))
         (f (lambda (k) (fpa--tree-get-value-from-key tree k)))
         (raw-values (seq-map f keys))
         ;; remove all nils
         (cleaned-values (seq-filter #'identity raw-values)))
    cleaned-values))

(defconst fpa--invoice-id-identifier '2-1-1-4
  "Immutable identifier for the invoice id, used to separate
 different invoices in the same file.")

(defconst fpa--lines-id-identifier '2-2-1-1
  "Immutable identifier for the line id, used to separate
 different lines in the same file.")

(defun fpa--count-multi (what fpa-list)
  "Return number of WHAT in FPA-LIST.

WHAT is either `invoices' or `lines'."
  (let ((id (pcase what
              ('invoices fpa--invoice-id-identifier)
              ('lines fpa--lines-id-identifier))))
    (let* ((id--schema-key (assoc id (fpa--schema)))
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
    (if (= n 1) (list fpa-list) ; return unchanged
      (cl-loop for invoice-idx below n
               collect
               (cl-loop for element in fpa-list
                        for id = (cadr element)
                                        ; split here all elements starting with 2 (fatturabody)
                        for element-prefix = (substring (symbol-name id) 0 1)
                        for header-flag = (string= element-prefix "1")
                        collect (if header-flag element
                                  (list (car element)
                                        (cadr element)
                                        (nth invoice-idx (caddr element)))))))))

(defun fpa--reshape-for-multi-lines (fpa-list)
  "Reshape the FPA-LIST repeating headers for each line. Return list
of lines.

Since an invoice can contains multiple lines, this functions
return a list of `fpa-list' with all the header fields for each
line, as example invoice recipient, invoice number, etc."
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
                      for val = (caddr el)
                      always (and (= (length val) 3)
                                  (symbolp (car val))
                                  (not     (cadr val))
                                  (stringp (caddr val)))
                      finally return t))
    (error "Sanity check #1: elements length <> 1.")))

(defun fpa--prepare-line (line)
  "Reshape LINE in a (header value) list."
  (cl-loop for element in line
           for header = (car element)
           for raw-value = (or (caddr (caddr element)) "")
           for sanitized-value = (fpa--sanitize-string raw-value)
           collect (list header sanitized-value)))

(defun fpa--file-to-invoice (file-name)
  "Parse FILE-NAME and return list of lines by invoices.

Return a list: (<invoices>
                (<lines>
                 (<line>
                  (inv1-line1-header inv1-line1-value)
                  (inv1-line2-header inv1-line2-value)))
               (((inv2-line1-header ...))))"
  (let* ((tree (fpa--xml-to-tree file-name))
         (invoices (fpa--invoices (fpa--tree-get-all-values tree))))
    (cl-loop for invoice in invoices
             for lines = (fpa--reshape-for-multi-lines invoice)
             collect (cl-loop for line in lines
                              ;; do (fpa--sanity-check-1 line)  ;; TODO move this check after the line is prepared. actually, put in 'prepare line'
                              collect (fpa--prepare-line line)))))

(defconst fpa--separator ";" "Separator for export to string.")

(defconst fpa--invalid-regexps-in-string `("\n" "\257" ,fpa--separator)
  "Regexps to be removed from strings during conversion.")

(defconst fpa--string-max-length 1099 "Max length for parser.")

(defun fpa--sanitize-string (str)
  "Remove invalid characters from string STR, and truncate if too long.

Invalid characters are in
`fpa--invalid-regexps-in-string' (including the separator).  Max
length of string is defined in `fpa--string-max-length'."
  (cl-loop for rx in fpa--invalid-regexps-in-string
           for cleaned = (replace-regexp-in-string rx "" str) then
           (replace-regexp-in-string rx "" cleaned)
           for max-len = (min (length cleaned) fpa--string-max-length)
           for shortened = (substring cleaned 0 max-len)
           finally return shortened))

(defun fpa--line-to-string (line &optional file-info)
  "Convert line to string.

Use separator `fpa--separator'.  Optional, append FILE-INFO (list
of file info) at the end of the line."
  ;; check if fpa-separator same as csv-separator, and warn user
  (if (or (member fpa--separator csv-separators)
           (yes-or-no-p (concat "fpa--separator not in csv-separators.  "
                                "csv-mode might not work.  Continue?")))
      (let ((str (cl-loop for el in line
                          for el-v = (cadr el)
                          for el-s = el-v then (format "%s%s" fpa--separator el-v)
                          concat el-s)))
        (if (and file-info (listp file-info))
            (concat str (cl-loop for info in file-info
                                 for i-s = (format "%s%s" fpa--separator info)
                                 concat i-s)) str))
    (error "Aborted")))

(defun fpa--file-info (&optional file-name)
  "Return header or file info list for FILE-NAME.

If no FILE-NAME is provided, return list of header column names
for the lines-specific file info."
  (cond
   ;; return header
   ((not file-name) (list "File name"))
   ;; return file info
  (file-name (list (file-name-base file-name)))))

(defun fpa--file-to-line-strings (file-name)
  "Return list of strings for each line and invoice."
  (cl-loop for invoice in (fpa--file-to-invoice file-name)
           for file-info = (fpa--file-info file-name)
           append (cl-loop for line in invoice
                           for line-ext = (fpa--line-to-string line file-info)
                           append (list line-ext))))

(defun fpa--header-string (&optional file-info)
  "Return string representing header, from `fpa--schema-file-name'.

If FILE-INFO is not-nil, append file info header columns."
  (let* ((schema (fpa--schema))
         (keys (cl-loop for key in schema
                        if (fpa--schema-key-get 'import-flag key)
                        collect (fpa--schema-key-get 'label key)))
         (header (cl-loop for k in keys
                          for k-s = k then (format "%s%s" fpa--separator k)
                          concat k-s)))
    (if file-info
        (concat header
                (cl-loop for col in (fpa--file-info)
                         for col-s = (format "%s%s" fpa--separator col)
                         concat col-s)) header)))

;;;; output to buffer and/or file

(require 'csv-mode)

;; TODO transform to variable customizable
(defun fpa--output-file ()
  "Return file name for output, timestamped."
  (concat "~/org/projects/fpa-reader/out/"
          (format-time-string "%Y-%m-%d-%H%M%S")
          "-invoices.csv"))

(defun fpa--strings-to-buffer (header line-strings &optional save-to-file)
  "Print HEADER and STRINGS to buffer in csv-mode.

Optionally, save to file `fpa--output-file'."
  (let ((out-buffer "*FPA*"))
    (save-excursion
      (with-output-to-temp-buffer out-buffer
        (goto-char (point-min))
        ;; print header
        (princ (format "%s\n" header))
        ;; print lines
        (dolist (line line-strings)
          (princ (format "%s\n" line)))
        (pop-to-buffer out-buffer)
        (csv-mode)
        (csv-align-mode)
        (if save-to-file
            (when (file-writable-p (fpa--output-file))
              (if (file-exists-p (fpa--output-file))
                  (let ((backup-name
                         (concat (make-backup-file-name
                                  (fpa--output-file)) 
                                 (format-time-string "%Y%m%dT%H%M%S"))))
                    (copy-file (fpa--output-file) backup-name)))
              (write-region nil nil (fpa--output-file))))))))

(defun fpa--count-valid-files (file-name-or-names)
  "Count number of valid files in FILE-NAME-OR-NAMES."
  (let ((file-names-list (if (listp file-name-or-names)
                            file-name-or-names (list file-name-or-names))))
    (message (format "Valid files: %s"
                     (if file-names-list
                         (length (fpa--valid-files file-names-list)) 0)))))

(defun fpa-file-to-buffer (file-name-or-names &optional save-to-file)
  "Convert FILE-NAME-OR-NAMES to buffer.

Include all invoices in each file.  FILE-NAME-OR-NAMES is a file
path, or a list of file paths.
Example: `(fpa-file-to-buffer
           (directory-files \"~/org/projects/MAMA/inbox\" t
                         directory-files-no-dot-files-regexp) t)'"
  ;; convert single name to list
  (let* ((file-names-raw (if (listp file-name-or-names)
                             file-name-or-names (list file-name-or-names)))
         ;; filter for valid file names only
         (file-names (or (fpa--valid-files file-names-raw)
                         (error "Cannot continue. No files.")))
         (header (fpa--header-string t))
         (line-strings (cl-loop for file in file-names
                                do (message (format "Working on %s" file))
                                append (fpa--file-to-line-strings file))))
    (fpa--strings-to-buffer header line-strings save-to-file)))

;;;; hydra

(require 'hydra)

(defhydra hydra-fpa (:color blue :hint nil)
  "
     fpa-reader commands
     -------------------
     _f_: convert file at point from dired
     _a_: convert all files in current directory in dired
     _c_: count valid files among the dired marked
"
  ("f" (fpa-file-to-buffer (dired-get-marked-files)))
  ("a" (progn (dired-mark-subdir-files)
              (fpa-file-to-buffer (dired-get-marked-files))))
  ("b" hydra-shortcuts/body "back")
  ("c" (fpa--count-valid-files (dired-get-marked-files)))
  ("q" nil "quit"))
