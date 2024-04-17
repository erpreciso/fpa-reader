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

;;;; manage schema file

(defvar fpa--schema-file-name "~/org/projects/fpa-reader/fpa-schema.el"
  "File containing the schema to parse `FatturaPA' xml.

Schema file is a elisp-formatted list derived from
`fatturapa.gov.it'.  Each element contains the identifier, the
flag if the elements is to be imported, the 'path' to extract the
corresponding element from the XML, and a label.")

(defun fpa--get-schema (&optional schema-file)
  "Return schema from `fpa--schema-file-name' as Lisp object.

If SCHEMA is not-nil, use as schema file name.
Schema is formatted as a list with this structure:
 (<identifier symbol 1-1-2>
 <import t or nil>
 <search path in xml (FatturaElettronicaHeader DatiTrasmissione ProgressivoInvio)
 <label string>)."
  (with-temp-buffer
    (insert-file-contents (or schema-file fpa--schema-file-name))
    (goto-char (point-min))
    (read (current-buffer))))

(defun fpa--schema-get-key (what key)
  "Return WHAT from the KEY schema key.

WHAT can be a symbol: 'id', 'path', `detail-flag',
'detail-label', `summary-flag', 'summary-label', 'database-flag',
'database-label'.  If `-label' is not present, the last value of
path is converted to string and returned as label."
  ;; TODO need fallback for unique key??
  (let* ((path (nth 1 key))
         (last-word-of-path (symbol-name (seq-first (reverse path)))))
    (pcase what
      ('id (nth 0 key))
      ('path path)
      ('detail-flag (nth 2 key))
      ('detail-label (or (nth 3 key) last-word-of-path))
      ('summary-flag (nth 4 key))
      ('summary-label (or (nth 5 key) last-word-of-path))
      ('database-flag (nth 6 key))
      ('database-label (or (nth 7 key) last-word-of-path))
      ('unique-key (apply #'concat (seq-map #'symbol-name path))))))

(defun fpa--schema-get-value-p (key flag)
  "Depends on FLAG, get or not value from KEY.

FLAG can be `detail', 'summary', 'database'."
  (pcase flag
    ('detail (fpa--schema-get-key 'detail-flag key))
    ('summary (fpa--schema-get-key 'summary-flag key))
    ('database (fpa--schema-get-key 'database-flag key))))

(defun fpa--schema-get-label (key flag)
  "Return appropriate label for KEY, based on FLAG.

FLAG can be `detail', 'summary', 'database'."
  (pcase flag
    ('detail (fpa--schema-get-key 'detail-label key))
    ('summary (fpa--schema-get-key 'summary-label key))
    ('database (fpa--schema-get-key 'database-label key))))
  
;;;; clean and parse raw xml file; return xmltree

(require 'xml)

(defvar fpa--root-header-prefixes '(ns0 n0 ns1 ns2 ns3 b p P nil)
  "List of possible prefix headers for the top level.")

(defun fpa--get-root-keys ()
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

(defun fpa--get-valid-files (file-names)
  "Return only valid fpa files from FILE-NAMES."
  (let ((valids (seq-filter
                 (lambda (f)
                   (and (not (string-match "metaDato" f))
                        ;; (not (string-match "p7m" f))
                        (string-match "\\.xml" f)))
                 file-names)))
    (if (not valids) (message "No valid file remained")) valids))

(defun fpa--convert-xml-to-tree (file-name)
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
    (cl-loop for key in (fpa--get-root-keys)
             for tree = (assq key parsed-xml-region)
             if tree return tree)))

;;;; parse xml w/schema; return raw fpa-list (messed-up if invoices and lines)

(defun fpa--tree-get-value-from-path (tree path)
  "Return parsed xml from TREE, parsing PATH w/ `xml-get-children'.

Path: (root child1 child2 .. childN leaf).  If the leaf is only
one value, it is returned as is.  Otherwise, it's a list."
  (cond ((not path) (caddr tree))
        (t (let ((children (xml-get-children tree (pop path))))
             (if (= 1 (length children))
                 (fpa--tree-get-value-from-path (car children) path)
               (cl-loop for child in children
                        collect
                        (fpa--tree-get-value-from-path child path)))))))

(defun fpa--tree-get-value-from-key (tree key flag)
  "Search KEY in TREE and return enriched value.

FLAG select if the value is to be imported or not, and the
 appropriate label."
  (when (fpa--schema-get-value-p key flag)
    (let* ((path (fpa--schema-get-key 'path key))
           (value (fpa--tree-get-value-from-path tree path))
           (label (fpa--schema-get-label key flag))
           (id (fpa--schema-get-key 'id key)))
        (list label id value))))

(defun fpa--convert-tree-to-fpalist (tree flag &optional schema-file)
  "Return all values from TREE using SCHEMA-FILE or `fpa--schema-file-name'.

Result is a list (`fpa-list' as input in following functions)
 whose car is FLAG and cadr an assoc list using `label' (see
 schema specs) as key. The FLAG determines which fields are to be
 imported, and how they are labeled."
  (let* ((keys (fpa--get-schema schema-file))
         (f (lambda (k) (fpa--tree-get-value-from-key tree k flag)))
         (raw-values (seq-map f keys))
         ;; remove all nils
         (cleaned-values (seq-filter #'identity raw-values)))
    (list flag cleaned-values)))


;;;; invoice struct

(cl-defstruct invoice-line
  "Line of an invoice.  Represent a single transaction within an
invoice."
  id
  description
  unit-taxable-price
  unit-tax-rate
  amount-taxable
  amount-tax
  amount)

(cl-defstruct invoice "Struct for an invoice."
              id
              date
              (payment-due-date nil)
              (payment-amount nil)
              seller-tax-id
              (seller-name nil)
              (buyer-tax-id nil)
              (buyer-name nil)
              (lines nil)
              (lines-amount-taxable nil)
              (lines-amount-tax nil)
              )

(defvar fpa--invoices-db nil "Invoices database (a list).")

(defun fpa--invoices-get-or-create (id date seller-tax-id)
  "Get invoice having ID and DATE from 'fpa--invoices'.  If the
invoice does not exist, create it."
  (or
   (seq-filter (lambda (inv)
                 (and (string= (invoice-id inv) id)
                      (string= (invoice-seller-tax-id inv) seller-tax-id)
                      (string= (invoice-date inv) date))) fpa--invoices-db)
   (let ((new-invoice
          (make-invoice :id id :date date :seller-tax-id seller-tax-id)))
     (push new-invoice fpa--invoices-db)
     new-invoice)))

(defun fpa--file-to-invoices-db (file-name &optional schema-file)
  "Populate 'fpa--invoices-db' with invoices/lines from FILE-NAME.

FILE-NAME is parsed using the 'database' flag."
  (let* ((flag 'database)
         (file-name fpa-test--multi-invoice-multi-line-file)
         (hvs (fpa--file-to-invoices-lines file-name flag schema-file)))
    hvs))

;; (fpa--file-to-invoices-db nil)

;;   ;; iterate invoices
;;   (cl-loop for invoice in header-value-list
;;            ;; iterate lines
;;            (cl-loop for line in invoice
                    
;;;; dedup invoices; return list of single-invoice fpa-list

;;;;; count invoices or lines support function
(defconst fpa--invoice-id-identifier '2-1-1-4
  "Immutable identifier for the invoice id, used to separate
 different invoices in the same file.")

(defconst fpa--lines-id-identifier '2-2-1-1
  "Immutable identifier for the line id, used to separate
 different lines in the same file.")

(defun fpa--count-multi (what fpa-list &optional schema-file)
  "Return number of WHAT in FPA-LIST.

WHAT is either `invoices' or `lines'. If SCHEMA-FILE is not-nil,
use that for schema."
  (let ((id (pcase what
              ('invoices fpa--invoice-id-identifier)
              ('lines fpa--lines-id-identifier))))
    (let* ((flag (car fpa-list))
           (fpa-list-keys (cadr fpa-list))
           (id--schema-key (assoc id (fpa--get-schema schema-file)))
           (id--schema-label (fpa--schema-get-label id--schema-key flag))
           (fpa-ids-el (assoc id--schema-label fpa-list-keys))
           (fpa-ids (caddr fpa-ids-el)))
      (if (stringp fpa-ids) 1 (length fpa-ids)))))

;;;;; split invoices

(defun fpa--split-invoices-in-fpalist (fpa-list)
  "Return list of invoices from FPA-LIST.

Each invoice is a fpa-list made of flag and keys."
  (let ((n (fpa--count-multi 'invoices fpa-list))
        (flag (car fpa-list))
        (fpa-list-keys (cadr fpa-list)))
    (cond ((= n 1) (list fpa-list)) ; if only one invoice, return list of one
          ((> n 1)           ; otherwise, unpack the invoices
           (cl-loop
            for invoice-idx below n
            collect
            (list flag
                  (cl-loop for element in fpa-list-keys
                           for id = (cadr element)
                           ;; split here all elements starting with
                           ;; 2 (fatturabody)
                           for element-prefix = (substring (symbol-name id) 0 1)
                           for header-flag = (string= element-prefix "1")
                           collect (if header-flag element
                                     (list (car element)
                                           (cadr element)
                                           (nth invoice-idx
                                                (caddr element)))))))))))

;;;; dedup lines; return list of single-invoice/single-line fpa-list

(defun fpa--split-fpalist-in-lines (fpa-list-or-lists &optional schema-file)
  "Reshape the FPA-LIST-OR-LISTS repeating headers for each
line. Return list of lines.

Since an invoice can contains multiple lines, this functions
return a list of `fpa-list' with all the header fields for each
line, as example invoice recipient, invoice number, etc."
  ;; check if input is single invoice, or list of those
  (let ((fpa-lists (if (symbolp (car fpa-list-or-lists))
                       (list fpa-list-or-lists) fpa-list-or-lists)))
    (cl-loop
     for fpa-list in fpa-lists
     collect
     (let* ((n (fpa--count-multi 'lines fpa-list schema-file))
            (flag (car fpa-list))
            (fpa-list-keys (cadr fpa-list)))
       (cl-loop for line-id below n
                collect
                (list flag
                      (cl-loop for element in fpa-list-keys
                               for id = (symbol-name (cadr element))
                               ;; split only elements starting with
                               ;; 2-2-1 (dettagliolinee)
                               for header-flag = (not (string-prefix-p "2-2-1" id))
                               collect
                               (cond
                                ;; if it's header, return element as is
                                (header-flag element)
                                ;; if there is only one line, return
                                ;; element as is
                                ((= n 1) element)
                                ;; otherwise, get the corresponding linea
                                ;; element from the list of lists
                                (t (list (car element)
                                         (cadr element)
                                         (nth line-id (caddr element))))))))))))

;; (defun fpa--patch-riepilogo (element)
;;   "Patch when there are multiple VAT rates, therefore the summary
;;   amount is split in multiple lines.
;;   As example, ('Riepilogo Imponibile' 2-2-2-5
;;                ((ImponibileImporto nil '5.93')
;;                 (ImponibileImporto nil '4.75')
;;                 (ImponibileImporto nil '0.00'))) patched will return
;;    ('Riepilogo Imponibile' 2-2-2-5 (ImponibileImporto nil 10.68))"
;;   (let* ((to-aggregate (caddr element)))
;;     ;; return unchanged if there is only one line
;;     (if (symbolp (car to-aggregate)) element
;;       (let ((aggregated-value
;;              ;; otherwise iterate and sum
;;              (cl-loop for el in to-aggregate
;;                       for h1 = (car el)
;;                       for h2 = (cadr el)
;;                       for val = (string-to-number (caddr el))
;;                       sum val into return-value
;;                       finally return
;;                       (list h1 h2 (number-to-string return-value)))))
;;         (list (car element)
;;               (cadr element)
;;               aggregated-value)))))

;;;; convert format from fpa-list to header-value

(defun fpa--convert-line-to-header-value (line)
  "Reshape LINE in a (header value) list."
  (let ((elements (cadr line)))
    (cl-loop for element in elements
             for header = (car element)
             for raw-value = (or (caddr element) "")
             for sanitized-value = (fpa--sanitize-string raw-value)
             collect (list header sanitized-value))))

(defun fpa--file-to-invoices-lines (file-name flag &optional schema-file)
  "Parse FILE-NAME and return list ('hvs') of lines by invoices.

Return a list whose car is FILE-NAME in the
'file-name-nondirectory' flavour, and cdr a list:
               (<invoices>
                (<lines>
                 (<line>
                  (inv1-line1-header inv1-line1-value)
                  (inv1-line2-header inv1-line2-value)))
               (((inv2-line1-header ...))))
FLAG determines the type of output.  Optionally, pass a
SCHEMA-FILE."
  (let* ((tree (fpa--convert-xml-to-tree file-name))
         (fpa-lists (fpa--split-invoices-in-fpalist
                     (fpa--convert-tree-to-fpalist tree flag schema-file)))
         (invoices-lines (fpa--split-fpalist-in-lines fpa-lists schema-file)))
    (cons (file-name-nondirectory file-name)
          (cl-loop for invoice in invoices-lines
                   collect
                   (cl-loop for line in invoice
                            collect (fpa--convert-line-to-header-value line))))))

;;;; convert to strings

;;;;; utils

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

(defun fpa--line-to-string (line file-name)
  "Convert LINE to string, using `fpa--separator'.  Prepend FILE-NAME."
  ;; check if fpa-separator same as csv-separator, and warn user
  (if (or (member fpa--separator csv-separators)
          (yes-or-no-p (concat "fpa--separator not in csv-separators.  "
                               "csv-mode might not work.  Continue?")))
      (format "%s%s%s" file-name
              fpa--separator
              (cl-loop for el in line
                       for el-v = (cadr el)
                       for el-s = el-v then (format "%s%s" fpa--separator el-v)
                       concat el-s))
  (error "Aborted")))

;;;;; end-to-end from file to strings

(defun fpa--file-to-line-strings (file-name flag &optional schema-file)
  "Return list of strings for each line and invoice.

FLAG determines the type of output: 'detail', 'summary' or 'database'."
  (let*  ((invoices-lines (fpa--file-to-invoices-lines file-name flag schema-file))
          (file-name (car invoices-lines))
          (invoices (cdr invoices-lines)))
    (cl-loop for invoice in invoices
             append (cl-loop for line in invoice
                             for line-string = (fpa--line-to-string line file-name)
                             append (list line-string)))))

(defun fpa--header-string (flag &optional schema-file)
  "Return string representing header, from `fpa--schema-file-name'.

If FILE-INFO is not-nil, append file info header columns."
  (let* ((schema (fpa--get-schema schema-file))
         (keys (cl-loop for key in schema
                        if (fpa--schema-get-value-p key flag)
                        collect (fpa--schema-get-label key flag)))
         (header (cl-loop for k in keys
                          for k-s = k then (format "%s%s" fpa--separator k)
                          concat k-s)))
        (format "%s%s%s" "file-name" fpa--separator header)))

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
                         (length (fpa--get-valid-files file-names-list)) 0)))))

(defun fpa-file-to-buffer (file-name-or-names flag &optional save-to-file)
  "Convert FILE-NAME-OR-NAMES to buffer.

Include all invoices in each file.  FILE-NAME-OR-NAMES is a file
path, or a list of file paths.
Example: `(fpa-file-to-buffer
           (directory-files \"~/org/projects/MAMA/inbox\" t
                         directory-files-no-dot-files-regexp) t)'.
FLAG determines output type."
  ;; convert single name to list
  (let* ((file-names-raw (if (listp file-name-or-names)
                             file-name-or-names (list file-name-or-names)))
         ;; filter for valid file names only
         (file-names (or (fpa--get-valid-files file-names-raw)
                         (error "Cannot continue. No files.")))
         (header (fpa--header-string flag))
         (line-strings (cl-loop for file in file-names
                                do (message (format "Working on %s" file))
                                append
                                (fpa--file-to-line-strings file flag))))
    (fpa--strings-to-buffer header line-strings save-to-file)))

;;;; utils

;;;;; get headers

(defun fpa--get-header (file-name)
  "Parse FILE-NAME and return list (file-name \"header: value\")
 for `header' fields."
  (let* ((tree (fpa--convert-xml-to-tree file-name))
         (fpa-list (fpa--convert-tree-to-fpalist tree 'detail))
         (line (fpa--convert-line-to-header-value
                (cl-loop for element in fpa-list
                         for id = (cadr element)
                         ;; consider only elements starting with 1-2,
                         ;; "cedente" fields
                         for element-prefix = (substring (symbol-name id) 0 3)
                         for header-flag = (string= element-prefix "1-2")
                         if header-flag collect element)))
         (formatted (cl-loop for element in line
                             for format-string = "    %s: %s" then "\n    %s: %s"
                             concat (format format-string
                                            (car element)
                                            (cadr element)))))
    (list (file-name-base file-name) formatted)))

(defun fpa--intersect-strings (strings)
  "Return intersection with wildcards of list STRINGS."
  (cl-assert (> (length strings) 1))
  (cl-loop for i from 0 to (seq-min (seq-map #'length strings))
           for a = (car strings)
           for char = (seq-elt a i)
           for check = (lambda (reduction checked-string)
                         (and reduction (eq char (seq-elt checked-string i))))
           if (seq-reduce check strings t) collect (seq-elt a i) into result
           else return (concat result "*")))

(defun fpa--dedup-headers (headers)
  "Dedup HEADERS (file-name header) returning list with wildcards."
  (let* ((aheaders nil)
         (aheaders (cl-loop for header in headers
                            for file-name = (car header)
                            for desc = (cadr header)
                            for val = (assoc desc aheaders)
                            if val do (push file-name (cadr val))
                            else do (push (list desc (list file-name)) aheaders)
                            finally return aheaders)))
    (cl-loop for aheader in aheaders
             for files = (cadr aheader)
             for files-n = (length files)
             for desc = (car aheader)
             if (eq files-n 1) collect (list (car files) desc) into result
             else collect (list (fpa--intersect-strings files) desc) into result
             finally return result)))

(defun fpa-headers (file-name-or-names)
  "Return headers of valid files in FILE-NAME-OR-NAMES."
  (let* ((file-names-list (if (listp file-name-or-names)
                              file-name-or-names (list file-name-or-names)))
         (file-names (or (fpa--get-valid-files file-names-list)
                         (error "Cannot continue. No files.")))
         (headers (cl-loop for file-name in file-names
                           collect (fpa--get-header file-name)))
         (dedup-headers (fpa--dedup-headers headers))
         ;; (dedup-headers headers)
         (result (cl-loop for header in dedup-headers
                          for format-string = "%s" then "\n\n%s"
                          for file-name = (car header)
                          for file-header = (cadr header)
                          for formatted = (format "  File: %s\n%s" file-name file-header)
                          concat (format format-string formatted)))
         (result-temp-buffer-name "*invoices headers*"))
    (save-excursion
      (with-output-to-temp-buffer result-temp-buffer-name
        (goto-char (point-min))
        (princ "Headers\n-------\n\n")
        (princ result)
        (pop-to-buffer result-temp-buffer-name)))))

;;;;; hydra

(require 'hydra)

(defhydra hydra-fpa (:color blue :hint nil)
  "
     fpa-reader commands
     -------------------
     _f_: convert file at point, or marked, from dired
     _a_: convert all files in current directory in dired
     _c_: count valid files among the dired marked
     _h_: get headers of file at point, or marked, from dired
     _s_: create summary of file at point, or marked
"
  ("f" (fpa-file-to-buffer (dired-get-marked-files) 'detail nil))
  ("a" (progn (dired-mark-subdir-files)
              (fpa-file-to-buffer (dired-get-marked-files) 'detail)))
  ("b" hydra-shortcuts/body "back")
  ("c" (fpa--count-valid-files (dired-get-marked-files)))
  ("h" (fpa-headers (dired-get-marked-files)))
  ("s" (fpa-file-to-buffer (dired-get-marked-files) 'summary nil))
  ("q" nil "quit"))

;;;; fpa-manager

;; (let ((file-name "~/org/projects/MAMA/archive/fatture/2. uncompressed/IT00967720285_6QBya.xml"))
;;   (fpa-file-to-buffer file-name 'database))

;; (fpa-file-to-buffer fpa-test-files 'database)
