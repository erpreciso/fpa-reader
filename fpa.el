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
  "Return schema from file `fpa-schema-file'."
  (with-temp-buffer
    (insert-file-contents fpa-schema-file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun fpa--get-schema-root ()
  "Return root label of `fpa-schema-file'."
  (cadddr (fpa--get-schema)))

(defun fpa--get-level (id)
  "Return int level give ID format as `1.2.3'."
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
  "Return all possible keys for the root header."
  (seq-map (lambda (p)
             (let ((rt (fpa--get-schema-root)))
               (if p (intern (concat (symbol-name p) ":" rt))
                 (intern rt)))) fpa--root-header-prefixes))
  
(defun fpa--xml-to-tree (file-name)
  "XML-parse FILE-NAME and return top node tree."
  (let ((parsed-xml-region (xml-parse-file file-name))
        (tree))
    (cl-loop for key in (fpa--root-keys)
             for tree = (assq key parsed-xml-region)
             if tree return tree)))

(defun fpa--tree-to-list (schema tree prefix)
  "Convert fpa xml TREE to nested list. Algo walk in parallel nested
   schema and nested tree."
  (cl-assert (= (length schema) 5))
  (let* ((schema-name         (nth 3 schema))
         (schema-children     (nth 4 schema))
         (schema-id           (nth 0 schema))
         (schema-level   (fpa--get-level schema-id))
         (new-prefix (if (> schema-level 2) (format "%s|%s" prefix schema-name) schema-name)))
    (cond ((not schema-children)
           (list schema-id new-prefix (or (nth 2 tree) "empty")))
          (schema-children
           (cl-loop for schema-child in schema-children
                    for child-name = (intern (nth 3 schema-child))
                    for child-id = (nth 0 schema-child)
                    for child-trees = (xml-get-children tree child-name)
                    collect
                    (list child-id child-name
                          (if child-trees
                              (cl-loop for child-tree in child-trees
                                       collect
                                       (fpa--tree-to-list schema-child child-tree new-prefix))
                            (fpa--tree-to-list schema-child nil new-prefix))))))))

(defun fpa--invoice-file-to-list (file-name)
  "Convert invoice at FILE-NAME to nested list."
  (let ((schema (fpa--get-schema))
        (tree (fpa--xml-to-tree file-name)))
    (fpa--tree-to-list schema tree "root")))

(defun fpa--split-list-by-invoices (fpa-list)
  "Return list of fpa-lists, each list one invoice in original FPA-LIST."
  (let* ((header (car fpa-list))
         (invoices (caddr (cadr fpa-list))))
    (cl-loop for invoice in invoices
             collect (list header invoice))))

(defun fpa--get-lines-count (fpa-list)
  "Return number of lines in FPA-LIST."
  (let* ((body (cadr fpa-list))
         (data1 (cadr body))
         (data2 (caddr data1))
         (lines (caddr (car (car data2)))))
    (length lines)))

(car (fpa--split-list-by-invoices (fpa--invoice-file-to-list fpa-test-file)))


(let ((out nil) (out2 nil)
      (el (car (fpa--split-list-by-invoices (fpa--invoice-file-to-list fpa-test-file)))))
  (defun fpa--flatten (element multi-flag ix)
    (pcase (fpa--element-type element)
      ('leaf (push (list (car element) (caddr element) ix) (if multi-flag out2 out)))
      ('leaves (seq-map (lambda (e) (fpa--flatten e multi-flag ix)) element))
      ('parent (fpa--flatten (cadr element) multi-flag ix))
      ('parents (seq-map (lambda (e) (fpa--flatten (cadr e) multi-flag ix)) element))
      ('list-of-parents (seq-map-indexed (lambda (e idx) (fpa--flatten e t (+ 4 ix idx))) element))))
  (fpa--flatten el nil 0)
  (list (reverse out) (reverse out2)))

(defun fpa--element-type (element)
  "Return type of ELEMENT."
  (cond ((not (listp element)) (error "invalid element"))
        ((stringp (car element))                          'leaf)
        ((and (listp element) (symbolp (car element)))    'parent)
        ((and (listp element)
              (not (seq-remove
                    (lambda (e) (eq (fpa--element-type e) 'parent))
                    element))) 'parents)
        ((and (listp element)
              (not (seq-remove
                    (lambda (e) (eq (fpa--element-type e) 'leaf))
                    element))) 'leaves)
        ((and (listp element) (not (seq-remove
                    (lambda (e) (eq (fpa--element-type e) 'parents))
                    element)) 'list-of-parents))))

(ert-deftest fpa--test-element-type ()
  (should (equal (fpa--element-type '("Id" export "IT")) 'leaf))
  (should (equal (fpa--element-type
                  '(("Causale" export "causale1")
                    ("Causale" export "causale2"))) 'leaves))
  (should (equal (fpa--element-type '((Id1 ("Id1" export "IT"))
                                      (Id2 ("Id2" export "IT")))) 'parents))
  (should (equal (fpa--element-type '(IdPaese ("Id" export "IT"))) 'parent))
  (should (equal (fpa--element-type '(((Dati ((Fat ((Num ("Nu" do "e"))
                                                    (Dat ("Da" do "e")))))))
                                      ((Dati ((Fat ((Num ("Nu" do "e"))
                                                    (Dat ("Da" do "e")))))))))
                 'list-of-parents)))

(fpa--fattura-to-plist fpa-test-file)


