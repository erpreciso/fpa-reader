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
  (cadr (fpa--get-schema)))

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

(defun fpa--element-to-plist (schema-element tree prefix level)
  "Convert fpa xml ELEMENT to plist `key value'."
  ;; (cl-assert (= (length schema-element) 3))
  (let* ((schema-el-name     (cadr schema-element))
         (schema-el-children (caddr schema-element))
         (new-prefix (if (> level 2)
                         (format "%s|%s" prefix schema-el-name)
                       schema-el-name)))
    (pcase schema-el-children
      ((app type-of 'cons)
       ;; recursion for element children
       (cl-loop for schema-child in schema-el-children
                for key = (intern (cadr schema-child))
                for subtrees = (xml-get-children tree key)
                for new-level = (+ 1 level)
                collect
                (if subtrees
                    (cl-loop for subtree in subtrees
                             collect
                             (fpa--element-to-plist schema-child
                                                    subtree new-prefix
                                                    new-level))
                  (fpa--element-to-plist schema-child nil new-prefix new-level))))
      ('nil
       ;; return element text
       (list new-prefix (caddr tree))))))

(let ((schema (fpa--get-schema))
      (tree (fpa--xml-to-tree fpa-test-file)))
  (fpa--element-to-plist schema tree "root" 0))
