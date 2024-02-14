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

(defvar fpa-schema-file "~/org/projects/fpa-reader/fpa-schema2.el"
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

(defun fpa--parseparse
(let ((schema (fpa--get-schema)))

  

;; (defun fpa--get-schema-lines ()
;;   "Return list of lines from schema file."
;;   (let* ((tree nil)
;;          (lines nil))
;;     (with-temp-buffer
;;       (insert-file-contents fpa-schema-file)
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         (push (buffer-substring
;;                (line-beginning-position) (line-end-position))
;;               lines)
;;         (forward-line 1))
;;       (reverse lines))))
      

;; (defun parse (lines tree level)
;;   (while lines
;;     (let ((line (pop lines))
;;           (prefix (substring line 0 5))
;;           (level-prefix (make-string level ? )))
;;       (if (string-match (concat "^" level-prefix) line)
          

;; (defun parse-tree (remaining-lines siblings parent level)
;;   (let* ((line (pop remaining-lines))
;;          (prefix (substring line 0 5))
;;          (level-prefix (make-string level ? )))
;;     (if (string-match (concat "^" level-prefix) line)
;;         (progn (push line siblings)
;;                (parse-tree
      
;;       (cl-loop for lev upfrom (- level 1) downto 0
;;                for prefix = (make-string lev ? )
;;                if (string-match (concat "^" prefix) line)
;;                do (push siblings parent)
                           


  (let ((lines (fpa--get-schema-lines))
        (level 0)
        (tree nil))
    
  
    ;; (cl-loop for line in lines
             ;; for 
             ;; if (string= prefix "     "))))
