;;; load data and define test files

(load-file "fpa.el")

(defvar fpa-test-files (directory-files "~/org/projects/fpa-reader/test" t
                                        directory-files-no-dot-files-regexp)
  "Test files")

(defconst fpa-test--one-invoice-one-line-file (car fpa-test-files)
  "Test file with one invoice and one line")

(defconst fpa-test--one-invoice-multi-line-file (cadr fpa-test-files)
  "Test file with one invoice and more than one line")

(defconst fpa-test--multi-invoice-multi-line-file (caddr fpa-test-files)
  "Test file with more than one invoice and more than one line")

(defconst fpa-test--multi-invoice-multi-line-multi-summary-file (cadddr fpa-test-files)
  "Test file with more than one invoice and more than one line")

;;; test-fpa--schema-get-key

(ert-deftest fpa--schema-get-key ()
  (let ((key '(1-2-1-1-2
               (FatturaElettronicaHeader
                IdFiscaleIVA IdCodice) t
               "detail-label" t "summary-label" nil "db-label")))
    (should (fpa--schema-get-key 'detail-flag key))
    (should (string= (fpa--schema-get-key 'detail-label key) "detail-label"))
    (should (fpa--schema-get-key 'summary-flag key))
    (should (string= (fpa--schema-get-key 'summary-label key) "summary-label"))
    (should (null (fpa--schema-get-key 'database-flag key)))
    (should (string= (fpa--schema-get-key 'database-label key) "db-label"))))

;;; test-fpa--schema-get-value-p

(ert-deftest fpa--schema-get-value-p ()
  (let ((key '(1-2-1-1-2
               (FatturaElettronicaHeader
                IdFiscaleIVA IdCodice) t
                "detail-label" t "summary-label" nil "db-label")))
    (should (fpa--schema-get-value-p key 'detail))
    (should (fpa--schema-get-value-p key 'summary))
    (should (not (fpa--schema-get-value-p key 'database)))))

;;; test-fpa--schema-get-label

(ert-deftest fpa--schema-get-label ()
  (let ((key '(1-2-1-1-2
               (FatturaElettronicaHeader
                IdFiscaleIVA IdCodice) t
                "detail-label" t "summary-label" nil "db-label")))
    (should (string= (fpa--schema-get-label key 'detail) "detail-label"))
    (should (string= (fpa--schema-get-label key 'summary) "summary-label"))
    (should (string= (fpa--schema-get-label key 'database) "db-label"))))

;;; test-fpa--convert-tree-to-fpalist

(ert-deftest fpa--convert-tree-to-fpalist ()
  (let ((schema-test-file "~/org/projects/fpa-reader/fpa-schema-test.el")
        (tree (fpa--convert-xml-to-tree fpa-test--one-invoice-multi-line-file))
        (tree-multi
         (fpa--convert-xml-to-tree fpa-test--multi-invoice-multi-line-file)))
    (should (equal
             (fpa--convert-tree-to-fpalist
              tree-multi 'detail schema-test-file)
             '(detail (("IVA detail" 1-2-1-1-2 "01234567890")
                       ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                       ("Data documento" 2-1-1-3 ("2017-01-18" "2017-01-20"))
                       ("Numero documento" 2-1-1-4 ("12" "456"))
                       ("ImportoTotaleDocumento" 2-1-1-9 (nil nil))
                       ("# linea" 2-2-1-1 (("1" "2") "1"))
                       ("Desc" 2-2-1-4 (("LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI" "FORNITURE VARIE PER UFFICIO") "PRESTAZIONE DEL SEGUENTE SERVIZIO PROFESSIONALE: LA DESCRIZIONE DELLA PRESTAZIONE PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI"))
                       ("Prezzo totale" 2-2-1-11 (("5.00" "20.00") "2000.00"))
                       ("Aliquota IVA" 2-2-1-12 (("22.00" "22.00") "22.00"))
                       ("Scadenza" 2-4-2-5 ("2017-02-18" "2017-02-20"))))))
    (should (equal (fpa--convert-tree-to-fpalist
                    tree 'database schema-test-file)
                   '(database (("seller-tax-id" 1-2-1-1-2 "01234567890")
                               ("seller-name" 1-2-1-3-1 "ALPHA SRL")
                               ("date" 2-1-1-3 "2017-01-18")
                               ("id" 2-1-1-4 "123")
                               ("payment-amount" 2-1-1-9 nil)
                               ("line-id" 2-2-1-1 ("1" "2"))
                               ("line-description" 2-2-1-4 ("LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI" "FORNITURE VARIE PER UFFICIO"))
                               ("payment-due-date" 2-4-2-5 "2017-03-30")
                               ))))
    (should (equal (fpa--convert-tree-to-fpalist
                    tree 'summary schema-test-file)
                   '(summary (("IVA summary" 1-2-1-1-2 "01234567890")
                              ("Data" 2-1-1-3 "2017-01-18")
                              ("Numero" 2-1-1-4 "123")
                              ("ImportoTotaleDocumento" 2-1-1-9 nil)
                              ("DataScadenzaPagamento" 2-4-2-5 "2017-03-30")))))
    (should (equal (fpa--convert-tree-to-fpalist tree 'detail schema-test-file)
                   '(detail (("IVA detail" 1-2-1-1-2 "01234567890")
                             ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                             ("Data documento" 2-1-1-3 "2017-01-18")
                             ("Numero documento" 2-1-1-4 "123")
                             ("ImportoTotaleDocumento" 2-1-1-9 nil)
                             ("# linea" 2-2-1-1 ("1" "2"))
                             ("Desc" 2-2-1-4 ("LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI" "FORNITURE VARIE PER UFFICIO"))
                             ("Prezzo totale" 2-2-1-11 ("5.00" "20.00"))
                             ("Aliquota IVA" 2-2-1-12 ("22.00" "22.00"))
                             ("Scadenza" 2-4-2-5 "2017-03-30")))))))

;;; test-fpa--count-multi

(ert-deftest fpa--count-multi ()
  (let* ((schema-test-file "~/org/projects/fpa-reader/fpa-schema-test.el")
         (flag 'detail)
         (single-invoice-single-line-fpa-list
          (fpa--convert-tree-to-fpalist
           (fpa--convert-xml-to-tree fpa-test--one-invoice-one-line-file)
           flag schema-test-file))
         (multi-invoices-fpa-list
          (fpa--convert-tree-to-fpalist
           (fpa--convert-xml-to-tree fpa-test--multi-invoice-multi-line-file)
           flag schema-test-file))
         (multi-lines-fpa-list
          (fpa--convert-tree-to-fpalist
           (fpa--convert-xml-to-tree fpa-test--one-invoice-multi-line-file)
           flag schema-test-file)))
    (should (eq (fpa--count-multi
                 'lines single-invoice-single-line-fpa-list schema-test-file) 1))
    (should (eq (fpa--count-multi
                 'invoices single-invoice-single-line-fpa-list schema-test-file) 1))
    (should (eq (fpa--count-multi
                 'invoices multi-invoices-fpa-list schema-test-file) 2))
    (should (eq (fpa--count-multi
                 'lines multi-lines-fpa-list schema-test-file) 2))))
        
;;; test-fpa--split-invoices-in-fpalist

(ert-deftest fpa--split-invoices-in-fpalist ()
  (let* ((schema-test-file "~/org/projects/fpa-reader/fpa-schema-test.el"))
    (should (equal (fpa--split-invoices-in-fpalist
                    (fpa--convert-tree-to-fpalist
                     (fpa--convert-xml-to-tree
                      fpa-test--one-invoice-one-line-file)
                     'detail schema-test-file))
                   ;; list of one invoice/one line
                   '((detail (("IVA detail" 1-2-1-1-2 "01234567890")
                              ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                              ("Data documento" 2-1-1-3 "2017-01-18")
                              ("Numero documento" 2-1-1-4 "123")
                              ("ImportoTotaleDocumento" 2-1-1-9 nil)
                              ("# linea" 2-2-1-1 "1")
                              ("Desc" 2-2-1-4 "DESCRIZIONE DELLA FORNITURA")
                              ("Prezzo totale" 2-2-1-11 "5.00")
                              ("Aliquota IVA" 2-2-1-12 "22.00")
                              ("Scadenza" 2-4-2-5 "2017-02-18"))))))
    (should (equal (fpa--split-invoices-in-fpalist
                    (fpa--convert-tree-to-fpalist
                     (fpa--convert-xml-to-tree
                      fpa-test--one-invoice-multi-line-file)
                     'detail schema-test-file))
                   ;; list of one invoice/multi line
                   '((detail (("IVA detail" 1-2-1-1-2 "01234567890")
                              ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                              ("Data documento" 2-1-1-3 "2017-01-18")
                              ("Numero documento" 2-1-1-4 "123")
                              ("ImportoTotaleDocumento" 2-1-1-9 nil)
                              ("# linea" 2-2-1-1 ("1" "2"))
                              ("Desc" 2-2-1-4 ("LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI" "FORNITURE VARIE PER UFFICIO"))
                              ("Prezzo totale" 2-2-1-11 ("5.00" "20.00"))
                              ("Aliquota IVA" 2-2-1-12 ("22.00" "22.00"))
                              ("Scadenza" 2-4-2-5 "2017-03-30"))))))
    (should (equal (fpa--split-invoices-in-fpalist
                    (fpa--convert-tree-to-fpalist
                     (fpa--convert-xml-to-tree
                      fpa-test--multi-invoice-multi-line-file)
                     'detail schema-test-file))
                   ;; list of two invoices
                   '((detail (("IVA detail" 1-2-1-1-2 "01234567890")
                              ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                              ("Data documento" 2-1-1-3 "2017-01-18")
                              ("Numero documento" 2-1-1-4 "12")
                              ("ImportoTotaleDocumento" 2-1-1-9 nil)
                              ("# linea" 2-2-1-1 ("1" "2"))
                              ("Desc" 2-2-1-4 ("LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI" "FORNITURE VARIE PER UFFICIO"))
                              ("Prezzo totale" 2-2-1-11 ("5.00" "20.00"))
                              ("Aliquota IVA" 2-2-1-12 ("22.00" "22.00"))
                              ("Scadenza" 2-4-2-5 "2017-02-18")))
                     (detail (("IVA detail" 1-2-1-1-2 "01234567890")
                              ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                              ("Data documento" 2-1-1-3 "2017-01-20")
                              ("Numero documento" 2-1-1-4 "456")
                              ("ImportoTotaleDocumento" 2-1-1-9 nil)
                              ("# linea" 2-2-1-1 "1")
                              ("Desc" 2-2-1-4 "PRESTAZIONE DEL SEGUENTE SERVIZIO PROFESSIONALE: LA DESCRIZIONE DELLA PRESTAZIONE PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI")
                              ("Prezzo totale" 2-2-1-11 "2000.00")
                              ("Aliquota IVA" 2-2-1-12 "22.00")
                              ("Scadenza" 2-4-2-5 "2017-02-20"))))))))

;;; test-fpa--split-fpalist-in-lines

(ert-deftest fpa--split-fpalist-in-lines ()
  (let ((schema-test-file "~/org/projects/fpa-reader/fpa-schema-test.el"))
    (let ((fpa-lists (fpa--split-invoices-in-fpalist
                              (fpa--convert-tree-to-fpalist
                               (fpa--convert-xml-to-tree
                                fpa-test--one-invoice-one-line-file)
                               'detail schema-test-file))))
    (should (equal (fpa--split-fpalist-in-lines fpa-lists schema-test-file)
                   '( ;; one invoice
                     ( ;; one line
                      (detail (("IVA detail" 1-2-1-1-2 "01234567890")
                               ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                               ("Data documento" 2-1-1-3 "2017-01-18")
                               ("Numero documento" 2-1-1-4 "123")
                               ("ImportoTotaleDocumento" 2-1-1-9 nil)
                               ("# linea" 2-2-1-1 "1")
                               ("Desc" 2-2-1-4 "DESCRIZIONE DELLA FORNITURA")
                               ("Prezzo totale" 2-2-1-11 "5.00")
                               ("Aliquota IVA" 2-2-1-12 "22.00")
                               ("Scadenza" 2-4-2-5 "2017-02-18"))))))))
    (let ((fpa-lists (fpa--split-invoices-in-fpalist
                              (fpa--convert-tree-to-fpalist
                               (fpa--convert-xml-to-tree
                                fpa-test--multi-invoice-multi-line-file)
                               'detail schema-test-file))))
    (should (equal (fpa--split-fpalist-in-lines fpa-lists schema-test-file)
                   '( ;; first invoice
                     ( ;; first line
                      (detail (("IVA detail" 1-2-1-1-2 "01234567890")
                               ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                               ("Data documento" 2-1-1-3 "2017-01-18")
                               ("Numero documento" 2-1-1-4 "12")
                               ("ImportoTotaleDocumento" 2-1-1-9 nil)
                               ("# linea" 2-2-1-1 "1")
                               ("Desc" 2-2-1-4 "LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI")
                               ("Prezzo totale" 2-2-1-11 "5.00")
                               ("Aliquota IVA" 2-2-1-12 "22.00")
                               ("Scadenza" 2-4-2-5 "2017-02-18")))
                      ;; second line
                      (detail (("IVA detail" 1-2-1-1-2 "01234567890")
                               ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                               ("Data documento" 2-1-1-3 "2017-01-18")
                               ("Numero documento" 2-1-1-4 "12")
                               ("ImportoTotaleDocumento" 2-1-1-9 nil)
                               ("# linea" 2-2-1-1 "2")
                               ("Desc" 2-2-1-4 "FORNITURE VARIE PER UFFICIO")
                               ("Prezzo totale" 2-2-1-11 "20.00")
                               ("Aliquota IVA" 2-2-1-12 "22.00")
                               ("Scadenza" 2-4-2-5 "2017-02-18"))))
                     ;; second invoice
                     ( ;; first and unique line
                      (detail (("IVA detail" 1-2-1-1-2 "01234567890")
                               ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                               ("Data documento" 2-1-1-3 "2017-01-20")
                               ("Numero documento" 2-1-1-4 "456")
                               ("ImportoTotaleDocumento" 2-1-1-9 nil)
                               ("# linea" 2-2-1-1 "1")
                               ("Desc" 2-2-1-4 "PRESTAZIONE DEL SEGUENTE SERVIZIO PROFESSIONALE: LA DESCRIZIONE DELLA PRESTAZIONE PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI")
                               ("Prezzo totale" 2-2-1-11 "2000.00")
                               ("Aliquota IVA" 2-2-1-12 "22.00")
                               ("Scadenza" 2-4-2-5 "2017-02-20"))))))))
    (let ((fpa-lists (fpa--split-invoices-in-fpalist
                              (fpa--convert-tree-to-fpalist
                               (fpa--convert-xml-to-tree
                                fpa-test--one-invoice-multi-line-file)
                               'detail schema-test-file))))
    (should (equal (fpa--split-fpalist-in-lines fpa-lists schema-test-file)
                   '( ;; one invoice
                     ( ;; first line
                      (detail (("IVA detail" 1-2-1-1-2 "01234567890")
                               ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                               ("Data documento" 2-1-1-3 "2017-01-18")
                               ("Numero documento" 2-1-1-4 "123")
                               ("ImportoTotaleDocumento" 2-1-1-9 nil)
                               ("# linea" 2-2-1-1 "1")
                               ("Desc" 2-2-1-4 "LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI")
                               ("Prezzo totale" 2-2-1-11 "5.00")
                               ("Aliquota IVA" 2-2-1-12 "22.00")
                               ("Scadenza" 2-4-2-5 "2017-03-30")))
                      ;; second line
                      (detail (("IVA detail" 1-2-1-1-2 "01234567890")
                               ("Cedente" 1-2-1-3-1 "ALPHA SRL")
                               ("Data documento" 2-1-1-3 "2017-01-18")
                               ("Numero documento" 2-1-1-4 "123")
                               ("ImportoTotaleDocumento" 2-1-1-9 nil)
                               ("# linea" 2-2-1-1 "2")
                               ("Desc" 2-2-1-4 "FORNITURE VARIE PER UFFICIO")
                               ("Prezzo totale" 2-2-1-11 "20.00")
                               ("Aliquota IVA" 2-2-1-12 "22.00")
                               ("Scadenza" 2-4-2-5 "2017-03-30"))))))))))

;;; test-fpa--file-to-invoices-lines

(ert-deftest fpa--file-to-invoices-lines ()
  (let* ((schema-test-file "~/org/projects/fpa-reader/fpa-schema-test.el"))
    (should (equal (fpa--file-to-invoices-lines
                    fpa-test--one-invoice-multi-line-file
                    'detail
                    schema-test-file)
                   '("IT01234567890_FPA02.xml"
                     ;; only invoice
                     ( ;; first line
                      (("IVA detail" "01234567890")
                       ("Cedente" "ALPHA SRL")
                       ("Data documento" "2017-01-18")
                       ("Numero documento" "123")
                       ("ImportoTotaleDocumento" "")
                       ("# linea" "1")
                       ("Desc" "LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI")
                       ("Prezzo totale" "5.00")
                       ("Aliquota IVA" "22.00")
                       ("Scadenza" "2017-03-30"))
                      ;; second line
                      (("IVA detail" "01234567890")
                       ("Cedente" "ALPHA SRL")
                       ("Data documento" "2017-01-18")
                       ("Numero documento" "123")
                       ("ImportoTotaleDocumento" "")
                       ("# linea" "2")
                       ("Desc" "FORNITURE VARIE PER UFFICIO")
                       ("Prezzo totale" "20.00")
                       ("Aliquota IVA" "22.00")
                       ("Scadenza" "2017-03-30"))))))
    (should (equal (fpa--file-to-invoices-lines
                    fpa-test--multi-invoice-multi-line-file
                    'detail
                    schema-test-file)
                   '("IT01234567890_FPA03.xml"
                     ;; first invoice
                     ( ;; first line
                      (("IVA detail" "01234567890")
                       ("Cedente" "ALPHA SRL")
                       ("Data documento" "2017-01-18")
                       ("Numero documento" "12")
                       ("ImportoTotaleDocumento" "")
                       ("# linea" "1")
                       ("Desc" "LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI")
                       ("Prezzo totale" "5.00")
                       ("Aliquota IVA" "22.00")
                       ("Scadenza" "2017-02-18"))
                      ;; second line
                      (("IVA detail" "01234567890")
                       ("Cedente" "ALPHA SRL")
                       ("Data documento" "2017-01-18")
                       ("Numero documento" "12")
                       ("ImportoTotaleDocumento" "")
                       ("# linea" "2")
                       ("Desc" "FORNITURE VARIE PER UFFICIO")
                       ("Prezzo totale" "20.00")
                       ("Aliquota IVA" "22.00")
                       ("Scadenza" "2017-02-18")))
                     ;; second invoice
                     ( ;; first line
                      (("IVA detail" "01234567890")
                       ("Cedente" "ALPHA SRL")
                       ("Data documento" "2017-01-20")
                       ("Numero documento" "456")
                       ("ImportoTotaleDocumento" "")
                       ("# linea" "1")
                       ("Desc" "PRESTAZIONE DEL SEGUENTE SERVIZIO PROFESSIONALE: LA DESCRIZIONE DELLA PRESTAZIONE PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI")
                       ("Prezzo totale" "2000.00")
                       ("Aliquota IVA" "22.00")
                       ("Scadenza" "2017-02-20"))))))))

;;; test-fpa--file-to-line-strings

(ert-deftest fpa--file-to-line-strings ()
  (let* ((schema-test-file "~/org/projects/fpa-reader/fpa-schema-test.el"))
    (should (equal (fpa--file-to-line-strings
                    fpa-test--multi-invoice-multi-line-file
                    'detail
                    schema-test-file)
                   '("IT01234567890_FPA03.xml;01234567890;ALPHA SRL;2017-01-18;12;;1;LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI;5.00;22.00;2017-02-18"
                     "IT01234567890_FPA03.xml;01234567890;ALPHA SRL;2017-01-18;12;;2;FORNITURE VARIE PER UFFICIO;20.00;22.00;2017-02-18"
                     "IT01234567890_FPA03.xml;01234567890;ALPHA SRL;2017-01-20;456;;1;PRESTAZIONE DEL SEGUENTE SERVIZIO PROFESSIONALE: LA DESCRIZIONE DELLA PRESTAZIONE PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI;2000.00;22.00;2017-02-20")))
    (should (equal (fpa--file-to-line-strings
                    fpa-test--one-invoice-one-line-file
                    'detail
                    schema-test-file)
                   '("IT01234567890_FPA01.xml;01234567890;ALPHA SRL;2017-01-18;123;;1;DESCRIZIONE DELLA FORNITURA;5.00;22.00;2017-02-18")))
    (should (equal (fpa--file-to-line-strings
                    fpa-test--one-invoice-multi-line-file
                    'detail
                    schema-test-file)
                   '("IT01234567890_FPA02.xml;01234567890;ALPHA SRL;2017-01-18;123;;1;LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI;5.00;22.00;2017-03-30"
                     "IT01234567890_FPA02.xml;01234567890;ALPHA SRL;2017-01-18;123;;2;FORNITURE VARIE PER UFFICIO;20.00;22.00;2017-03-30")))))

(ert-deftest fpa--header-string ()
  (should (string= (fpa--header-string 'detail)
                   "file-name;IVA cedente;Cedente;Cessionario;Data documento;Numero documento;ImportoTotaleDocumento;Numero Linea;Descrizione Linea;Prezzo totale;Aliquota IVA;Scadenza"))
  (should (string= (fpa--header-string 'summary)
                   "file-name;IdCodice;Nome;Data;Numero;ImportoTotaleDocumento;line-id;DataScadenzaPagamento"))
  (should (string= (fpa--header-string 'database)
                   "file-name;seller-tax-id;seller-name;buyer-tax-id;buyer-name;date;id;payment-amount;line-id;line-description;unit-taxable-price;unit-tax-rate;summary-amount-taxable;summary-amount-tax;reference;payment-due-date")))

;;; test invoice database

(ert-deftest fpa--files-to-invoices-db ()
  (fpa--invoices-db-reset)
  (should (equal 
           (fpa--files-to-invoices-db fpa-test-files)
           '(#s(invoice "456" "2014-12-20" "/home/erpreciso/org/projects/fpa-reader/test/IT01234567890_FPR03.xml" nil "01234567890" "SOCIETA' ALPHA SRL" nil "BETA GAMMA" (#s(invoice-line "1" "PRESTAZIONE DEL SEGUENTE SERVIZIO PROFESSIONALE: LA DESCRIZIONE DELLA PRESTAZIONE PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI" "2000.00" "22.00" nil nil nil)) (#s(invoice-summary "2000.00" "440.00" nil))) #s(invoice "123" "2014-12-18" "/home/erpreciso/org/projects/fpa-reader/test/IT01234567890_FPR03.xml" nil "01234567890" "SOCIETA' ALPHA SRL" nil "BETA GAMMA" (#s(invoice-line "1" "LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI" "5.00" "22.00" nil nil nil) #s(invoice-line "2" "FORNITURE VARIE PER UFFICIO" "20.00" "22.00" nil nil nil)) (#s(invoice-summary "27.00" "5.95" nil))) #s(invoice "456" "2017-01-20" "/home/erpreciso/org/projects/fpa-reader/test/IT01234567890_FPA04.xml" nil "01234567890" "ALPHA SRL" nil "AMMINISTRAZIONE BETA" (#s(invoice-line "1" "PRESTAZIONE DEL SEGUENTE SERVIZIO PROFESSIONALE" "2000.00" "22.00" nil nil nil)) (#s(invoice-summary "2000.00" "440.00" nil) #s(invoice-summary "270.00" "8.00" "Non soggetta art. 1/54-89 L. 190/2014") #s(invoice-summary "26.00" "6.00" "Escl. art. 15 DPR 633/72"))) #s(invoice "12" "2017-01-18" "/home/erpreciso/org/projects/fpa-reader/test/IT01234567890_FPA04.xml" nil "01234567890" "ALPHA SRL" nil "AMMINISTRAZIONE BETA" (#s(invoice-line "1" "DESCRIZIONE DELLA FORNITURA" "5.00" "22.00" nil nil nil) #s(invoice-line "2" "FORNITURE VARIE PER UFFICIO" "20.00" "22.00" nil nil nil)) (#s(invoice-summary "25.00" "5.50" nil) #s(invoice-summary "262.08" "0.00" "Non soggetta art. 1/54-89 L. 190/2014") #s(invoice-summary "30.00" "0.00" "Escl. art. 15 DPR 633/72"))) #s(invoice "123" "2017-01-18" "/home/erpreciso/org/projects/fpa-reader/test/IT01234567890_FPA02.xml" nil "01234567890" "ALPHA SRL" nil "AMMINISTRAZIONE BETA" (#s(invoice-line "1" "LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI" "5.00" "22.00" nil nil nil) #s(invoice-line "2" "FORNITURE VARIE PER UFFICIO" "20.00" "22.00" nil nil nil)) (#s(invoice-summary "25.00" "5.50" nil))))))
  (fpa--invoices-db-reset)
  (should (equal 
           (fpa--files-to-invoices-db fpa-test--one-invoice-multi-line-file)
           '(#s(invoice "123" "2017-01-18" "/home/erpreciso/org/projects/fpa-reader/test/IT01234567890_FPA02.xml" nil "01234567890" "ALPHA SRL" nil "AMMINISTRAZIONE BETA" (#s(invoice-line "1" "LA DESCRIZIONE DELLA FORNITURA PUO' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E' STATO PORTATO A MILLE CARATTERI" "5.00" "22.00" nil nil nil) #s(invoice-line "2" "FORNITURE VARIE PER UFFICIO" "20.00" "22.00" nil nil nil)) (#s(invoice-summary "25.00" "5.50" nil)))))))
           

;;; test get and count valid files

(ert-deftest fpa--get-valid-files ()
  (should (equal
           (seq-map #'file-name-base
                    (fpa--get-valid-files fpa-test--multi-invoice-multi-line-file))
           '("IT01234567890_FPA03")))
  (should (equal
           (seq-map #'file-name-base (fpa--get-valid-files fpa-test-files))
           '("IT01234567890_FPA01" "IT01234567890_FPA02" "IT01234567890_FPA03"
             "IT01234567890_FPA04" "IT01234567890_FPR01" "IT01234567890_FPR02"
             "IT01234567890_FPR03"))))

(ert-deftest fpa--count-valid-files ()
  (should (string= (fpa--count-valid-files fpa-test-files) "Valid files: 7")))
