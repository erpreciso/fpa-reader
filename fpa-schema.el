("root" export "FatturaElettronica"
 (("1" export "FatturaElettronicaHeader"
   (("1.1" do-not-export "DatiTrasmissione"
     (("1.1.1" do-not-export "IdTrasmittente"
       (("1.1.1.1" do-not-export "IdPaese" nil)
        ("1.1.1.2" do-not-export "IdCodice" nil)))
      ("1.1.2" do-not-export "ProgressivoInvio" nil)
      ("1.1.3" do-not-export "FormatoTrasmissione" nil)
      ("1.1.4" do-not-export "CodiceDestinatario" nil)
      ("1.1.5" do-not-export "ContattiTrasmittente"
       (("1.1.5.1" do-not-export "Telefono" nil)
        ("1.1.5.2" do-not-export "Email" nil)))
      ("1.1.6" do-not-export "PECDestinatario" nil)))
    ("1.2" export "CedentePrestatore"
     (("1.2.1" export "DatiAnagrafici"
       (("1.2.1.1" export "IdFiscaleIVA"
         (("1.2.1.1.1" export "IdPaese" nil)
          ("1.2.1.1.2" export "IdCodice" nil)))
        ("1.2.1.2" export "CodiceFiscale" nil)
        ("1.2.1.3" export "Anagrafica"
         (("1.2.1.3.1" export "Denominazione" nil)
          ("1.2.1.3.2" export "Nome" nil)
          ("1.2.1.3.3" export "Cognome" nil)
          ("1.2.1.3.4" do-not-export "Titolo" nil)
          ("1.2.1.3.5" do-not-export "CodEORI" nil)))
        ("1.2.1.4" do-not-export "AlboProfessionale" nil)
        ("1.2.1.5" do-not-export "ProvinciaAlbo" nil)
        ("1.2.1.6" do-not-export "NumeroIscrizioneAlbo" nil)
        ("1.2.1.7" do-not-export "DataIscrizioneAlbo" nil)
        ("1.2.1.8" export "RegimeFiscale" nil)))
      ("1.2.2" export "Sede"
       (("1.2.2.1" do-not-export "Indirizzo" nil)
        ("1.2.2.2" do-not-export "NumeroCivico" nil)
        ("1.2.2.3" do-not-export "CAP" nil)
        ("1.2.2.4" export "Comune" nil)
        ("1.2.2.5" export "Provincia" nil)
        ("1.2.2.6" export "Nazione" nil)))
      ("1.2.3" do-not-export "StabileOrganizzazione"
       (("1.2.3.1" do-not-export "Indirizzo" nil)
        ("1.2.3.2" do-not-export "NumeroCivico" nil)
        ("1.2.3.3" do-not-export "CAP" nil)
        ("1.2.3.4" do-not-export "Comune" nil)
        ("1.2.3.5" do-not-export "Provincia" nil)
        ("1.2.3.6" do-not-export "Nazione" nil)))
      ("1.2.4" do-not-export "IscrizioneREA"
       (("1.2.4.1" do-not-export "Ufficio" nil)
        ("1.2.4.2" do-not-export "NumeroREA" nil)
        ("1.2.4.3" do-not-export "CapitaleSociale" nil)
        ("1.2.4.4" do-not-export "SocioUnico" nil)
        ("1.2.4.5" do-not-export "StatoLiquidazione" nil)))
      ("1.2.5" do-not-export "Contatti"
       (("1.2.5.1" do-not-export "Telefono" nil)
        ("1.2.5.2" do-not-export "Fax" nil)
        ("1.2.5.3" do-not-export "Email" nil)))
      ("1.2.6" do-not-export "RiferimentoAmministrazione" nil)))
    ("1.3" do-not-export "RappresentanteFiscale"
     (("1.3.1" do-not-export "DatiAnagrafici"
       (("1.3.1.1" do-not-export "IdFiscaleIVA"
         (("1.3.1.1.1" do-not-export "IdPaese" nil)
          ("1.3.1.1.2" do-not-export "IdCodice" nil)))
        ("1.3.1.2" do-not-export "CodiceFiscale" nil)
        ("1.3.1.3" do-not-export "Anagrafica"
         (("1.3.1.3.1" do-not-export "Denominazione" nil)
          ("1.3.1.3.2" do-not-export "Nome" nil)
          ("1.3.1.3.3" do-not-export "Cognome" nil)
          ("1.3.1.3.4" do-not-export "Titolo" nil)
          ("1.3.1.3.5" do-not-export "CodEORI" nil)))))))
    ("1.4" export "CessionarioCommittente"
     (("1.4.1" export "DatiAnagrafici"
       (("1.4.1.1" export "IdFiscaleIVA"
         (("1.4.1.1.1" do-not-export "IdPaese" nil)
          ("1.4.1.1.2" export "IdCodice" nil)))
        ("1.4.1.2" export "CodiceFiscale" nil)
        ("1.4.1.3" export "Anagrafica"
         (("1.4.1.3.1" export "Denominazione" nil)
          ("1.4.1.3.2" export "Nome" nil)
          ("1.4.1.3.3" export "Cognome" nil)
          ("1.4.1.3.4" do-not-export "Titolo" nil)
          ("1.4.1.3.5" do-not-export "CodEORI" nil)))))
      ("1.4.2" export "Sede"
       (("1.4.2.1" do-not-export "Indirizzo" nil)
        ("1.4.2.2" do-not-export "NumeroCivico" nil)
        ("1.4.2.3" do-not-export "CAP" nil)
        ("1.4.2.4" export "Comune" nil)
        ("1.4.2.5" export "Provincia" nil)
        ("1.4.2.6" export "Nazione" nil)))
      ("1.4.3" do-not-export "StabileOrganizzazione"
       (("1.4.3.1" do-not-export "Indirizzo" nil)
        ("1.4.3.2" do-not-export "NumeroCivico" nil)
        ("1.4.3.3" do-not-export "CAP" nil)
        ("1.4.3.4" do-not-export "Comune" nil)
        ("1.4.3.5" do-not-export "Provincia" nil)
        ("1.4.3.6" do-not-export "Nazione" nil)))
      ("1.4.4" do-not-export "RappresentanteFiscale"
       (("1.4.4.1" do-not-export "IdFiscaleIVA"
         (("1.4.4.1.1" do-not-export "IdPaese" nil)
          ("1.4.4.1.2" do-not-export "IdCodice" nil)))
        ("1.4.4.2" do-not-export "Denominazione" nil)
        ("1.4.4.3" do-not-export "Nome" nil)
        ("1.4.4.4" do-not-export "Cognome" nil)))))
    ("1.5" do-not-export "TerzoIntermediarioOSoggettoEmittente"
     (("1.5.1" do-not-export "DatiAnagrafici"
       (("1.5.1.1" do-not-export "IdFiscaleIVA"
         (("1.5.1.1.1" do-not-export "IdPaese" nil)
          ("1.5.1.1.2" do-not-export "IdCodice" nil)))
        ("1.5.1.2" do-not-export "CodiceFiscale" nil)
        ("1.5.1.3" do-not-export "Anagrafica"
         (("1.5.1.3.1" do-not-export "Denominazione" nil)
          ("1.5.1.3.2" do-not-export "Nome" nil)
          ("1.5.1.3.3" do-not-export "Cognome" nil)
          ("1.5.1.3.4" do-not-export "Titolo" nil)
          ("1.5.1.3.5" do-not-export "CodEORI" nil)))))))
    ("1.6" do-not-export "SoggettoEmittente" nil)))
  ("2" export "FatturaElettronicaBody"
   (("2.1" export "DatiGenerali"
     (("2.1.1" export "DatiGeneraliDocumento"
       (("2.1.1.1" export "TipoDocumento" nil)
        ("2.1.1.2" export "Divisa" nil)
        ("2.1.1.3" export "Data" nil)
        ("2.1.1.4" export "Numero" nil)
        ("2.1.1.5" do-not-export "DatiRitenuta"
         (("2.1.1.5.1" do-not-export "TipoRitenuta" nil)
          ("2.1.1.5.2" do-not-export "ImportoRitenuta" nil)
          ("2.1.1.5.3" do-not-export "AliquotaRitenuta" nil)
          ("2.1.1.5.4" do-not-export "CausalePagamento" nil)))
        ("2.1.1.6" do-not-export "DatiBollo"
         (("2.1.1.6.1" do-not-export "BolloVirtuale" nil)
          ("2.1.1.6.2" do-not-export "ImportoBollo" nil)))
        ("2.1.1.7" do-not-export "DatiCassaPrevidenziale"
         (("2.1.1.7.1" do-not-export "TipoCassa" nil)
          ("2.1.1.7.2" do-not-export "AlCassa" nil)
          ("2.1.1.7.3" do-not-export "ImportoContributoCassa" nil)
          ("2.1.1.7.4" do-not-export "ImponibileCassa" nil)
          ("2.1.1.7.5" do-not-export "AliquotaIVA" nil)
          ("2.1.1.7.6" do-not-export "Ritenuta" nil)
          ("2.1.1.7.7" do-not-export "Natura" nil)
          ("2.1.1.7.8" do-not-export "RiferimentoAmministrazione" nil)))
        ("2.1.1.8" do-not-export "ScontoMaggiorazione"
         (("2.1.1.8.1" do-not-export "Tipo" nil)
          ("2.1.1.8.2" do-not-export "Percentuale" nil)
          ("2.1.1.8.3" do-not-export "Importo" nil)))
        ("2.1.1.9" export "ImportoTotaleDocumento" nil)
        ("2.1.1.10" export "Arrotondamento" nil)
        ("2.1.1.11" export "Causale" nil)
        ("2.1.1.12" do-not-export "Art73" nil)))
      ("2.1.2" do-not-export "DatiOrdineAcquisto"
       (("2.1.2.1" do-not-export "RiferimentoNumeroLinea" nil)
        ("2.1.2.2" do-not-export "IdDocumento" nil)
        ("2.1.2.3" do-not-export "Data" nil)
        ("2.1.2.4" do-not-export "NumItem" nil)
        ("2.1.2.5" do-not-export "CodiceCommessaConvenzione" nil)
        ("2.1.2.6" do-not-export "CodiceCUP" nil)
        ("2.1.2.7" do-not-export "CodiceCIG" nil)))
      ("2.1.3" do-not-export "DatiContratto" nil)
      ("2.1.4" do-not-export "DatiConvenzione" nil)
      ("2.1.5" do-not-export "DatiRicezione" nil)
      ("2.1.6" do-not-export "DatiFattureCollegate" nil)
      ("2.1.7" do-not-export "DatiSAL"
       (("2.1.7.1" do-not-export "RiferimentoFase" nil)))
      ("2.1.8" do-not-export "DatiDDT"
       (("2.1.8.1" do-not-export "NumeroDDT" nil)
        ("2.1.8.2" do-not-export "DataDDT" nil)
        ("2.1.8.3" do-not-export "RiferimentoNumeroLinea" nil)))
      ("2.1.9" do-not-export "DatiTrasporto"
       (("2.1.9.1" do-not-export "DatiAnagraficiVettore"
         (("2.1.9.1.1" do-not-export "IdFiscaleIVA"
           (("2.1.9.1.1.1" do-not-export "IdPaese" nil)
            ("2.1.9.1.1.2" do-not-export "IdCodice" nil)))
          ("2.1.9.1.2" do-not-export "CodiceFiscale" nil)
          ("2.1.9.1.3" do-not-export "Anagrafica"
           (("2.1.9.1.3.1" do-not-export "Denominazione" nil)
            ("2.1.9.1.3.2" do-not-export "Nome" nil)
            ("2.1.9.1.3.3" do-not-export "Cognome" nil)
            ("2.1.9.1.3.4" do-not-export "Titolo" nil)
            ("2.1.9.1.3.5" do-not-export "CodEORI" nil)))
          ("2.1.9.1.4" do-not-export "NumeroLicenzaGuida" nil)))
        ("2.1.9.2" do-not-export "MezzoTrasporto" nil)
        ("2.1.9.3" do-not-export "CausaleTrasporto" nil)
        ("2.1.9.4" do-not-export "NumeroColli" nil)
        ("2.1.9.5" do-not-export "Descrizione" nil)
        ("2.1.9.6" do-not-export "UnitaMisuraPeso" nil)
        ("2.1.9.7" do-not-export "PesoLordo" nil)
        ("2.1.9.8" do-not-export "PesoNetto" nil)
        ("2.1.9.9" do-not-export "DataOraRitiro" nil)
        ("2.1.9.10" do-not-export "DataInizioTrasporto" nil)
        ("2.1.9.11" do-not-export "TipoResa" nil)
        ("2.1.9.12" do-not-export "IndirizzoResa"
         (("2.1.9.12.1" do-not-export "Indirizzo" nil)
          ("2.1.9.12.2" do-not-export "NumeroCivico" nil)
          ("2.1.9.12.3" do-not-export "CAP" nil)
          ("2.1.9.12.4" do-not-export "Comune" nil)
          ("2.1.9.12.5" do-not-export "Provincia" nil)
          ("2.1.9.12.6" do-not-export "Nazione" nil)))
        ("2.1.9.13" do-not-export "DataOraConsegna" nil)))
      ("2.1.10" do-not-export "FatturaPrincipale"
       (("2.1.10.1" do-not-export "NumeroFatturaPrincipale" nil)
        ("2.1.10.2" do-not-export "DataFatturaPrincipale" nil)))))
    ("2.2" export "DatiBeniServizi"
     (("2.2.1" export "DettaglioLinee"
       (("2.2.1.1" export "NumeroLinea" nil)
        ("2.2.1.2" do-not-export "TipoCessionePrestazione" nil)
        ("2.2.1.3" export "CodiceArticolo"
         (("2.2.1.3.1" export "CodiceTipo" nil)
          ("2.2.1.3.2" export "CodiceValore" nil)))
        ("2.2.1.4" export "Descrizione" nil)
        ("2.2.1.5" export "Quantita" nil)
        ("2.2.1.6" export "UnitaMisura" nil)
        ("2.2.1.7" do-not-export "DataInizioPeriodo" nil)
        ("2.2.1.8" do-not-export "DataFinePeriodo" nil)
        ("2.2.1.9" export "PrezzoUnitario" nil)
        ("2.2.1.10" do-not-export "ScontoMaggiorazione"
         (("2.2.1.10.1" do-not-export "Tipo" nil)
          ("2.2.1.10.2" do-not-export "Percentuale" nil)
          ("2.2.1.10.3" do-not-export "Importo" nil)))
        ("2.2.1.11" export "PrezzoTotale" nil)
        ("2.2.1.12" export "AliquotaIVA" nil)
        ("2.2.1.13" do-not-export "Ritenuta" nil)
        ("2.2.1.14" do-not-export "Natura" nil)
        ("2.2.1.15" do-not-export "RiferimentoAmministrazione" nil)
        ("2.2.1.16" do-not-export "AltriDatiGestionali"
         (("2.2.1.16.1" do-not-export "TipoDato" nil)
          ("2.2.1.16.2" do-not-export "RiferimentoTesto" nil)
          ("2.2.1.16.3" do-not-export "RiferimentoNumero" nil)
          ("2.2.1.16.4" do-not-export "RiferimentoData" nil)))))
      ("2.2.2" do-not-export "DatiRiepilogo"
       (("2.2.2.1" do-not-export "AliquotaIVA" nil)
        ("2.2.2.2" do-not-export "Natura" nil)
        ("2.2.2.3" do-not-export "SpeseAccessorie" nil)
        ("2.2.2.4" do-not-export "Arrotondamento" nil)
        ("2.2.2.5" do-not-export "ImponibileImporto" nil)
        ("2.2.2.6" do-not-export "Imposta" nil)
        ("2.2.2.7" do-not-export "EsigibilitaIVA" nil)
        ("2.2.2.8" do-not-export "RiferimentoNormativo" nil)))))
    ("2.3" do-not-export "DatiVeicoli"
     (("2.3.1" do-not-export "Data" nil)
      ("2.3.2" do-not-export "TotalePercorso" nil)))
    ("2.4" export "DatiPagamento"
     (("2.4.1" export "CondizioniPagamento" nil)
      ("2.4.2" export "DettaglioPagamento"
       (("2.4.2.1" export "Beneficiario" nil)
        ("2.4.2.2" export "ModalitaPagamento" nil)
        ("2.4.2.3" export "DataRiferimentoTerminiPagamento" nil)
        ("2.4.2.4" export "GiorniTerminiPagamento" nil)
        ("2.4.2.5" export "DataScadenzaPagamento" nil)
        ("2.4.2.6" export "ImportoPagamento" nil)
        ("2.4.2.7" do-not-export "CodUfficioPostale" nil)
        ("2.4.2.8" do-not-export "CognomeQuietanzante" nil)
        ("2.4.2.9" do-not-export "NomeQuietanzante" nil)
        ("2.4.2.10" do-not-export "CFQuietanzante" nil)
        ("2.4.2.11" do-not-export "TitoloQuietanzante" nil)
        ("2.4.2.12" do-not-export "IstitutoFinanziario" nil)
        ("2.4.2.13" do-not-export "IBAN" nil)
        ("2.4.2.14" do-not-export "ABI" nil)
        ("2.4.2.15" do-not-export "CAB" nil)
        ("2.4.2.16" do-not-export "BIC" nil)
        ("2.4.2.17" do-not-export "ScontoPagamentoAnticipato" nil)
        ("2.4.2.18" do-not-export "DataLimitePagamentoAnticipato" nil)
        ("2.4.2.19" do-not-export "PenalitaPagamentiRitardati" nil)
        ("2.4.2.20" do-not-export "DataDecorrenzaPenale" nil)
        ("2.4.2.21" do-not-export "CodicePagamento" nil)))))
    ("2.5" do-not-export "Allegati"
     (("2.5.1" do-not-export "NomeAttachment" nil)
      ("2.5.2" do-not-export "AlgoritmoCompressione" nil)
      ("2.5.3" do-not-export "FormatoAttachment" nil)
      ("2.5.4" do-not-export "DescrizioneAttachment" nil)
      ("2.5.5" do-not-export "Attachment" nil)))))))
