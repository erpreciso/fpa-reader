("root" export single "FatturaElettronica"
 (("1" export single "FatturaElettronicaHeader"
   (("1.1" do-not-export single "DatiTrasmissione"
     (("1.1.1" do-not-export single "IdTrasmittente"
       (("1.1.1.1" do-not-export single "IdPaese" nil)
        ("1.1.1.2" do-not-export single "IdCodice" nil)))
      ("1.1.2" do-not-export single "ProgressivoInvio" nil)
      ("1.1.3" do-not-export single "FormatoTrasmissione" nil)
      ("1.1.4" do-not-export single "CodiceDestinatario" nil)
      ("1.1.5" do-not-export single "ContattiTrasmittente"
       (("1.1.5.1" do-not-export single "Telefono" nil)
        ("1.1.5.2" do-not-export single "Email" nil)))
      ("1.1.6" do-not-export single "PECDestinatario" nil)))
    ("1.2" export single "CedentePrestatore"
     (("1.2.1" export single "DatiAnagrafici"
       (("1.2.1.1" export single "IdFiscaleIVA"
         (("1.2.1.1.1" export single "IdPaese" nil)
          ("1.2.1.1.2" export single "IdCodice" nil)))
        ("1.2.1.2" export single "CodiceFiscale" nil)
        ("1.2.1.3" export single "Anagrafica"
         (("1.2.1.3.1" export single "Denominazione" nil)
          ("1.2.1.3.2" export single "Nome" nil)
          ("1.2.1.3.3" export single "Cognome" nil)
          ("1.2.1.3.4" do-not-export single "Titolo" nil)
          ("1.2.1.3.5" do-not-export single "CodEORI" nil)))
        ("1.2.1.4" do-not-export single "AlboProfessionale" nil)
        ("1.2.1.5" do-not-export single "ProvinciaAlbo" nil)
        ("1.2.1.6" do-not-export single "NumeroIscrizioneAlbo" nil)
        ("1.2.1.7" do-not-export single "DataIscrizioneAlbo" nil)
        ("1.2.1.8" export single "RegimeFiscale" nil)))
      ("1.2.2" export single "Sede"
       (("1.2.2.1" do-not-export single "Indirizzo" nil)
        ("1.2.2.2" do-not-export single "NumeroCivico" nil)
        ("1.2.2.3" do-not-export single "CAP" nil)
        ("1.2.2.4" export single "Comune" nil)
        ("1.2.2.5" export single "Provincia" nil)
        ("1.2.2.6" export single "Nazione" nil)))
      ("1.2.3" do-not-export single "StabileOrganizzazione"
       (("1.2.3.1" do-not-export single "Indirizzo" nil)
        ("1.2.3.2" do-not-export single "NumeroCivico" nil)
        ("1.2.3.3" do-not-export single "CAP" nil)
        ("1.2.3.4" do-not-export single "Comune" nil)
        ("1.2.3.5" do-not-export single "Provincia" nil)
        ("1.2.3.6" do-not-export single "Nazione" nil)))
      ("1.2.4" do-not-export single "IscrizioneREA"
       (("1.2.4.1" do-not-export single "Ufficio" nil)
        ("1.2.4.2" do-not-export single "NumeroREA" nil)
        ("1.2.4.3" do-not-export single "CapitaleSociale" nil)
        ("1.2.4.4" do-not-export single "SocioUnico" nil)
        ("1.2.4.5" do-not-export single "StatoLiquidazione" nil)))
      ("1.2.5" do-not-export single "Contatti"
       (("1.2.5.1" do-not-export single "Telefono" nil)
        ("1.2.5.2" do-not-export single "Fax" nil)
        ("1.2.5.3" do-not-export single "Email" nil)))
      ("1.2.6" do-not-export single "RiferimentoAmministrazione" nil)))
    ("1.3" do-not-export single "RappresentanteFiscale"
     (("1.3.1" do-not-export single "DatiAnagrafici"
       (("1.3.1.1" do-not-export single "IdFiscaleIVA"
         (("1.3.1.1.1" do-not-export single "IdPaese" nil)
          ("1.3.1.1.2" do-not-export single "IdCodice" nil)))
        ("1.3.1.2" do-not-export single "CodiceFiscale" nil)
        ("1.3.1.3" do-not-export single "Anagrafica"
         (("1.3.1.3.1" do-not-export single "Denominazione" nil)
          ("1.3.1.3.2" do-not-export single "Nome" nil)
          ("1.3.1.3.3" do-not-export single "Cognome" nil)
          ("1.3.1.3.4" do-not-export single "Titolo" nil)
          ("1.3.1.3.5" do-not-export single "CodEORI" nil)))))))
    ("1.4" export single "CessionarioCommittente"
     (("1.4.1" export single "DatiAnagrafici"
       (("1.4.1.1" export single "IdFiscaleIVA"
         (("1.4.1.1.1" do-not-export single "IdPaese" nil)
          ("1.4.1.1.2" export single "IdCodice" nil)))
        ("1.4.1.2" export single "CodiceFiscale" nil)
        ("1.4.1.3" export single "Anagrafica"
         (("1.4.1.3.1" export single "Denominazione" nil)
          ("1.4.1.3.2" export single "Nome" nil)
          ("1.4.1.3.3" export single "Cognome" nil)
          ("1.4.1.3.4" do-not-export single "Titolo" nil)
          ("1.4.1.3.5" do-not-export single "CodEORI" nil)))))
      ("1.4.2" export single "Sede"
       (("1.4.2.1" do-not-export single "Indirizzo" nil)
        ("1.4.2.2" do-not-export single "NumeroCivico" nil)
        ("1.4.2.3" do-not-export single "CAP" nil)
        ("1.4.2.4" export single "Comune" nil)
        ("1.4.2.5" export single "Provincia" nil)
        ("1.4.2.6" export single "Nazione" nil)))
      ("1.4.3" do-not-export single "StabileOrganizzazione"
       (("1.4.3.1" do-not-export single "Indirizzo" nil)
        ("1.4.3.2" do-not-export single "NumeroCivico" nil)
        ("1.4.3.3" do-not-export single "CAP" nil)
        ("1.4.3.4" do-not-export single "Comune" nil)
        ("1.4.3.5" do-not-export single "Provincia" nil)
        ("1.4.3.6" do-not-export single "Nazione" nil)))
      ("1.4.4" do-not-export single "RappresentanteFiscale"
       (("1.4.4.1" do-not-export single "IdFiscaleIVA"
         (("1.4.4.1.1" do-not-export single "IdPaese" nil)
          ("1.4.4.1.2" do-not-export single "IdCodice" nil)))
        ("1.4.4.2" do-not-export single "Denominazione" nil)
        ("1.4.4.3" do-not-export single "Nome" nil)
        ("1.4.4.4" do-not-export single "Cognome" nil)))))
    ("1.5" do-not-export single "TerzoIntermediarioOSoggettoEmittente"
     (("1.5.1" do-not-export single "DatiAnagrafici"
       (("1.5.1.1" do-not-export single "IdFiscaleIVA"
         (("1.5.1.1.1" do-not-export single "IdPaese" nil)
          ("1.5.1.1.2" do-not-export single "IdCodice" nil)))
        ("1.5.1.2" do-not-export single "CodiceFiscale" nil)
        ("1.5.1.3" do-not-export single "Anagrafica"
         (("1.5.1.3.1" do-not-export single "Denominazione" nil)
          ("1.5.1.3.2" do-not-export single "Nome" nil)
          ("1.5.1.3.3" do-not-export single "Cognome" nil)
          ("1.5.1.3.4" do-not-export single "Titolo" nil)
          ("1.5.1.3.5" do-not-export single "CodEORI" nil)))))))
    ("1.6" do-not-export single "SoggettoEmittente" nil)))
  ("2" export multi "FatturaElettronicaBody"
   (("2.1" export single "DatiGenerali"
     (("2.1.1" export single "DatiGeneraliDocumento"
       (("2.1.1.1" export single "TipoDocumento" nil)
        ("2.1.1.2" export single "Divisa" nil)
        ("2.1.1.3" export single "Data" nil)
        ("2.1.1.4" export single "Numero" nil)
        ("2.1.1.5" do-not-export single "DatiRitenuta"
         (("2.1.1.5.1" do-not-export single "TipoRitenuta" nil)
          ("2.1.1.5.2" do-not-export single "ImportoRitenuta" nil)
          ("2.1.1.5.3" do-not-export single "AliquotaRitenuta" nil)
          ("2.1.1.5.4" do-not-export single "CausalePagamento" nil)))
        ("2.1.1.6" do-not-export single "DatiBollo"
         (("2.1.1.6.1" do-not-export single "BolloVirtuale" nil)
          ("2.1.1.6.2" do-not-export single "ImportoBollo" nil)))
        ("2.1.1.7" do-not-export single "DatiCassaPrevidenziale"
         (("2.1.1.7.1" do-not-export single "TipoCassa" nil)
          ("2.1.1.7.2" do-not-export single "AlCassa" nil)
          ("2.1.1.7.3" do-not-export single "ImportoContributoCassa" nil)
          ("2.1.1.7.4" do-not-export single "ImponibileCassa" nil)
          ("2.1.1.7.5" do-not-export single "AliquotaIVA" nil)
          ("2.1.1.7.6" do-not-export single "Ritenuta" nil)
          ("2.1.1.7.7" do-not-export single "Natura" nil)
          ("2.1.1.7.8" do-not-export single "RiferimentoAmministrazione" nil)))
        ("2.1.1.8" do-not-export single "ScontoMaggiorazione"
         (("2.1.1.8.1" do-not-export single "Tipo" nil)
          ("2.1.1.8.2" do-not-export single "Percentuale" nil)
          ("2.1.1.8.3" do-not-export single "Importo" nil)))
        ("2.1.1.9" export single "ImportoTotaleDocumento" nil)
        ("2.1.1.10" export single "Arrotondamento" nil)
        ("2.1.1.11" export single "Causale" nil)
        ("2.1.1.12" do-not-export single "Art73" nil)))
      ("2.1.2" do-not-export single "DatiOrdineAcquisto"
       (("2.1.2.1" do-not-export single "RiferimentoNumeroLinea" nil)
        ("2.1.2.2" do-not-export single "IdDocumento" nil)
        ("2.1.2.3" do-not-export single "Data" nil)
        ("2.1.2.4" do-not-export single "NumItem" nil)
        ("2.1.2.5" do-not-export single "CodiceCommessaConvenzione" nil)
        ("2.1.2.6" do-not-export single "CodiceCUP" nil)
        ("2.1.2.7" do-not-export single "CodiceCIG" nil)))
      ("2.1.3" do-not-export single "DatiContratto" nil)
      ("2.1.4" do-not-export single "DatiConvenzione" nil)
      ("2.1.5" do-not-export single "DatiRicezione" nil)
      ("2.1.6" do-not-export single "DatiFattureCollegate" nil)
      ("2.1.7" do-not-export single "DatiSAL"
       (("2.1.7.1" do-not-export single "RiferimentoFase" nil)))
      ("2.1.8" do-not-export single "DatiDDT"
       (("2.1.8.1" do-not-export single "NumeroDDT" nil)
        ("2.1.8.2" do-not-export single "DataDDT" nil)
        ("2.1.8.3" do-not-export single "RiferimentoNumeroLinea" nil)))
      ("2.1.9" do-not-export single "DatiTrasporto"
       (("2.1.9.1" do-not-export single "DatiAnagraficiVettore"
         (("2.1.9.1.1" do-not-export single "IdFiscaleIVA"
           (("2.1.9.1.1.1" do-not-export single "IdPaese" nil)
            ("2.1.9.1.1.2" do-not-export single "IdCodice" nil)))
          ("2.1.9.1.2" do-not-export single "CodiceFiscale" nil)
          ("2.1.9.1.3" do-not-export single "Anagrafica"
           (("2.1.9.1.3.1" do-not-export single "Denominazione" nil)
            ("2.1.9.1.3.2" do-not-export single "Nome" nil)
            ("2.1.9.1.3.3" do-not-export single "Cognome" nil)
            ("2.1.9.1.3.4" do-not-export single "Titolo" nil)
            ("2.1.9.1.3.5" do-not-export single "CodEORI" nil)))
          ("2.1.9.1.4" do-not-export single "NumeroLicenzaGuida" nil)))
        ("2.1.9.2" do-not-export single "MezzoTrasporto" nil)
        ("2.1.9.3" do-not-export single "CausaleTrasporto" nil)
        ("2.1.9.4" do-not-export single "NumeroColli" nil)
        ("2.1.9.5" do-not-export single "Descrizione" nil)
        ("2.1.9.6" do-not-export single "UnitaMisuraPeso" nil)
        ("2.1.9.7" do-not-export single "PesoLordo" nil)
        ("2.1.9.8" do-not-export single "PesoNetto" nil)
        ("2.1.9.9" do-not-export single "DataOraRitiro" nil)
        ("2.1.9.10" do-not-export single "DataInizioTrasporto" nil)
        ("2.1.9.11" do-not-export single "TipoResa" nil)
        ("2.1.9.12" do-not-export single "IndirizzoResa"
         (("2.1.9.12.1" do-not-export single "Indirizzo" nil)
          ("2.1.9.12.2" do-not-export single "NumeroCivico" nil)
          ("2.1.9.12.3" do-not-export single "CAP" nil)
          ("2.1.9.12.4" do-not-export single "Comune" nil)
          ("2.1.9.12.5" do-not-export single "Provincia" nil)
          ("2.1.9.12.6" do-not-export single "Nazione" nil)))
        ("2.1.9.13" do-not-export single "DataOraConsegna" nil)))
      ("2.1.10" do-not-export single "FatturaPrincipale"
       (("2.1.10.1" do-not-export single "NumeroFatturaPrincipale" nil)
        ("2.1.10.2" do-not-export single "DataFatturaPrincipale" nil)))))
    ("2.2" export single "DatiBeniServizi"
     (("2.2.1" export multi "DettaglioLinee"
       (("2.2.1.1" export single "NumeroLinea" nil)
        ("2.2.1.2" do-not-export single "TipoCessionePrestazione" nil)
        ("2.2.1.3" export single "CodiceArticolo"
         (("2.2.1.3.1" export single "CodiceTipo" nil)
          ("2.2.1.3.2" export single "CodiceValore" nil)))
        ("2.2.1.4" export single "Descrizione" nil)
        ("2.2.1.5" export single "Quantita" nil)
        ("2.2.1.6" export single "UnitaMisura" nil)
        ("2.2.1.7" do-not-export single "DataInizioPeriodo" nil)
        ("2.2.1.8" do-not-export single "DataFinePeriodo" nil)
        ("2.2.1.9" export single "PrezzoUnitario" nil)
        ("2.2.1.10" do-not-export single "ScontoMaggiorazione"
         (("2.2.1.10.1" do-not-export single "Tipo" nil)
          ("2.2.1.10.2" do-not-export single "Percentuale" nil)
          ("2.2.1.10.3" do-not-export single "Importo" nil)))
        ("2.2.1.11" export single "PrezzoTotale" nil)
        ("2.2.1.12" export single "AliquotaIVA" nil)
        ("2.2.1.13" do-not-export single "Ritenuta" nil)
        ("2.2.1.14" do-not-export single "Natura" nil)
        ("2.2.1.15" do-not-export single "RiferimentoAmministrazione" nil)
        ("2.2.1.16" do-not-export single "AltriDatiGestionali"
         (("2.2.1.16.1" do-not-export single "TipoDato" nil)
          ("2.2.1.16.2" do-not-export single "RiferimentoTesto" nil)
          ("2.2.1.16.3" do-not-export single "RiferimentoNumero" nil)
          ("2.2.1.16.4" do-not-export single "RiferimentoData" nil)))))
      ("2.2.2" do-not-export single "DatiRiepilogo"
       (("2.2.2.1" do-not-export single "AliquotaIVA" nil)
        ("2.2.2.2" do-not-export single "Natura" nil)
        ("2.2.2.3" do-not-export single "SpeseAccessorie" nil)
        ("2.2.2.4" do-not-export single "Arrotondamento" nil)
        ("2.2.2.5" do-not-export single "ImponibileImporto" nil)
        ("2.2.2.6" do-not-export single "Imposta" nil)
        ("2.2.2.7" do-not-export single "EsigibilitaIVA" nil)
        ("2.2.2.8" do-not-export single "RiferimentoNormativo" nil)))))
    ("2.3" do-not-export single "DatiVeicoli"
     (("2.3.1" do-not-export single "Data" nil)
      ("2.3.2" do-not-export single "TotalePercorso" nil)))
    ("2.4" export single "DatiPagamento"
     (("2.4.1" export single "CondizioniPagamento" nil)
      ("2.4.2" export single "DettaglioPagamento"
       (("2.4.2.1" export single "Beneficiario" nil)
        ("2.4.2.2" export single "ModalitaPagamento" nil)
        ("2.4.2.3" export single "DataRiferimentoTerminiPagamento" nil)
        ("2.4.2.4" export single "GiorniTerminiPagamento" nil)
        ("2.4.2.5" export single "DataScadenzaPagamento" nil)
        ("2.4.2.6" export single "ImportoPagamento" nil)
        ("2.4.2.7" do-not-export single "CodUfficioPostale" nil)
        ("2.4.2.8" do-not-export single "CognomeQuietanzante" nil)
        ("2.4.2.9" do-not-export single "NomeQuietanzante" nil)
        ("2.4.2.10" do-not-export single "CFQuietanzante" nil)
        ("2.4.2.11" do-not-export single "TitoloQuietanzante" nil)
        ("2.4.2.12" do-not-export single "IstitutoFinanziario" nil)
        ("2.4.2.13" do-not-export single "IBAN" nil)
        ("2.4.2.14" do-not-export single "ABI" nil)
        ("2.4.2.15" do-not-export single "CAB" nil)
        ("2.4.2.16" do-not-export single "BIC" nil)
        ("2.4.2.17" do-not-export single "ScontoPagamentoAnticipato" nil)
        ("2.4.2.18" do-not-export single "DataLimitePagamentoAnticipato" nil)
        ("2.4.2.19" do-not-export single "PenalitaPagamentiRitardati" nil)
        ("2.4.2.20" do-not-export single "DataDecorrenzaPenale" nil)
        ("2.4.2.21" do-not-export single "CodicePagamento" nil)))))
    ("2.5" do-not-export single "Allegati"
     (("2.5.1" do-not-export single "NomeAttachment" nil)
      ("2.5.2" do-not-export single "AlgoritmoCompressione" nil)
      ("2.5.3" do-not-export single "FormatoAttachment" nil)
      ("2.5.4" do-not-export single "DescrizioneAttachment" nil)
      ("2.5.5" do-not-export single "Attachment" nil)))))))
