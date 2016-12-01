#install.packages("XLConnect")

library(XLConnect)

#############################################################################
# (1) LADEN DER INPUTDATEN
# file.choose() öffnet einen Editor
srcFile <- file.choose()
#############################################################################

#############################################################################
# (2) ERSTELLEN DER DATAFRAMES und umbenennen von Variablen
kreditor = readWorksheetFromFile(srcFile,sheet="Kreditor", header=T)
änderungsHist = readWorksheetFromFile(srcFile,sheet="Änderungshistorie", header=T)
bestellung = readWorksheetFromFile(srcFile,sheet="Bestellung", header=T)
bestellPos = readWorksheetFromFile(srcFile,sheet="Bestellposition", header=T)
warenEingang = readWorksheetFromFile(srcFile,sheet="Wareneingang", header=T)
rechnung = readWorksheetFromFile(srcFile,sheet="Rechnung", header=T)
zahlung = readWorksheetFromFile(srcFile,sheet="Zahlung", header=T)
# Soweit funktioniert mal das Laden
# Umbenennen von Spalten
names(kreditor)[names(kreditor) == 'ErstellTS'] <- 'ErstellTS_Kreditor'
names(bestellung)[names(bestellung) == 'ErstellTS'] <- 'ErstellTS_Bestellung'
names(bestellPos)[names(bestellPos) == 'PosNr'] <- 'PosNr_Bestellpos'
names(bestellPos)[names(bestellPos) == 'ErstellTS'] <- 'ErstellTS_Bestellpos'
names(warenEingang)[names(warenEingang) == 'EingangsTS'] <- 'EingangsTS_WAE'
names(zahlung)[names(zahlung) == 'ZahlTS'] <- 'ZahlTS_Zahlung'
#######################################################################

# Zeilen mit NA in Joinattributen löschen
no_BN_WAE<-which(is.na(warenEingang$BestellNr))
warenEingang<- warenEingang[-no_BN_WAE,]
# Funktioniert an der Stelle. NA in Wareneingang gelöscht.

no_BN_RE<-which(is.na(rechnung$BestellNr))
rechnung<-rechnung[-no_BN_RE,]
# Funktioniert an der Stelle. NA in Rechnung gelöscht.

# Keine NA in BestellNr. 
# zweite Zeile negiert und löscht alle Einträge!! 
# no_BN_BE<-which(is.na(bestellung$BestellNr))
# bestellung<-bestellung[-no_BN_BE,]

# Selber Fehler hier !
# no_BN_BPos<-which(is.na(bestellPos$BestellNr))
# bestellPos<-bestellPos[-no_BN_BPos,]

# Again !
# no_KR_Kredit<-which(is.na(kreditor$KredNr))
# kreditor<-kreditor[-no_KR_Kredit,]

# And Again
# no_KR_zahlung<-which(is.na(zahlung$KredNr))
# zahlung<-zahlung[-no_KR_zahlung,]

######################################################################
# (4) Joinen der Tabellen

#Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
#Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
#Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
#Cross join: merge(x = df1, y = df2, by = NULL)


# join Bestellung mit BestellPos
merge_bb<-merge(x=bestellung, y=bestellPos, by= "BestellNr")

# join mit Wareneingang
merge_bbw<-merge(x=merge_bb, y=warenEingang, by="BestellNr")

#join mit Kreditor
merge_bbwk<-merge(x=merge_bbw, y=kreditor, by.x="KredNr.x", by.y="KredNr")

# Rechnung
merge_bbwkr<-merge(x=merge_bbwk, y=rechnung, by.x="BestellNr", by.y="BestellNr")

# Zahlung
merge_bbwkrz<-merge(x=merge_bbwkr, y=zahlung, by.x="KredNr.x", by.y="KredNr")

# Änderungshistorie
# Tabelle Kreditor
kredit<-which(änderungsHist$Tabelle == "Kreditor")
änderungsHist_Kredit <- änderungsHist[kredit,]
merge_bbwkrza_k<-merge(x=merge_bbwkrz, y=änderungsHist_Kredit, by.x="KredNr.x", by.y="ID", all=TRUE)
# Tabelle Bestellung
bestell<-which(änderungsHist$Tabelle == "Bestellung")
änderungsHist_bestell <- änderungsHist[bestell,]
merge_bbwkrza_kb<-merge(x=merge_bbwkrza_k, y=änderungsHist_bestell, by.x="BestellNr", by.y="ID", all=TRUE)
# Tabelle Bestellposition
# Hilfsspalte mit geänderter ID
x<-substr(änderungsHist$ID,2,7)
änderungsHist$IDTrim=x
bestellP<-which(änderungsHist$Tabelle == "Bestellposition")
änderungsHist_bestellP <- änderungsHist[bestellP,]
merge_bbwkrza_kbb<-merge(x=merge_bbwkrza_kb, y=änderungsHist_bestellP, by.x="BestellNr", by.y="IDTrim", all=TRUE)


# Festlegen der benötigten Spalten
spalten<-c("PosNr_Bestellpos","BestellNr","ErstellTS_Bestellpos","ErstellTS_Bestellung","ErstellTS_Kreditor","EingansDat",
           "RechnungsDatum","EingangsTS_WAE","ZahlTS_Zahlung","AenderTS","Feld","Wert_neu","Tabelle")

# Relevante Ergebnisse als Hilfstabelle als Input für den a-Algorithmus
hilfstabelle <- merge_bbwkrza_kbb[,spalten]


bestellNr <- unique(hilfstabelle$BestellNr)
bestellNr
