#install.packages("XLConnect")

library(XLConnect)

#############################################################################
# LADEN DER INPUTDATEN
# file.choose() öffnet einen Editor
srcFile <- file.choose()
#############################################################################

################################################################################
# ERSTELLEN DER DATAFRAMES und umbenennen von Variablen
kreditor = readWorksheetFromFile(srcFile,sheet="Kreditor", header=T)
änderungsHist = readWorksheetFromFile(srcFile,sheet="Änderungshistorie", header=T)
bestellung = readWorksheetFromFile(srcFile,sheet="Bestellung", header=T)
bestellPos = readWorksheetFromFile(srcFile,sheet="Bestellposition", header=T)
warenEingang = readWorksheetFromFile(srcFile,sheet="Wareneingang", header=T)
rechnung = readWorksheetFromFile(srcFile,sheet="Rechnung", header=T)
zahlung = readWorksheetFromFile(srcFile,sheet="Zahlung", header=T)
# Soweit funktioniert einmal das Laden der Daten
# Umbenennen von Spaltennamen um Namenskonflikte in der Hilfstabelle zu umgehen
names(kreditor)[names(kreditor) == 'ErstellTS'] <- 'ErstellTS_Kreditor'
names(bestellung)[names(bestellung) == 'ErstellTS'] <- 'ErstellTS_Bestellung'
names(bestellPos)[names(bestellPos) == 'PosNr'] <- 'PosNr_Bestellpos'
names(bestellPos)[names(bestellPos) == 'ErstellTS'] <- 'ErstellTS_Bestellpos'
names(warenEingang)[names(warenEingang) == 'EingangsTS'] <- 'EingangsTS_WAE'
names(zahlung)[names(zahlung) == 'ZahlTS'] <- 'ZahlTS_Zahlung'
################################################################################

################################################################################
# DATENMANIPULATIONEN
# Zeilen mit NA in Joinattributen löschen

if ('True' %in% is.na(warenEingang$BestellNr)) {
  no_BN_WAE<-which(is.na(warenEingang$BestellNr))
  warenEingang<- warenEingang[-no_BN_WAE,]
  # Funktioniert an der Stelle. NA in Wareneingang gelöscht.
}

if ('True' %in% is.na(rechnung$BestellNr)) {
  no_BN_RE<-which(is.na(rechnung$BestellNr))
  rechnung<-rechnung[-no_BN_RE,]
  # Funktioniert an der Stelle. NA in Rechnung gelöscht.
}
?is.element

if ('True' %in% is.na(bestellung$BestellNr)) {
  no_BN_BE<-which(is.na(bestellung$BestellNr))
  bestellung<-bestellung[-no_BN_BE,]
  # Keine NA in BestellNr. 
  # Zweite Zeile negiert und löscht alle Einträge !!
  # Daher sind die if Abfragen notwendig um Generizität für verschiedene Input-
  # daten zu gewährleisten.
}

if ('True' %in% is.na(bestellPos$BestellNr)) {
  no_BN_BPos<-which(is.na(bestellPos$BestellNr))
  bestellPos<-bestellPos[-no_BN_BPos,]
}

if ('True' %in% is.na(kreditor$KredNr)) {
  no_KR_Kredit<-which(is.na(kreditor$KredNr))
  kreditor<-kreditor[-no_KR_Kredit,]
}

if ('True' %in% is.na(zahlung$KredNr)) {
  no_KR_zahlung<-which(is.na(zahlung$KredNr))
  zahlung<-zahlung[-no_KR_zahlung,]
}
################################################################################

################################################################################
# verknüpfen der Tabellen
# Äquivalenzen zu SQL Joins in R
# Inner Join wie Outer Join ohne , all = True
# Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
# Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
# Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
# Cross join: merge(x = df1, y = df2, by = NULL)

# Left Join Bestellung mit BestellPos
merge_bb<-merge(x=bestellung, y=bestellPos, by= "BestellNr", all.x = TRUE)

# Left Join mit Wareneingang
merge_bbw<-merge(x=merge_bb, y=warenEingang, by="BestellNr", all.x = TRUE)

# Left Join mit Rechnung
merge_bbwr<-merge(x=merge_bbw, y=rechnung, by="BestellNr", all.x = TRUE)

# Left Join mit Zahlung
merge_bbwrz<-merge(x=merge_bbwr, y=zahlung, by.x="KredNr.x", by.y="KredNr", all.x = TRUE)

# Left Join mit Kreditor
merge_bbwrzk<-merge(x=merge_bbwrz, y=kreditor, by.x="KredNr.x", by.y="KredNr", all.x = TRUE)

# Änderungshistorie
# Tabelle Kreditor
kredit<-which(änderungsHist$Tabelle == "Kreditor")
änderungsHist_Kredit <- änderungsHist[kredit,]
merge_bbwrzk_ak<-merge(x=merge_bbwrzk, y=änderungsHist_Kredit, by.x="KredNr.x", by.y="ID", all.x = TRUE)

# Tabelle Bestellung
bestell<-which(änderungsHist$Tabelle == "Bestellung")
änderungsHist_bestell <- änderungsHist[bestell,]
merge_bbwrzk_akb<-merge(x=merge_bbwrzk_ak, y=änderungsHist_bestell, by.x="BestellNr", by.y="ID", all.x = TRUE)

# Tabelle Bestellposition
# Hilfsspalte mit geänderter ID - erste Nummer weggeschnitten
x<-substr(änderungsHist$ID,2,7)
änderungsHist$IDTrim=x
bestellP<-which(änderungsHist$Tabelle == "Bestellposition")
änderungsHist_bestellP <- änderungsHist[bestellP,]
merge_bbwrzk_akbb<-merge(x=merge_bbwrzk_akb, y=änderungsHist_bestellP, by.x="BestellNr", by.y="IDTrim", all.x = TRUE)
################################################################################

################################################################################
# Festlegen der benötigten Spalten für die Hilfstabelle
spalten<-c("PosNr_Bestellpos","BestellNr","ErstellTS_Bestellpos","ErstellTS_Bestellung","ErstellTS_Kreditor","EingansDat",
           "RechnungsDatum","EingangsTS_WAE","ZahlTS_Zahlung","AenderTS","Feld","Wert_neu","Tabelle")

# Relevante Ergebnisse als Hilfstabelle. Input für den a-Algorithmus
hilfstabelle <- merge_bbwrzk_akbb[,spalten]
################################################################################

################################################################################
# PERSISTIEREN DER HILFSTABELLE
# workbook anscheinend für createSheet benötigt.

# Implementierung laut Doku
wb <- loadWorkbook(srcFile, create = T)
createSheet(wb,"Hilfstabelle") # Name für das Arbeitsblatt
setColumnWidth(wb,"Hilfstabelle",column = c(1:13),5000)
saveWorkbook(wb)
writeWorksheetToFile(srcFile, hilfstabelle, sheet = "Hilfstabelle") 
################################################################################

################################################################################
# NEXT STEPS HERE

bestellNr <- unique(hilfstabelle$BestellNr)
bestellNr

View(hilfstabelle)
unique(hilfstabelle$Tabelle)

table(hilfstabelle$BestellNr)
View(hilfstabelle[which(hilfstabelle$BestellNr==600003),])
str(hilfstabelle)


#Eventlog initialisieren
eventlog<-data.frame(CaseID= 0, Timestamp= Sys.time(), Aktivitaet= "Test", Kurz="t", stringsAsFactors = F)
str(eventlog)

#Erstellen des Eventlogs für alle BestellNummern

for(i in bestellNr){
  tab<- hilfstabelle[which(hilfstabelle$BestellNr==i),]
  
  bestPosTS<- unique(tab$ErstellTS_Bestellpos)
 # bestPosTS<- bestPosTS[-is.na(bestPosTS)]
  
  bestellTS<- unique(tab$ErstellTS_Bestellung)
#  bestellTS<- bestellTS[-is.na(bestellTS)]
  
  kreditorTS<- unique(tab$ErstellTS_Kreditor)
 # kreditorTS<- kreditorTS[-is.na(kreditorTS)]
  
  einRechnTS<- unique(tab$EingansDat)
#  einRechnTS<- einRechnTS[-is.na(einRechnTS)]
  
  rechnungsTS<- unique(tab$RechnungsDatum)
 # rechnungsTS<- rechnungsTS[-is.na(rechnungsTS)]
  
  warenEinTS<- unique(tab$EingangsTS_WAE)
#  warenEinTS<- warenEinTS[-is.na(warenEinTS)]
  
  zahlTS<- unique(tab$ZahlTS_Zahlung)
 # zahlTS<- zahlTS[-is.na(zahlTS)]
  
  
  bestPosL<-rep("Bestellposition erstellt", length(bestPosTS))
  bestPosK<-rep("b", length(bestPosTS))
  
  bestellL<-rep("Bestellung erstellt", length(bestellTS))
  bestellK<-rep("d", length(bestellTS))
  
  kreditorL<-rep("Kreditor erstellt", length(kreditorTS))
  kreditorK<-rep("f", length(kreditorTS))
  
  einRechnL<-rep("Rechnung eingegangen", length(einRechnTS))
  einRechnK<-rep("i", length(einRechnTS))
  
  rechnungL<-rep("Rechnung gestellt", length(rechnungsTS))
  rechnungK<-rep("j", length(rechnungsTS))
  
  wareEinL<-rep("Ware eingegangen", length(warenEinTS))
  wareEinK<-rep("k", length(warenEinTS))
  
  zahlL<-rep("Zahlung durchgeführt", length(zahlTS))
  zahlK<-rep("l", length(zahlTS))
  
  #####Änderungshistorie mit ifs fehlt noch
  
  ts<-c(bestPosTS, bestellTS, kreditorTS, einRechnTS, rechnungsTS, warenEinTS, zahlTS)
  lang<- c(bestPosL, bestellL, kreditorL, einRechnL, rechnungL, wareEinL, zahlL)
  kurz<- c(bestPosK, bestellK, kreditorK, einRechnK, rechnungK, wareEinK, zahlK)
  
  eventlog1<-data.frame(CaseID= i, Timestamp= ts, Aktivitaet= lang, Kurz= kurz, stringsAsFactors = F)
  eventlog1_ord<- eventlog1[order(eventlog1$Timestamp),]
  eventlog<- rbind(eventlog, eventlog1_ord)
}

#Eventlog 1. Hilfzeile löschen
eventlog<- eventlog[-1,]

#Ansehen Eventlog
View(eventlog)
head(eventlog,30)
dim(eventlog)

#NAs entfernen
eventlog_ohneNA<- eventlog[-which(is.na(eventlog$Timestamp)),]
head(eventlog_ohneNA,10)
View(eventlog_ohneNA)

eventlog_ohneNA
