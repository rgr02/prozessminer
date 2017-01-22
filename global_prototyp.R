library(shiny)
library(shinydashboard)
library(lubridate)
library(visNetwork)
library(XLConnect)
library(ggplot2)
library(plyr)
library(scales)

#Eventlog unserer Daten
#Dateneinlesen
srcFile <- file.choose()
#############################################################################

################################################################################
# ERSTELLEN DER DATAFRAMES und umbenennen von Variablen
kreditor = readWorksheetFromFile(srcFile,sheet="Kreditor", header=T)
aenderungsHist = readWorksheetFromFile(srcFile,sheet="Aenderungshistorie", header=T)
bestellung = readWorksheetFromFile(srcFile,sheet="Bestellung", header=T)
bestellPos = readWorksheetFromFile(srcFile,sheet="Bestellposition", header=T)
warenEingang = readWorksheetFromFile(srcFile,sheet="Wareneingang", header=T)
rechnung = readWorksheetFromFile(srcFile,sheet="Rechnung", header=T)
zahlung = readWorksheetFromFile(srcFile,sheet="Zahlung", header=T)
# 
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

if ('True' %in% is.na(bestellung$BestellNr)) {
  no_BN_BE<-which(is.na(bestellung$BestellNr))
  bestellung<-bestellung[-no_BN_BE,]
  
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
# verknuepfen der Tabellen
# aequivalenzen zu SQL Joins in R
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
dim(merge_bbwrzk)
# aenderungshistorie
# Tabelle Kreditor
kredit<-which(aenderungsHist$Tabelle == "Kreditor")
aenderungsHist_Kredit <- aenderungsHist[kredit,]
merge_bbwrzk_ak<-merge(x=merge_bbwrzk, y=aenderungsHist_Kredit, by.x="KredNr.x", by.y="ID", all.x = TRUE)
dim(merge_bbwrzk_ak)
# Tabelle Bestellung
bestell<-which(aenderungsHist$Tabelle == "Bestellung")
aenderungsHist_bestell <- aenderungsHist[bestell,]
merge_bbwrzk_akb<-merge(x=merge_bbwrzk_ak, y=aenderungsHist_bestell, by.x="BestellNr", by.y="ID", all.x = TRUE)
dim(merge_bbwrzk_akb)
# Tabelle Bestellposition
# Hilfsspalte mit geaenderter ID - erste Nummer weggeschnitten
x<-substr(aenderungsHist$ID,2,7)
aenderungsHist$IDTrim=x
bestellP<-which(aenderungsHist$Tabelle == "Bestellposition")
aenderungsHist_bestellP <- aenderungsHist[bestellP,]
merge_bbwrzk_akbb<-merge(x=merge_bbwrzk_akb, y=aenderungsHist_bestellP, by.x="BestellNr", by.y="IDTrim", all.x = TRUE)
################################################################################

################################################################################
# Festlegen der benötigten Spalten für die Hilfstabelle
spalten<-c("PosNr_Bestellpos","BestellNr","ErstellTS_Bestellpos","ErstellTS_Bestellung","ErstellTS_Kreditor","EingansDat",
           "RechnungsDatum","EingangsTS_WAE","ZahlTS_Zahlung","AenderTS","Feld","Wert_neu","Tabelle")

# Relevante Ergebnisse als Hilfstabelle. Input für den a-Algorithmus
hilfstabelle <- merge_bbwrzk_akbb[,spalten]
################################################################################
caseID<-hilfstabelle[,"BestellNr"]*10+hilfstabelle[,"PosNr_Bestellpos"]
hilfstabelle<-cbind(caseID,hilfstabelle)
################################################################################
# PERSISTIEREN DER HILFSTABELLE
# workbook anscheinend für createSheet benötigt.

# Implementierung laut Doku
# wb <- loadWorkbook(srcFile, create = T)
# createSheet(wb,"Hilfstabelle") # Name für das Arbeitsblatt
# setColumnWidth(wb,"Hilfstabelle",column = c(1:13),5000)
# saveWorkbook(wb)
# writeWorksheetToFile(srcFile, hilfstabelle, sheet = "Hilfstabelle") 
################################################################################

################################################################################
# Eventlog erstellen

bestellNr <- unique(hilfstabelle$caseID)

aggregate(hilfstabelle$ErstellTS_Bestellpos, by=list(hilfstabelle$PosNr_Bestellpos,hilfstabelle$BestellNr),mean)

#Eventlog initialisieren
eventlog<-data.frame(caseID= 0, timestamp= Sys.time(), akt= "Test",  stringsAsFactors = F)

#Erstellen des Eventlogs für alle BestellNummern
for(i in bestellNr){
  #Hilfstabelle Tab mit nur eintraegen mit bestellNr
  tab<- hilfstabelle[which(hilfstabelle$caseID==i),]
  
  #Herauslesen Timestamps
  bestPosTS<- unique(tab$ErstellTS_Bestellpos)
  bestellTS<- unique(tab$ErstellTS_Bestellung)
  kreditorTS<- unique(tab$ErstellTS_Kreditor)
  einRechnTS<- unique(tab$EingansDat)
  rechnungsTS<- unique(tab$RechnungsDatum)
  warenEinTS<- unique(tab$EingangsTS_WAE)
  zahlTS<- unique(tab$ZahlTS_Zahlung)
  
  #Erstellung Aktivitaetennamen
  bestPosL<-rep("Bestellposition erstellt", length(bestPosTS))
  bestellL<-rep("Bestellung erstellt", length(bestellTS))
  kreditorL<-rep("Kreditor erstellt", length(kreditorTS))
  einRechnL<-rep("Rechnung eingegangen", length(einRechnTS))
  rechnungL<-rep("Rechnung gestellt", length(rechnungsTS))
  wareEinL<-rep("Ware eingegangen", length(warenEinTS))
  zahlL<-rep("Zahlung durchgefuehrt", length(zahlTS))
  
  tabAender<- tab[!is.na(tab$AenderTS),c("AenderTS", "Feld", "Wert_neu","Tabelle")]
  tabAender<-tabAender[!duplicated(tabAender),]
  
  #Herauslesen der Eventlogeintraege von Tabelle Aenderungshistorie
  aenderTS<-tabAender$AenderTS
  aenderL<-NULL
  if(dim(tabAender)[1]>0){
    for(j in 1:dim(tabAender)[1]){
      if(tabAender[j,"Feld"]=="Menge"){
        aenderL<- c(aenderL, "Bestellmenge geaendert")
      }
      if(tabAender[j,"Feld"]=="Preis"){
        aenderL<- c(aenderL, "Preis geaendert")
      }
      if(tabAender[j,"Feld"]=="SperrKZ" && tabAender[j,"Wert_neu"]=="X"){
        aenderL<- c(aenderL, "Kreditor gesperrt")
      }
      if(tabAender[j,"Feld"]=="SperrKZ" && is.na(tabAender[j,"Wert_neu"])){
        aenderL<- c(aenderL, "Kreditor entsperrt")
      }
      if(tabAender[j,"Feld"]=="StornoKZ" && tabAender[j,"Tabelle"]=="Bestellposition"){
        aenderL<- c(aenderL, "Bestellposition storniert")
      }
      if(tabAender[j,"Feld"]=="StornoKZ" && tabAender[j,"Tabelle"]=="Bestellung"){
        aenderL<- c(aenderL, "Bestellung storniert")
      }
    }
    
  }
  if(length(aenderL)!=length(aenderTS)){
    aenderL<-NULL
    aenderTS<-NULL
  }
  
  
  
  ts<-c(bestPosTS, bestellTS, kreditorTS, einRechnTS, rechnungsTS, warenEinTS, zahlTS,aenderTS)
  lang<- c(bestPosL, bestellL, kreditorL, einRechnL, rechnungL, wareEinL, zahlL, aenderL)
  #kurz<- c(bestPosK, bestellK, kreditorK, einRechnK, rechnungK, wareEinK, zahlK)
  
  eventlog1<-data.frame(caseID= i, timestamp= ts, akt= lang, stringsAsFactors = F)
  eventlog1_ord<- eventlog1[order(eventlog1$timestamp),]
  
  eventlog<- rbind(eventlog, eventlog1_ord)
}

#Eventlog 1. Hilfzeile löschen
eventlog<- eventlog[-1,]
#NAs entfernen
eventlog_ohneNA<- eventlog[-which(is.na(eventlog$timestamp)),]
eventlog<-eventlog_ohneNA
#In Exeldokument speichern
# wb_Event <- loadWorkbook(srcFile, create = T)
# createSheet(wb_Event,"Eventlog") # Name für das Arbeitsblatt
# setColumnWidth(wb_Event,"Eventlog",column = c(1:13),5000)
# saveWorkbook(wb_Event)
# writeWorksheetToFile(srcFile, eventlog, sheet = "Eventlog")
