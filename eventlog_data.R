#install.packages("XLConnect")
#install.packages("XLConnectJars")

#library(XLConnectJars)
library(XLConnect)

#############################################################################
# (1) LADEN DER INPUTDATEN
# file.choose() öffnet einen Editor
srcFile <- file.choose()
#############################################################################

#############################################################################
# (2) ERSTELLEN DER DATAFRAMES
kreditor = readWorksheetFromFile(srcFile,sheet="Kreditor", header=T)
aenderungsHist = readWorksheetFromFile(srcFile,sheet="Änderungshistorie", header=T)
bestellung = readWorksheetFromFile(srcFile,sheet="Bestellung", header=T)
bestellPos = readWorksheetFromFile(srcFile,sheet="Bestellposition", header=T)
warenEingang = readWorksheetFromFile(srcFile,sheet="Wareneingang", header=T)
rechnung = readWorksheetFromFile(srcFile,sheet="Rechnung", header=T)
zahlung = readWorksheetFromFile(srcFile,sheet="Zahlung", header=T)

#######################################################################
#(3) Bereinigung von Fehlern

no_bestellNr<-which(is.na(warenEingang$BestellNr))
warenEingang<- warenEingang[-no_bestellNr,]
######################################################################
# (4) Joinen der Tabellen

#Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
#Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
#Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
#Cross join: merge(x = df1, y = df2, by = NULL)

head(bestellung)
str(bestellPos)

#bestellung mit bestellPos
merge_bb<-merge(x=bestellung, y=bestellPos, by= "BestellNr", all=TRUE)
dim(merge_bb)
dim(bestellung)
dim(bestellPos)
bestellPos$BestellNr%in%merge_bb$BestellNr
bestellung$BestellNr%in%merge_bb$BestellNr

#join mit wareneingang
str(warenEingang)
merge_bbw<-merge(x=merge_bb, y=warenEingang, by="BestellNr", all=TRUE)
dim(merge_bbw)
dim(warenEingang)
dim(merge_bb)
merge_bb$BestellNr%in%merge_bbw$BestellNr
merge_bbw$BestellNr%in%merge_bb$BestellNr

#join mit kreditor
str(kreditor)
str(merge_bbw)
merge_bbwk<-merge(x=merge_bbw, y=kreditor, by.x="KredNr.x", by.y="KredNr", all=TRUE)

merge_bbw$BestellNr%in%merge_bbwk$BestellNr
merge_bbwk$BestellNr%in%merge_bbw$BestellNr# False Fälle genauer Ansehen

merge_bbw$KredNr.x%in%merge_bbwk$KredNr.x
merge_bbwk$KredNr.x%in%merge_bbw$KredNr.x# False Fälle genauer Ansehen

#FAlse Fälle werden ansgeschaut -> keine Ahnung was wir damit machen
merge_bbw[which(!(merge_bbwk$BestellNr%in%merge_bbw$BestellNr)),]
merge_bbwk[which(!(merge_bbwk$KredNr.x%in%merge_bbw$KredNr.x)),]

str(rechnung)
