library(XLConnect)

#############################################################################
# (1) LADEN DER INPUTDATEN
# file.choose() öffnet einen Editor
srcFile <- file.choose()
#############################################################################

#############################################################################
# (2) ERSTELLEN DER DATAFRAMES
kreditor = readWorksheetFromFile(srcFile,sheet="Kreditor", header=T)
änderungsHist = readWorksheetFromFile(srcFile,sheet="Änderungshistorie", header=T)
bestellung = readWorksheetFromFile(srcFile,sheet="Bestellung", header=T)
bestellPos = readWorksheetFromFile(srcFile,sheet="Bestellposition", header=T)
warenEingang = readWorksheetFromFile(srcFile,sheet="Wareneingang", header=T)
rechnung = readWorksheetFromFile(srcFile,sheet="Rechnung", header=T)
zahlung = readWorksheetFromFile(srcFile,sheet="Zahlung", header=T)

# Der Versuch, die Arbeitsblätter als Vektor zu übergeben sollte laut Doku
# funktionieren, liefert hier aber eine Exception

# sheet <- c("kreditor",
#            "änderungsHist",
#            "bestellung",
#            "bestellPos",
#            "warenEingang",
#            "rechnung",
#            "zahlung")
# 
# data <- readWorksheetFromFile(srcFile,
#                               sheet=c("kreditor",
#                                       "änderungsHist",
#                                       "bestellung",
#                                       "bestellPos",
#                                       "warenEingang",
#                                       "rechnung",
#                                       "zahlung"),
#                               header=T)
# Error: IllegalArgumentException (Java): Sheet index (-1) is out of range (0..7)
# Ich habe keine Ahnung warum der sheetIndex bei -1 anfängt?
# Entspricht -1 == Default = False ? Existiert der Vektor?
# str(data)
# head(data)
# 
# Testausgabe
str(c(kreditor, änderungsHist, bestellung, bestellPos, warenEingang, rechnung, zahlung))
head(c(kreditor, änderungsHist, bestellung, bestellPos, warenEingang, rechnung, zahlung))
############################################################################# 

#############################################################################
# (3) PERSISTIEREN DES EVENTLOG
# Annahme das der Name für den EventLog DataFrame event ist
# workbook anscheinend für createSheet benötigt.

# Auskommentiert bis der Alpha Algorithmus implementiert ist.
# wb <- loadWorkbook(srcFile, create = T)
# writeWorksheetToFile(srcFile, event, sheet = createSheet(wb,"EventLog")) 
