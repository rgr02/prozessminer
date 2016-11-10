library(XLConnect)

# file.choose() öffnet einen Editor
# Laden der Inputdaten 
srcFile <- file.choose()

# Erstellen der Dataframes
kreditor = readWorksheetFromFile(srcFile,sheet="Kreditor", header=T)
änderungsHist = readWorksheetFromFile(srcFile,sheet="Änderungshistorie", header=T)
bestellung = readWorksheetFromFile(srcFile,sheet="Bestellung", header=T)
bestellPos = readWorksheetFromFile(srcFile,sheet="Bestellposition", header=T)
warenEingang = readWorksheetFromFile(srcFile,sheet="Wareneingang", header=T)
rechnung = readWorksheetFromFile(srcFile,sheet="Rechnung", header=T)
zahlung = readWorksheetFromFile(srcFile,sheet="Zahlung", header=T)

# Testausgabe
str(c(kreditor, änderungsHist, bestellung, bestellPos, warenEingang, rechnung, zahlung))
head(c(kreditor, änderungsHist, bestellung, bestellPos, warenEingang, rechnung, zahlung))

