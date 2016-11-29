#install.packages("edeaR")
library(edeaR)

caseID<-rep(c(1,2,3,4), c(4,5,6,4))
akt<- c("a","b","d","f","a","e","c","d","f", "g","c","e","b","a","f","a","b","d","f")

eventlog<-data.frame(caseID,akt,stringsAsFactors = F)
str(eventlog)

end_activities(eventlog)

cases<-unique(eventlog$caseID)
maxVar<- max(table(eventlog$caseID))
varianten<- NULL

for(i in 1:length(cases)){
  case<- eventlog[which(eventlog$caseID==cases[i]),]
  variante<- c(case$akt, rep("X", maxVar-length(case$akt)))
  varianten<- rbind(varianten, variante)
}

anzKennzahl<-NULL

for(i in 1:dim(varianten)[1]){
  anz<-1
  if(i>dim(varianten)[1]){
    print("bin break")
    break
  }
  for(j in 1:dim(varianten)[1]){
    
    if(identical(varianten[i,],varianten[j,])&& i!=j){
      anz<-anz+1 
      print("hier")
      varianten<-varianten[-j,]
      print("bin hier")
      print(anz)
    }
  }
  anzKennzahl<- c(anzKennzahl,anz)
}
anzKennzahl
varianten
eventlog

startEvents<- unique(varianten[,1])
startEvent
####
#TO Endevent

