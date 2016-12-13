#install.packages("edeaR")
library(edeaR)
library(visNetwork)

caseID<-rep(c(1,2,3,4), c(5,5,6,4))
akt<- c("a","a","b","d","f","a","e","c","d","f", "g","c","e","b","a","f","a","b","d","g")

eventlog<-data.frame(caseID,akt,stringsAsFactors = F)

cases<-unique(eventlog$caseID)
maxVar<- max(table(eventlog$caseID))
varianten<- NULL

for(i in 1:length(cases)){
  case<- eventlog[which(eventlog$caseID==cases[i]),]
  variante<- c(case$akt, rep("X", maxVar-length(case$akt)))
  varianten<- rbind(varianten, variante)
}
View(varianten)
anzKennzahl<-NULL
dim(varianten)
head(varianten)

#Doppelte Varianten löschen
for(i in 1:dim(varianten)[1]){
  anz<-1
  if(i>dim(varianten)[1]){
    break
  }
  for(j in i:dim(varianten)[1]){
    if(j>dim(varianten)[1]){
      break
    }
    if(identical(varianten[i,],varianten[j,])&& i!=j){
      anz<-anz+1 
      varianten<-varianten[-j,]
    }
  }
  anzKennzahl<- c(anzKennzahl,anz)
}
varianten_Anz<- cbind(varianten,anzKennzahl)
View(varianten)

startEvents<- unique(varianten[,1])
startEvents


varianten
###Beziehungen
bez<-NULL

for(i in 1:dim(varianten)[1]){
  for(j in 1:(dim(varianten)[2]-1)){
    bez_help<-c(varianten[i,j], varianten[i,j+1])
    bez_sub<-bez[which(bez[,1]==bez_help[1]),]
    bez_sub<- bez_sub[which(bez_sub[2]==bez_help[2])]
    bez<-rbind(bez, bez_help)
    
  }
}
#Endknoten
end_help<-bez[which(bez[,2]=="X"),1]
endEvents<- varianten[,dim(varianten)[2]]
endEvents<-unique(c(end_help,endEvents))
endEvents<- endEvents[which(endEvents!="X")]

#Beziehungen X entfernen
bez<- bez[which(bez[,1]!="X"),]
bez<- bez[which(bez[,2]!="X"),]


#doppelte Beziehungen entfernen
alsWort<-unique(paste0(bez[,1],"-",bez[,2]))
alsWort
wieOftBez1<-table(paste0(bez[,1],"-",bez[,2]))
wieOftBez1
namesBez<-  unlist(strsplit(names(wieOftBez1), split="-"))
namesBez
wieOftBez<- data.frame(wieOftBez1, matrix(namesBez, ncol = 2, byrow = T),stringsAsFactors = F)
wieOftBez<-wieOftBez[,-1]#Spalte Var1 wird gelöscht

vec<-unlist(strsplit(alsWort, split="-"))

bez<-data.frame(matrix(vec, ncol = 2, byrow = T),stringsAsFactors = F)
dim(bez)
View(bez)

#symetrische Beziehung entfernen -> Abhängigkeiten
for(i in 1:dim(bez)[1]){
  if(i>dim(bez)[1]){
    break
  }
  rowi<-as.vector(bez[i,])
  for(j in 1:dim(bez)[1]){
    if(j>dim(bez)[1]){
      break
    }
    rowj_sym<-c(bez[j,2], bez[j,1])
    if(rowi[1]==rowj_sym[1]&& rowi[2]==rowj_sym[2] && i!=j){
      print("hier")
      bez<-bez[-c(i,j),]
    }
  }
}
bez


#Startpunkt erstellen
startNodes<-data.frame(Startknoten="S", start=startEvents, stringsAsFactors = F)

#Endknoten erstellen
endNodes<-data.frame(end=endEvents, Endknoten="E", stringsAsFactors = F)


#####Graph erstellen
nodes1<-unique(c("E", "S",unique(bez$X1, bez$X2)))
nodes1
nodes<- data.frame(id=nodes1, label= nodes1,color=c("red","green",rep("#CECEF6",length(nodes1)-2)), x=c(1,rep(NULL,length(nodes1)-1)))
nodes
from<- c(startNodes$Startknoten, endNodes$end,bez$X1)
to<- c(startNodes$start, endNodes$Endknoten, bez$X2)
edges<- data.frame(from=from, to=to)


edges
nodes

visNetwork(nodes,edges)%>%visEdges(arrows = 'to')%>%
  visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}")
#Position stetzen
#http://stackoverflow.com/questions/38265713/how-to-manually-change-the-position-of-nodes-in-visnetwork-in-r
