#install.packages("edeaR")
library(edeaR)
library(visNetwork)

caseID<-rep(c(1,2,3,4), c(4,5,6,4))
akt<- c("a","b","d","f","a","e","c","d","f", "g","c","e","b","a","f","a","b","d","g")

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
    break
  }
  for(j in 1:dim(varianten)[1]){
    
    if(identical(varianten[i,],varianten[j,])&& i!=j){
      anz<-anz+1 
      varianten<-varianten[-j,]
    }
  }
  anzKennzahl<- c(anzKennzahl,anz)
}
anzKennzahl
varianten
eventlog

startEvents<- unique(varianten[,1])
startEvents


varianten
###Beziehungen
bez<-NULL

for(i in 1:dim(varianten)[1]){
  for(j in 1:(dim(varianten)[2]-1)){
    bez_help<-c(varianten[i,j], varianten[i,j+1])
    print(bez_help)
    print(i)
    print(j)
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

bez
dim(bez)
#doppelte Beziehungen entfernen
alsWort<-unique(paste0(bez[,1],"-",bez[,2]))
vec<-unlist(strsplit(alsWort, split="-"))

bez<-data.frame(matrix(vec, ncol = 2, byrow = T),stringsAsFactors = F)
dim(bez)
bez

#symetrische Beziehung entfernen -> Abhängigkeiten
for(i in 1:dim(bez)[1]){
  if(i>dim(bez)[1]){
    break
  }
  rowi<-as.vector(bez[i,])
  #print((rowi[1]))
  for(j in 1:dim(bez)[1]){
    if(j>dim(bez)[1]){
      break
    }
    rowj_sym<-c(bez[j,2], bez[j,1])
   # print(rowj_sym[1])
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

bez
#####Graph erstellen
nodes1<-c("E", "S",unique(bez$X1, bez$X2))
nodes<- data.frame(label=nodes1, stringsAsFactors = F)

from<- c(startNodes$Startknoten, endNodes$end,bez$X1)
to<- c(startNodes$start, endNodes$Endknoten, bez$X2)
edges<- data.frame(from=from, to=to, stringsAsFactors = F)
edges

visNetwork(nodes,edges) %>%visEdges(arrows ="to")
?visNetwork

edges

nodes <- data.frame(id = )
edges <- data.frame(from = c(1,2), to = c(1,3))

visNetwork(nodes, edges)
