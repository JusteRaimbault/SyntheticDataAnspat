

###
# L2 ANSPAT 2016-2017
#
# Synthetic Data Generation for incomplete questionnaire
#

setwd(paste0(Sys.getenv('MONITORAT'),'/L2AnalyseSpatiale/DM/SyntheticData'))

library(readODS)
library(dplyr)
library(ggplot2)
library(igraph)

##
# Estimation procedure
#   1) Draw supplementary adresses from adress file
#   2) Estimate min transportation time to facs
#   3) Estimate average and max time (simple model of min time)
#   4) Draw dwelling situation conditionnaly to address (Paris or not)
#   5) Modal Choice : MultinomialLogit[dwelling;tpsmin;tpsmax]
#   6) Comments conditionnaly to modal choice
#   7) Choice of fac : proba of distance (gravity model) ; random then
#   8) Service access : somehow similar to fac
#
 

## Data Loading

data2016 <- as.tbl(read_ods(path = 'data/DataQuest_etus_2016.ods',col_names = TRUE,sheet = 'Etudiants'))
adresses <- as.tbl(read_ods(path='data/L2_comresid.ods',col_names=TRUE,sheet=2))
d<- as.tbl(read_ods(path='data/DM_21quest.ods',col_names=TRUE,sheet='Feuil1'))

data2016$DEPRES = sapply(data2016$Codresid,function(s){substr(s,1,2)})

## Data Generation global parameters
NEtus = 20
synth=data.frame(synthid=1:NEtus)

# seed for reproducibility
set.seed(1)

###
## 1) Adresses

#depcount = adresses %>% group_by(DEP) %>% summarise(count=n())

# random
#synth$Domcode = sample(data2016$Codresid,NEtus)
#synth$DEPRES = sapply(synth$Domcode,function(s){substr(s,1,2)})

synth$Domcode = sample(setdiff(adresses$CODCOM,d$Domcode),size=NEtus,replace = FALSE)
synth$DEPRES = sapply(synth$Domcode,function(s){substr(s,1,2)})

###
## 2) Transportation times

# random
#synth$TpsP7 = sample(x=c(data2016$TpsUniv_min,d$TpsP7),size=NEtus,replace=TRUE)

# -> use TC network
source('network.R')

# construct network
# speeds : RER 60km.h-1 -> 0.001 min.m-1 ; Transilien 100kmh ; Metro 30kmh ; Tram 20kmh
# communes : car, 50kmh ; fac : pied 5kmh
# RER
trgraph=addTransportationLayer('data/gis/gares.shp','data/gis/rer_lignes.shp',speed=0.001)
# Transilien
trgraph=addTransportationLayer('data/gis/empty.shp','data/gis/train_banlieue_lignes.shp',g = trgraph,speed=6e-04)
# Metro
trgraph=addTransportationLayer('data/gis/metro_stations.shp','data/gis/test_metro.shp',g = trgraph,speed=0.002)
# Tram
trgraph=addTransportationLayer('data/gis/TCSP_arrets.shp','data/gis/TCSP_lignes.shp',g = trgraph,speed=0.003)

# add communes
comgraph = addAdministrativeLayer(trgraph,"data/gis/communes.shp",connect_speed = 0.0012)

# add destination facs
fullgraph = addPointsLayer(comgraph,'data/gis/facs.shp',connect_speed = 0.012)

#save(fullgraph,file='data/fullgraph.RData')
#load('data/fullgraph.RData')

# compute times
fromids = c();for(cp in synth$Domcode){if(length(which(V(fullgraph)$CP==cp))>0){show(cp)};fromids=append(fromids,which(V(fullgraph)$CP==cp))}
#fromids = c();for(cp in d$Domcode){if(length(which(V(fullgraph)$CP==cp))>0){show(cp)};fromids=append(fromids,which(V(fullgraph)$CP==cp))}
toids = c();for(fac in c("P1","P7","P4","P8","P10","P12","UPEM")){toids = append(toids,which(V(fullgraph)$pointname==fac))}
factimes = distances(graph = fullgraph,v = fromids,to = toids,weights = E(fullgraph)$speed*E(fullgraph)$length)

factimes = round(factimes)

#min(d[,7:13])
# -> a bit underestimated, add 10 minutes ?
#factimes = factimes + 10
#abserr=c();for(penalty in 0:30){abserr=append(abserr,sum(abs(d[c(1:17,21),7:13]-factimes[c(1:17,21),]-penalty)))}
#plot(0:30,abserr,type='l')
# -> 21 minutes on real times
#factimes = factimes + 21
# but gives shitty modal choices
factimes = factimes + 10

synth=cbind(synth,factimes)
colnames(synth)[3:9]<-c("P1","P7","P4","P8","P10","P12","UPEM")

synth$TpsP7=synth$P7

###
## 3) Average and max time estimations

times=data.frame(tpsmin=c(data2016$TpsUniv_min,d$TpsP7),tpsmax=c(data2016$TpsUniv_max,d$MaxTps))
#plot(c(data2016$TpsUniv_min,d$TpsP7),c(data2016$TpsUniv_max,d$MaxTps))
#ggplot(times,aes(x=tpsmin,y=tpsmax))+
#  geom_point()+stat_smooth()

# -> a univariate linear should do, with fat tail residuals (sample from model residuals)
est = lm(tpsmax~tpsmin,times)
#hist(est$residuals,breaks=30)
synth$MaxTps = synth$TpsP7 * est$coefficients[2] + sample(x=est$residuals,size=NEtus,replace=TRUE)
# round at 5 min
synth$MaxTps = round(synth$MaxTps / 5)*5

# average time available only for 2017, estimation will be ultra poor
#ggplot(data.frame(avgtps=d$AvgTps,tpsmin=d$TpsP7),aes(x=mintps,y=tpsmin))+
#  geom_point()+stat_smooth()

est = lm(avgtps~tpsmin,data.frame(avgtps=d$AvgTps,tpsmin=d$TpsP7))
# interesting : they actually take less time than the "min" time (ratp times)
synth$AvgTps = synth$TpsP7 * est$coefficients[2] + sample(x=est$residuals,size=NEtus,replace=TRUE)
synth$AvgTps = round(synth$AvgTps / 5)*5



###
## 4) Dwelling status

tabfamille = table(rbind(data2016[,c("DEPRES","Famille")],d[,c("DEPRES","Famille")]))
# -> for Paris, clear proportion out of family, for petite couronne, small chance also
#  just draw conditionnaly to the contingency table
synth$Famille = sapply(synth$DEPRES,function(dep){if(!dep%in%rownames(tabfamille)){"Oui"}else{ifelse(runif(1)<tabfamille[dep,1]/(tabfamille[dep,1]+tabfamille[dep,2]),"Non","Oui")}})


###
## 5) Modal Choice

library(mlogit)

# Q : consider mode succession or alternative ? (could then duplicate rows)

# test the mlogit package
#data("Fishing", package = "mlogit")
#Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")

# test with raw modes
cdata = data.frame(mode=c(data2016$ModeTransp,d$ModalChoice),famille = c(data2016$Famille,d$Famille),tpsmin=c(data2016$TpsUniv_min,d$TpsP7),tpsmax=c(data2016$TpsUniv_max,d$MaxTps))

#modes = unique(as.character(cdata$mode))
#choice = data.frame();rnames = c()
#for(i in 1:nrow(cdata)){
#  for(cmode in modes){
#    choice = rbind(choice,data.frame(mode=(cmode==cdata[i,1]),alt=cmode,tpsmin=cdata[i,3],tpsmax=cdata[i,4],chid=i))#,famille=cdata[i,2]))
#    rnames = append(rnames,paste0(i,'.',cmode))
#  }
#}
#rownames(choice)<-rnames

mtable = table(as.character(cdata$mode))

rows =  cdata$mode%in%names(mtable)[mtable>3]
cdata$mode <- as.factor(cdata$mode)
choice <-  mlogit.data(cdata[rows,],shape="wide",choice="mode")

est = mlogit(mFormula(mode ~ 1 | tpsmin + tpsmax), data = choice)

bPiedMetroMin = est$coefficients["A pied (si plus de 10 minutes), Métro / RER:tpsmin"]
bPiedMetroMax = est$coefficients["A pied (si plus de 10 minutes), Métro / RER:tpsmax"]
bPiedMetroBusMin=est$coefficients["A pied (si plus de 10 minutes), Métro / RER, Bus:tpsmin"]
bPiedMetroBusMax=est$coefficients["A pied (si plus de 10 minutes), Métro / RER, Bus:tpsmax"]
bMetroMin = est$coefficients["Métro / RER:tpsmin"]
bMetroMax = est$coefficients["Métro / RER:tpsmax"]
bMetroBusMin = est$coefficients["Métro / RER, Bus:tpsmin"]
bMetroBusMax = est$coefficients["Métro / RER, Bus:tpsmax"]
bMetroVoitMin = est$coefficients["Métro / RER, Bus, Voiture:tpsmin"]
bMetroVoitMax = est$coefficients["Métro / RER, Bus, Voiture:tpsmax"]

UPiedMetro = bPiedMetroMin*synth$TpsP7 + bPiedMetroMax*synth$MaxTps
UPiedMetroBus = bPiedMetroBusMin*synth$TpsP7 + bPiedMetroBusMax*synth$MaxTps
UMetro =  bMetroMin*synth$TpsP7 + bMetroMax*synth$MaxTps
UMetroBus = bMetroBusMin*synth$TpsP7 + bMetroBusMax*synth$MaxTps
UMetroVoit = bMetroVoitMin*synth$TpsP7 + bMetroVoitMax*synth$MaxTps
etot = exp(UPiedMetro)+exp(UPiedMetroBus)+exp(UMetro)+exp(UMetroBus)+exp(UMetroVoit)

PPiedMetro = exp(UPiedMetro)/etot
PPiedMetroBus = exp(UPiedMetroBus)/etot
PMetro = exp(UMetro)/etot
PMetroBus = exp(UMetroBus)/etot
PMetroVoit = exp(UMetroVoit)/etot
probas = data.frame(PPiedMetro,PPiedMetroBus,PMetro,PMetroBus,PMetroVoit)
cumprobas = t(apply(probas,1,cumsum))
colnames(cumprobas)<-c("A pied (si plus de 10 minutes), Métro / RER","A pied (si plus de 10 minutes), Métro / RER, Bus","Métro / RER","Métro / RER, Bus","Métro / RER, Bus, Voiture")
# draw mode by hand
trmodes = c()
for(i in 1:NEtus){
  found=FALSE;r=runif(1)
  for(j in 1:ncol(cumprobas)){if(r<cumprobas[i,j]&!found){trmodes=append(trmodes,colnames(cumprobas)[j]);found=TRUE}}
}

synth$ModalChoice = trmodes

#apply(cumprobas,1,function(r){inds = which(r>runif(1));if(length(inds)==0){return(1)}else{}})
#drawMode<-function()

###
## 6) Comments

# just draw from contingency table

comtable = table(data2016[,c("ModeTransp","PercepTransp")])
t(apply(comtable,1,cumsum))

# -> does not work, not normalized !


###
## 7) Fac choice

timetable = table(cut(d$TpsP7,breaks = 5),d$FacChoice)
distfactor = grep("L'Université est proche de chez moi",d$FacChoice)
table(cut(d$TpsP7,breaks = 5),d$FacChoice%in%d$FacChoice[distfactor])
# -> ultra rough distance effect : juste sample within with "fac proche de chez moi" si < 50 min

facchoice=c()
for(i in 1:NEtus){
  if(synth$TpsP7[i]<50){facchoice=append(facchoice,sample(d$FacChoice%in%d$FacChoice[distfactor],size=1))}
  else{facchoice=append(facchoice,sample(d$FacChoice,size=1))}
}

synth$FacChoice = facchoice


#### Export

#write.table(synth,file='synth.csv',sep=";",quote=FALSE)


###
# 8) Cinema access

# synth<-read.table(file='synth.csv',sep=';',quote="")

# euclidian distances
getDistances<-function(fromCP,toCP,mode){
  fromids = c();for(cp in fromCP){if(length(which(V(fullgraph)$CP==cp))>0){fromids=append(fromids,which(V(fullgraph)$CP==cp))}}
  toids = c();for(cp in toCP){if(length(which(V(fullgraph)$CP==cp))>0){toids = append(toids,which(V(fullgraph)$CP==cp))}}
  fromids=unique(fromids);toids=unique(toids)
  if(mode=="euclidian"){
    dmat = matrix(rep(0,length(fromids)*length(toids)),nrow=length(fromids),ncol=length(toids))
    for(i in 1:length(fromids)){
      for(j in 1:length(toids)){
        dmat[i,j]=sqrt((V(fullgraph)$x[fromids[i]]-V(fullgraph)$x[toids[j]])^2 + (V(fullgraph)$y[fromids[i]]-V(fullgraph)$y[toids[j]])^2)
      }
    }
    rownames(dmat)=V(fullgraph)$CP[fromids];colnames(dmat)=V(fullgraph)$CP[toids]
    return(dmat)
  }
  if(mode=="network"){
    dmat = distances(graph = fullgraph,v = fromids,to = toids,weights = E(fullgraph)$speed*E(fullgraph)$length)
    rownames(dmat)=V(fullgraph)$CP[fromids];colnames(dmat)=V(fullgraph)$CP[toids]
    return(dmat)
  }
}

eucldist = getDistances(d$Domcode,d$TheaterCode,mode="euclidian")
nwdist = getDistances(d$Domcode,d$TheaterCode,mode="network")

ed=c();nd=c()
for(i in 1:length(d$Domcode)){
  if(!is.na(d$TheaterCode[i])&as.character(d$TheaterCode[i])%in%colnames(eucldist)){
    ed=append(ed,eucldist[as.character(d$Domcode[i]),as.character(d$TheaterCode[i])]);nd=append(nd,nwdist[as.character(d$Domcode[i]),as.character(d$TheaterCode[i])])}}

plot(ed,nd)

# -> sample codes in radius < 10km
# travel time seems random
communes <- readOGR('data/gis','communes')
adj <- gTouches(communes,byid = TRUE)
rownames(adj)<-communes$INSEE_COMM;colnames(adj)<-communes$INSEE_COMM

theatercodes=c();theatertimes=c();theatermodes=c()
for(i in 1:nrow(synth)){
  if(runif(1)<0.25){
    theatercodes=append(theatercodes,as.character(synth$Domcode[i]))
    theatertimes=append(theatertimes,runif(1,min = 5,max=10))
    theatermodes=append(theatermodes,sample(c("A pied (si seul mode ou si plus de 10 minutes combiné à un autre mode)","Voiture"),size=1))
  }
  else{
    theatercodes=append(theatercodes,sample(colnames(adj)[which(adj[as.character(synth$Domcode[i]),])],size=1))
    theatertimes=append(theatertimes,runif(1,min = 10,max=40))
    theatermodes=append(theatermodes,sample(d$TheaterMode,size=1))
  }
}

theatertimes=round(theatertimes / 5)*5

theaterfreq = sample(d$TheaterFreq,replace = TRUE,size=nrow(synth))

synth$TheaterCode = theatercodes
synth$TheaterFreq = theaterfreq
synth$TheaterTime = theatertimes
synth$TheaterMode = theatermodes


######

write.table(synth,file='synth_Cinema.csv',sep=";",quote=FALSE)




