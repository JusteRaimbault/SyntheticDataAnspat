

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

depcount = adresses %>% group_by(DEP) %>% summarise(count=n())

# random
synth$Domcode = sample(data2016$Codresid,NEtus)

synth$DEPRES = sapply(synth$Domcode,function(s){substr(s,1,2)})

###
## 2) Transportation times

# -> use TC network
source('network.R')

# RER
trgraph=addTransportationLayer('data/gis/gares.shp','data/gis/rer_lignes.shp')
# Transilien
trgraph=addTransportationLayer('data/gis/empty.shp','data/gis/train_banlieue_lignes.shp',g = trgraph)
# Metro
trgraph=addTransportationLayer('data/gis/metro_stations.shp','data/gis/test_metro.shp',g = trgraph)
# Tram
trgraph=addTransportationLayer('data/gis/TCSP_arrets.shp','data/gis/TCSP_lignes.shp',g = trgraph)




# random for now
#synth$TpsP7 = sample(x=c(data2016$TpsUniv_min,d$TpsP7),size=NEtus,replace=TRUE)

###
## 3) Average and max time estimations

times=data.frame(tpsmin=c(data2016$TpsUniv_min,d$TpsP7),tpsmax=c(data2016$TpsUniv_max,d$MaxTps))
#plot(c(data2016$TpsUniv_min,d$TpsP7),c(data2016$TpsUniv_max,d$MaxTps))
ggplot(times,aes(x=tpsmin,y=tpsmax))+
  geom_point()+stat_smooth()

# -> a univariate linear should do, with fat tail residuals (sample from model residuals)
est = lm(tpsmax~tpsmin,times)
#hist(est$residuals,breaks=30)
synth$MaxTps = synth$TpsP7 * est$coefficients[2] + sample(x=est$residuals,size=NEtus,replace=TRUE)
# round at 5 min
synth$MaxTps = round(synth$MaxTps / 5)*5

# average time available only for 2017, estimation will be ultra poor
ggplot(data.frame(avgtps=d$AvgTps,tpsmin=d$TpsP7),aes(x=mintps,y=tpsmin))+
  geom_point()+stat_smooth()
est = lm(avgtps~tpsmin,data.frame(avgtps=d$AvgTps,tpsmin=d$TpsP7))
# interesting : they actually take less time than the "min" time (ratp times)
synth$AvgTps = synth$TpsP7 * est$coefficients[2] + sample(x=est$residuals,size=NEtus,replace=TRUE)
synth$AvgTps = round(synth$AvgTps / 5)*5



###
## 4) Dwelling status

tabfamille = table(rbind(data2016[,c("DEPRES","Famille")],d[,c("DEPRES","Famille")]))
# -> for Paris, clear proportion out of family, for petite couronne, small chance also
#  just draw conditionnaly to the contingency table
synth$Famille = sapply(synth$DEPRES,function(dep){ifelse(runif(1)<tabfamille[dep,1]/(tabfamille[dep,1]+tabfamille[dep,2]),"Non","Oui")})


###
## 5) Modal Choice

library(mlogit)

# Q : consider mode succession or alternative ? (could then duplicate rows)

# test the mlogit package
data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")

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

mlogit(mFormula(mode ~ 1 | tpsmin + tpsmax), data = choice)





