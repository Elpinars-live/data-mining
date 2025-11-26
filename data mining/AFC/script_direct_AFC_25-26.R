################################
################################
#####
#####
#####   TD AFC-AFCM
#####
#####
################################
################################

######
## Zola
######

##
# Q1
setwd("~/enseignement/EDSB/M1/Data Mining/TD3/")
#zola <- read.table("Zola_bis.txt",header=TRUE,sep="",row.names=1)
zola <- read.table("Zola_bis.txt",header=TRUE)
head(zola)
summary(zola)
class(zola)

##
# Q2
source("fonctions_afc.R")

##
# Q3
resafc <- afc(zola,k=5)
dim(resafc$C)
# reconstitution de la représentation des oeuvres
plot(resafc$C[,1:2],type="n")
text(resafc$C[,1:2],labels=rownames(zola))
# ajout des COS2 en couleur (optionnel)
colmoi=rainbow(n=10000)
text(resafc$C[,1:2],labels=rownames(zola),col=colmoi[resafc$COSus[,1]])

###
# pbiom
###

##
# Q1
# déjà fait

##
# Q2
pbiom <- read.table("pbiom.txt",header=TRUE)

##
# Q3
pbiomDC<-Codisjc(pbiom)$U
dim(pbiomDC)
pbiom[,1]
table(pbiom[,1])
head(pbiomDC[,1:3])

##
# Q4
resafcm <- afc(pbiomDC)

##
# Q6
star.graph(resafcm$C[,1:2],pbiom$SITPROF)
1