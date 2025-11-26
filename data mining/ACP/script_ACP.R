# TD 2 ACP 
##########

### espace de travail
getwd()
setwd("D:\\M1 EDSB Data Mining\\2025-2026")
### Poissons d'amiard
#1 importation
amiard=read.table("Amiard.txt",header=T,sep="")

#2 jeu de données centré seulement
amiardC=scale(amiard,center=T,scale=F)

#3 jeu de données centré réduit

amiardCR=scale(amiard)

# analyses univariées et bivariées 
# 1 moy et ecart-type
moyA=apply(amiard,2,mean)
ecarttypeA=apply(amiard,2,sd)

#2-3 distribution avant CR/ C/ avec CR
#en 1 graphique
par(mfrow=c(1,3))
boxplot(amiard, main="brutes")
boxplot(amiardC,main="centrées")
boxplot(amiardCR,main="réduites")

#4 
pairs(amiard)
pairs(amiard,lower.panel=panel.smooth)

source("N:\\M1 EDSB Data Mining\\2025-2026\\qq_fonctions_TP1.txt")
pairs(amiard,lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)

#############################
#           ACP
############################

# 1 ACP centrée- réduite
resAcpCR=princomp(amiard,cor=T,scores=T)
summary(resAcpCR)
# on peut y lire les racine(valp), % d'inertie expliquée et %d'inertie expliquée cumulée

names(resAcpCR)

# la somme des valeur propres =16 car données CR
sum(resAcpCR$sdev^2) #16 nombre de variables car CR = somme des valeurs propres

# pourcentage de variance expliquée cumulé
round(cumsum(resAcpCR$sdev^2)/sum(resAcpCR$sdev^2)*100,2)

# choix du nombre d'axes à étudier
# barplot des valeurs propres
par(mfrow=c(1,1))
barplot(resAcpCR$sdev^2)
# ou 
plot(resAcpCR)

resAcpCR$sdev^2

#regle de Kaiser : 4
abline(h=1)
# % variance(inertie) expliquée >=80% : 3 ou 4 
# règle du coude : 2
# regle de saporta 1+2*sqrt((p-1)/(n-1)) = 2.65 --> 2
1+2*sqrt((ncol(amiard)-1)/(nrow(amiard)-1)) 

# 4 graphiques 
par(mfrow=c(1,2))

#plot des individus (u.s.) 

plot(resAcpCR$scores[,c(1,2)],main='représentation des u.s.', xlab='axe 1 47.54%',ylab='axe 2 23.52%',pch=16)
text(resAcpCR$scores[,1],resAcpCR$scores[,2],rownames(amiard),pos=3,cex=0.8)
abline(h=0,v=0,lty=2,col="grey")

# représentation par bassin
groupe=rep(1:3,each=8)
plot(resAcpCR$scores[,c(1,2)],main='représentation des us',xlab='axe1 47.54%', ylab='axe2 23.52%',col=groupe,pch=16)
text(resAcpCR$scores[,1],resAcpCR$scores[,2],rownames(amiard),pos=3,cex=0.8)
abline(h=0,v=0,lty=2,col="grey")


# représentation des variables : cercles des corrélations (ACP CR)
corvar=cor(amiard,resAcpCR$scores[,1:2])

plot(corvar[,1],corvar[,2],xlim=c(-1,1),ylim=c(-1,1),xlab='axe1 47.54%',ylab='axe2 23.52%',main='représentation des variables',type='n')
text(corvar[,1],corvar[,2],labels=colnames(amiard),col=c(rep("violet",9),rep("red",7)))
abline(h=0,v=0,lty=2,col="grey")
# pour le cercle :
angles=seq(0,2*pi,length=1000)
points(cos(angles),sin(angles),type="l",col="blue")
arrows(0, 0, corvar[, 1] * 0.95, corvar[, 2] * 0.95, length = 0.1, angle = 15) # flèches

# autre façon de représenter le cercle de corrélations :
coord <- cbind(resAcpCR$sdev[1]*resAcpCR$loadings[, 1],resAcpCR$sdev[2]*resAcpCR$loadings[, 2])
#=> coord = corvar !
par(mfrow=c(1,2))
plot(coord, 
     main = "Cercle des corrélations",
     xlab = paste("Composante Principale 1"),
     ylab = paste("Composante Principale 2"),
     xlim = c(-1, 1), ylim = c(-1, 1), type = "n", asp = 1) # asp=1 assure un cercle parfait

symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "blue", lty = 2)
abline(h = 0, v = 0, lty = 2, col = "gray")
text(coord, labels = colnames(amiard), cex = 0.8)
arrows(0, 0, coord[, 1] * 0.9, coord[, 2] * 0.95, length = 0.1, angle = 15)
# 
# analyse : à vous !

#################
###2 les villes #
#################
#########
## partie 1 : exploration

##
# Q1-2
setwd("~/enseignement/EDSB/M1/Data Mining/TD2")
villes = read.table("villes.csv",header=TRUE,sep=";",dec=".",row.names=1)
head(villes)

##
# Q3
villesCR=scale(villes)
 apply(villes,2,mean)
dim(villesCR)
boxplot(villesCR)
apply(villesCR,2,mean)
apply(villesCR,2,sd)
par(mfrow=c(1,1))
##
# Q4
# à faire à la maison

###########
## partie 2


## ACP centrée-réduite
resacp = princomp(villes,cor=TRUE)


## inertie cumulée des composantes
cumsum(resacp$sdev^2/sum(resacp$sdev^2))


## choix du nombre de composantes
# règle du coude
barplot(resacp$sdev^2,las=2)
## => 2 composantes

# critère de Saporta (1+2*sqrt((p-1)/(n-1)))
1+2*sqrt((ncol(villes)-1)/(nrow(villes)-1)) 
# 2.69 (=valeur seuil)
resacp$sdev^2
# seulement 2 valeurs propres > 2.69 => 2 composantes

# Kaiser
resacp$sdev^2
# => 5 composantes ont une vp > 1

# 80% d'inertie
cumsum(resacp$sdev^2/sum(resacp$sdev^2))
# => 4 composantes 

### => 2 composantes pour la suite

par(mfrow=c(1,2))
## représentation du plan 1-2
# représentation des observations
plot(resacp$scores[,1:2],pch=20,main="représentation des observations")
text(resacp$scores[,1:2],labels=rownames(villes),cex=0.5,pos=3)
abline(h=0,v=0,lty=2)

# représentation des variables
# calcul des coordonnées des variables
corvar=cor(villes,resacp$scores[,1:2])

#graphique :
plot(corvar[,1],corvar[,2],xlim=c(-1,1),ylim=c(-1,1),xlab="PC1",ylab="PC2",main='représentation des variables',type='n',asp=1) # asp utile sinon le cercle ci-après sort de la fenêtre
# représentation du cercle :
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "blue", lty = 2)
# ajout des noms des variables
text(corvar[,1],corvar[,2],labels=colnames(villes),cex=0.7,col='red')
# ajout des axes :
abline(h=0,v=0,lty=2)
#ajout des flèches
arrows(0,0,corvar[,1]*0.95,corvar[,2]*0.95,angle=15,code=2,length=0.1)
?plot
#asp ! the y/x asp ratio ! fait en sorte qu'une unité x= une unité y. ce qui est utile pour le cercle

# interprétation : à vous !


