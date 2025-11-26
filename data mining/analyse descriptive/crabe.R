#1 et 2 porjet créer
getwd()
#setwd() inutile si projet déjas créer

#3
source("qq_fonctions_TP1.txt")

#4
library(readr)
crabes <- read_table("crabes.txt")
View(crabes)
#5. Quel type d’objet obtenez-vous ? df 
class(crabes)

#6. Explorez ce jeu de données à l’aide de la fonction summary. Que remarquez-vous ? 
summary(crabes)
# ya 8 vars 200 lignes 2 vars char et 6 var int

#7. Réalisez un histogramme de la variable RW en effectifs et en probabilités, ajoutez 
#l’étiquette « RW » sur l’axe des abscisses.
hist(crabes$`"RW"`, xlab = "RW", main = "histograme de RW") #effectif
hist(crabes$`"RW"`, freq = FALSE, xlab = "RW", main = "histograme de RW") #probabilité
hist(crabes$`"RW"`, freq = FALSE, xlab = "RW", main = "histograme de RW", breaks = 12)

#8. Utilisez la fonction density pour réaliser une estimation de la densité sous-jacente et 
#représentez-la sur l’histogramme.  
d <- density(crabes$`"RW"`)
plot(d)

names(d)
hist(crabes$`"RW"`, freq = FALSE, xlab = "RW", main = "histograme de RW")
plot(d$x,d$y, type="l")

#la sa apparait sur le hist
hist(crabes$`"RW"`, freq = FALSE, xlab = "RW", main = "histograme de RW")
points(d,type="l",col=2,lwd=2,lty=3)

#9. Réalisez un boxplot de la variable RW.  
boxplot(crabes$`"RW"`)
#10 Représentez sur un même graphique les boxplots de toutes les variables 
#quantitatives. Commentez. 
boxplot(crabes[,4:8])

#11. Calculez la moyenne et l’écart-type de toutes les variables quantitatives (utilisation 
#de la fonction apply).  
moy = apply(crabes[,4:8],2,mean) # apply(x,ligne=1/colone=2,fonction a appliqué ici mean)
sd = apply(crabes[,4:8],2,sd)
#différence tapply sur groupe apply sur colonne ou ligne

# 12. Réalisez un centrage/réduction du jeu de données à l’aide de la fonction scale. 
# Observez graphiquement l’effet de cette transformation. Commentez.
#centrage reduction avec scale
#centrage on retire la moyenne
crabecentre = scale(crabes[,4:8],center=TRUE,scale=FALSE) #centré non-réduit
View(crabecentre)
boxplot(crabecentre)
#maintenant on réduit par /écartype
crabecentrereduit = scale(crabes[,4:8],center=TRUE,scale=TRUE) #centré + réduit
boxplot(crabecentrereduit)
#centrage réduction utile pour évité qu'une variable soit trop impactante à cause de son échelle

#13. Utilisez la fonction cor pour calculer la matrice de corrélation entre les variables 
#quantitatives. 
matcor = cor(crabes[,4:8])
matcor

#14. Utilisez la fonction image pour représenter cette matrice.
image(matcor)
#15. Utilisez la fonction pairs pour représenter les informations de cette matrice.  
pairs(crabes[,4:8])

#16. Appliquez le code suivant et commentez code suivant:  
pairs(crabes[,4:8],lower.panel=panel.smooth,upper.panel=panel.cor,diag.panel=panel.hist) 

# #17. Enregistrer le jeu de données centrées-réduites à l’aide de la fonction save, 
# supprimez l’objet R de votre environnement (fonction rm et ls pour vérifier) et 
# rechargez-le dans R à l’aide de la fonction load.
## on peut save l'environnement
save.image("nomrandom.Rdata") #faut un d majuscule au cas ou sa bug
#vider environnement travail:
#rm(list=ls())
## charger un environnement
load("nomrandom.Rdata")

## sauver un objet
save(crabecentrereduit, file = "crabeCR.Rdata")
#suppression un objet
rm(crabecentrereduit)
#load objet
load("crabeCR.Rdata")

###########
#Les données de Fisher :  
############

#1. Importez le fichier Fishertot.txt.  
fisher = read.table("Fishertot.txt.",header = TRUE, sep=";")
View(fisher)
summary(fisher)

#2. Utilisez la fonction table pour décrire la distribution de chaque variable.  
table(fisher$col_yeux)
table(fisher$col_cheveux)
#exemple avec apply
apply(fisher,2,table)

#3. Avec la même fonction, construire la table de contingence croisant ces deux 
#variables.  
table(fisher$col_yeux,fisher$col_cheveux)

#4.  A l’aide de la fonction which.max, identifier le mode de chaque variable. 
which.max(table(fisher$col_yeux))
which.max(table(fisher$col_cheveux))

#5. Utilisez la fonction barplot pour représenter graphiquement la distribution de la 
#variable « couleur des yeux ». 
barplot(table(fisher$col_yeux))

#6. Appliquez la fonction barplot à la table de contingence obtenue précédemment. Que 
#représentez-vous ? Ajoutez une légende. 
barplot(table(fisher$col_yeux,fisher$col_cheveux),col=rainbow(5))
legend("topleft",legend=names(table(fisher$col_yeux)),fill=rainbow(5))
help(legend)

#7. Représentez la distribution des couleurs de cheveux en fonction de la couleur des 
#yeux.
barplot(table(fisher$col_cheveux,fisher$col_yeux))
#8
barplot(table(fisher$col_cheveux,fisher$col_yeux),beside = TRUE)
#9. Utilisez la fonction pie pour représenter la distribution de la variable « couleur des 
#cheveux ». 
pie(table(fisher$col_cheveux))
#10. Utilisez la fonction text_pie pour ajouter les pourcentages de chaque catégorie. 
text_pie(table(fisher$col_cheveux))