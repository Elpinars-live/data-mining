#td5 LDA
#import fichier
paintings8 <- read.delim("~/data_mining/paintings8.txt", header=FALSE, row.names = 1)
View(paintings8)
paintings64 <- read.delim("~/data_mining/paintings64.txt", header=FALSE, row.names = 1)
View(paintings64)
#en matrice
paintings8 = as.matrix(paintings8)
paintings64 = as.matrix(paintings64)
#2 Explorer les données
summary(paintings8)
summary(paintings64)

boxplot(paintings8)
boxplot(paintings64)

image(paintings8)
image(paintings64)

pairs(paintings8)
pairs(paintings64)
#extraire initial as.factor
gp = as.factor(substr(rownames(paintings8),1,1))

#3. Appliquer une classification hiérarchique pour étudier les ressemblances entre 
# échantillons. En déduire la nécessité ou non d’une transformation des données.

#en s'en passe

#4Pourquoi faut-il supprimer une variable avant de lancer la méthode LDA ?
#car c'est pourcentage en veux avoir 100%

#5. Pour le jeu à 64 couleurs, éliminer les variables dont la variance est inférieure à 10-4
vecvar = apply(paintings64,2,sd)
paintings64sel = paintings64[,vecvar=0.0001]
paintings64sel = paintings64[,-8]


#librairie masse
library(MASS)
lda8 = lda(x=paintings8,grouping=gp) #enlever une données
lda64 = lda(x=paintings64,grouping=gp)
names(lda8)
names(lda64)

#6 Appliquer la méthode LDA sur les deux jeux de données. Combien d'axes obtient-on ? 
?predict
class(lda8)
?predict.lda
pred8 = predict(lda8,newdata= paintings8)
names(pred8)
pred8$class
#7 pourcentage de prédit
mean(pred8$class==gp)
sum((pred8$class==gp)/length(gp))
table(pred8$class,gp)

#donne 64
class(lda64)
pred64 = predict(lda64,newdata= paintings64)
pred64$class
#7 pourcentage de prédit
mean(pred64$class==gp)
sum((pred64$class==gp)/length(gp))
table(pred64$class,gp)

# 8 Représenter le plan discriminant 1-2 pour les deux jeux de données. Repérer les
# erreurs. En déduire, le meilleur jeu de données pour la discrimination. On ne
# travaillera que sur ce jeu pour la suite des questions
names(pred8)
plot(pred8$x[,1:2],type="n") # var discriminante dans x
abline(v=0,h=0)
text(pred8$x[,1:2],labels = gp)
colmol=rep("black",length(gp))
colmol[pred8$class!=gp]="red"
text(pred8$x[,1:2],labels=gp,col=colmol)

names(pred64)
plot(pred64$x[,1:2],type="n") # var discriminante dans x
abline(v=0,h=0)
text(pred64$x[,1:2],labels = gp)
colmol=rep("black",length(gp))
colmol[pred64$class!=gp]="red"
text(pred64$x[,1:2],labels=gp,col=colmol)
# 9. Représenter dans l'espace les trois axes discriminants (utilisation de la fonction 
# plot3d de la library(rgl)).
library(rgl)
cols <- rainbow(length(levels(gp)))
plot3d(pred8$x[,1:3], type="n")
text3d(pred8$x,texts=gp, col = cols[gp])

#pour voir les erreurs
plot3d(pred8$x[,1:3], type="n")
colmol=rep("black",length(gp))
colmol[pred8$class!=gp]="red"
text3d(pred8$x,texts=gp, col = colmol)

# 10. Calculer les corrélations entre les valeurs des couleurs et les deux premiers axes 
# discriminants et les représenter pour le premier plan. En déduire quelques couleurs
# candidates discriminantes et étudier leur distribution.
par(mfrow=c(1,2))
corvar=cor(paintings8,pred8$x[,1:2])

plot(corvar[,1],corvar[,2],xlim=c(-1,1),ylim=c(-1,1),xlab="PC1",ylab="PC2",main='représentation des variables painting8',type='n',asp=1) # asp utile sinon le cercle ci-après sort de la fenêtre
# ajout des noms des variables
text(corvar[,1],corvar[,2],labels=colnames(paintings8),cex=0.7,col='red')
# représentation du cercle :
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "blue", lty = 2)
# ajout des axes :
abline(h=0,v=0,lty=2)

#version donnes64
par(mfrow=c(1,2))
corvar=cor(paintings64,pred64$x[,1:2])

plot(corvar[,1],corvar[,2],xlim=c(-1,1),ylim=c(-1,1),xlab="PC1",ylab="PC2",main='représentation des variables painting8',type='n',asp=1) # asp utile sinon le cercle ci-après sort de la fenêtre
# ajout des noms des variables
text(corvar[,1],corvar[,2],labels=colnames(paintings8),cex=0.7,col='red')
# représentation du cercle :
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "blue", lty = 2)
# ajout des axes :
abline(h=0,v=0,lty=2)

# 11. Utiliser la fonction lda_CV (qui nécessite d'avoir sourcé la fonction constCV) pour 
# calculer le taux de bien classés de validation. 
source("~/data_mining/constCV.R")
source("~/data_mining/lda_CV.R")
blocs=constCV(gp,2)
lda_CV(paintings8,gp,2,100)
