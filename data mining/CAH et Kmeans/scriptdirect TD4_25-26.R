#### script TD 4

## Q1
villesCR=scale(villes)
boxplot(villesCR)
distVilleCR=dist(villesCR)
round(distVilleCR,2)

#2 faire les cah : hclust + plot 

# single (avec d)
HclustVilleCR.single=hclust(distVilleCR,method="single")
unclass(HclustVilleCR.single) # pour afficher tous les résultats du hclust
plot(HclustVilleCR.single,hang=-1,main="lien simple + distance euclidienne",ylab="ultramétrique")
rect.hclust(HclustVilleCR.single,k=4)


# complete (avec d)
HclustVilleCR.complete=hclust(distVilleCR,method="complete")
plot(HclustVilleCR.complete,hang=-1,main="lien complet + distance euclidienne",ylab="ultramétrique")
HclustVilleCR.complete$height
unclass(HclustVilleCR.complete)
rect.hclust(HclustVilleCR.complete,k=4)

# ward.d2
HclustVilleCR.wardd2=hclust(distVilleCR,method="ward.D2")
plot(HclustVilleCR.wardd2,hang=-1,main="Ward + distance euclidienne au carr?",ylab="ultramétrique")
rect.hclust(HclustVilleCR.wardd2,k=4)

# trois graphiques
par(mfrow=c(1,3))
plot(HclustVilleCR.single,hang=-1,main="lien simple + distance euclidienne",ylab="ultramétrique")
plot(HclustVilleCR.complete,hang=-1,main="lien complet + distance euclidienne",ylab="ultramétrique")
plot(HclustVilleCR.wardd2,hang=-1,main="Ward + distance euclidienne au carr?",ylab="ultramétrique")

# NbClust

villescr.ch.single=NbClust(data = villesCR, diss = NULL, distance = "euclidean", min.nc = 2,  max.nc = 15, method = "single", index = "ch")
villescr.ch.complete=NbClust(data = villesCR, diss = NULL, distance = "euclidean", min.nc = 2,  max.nc = 15, method = "complete", index = "ch")
villescr.ch.ward2=NbClust(data = villesCR, diss = NULL, distance = "euclidean", min.nc = 2,  max.nc = 15, method = "ward.D2", index = "ch")
par(mfrow=c(1,1))

plot(2:15,villescr.ch.ward2$All.index,type="l")
abline(v=4)

# récupérer les groupes du Hclust
groupeWard4=cutree(HclustVilleCR.wardd2,k=4)

villescr.ch.single=NbClust(data = villesCR, diss = NULL, distance = "euclidean", min.nc = 2,  max.nc = 15, method = "single", index = "ch")
villescr.ch.single$Best.nc

villescr.ch.complete=NbClust(data = villesCR, diss = NULL, distance = "euclidean", min.nc = 2,  max.nc = 15, method = "complete", index = "ch")
villescr.ch.complete$Best.nc
villescr.ch.ward2=NbClust(data = villesCR, diss = NULL, distance = "euclidean", min.nc = 2,  max.nc = 15, method = "ward.D2", index = "ch")

#3 CAH sur les composantes de l'ACP CR
#classification sur les 2 premi?res composantes de l'acp 
par(mfrow=c(1,1))
distACPvillesCR=dist(resACPvillesCR$scores[,1:2])
HclustACPvillescr=hclust(distACPvillesCR,method="ward.D2")
plot(HclustACPvillescr,hang =-1,main="Ward sur les 2 premieres composantes de l'ACP")
abline(h=8,col="red")

groupeScore=cutree(HclustACPvillescr,k=4)

# représentation des groupes sur le premier plan de l'acp
par(mfrow=c(1,3))
plot(resACPvillesCR$scores[,1:2],xlab="PC1",pch=16,col=groupeScore,ylab="PC2",main="observations - groupes CAH sur scores")
text(resACPvillesCR$scores[,1:2],pos=4,labels=rownames(villes),cex=0.9)
abline(h=0,v=0,lty=2,col="grey")

# graphique avec groupe Ward sur données complètes
plot(resACPvillesCR$scores[,1:2],pch=16,xlab="PC1",col=groupeWard4,ylab="PC2",main="observations- groupe CAH données initiales")
text(resACPvillesCR$scores[,1:2],labels=rownames(villes),cex=0.9,pos=4)
abline(h=0,v=0,lty=2,col="grey")

#par(mfrow=c(1,1))
angle=seq(0,2*pi,length=1000)
plot(cos(angle),sin(angle),type="l",xlab="PC1",ylab="PC2",main="variables")
abline(h=0,v=0,lty=2,col="grey")
matcorVillesCP=cor(villes,resACPvillesCR$scores[,1:2])
text(matcorVillesCP,labels=colnames(villes),cex=0.9)
arrows(0,0,matcorVillesCP[,1]*0.95,matcorVillesCP[,2]*0.95,length = 0.1, angle = 15)

##### 4 heatmap : double dendogram

par(mfrow=c(1,2))
matcor <- cor(villes)
distvar <- as.dist(1-matcor^2)
reshclustvar <- hclust(distvar,method="ward.D2")
plot(reshclustvar,hang=-1)

heatmap(villesCR,Rowv=as.dendrogram(HclustVilleCR.wardd2),Colv=as.dendrogram(reshclustvar))

# 5 Kmeans
reskmeanVille4gr1=kmeans(villesCR,centers=4)
reskmeanVille4gr1$cluster
reskmeanVille4gr1$iter
reskmeanVille4gr1$size
reskmeanVille4gr2=kmeans(villesCR,centers=4)
reskmeanVille4gr2$size
reskmeanVille4gr3=kmeans(villesCR,centers=4)
reskmeanVille4gr3$size
reskmeanVille4gr3$cluster
reskmeanVille4gr3$tot.withins
reskmeanVille4gr3$withinss
table(reskmeanVille4gr1$cluster,reskmeanVille4gr3$cluster)

# question 6 calinski harabasz
CHkmeansVilles=NbClust(data=villesCR,distance="euclidean",method="kmeans",index="ch")

plot(2:15,CHkmeansVilles$All.index,type='l')
#-> 4 groupes

par(mfrow=c(1,1))
#boucle : pour voir la variation des variance intras. 
enrintra=NULL
for (i in 2:10){
  reskmloc=kmeans(villesCR,centers=i)
  enrintra=c(enrintra,reskmloc$tot.withinss)
  print(enrintra)
}
plot(2:10,enrintra)
abline(v=5)

## representation groupe sur premier plan de l'acp
groupeVilleKmean4=reskmeanVille4gr3$cluster
par(mfrow=c(1,1))
plot(resACPvillesCR$scores[,1:2],xlab="PC1",pch=16,col=groupeVilleKmean4,ylab="PC2",main="observations - groupes kmeans")
text(resACPvillesCR$scores[,1:2],pos=4,labels=rownames(villes),cex=0.9)
abline(h=0,v=0,lty=2,col="grey")

## question 7
par(mfrow=c(1,2))
plot(resACPvillesCR$scores[,1:2],xlab="PC1",pch=16,col=groupeVilleKmean4,ylab="PC2",main="observations - groupes kmeans")
text(resACPvillesCR$scores[,1:2],pos=4,labels=rownames(villes),cex=0.9)
abline(h=0,v=0,lty=2,col="grey")

plot(resACPvillesCR$scores[,1:2],pch=16,xlab="PC1",col=groupeWard4,ylab="PC2",main="observations- groupe CAH données initiales")
text(resACPvillesCR$scores[,1:2],labels=rownames(villes),cex=0.9,pos=4)
abline(h=0,v=0,lty=2,col="grey")

# presque les mêmes groupes. (4 villes ont changé de groupe)