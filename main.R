## 1 Exploration de données
mens<-read.table("MEASURE.txt")
boxplot(mens)

# Normalisation des mesures
#mens<-scale(mens)
#boxplot(mens)

## 2 Partitionnement KMeans
mens.class<-kmeans(mens,3)
mens.class
# Application de kmeans => 3 Cluster de taille 9,2,9
mens.hclust<-hclust(dist(mens.class$centers),method="ward")
mens.hclust
plot(mens.hclust)
# On trace le dendrogramme obtenu et on observe très clairement que 2 clusters sont
# Très proches et qu'il n'y a au final que deux classes différentes (même si on le savait d'avance : Homme/Femme)

mens.class<-kmeans(mens,2)
mens.class
# Application de Kmeans avec K=2

plot(mens[1:2],col=mens.class$cluster)
points(mens.class$centers, col = 1:2, pch = 8)
text(mens[1:2],attributes(mens)$row.names,col=mens.class$cluster,pos=1)
# Plot de chest/waist (1,2)

## 3 Classification Hierarchique
D <- dist(mens)
res <- hclust(D,method="ward")
plot(res,hang = -1)

res2 <- cutree(res, k = 5)
res2
# Assigne un cluster a chaque élément

cent <- NULL
for(k in 1:5) {cent<-rbind(cent,colMeans(mens[res2 == k,, drop=FALSE]))}
cent

res3<-hclust(dist(cent)^2,method="cen",members=table(res2))

res4 <- par(mfrow = c(1,2))
plot(res,labels=FALSE,hang=-1,main="Original Tree")
plot(res3,labels=FALSE,hang=-1,main="Restart from 5 clusters")
par(res4)