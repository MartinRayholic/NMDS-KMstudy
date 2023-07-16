library(vegan)
#NMDS過程
#autotransform = F
set.seed(19990708)
fishmds <- metaMDS(sqrt(secndnmdspreUN[,-1]), distance = "bray",k=2,trymax=10000,
                   autotransform = F)
plot(fishmds, type = "n")
orditorp(fishmds,display="species",col="red",air=0.01)
legend("topleft", "stress = 0", bty = "n", cex = 1)
orditorp(fishmds,display="sites",cex=1.25,air=0.01)
legend("bottomright",legend = c(secndnmdspreUN[,1]),pch=c(1:6),cex = 1,col = c(1:6),
       bty = "n")
points(fishmds, pch = c(1:6),col=(1:6), cex=5)


#UMAP 過程
#UN = 軟骨魚類data
library(umap)
bray2.distance <- vegdist(sqrt(secndnmdspreUN[,-1]), method = 'bray' )
distbray2 <- as.matrix(bray2.distance)
distbray2
row.names(distbray2)<- secndnmdspreUN$Group
colnames(distbray2)<- secndnmdspreUN$Group
set.seed(19990708)
umapfish_dis <- umap(distbray2, n_neighbors = 5,min_dist = 0.01)
umap_distance2 <- data.frame(x = umapfish_dis$layout[,1],
                             y = umapfish_dis$layout[,2],
                             Group = secndnmdspreUN$Group )

ggplot(umap_distance2, aes(x, y, colour = Group)) +
  geom_point(size=2.5)+theme_apa()+
  xlab("UMAP1")+ylab("UMAP2")+theme(legend.text = element_text(size = 14))

library(dbscan)
#NMDS hdbscan
dendnmds <- as.data.frame(scores(fishmds)$sites)#跟下面一樣的,$species會輸出所有物種點
dendnmds <- as.data.frame(scores(fishmds$points))
dendnmds
clusterhdb <- hdbscan(dendnmds,minPts =2)
clusterhdb
plot(dendnmds, col=clusterhdb$cluster+1,pch=16 ,cex = 2)
points(fishmds, pch = c(1:6), cex=2)
legend("topright",legend = c(secndnmdspreUN[,1]),pch=c(1:6),cex = 1,
       bty = "n")


plot(clusterhdb$h, labels =secndnmdspreUN[,1] )
plot(clusterhdb)

colhdb <- as.factor(clusterhdb$cluster+1)
ggplot(data=datascores, aes(x=MDS1,y=MDS2))+
  geom_point(aes(col=colhdb))

#UMAP hdbscan
bray2.distance <- vegdist(sqrt(secndnmdspreUN[,-1]), method = 'bray' )
distbray2 <- as.matrix(bray2.distance)
distbray2
row.names(distbray2)<- secndnmdspreUN$Group
colnames(distbray2)<- secndnmdspreUN$Group
set.seed(19990708)
umapfish_dis <- umap(distbray2, n_neighbors = 3,min_dist = 0.01)


dendumap <- data.frame(x = umapfish_dis$layout[,1],
                       y = umapfish_dis$layout[,2],
                       Group = secndnmdspreUN$Group)
dendumap
clusterhdbumap <- hdbscan(dendumap[,-3],minPts = 2)
clustercol <- as.factor(clusterhdbumap$cluster+1)
ggplot(dendumap, aes(x, y, colour = clustercol, label=secndnmdspreUN$Group)
       ,legend_title) +
  geom_point(size=2.5)+theme_apa()+
  geom_text(size=5,hjust = 0, nudge_x = 0.05)+
  theme(legend.text = element_text(size = 14))

plot(clusterhdbumap)
plot(clusterhdbumap$hc,labels =secndnmdspreUN[,1])



