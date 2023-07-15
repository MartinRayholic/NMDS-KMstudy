library(vegan)
#NMDS過程
set.seed(19990708)
fishmds <- metaMDS(sqrt(secndnmdspreUN[,-1]), distance = "bray")
plot(fishmds, type = "n")

orditorp(fishmds,display="species",col="red",air=0.01)
legend("topleft", "stress = 0", bty = "n", cex = 1)
orditorp(fishmds,display="sites",cex=1.25,air=0.01)
legend("bottomright",legend = c(secndnmdspreUN[,1]),pch=c(1:6),cex = 1,col = c(1:6),
       bty = "n")
points(fishmds, pch = c(1:6),col=(1:6))

#UMAP 過程
#UN = 軟骨魚類data
bray2.distance <- vegdist(sqrt(UN[,-1]), method = 'bray' )
distbray2 <- as.matrix(bray2.distance)
distbray2
row.names(distbray2)<- secndnmdspreUN$Group
colnames(distbray2)<- secndnmdspreUN$Group
set.seed(19990708)
umapfish_dis <- umap(distbray2, n_neighbors = 5,min_dist = 0.1)
umap_distance2 <- data.frame(x = umapfish_dis$layout[,1],
                             y = umapfish_dis$layout[,2],
                             Group = secndnmdspreUN$Group )

ggplot(umap_distance2, aes(x, y, colour = Group)) +
  geom_point(size=2.5)+theme_apa()+
  xlab("UMAP1")+ylab("UMAP2")+theme(legend.text = element_text(size = 14))