library(igraph)
library(plyr)
library(ggplot2)
library(plotly)

loc1 <- read.csv("C:\\Users\\ganeshl\\Downloads\\sy_consup_sic4desc_fortboth_loc_sample.txt",sep="\t")
loc1$X <- NULL
locV11 <- loc1[,c("supplier_sic4_desc","consumer_sic4_desc")]
locV11$consumer_sic4_desc <- gsub(pattern = ",", replacement= " ", locV11$consumer_sic4_desc)
locV11$supplier_sic4_desc <- gsub(pattern = ",", replacement= " ", locV11$supplier_sic4_desc)
locV11$consumer_sic4_desc <- gsub(pattern = " ", replacement= "-", locV11$consumer_sic4_desc)
locV11$supplier_sic4_desc <- gsub(pattern = " ", replacement= "-", locV11$supplier_sic4_desc)
locV11$consumer_sic4_desc<-tolower(locV11$consumer_sic4_desc)
locV11$supplier_sic4_desc<-tolower(locV11$supplier_sic4_desc)
names(locV11)<- c("Source","Target")
sup <- locV11[("Source")]
con <- locV11[("Target")]
names(sup)<-"name"
names(con)<-"name"
sup_con <- rbind(sup,con)
sup_con <- unique(sup_con)
#locV11 <- unique(locV11)
write.csv(locV11, "C:\\Users\\ganeshl\\Downloads\\location11.csv")
write.csv(sup_con, "C:\\Users\\ganeshl\\Downloads\\location11nodes.csv")
g <- graph.data.frame(d=locV11,directed=TRUE,vertices=sup_con)
saveRDS(g,file = "communitygraph")
summary(g)
plot(g)

set.seed(5)
wc <- cluster_walktrap(g, steps = 7)
modularity(wc)
membership(wc)
communities(wc)
plot(vertex.color=membership(wc),g,edge.arrow.size=0.5,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.size=5)
diameter(g)

set.seed(5)
imc <- cluster_infomap(g)
membership(imc)
communities(imc)
modularity(imc)
plot(vertex.color=membership(imc),g,edge.arrow.size=0.5,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.size=5)

set.seed(5)
kc <- cluster_edge_betweenness(g,bridges = TRUE)
membership(kc)
communities(kc)
modularity(kc)
plot(vertex.color=membership(kc),g,edge.arrow.size=0.5,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.size=5)

#fast greedy, louvain only for undirected graph
set.seed(5)
kf <- cluster_leading_eigen(g)
membership(kf)
communities(kf)
modularity(kf)
plot(vertex.color=membership(kf),g,edge.arrow.size=0.5,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.size=5)

set.seed(5)
m <- cluster_label_prop(g)
membership(m)
communities(m)
modularity(m)
plot(vertex.color=membership(m),g,edge.arrow.size=0.5,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.size=5)

set.seed(5)
s <- cluster_spinglass(g)
membership(s)
communities(s)
modularity(s)
plot(vertex.color=membership(s),g,edge.arrow.size=0.5,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.size=5)

membership_spinglass <- as.data.frame.vector(membership(s))
membership_spinglass$Names <- rownames(membership_spinglass)
names(membership_spinglass) <- c("Membership","Names")
rownames(membership_spinglass)<- NULL
membership_spinglass$ID <- seq(from=1, to=(nrow(membership_spinglass)))
membership_spinglass<-membership_spinglass[,c("ID","Names","Membership")]
locV112 <- merge(locV11,membership_spinglass,by.x="Source",by.y="Names")
colnames(locV112) <- c("Source.Names","Target.Names","Source","Source.Membership")
locV112 <- locV112[,c("Source","Source.Names","Target.Names","Source.Membership")]
locV112 <- merge(locV112,membership_spinglass,by.x="Target.Names",by.y="Names")
colnames(locV112) <- c("Target.Names","Source","Source.Names","Source.Membership","Target","Target.Membership")
locV112 <- locV112[,c("Source","Target","Source.Names","Target.Names","Source.Membership","Target.Membership")]


g1 <- graph.data.frame(d=locV112, directed = TRUE,vertices = membership_spinglass)


#get the node coordinates
plotcord <- data.frame(layout.fruchterman.reingold(g1))
colnames(plotcord) = c("X1","X2")
plotcord$ID <- rownames(plotcord)
plotcord <- merge(plotcord,membership_spinglass,by.x="ID",by.y="ID")

#get edges, which are pairs of node IDs
edgelist <- get.edgelist(g1)

#convert to a four column edge data frame with source and destination coordinates
edges <- data.frame(plotcord[edgelist[,1],], plotcord[edgelist[,2],])
colnames(edges)[2:3] <- c("X1","Y1")
colnames(edges)[7:8] <- c("X2","Y2")
rownames(edges) <- NULL
plotcord$Membership <- as.factor(plotcord$Membership)
mem5 <- data.frame(membership_spinglass[which(membership_spinglass$Membership==5),])
mem4 <- data.frame(membership_spinglass[which(membership_spinglass$Membership==4),])
mem3 <- data.frame(membership_spinglass[which(membership_spinglass$Membership==3),])
mem2 <- data.frame(membership_spinglass[which(membership_spinglass$Membership==2),])
mem1 <- data.frame(membership_spinglass[which(membership_spinglass$Membership==1),])

dim(membership_spinglass[membership_spinglass$Membership==1, ])


resize.win <- function(Width=n, Height=n)
{
  # works for windows
  dev.off(); # dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height)
}

resize.win(15,10)
p <- ggplot() + geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), data=edges, size = 0.5, colour="grey") + geom_point(data=plotcord,aes(X1, X2,color=Membership,text=plotcord$Names))
ggplotly(p)%>%layout(legend = list(x = 100, y = 0.5))

memV1 <- mem1
memV1$Membership<-NULL
g2 <- induced.subgraph(graph=g,vids = memV1$ID)

cliques_mem1 = table(sapply(maximal.cliques(g2), length))
cliques_mem1
plot(cliques_mem1, type = 'l', xlab = "Cliques", ylab = "Frequency",
     main = "Clique Frequency Of Membership 1", col = "red")
clique_seg_mem1 <- maximal.cliques(g2,14,14)
clique_seg_mem1 <- lapply(clique_seg_mem1,as_ids)
clique_seg_mem1 <- lapply(clique_seg_mem1,as.data.frame)
clique_seg_mem1 <- lapply(clique_seg_mem1,setNames,"name")
memV1$Color1 <- as.numeric(memV1$Names %in% clique_seg_mem1[[1]]$name)
memV1$ID <- NULL
rownames(memV1)<-NULL
write.csv(memV1,"C:\\Users\\ganeshl\\Downloads\\colmem1.csv")


memV2 <- mem2
memV2$Membership<-NULL
g3 <- induced.subgraph(graph=g,vids = memV2$ID)

cliques_mem2 = table(sapply(maximal.cliques(g3), length))
cliques_mem2
plot(cliques_mem2, type = 'l', xlab = "Cliques", ylab = "Frequency",
     main = "Clique Frequency Of Membership 2", col = "red")
clique_seg_mem2 <- maximal.cliques(g3,3,3)
clique_seg_mem2 <- lapply(clique_seg_mem2,as_ids)
clique_seg_mem2 <- lapply(clique_seg_mem2,as.data.frame)
clique_seg_mem2 <- lapply(clique_seg_mem2,setNames,"name")
memV2$Color1 <- as.numeric(memV2$Names %in% clique_seg_mem2[[1]]$name)
memV2$ID <- NULL
rownames(memV2)<-NULL
write.csv(memV2,"C:\\Users\\ganeshl\\Downloads\\colmem2.csv")


memV3 <- mem3
memV3$Membership<-NULL
g4 <- induced.subgraph(graph=g,vids = memV3$ID)

cliques_mem3 = table(sapply(maximal.cliques(g4), length))
cliques_mem3
plot(cliques_mem3, type = 'l', xlab = "Cliques", ylab = "Frequency",
     main = "Clique Frequency Of Membership 3", col = "red")
clique_seg_mem3 <- maximal.cliques(g4,13,13)
clique_seg_mem3 <- lapply(clique_seg_mem3,as_ids)
clique_seg_mem3 <- lapply(clique_seg_mem3,as.data.frame)
clique_seg_mem3 <- lapply(clique_seg_mem3,setNames,"name")
clique_seg_mem3
memV3$Color3 <- as.numeric(memV3$Names %in% clique_seg_mem3[[1]]$name)
memV3$ID <- NULL
rownames(memV3)<-NULL
write.csv(memV3,"C:\\Users\\ganeshl\\Downloads\\colmem3.csv")


memV4 <- mem4
memV4$Membership<-NULL
g5 <- induced.subgraph(graph=g,vids = memV4$ID)

cliques_mem4 = table(sapply(maximal.cliques(g5), length))
cliques_mem4
plot(cliques_mem4, type = 'l', xlab = "Cliques", ylab = "Frequency",
     main = "Clique Frequency Of Membership 4", col = "red")
clique_seg_mem4 <- maximal.cliques(g5,8,8)
clique_seg_mem4 <- lapply(clique_seg_mem4,as_ids)
clique_seg_mem4 <- lapply(clique_seg_mem4,as.data.frame)
clique_seg_mem4 <- lapply(clique_seg_mem4,setNames,"name")
memV4$Color1 <- as.numeric(memV4$Names %in% clique_seg_mem4[[1]]$name)
memV4$ID <- NULL
rownames(memV4)<-NULL
write.csv(memV4,"C:\\Users\\ganeshl\\Downloads\\colmem4.csv")



memV5 <- mem5
memV5$Membership<-NULL
g6 <- induced.subgraph(graph=g,vids = memV5$ID)

cliques_mem5 = table(sapply(maximal.cliques(g6), length))
cliques_mem5
plot(cliques_mem5, type = 'l', xlab = "Cliques", ylab = "Frequency",
     main = "Clique Frequency Of Membership 5", col = "red")
clique_seg_mem5 <- maximal.cliques(g6,12,12)
clique_seg_mem5 <- lapply(clique_seg_mem5,as_ids)
clique_seg_mem5 <- lapply(clique_seg_mem5,as.data.frame)
clique_seg_mem5 <- lapply(clique_seg_mem5,setNames,"name")
memV5$Color1 <- as.numeric(memV5$Names %in% clique_seg_mem5[[1]]$name)
memV5$ID <- NULL
rownames(memV5)<-NULL
write.csv(memV5,"C:\\Users\\ganeshl\\Downloads\\colmem5.csv")





