# Clean the environment
rm(list = ls())

# Load the library
library(igraph)

# YouTube - Shared Subscrptions
data_1 = read.csv("YouTube-dataset/data/1-edges.csv")
data_2 = read.csv("YouTube-dataset/data/2-edges.csv")
data_3 = read.csv("YouTube-dataset/data/3-edges.csv")
#data_4 = read.csv("YouTube-dataset/data/4-edges.csv")
data_5 = read.csv("YouTube-dataset/data/5-edges.csv")

# Graph Objects
g_youTube_1 = graph.data.frame(data_1, directed = FALSE)
youTube_1 = delete.vertices(g_youTube_1, which(degree(g_youTube_1)<=30))

g_youTube_2 = graph.data.frame(data_2, directed = FALSE)
youTube_2 = delete.edges(g_youTube_2, which(E(g_youTube_2)$Weight < 7 ))
youTube_2 = delete.vertices(youTube_2, which(degree(youTube_2)<=20))

g_youTube_3 = graph.data.frame(data_3, directed = FALSE)
youTube_3 = delete.edges(g_youTube_3, which(E(g_youTube_3)$Weight < 12 ))
youTube_3 = delete.vertices(youTube_3, which(degree(youTube_3)<=30))

g_youTube_5 = graph.data.frame(data_5, directed = FALSE)
youTube_5 = delete.edges(g_youTube_5, which(E(g_youTube_5)$Weight < 25 ))
youTube_5 = delete.vertices(youTube_5, which(degree(youTube_5)<=7))

######################################################################
# Cliques - Network 1 ####
######################################################################
cliques_1 = table(sapply(maximal.cliques(youTube_1), length))
bmp("Clique Freq 1.jpg", 512, 512)
plot(cliques_1, type = 'l', xlab = "Cliques", ylab = "Frequency",
     main = "Clique Frequency", col = "red")
dev.off()

######################################################################
# Cliques - Network 2 ####
######################################################################
cliques_2 = table(sapply(maximal.cliques(youTube_2), length))
bmp("Clique Freq 2.jpg", 512, 512)
plot(cliques_2, type = 'l', xlab = "Cliques", ylab = "Frequency",
     main = "Clique Frequency", col = "red")
dev.off()

######################################################################
# Cliques - Network 3 ####
######################################################################
cliques_3 = table(sapply(maximal.cliques(youTube_3), length))
bmp("Clique Freq 3.jpg", 512, 512)
plot(cliques_3, type = 'l', xlab = "Cliques", ylab = "Frequency",
     main = "Clique Frequency", col = "red")
dev.off()

######################################################################
# Cliques - Network 5 ####
######################################################################
cliques_5 = table(sapply(maximal.cliques(youTube_5), length))
bmp("Clique Freq 5.jpg", 512, 512)
plot(cliques_5, type = 'l', xlab = "Cliques", ylab = "Frequency",
     main = "Clique Frequency ", col = "red")
dev.off()