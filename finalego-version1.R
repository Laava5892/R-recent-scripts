
getwd()
#Set current working directory
setwd("C:\\Users\\ganeshl\\OneDrive - Dun & Bradstreet\\Desktop\\Network Dependcies") 
library(igraph) #load igraph package
library(plyr)   #load plyr package

#Get nodes and edges

nodes<-read.csv("C:\\Users\\ganeshl\\OneDrive - Dun & Bradstreet\\Desktop\\Network Dependcies\\N113.csv") #read the nodes file
edges<-read.csv("C:\\Users\\ganeshl\\OneDrive - Dun & Bradstreet\\Desktop\\Network Dependcies\\E12.csv")  #read the edges file

#get normalized scores in a column of nodes file
nodes$normalized <- (nodes$Scores- min(nodes$Scores,na.rm = TRUE))/(max(nodes$Scores,na.rm = TRUE)-min(nodes$Scores,na.rm = TRUE))

#get a indicator for missing scores
nodes$missingIndicator <- ifelse(is.na(nodes$normalized),1,0)

# generate a graph from the nodes and edges file
g <- graph.data.frame(d=edges, directed = TRUE, vertices=nodes)

#get eigenvalue for each node
V(g)$eigen <- evcent(g)$vector

#get out-degree for each node
V(g)$outdeg <- degree(g, mode="out")

#get in-degree for each node
V(g)$indeg <- degree (g, mode="in")

#get betweenness for each node
V(g)$nodebetweenness <- betweenness(g, v=V(g),directed = TRUE)

#Calculate the cyber score : Normalized score * Eigen value of the node
V(g)$NodeCyberScore <- V(g)$normalized*V(g)$eigen

#get betweenness for each edge
E(g)$edgebetweenness <- edge.betweenness(g,e=E(g),directed=TRUE)


# select the egonode, order of neighbours for egonode, mode 
egonode <-"6250740"
Oorder <-3
Mmode <- "out"
egonodes <- ego(g,Oorder,nodes = (which(V(g)$name==egonode)),mode = Mmode) #get the nodes as part of the ego network 

# subset graph based on egonode
g1 <- induced_subgraph(g,egonodes[[1]]$name)
summary(g1)

#get nodes attributes and edgelist of ego graph
nodesego <- as.data.frame.list(get.vertex.attribute(g1),stringsAsFactors = FALSE)
edgesego <- as.data.frame.matrix(get.edgelist(g1), stringsAsFactors = FALSE)
names(edgesego) <- c("Source", "Target")

#get all shortest paths for ego-node
allshortestpaths = (all_shortest_paths(g1, from = egonode, to= V(g1),mode = "out"))$res

#convert vertex sequence to character vectors for further matching
allshortestpaths <- lapply(allshortestpaths, as_ids)

# create empty list and data frame for future use
bpathproperties  <- list()
bpathproperties1 <- list()
bpathproperties2 <- list()
bpathproperties3 <- list()
bpathproperties4 <- list()
bpathproperties5 <- list()
dpathscore<- data.frame()

# get the node names, missing indicators and their sum, normalized scores, product of normalized scores considering and not considering NA values,  sum of (scores *eigen) for each path
for(i in 1:length(allshortestpaths))
{
  
  allshortestpaths[[i]] <- c(list(V(g)[match(allshortestpaths[[i]], V(g)$name)]$name, V(g)[match(allshortestpaths[[i]], V(g)$name)]$missingIndicator, sum(V(g)[match(allshortestpaths[[i]], V(g)$name)]$missingIndicator),V(g)[match(allshortestpaths[[i]], V(g)$name)]$normalized, prod(V(g)[match(allshortestpaths[[i]], V(g)$name)]$normalized, na.rm = TRUE), prod(V(g)[match(allshortestpaths[[i]], V(g)$name)]$normalized, na.rm = FALSE),V(g)[match(allshortestpaths[[i]], V(g)$name)]$NodeCyberScore, sum(V(g)[match(allshortestpaths[[i]], V(g)$name)]$NodeCyberScore, na.rm = TRUE)))
  allshortestpaths[[i]][[8]] <- as.data.frame.list(allshortestpaths[[i]][[8]])
  colnames(allshortestpaths[[i]][[8]])[1] <- "TotalCyberScoreWithoutConsideringNA"
}


# data frame with all unique total cyber score of each path
for(i in 1:length(allshortestpaths))
{
  dpathscore[i,1] <- unique(allshortestpaths[[i]][[8]]$TotalCyberScoreWithoutConsideringNA) # get the scores of each path into a data frame
}
names(dpathscore) <- "TotalCyberScoreWithoutConsideringNA"
dpathscore <- data.frame(dpathscore[rev(order(dpathscore$TotalCyberScoreWithoutConsideringNA)),])
head(dpathscore)
names(dpathscore) <- "TotalCyberScoreWithoutConsideringNA"

#Function to get any path and its properties
#In a list B,
#B[[1]]: character vector of all nodes in the path
#B[[2]]: logical numeric vector for each node in the path  indicating whether the node has NA values
#B[[3]]: sum of all the NA values
#B[[4]]: Normalized scores of the nodes in the path
#B[[5]]: product of normalized scores in a path considering NA values
#B[[6]]: product of normalized scores in a path without considering NA values
#B[[7]]: NodeCyberScore (normalized*eigen) for all node in the path
#B[[8]]: sum of NodeCyberScore
#B[[9]]: Source and Target form

FuncGetPath <- function(n,allshortestpaths,dpathscore)
{
  
  if(n==1)
  {
  for(i in 1:length(allshortestpaths))
  {
    if(unique(allshortestpaths[[i]][[8]]$TotalCyberScoreWithoutConsideringNA==dpathscore[n,1])) # get the top scoring path into a data frame 
    { bpathproperties1 <- (allshortestpaths[[i]])
    }
  }
  
  #Convert Nodes of a path to character
  bpathproperties1[[9]]<- as.data.frame.character(bpathproperties1[[1]])
  names(bpathproperties1[[9]]) <- "Source"
  bpathproperties1[[9]]$Source <- as.character(bpathproperties1[[9]]$Source)
  
  # Get target of each node for visualization
  c <- 1
  for( i in 1:nrow(bpathproperties1[[9]]))
  {
    bpathproperties1[[9]]$Target[i] <- bpathproperties1[[9]][(c+1),1] 
    c<-c+1
    
  }
  bpathproperties1[[9]]$Together<- paste(bpathproperties1[[9]]$Source, bpathproperties1[[9]]$Target, sep="|")
  
  assign("bpathproperties1",bpathproperties1,envir=.GlobalEnv)
  }
  else if(n==2)
  {
    for(i in 1:length(allshortestpaths))
    {
      if(unique(allshortestpaths[[i]][[8]]$TotalCyberScoreWithoutConsideringNA==dpathscore[n,1])) # get the top scoring path into a data frame 
      { bpathproperties2 <- (allshortestpaths[[i]])
      }
    }
    
    #Convert Nodes of a path to character
    bpathproperties2[[9]]<- as.data.frame.character(bpathproperties2[[1]])
    names(bpathproperties2[[9]]) <- "Source"
    bpathproperties2[[9]]$Source <- as.character(bpathproperties2[[9]]$Source)
    
    # Get target of each node for visualization
    c <- 1
    for( i in 1:nrow(bpathproperties2[[9]]))
    {
      bpathproperties2[[9]]$Target[i] <- bpathproperties2[[9]][(c+1),1] 
      c<-c+1
      
    }
    bpathproperties2[[9]]$Together<- paste(bpathproperties2[[9]]$Source, bpathproperties2[[9]]$Target, sep="|")
    
    assign("bpathproperties2",bpathproperties2,envir=.GlobalEnv)
    
  }
  else  if(n==3)
  {
    for(i in 1:length(allshortestpaths))
    {
      if(unique(allshortestpaths[[i]][[8]]$TotalCyberScoreWithoutConsideringNA==dpathscore[n,1])) # get the top scoring path into a data frame 
      { bpathproperties3 <- (allshortestpaths[[i]])
      }
    }
    
    #Convert Nodes of a path to character
    bpathproperties3[[9]]<- as.data.frame.character(bpathproperties3[[1]])
    names(bpathproperties3[[9]]) <- "Source"
    bpathproperties3[[9]]$Source <- as.character(bpathproperties3[[9]]$Source)
    
    # Get target of each node for visualization
    c <- 1
    for( i in 1:nrow(bpathproperties3[[9]]))
    {
      bpathproperties3[[9]]$Target[i] <- bpathproperties3[[9]][(c+1),1] 
      c<-c+1
      
    }
    bpathproperties3[[9]]$Together<- paste(bpathproperties3[[9]]$Source, bpathproperties3[[9]]$Target, sep="|")
    
    assign("bpathproperties3",bpathproperties3,envir=.GlobalEnv)
  }
  else  if(n==4)
  {
    for(i in 1:length(allshortestpaths))
    {
      if(unique(allshortestpaths[[i]][[8]]$TotalCyberScoreWithoutConsideringNA==dpathscore[n,1])) # get the top scoring path into a data frame 
      { bpathproperties4 <- (allshortestpaths[[i]])
      }
    }
    
    #Convert Nodes of a path to character
    bpathproperties4[[9]]<- as.data.frame.character(bpathproperties4[[1]])
    names(bpathproperties4[[9]]) <- "Source"
    bpathproperties4[[9]]$Source <- as.character(bpathproperties4[[9]]$Source)
    
    # Get target of each node for visualization
    c <- 1
    for( i in 1:nrow(bpathproperties4[[9]]))
    {
      bpathproperties4[[9]]$Target[i] <- bpathproperties4[[9]][(c+1),1] 
      c<-c+1
      
    }
    bpathproperties4[[9]]$Together<- paste(bpathproperties4[[9]]$Source, bpathproperties4[[9]]$Target, sep="|")
    
    assign("bpathproperties4",bpathproperties4,envir=.GlobalEnv)
  }
  else  if(n==5)
  {
    for(i in 1:length(allshortestpaths))
    {
      if(unique(allshortestpaths[[i]][[8]]$TotalCyberScoreWithoutConsideringNA==dpathscore[n,1])) # get the top scoring path into a data frame 
      { bpathproperties5 <- (allshortestpaths[[i]])
      }
    }
    
    #Convert Nodes of a path to character
    bpathproperties5[[9]]<- as.data.frame.character(bpathproperties5[[1]])
    names(bpathproperties5[[9]]) <- "Source"
    bpathproperties5[[9]]$Source <- as.character(bpathproperties5[[9]]$Source)
    
    # Get target of each node for visualization
    c <- 1
    for( i in 1:nrow(bpathproperties5[[9]]))
    {
      bpathproperties5[[9]]$Target[i] <- bpathproperties5[[9]][(c+1),1] 
      c<-c+1
      
    }
    bpathproperties5[[9]]$Together<- paste(bpathproperties5[[9]]$Source, bpathproperties5[[9]]$Target, sep="|")

    assign("bpathproperties5",bpathproperties5,envir=.GlobalEnv)
  }
  else
  {
    for(i in 1:length(allshortestpaths))
    {
      if(unique(allshortestpaths[[i]][[8]]$TotalCyberScoreWithoutConsideringNA==dpathscore[n,1])) # get the top scoring path into a data frame 
      { bpathproperties <- (allshortestpaths[[i]])
      }
    }
    
    #Convert Nodes of a path to character
    bpathproperties[[9]]<- as.data.frame.character(bpathproperties[[1]])
    names(bpathproperties[[9]]) <- "Source"
    bpathproperties[[9]]$Source <- as.character(bpathproperties[[9]]$Source)
    
    # Get target of each node for visualization
    c <- 1
    for( i in 1:nrow(bpathproperties[[9]]))
    {
      bpathproperties[[9]]$Target[i] <- bpathproperties[[9]][(c+1),1] 
      c<-c+1
      
    }
    bpathproperties[[9]]$Together<- paste(bpathproperties[[9]]$Source, bpathproperties[[9]]$Target, sep="|")
    
    assign("bpathproperties",bpathproperties,envir=.GlobalEnv)
  }
    
}

FuncGetPath(5,allshortestpaths,dpathscore) #Path 1

#print the number of NA values for the path
paste(c("The number of missing values in the path :"), bpathproperties[[3]])

# Create membership group and weights for nodes and edges for a particular path for visualization
FuncMemandWeight <- function(n)
{
  edgesego$Together<- paste(edgesego$Source, edgesego$Target, sep="|")
  if(n==1)
  {
    #edge membership
    edgesego$Group1 <- as.numeric(edgesego$Together %in% bpathproperties1[[9]]$Together)
    #node membership
    nodesego$Group1<- as.numeric(nodesego$name %in% bpathproperties1[[9]]$Source)
    f <- 7    #counter
    edgesego$Weight1 <- NA   #creating empty weight column
    # assign edge weights from 8px to each edge in the path
    for( i in 1: (nrow(bpathproperties1[[9]])-1))
    {
      edgesego[which(edgesego$Together==bpathproperties1[[9]][i,3]),]$Weight1 <- (f+1)
      f<-f+1
    }
    
    #replace the NAs in the weight column with 1
    edgesego$Weight1[is.na(edgesego$Weight1)] <- 1
  }
  else if(n==2)  #Path 2 same logic
  {
    edgesego$Group2 <- as.numeric(edgesego$Together %in% bpathproperties2[[9]]$Together)
    nodesego$Group2<- as.numeric(nodesego$name %in% bpathproperties2[[9]]$Source) 
    f <- 7
    edgesego$Weight2 <- NA
    for( i in 1: (nrow(bpathproperties2[[9]])-1))
    {
      edgesego[which(edgesego$Together==bpathproperties2[[9]][i,3]),]$Weight2 <- (f+1)
      f<-f+1
    }
    edgesego$Weight2[is.na(edgesego$Weight2)] <- 1
  }
  else  if(n==3)  # Path 3 same logic
  {
    edgesego$Group3 <- as.numeric(edgesego$Together %in% bpathproperties3[[9]]$Together)
    nodesego$Group3<- as.numeric(nodesego$name %in% bpathproperties3[[9]]$Source)
    f <- 7
    edgesego$Weight3 <- NA
    for( i in 1: (nrow(bpathproperties3[[9]])-1))
    {
      edgesego[which(edgesego$Together==bpathproperties3[[9]][i,3]),]$Weight3 <- (f+1)
      f<-f+1
    }
    edgesego$Weight3[is.na(edgesego$Weight3)] <- 1
  }
  else  if(n==4) # path 4 same logic
  {
    edgesego$Group4 <- as.numeric(edgesego$Together %in% bpathproperties4[[9]]$Together)
    nodesego$Group4<- as.numeric(nodesego$name %in% bpathproperties4[[9]]$Source) 
    f <- 7
    edgesego$Weight4 <- NA
    for( i in 1: (nrow(bpathproperties4[[9]])-1))
    {
      edgesego[which(edgesego$Together==bpathproperties4[[9]][i,3]),]$Weight4 <- (f+1)
      f<-f+1
    }
    edgesego$Weight4[is.na(edgesego$Weight4)] <- 1
  }
  else  if(n==5) # Path 5 same logic
  {
    edgesego$Group5 <- as.numeric(edgesego$Together %in% bpathproperties5[[9]]$Together)
    nodesego$Group5<- as.numeric(nodesego$name %in% bpathproperties5[[9]]$Source) 
    f <- 7
    edgesego$Weight5 <- NA
    for( i in 1: (nrow(bpathproperties5[[9]])-1))
    {
      edgesego[which(edgesego$Together==bpathproperties5[[9]][i,3]),]$Weight5 <- (f+1)
      f<-f+1
    }
    edgesego$Weight5[is.na(edgesego$Weight5)] <- 1
  }
 else      #any othe path other than top 5
  {
    edgesego$Group <- as.numeric(edgesego$Together %in% bpathproperties[[9]]$Together)
    nodesego$Group<- as.numeric(nodesego$name %in% bpathproperties[[9]]$Source)
    f <- 7
    edgesego$Weight <- NA
    for( i in 1: (nrow(bpathproperties[[9]])-1))
    {
      edgesego[which(edgesego$Together==bpathproperties[[9]][i,3]),]$Weight <- (f+1)
      f<-f+1
    }
    edgesego$Weight[is.na(edgesego$Weight)] <- 1
  }
  assign("edgesego",edgesego,envir=.GlobalEnv)  
  assign("nodesego",nodesego,envir=.GlobalEnv)
}

FuncMemandWeight(5)


#Name List Items
FuncListName <- function(n)
{
  if(n==1)
  {
    bpathproperties1 <- list(DUNSinPath=bpathproperties1[[1]],NAIndicatorOfEachVertex=bpathproperties1[[2]],TotalNAinPath=bpathproperties1[[3]],NormalizedScoreOfEachVertex=bpathproperties1[[4]],ProductOfNormalizedWithoutNA=bpathproperties1[[5]],ProductOfNormalizedWithNA=bpathproperties1[[6]],NormalizedIntoEigenForEachVertex=bpathproperties1[[7]],SumOfNormalizedIntoEigenForPath=bpathproperties1[[8]],EdgelistOfPath=bpathproperties1[[9]])
    assign("bpathproperties1",bpathproperties1,envir=.GlobalEnv)
    }
  else if(n==2)
  {
    bpathproperties2 <- list(DUNSinPath=bpathproperties2[[1]],NAIndicatorOfEachVertex=bpathproperties2[[2]],TotalNAinPath=bpathproperties2[[3]],NormalizedScoreOfEachVertex=bpathproperties2[[4]],ProductOfNormalizedWithoutNA=bpathproperties2[[5]],ProductOfNormalizedWithNA=bpathproperties2[[6]],NormalizedIntoEigenForEachVertex=bpathproperties2[[7]],SumOfNormalizedIntoEigenForPath=bpathproperties2[[8]],EdgelistOfPath=bpathproperties2[[9]])
    assign("bpathproperties2",bpathproperties2,envir=.GlobalEnv)
    } 
  else if(n==3)
  {
    bpathproperties3 <- list(DUNSinPath=bpathproperties3[[1]],NAIndicatorOfEachVertex=bpathproperties3[[2]],TotalNAinPath=bpathproperties3[[3]],NormalizedScoreOfEachVertex=bpathproperties3[[4]],ProductOfNormalizedWithoutNA=bpathproperties3[[5]],ProductOfNormalizedWithNA=bpathproperties3[[6]],NormalizedIntoEigenForEachVertex=bpathproperties3[[7]],SumOfNormalizedIntoEigenForPath=bpathproperties3[[8]],EdgelistOfPath=bpathproperties3[[9]])
    assign("bpathproperties3",bpathproperties3,envir=.GlobalEnv)
    } 
  else if(n==4)
  {
    bpathproperties4 <- list(DUNSinPath=bpathproperties4[[1]],NAIndicatorOfEachVertex=bpathproperties4[[2]],TotalNAinPath=bpathproperties4[[3]],NormalizedScoreOfEachVertex=bpathproperties4[[4]],ProductOfNormalizedWithoutNA=bpathproperties4[[5]],ProductOfNormalizedWithNA=bpathproperties4[[6]],NormalizedIntoEigenForEachVertex=bpathproperties4[[7]],SumOfNormalizedIntoEigenForPath=bpathproperties4[[8]],EdgelistOfPath=bpathproperties4[[9]])
    assign("bpathproperties4",bpathproperties4,envir=.GlobalEnv)
    } 
  else if(n==5)
  {
    bpathproperties5 <- list(DUNSinPath=bpathproperties5[[1]],NAIndicatorOfEachVertex=bpathproperties5[[2]],TotalNAinPath=bpathproperties5[[3]],NormalizedScoreOfEachVertex=bpathproperties5[[4]],ProductOfNormalizedWithoutNA=bpathproperties5[[5]],ProductOfNormalizedWithNA=bpathproperties5[[6]],NormalizedIntoEigenForEachVertex=bpathproperties5[[7]],SumOfNormalizedIntoEigenForPath=bpathproperties5[[8]],EdgelistOfPath=bpathproperties5[[9]])
    assign("bpathproperties5",bpathproperties5,envir=.GlobalEnv)
    } 
  else
  {
    bpathproperties <- list(DUNSinPath=bpathproperties[[1]],NAIndicatorOfEachVertex=bpathproperties[[2]],TotalNAinPath=bpathproperties[[3]],NormalizedScoreOfEachVertex=bpathproperties[[4]],ProductOfNormalizedWithoutNA=bpathproperties[[5]],ProductOfNormalizedWithNA=bpathproperties[[6]],NormalizedIntoEigenForEachVertex=bpathproperties[[7]],SumOfNormalizedIntoEigenForPath=bpathproperties[[8]],EdgelistOfPath=bpathproperties[[9]])
    assign("bpathproperties",bpathproperties,envir=.GlobalEnv)
    } 
}

FuncListName(5)
colnames(nodesego)[1]<-"ID"
nodesego$Label <- nodesego$ID

write.csv(edgesego, "C:\\Users\\ganeshl\\OneDrive - Dun & Bradstreet\\Desktop\\Network Dependcies\\edgeegoPathScore.csv")
write.csv(nodesego, "C:\\Users\\ganeshl\\OneDrive - Dun & Bradstreet\\Desktop\\Network Dependcies\\nodesegoPathScore.csv")
