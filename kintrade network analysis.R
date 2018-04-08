# Script to create network graphs, shortest paths, caculate betweeness centrality, and plot relationships

# install.packages
myPackages <- c("igraph", "RColorBrewer")
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[, 1])) 
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(myPackages, usePackage)

# set working directory put "" for no directory and put a / at the end of the directory name if present
setfile <- "test data/"
fname <- gsub('/','-',setfile)
fname <- gsub(' ','-',fname)

# Load Data
df <- read.csv(paste0(setfile,'linklist.csv')) # Change if needed!

# Create colors for villages and add to data frame for agent1 and agent2
villages <- data.frame(village = sort(unique(c(df$current.village.of.end1,df$current.village.of.end2))))

# Create Vertex attributes
agent1 <- df[,c(3,6)]
agent2 <- df[,c(5,7)]
names(agent2) <- names(agent1)
agent <- unique(rbind(agent1,agent2))
names(agent) <- c("Agent", "village")
agent <- merge(agent,villages, by = "village" )
agent <- agent[,c(2,1)]
rm(agent1)
rm(agent2)

# ensure all nodes are in from and to columns
a1 <- df$agent.number
a2 <- df$agent.number.1
dfg <- cbind.data.frame(c(a1,a2),c(a2,a1))
dfg <- unique(dfg)
names(dfg) <- c("From","To")

# Create graph
g1 <- graph_from_data_frame(data.frame(From = dfg$From, to = dfg$To), directed = F, vertices = agent) 
g1 <- simplify(g1)

# get colors
cols <- brewer.pal(8,"Set1")
gCols <- cols[as.numeric(as.factor(V(g1)$village))]
coldf <- data.frame(village = 1:8, color = cols)

# plot
plot(g1, vertex.size = 3, vertex.label = NA, edge.width = .2,
     edge.color = "darkgray",vertex.color = gCols)
legend("bottomleft", legend=levels(as.factor(V(g1)$village)), col = cols,
       bty = "n", pch=15 , title = 'Village', cex = 1.2)

# save plot
png(paste('Plots/FullNetwork',fname,'.png'), res = 600, height = 7.5, width = 10, units = 'in')
plot(g1, vertex.size = 3, vertex.label = NA, edge.width = .3,
     edge.color = "black", vertex.color = gCols)
legend("bottomleft", legend=levels(as.factor(V(g1)$village)), col = cols,
       bty = "n", pch=15 , title = 'Village', cex = 1.2)
dev.off()

# Get paths
paths <- shortest.paths(g1)

# subset paths by producing villages and villages at the end ofthe sequence
ProducingVillage <- agent[agent$village %in% 1:2,1]
EndVillage <- agent[agent$village %in% 7:8,1]
pathsEndtoProducing <- paths[rownames(paths) %in% ProducingVillage,colnames(paths) %in% EndVillage] 

# determine which agents have the shortest links
min(pathsEndtoProducing)
toPos <- which(pathsEndtoProducing == min(pathsEndtoProducing), arr.ind = T)
closestTies <- colnames(pathsEndtoProducing)[toPos[,2]]

# These are the agents in the end villages with the closest ties to the producing villages
print(closestTies)

# show exact path between agents
# get.shortest.paths(g1,from = "41530", to = "29943")

# Function to generate graph showing the relationships based on # of links between
# relations: can be 1 to 4. Also allows optional modifiers for node and label sizes (primary node will
# be twice as big)
graphRelation <- function(AgentNumber,Relations,vertex.size = NULL,label.cex = NULL){
  if(Relations > 16){print("Only returns up to 16 relation links")}
  # values for tests, comment out when not testing
  # AgentNumber <- 5466
  # Relations <- 4
  # vertex.size <- 10
  # label.cex <- .5
  
  # Generate all connections to the specified distance
  myAgent <- dfg[dfg$From == AgentNumber,]
  if(nrow(myAgent) == 0){
    stop("No agents returned")
  }
  for(j in 1:Relations){
    for(i in 1:nrow(myAgent)){
      myAgent <- rbind(myAgent,dfg[dfg$From == myAgent$To[i],])
    }
  }
  
  # make sure all agents are in both columns and ensure there are no duplicates
  a1 <- myAgent$From
  a2 <- myAgent$To
  myAgent <- cbind.data.frame(c(a1,a2),c(a2,a1))
  myAgent <- unique(myAgent)
  names(myAgent) <- c("From","To")
  
  # Create Vertex attributes
  agentAttr <- agent[agent$Agent %in% myAgent$From,]
  
  # create graph
  g <- graph_from_data_frame(myAgent, vertices = agentAttr, directed = F)
  
  # changes node size base on specified value or default value
  if(missing(vertex.size)) {vertex.size <- 5}
  V(g)$size <- vertex.size
  V(g)$size[V(g)$name == AgentNumber] <- vertex.size * 2
  
  # changes label size base on specified value or default value
  if(missing(label.cex)) {label.cex <- .2}
  V(g)$labelcex <- label.cex
  V(g)$labelcex[V(g)$name == AgentNumber] <- label.cex * 1.5
  
  return(g)
} # End of function


#Generate graph based on desired agent number and # of links between relations
# run all graphs for all minimum links
# for (i in 1:length(closestTies)){
#   g2 <- graphRelation(closestTies[i],2,vertex.size = 10, label.cex = .5)
#   vils <- data.frame(village = V(g2)$village)
#   vils <- merge(vils,coldf, by = "village")
#   vils$color <- as.character(vils$color)
#   g2 <- simplify(g2)
#   plot(g2, vertex.color = as.vector(vils$color), vertex.label.cex = V(g2)$labelcex)
#   legend("bottomleft", legend=unique(vils$village), col = unique(vils$color),
#          bty = "n", pch=15 , title = 'Village', cex = 1.2)
# 
# #   Save plots - uncomment to run
# png(paste0('Plots/',closestTies[i],'.png'), res = 600, height = 7.5, width = 10, units = 'in')
# plot(g2, vertex.color = as.vector(vils$color), vertex.label.cex = V(g2)$labelcex)
# legend("bottomleft", legend=unique(vils$village), col = unique(vils$color),
#        bty = "n", pch=15 , title = 'Village', cex = 1.2)
# dev.off()
# }

# generate plot for specific agent
# agentNum <- 1922 # enter the number of the agent to graph relations.
# g3 <- graphRelation(agentNum,3,vertex.size = 10, label.cex = .5)
# vils <- data.frame(village = V(g3)$village)
# vils <- merge(vils,coldf, by = "village")
# vils$color <- as.character(vils$color)
# g3 <- simplify(g3)
# plot(g3, vertex.color = as.vector(vils$color), vertex.label.cex = V(g3)$labelcex)
# legend("bottomleft", legend=unique(vils$village), col = unique(vils$color),
#        bty = "n", pch=15 , title = 'Village', cex = 1.2)

#  Save plots - uncomment to run
# png(paste0('Plots/',agentNum,'.png'), res = 600, height =  7.5, width = 10, units = 'in')
# plot(g3, vertex.color = as.vector(vils$color), vertex.label.cex = V(g3)$labelcex)
# legend("bottomleft", legend=unique(vils$village), col = unique(vils$color),
#        bty = "n", pch=15 , title = 'Village', cex = 1.2)
# dev.off()

# calculate betweeness centrality
bcent <- as.data.frame(betweenness(g1, directed = F))
bcent$Agent <- row.names(bcent)
row.names(bcent) <- NULL
names(bcent)[1] <- "Betweenness"

# get shortest paths
prodAgents <- agent[agent$village %in% 1:2,1]
sPathdf <- paths[,colnames(paths) %in% prodAgents] 
ShortestPath <- as.data.frame(apply(sPathdf,1, min, na.rm = T))
ShortestPath$Agent <- row.names(ShortestPath)
row.names(ShortestPath) <- NULL
names(ShortestPath)[1] <- "ShortestPath"

# generate data frame and save to csv containing the agent number, current village, number of pots,
# shortest path between producing village, and betweeness centrality score
df2 <- read.csv(paste0(setfile,'agentlist.csv')) # Change if needed!
df2 <- df2[,c(2,4:6,11)]
names(df2) <- c("Agent","Village","PotsOwned","PotsTraded","Degree Centrality")
df2 <- merge(df2,ShortestPath, by = "Agent")
df2 <- merge(df2,bcent, by = "Agent")
write.csv(df2,paste0("AgentMetricsfor ",fname,".csv"), row.names = F)

