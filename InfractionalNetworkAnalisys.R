library(statnet)
library(RColorBrewer)
library(network)

netmat <- rbind(c(1,2),
                c(1,3),
                c(2,3),
                c(1,4),
                c(5,6),
                c(7,8),
                c(5,7),
                c(5,8),
                c(5,9),
                c(6,7),
                c(6,8),
                c(6,9),
                c(5,10),
                c(6,10),
                c(11,12),
                c(11,13),
                c(13,14),
                c(14,19),
                c(13,19),
                c(14,1),
                c(19,15),
                c(19,16),
                c(19,17),
                c(19,18),
                c(12,15),
                c(12,16),
                c(12,17),
                c(12,18),
                c(20,8),
                c(20,9),
                c(21,8),
                c(21,9),
                c(3,8),
                c(3,9),
                c(1,8),
                c(1,9))


net <- network(netmat, matrix.type="edgelist")
netmatsym <- symmetrize(as.sociomatrix(net), rule ="weak")


netsym <- network(netmatsym, matrix.type="adjacency")
network.vertex.names(netsym) <- c("B***cu L***na", 
                                  "B***cu An***us", 
                                  "B**scu C***nel",
                                  "B**hiu G***ge",
                                  "M**tu M**na",
                                  "Ma**u I***he",
                                  "T**a F**p",
                                  "T**a G***ghe",
                                  "S**m An**la",
                                  "G**ca G****ghe",
                                  "C**u I**n",
                                  "M***u L**do",
                                  "D**a D**a",
                                  "D**a C**l",
                                  "N**cu P**u",
                                  "N**se T**er",
                                  "S***an C***tin",
                                  "O***u A**ei",
                                  "D**a I***l",
                                  "P**ci V***e",
                                  "D***mir R**a")
set.vertex.attribute(netsym, "role", c("C", 
                                       "C", 
                                       "C",
                                       "CR",
                                       "C",
                                       "C",
                                       "CT",
                                       "CT",
                                       "CT",
                                       "C",
                                       "C",
                                       "A",
                                       "A",
                                       "C",
                                       "C",
                                       "C",
                                       "C",
                                       "C",
                                       "CT",
                                       "D",
                                       "D"))
# C : Comerciant, CR : Cartita, CT: contrabandist, A: aducator clienti, D: depozitare
set.vertex.attribute(netsym, "abrev_name", c("BL", 
                                             "BA",
                                             "BC",
                                             "BG",
                                             "MM",
                                             "MI",
                                             "TF",
                                             "TG",
                                             "SA",
                                             "GG",
                                             "CI",
                                             "ML",
                                             "DD",
                                             "DC",
                                             "NP",
                                             "NT",
                                             "SC",
                                             "OA",
                                             "DI",
                                             "PV",
                                             "DR"))
netsym %v% "alldeg" <- degree(netsym)
summary(netsym)
namelab <- get.vertex.attribute(netsym, "vertex.names")
rolelab <- get.vertex.attribute(netsym, "role")
abrevnamelab <-get.vertex.attribute(netsym, "abrev_name")
my_pal <- brewer.pal(5,"Dark2")
rolecat <- as.factor(get.vertex.attribute(netsym,"role"))
plot(netsym,
     main = "Infractional network",
     usearrows=FALSE, 
     mode="fruchtermanreingold", 
     vertex.col = my_pal[rolecat],
     label=rolelab,
     displaylabels=T,
     vertex.cex = 1.5)


# Capitolul 2 
print("BASIC CHARACTERISTICS")

print("Size:")
print(network.size(netsym))

print("Density:")
print(gden(netsym))

print("Components:")
print(components(netsym))

print("Diameter:")
gd <- geodist(netsym)
print(max(gd$gdist))

print("Transitivity:")
print(gtrans(netsym, mode="graph"))

# Capitolul 3
print("DIFFERENT REPRESENTATIONS")

print("Sociomatrix:")
print(as.sociomatrix(netsym))

print("Edge list:")
print(as.matrix(netsym, matrix.type="edgelist"))



print("Filtering networks")
print(get.vertex.attribute(netsym, "role"))
comercianti <- get.inducedSubgraph(netsym, which (netsym %v% "role"=="C"))
gplot(comercianti,displaylabels=TRUE, main="Comercianti")
delete.vertices(comercianti, isolates(comercianti))
gplot(comercianti, displaylabels = TRUE, main="Grupuri de comercianti")

# Capitolul 4
print("NETWORK VISUALISATION")
gplot(netsym,gmode="graph",edge.col="grey75",displaylabels=T,
      vertex.cex=1.5,mode='circle',main="circle")
gplot(netsym,gmode="graph",edge.col="grey75",displaylabels=T,
      vertex.cex=1.5,mode='eigen',main="eigen")
gplot(netsym,gmode="graph",edge.col="grey75",displaylabels=T,
      vertex.cex=1.5,mode='random',main="random")
gplot(netsym,gmode="graph",edge.col="grey75",displaylabels=T,
      vertex.cex=1.5,mode='spring',main="spring")
gplot(netsym,gmode="graph",edge.col="grey75",displaylabels=T,
      vertex.cex=1.5,mode='fruchtermanreingold',main='fruchtermanreingold')
gplot(netsym,gmode="graph",edge.col="grey75",displaylabels=T,
      vertex.cex=1.5,mode='kamadakawai',
      main='kamadakawai')

# Capitolul 5
plot(netsym,vertex.cex=0.5,main="Too small nodes")
plot(netsym,vertex.cex=6,main="Too large nodes")
plot(netsym,vertex.cex=2,main="Just right node size")

sidenum <- 3:7
rolecat <- as.factor(get.vertex.attribute(asIgraph(netsym),"role"))
plot(netsym,usearrows=FALSE,vertex.cex=4, main="Different node type",
     displaylabels=F,vertex.sides=sidenum[rolecat])

n_edge <- network.edgecount(netsym)
linecol_pal <- c("blue","red","green")
edge_cat <- sample(1:3,n_edge,replace=T)
plot(netsym,vertex.cex=1.5,vertex.col="grey25", main="Edge coloring example",
     edge.col=linecol_pal[edge_cat],edge.lwd=2)

widths <- c(2,6,10)
plot(netsym,vertex.cex=1.5,main="Different edge width",
     edge.lwd=1.5*widths)

n_edge <- network.edgecount(netsym)
edge_cat <- sample(1:3,n_edge,replace=T)
line_pal <- c(2,3,4)
gplot(netsym,vertex.cex=0.8,gmode="graph", main="Different edge type",
      vertex.col="gray50",edge.lwd=1.5,
      edge.lty=line_pal[edge_cat])


my_pal <- brewer.pal(5,"Dark2")
rolecat <- as.factor(get.vertex.attribute(netsym,"role"))
plot(netsym,
     main = "Infractional network",
     usearrows=FALSE, 
     mode="fruchtermanreingold", 
     vertex.col = my_pal[rolecat],
     label=abrevnamelab,
     displaylabels=T,
     vertex.cex = 1.5)
legend("bottomleft",legend=c("Aducator clienti","Comerciant","Cartita","Contrabandist","Depozitare"),
       col=my_pal,pch=19,pt.cex=1.5,bty="n",
       title="Criminal Role")

# necessary, caused conflicts
detach("package:statnet", unload=TRUE)


library(network)

library(intergraph)
library(igraph)

library(networkD3)

# Capitolul 6
# Tkplot
inetsym <- asIgraph(netsym)
Coord <- tkplot(inetsym, vertex.size=3,
                vertex.label=V(inetsym)$role,
                vertex.color="darkgreen")
MCoords <- tkplot.getcoords(Coord)
plot(inetsym, layout=MCoords, vertex.size=5,main="Interactive tkplot",
     vertex.label=NA, vertex.color="lightblue")


# NetworkD3
inetsym_edge <- get.edgelist(inetsym)
inetsym_edge <- inetsym_edge - 1
inetsym_edge <- data.frame(inetsym_edge)
print(V(inetsym)$role)
inetsym_nodes <- data.frame(NodeID=as.numeric(V(inetsym)-1),
                          Group=V(inetsym)$role,
                          Nodesize=(degree(inetsym)))
net_D3 <- forceNetwork(Links = inetsym_edge, Nodes = inetsym_nodes,
             Source = "X1", Target = "X2",
             NodeID = "NodeID",Nodesize = "Nodesize",
             radiusCalculation="Math.sqrt(d.nodesize)*3",
             Group = "Group", opacity = 0.8,
             legend=TRUE)

saveNetwork(net_D3,file = 'Net_test2.html',
            selfcontained=TRUE)


#Visnetwork
library(visNetwork)
inetsym_edge <- get.edgelist(inetsym)
inetsym_edge <- data.frame(from = inetsym_edge[,1],
                         to = inetsym_edge[,2])
inetsym_nodes <- data.frame(id = as.numeric(V(inetsym)))
visNetwork(inetsym_nodes, inetsym_edge, width = "100%")
net <- visNetwork(inetsym_nodes, inetsym_edge,
                  width = "100%",legend=TRUE)
net <- visOptions(net,highlightNearest = TRUE)
net <- visInteraction(net,navigationButtons = TRUE)
library(htmlwidgets)
saveWidget(net, "Net_test3.html")


#Arcdiagram
library(devtools)
install_github("gastonstat/arcdiagram")
library(arcdiagram)
inetsym <- asIgraph(netsym)
netsym_edge <- get.edgelist(netsym)
arcplot(netsym)

#Chord diagram
library(circlize)
library(statnet)
sociomat <- as.sociomatrix(netsym,attrname='passes')
chordDiagram(sociomat)
detach("package:statnet", unload=TRUE)
detach("package:circlize", unload=TRUE)


#Chapter 7
detach("package:networkD3", unload=TRUE)
detach("package:igraph", unload=TRUE)
print("CENTRALITY DEGREES")
print(degree(netsym, gmode="graph"))
print(closeness(netsym, gmode="graph"))
print(betweenness(netsym, gmode="graph"))

#Cutpoints
cpnet <- cutpoints(netsym,mode="graph",
                   return.indicator=TRUE)
gplot(netsym,gmode="graph",vertex.col=cpnet+2,coord=MCoords,
      jitter=FALSE,displaylabels=TRUE)

#Bridges
bridges <- function(dat,mode="graph",
                    connected=c("strong", "weak")) {
   e_cnt <- network.edgecount(dat)
   if (mode == "graph") {
      cmp_cnt <- components(dat)
      b_vec <- rep(FALSE,e_cnt)
      for(i in 1:e_cnt){
         dat2 <- dat
         delete.edges(dat2,i)
         b_vec[i] <- (components(dat2) != cmp_cnt)
      }
   }
   else {
      cmp_cnt <- components(dat,connected=connected)
      b_vec <- rep(FALSE,e_cnt)
      for(i in 1:e_cnt){
         dat2 <- dat
         delete.edges(dat2,i)
         b_vec[i] <- (components(dat2) != cmp_cnt)
      }
   }
   return(b_vec)
}
bridges(netsym)


### Chapter 7
colors <- c("blue", "red")

# Determining the centre nodes using the degree
deg <- degree(netsym, gmode="graph")
plot(netsym,
     usearrows=FALSE, 
     vertex.col = colors[(deg >= 5) + 1],
     label = netsym %v% "abrev_name",
     displaylabels=T,
     vertex.cex = deg/2)

# Determining the centre nodes using the closeness function
cls <- closeness(netsym, gmode="graph")
plot(netsym,
     usearrows=FALSE, 
     vertex.col = colors[(cls >= 0.33) + 1],
     label = netsym %v% "abrev_name",
     displaylabels=T,
     vertex.cex = cls*10)

# Determining the centre nodes using the betweenness function
bet <- betweenness(netsym, gmode="graph")
plot(netsym,
     usearrows=FALSE, 
     vertex.col = colors[(bet >= 90) + 1],
     label = netsym %v% "abrev_name",
     displaylabels=T,
     vertex.cex = sqrt(bet+1))

# Computing the level of correlation between multiple centrality measures
df.prom <- data.frame(
        deg = degree(netsym),
        cls = closeness(netsym),
        btw =  betweenness(netsym),
        evc = evcent(netsym),
        inf = infocent(netsym),
        flb = flowbet(netsym)
)
cor(df.prom)

# Tabular visualization for multiple centrality measures
# Defining a data frame in which is computed the centrality for all nodes using
# multiple methods
df.prom2 <- data.frame(
        name = network.vertex.names(netsym),
        degree = degree(netsym, gmode="graph"),
        closeness = closeness(netsym, gmode="graph"),
        betweenness = betweenness(netsym, gmode="graph"))
df.promsort <- df.prom2[order(-df.prom2$degree),]
cd <- centralization(netsym,degree)
cc <- centralization(netsym,closeness)
cb <- centralization(netsym,betweenness)
df.promsort <- rbind(df.promsort,data.frame(
        name = "Centralization level",
        degree = cd,
        closeness = cc,
        betweenness = cb
))
df.promsort

# Cutpoints are nodes that if removed will affect the conectivity of the network
# In the graphic below, it is displayed with green the cutpoint nodes.
cpnet <- cutpoints(netsym,mode="graph",return.indicator=TRUE)
gplot(netsym,gmode="graph",vertex.cex=cpnet+2,vertex.col=cpnet+2,jitter=FALSE,
      displaylabels=TRUE,label=netsym %v% "abrev_name")

# Bridges are edges that if removed will affect the conectivity of the network
# In the graphic below it is displayed with green the edges that are bridges.
bridges <- function(dat,mode="graph",connected=c("strong", "weak")) {
        e_cnt <- network.edgecount(dat)
        if (mode == "graph") {
                cmp_cnt <- components(dat)
                b_vec <- rep(FALSE,e_cnt)
                for(i in 1:e_cnt){
                        dat2 <- dat
                        delete.edges(dat2,i)
                        b_vec[i] <- (components(dat2) != cmp_cnt)
                }
        }
        else {
                cmp_cnt <- components(dat,connected=connected)
                b_vec <- rep(FALSE,e_cnt)
                for(i in 1:e_cnt){
                        dat2 <- dat
                        delete.edges(dat2,i)
                        b_vec[i] <- (components(dat2,connected=connected) != cmp_cnt)
                }
        }
        return (b_vec)
}
bridges(netsym)
brnet <- bridges(netsym)
gplot(netsym,gmode="graph",vertex.col="red",edge.col=brnet+2,jitter=FALSE,
      displaylabels=TRUE,label=netsym %v% "abrev_name",edge.lwd=3*brnet+2)



### Chapter 8
# Setup
# Import igraph for this part of the project
library(igraph)
library(intergraph)
# Transfer network from statnet format to igraph format
inetsym <- as.undirected(asIgraph(netsym))
V(inetsym)$name <- netsym %v% "abrev_name"
V(inetsym)$fullname <- network.vertex.names(netsym)
V(inetsym)$role <- rolecat

# Cliques
# Determine the cliques from the network as well as the biggest clique.
clique.number(inetsym)
cliques(inetsym, min=3)
maximal.cliques(inetsym, min=3)
largest.cliques(inetsym)

# k-Cores
coreness <- graph.coreness(inetsym)
table(coreness)
maxCoreness <- max(coreness)
maxCoreness
colors <- rainbow(maxCoreness)
plot(inetsym,vertex.label=coreness,vertex.color=colors[coreness],layout=layout_with_fr)

i1_3 <- inetsym
i2_3 <- induced.subgraph(inetsym, vids=which(coreness > 1))
i3_3 <- induced.subgraph(inetsym, vids=which(coreness > 2))
lay <- layout.fruchterman.reingold(inetsym)
op <- par(mfrow=c(1,3),mar = c(3,0,2,0))
plot(i1_3,layout=lay,vertex.label=coreness,vertex.color=colors[coreness],main="All k-cores")
plot(i2_3,layout=lay[which(coreness > 1),],vertex.label=coreness[which(coreness > 1)],vertex.color=colors[coreness[which(coreness > 1)]],main="k-cores 2-3")
plot(i3_3,layout=lay[which(coreness > 2),],vertex.label=coreness[which(coreness > 2)],vertex.color=colors[coreness[which(coreness > 2)]],main="k-cores 3")
par(op)

# Modularity is a measure that describes how good is a network clusterization
colors <- brewer.pal(5,"Dark2")
roles <- c("C","CR","CT","A","D")
V(inetsym)[V(inetsym)$role == "C"]$color <- colors[1]
V(inetsym)[V(inetsym)$role == "CR"]$color <- colors[2]
V(inetsym)[V(inetsym)$role == "CT"]$color <- colors[3]
V(inetsym)[V(inetsym)$role == "A"]$color <- colors[4]
V(inetsym)[V(inetsym)$role == "D"]$color <- colors[5]

V(inetsym)[V(inetsym)$role == "C"]$group <- 1
V(inetsym)[V(inetsym)$role == "CR"]$group <- 2
V(inetsym)[V(inetsym)$role == "CT"]$group <- 3
V(inetsym)[V(inetsym)$role == "A"]$group <- 4
V(inetsym)[V(inetsym)$role == "D"]$group <- 5

op <- par(mfrow=c(1,1))
plot(inetsym,vertex.color=V(inetsym)$color,vertex.size=10)

# Modularity based on the role of each person
modularity(inetsym, V(inetsym)$group)
# The result is smaller than 0, which means a bad clusterization result using this method

# Community detection algorithms
cw <- cluster_walktrap(inetsym)
modularity(cw)
membership(cw)
ceb <- cluster_edge_betweenness(inetsym)
modularity(ceb)
membership(ceb)
cs <- cluster_spinglass(inetsym)
modularity(cs)
membership(cs)
cfg <- cluster_fast_greedy(inetsym)
modularity(cfg)
membership(cfg)
clp <- cluster_label_prop(inetsym)
modularity(clp)
membership(clp)
cle <- cluster_leading_eigen(inetsym)
modularity(cle)
membership(cle)
cl <- cluster_louvain(inetsym)
modularity(cl)
membership(cl)
table(V(inetsym)$role,membership(cw))
compare(as.numeric(factor(V(inetsym)$role)),cw,method="adjusted.rand")
compare(cw,ceb,method="adjusted.rand")
compare(cw,cs,method="adjusted.rand")
compare(cw,cfg,method="adjusted.rand")
op <- par(mfrow=c(3,2),mar=c(3,0,2,0))
plot(ceb, inetsym,vertex.label=V(inetsym)$name,main="Edge Betweenness")
plot(cfg, inetsym,vertex.label=V(inetsym)$name,main="Fastgreedy")
plot(clp, inetsym,vertex.label=V(inetsym)$name,main="Label Propagation")
plot(cle, inetsym,vertex.label=V(inetsym)$name,main="Leading Eigenvector")
plot(cs, inetsym,vertex.label=V(inetsym)$name,main="Spinglass")
plot(cw, inetsym,vertex.label=V(inetsym)$name,main="Walktrap")
par(op)


# Chapter 10

# Trying to generate a similar network using Erdos-Renyi method
no_nodes <- length(V(inetsym))
no_edges <- length(E(inetsym))
generated_network <- erdos.renyi.game(n=no_nodes,no_edges,type='gnm')
op <- par(mfrow=c(1,2))
plot(inetsym,vertex.label=NA,vertex.size=5)
plot(generated_network, vertex.label=NA, vertex.size=5)
par(op)

# Trying to generate a similar network using Small-World Model
avg_degree <- no_edges/no_nodes*2
g1 <- watts.strogatz.game(dim=1, size=no_nodes, nei=avg_degree/2, p=.05)
g2 <- watts.strogatz.game(dim=1, size=no_nodes, nei=avg_degree/2, p=.15)
g3 <- watts.strogatz.game(dim=1, size=no_nodes, nei=avg_degree/2, p=.30)
op <- par(mfrow=c(2,2))
plot(inetsym,vertex.label=NA,vertex.size=5)
plot(g1, vertex.label=NA, vertex.size=5)
plot(g2, vertex.label=NA, vertex.size=5)
plot(g3, vertex.label=NA, vertex.size=5)
par(op)

# Trying to generate a similar network using Scale-Free Model
barabasi_network <- barabasi.game(no_nodes, directed=FALSE)
op <- par(mfrow=c(1,2))
plot(inetsym,vertex.label=NA, vertex.size=5)
plot(barabasi_network,vertex.label=NA, vertex.size=5)
par(op)


# Comparing random models with the empirical network
list_network <- c(generated_network, g2, barabasi_network, inetsym)
comparison_table <- data.frame(
  Name = c("Erdos-Renyi", "Small world", "Scale-free model", "Empiric network"),
  Size = c(length(V(generated_network)), length(V(g2)), length(V(barabasi_network)), length(V(inetsym))),
  Density = c(gden(asNetwork(generated_network)),gden(asNetwork(g2)),gden(asNetwork(barabasi_network)),gden(asNetwork(inetsym))),
  Avg_Degree = c(length(E(generated_network))/length(V(generated_network)),length(E(g2))/length(V(g2)),length(E(barabasi_network))/length(V(barabasi_network)),length(E(inetsym))/length(V(inetsym))),
  Transitivity = c(transitivity(generated_network), transitivity(g2), transitivity(barabasi_network), transitivity(inetsym)),
  Isolates = c(sum(degree(generated_network)==0),sum(degree(g2)==0),sum(degree(barabasi_network)==0),sum(degree(inetsym)==0))
)
comparison_table
