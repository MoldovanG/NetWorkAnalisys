library(RColorBrewer)
library(statnet)

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
                c(14,19),
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
network.vertex.names(netsym) <- c("Berescu Liliana", 
                                  "Berescu Angelus", 
                                  "Berescu Cristinel",
                                  "Bighiu George",
                                  "Mantu Mariana",
                                  "Mantu Iordache",
                                  "Taga Filip",
                                  "Taga Gheorghe",
                                  "Saim Angela",
                                  "Galca Gheorghe",
                                  "Chelaru Ioan",
                                  "Melciu Leonardo",
                                  "Dura Dana",
                                  "Dura Costel",
                                  "Nedelcu Petru",
                                  "Nastase Toader",
                                  "Stoian Constatin",
                                  "Olaru Andrei",
                                  "Dura Ionel",
                                  "Palici Vasile",
                                  "Dragomir Ramona")
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
                                       "A",
                                       "A",
                                       "C",
                                       "C",
                                       "C",
                                       "C",
                                       "CT",
                                       "D",
                                       "D"))
# C : Comerciant, CR : Cartita, CT: contrabandist, A: aducator clienti, D: depozitare
netsym %v% "alldeg" <- degree(netsym)
summary(netsym)
namelab <- get.vertex.attribute(netsym, "vertex.names")
rolelab <- get.vertex.attribute(netsym, "role")
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
     label=rolelab,
     displaylabels=T,
     vertex.cex = 1.5)
legend("bottomleft",legend=c("C","CT","CR","A","D"),
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
