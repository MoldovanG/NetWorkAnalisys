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
     usearrows=FALSE, 
     mode="fruchtermanreingold", 
     vertex.col = my_pal[rolecat],
     label=rolelab,
     displaylabels=T,
     vertex.cex = 1.5)

