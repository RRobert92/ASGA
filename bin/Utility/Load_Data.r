##########################################
# Load information about KMT/ "Segments" #
##########################################

ncol<- ncol(Segments)

Segments_1_KMT <- Segments %>% filter_at(vars(starts_with(Pole1)),
                                         any_vars(.>= 1))
Segments_1_KMT <- Segments_1_KMT %>% select("Segment ID",
                                            "length",
                                            "Node ID #1",
                                            "Node ID #2",
                                            "Point IDs")

Segments_2_KMT <- Segments %>% filter_at(vars(starts_with(Pole2)),
                                         any_vars(.>= 1))
Segments_2_KMT <- Segments_2_KMT %>% select("Segment ID",
                                            "length",
                                            "Node ID #1",
                                            "Node ID #2",
                                            "Point IDs")

Segments_KMT <- Segments %>% filter_at(vars(starts_with("Pole")),
                                       any_vars(.>=1))

Segments_SMT <- Segments %>% filter_at(vars(starts_with("Pole")),
                                       all_vars(.< 1))
Segments_SMT <- Segments_SMT %>% select("Segment ID",
                                        "length",
                                        "Node ID #1",
                                        "Node ID #2",
                                        "Point IDs")

############################################
# Load information about Poles coordiantes #
############################################

Pole1 <- Nodes %>% filter_at(vars(Pole1),
                                 any_vars(.>=1))
Pole1 <- data.frame(X = c(Pole1 %>% select("X Coord")/10000),
                    Y = c(Pole1 %>% select("Y Coord")/10000),
                    Z = c(Pole1 %>% select("Z Coord")/10000))
Pole2 <- Nodes %>% filter_at(vars(Pole2),
                                 any_vars(.>=1))
Pole2 <- data.frame(X = c(Pole2 %>% select("X Coord")/10000),
                    Y = c(Pole2 %>% select("Y Coord")/10000),
                    Z = c(Pole2 %>% select("Z Coord")/10000))

#########################################################
# Load information about (+) and (-) KMT ends / "Nodes" #
#########################################################
## If exist also load information about end morphology.

if(ncol(Nodes %>% select(starts_with("EndType"))) == 1){
  Nodes <- Nodes %>% select("Node ID", 
                            "X Coord",
                            "Y Coord",
                            "Z Coord",
                            starts_with("EndType"))
  
} else if (ncol(Nodes %>% select(starts_with("EndType"))) == 2){
  Nodes <- Nodes %>% select("Node ID", 
                            "X Coord",
                            "Y Coord",
                            "Z Coord",
                            starts_with("EndType"))
  
  compare <- data.frame()
  
  for(i in 1:nrow(Nodes %>% select(starts_with("EndType")))){
    compare[i,1] <- Nodes[i,5] == Nodes[i,6]
  }
  Nodes <- cbind(Nodes,
                 compare)
  names(Nodes)[7] <- "Entype_Different"
  rm(compare)
  
} else {
  Nodes <- Nodes %>% select("Node ID", 
                            "X Coord",
                            "Y Coord",
                            "Z Coord")
}

Nodes[2:4] <- Nodes[2:4]/10000

#########################################################
# Load information about points which create "Segments" #
#########################################################

Points <- Points %>% select("Point ID", 
                            "X Coord",
                            "Y Coord",
                            "Z Coord")

Points[2:4] <- Points[2:4]/10000
names(Points)[1] <- "Point_ID"
