##########################################
# Load information about KMT/ "Segments" #
##########################################

ncol<- ncol(Segments_KMT)

Segments_1_KMT <- Segments_KMT %>% filter_at(vars(starts_with(Pole1)),
                                             any_vars(.>= 1))
Segments_1_KMT <- Segments_1_KMT %>% select("Segment ID",
                                            "length",
                                            "Node ID #1",
                                            "Node ID #2",
                                            "Point IDs")

Segments_2_KMT <- Segments_KMT %>% filter_at(vars(starts_with(Pole2)),
                                             any_vars(.>= 1))
Segments_2_KMT <- Segments_2_KMT %>% select("Segment ID",
                                            "length",
                                            "Node ID #1",
                                            "Node ID #2",
                                            "Point IDs")

############################################
# Load information about Poles coordiantes #
############################################

Pole1 <- Nodes_KMT %>% filter_at(vars(Pole1),
                                 any_vars(.>=1))
Pole1 <- data.frame(X = c(Pole1 %>% select("X Coord")/10000),
                    Y = c(Pole1 %>% select("Y Coord")/10000),
                    Z = c(Pole1 %>% select("Z Coord")/10000))
Pole2 <- Nodes_KMT %>% filter_at(vars(Pole2),
                                 any_vars(.>=1))
Pole2 <- data.frame(X = c(Pole2 %>% select("X Coord")/10000),
                    Y = c(Pole2 %>% select("Y Coord")/10000),
                    Z = c(Pole2 %>% select("Z Coord")/10000))

#########################################################
# Load information about (+) and (-) KMT ends / "Nodes" #
#########################################################
## If exist also load information about end morphology.

if(ncol(Nodes_KMT %>% select(starts_with("EndType"))) == 1){
  Nodes_KMT <- Nodes_KMT %>% select("Node ID", 
                                    "X Coord",
                                    "Y Coord",
                                    "Z Coord",
                                    starts_with("EndType"))
  
} else if (ncol(Nodes_KMT %>% select(starts_with("EndType"))) == 2){
  Nodes_KMT <- Nodes_KMT %>% select("Node ID", 
                                    "X Coord",
                                    "Y Coord",
                                    "Z Coord",
                                    starts_with("EndType"))
  
  compare <- data.frame()
  for(i in 1:nrow(Nodes_KMT %>% select(starts_with("EndType")))){
    compare[i,1] <- Nodes_KMT[i,5] == Nodes_KMT[i,6]
  }
  Nodes_KMT <- cbind(Nodes_KMT,
                     compare)
  names(Nodes_KMT)[7] <- "Entype_Different"
  rm(compare)
  
} else {
  Nodes_KMT <- Nodes_KMT %>% select("Node ID", 
                                    "X Coord",
                                    "Y Coord",
                                    "Z Coord")
}

Nodes_KMT[2:4] <- Nodes_KMT[2:4]/10000

#########################################################
# Load information about points which create "Segments" #
#########################################################

Points_KMT <- Points_KMT %>% select("Point ID", 
                                    "X Coord",
                                    "Y Coord",
                                    "Z Coord")

Points_KMT[2:4] <- Points_KMT[2:4]/10000
names(Points_KMT)[1] <- "Point_ID"
