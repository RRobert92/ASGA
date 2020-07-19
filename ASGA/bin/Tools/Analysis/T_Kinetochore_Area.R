################################################################################################
# Tool Kinetochore size 
#
# Set of functions to define kinetochore size and compare it it number of KMTs per kinetohore
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-04-17
# Debugged/Reviewed: Robert Kiewisz 19/07/2020
################################################################################################

# Function: to calculate kinetochore size  -----------------------------------------------------
Kinetochore_Size <- function(x){
  DF <- select(get(paste(colnames(Segments)[x], 
                         "fiber", 
                         sep = "_")), 
               starts_with("V"))
  
  Mean_DF <- get(paste(colnames(Segments)[x], 
                       "fiber", 
                       sep = "_"))[1:3]
  
  dist <- data.frame()
  
  for(j in 1:ncol(DF)){
    dist[j,1] <- Points[as.numeric(DF[2,j] + 1), "X Coord"]
    dist[j,2] <- Points[as.numeric(DF[2,j] + 1), "Y Coord"]
    dist[j,3] <- Points[as.numeric(DF[2,j] + 1), "Z Coord"]
  }
  
  dist <- na.omit(dist)
  dist[4:6] <- Mean_DF[2,1:3]
  dist$distance <- apply(dist, 
                         1, 
                         function(y) dist(matrix(y, 
                                                 nrow = 2, 
                                                 byrow = TRUE)))
  
  fiber_radius <- as.numeric(max(dist$distance))
  
  rm(dist)
  
  DF <- data.frame(Kinetochore_area = as.numeric(pi * fiber_radius^2),
                   KMT_no = as.numeric(nrow(get(paste(colnames(Segments)[x])))),
                   Fiber_name = get(paste(colnames(Segments)[x]))[1,"Fiber_Name"],
                   Plus_dist_to_pole = get(paste(colnames(Segments)[x]))[1,"plus_dist_to_pole"],
                   Elipse_position = get(paste(colnames(Segments)[x]))[1,"Elipse_Position"]
                   )
  DF
}