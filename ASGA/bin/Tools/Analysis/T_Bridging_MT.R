########################################################################################################
# Tool MT_bridging
#
# Function to calculate interaction lenth for any KMTs

#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz/ Gunar Fabig
# Created: 2020-09-01
# Reviewed: 
########################################################################################################

# Function:  -------------------------------------------------------------------------------------------
Segment_Interaction <- function(x){
  cores <- detectCores()
  cl<- makeCluster(cores)
  registerDoParallel(cl)
  
  system.time({
    DF <- data.frame()
    Final <- foreach(x = 1:1000, .combine=rbind) %dopar% {
      p_to_P <- Points[with(Points, `X Coord` <= as.numeric(Points[x,2] + (Minus_Distance)) &
                              `X Coord` >= as.numeric(Points[x,2] - (Minus_Distance))), ]
      p_to_P <- p_to_P[with(p_to_P, `Y Coord` <= as.numeric(Points[x,3] + (Minus_Distance * 2)) &
                              `Y Coord` >= as.numeric(Points[x,3] - (Minus_Distance))), ]
      p_to_P <- p_to_P[with(p_to_P, `Z Coord` <= as.numeric(Points[x,4] + (Minus_Distance * 2)) &
                              `Z Coord` >= as.numeric(Points[x,4] - (Minus_Distance))), ]
      p_to_P[5:7] <- Points[x, 2:4]
      
      p_to_P$dist <- apply(
        p_to_P[2:7],
        1,
        function(y) {
          dist(matrix(y,
                      nrow = 2,
                      byrow = TRUE
          ))
        }
      )
      
      DF_1 <- data.frame(p_to_P[with(p_to_P, dist <= Minus_Distance & dist > 0), "Point_ID"],
                         p_to_P[with(p_to_P, dist <= Minus_Distance & dist > 0), "dist"])
      names(DF_1)[1] <- "Point_ID_2"
      DF_1 <- data.frame(Points[x,1],
                         DF_1)
      names(DF_1)[1] <- "Point_ID_1"
      names(DF_1)[3] <- "dist"
      DF_1
    }
    stopCluster(cl)
  })
}

Bridging_MT <- function(x) {
  

}