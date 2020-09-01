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

Bridging_MT <- function(x) {
  
  for (i in 1:nrow(get(paste(colnames(Segments)[x])))) {
    DF <- data.frame()
    
    for (j in 1:nrow(get(paste(colnames(Segments)[x], i, sep = "_")))) {
      
      # Creat boundery box around selected point
      tryCatch(
        {
          p_to_P <- Points[with(Points, `X Coord` <= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 2] + as.numeric(Minus_Distance * 2)) &
                                 `X Coord` >= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 2] - as.numeric(Minus_Distance * 2))), ]
          p_to_P <- p_to_P[with(p_to_P, `Y Coord` <= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 3] + as.numeric(Minus_Distance * 2)) &
                                  `Y Coord` >= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 3] - as.numeric(Minus_Distance * 2))), ]
          p_to_P <- p_to_P[with(p_to_P, `Z Coord` <= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 4] + as.numeric(Minus_Distance * 2)) &
                                  `Z Coord` >= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 4] - as.numeric(Minus_Distance * 2))), ]
          p_to_P[5:7] <- get(paste(colnames(Segments)[x], i, sep = "_"))[j, 2:4]
          
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
          DF <- data.frame(
            p_to_P[with(p_to_P, dist <= Minus_Distance & dist >= 0), "Node ID"],
            p_to_P[with(p_to_P, dist <= Minus_Distance & dist >= 0), "dist"]
          )
        },
        warning = function(w) {}
      )
      
      
    }
  }
}