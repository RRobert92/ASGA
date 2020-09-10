########################################################################################################
# Tool MT_bridging
#
# Function to calculate interaction length for all MT
# The tool is calculating interaction for all points and searching for continues interaction on MT of
# 500 nm (modifiable). Sorted interactions are then fed to the network analysis to determine no. of
# first-degree interaction, and no. of bundled MT.
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz / Gunar Fabig
# Created: 2020-09-01
# Reviewed:
########################################################################################################

# Function:  -------------------------------------------------------------------------------------------
Point_interaction <- function() {

  # Calculate distant of the point to other point in the boundary box ~15 min --------------------------
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  DF <- foreach(i = 1:nrow(Points), .combine = rbind) %dopar% {
    p_to_P <- Points[with(Points, `X Coord` <= as.numeric(Points[i, 2] + (Minus_Distance)) &
      `X Coord` >= as.numeric(Points[i, 2] - (Minus_Distance))), ]
    p_to_P <- p_to_P[with(p_to_P, `Y Coord` <= as.numeric(Points[i, 3] + (Minus_Distance * 2)) &
      `Y Coord` >= as.numeric(Points[i, 3] - (Minus_Distance))), ]
    p_to_P <- p_to_P[with(p_to_P, `Z Coord` <= as.numeric(Points[i, 4] + (Minus_Distance * 2)) &
      `Z Coord` >= as.numeric(Points[i, 4] - (Minus_Distance))), ]
    p_to_P[5:7] <- Points[i, 2:4]

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

    DF_1 <- data.frame(
      p_to_P[with(p_to_P, dist <= Minus_Distance & dist > 0), "Point_ID"],
      p_to_P[with(p_to_P, dist <= Minus_Distance & dist > 0), "dist"]
    )
    names(DF_1)[1] <- "Point_ID_2"
    DF_1 <- data.frame(
      Points[i, 1],
      DF_1
    )
    names(DF_1)[1] <- "Point_ID_1"
    names(DF_1)[3] <- "dist"
    DF_1 <- cbind(DF_1, c(DF_1[1] - DF_1[2]))
    names(DF_1)[4] <- "V1"

    DF_1[with(DF_1, `V1` < -1 | `V1` > 1), 1:3]
  }
  stopCluster(cl)
}

Segment_to_point <- function() {
  # Assign segments to the points ----------------------------------------------------------------------
    t <- tibble(round(Segments$length/190, 0))
    names(t)[1] <- "n"
    t_df <- tibble()
    for(i in 1:nrow(t)){
      t_df[i,1] <- sum(t$n[1:i]) - 100
    }
    
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  Segment_ID <- foreach(i = 1:nrow(DF), .combine = rbind) %dopar% {
    
     j = which.min(as.matrix(abs(t_df[1] - DF[i,1]))) - 5
    if(j < 1){
      j=1
    }
   

    test_condition <- TRUE
    while (test_condition == TRUE) {
      
      if (0 < gregexpr(paste(",", as.character(DF[i, 1]), ",", sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1] || 
          0 < gregexpr(paste(as.character(DF[i, 1]), ",", sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1] || 
          0 < gregexpr(paste(",", as.character(DF[i, 1]), sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1]) {
        
        test_condition <- FALSE
      } else {
        
        j <- j + 1
      }
    }
    
    Segment_ID_DF <- j - 1
    Segment_ID_DF
  }
  stopCluster(cl)

  Segment_ID <- tibble(Segments_ID_1 = Segment_ID)
  DF <- cbind(DF, Segment_ID)

  
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  Segment_ID <- foreach(i = 1:nrow(DF), .combine = rbind) %dopar% {
    
    j = which.min(as.matrix(abs(t_df[1] - DF[i,2]))) - 5
    if(j < 1){
      j=1
    }
    
    
    test_condition <- TRUE
    while (test_condition == TRUE) {
      
      if (0 < gregexpr(paste(",", as.character(DF[i, 2]), ",", sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1] || 
          0 < gregexpr(paste(as.character(DF[i, 2]), ",", sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1] || 
          0 < gregexpr(paste(",", as.character(DF[i, 2]), sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1]) {
        
        test_condition <- FALSE
      } else {
        
        j <- j + 1
      }
    }
    
    Segment_ID_DF <- j - 1
    Segment_ID_DF
  }
  stopCluster(cl)
  
  Segment_ID <- tibble(Segments_ID_2 = Segment_ID)
  DF <- cbind(DF, Segment_ID)

  Compare_ID <- apply(DF[4:5], 1, function(y) {
    y[1] == y[2]
  })
}
