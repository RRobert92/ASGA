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
  system.time({
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

      DF_1$Segment_ID_1 <- apply(DF_1[1], 1, function(y) {
        i <- 1
        while (i < nrow(Segments)) {
          if (y[1] %in% strsplit(gsub("[^[:digit:]]", ",", Segments[i, 98]), split = ",")[[1]] == FALSE) {
            i <- i + 1
          } else {
            break
          }
        }
        i - 1
      })

      DF_1$Segment_ID_2 <- apply(DF_1[2], 1, function(y) {
        i <- 1
        while (i < nrow(Segments)) {
          if (y[1] %in% strsplit(gsub("[^[:digit:]]", ",", Segments[i, 98]), split = ",")[[1]] == FALSE) {
            i <- i + 1
          } else {
            break
          }
        }
        i - 1
      })
      Compare_ID <- apply(DF_1[4:5], 1, function(y) {
        y[1] == y[2]
      })

      for (i in 1:nrow(DF_1)) {
        if (Compare_ID[i] == TRUE) {
          DF_1[i, 1:5] <- NA
        }
      }
      DF_1
    }
    stopCluster(cl)
    DF <- na.omit(DF)
  })
}

Bridging_MT <- function(x) {

}
