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

  # Calculate distant of the point to other point in the boundery box-----------------------------------
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

  # Assign segments to the points --------------------------------------------------------------------
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  Segment_ID_1 <- foreach(i = 1:nrow(DF), .combine = rbind) %dopar% {
    if (i > 1000) {
      j <- round(DF[i, 1] / 200, 0)
    } else {
      j <- 1
    }

    while (j < nrow(Segments)) {
      if (DF[i, 1] %in% strsplit(gsub("[^[:digit:]]", ",", Segments[j, "Point IDs"]), split = ",")[[1]] == FALSE) {
        j <- j + 1
      } else {
        break
      }
    }
    Segment_ID_DF <- j - 1
    Segment_ID_DF
  }
  stopCluster(cl)
  DF <- cbind(DF, as_tibble(Segment_ID_1))

  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  Segment_ID_2 <- foreach(i = 1:nrow(DF), .combine = rbind) %dopar% {
    if (i > 1000) {
      j <- round(DF[i, 2] / 200, 0)
    } else {
      j <- 1
    }

    while (j < nrow(Segments)) {
      if (DF[i, 2] %in% strsplit(gsub("[^[:digit:]]", ",", Segments[j, "Point IDs"]), split = ",")[[1]] == FALSE) {
        j <- j + 1
      } else {
        break
      }
    }

    Segment_ID_DF <- j - 1
    Segment_ID_DF
  }
  stopCluster(cl)
  DF <- cbind(DF, as_tibble(Segment_ID_2))


  Compare_ID <- apply(DF[4:5], 1, function(y) {
    y[1] == y[2]
  })
}

Segment_to_point <- function(x) {
  system.time({
    cores <- detectCores()
    cl <- makeCluster(cores)
    registerDoParallel(cl)

    IDs <- foreach(i = 1:nrow(DF), .combine = rbind) %dopar% {
      j <- 1
      while (j < nrow(Segments)) {
        if (DF[i, 1] %in% strsplit(gsub("[^[:digit:]]", ",", Segments[i, 98]), split = ",")[[1]] == FALSE) {
          j <- j + 1
        } else {
          break
        }
      }
      i - 1
    }
    stopCluster(cl)
  })
  IDs <- as.data.frame(IDs)
  names(IDs)[1] <- "Segment_ID_1"
  DF <- cbind(DF, IDs)
  rm(IDs)


  cl <- makeCluster(cores)
  registerDoParallel(cl)

  IDs <- foreach(i = 1:nrow(DF), .combine = rbind) %dopar% {
    j <- 1
    while (j < nrow(Segments)) {
      if (DF[i, 1] %in% strsplit(gsub("[^[:digit:]]", ",", Segments[i, 98]), split = ",")[[1]] == FALSE) {
        j <- j + 1
      } else {
        break
      }
    }
    i - 1
  }
  stopCluster(cl)

  IDs <- as.data.frame(IDs)
  names(IDs)[1] <- "Segment_ID_2"
  DF <- cbind(DF, IDs)
  rm(IDs)



  Compare_ID <- apply(DF[4:5], 1, function(y) {
    y[1] == y[2]
  })

  for (i in 1:nrow(DF)) {
    if (Compare_ID[i] == TRUE) {
      DF_1[i, 1:5] <- NA
    } else {
      DF_1[i, 1:5] <- DF[i, 1:5]
    }
  }
}
