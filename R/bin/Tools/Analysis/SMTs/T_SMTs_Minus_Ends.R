########################################################################################################
# Tool SMTs_Minus_Ends
#
# Function to calculate distance and relative position of SMTs minus ends in the spindle
# Firstly, this tool is defining the minus-end of SMT based on the distance to the closest pole
# After that, 3D Euclidean distance and relative position on the spindle pole axis
# are calculated for each SMTs
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-02-11
# Reviewed:
########################################################################################################


SMT_Minus_Ends <- function() {
  # For each entrance in Segments_SMT collect and stored Node_1 and Node_2
  SMT_Ends <- tibble()
  for (i in 1:nrow(Segments_SMT)) {
    Node_1 <- as.numeric(Segments_SMT[i, "Node ID #1"])
    Node_1 <- filter(Nodes, `Node ID` == Node_1)[2:4]
    names(Node_1)[1:3] <- c("X.Coord", "Y.Coord", "Z.Coord")

    Node_2 <- as.numeric(Segments_SMT[i, "Node ID #2"])
    Node_2 <- filter(Nodes, `Node ID` == Node_2)[2:4]
    names(Node_2)[1:3] <- c("X.Coord", "Y.Coord", "Z.Coord")
    DF <- tibble()

    # Calculate distance to both poles for each Node_1 and _2
    DF[1, 1] <- as.numeric(dist(rbind(Node_1, Pole1), method = "euclidean")) # Pole1 Node1
    DF[2, 1] <- as.numeric(dist(rbind(Node_1, Pole2), method = "euclidean")) # Pole1 Node2

    DF[3, 1] <- as.numeric(dist(rbind(Node_2, Pole1), method = "euclidean")) # Pole2 Node1
    DF[4, 1] <- as.numeric(dist(rbind(Node_2, Pole2), method = "euclidean")) # Pole2 Node2
    Minus_end <- which.min(DF$...1)
    Dist <- DF[Minus_end, 1]

    # select closest as minus-end
    if (Minus_end == 1 || Minus_end == 2) {
      Minus_end <- tibble(
        Node_1,
        Segments_SMT[i, "Node ID #1"]
      )
    }
    if (Minus_end == 3 || Minus_end == 4) {
      Minus_end <- tibble(
        Node_2,
        Segments_SMT[i, "Node ID #2"]
      )
    }

    # Calculate position on the spindle pole axis between two poles 0 (Pole1) 1 (Pole2)
    if (Pole1[1, 2] < Pole2[1, 2]) {
      Relative_position <- (Minus_end$Y.Coord - Pole1[1, 2]) / (Pole2[1, 2] - Pole1[1, 2])
    } else {
      Relative_position <- (Minus_end$Y.Coord - Pole2[1, 2]) / (Pole1[1, 2] - Pole2[1, 2])
    }


    SMT_Ends[i, 1:4] <- tibble(
      Segment_ID = as.numeric(Minus_end[4]),
      Distance_to_Pole = as.numeric(Dist[1]),
      Relativ_Position = as.numeric(Relative_position),
      Length = as.numeric(Segments_SMT[i, "length"]/10000)
    )
  }

  return(SMT_Ends)
}
