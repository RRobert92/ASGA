#####################################################################################
# Tool Fiber_Length_&_curvature
#
# The analysis tool to count no. of KMTs at each kinetochore
#
# x variable is a number of a column contain KMTs information (e.g. 1, 2, 3...., etc.)
# Count how many KMTs exist for each label
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-07-07
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
#####################################################################################

# Function: Find point for lading KMTs, for j = j+4 == 0.1um ------------------------
# Function: select leading KMT ------------------------------------------------------

Leadig_Pointsv2 <- function(x) {
  j <- 1
  
  leading_points <- data.frame(Leading_ID = as.numeric())

  while (j <= nrow(get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])["Leading"])), sep = "_")))) {
    leading_points[j, ] <- get(paste(colnames(Segments)[x],
      which.max(as.matrix(get(colnames(Segments)[x])["Leading"])),sep = "_" ))[j, 1]

    j <- j + 4
  }
  
  leading_points <- na.omit(leading_points)
}

# Function: Get a length of the fiber  ---------------------------------------------------------
Fiber_Length <- function(x) {
  position <- data.frame()
  for (i in 1:as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    position[i, 1] <- data.frame(x = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, 1]))
    position[i, 2] <- data.frame(y = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, 2]))
    position[i, 3] <- data.frame(z = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, 3]))
    position[i, 4] <- data.frame(x1 = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i + 1, 1]))
    position[i, 5] <- data.frame(y1 = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i + 1, 2]))
    position[i, 6] <- data.frame(z1 = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i + 1, 3]))
  }
  
  position$distance <- apply(position, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))
  position[nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))), 
           ncol(position)] <- sum(na.omit(position[1:as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))) - 1), 
                                                   ncol(position)]))
  
  DF <- cbind(position[ncol(position)], get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1:3])
  DF[nrow(DF), 1]
}

# Function: Get a total curvature  ---------------------------------------------------------
Fiber_Total_Curvature <- function(x) {
  position <- data.frame()
  position[1, 1] <- data.frame(x = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1, 1]))
  position[1, 2] <- data.frame(y = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1, 2]))
  position[1, 3] <- data.frame(z = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1, 3]))
  position[1, 4] <- data.frame(x1 = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))), 1]))
  position[1, 5] <- data.frame(y1 = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))), 2]))
  position[1, 6] <- data.frame(z1 = c(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))), 3]))
  position$dist <- apply(position, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))

  output_length <- data.frame()
  KMT <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))
  
  j <- 1

  while (j < nrow(KMT)) {
    output_length[j, 1] <- sqrt((KMT[j, 1] - KMT[j + 1, 1])^2 + (KMT[j, 2] - KMT[j + 1, 2])^2 + (KMT[j, 3] - KMT[j + 1, 3])^2)
    
    j <- j + 1
  }
  output_length <- sum(na.omit(output_length))

  ratio <- output_length / position$dist
  data.frame(
    Ratio = c(ratio),
    Fiber_Length = output_length
  )
}

# Function: Get a relative position for pole 1  ---------------------------------------------------------
relativ_pos_1_curv <- function(x) {
  Fiber <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1:3]

  relativ_pos_part1 <- lapply(
    Fiber[2],
    function(y) {Fiber[2] - Pole1[1, 2]}
  )
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y_Coord"]][["Y_Coord"]])

  relativ_pos_part2 <- Fiber[which.min(Fiber[, 2]), 2] - Pole1[1, 2]

  relativ_positon <- lapply(
    relativ_pos_part1,
    function(y) {round(relativ_pos_part1[1] / relativ_pos_part2, 2)}
  )

  relat_pos <- data.frame(Relative_Position = relativ_positon[["relativ_pos_part1...Y_Coord......Y_Coord..."]])

  names(relat_pos)[1] <- "Relative_position"

  cbind(
    Fiber,
    relat_pos
  )
}

# Function: Get a relative position for pole 2  ---------------------------------------------------------
relativ_pos_2_curv <- function(x) {
  Fiber <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1:3]

  relativ_pos_part1 <- lapply(
    Fiber[2],
    function(y) {Fiber[2] - Pole2[1, 2]}
  )
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y_Coord"]][["Y_Coord"]])

  relativ_pos_part2 <- Fiber[which.max(Fiber[, 2]), 2] - Pole2[1, 2]
  relativ_positon <- lapply(
    relativ_pos_part1,
    function(y) {round(relativ_pos_part1[1] / relativ_pos_part2, 2)}
  )

  relat_pos <- data.frame(Relative_Position = relativ_positon[["relativ_pos_part1...Y_Coord......Y_Coord..."]])

  names(relat_pos)[1] <- "Relative_position"
  cbind(
    Fiber,
    relat_pos
  )
}

# Function: Get a local curvature  ---------------------------------------------------------
Fiber_Local_Curvature <- function(x) {
  Fiber <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1:4]

  DF <- data.frame()
  i <- 1
  
  while (i < nrow(Fiber)) {
    curve_seg <- sqrt((Fiber[i, 1] - Fiber[i + 4, 1])^2 + (Fiber[i, 2] - Fiber[i + 4, 2])^2 + (Fiber[i, 3] - Fiber[i + 4, 3])^2)

    full <- data.frame()
    full_bin <- Fiber[i:as.numeric(i + 4), ]
    
    for (j in 1:nrow(full_bin)) {
      full[j, 1] <- sqrt((full_bin[j, 1] - full_bin[j + 2, 1])^2 + (full_bin[j, 2] - full_bin[j + 2, 2])^2 + (full_bin[j, 3] - full_bin[j + 2, 3])^2)
    }
    
    full <- sum(na.omit(full))
    
    DF[i, 1] <- full / curve_seg
    DF[i, 2] <- full
    DF[i, 3] <- mean(Fiber[i:as.numeric(i + 4), 4])
    
    i <- i + 4
  }
  
  DF <- na.omit(DF)

  if (nrow(DF) == 0) {

  } else {
    DF <- cbind.data.frame(
      Curvature = DF[1],
      Relative_Position = DF[3],
      K_fiber_no = paste(colnames(Segments)[x]),
      End_Position = get(paste(colnames(Segments)[x]))[1, 4],
      End_to_Pole = get(paste(colnames(Segments)[x]))[1, 5],
      Elipse_Position = get(paste(colnames(Segments)[x]))[1, 6]
    )
    names(DF)[1] <- "Curvature"
    names(DF)[2] <- "Relative_Position"
    
    DF
  }
}
