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
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################################

# Function: to calculate kinetochore size  -----------------------------------------------------
Kinetochore_Size <- function(x) {
  DF <- select(get(paste(colnames(Segments)[x], "fiber", sep = "_")), starts_with("V"))

  Mean_DF <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1:3]

  # Calculate distance of each KMT plus end to the center of the k-fiber
  dist <- data.frame()
  for (j in 1:ncol(DF)) {
    dist[j, 1] <- Points[as.numeric(DF[2, j] + 1), "X Coord"]
    dist[j, 2] <- Points[as.numeric(DF[2, j] + 1), "Y Coord"]
    dist[j, 3] <- Points[as.numeric(DF[2, j] + 1), "Z Coord"]
  }

  dist <- na.omit(dist)
  dist[4:6] <- Mean_DF[2, 1:3]
  dist$distance <- apply(
    dist,
    1,
    function(y) {
      dist(matrix(y, nrow = 2, byrow = TRUE))
    }
  )

  # Calculate fiber radius based on max distance of KMTs to the center
  fiber_radius <- as.numeric(max(dist$distance))

  # calculate density KMTs/um^2
  density_packing <- round(nrow(dist) / (pi*fiber_radius^2), 1)

  # Calculate distance between KMTs with fast nearest neighbour search
  neighbour_matrix <- data.frame()

  all_points <- data.frame(dist[1], dist[3])
  names(all_points)[1:2] <- c("X", "Y")
  df <- all_points %>% mutate(k = 1)
  df <- df %>%
    full_join(df, by = "k") %>%
    mutate(dist = sqrt((X.x - X.y)^2 + (Y.x - Y.y)^2)) %>%
    select(-k)

  # J indicate each MT
  start = 1
  iter = nrow(dist)
  end = iter
  for(j in 1:nrow(dist)){
      neighbour_matrix[1:nrow(dist), j] <- as.data.frame(df[start:end, "dist"])
      start = end + 1
      end = end + iter
  }


  neighbours <- data.frame(matrix(0,
                          nrow = nrow(neighbour_matrix),
                          ncol = nrow(neighbour_matrix)))

  for(j in 1:ncol(neighbour_matrix)){
      closest = which.min(as.numeric(neighbour_matrix[j,-j]))
      if(closest >= j){
          closest = closest + 1
      }
      neighbours[j, closest] <- neighbours[j, closest] + 1
  }
  for(j in 1:nrow(neighbour_matrix)){
      closest = which.min(as.numeric(neighbour_matrix[-j, j]))
      if(closest >= j){
          closest = closest + 1
      }
      neighbours[closest, j] <- 1
  }

  # get distance between neighbors
  neigbour_dist <- data.frame()
  n = 1
  for(j in 1:ncol(neighbours)){
      list_of_neigbours = which(neighbours[j] == 1)
      for(k in list_of_neigbours){
          neigbour_dist[n, 1]  <- neighbour_matrix[k, j]
          n = n + 1
      }
  }

  DF <- data.frame(
    Kinetochore_area = as.numeric(pi * fiber_radius^2),
    KMT_no = as.numeric(nrow(get(paste(colnames(Segments)[x])))),
    Fiber_name = get(paste(colnames(Segments)[x]))[1, "Fiber_Name"],
    Plus_dist_to_pole = get(paste(colnames(Segments)[x]))[1, "plus_dist_to_pole"],
    Elipse_position = get(paste(colnames(Segments)[x]))[1, "Elipse_Position"],
    KMT_density = as.numeric(density_packing),
    neigbour_mean = round(mean(neigbour_dist$V1), 3),
    neigbour_std = round(sd(neigbour_dist$V1), 3)
  )

  DF
}
