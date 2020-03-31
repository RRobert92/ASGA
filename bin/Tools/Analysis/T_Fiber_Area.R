##################################################################
# Set of functions to define fiber area and neighborhood density # 
##################################################################
## Calculate ratio of KMT_length / (-) end distance to the pole. Lower ratio, define leading KMTs
## x is an number of the column with the label name
## Y is name of the pole Pole1 or Pole2

leading_KMTsv2 <- function(x, y) {
  j = 1
  leading <- data.frame(Leading = as.numeric())
  while (j <= as.numeric(nrow(get(colnames(Segments)[x])))) {
    KMT_lenght <- Segments[as.numeric(get(colnames(Segments)[x])[j, 1] + 1), as.numeric(ncol(Segments) - 3)] / 10000
    m_end_to_pole <- data.frame(x = c(y[1, 1]),
                                y = c(y[1, 2]),
                                z = c(y[1, 3]),
                                x1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 2]),
                                y1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 3]),
                                z1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 4]))
    m_end_to_pole$distance <- apply(m_end_to_pole, 1, function(z) dist(matrix(z,
                                                                              nrow = 2,
                                                                              byrow = TRUE)))
    leading[j, ] <- KMT_lenght[1, 1] / m_end_to_pole$distance[1]
    j = j + 1
  }
  bind_cols(get(paste(colnames(Segments)[x])), leading)
}

##find point for lading KMTS, for j = j+5 == 0.5um
Leadig_Points <- function(x) {
  j = 1
  leading_points <- data.frame(Leading_ID = as.numeric())
  while (j <= nrow(get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])["Leading"])), sep = "_")))) {
    leading_points[j, ] <- get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])["Leading"])), sep = "_"))[j, 1]
    j = j + 24
  }
  leading_points <- na.omit(leading_points)
}

##find all points which corespond to the slice of the fiber
find_polygon <- function(x) {
  i = 1
  lead_points_id <- data.frame(Distance = as.numeric())
  Distance <- data.frame(V1 = as.numeric())
  
  while (i <= as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    lead_points_id <- data.frame(X_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i, ] + 1), 2],
                                 Y_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i, ] + 1), 3],
                                 Z_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i, ] + 1), 4])
    j = 1
    lead_points_id_full <- data.frame(t = as.numeric())
    
    for (j in 1:as.numeric(nrow(get(paste(colnames(Segments)[x]))))) {
      lead_points_id_full <- apply(lead_points_id, MARGIN = 2,
                                   function(y) rep(y, as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_"))))))
      
      lead_points_id_full <- cbind(get(paste(colnames(Segments)[x], j, sep = "_"))[1], lead_points_id,
                                   get(paste(colnames(Segments)[x], j, sep = "_"))[2:4])
      
      lead_points_id_full$distance <- apply(lead_points_id_full[2:7], 1, 
                                            function(x) dist(matrix(x, 
                                                                    nrow = 2, 
                                                                    byrow = TRUE)))
      
      Distance[i, j] <- lead_points_id_full[as.numeric(which.min(lead_points_id_full$distance)), 1]
      lead_points_id_full <- data.frame(t = as.numeric())
    }
    i = i + 1
  }
  bind_cols(get(paste(colnames(Segments)[x], "fiber", sep = "_")), Distance)
}

##remove duplicated points
duplicated_points <- function(x){
  DF <- get(paste(colnames(Segments)[x], "fiber",  sep = "_"))
  
  for(i in 1:ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_")))){
    DF[,i][duplicated(DF[,i])] <- NA
  }
  ##check if there is no hole in dataset if yes remove 
  for(i in 1:ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_")))){
    for(j in 2:nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_")))){
      if(is.na(DF[j-1,i]) && is.na(DF[j+2,i])){
        DF[j:nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))),i] <- NA
      }
      else{}
    }
  }
  DF
}

##get a median point for each positin and put cbin in first col
median_point <- function(x){
  ##for looop to creat df of x y z coord for eahc position
  ##mediana of x y z coord
  ## writ it in a table
  ##cbin with Pole1_00_fiber
  Median_id <- data.frame(X_Coord = as.numeric(),
                          Y_Coord = as.numeric(),
                          Z_Coord = as.numeric())
  
  for (i in 1:as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    DF <- data.frame(X_Coord = as.numeric(),
                     Y_Coord = as.numeric(),
                     Z_Coord = as.numeric())
    
    for (j in 1:as.numeric(ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
      DF[j,1] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),2]
      DF[j,2] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),3]
      DF[j,3] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),4]
    }
    
    Median_id[i,1] <- median(na.omit(DF$X_Coord))
    Median_id[i,2] <- median(na.omit(DF$Y_Coord))
    Median_id[i,3] <- median(na.omit(DF$Z_Coord))
  }
  
  DF <- cbind(Median_id, get(paste(colnames(Segments)[x], "fiber", sep = "_")))
  for (i in 1:nrow(DF)){
    if(sum(na.omit(colSums(DF[i, which(colnames(DF) == "V1"):ncol(DF)] != 0))) < 3){
      DF[i,1:12] <- NA
    } else {}
  }
  if (sum(which(is.na(DF[1]))) != 0){
    DF[-c(which(is.na(DF[1]))),]
  } else {
    DF
  }
}