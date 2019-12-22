##Load data, bin of 1000A or 0.1um
library(readxl)
library(tidyverse)

Points <- read_excel("C:/Users/kerth/Desktop/Metaphase_1.am.xlsx", 
                     sheet = "Points")
Points <- data.frame(Point_ID = c(Points$`Point ID`),
                     X_Coord = c(Points$`X Coord`) / 10000,
                     Y_Coord = c(Points$`Y Coord`) / 10000,
                     Z_Coord = c(Points$`Z Coord`) / 10000)

Segments <- read_excel("C:/Users/kerth/Desktop/Metaphase_1.am.xlsx", 
                       sheet = "Segments")

##Define Pole1 and Pole2 position in um
Pole1 <- data.frame(X = c(3.63459),
                    Y = c(9.58781),
                    Z = c(2.99921))
Pole2 <- data.frame(X = c(5.03459),
                    Y = c(2.58781),
                    Z = c(2.79921))

##Select one fiber
Sort_by_fiber <- function(x) {
  fiber <- Segments %>% filter_at(vars(starts_with(x)), 
                                  any_vars(. >= 1))
  fiber %>% select(1, 
                   ncol(fiber))
}

## remove "," and spread numbers in single cells
Select_Points <- function(x, y) {
  selected_points <- data.frame(str_split(gsub("[^[:digit:]]", 
                                               ",", 
                                               y[x, 2]), 
                                          pattern = ","))
  points <- data.frame(Point_ID = selected_points[, 1])
}

## combin point_id with xyz
nrow_1 <- seq(from = 1,
              to = nrow(Segments),
              by = 1)

library(plyr)
Find_XYZ <- function(x) {
  joined_data <- join_all(list(x,
                               Points),
                          by = "Point_ID")
  mutate_all(joined_data, 
             function(y) as.numeric(as.character(y)))
}

##sort point by there position in spindle pole axis first point is kinetochore
Sort_by_distance_to_pole <- function(y) {
  position <- data.frame(x = c(Pole1[1, 1], 
                               Pole1[1, 1]),
                         y = c(Pole1[1, 2],
                               Pole1[1, 2]),
                         z = c(Pole1[1, 3], 
                               Pole1[1, 3]),
                         x1 = c(y[1, 2], 
                                y[nrow(y), 2]),
                         y1 = c(y[1, 3], 
                                y[nrow(y), 3]),
                         z1 = c(y[1, 4], 
                                y[nrow(y), 4]))
  
  position$distance <- apply(position, 1, 
                             function(z) dist(matrix(z, 
                                                     nrow = 2, 
                                                     byrow = TRUE)))
  
  if (position[1, 7] < position[2, 7]) {
    y %>% arrange(desc(Point_ID))
  } else {
    y %>% arrange(Point_ID)
  }
}

##function per fiber
## leading_KMTs <- find longest KMTs
leading_KMTs <- function(x) {
  j = 1
  leading <- data.frame(Leading = as.numeric())
  while (j <= as.numeric(nrow(get(colnames(Segments)[x])))) {
    leading[j, ] <- as.numeric(nrow(get(paste(colnames(Segments)[x], 
                                              j, 
                                              sep = "_"))))
    j = j + 1
  }
  bind_cols(get(paste(colnames(Segments)[x])), 
            leading)
}

##Calculate ratio of KMT_length / (-) end distance to the pole
leading_KMTsv2 <- function(x, y) {
  j = 1
  leading <- data.frame(Leading = as.numeric())
  while (j <= as.numeric(nrow(get(colnames(Segments)[x])))) {
    KMT_lenght <- Segments[as.numeric(get(colnames(Segments)[x])[j, 1] + 1), 
                           as.numeric(ncol(Segments) - 3)] / 10000
    m_end_to_pole <- data.frame(x = c(y[1, 1]),
                                y = c(y[1, 2]),
                                z = c(y[1, 3]),
                                x1 = c(get(paste(colnames(Segments)[x], 
                                                 j, 
                                                 sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 2]),
                                y1 = c(get(paste(colnames(Segments)[x], 
                                                 j, 
                                                 sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 3]),
                                z1 = c(get(paste(colnames(Segments)[x], 
                                                 j, 
                                                 sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 4]))
    m_end_to_pole$distance <- apply(m_end_to_pole, 
                                    1, 
                                    function(z) dist(matrix(z, 
                                                            nrow = 2, 
                                                            byrow = TRUE)))
    leading[j, ] <- KMT_lenght[1, 1] / m_end_to_pole$distance[1]
    j = j + 1
  }
  bind_cols(get(paste(colnames(Segments)[x])), 
            leading)
}
##find point for lading KMTS, for i = i+5 == 0.5um
Leadig_Points <- function(x) {
  i = 1
  leading_points <- data.frame(Leading_ID = as.numeric())
  while (i <= nrow(get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])[3])), sep = "_")))) {
    leading_points[i, ] <- get(paste(colnames(Segments)[x], 
                                     which.max(as.matrix(get(colnames(Segments)[x])[3])), 
                                     sep = "_"))[i, 1]
    i = i + 5
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
      lead_points_id_full <- apply(lead_points_id, 
                                   MARGIN = 2,
                                   function(y) rep(y, 
                                                   as.numeric(nrow(get(paste(colnames(Segments)[x], 
                                                                             j, 
                                                                             sep = "_"))))))
      lead_points_id_full <- cbind(get(paste(colnames(Segments)[x], 
                                             j,
                                             sep = "_"))[1],
                                   lead_points_id,
                                   get(paste(colnames(Segments)[x], 
                                             j,
                                             sep = "_"))[2:4])
      lead_points_id_full$distance <- apply(lead_points_id_full[2:7], 
                                            1, 
                                            function(x) dist(matrix(x, 
                                                                    nrow = 2, 
                                                                    byrow = TRUE)))
      Distance[i, j] <- lead_points_id_full[as.numeric(which.min(lead_points_id_full$distance)), 1]
      lead_points_id_full <- data.frame(t = as.numeric())
    }
    i = i + 1
  }
  bind_cols(get(paste(colnames(Segments)[x], 
                      "fiber", 
                      sep = "_")), 
            Distance)
}
##remove duplicated points
duplicated_points <- function(x){
  DF <- get(paste(colnames(Segments)[x], 
                  "fiber", 
                  sep = "_"))
  for(i in 1:ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_")))){
    DF[,i][duplicated(DF[,i])] <- NA
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
      DF[j,1] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]),2]
      DF[j,2] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]),3]
      DF[j,3] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]),4]
    }
    Median_id[i,1] <- median(na.omit(DF$X_Coord))
    Median_id[i,2] <- median(na.omit(DF$Y_Coord))
    Median_id[i,3] <- median(na.omit(DF$Z_Coord))
  }
  cbind(Median_id, 
        get(paste(colnames(Segments)[x], 
                             "fiber", 
                  sep = "_")))
}
##count geometry and find polygon area
circular_area <- function(x){
  ##calc distance of a point to the median point
  ##find largest distance
  #calculate area of a circle
  ## store data in POle1_00_fiber$Circl_Area
  Point_id <- data.frame(X_Coord = as.numeric(),
                         Y_Coord = as.numeric(),
                         Z_Coord = as.numeric())
  for (i in 1:as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    DF <- data.frame(X_Coord = as.numeric(),
                     Y_Coord = as.numeric(),
                     Z_Coord = as.numeric())
    for (j in 4:as.numeric(ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
      DF[j,1] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]),2]
      DF[j,2] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]),3]
      DF[j,3] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]),4]
    }
  
  }
  
}
library(alphashape3d)
polygon_area <- function(x){
  
}

##4->101
library(tcltk)
total <- as.numeric(ncol(Segments) - 4)
pb <- winProgressBar(title = "Progress",
                    min = 0,
                    max =  total,
                    width = 300)
for (i in 2:as.numeric(ncol(Segments) - 4)) {
  ##find individual fiber
  assign(colnames(Segments)[i], 
         Sort_by_fiber(colnames(Segments)[i]))
  ##select individual KMTs from fiber naming it POleX_YY_ZZ
  ##X  - pole 1 or 3
  ##YY - number of fiber
  ##ZZ - number of KMTs in the fiber
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], 
                 j, 
                 sep = "_"), 
           Select_Points(j, 
                         get(colnames(Segments)[i])))
    j = j + 1
  }
  
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], 
                 j, 
                 sep = "_"),
           Find_XYZ(get(paste(colnames(Segments)[i], 
                              j, 
                              sep = "_"))))
    j = j + 1
  }
  ##Sort points in the individual fiber, make 1 point in df corespond to (+) end
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], 
                 j, 
                 sep = "_"),
           Sort_by_distance_to_pole(get(paste(colnames(Segments)[i], 
                                              j, 
                                              sep = "_"))))
    j = j + 1
  }
  Sys.sleep(0.1)
  setWinProgressBar(pb, 
                    i, 
                    label = paste(round(i / total * 100, 
                                        0), 
                                  "% Done"))
}
close(pb)
##find leading KMTs in the fiber ..... 1->49
for (i in 2:as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
  assign(paste(colnames(Segments)[i]), 
         leading_KMTsv2(i, 
                        Pole1))
}
for (i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments) - 4)) {
  assign(paste(colnames(Segments)[i]), 
         leading_KMTsv2(i, 
                        Pole2))
}
pb <- winProgressBar(title = "Progress",
                    min = 0,
                    max =  total,
                    width = 300)
for (i in 2:99) {
  ##find leading poits for each fiber, creat new frame Segments[i]_fiber
  assign(paste(colnames(Segments)[i], 
               "fiber", sep = "_"), 
         Leadig_Points(i))
  ##find points which correspond to the leading fier
  assign(paste(colnames(Segments)[i], 
               "fiber", sep = "_"),
         find_polygon(i))
  ##Remove all duplicates
  assign(paste(colnames(Segments)[i], 
               "fiber", sep = "_"), 
         duplicated_points(i))
  ##for each position find medan point
  assign(paste(colnames(Segments)[i], 
               "fiber", sep = "_"),
         median_point(i))
  Sys.sleep(0.1)
  setWinProgressBar(pb, 
                    i, 
                    label = paste(round(i / total * 100, 
                                        0), 
                                  "% Done"))
}
close(pb)
