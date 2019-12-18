##Load data, bin of 1000A or 0.1um
library(readxl)
library(tidyverse)

Points <- read_excel("C:/Users/kerth/Desktop/Metaphase_1.am.xlsx", 
                     sheet = "Points")
Points <- data.frame(Point_ID = c(Points$`Point ID`),
                     X_Coord = c(Points$`X Coord`)/10000,
                     Y_Coord = c(Points$`Y Coord`)/10000,
                     Z_Coord = c(Points$`Z Coord`)/10000)

Segments <- read_excel("C:/Users/kerth/Desktop/Metaphase_1.am.xlsx", 
                       sheet = "Segments")

##Define Pole1 and Pole2 position in um
Pole1 <- data.frame(X = c(3.63459), Y = c(9.58781), Z = c(2.99921))
Pole2 <- data.frame(X = c(5.03459), Y = c(2.58781), Z = c(2.79921))

##Select one fiber
Sort_by_fiber <- function(x){
  fiber <- Segments %>% filter_at(vars(starts_with(x)), any_vars(.>= 1))
  fiber %>% select(1,ncol(fiber))
}

## remove "," and spread numbers in single cells
Select_Points <- function(x, y){
  selected_points <- data.frame(str_split(gsub("[^[:digit:]]",",",y[x,2]), pattern = ","))
  points <- data.frame(Point_ID = selected_points[,1])
}

## combin point_id with xyz
nrow_1 <- seq(from = 1, to = nrow(Segments),by = 1)
library(plyr)
Find_XYZ <- function(x){
  joined_data <- join_all(list(x,
                               Points),
                          by = "Point_ID")
  mutate_all(joined_data, function(y) as.numeric(as.character(y)))
}

##sort point by there position in spindle pole axis first point is kinetochore
Sort_by_distance_to_pole <- function(y){
  position <- data.frame(x = c(Pole1[1,1], Pole1[1,1]),
                         y = c(Pole1[1,2], Pole1[1,2]),
                         z = c(Pole1[1,3], Pole1[1,3]),
                         x1 = c(y[1,2], y[nrow(y),2]),
                         y1 = c(y[1,3], y[nrow(y),3]),
                         z1 = c(y[1,4], y[nrow(y),4]))
  
  position$distance <- apply(position, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))
  
  if(position[1,7] < position[2,7]){
    y %>% arrange(desc(Point_ID))
  } else {
    y %>% arrange(Point_ID)
  }
}

##function per fiber
## leading_KMTs <- find longest KMTs
leading_KMTs <- function(x){
  j = 1
  leading <- data.frame(Leading = as.numeric())
  while (j <= as.numeric(nrow(get(colnames(Segments)[x])))) {
    leading[j,] <- as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_"))))
    j = j + 1
  }
  bind_cols(get(paste(colnames(Segments)[x])), leading)
}

##Calculate ratio of KMT_length / (-) end distance to the pole
leading_KMTsv2 <- function(x, y){
  j = 1
  leading <- data.frame(Leading = as.numeric())
  while (j <= as.numeric(nrow(get(colnames(Segments)[x])))) {
    KMT_lenght <- Segments[as.numeric(get(colnames(Segments)[x])[j,1] + 1),as.numeric(ncol(Segments) - 3)]/10000
    m_end_to_pole <- data.frame(x = c(y[1,1]),
                                y = c(y[1,2]),
                                z = c(y[1,3]),
                                x1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))),2]),
                                y1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))),3]),
                                z1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))),4])
    )
    m_end_to_pole$distance <- apply(m_end_to_pole, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))
    leading[j,] <- KMT_lenght[1,1] / m_end_to_pole$distance[1]
    j = j + 1
  }
  bind_cols(get(paste(colnames(Segments)[x])), leading)  
}
##find point for lading KMTS, for i = i+5 == 0.5um
Leadig_Points <- function(x){
  i = 1
  leading_points <- data.frame(Leading_ID = as.numeric())
  while (i <= nrow(get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])[3])), sep = "_")))) {
    leading_points[i,] <- get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])[3])), sep = "_"))[i,1]
    i=i+5
  }
  leading_points <- na.omit(leading_points)
}

find_polygon <- function(x){
  ## take n+1 point from the lead, rep for n= nrow, make a matrix
  ##cont a distance between each points 
  ##find the closest point 
  ## write ID in a table
  ## left only unique rest is NA 
  #bind with lead
  i = 1
  lead_points_id <- data.frame(Distance = as.numeric())
  while (i <= as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    lead_points_id <- data.frame(X_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i,] + 1), 2],
                                 Y_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i,] + 1), 3],
                                 Z_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i,] + 1), 4])
    j = 1
    for (j in 1:as.numeric(nrow(get(paste(colnames(Segments)[x]))))) {
      lead_points_id <- apply(lead_points_id, MARGIN = 2, function(y) rep(y,as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_"))))))
      lead_points_id <- cbind(get(paste(colnames(Segments)[x], j, sep = "_"))[1], lead_points_id, get(paste(colnames(Segments)[x], j, sep = "_"))[2:4])
      lead_points_id$distance <- apply(lead_points_id[2:7], 1, function(x) dist(matrix(x, nrow = 2, byrow = TRUE)))
      ##find the closest point 
      ## write ID in a table
      ## left only unique rest is NA
      }
  }
 
  
}

##4->101
for (i in 2:as.numeric(ncol(Segments)-101)) {
  ##find individual fiber
  assign(colnames(Segments)[i], Sort_by_fiber(colnames(Segments)[i]))
  ##select individual KMTs from fiber naming it POleX_YY_ZZ
  ##X  - pole 1 or 3
  ##YY - number of fiber
  ##ZZ - number of KMTs in the fiber
  j = 1
  while(j <= as.numeric(nrow(get(colnames(Segments)[i])))){
    assign(paste(colnames(Segments)[i], j, sep = "_"), Select_Points(j, get(colnames(Segments)[i])))
    j = j + 1
  }
  
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"), Find_XYZ(get(paste(colnames(Segments)[i], j, sep = "_"))))
    j = j + 1
  }
  ##Sort points in the individual fiber, make 1 point in df corespond to (+) end
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"), Sort_by_distance_to_pole(get(paste(colnames(Segments)[i], j, sep = "_"))))
    j = j + 1
  }
}
##find leading KMTs in the fiber ..... 1->48
for (i in 2:as.numeric(which(colnames(Segments) == "Pole2_00") - 49)) {
  assign(paste(colnames(Segments)[i]), leading_KMTsv2(i, Pole1))
}
for (i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments)-4)) {
  assign(paste(colnames(Segments)[i]), leading_KMTsv2(i, Pole2))
}
##find leading poits for each fiber, creat new frame Segments[i]_fiber
for (i in 2) {
  assign(paste(colnames(Segments)[i], "fiber", sep = "_"), Leadig_Points(i))
}

