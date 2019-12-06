##Load data, bin of 1000A or 0.1um
library(readxl)
library(tidyverse)

Points <- read_excel("Pulpit/Metaphase_1_KMTs.xlsx", 
                     sheet = "Points")
Points <- data.frame(Point_ID = c(Points$`Point ID`),
                     X_Coord = c(Points$`X Coord`)/10000,
                     Y_Coord = c(Points$`Y Coord`)/10000,
                     Z_Coord = c(Points$`Z Coord`)/10000)

Segments <- read_excel("Pulpit/Metaphase_1_KMTs.xlsx", 
                       sheet = "Segments")

##Define Pole1 and Pole2 position in um
Pole1 <- data.frame(X = c(3.63459), Y = c(9.58781), Z = c(2.99921))
Pole2 <- data.frame(X = c(5.03459), Y = c(2.58781), Z = c(2.79921))

##Select one fiber
Sort_by_fiber <- function(x){
 fiber <- Segments %>% filter_at(vars(starts_with(x)), any_vars(.>= 1))
 fiber %>% select(1,ncol(fiber))
}
for (i in 2:99) {
  name <- colnames(Segments)[i]
  assign(colnames(Segments)[i], Sort_by_fiber(name))
}

Select_Points <- function(x, y){
  selected_points <- data.frame(str_split(gsub("[^[:digit:]]",",",y[x,2]), pattern = ","))
  points <- data.frame(Point_ID = selected_points[,1])
}

for (i in 2:99) {
  j = 1
  while(j <= as.numeric(nrow(get(colnames(Segments)[i])))){
    assign(paste(colnames(Segments)[i], j, sep = "_"), Select_Points(j, get(colnames(Segments)[i])))
    j = j + 1
  }
}

library(plyr)
## combin point_id with xyz
nrow_1 <- seq(from = 1, to = nrow(Segments),by = 1)

Find_XYZ <- function(x){
  joined_data <- join_all(list(x,
                               Points),
                          by = "Point_ID")
  mutate_all(joined_data, function(y) as.numeric(as.character(y)))
}

for(i in 2:99){
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"), Find_XYZ(get(paste(colnames(Segments)[i], j, sep = "_"))))
    j = j + 1
  }
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

for(i in 2:99){
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"), Sort_by_distance_to_pole(get(paste(colnames(Segments)[i], j, sep = "_"))))
    j = j + 1
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

for(i in 2:99){
  assign(paste(colnames(Segments)[i]), leading_KMTs(i))
}

 ## creat table which contain points, for each plain given by i = i + 5 for leading_kmts
    ## save as (colnames(Segments)[i,], "points", sep = "_")
polygon_points <- function(x){
  ##find lead
  ##x = number of Pole1_X
  i = 1
  leading_points <- data.frame(Point_ID = as.numeric())
  while (i <= nrow(get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])[3])), sep = "_")))) {
    leading_points[i,] <- get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])[3])), sep = "_"))[i,1]
    i=i+5
  }
  leading_points <- na.omit(leading_points)
  
  ##calculate distance for each lead
  i = 1
  for (j in 1:as.numeric(nrow(get(colnames(Segments)[x])))) {
      while (i <= nrow(leading_points)) {
    ##get table
    lead_kmts <- data.frame(X_lead = Points[as.numeric(leading_points[i,] + 1), 2],
                            Y_lead = Points[as.numeric(leading_points[i,] + 1), 3],
                            Z_lead = Points[as.numeric(leading_points[i,] + 1), 4])
    lead_kmts <- matrix(rep(lead_kmts[1,1:3], each = nrow(Pole1_00_1)), nrow = nrow(Pole1_00_1))
    lead_kmts <- cbind(lead_kmts, Pole1_00_1[2:4]) 
    ##get dist
    lead_kmts$distance <- apply(lead_kmts, 1, function(x) dist(matrix(x, nrow = 2, byrow = TRUE)))
    ##put in the table
    Pole1_0_fiber[i,1] <- Pole1_00_1[as.numeric(which.min(lead_kmts$distance)),1]
    i = i + 1
  }
  Pole1_0_fiber$`Segment ID`[duplicated(Pole1_00$`Segment ID`)] <- NA
  names(Pole1_0_fiber)[1] <- "Position_1"
  }
}

  ##creat alphasphear
  ##calculate volume and area
  ##dataframe of area data/normlaized position 
##for loop for each fiber
##for(i in 2:99){
##  rm(list = paste(colnames(Segments)[i]))
##}
                                
