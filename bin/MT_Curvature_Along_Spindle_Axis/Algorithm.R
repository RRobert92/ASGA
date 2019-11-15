## load data
library(readxl)
library(tidyverse)
Segments <- read_excel("Pulpit/Metaphase_1_KMTs.xlsx", 
                       sheet = "Segments")
Points <- read_excel("Pulpit/Metaphase_1_KMTs.xlsx", 
                       sheet = "Points")

## select KTMs for one pole
Segments_1 <- Segments %>% filter_at(vars(starts_with("Pole1")), 
                                      any_vars(.>= 1))

##define Pole1 and Pole2 position
Pole1 <- data.frame(X = c(3.63459), Y = c(9.58781), Z = c(2.99921))
Pole2 <- data.frame(X = c(5.03459), Y = c(2.58781), Z = c(2.79921))

## count n. of columns and selected only needed one + ! end polaritis
ncol<- ncol(Segments_1)
nrow_1 <- seq(from = 1, to = nrow(Segments_1),by = 1)
Segments_1 <- Segments_1 %>% select(1,
                                      ncol-3, 
                                      ncol-2, 
                                      ncol-1,
                                      ncol)

## extract xyz coord for each points in um
Points <- data.frame(Point_ID = c(Points$`Point ID`),
                     X_Coord = c(Points$`X Coord`)/10000,
                     Y_Coord = c(Points$`Y Coord`)/10000,
                     Z_Coord = c(Points$`Z Coord`)/10000)

## creat for each fiber list of points
Select_Points <- function(x){
  selected_points <- data.frame(str_split(gsub("[^[:digit:]]",",",Segments_1[x,5]), pattern = ","))
  selected_points <- data.frame(Point_ID = selected_points[,1])
  
  ## take a no. from [1,], calculate distance to Pole1
  selected_points_nrow <- nrow(selected_points)
  x1_x2 <- (Points[as.numeric(selected_points[1,1])+1,2] - Pole1[1,1])^2
  y1_y2 <- (Points[as.numeric(selected_points[1,1])+1,3] - Pole1[1,2])^2
  z1_z2 <- (Points[as.numeric(selected_points[1,1])+1,4] - Pole1[1,3])^2
  distance_first <- sqrt(x1_x2 + y1_y2 + z1_z2)
  x11_x22 <- (Points[as.numeric(selected_points[nrow(selected_points),1])+1,2] - Pole1[1,1])^2
  y11_y22 <- (Points[as.numeric(selected_points[nrow(selected_points),1])+1,3] - Pole1[1,2])^2
  z11_z22 <- (Points[as.numeric(selected_points[nrow(selected_points),1])+1,4] - Pole1[1,3])^2
  distance_last <- sqrt(x11_x22 + y11_y22 + z11_z22)
  
  selected_points$Point_ID <- as.numeric(as.character(selected_points$Point_ID))
  if(distance_first < distance_last){
    selected_points <- data.frame(Point_ID = selected_points[order(selected_points$Point_ID, decreasing = TRUE),])
  } else {
    selected_points <- data.frame(Point_ID = selected_points[order(selected_points$Point_ID, decreasing = FALSE),])
  }
}

for (i in nrow_1) {
  name <- paste("Pole1_", i, sep = "")
  assign(name, Select_Points(i))
}

## combin point_id with xyz coord
library(plyr)
Find_XYZ <- function(x){
  join_all(list(x,
                Points),
                by = "Point_ID")
} 

for(i in nrow_1){
  name <- paste("Pole1_", i, sep = "")
  assign(name, Find_XYZ(get(name)))
}

##sort by Y_Coord
sort_by_Y_Coord <- function(x){
  x[order(x$Y_Coord),]
}

for(i in nrow_1){
  name <- paste("Pole1_", i, sep = "")
  assign(name, sort_by_Y_Coord(get(name)))
}

## relative position of point between kinetochore and pole ( 1 - 0 )
##  (n - min)
## __________
## (max - min)
relativ_pos <- function(y){
relativ_pos_part1 <- lapply(y, function(x){y[3] - y[as.numeric(which.min(y[1,3])),3]})
relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y_Coord"]][["Y_Coord"]])
relativ_pos_part2 <- lapply(y, function(x){Pole1[1,2] - y[which.min(y$Y_Coord),3]})
relativ_pos_part2 <- data.frame(relativ_pos_part2[["Y_Coord"]])
relativ_positon <- lapply(relativ_pos_part1, function(x){round(relativ_pos_part1[1] / relativ_pos_part2[1,1], 2)})
relat_pos = data.frame(Relative_Position = relativ_positon[["relativ_pos_part1...Y_Coord......Y_Coord..."]])
data.frame(Point_ID = y$Point_ID,
           Relative_Position = relat_pos$relativ_pos_part1...Y_Coord......Y_Coord...,
           X_Coord = y$X_Coord,
           Y_Coord = y$Y_Coord,
           Z_Coord = y$Z_Coord)
}
for(i in nrow_1){
  name <- paste("Pole1_", i, sep = "")
  assign(name, relativ_pos(get(name)))
}

##count a distance between first point and first point + 5 (1+5)
count_curvature <- function(x){
  
  i = 1
  output_curve <- data.frame(Curve = as.numeric())
  while (i < nrow(x)) {
    output_curve[i,] <- sqrt((x[i,3] - x[i+5,3])^2 + (x[i,4] - x[i+5,4])^2 + (x[i,5] - x[i+5,5])^2) 
    i=i+5
  }
  
  output_full <- data.frame(Full_L = as.numeric())
  i = 1
  while (i < nrow(x)){
    full1 <- sqrt((x[i,3] - x[i+1,3])^2 + (x[i,4] - x[i+1,4])^2 + (x[i,5] - x[i+1,5])^2)
    full2 <- sqrt((x[i+1,3] - x[i+2,3])^2 + (x[i+1,4] - x[i+2,4])^2 + (x[i+1,5] - x[i+2,5])^2)
    full3 <- sqrt((x[i+2,3] - x[i+3,3])^2 + (x[i+2,4] - x[i+3,4])^2 + (x[i+2,5] - x[i+3,5])^2)
    full4 <- sqrt((x[i+3,3] - x[i+4,3])^2 + (x[i+3,4] - x[i+4,4])^2 + (x[i+3,5] - x[i+4,5])^2)
    full5 <- sqrt((x[i+4,3] - x[i+5,3])^2 + (x[i+4,4] - x[i+5,4])^2 + (x[i+4,5] - x[i+5,5])^2)
    output_full[i,] <- sum(full1,full2, full3, full4, full5)
    i=i+5
  }
  
  output_mean <- data.frame(Mean_Position = as.numeric())
  i = 1  
  while(i < nrow(x)){
    output_mean[i,] <- (x[i,2] + x[i+5,2])/2
    i=i+5
  }
  cbind(Point_ID = x$Point_ID,
             Relative_Position = x$Relative_Position,
             X_Coord = x$X_Coord,
             Y_Coord = x$Y_Coord,
             Z_Coord = x$Z_Coord,
             Full_Length = output_full$Full_L,
             Curve_Length = output_curve$Curve,
             Curvature = output_full$Full_L/output_curve$Curve,
             Mean_Position = output_mean$Mean_Position)
}
count_curvature(Pole1_224)
for(i in nrow_1){
  name <- paste("Pole1_", i, sep = "")
  assign(name, count_curvature(get(name)))
}

Pole1_full <- data.frame()

for(i in nrow_1){
  name <- paste("Pole1_", i, sep = "")
  Pole1_full <- rbind.data.frame(Pole1_full, get(name))
}
Pole1_full <- data.frame(Pole1_full[complete.cases(Pole1_full), ])
write.csv(Pole1_full, "Pole1_full_2.csv") 



## save in table c("Bin"  , "Pole1_1_total_L", "Pole1_1_curv_L", "Ratio",...),
##               c(0 - 0.5,                  ,                 ,        ,...),
##                .
##                .
##                .
