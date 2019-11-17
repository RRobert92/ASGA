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
Segments_2 <- Segments %>% filter_at(vars(starts_with("Pole2")), 
                                     any_vars(.>= 1))

##define Pole1 and Pole2 position
Pole1 <- data.frame(X = c(3.63459), Y = c(9.58781), Z = c(2.99921))
Pole2 <- data.frame(X = c(5.03459), Y = c(2.58781), Z = c(2.79921))

## count n. of columns and selected only needed one + ! end polaritis
ncol<- ncol(Segments_1)
nrow_1 <- seq(from = 1, to = nrow(Segments_1),by = 1)
nrow_2 <- seq(from = 1, to = nrow(Segments_1),by = 1)
Segments_1 <- Segments_1 %>% select(1,
                                      ncol-3, 
                                      ncol-2, 
                                      ncol-1,
                                      ncol)
Segments_2 <- Segments_2 %>% select(1,
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
Select_Points <- function(x, y){
  selected_points <- data.frame(str_split(gsub("[^[:digit:]]",",",y[x,5]), pattern = ","))
  points <- data.frame(Point_ID = selected_points[,1])
  ##selected_points <- data.frame(Point_ID = selected_points[order(selected_points$Point_ID, decreasing = FALSE),])
  ## <- data.frame(Point_ID = selected_points[order(selected_points$Point_ID, decreasing = FALSE),])
}

for (i in nrow_1) {
  name <- paste("Pole1_", i, sep = "")
  assign(name, Select_Points(i, Segments_1))
}
for (i in nrow_1) {
  name <- paste("Pole2_", i, sep = "")
  assign(name, Select_Points(i, Segments_2))
}
## combin point_id with xyz
library(plyr)
Find_XYZ <- function(x){
  joined_data <- join_all(list(x,
                               Points),
                          by = "Point_ID")
  mutate_all(joined_data, function(y) as.numeric(as.character(y)))
}

for(i in nrow_1){
  name <- paste("Pole1_", i, sep = "")
  assign(name, Find_XYZ(get(name)))
}
for(i in nrow_1){
  name <- paste("Pole2_", i, sep = "")
  assign(name, Find_XYZ(get(name)))
}

##sort if y Coord[1,3] is higher then coord Y Coord of Pole_1[1,2] -> sord decreasingly
sort_by_Y_Coord <- function(x, y){
 position <- data.frame(x = c(x[1,1], x[1,1]),
                    y = c(x[1,2], x[1,2]),
                    z = c(x[1,3], x[1,3]),
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

for(i in nrow_1){
  name <- paste("Pole1_", i, sep = "")
  assign(name, sort_by_Y_Coord(Pole1, get(name)))
}
for(i in nrow_1){
  name <- paste("Pole2_", i, sep = "")
  assign(name, sort_by_Y_Coord(Pole2, get(name)))
}

## relative position of point between kinetochore and pole ( 1 - 0 ) for Pole_1
##  (n - min)  part_1
## __________
## (max - min) part_2
relativ_pos <- function(y){
relativ_pos_part1 <- lapply(y, function(x){y[3] - Pole1[1,2]})
relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y_Coord"]][["Y_Coord"]])
relativ_pos_part2 <- lapply(y, function(x){y[as.numeric(which.min(y[1,3])),3] - Pole1[1,2]})
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

## relative position of point between kinetochore and pole ( 1 - 0 ) for Pole_2
##  (n - min)  part_1
## __________
## (max - min) part_2
relativ_pos <- function(y){
  relativ_pos_part1 <- lapply(y, function(x){y[3] - y[as.numeric(which.min(y[1,3])),3]})
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y_Coord"]][["Y_Coord"]])
  relativ_pos_part2 <- lapply(y, function(x){Pole2[1,2] - y[as.numeric(which.min(y[1,3])),3]})
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
  name <- paste("Pole2_", i, sep = "")
  assign(name, relativ_pos(get(name), Pole2))
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
  cbind.data.frame(Full_Length = output_full$Full_L,
                   Curve_Length = output_curve$Curve,
                   Curvature = output_full$Full_L/output_curve$Curve,
                   Mean_Position = output_mean$Mean_Position)
}
for(i in nrow_1){
  name <- paste("Pole1_", i, sep = "")
  assign(name, count_curvature(get(name)))
}
for(i in nrow_1){
  name <- paste("Pole2_", i, sep = "")
  assign(name, count_curvature(get(name)))
}

##Bined data in one table
Pole1_full <- data.frame()
Pole2_full <- data.frame()
for(i in nrow_1){
  name <- paste("Pole1_", i, sep = "")
  Pole1_full <- rbind.data.frame(Pole1_full, get(name))
}
for(i in nrow_1){
  name <- paste("Pole2_", i, sep = "")
  Pole2_full <- rbind.data.frame(Pole2_full, get(name))
}

##Spread data for bins
Pole1_full_1.0 <- data.frame(To_0.1_c = Pole1_full[with(Pole1_full, Mean_Position < 1. & Mean_Position > 0.100),][,3],
                             To_0.1_p = Pole1_full[(Pole1_full$Mean_Position < 0.101),][,4])
Pole1_full_0.9 <- data.frame(To_0.2_c = Pole1_full[with(Pole1_full, Mean_Position < 0.201 & Mean_Position > 0.100),][,3],
                             To_0.2_p = Pole1_full[with(Pole1_full, Mean_Position < 0.201 & Mean_Position > 0.100),][,4])
Pole1_full_0.8 <- data.frame(To_0.3_c = Pole1_full[with(Pole1_full, Mean_Position < 0.301 & Mean_Position > 0.200),][,3],
                             To_0.3_p = Pole1_full[with(Pole1_full, Mean_Position < 0.301 & Mean_Position > 0.200),][,4])
Pole1_full_0.7 <- data.frame(To_0.4_c = Pole1_full[with(Pole1_full, Mean_Position < 0.401 & Mean_Position > 0.300),][,3],
                             To_0.4_p = Pole1_full[with(Pole1_full, Mean_Position < 0.401 & Mean_Position > 0.300),][,4])
Pole1_full_0.6 <- data.frame(To_0.5_c = Pole1_full[with(Pole1_full, Mean_Position < 0.501 & Mean_Position > 0.400),][,3],
                             To_0.5_p = Pole1_full[with(Pole1_full, Mean_Position < 0.501 & Mean_Position > 0.400),][,4])
Pole1_full_0.5 <- data.frame(To_0.6_c = Pole1_full[with(Pole1_full, Mean_Position < 0.601 & Mean_Position > 0.500),][,3],
                             To_0.6_p = Pole1_full[with(Pole1_full, Mean_Position < 0.601 & Mean_Position > 0.500),][,4])
Pole1_full_0.4 <- data.frame(To_0.7_c = Pole1_full[with(Pole1_full, Mean_Position < 0.701 & Mean_Position > 0.600),][,3],
                             To_0.7_p = Pole1_full[with(Pole1_full, Mean_Position < 0.701 & Mean_Position > 0.600),][,4])
Pole1_full_0.3 <- data.frame(To_0.8_c = Pole1_full[with(Pole1_full, Mean_Position < 0.801 & Mean_Position > 0.700),][,3],
                             To_0.8_p = Pole1_full[with(Pole1_full, Mean_Position < 0.801 & Mean_Position > 0.700),][,4])
Pole1_full_0.2 <- data.frame(To_0.9_c = Pole1_full[with(Pole1_full, Mean_Position < 0.901 & Mean_Position > 0.800),][,3],
                             To_0.9_p = Pole1_full[with(Pole1_full, Mean_Position < 0.901 & Mean_Position > 0.800),][,4])
Pole1_full_0.1 <- data.frame(To_1.0_c = Pole1_full[with(Pole1_full, Mean_Position < 1.001 & Mean_Position > 0.900),][,3],
                             To_1.0_p = Pole1_full[with(Pole1_full, Mean_Position < 1.001 & Mean_Position > 0.900),][,4])
Pole1_full_0.0 <- data.frame(To_1.1_c = Pole1_full[with(Pole1_full, Mean_Position < 1.101 & Mean_Position > 1.000),][,3],
                             To_1.1_p = Pole1_full[with(Pole1_full, Mean_Position < 1.101 & Mean_Position > 1.000),][,4])
Pole1_full_m0.1 <- data.frame(To_1.2_c = Pole1_full[with(Pole1_full, Mean_Position < 1.201 & Mean_Position > 1.100),][,3],
                             To_1.2_p = Pole1_full[with(Pole1_full, Mean_Position < 1.201 & Mean_Position > 1.100),][,4])
Pole1_full_m0.2 <- data.frame(To_1.3_c = Pole1_full[with(Pole1_full, Mean_Position < 1.301 & Mean_Position > 1.200),][,3],
                             To_1.3_p = Pole1_full[with(Pole1_full, Mean_Position < 1.301 & Mean_Position > 1.200),][,4])
##Plot
ggplot() + geom_boxplot(aes(Pole1_full_0.1$To_0.1_p, Pole1_full_0.1$To_0.1_c)) + geom_boxplot(
                              aes(Pole1_full_0.2$To_0.2_p, Pole1_full_0.2$To_0.2_c)) + geom_boxplot(
                                aes(Pole1_full_0.3$To_0.3_p, Pole1_full_0.3$To_0.3_c)) + geom_boxplot(
                                  aes(Pole1_full_0.4$To_0.4_p, Pole1_full_0.4$To_0.4_c)) + geom_boxplot(
                                    aes(Pole1_full_0.5$To_0.5_p, Pole1_full_0.5$To_0.5_c)) + geom_boxplot(
                                      aes(Pole1_full_0.6$To_0.6_p, Pole1_full_0.6$To_0.6_c)) + geom_boxplot(
                                        aes(Pole1_full_0.7$To_0.7_p, Pole1_full_0.7$To_0.7_c)) + geom_boxplot(
                                          aes(Pole1_full_0.8$To_0.8_p, Pole1_full_0.8$To_0.8_c)) + geom_boxplot( 
                                            aes(Pole1_full_0.9$To_0.9_p, Pole1_full_0.9$To_0.9_c)) + geom_boxplot(
                                              aes(Pole1_full_1.0$To_1.0_p, Pole1_full_1.0$To_1.0_c)) + geom_boxplot(
                                                aes(Pole1_full_1.1$To_1.1_p, Pole1_full_1.1$To_1.1_c)) + geom_boxplot( 
                                                  aes(Pole1_full_1.2$To_1.2_p, Pole1_full_1.2$To_1.2_c)) + geom_boxplot(
                                                    aes(Pole1_full_1.3$To_1.3_p, Pole1_full_1.3$To_1.3_c))

##Marge data
Pole1_full_bins <- rbind.fill(Pole1_full_0.1, 
                              Pole1_full_0.2, 
                              Pole1_full_0.3, 
                              Pole1_full_0.4,
                              Pole1_full_0.5,
                              Pole1_full_0.6,
                              Pole1_full_0.7,
                              Pole1_full_0.8,
                              Pole1_full_0.9,
                              Pole1_full_1.0,
                              Pole1_full_1.1,
                              Pole1_full_1.2,
                              Pole1_full_1.3)

Pole1_full_bins <- data.frame(Pole1_full_bins[complete.cases(Pole1_full_bins), ])
write.csv(Pole1_full_bins, "Pole1_full_bins.csv")
