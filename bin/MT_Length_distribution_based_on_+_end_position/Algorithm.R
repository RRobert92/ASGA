############################################################################################################################
# Length distribution analysis based on the plus end distance to the pole or position of the plus end in a metaphase plate #
############################################################################################################################

###########
# Library #
###########
library(readxl)
library(tidyverse)
library(plyr)
library(tcltk) 
library(ggpubr)
library(ggplot2)

############
# Settings #
############
Points <- read_excel("H:/Robert/Metaphase_1_KMTs.resampled.rotated.2_75.0.-9_55.resampled200.xlsx", 
                     sheet = "Points")
Segments <- read_excel("H:/Robert/Metaphase_1_KMTs.resampled.rotated.2_75.0.-9_55.resampled200.xlsx",
                       sheet = "Segments")
Nodes <- read_excel("H:/Robert/Metaphase_1_KMTs.resampled.rotated.2_75.0.-9_55.resampled200.xlsx",
                    sheet = "Nodes")

Pole1 <- "Pole1" ## Name of the label for the Pole1 in the Node section
Pole2 <- "Pole2" ## Name of the label for the Pole2 in the Node section

#############
# Functions #
#############
##Select one fiber
Sort_by_fiber <- function(x) {
  fiber <- Segments %>% filter_at(vars(starts_with(x)), any_vars(. >= 1))
  fiber %>% select(1, which(colnames(Segments) == "Point IDs"), which(colnames(Segments) == "length")) 
}

## remove "," and spread numbers in single cells
Select_Points <- function(x, y) {
  selected_points <- data.frame(str_split(gsub("[^[:digit:]]", ",", y[x, 2]), pattern = ","))
  points <- data.frame(Point_ID = selected_points[, 1])
}

## combin point_id with xyz
Find_XYZ <- function(x) {
  joined_data <- join_all(list(x, Points),
                          by = "Point_ID")
  mutate_all(joined_data, 
             function(y) as.numeric(as.character(y)))
}

##sort point by there position in spindle pole axis first point is kinetochore
Sort_by_distance_to_pole1 <- function(y){
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

##sort point by there position in spindle pole axis first point is kinetochore
Sort_by_distance_to_pole2 <- function(y){
  position <- data.frame(x = c(Pole2[1,1], Pole2[1,1]),
                         y = c(Pole2[1,2], Pole2[1,2]),
                         z = c(Pole2[1,3], Pole2[1,3]),
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

##find median of the (+) ends and calculate distance to the pole
##find (-) end distance to the pole
##find length of KMTs and marge table
Analyse_LD <- function(x, y){ ##  x <- KMTs ID "1, 2, 3...", y <- Pole1 or Pole2..
  Plus_end <- data.frame()
 for (i in 1:nrow(get(colnames(Segments)[x]))){
   Plus_end[i,1:3] <- get(paste(colnames(Segments)[x], i, sep = "_"))[1,2:4]
 }
  Plus_end <- data.frame(X_Median = c(median(as.matrix(Plus_end[1]))),
                   Y_Median = c(median(as.matrix(Plus_end[2]))),
                   Z_Median = c(median(as.matrix(Plus_end[3]))))
  Bind_Data <- data.frame()
  Plus_Distst_to_the_pole <- sqrt((y[1,1] - (Plus_end[1,1]))^2 + (y[1,2] - (Plus_end[1,2]))^2 + (y[1,3] - (Plus_end[1,3]))^2)
  for (i in 1:nrow(get(colnames(Segments)[x]))){
  Minus_end <- paste(colnames(Segments)[x], i, sep = "_")
  Minus_Distst_to_the_pole <- sqrt((y[1,1] - (get(Minus_end)[nrow(get(Minus_end)),2]))^2 + (y[1,2] - (get(Minus_end)[nrow(get(Minus_end)),3]))^2 + (y[1,3] - (get(Minus_end)[nrow(get(Minus_end)),4]))^2)
  Bind_Data [i,1] <- get(colnames(Segments)[x])[i,3]/10000
  Bind_Data [i,2] <- Minus_Distst_to_the_pole
  Bind_Data [i,3] <- Plus_Distst_to_the_pole
  }
  names(Bind_Data)[1] <- "length"
  names(Bind_Data)[2] <- "minus_dist_to_pole"
  names(Bind_Data)[3] <- "plus_dist_to_pole"
  Bind_Data
}
  
## Relative postion of points between kinetochore and the pole1
Relativ_Pos_1 <- function(x){##  x <- KMTs ID "1, 2, 3..."
  relativ_position_fiber<- data.frame()
  for (i in 1:nrow(get(colnames(Segments)[x]))){
    relativ_pos_part1 <- get(paste(colnames(Segments)[x], i, sep = "_"))[nrow(get(paste(colnames(Segments)[x], i, sep = "_"))),3] - Pole1[1,2]
    relativ_pos_part2 <- get(paste(colnames(Segments)[x], i, sep = "_"))[which.min(get(paste(colnames(Segments)[x], i, sep = "_"))[1,3]),3] - Pole1[1,2]
    relativ_position_fiber[i,1] <- round(relativ_pos_part1 / relativ_pos_part2, 2)
  }
  All <- cbind(get(paste(colnames(Segments)[x])),
               relativ_position_fiber)
  names(All)[4] <- "relative_pos"
  All
}

## Relative postion of points between kinetochore and the pole2
Relativ_Pos_2 <- function(x){
  relativ_position_fiber<- data.frame()
  for (i in 1:nrow(get(colnames(Segments)[x]))){
    relativ_pos_part1 <- get(paste(colnames(Segments)[x], i, sep = "_"))[nrow(get(paste(colnames(Segments)[x], i, sep = "_"))),3] - Pole2[1,2]
    relativ_pos_part2 <- get(paste(colnames(Segments)[x], i, sep = "_"))[which.max(get(paste(colnames(Segments)[x], i, sep = "_"))[1,3]),3] - Pole2[1,2]
    relativ_position_fiber[i,1] <- round(relativ_pos_part1 / relativ_pos_part2, 2)
  }
  All <- cbind(get(paste(colnames(Segments)[x])),
               relativ_position_fiber)
  names(All)[4] <- "relative_pos"
  All
}

#############
# Load Data #
#############
Points <- data.frame(Point_ID = c(Points$`Point ID`),
                     X_Coord = c(Points$`X Coord`)/10000,
                     Y_Coord = c(Points$`Y Coord`)/10000,
                     Z_Coord = c(Points$`Z Coord`)/10000)
Pole1 <- Nodes %>% filter_at(vars(Pole1),
                             any_vars(.>=1))
Pole1 <- data.frame(X = c(Pole1 %>% select("X Coord")/10000), 
                    Y = c(Pole1 %>% select("Y Coord")/10000), 
                    Z = c(Pole1 %>% select("Z Coord")/10000))
Pole2 <- Nodes %>% filter_at(vars(Pole2),
                             any_vars(.>=1))
Pole2 <- data.frame(X = c(Pole2 %>% select("X Coord")/10000), 
                    Y = c(Pole2 %>% select("Y Coord")/10000), 
                    Z = c(Pole2 %>% select("Z Coord")/10000))

############
# Analysis #
############
total <- as.numeric(ncol(Segments) - 4)
pb <- tkProgressBar(title = "Finding KMTs for each fiber...",
                    min = 0,
                    max =  total,
                    width = 300)

for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(ncol(Segments) - 4)) {
  ##find individual fiber
  assign(colnames(Segments)[i], 
         Sort_by_fiber(colnames(Segments)[i]))
  ##select individual KMTs from fiber naming it POleX_YY_ZZ
  ##X  - pole 1 or 3
  ##YY - number of fiber
  ##ZZ - number of KMTs in the fiber
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"), 
           Select_Points(j, get(colnames(Segments)[i])))
    j = j + 1
  }
  
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"),
           Find_XYZ(get(paste(colnames(Segments)[i], j,sep = "_"))))
    j = j + 1
  }
  Sys.sleep(0.1)
  setTkProgressBar(pb, i, 
                   label = paste(round(i / total * 100, 0), "% Done"))
}
close(pb)

total <- as.numeric(as.numeric(which(colnames(Segments) == "Pole2_00") - 1) - which(colnames(Segments) == "Pole1_00"))
pb <- tkProgressBar(title = "Calculate dist. of (+) and (-) ends to the Pole1",
                    min = 2,
                    max =  total,
                    width = 380)
##Sort points in the individual fiber, make 1 point in df corespond to (+) end
for(i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  tryCatch({
    j = 1
    while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
      assign(paste(colnames(Segments)[i], j, sep = "_"),
             Sort_by_distance_to_pole1(get(paste(colnames(Segments)[i], j, sep = "_"))))
      j = j + 1
    }
    assign(paste(colnames(Segments)[i]),
           Analyse_LD(i, Pole1))
    
    assign(paste(colnames(Segments)[i]),
           Relativ_Pos_1(i))

  },
  error = function(e){})
  Sys.sleep(0.1)
  setTkProgressBar(pb, i, 
                   label = paste(round(i / total * 100, 0), "% Done"))
}
close(pb)

total <- as.numeric(which(colnames(Segments) == "Pole2_00") - 1) - which(colnames(Segments) == "Pole1_00")
pb <- tkProgressBar(title = "Calculate dist. of (+) and (-) ends to the Pole2",
                    min = 0,
                    max =  total,
                    width = 380)
for(i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments) - 4)){
  j = 1
  tryCatch({
    while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
      assign(paste(colnames(Segments)[i], j, sep = "_"),
             Sort_by_distance_to_pole2(get(paste(colnames(Segments)[i], j, sep = "_"))))
      j = j + 1
      
    }
    
    assign(paste(colnames(Segments)[i]),
           Analyse_LD(i, Pole2))
    assign(paste(colnames(Segments)[i]),
           Relativ_Pos_2(i))
  },
  error = function(e){})
  Sys.sleep(0.1)
  setTkProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                   label = paste(round((i - as.numeric(which(colnames(Segments) == "Pole2_00"))) / total * 100, 0), "% Done"))
}
close(pb)

Data <- Pole1_00
for (i in as.numeric(which(colnames(Segments) == "Pole1_00")+1):as.numeric(ncol(Segments) - 4)){
  tryCatch({
    Data <- rbind(Data,
                  get(paste(colnames(Segments)[i]))[1:4])
  },
  error = function(e){}
  )
}

#########
# Plots #
#########
minus <- ggplot(Data, aes(length, minus_dist_to_pole)) + geom_point() + ylim(c(0, 6)) + geom_smooth(method="lm", se=T) + theme_classic2()
plus <- ggplot(Data, aes(length, plus_dist_to_pole)) + geom_point() + ylim(c(1, 6)) + geom_smooth(method="lm", se=T) + theme_classic2()
ggarrange(minus, plus,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
ggplot(Data, aes(relative_pos, minus_dist_to_pole)) + geom_point() + ylim(c(0, 6)) + geom_smooth(method="lm", se=T) + theme_classic2()
