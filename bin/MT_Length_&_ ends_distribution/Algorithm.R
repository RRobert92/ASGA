########################################################################################
# MTs length distribution analysis based on the ends position on the pole-to-pole axis #
########################################################################################
###########################################################
# MTs ends distribution analysis on the Pole-to-Pole axis #
###########################################################

###########
# Library #
###########
library(readxl)
library(tidyverse)
library(plyr)
library(tcltk) 
library(ggpubr)
library(ggplot2)
library(xlsx)

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

Output <- "H:/Robert/Meta#1.xlsx"

Minus_Threshold <- 1

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

##Get all kinetochore and fide average point, and project this point to the spindle pole axis
Kinetochore_position <- function(){
  Plus_end <- data.frame()
  Kinetochore_Avg <- data.frame()
  for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(ncol(Segments) - 4)){
    j = 1
    tryCatch({
       while (j <= as.numeric(nrow(get(paste(colnames(Segments)[i]))))) {
      Plus_end[j,1:3] <- get(paste(colnames(Segments)[i], j, sep = "_"))[1,2:4]
      j = j + 1
    }
    Plus_end <- data.frame(X_Median = c(median(as.matrix(Plus_end[1]))),
                           Y_Median = c(median(as.matrix(Plus_end[2]))),
                           Z_Median = c(median(as.matrix(Plus_end[3]))))
    Kinetochore_Avg[i,1:3] <- Plus_end
    },
    error = function(e){
      Kinetochore_Avg[i,1:3] <- NA
    })
    }
  Kinetochore_Avg <- na.omit(Kinetochore_Avg)
  Kinetochore_Avg <- data.frame(X_Median = c(mean(as.matrix(Kinetochore_Avg[1]))),
                                Y_Median = c(mean(as.matrix(Kinetochore_Avg[2]))),
                                Z_Median = c(mean(as.matrix(Kinetochore_Avg[3]))))
  Pole_avg <- rbind(Pole1, Pole2)
  Pole_avg <- data.frame(X_Median = c(mean(as.matrix(Pole_avg[1]))),
                         Y_Median = c(mean(as.matrix(Pole_avg[2]))),
                         Z_Median = c(mean(as.matrix(Pole_avg[3]))))
  Kinetochore_projected <- data.frame(X_Median = c(mean(as.matrix(Pole_avg[1]))),
                                      Y_Median = c(mean(as.matrix(Kinetochore_Avg[2]))),
                                      Z_Median = c(mean(as.matrix(Pole_avg[3]))))
  Kinetochore_projected
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
  Plus_Distst_to_the_pole <- sqrt((Plus_end[1,1] - (Kinetochore_projected[1,1]))^2  + (Plus_end[1,3] - (Kinetochore_projected[1,3]))^2)
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
  
## Relative postion of points between kinetochore and the Pole1
Relativ_Pos_1 <- function(x){##  x <- KMTs ID "1, 2, 3..."
  Plus_end <- data.frame()
  for (i in 1:nrow(get(colnames(Segments)[x]))){
    Plus_end[i,1:3] <- get(paste(colnames(Segments)[x], i, sep = "_"))[1,2:4]
  }
  Plus_end <- data.frame(X_Median = c(median(as.matrix(Plus_end[1]))),
                         Y_Median = c(median(as.matrix(Plus_end[2]))),
                         Z_Median = c(median(as.matrix(Plus_end[3]))))
  
  relativ_position_fiber<- data.frame()
  for (i in 1:nrow(get(colnames(Segments)[x]))){
    relativ_pos_part1 <- get(paste(colnames(Segments)[x], i, sep = "_"))[nrow(get(paste(colnames(Segments)[x], i, sep = "_"))),3] - Pole1[1,2]
    relativ_pos_part2 <- Plus_end[1,2] - Pole1[1,2]
    relativ_position_fiber[i,1] <- round(relativ_pos_part1 / relativ_pos_part2, 2)
  }
  All <- cbind(get(paste(colnames(Segments)[x])),
               relativ_position_fiber)
  names(All)[4] <- "relative_pos"
  All
}

## Relative postion of points between kinetochore and the Pole2
Relativ_Pos_2 <- function(x){
  Plus_end <- data.frame()
  for (i in 1:nrow(get(colnames(Segments)[x]))){
    Plus_end[i,1:3] <- get(paste(colnames(Segments)[x], i, sep = "_"))[1,2:4]
  }
  Plus_end <- data.frame(X_Median = c(median(as.matrix(Plus_end[1]))),
                         Y_Median = c(median(as.matrix(Plus_end[2]))),
                         Z_Median = c(median(as.matrix(Plus_end[3]))))
  
  relativ_position_fiber<- data.frame()
  for (i in 1:nrow(get(colnames(Segments)[x]))){
    relativ_pos_part1 <- get(paste(colnames(Segments)[x], i, sep = "_"))[nrow(get(paste(colnames(Segments)[x], i, sep = "_"))),3] - Pole2[1,2]
    relativ_pos_part2 <-  Plus_end[1,2] - Pole2[1,2]
    relativ_position_fiber[i,1] <- round(relativ_pos_part1 / relativ_pos_part2, 2)
  }
  All <- cbind(get(paste(colnames(Segments)[x])),
               relativ_position_fiber)
  names(All)[4] <- "relative_pos"
  All
}

## Count how many KMTs with a minus end distance of "Minus_Threshold" is in the fiber
KMTs_to_the_Pole <- function(x){##  x <- KMTs ID "1, 2, 3..."
  No_of_KMTs <- data.frame()
  DF <- get(colnames(Segments)[x])[with(get(colnames(Segments)[x]), minus_dist_to_pole <= Minus_Threshold & minus_dist_to_pole > 0),]
  if (nrow(DF) == 0){
    No_of_KMTs <- 0
  } else {
    No_of_KMTs <- nrow(DF)
  }
  No_of_KMTs
}

## Count how many KMTs with a minus end distance of "Minus_Threshold" is in the fiber
KMTs_to_the_Pole_vs_length <- function(x){##  x <- KMTs ID "1, 2, 3..."
  No_of_KMTs <- data.frame()
  DF <- get(colnames(Segments)[x])[with(get(colnames(Segments)[x]), minus_dist_to_pole <= Minus_Threshold & minus_dist_to_pole > 0),]
  if (nrow(DF) == 0){
    No_of_KMTs <- 0
  } else {
    No_of_KMTs <- data.frame(c(nrow(DF)),
                             c(DF[1]))
  }
  No_of_KMTs
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

assign("Kinetochore_projected", 
       Kinetochore_position())

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
pb <- tkProgressBar(title = "Calculate dist. of (+) and hi(-) ends to the Pole2",
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

KMTs_at_the_Pole <- KMTs_to_the_Pole(2)
DF1 <- data.frame()
for (i in which(colnames(Segments) == "Pole1_01"):as.numeric(ncol(Segments) - 4)) {
  tryCatch({
      assign("DF1",
         KMTs_to_the_Pole(i))
    KMTs_at_the_Pole <- rbind(KMTs_at_the_Pole, DF1)
  },
  error = function(e){}
  )
}

KMTs_to_the_Pole_and_length <- KMTs_to_the_Pole_vs_length(2)
DF1 <- data.frame()
for (i in which(colnames(Segments) == "Pole1_01"):as.numeric(ncol(Segments) - 4)) {
  tryCatch({
    assign("DF1",
           KMTs_to_the_Pole_vs_length(i))
    if(DF1 == 0){
      KMTs_to_the_Pole_and_length <- rbind(KMTs_to_the_Pole_and_length, 0)
    }else{
      KMTs_to_the_Pole_and_length <- rbind(KMTs_to_the_Pole_and_length, DF1)
    }
  },
  error = function(e){}
  )
}

Data <- Pole1_00
for (i in as.numeric(which(colnames(Segments) == "Pole1_00")+1):as.numeric(ncol(Segments) - 4)){
  tryCatch({
    Data <- rbind(Data,
                  get(paste(colnames(Segments)[i]))[1:4])
  },
  error = function(e){}
  )
}

########################
# Export data to .xlsx #
########################

write.xlsx(Data, Output)

#########
# Plots #
#########

  ##################################################################
  # MT length dependency from MT ends distance to the spindle pole #
  ##################################################################
    minus <- ggplot(Data, aes(minus_dist_to_pole, length)) + geom_point() + ylim(c(0, 7)) + geom_smooth(method = "loess") + theme_classic2() + labs(y = "KMT length", x = "(-) end distance to the pole")
    plus <- ggplot(Data, aes(plus_dist_to_pole, length)) + geom_point() + ylim(c(0, 7)) + geom_smooth(method = "loess") + theme_classic2() + labs(y = "KMT length", x = "(+) end distance to the spindle axis")
    ggarrange(minus, plus,
              labels = c("A", "B"),
              ncol = 2, nrow = 1)

  #######################################################################################
  # MT length distribution based on the position of the (+) end along Pole-to-Pole axis #
  #######################################################################################
  LD1DF <- Data[with(Data, plus_dist_to_pole <= 4 & plus_dist_to_pole > 3),1]
  LD1DF <- data.frame(c(LD1DF),
                      c("4 - 3 um"))
  names(LD1DF)[1] <- "Length"
  names(LD1DF)[2] <- "Plus_end_distance_from_the_Spindle_Pole_Axis"
  LD2DF <- Data[with(Data, plus_dist_to_pole <= 3 & plus_dist_to_pole > 2),1]
  LD2DF <- data.frame(c(LD2DF),
                      c("3 - 2 um"))
  names(LD2DF)[1] <- "Length"
  names(LD2DF)[2] <- "Plus_end_distance_from_the_Spindle_Pole_Axis"
  LD3DF <- Data[with(Data, plus_dist_to_pole <= 2 & plus_dist_to_pole > 0),1]
  LD3DF <- data.frame(c(LD3DF),
                      c("2 - 0 um"))
  names(LD3DF)[1] <- "Length"
  names(LD3DF)[2] <- "Plus_end_distance_from_the_Spindle_Pole_Axis"
  LD <- rbind(LD1DF, LD2DF, LD3DF)

  ggplot(LD, aes(Length, colour = Plus_end_distance_from_the_Spindle_Pole_Axis)) + geom_freqpoly(binwidth = 0.5, size = 1.5) + theme_classic2()  + labs(x = "KMT length", y = "KMT numbers") + ylim(c(0, 60))

  #################################################################################################################
  # MT (-) ends distribution alone Pole-to-Pole axis based on the position of the (+) end along Pole-to-Pole axis #
  #################################################################################################################
  BM1 <- ggplot(Data[with(Data, plus_dist_to_pole <= 4 & plus_dist_to_pole > 3),], 
                aes(relative_pos, minus_dist_to_pole)) + geom_point(colour = 'red', alpha = 1/5) + ylim(c(0, 5)) + xlim(c(-0.3, 1)) + geom_smooth(colour = 'black') + theme_classic2() + labs(x = "Relative position", y = "(-) end distance to the pole")
  BM2 <- ggplot(Data[with(Data, plus_dist_to_pole <= 3 & plus_dist_to_pole > 2),], 
                aes(relative_pos, minus_dist_to_pole)) + geom_point(colour = 'green', alpha = 1/5) + ylim(c(0, 5)) + xlim(c(-0.3, 1)) + geom_smooth(colour = 'black') + theme_classic2() + labs(x = "Relative position", y = "(-) end distance to the pole")
  BM3 <- ggplot(Data[with(Data, plus_dist_to_pole <= 2 & plus_dist_to_pole > 0),], 
                aes(relative_pos, minus_dist_to_pole)) + geom_point(colour = 'blue', alpha = 1/5) + ylim(c(0, 5)) + xlim(c(-0.3, 1)) + geom_smooth(colour = 'black') + theme_classic2() + labs(x = "Relative position", y = "(-) end distance to the pole")
  ggarrange(BM1, BM2, BM3,
            labels = c("A", "B", "C"),
            ncol = 3, nrow = 1)

  #################################################################################################################
  # MT (-) ends distribution alone Pole-to-Pole axis based on the position of the (+) end along Pole-to-Pole axis #
  #################################################################################################################
  M1 <- Data[with(Data, minus_dist_to_pole <= 1.1 & minus_dist_to_pole > 0),]
  ggplot(M1, aes(minus_dist_to_pole)) + labs(y = "KMT numbers", x = "KMTs (-) end distance to the pole (um)") + geom_freqpoly(binwidth = 0.025, size = 1.5) + theme_classic2()

  ########################################################################
  # No. of KMTs per fiber with (-) end within 1um distance from the pole #
  ########################################################################
  M2 <- data.frame(KMTs_at_the_Pole,
                  "1")
  names(M2)[1] <- "KMTs_no."
  names(M2)[2] <- "KMTs_at_a_Pole_per_fiber"
  ggplot(M2, aes(KMTs_at_a_Pole_per_fiber, KMTs_no.)) + geom_boxplot(fill = "red", alpha = 0.5) + labs(x = "KMTs at the Pole per fiber", y = "KMTs no.") + geom_jitter(height = 0, width = 0.05, alpha = 0.1) + theme_classic2()

  ########################################################################
  # No. of KMTs per fiber with (-) end within 1um distance from the pole #
  ########################################################################
  ggplot(KMTs_to_the_Pole_and_length, aes(c.nrow.DF.., length)) + geom_boxplot(aes(group=c.nrow.DF..),width = 0.2) + geom_jitter(aes(group=c.nrow.DF..), width = 0.2) + xlim(c(0.9, 8)) + theme_classic2()  + labs(x = "No. of KMTs on the Pole", y = "KMTs length (um)")  + stat_summary(fun.y='median', geom='point', size=2, col='red') + geom_smooth(method = "lm")
  ggplot(KMTs_to_the_Pole_and_length, aes(length, colour = c.nrow.DF..)) + geom_freqpoly(binwidth = 0.5, size = 1) + theme_classic2()  + labs(x = "KMT number", y = "k-fiber number") + xlim(c(0,7))
  
  #########################################################################################
  # KMTs length distribution based on the position of the (+) end along Pole-to-Pole axis #
  #########################################################################################
  LM1 <- ggplot(Data[with(Data, plus_dist_to_pole <= 4 & plus_dist_to_pole > 3),], 
                aes(minus_dist_to_pole, length)) + geom_point(colour = 'red', alpha = 1/5) + geom_smooth(colour = 'black') + theme_classic2() + labs(y = "KMT length", x = "(-) end distance to the pole") + ylim(c(0, 7)) + xlim(c(0, 5))
  LM2 <- ggplot(Data[with(Data, plus_dist_to_pole <= 3 & plus_dist_to_pole > 2),], 
                aes(minus_dist_to_pole, length)) + geom_point(colour = 'green', alpha = 1/5) + geom_smooth(colour = 'black') + theme_classic2() + labs(y = "KMT length", x = "(-) end distance to the pole") + ylim(c(0, 7)) + xlim(c(0, 5))
  LM3 <- ggplot(Data[with(Data, plus_dist_to_pole <= 2 & plus_dist_to_pole > 0),], 
                aes(minus_dist_to_pole, length)) + geom_point(colour = 'blue', alpha = 1/5) + geom_smooth(colour = 'black') + theme_classic2() + labs(y = "KMT length", x = "(-) end distance to the pole") + ylim(c(0, 7)) + xlim(c(0, 5))
  ggarrange(LM1, LM2, LM3,
            labels = c("A", "B", "C"),
            ncol = 3, nrow = 1) 
  
  #########################################################################################
  # KMTs length distribution based on the position of the (+) end along Pole-to-Pole axis #
  #########################################################################################
  LD1ME <- Data[with(Data, plus_dist_to_pole <= 4 & plus_dist_to_pole > 3),2]
  LD1ME <- data.frame(c(LD1ME),
                      c("4 - 3 um"))
  names(LD1ME)[1] <- "Length"
  names(LD1ME)[2] <- "Plus_end_distance_from_the_Spindle_Pole_Axis"
  LD2ME <- Data[with(Data, plus_dist_to_pole <= 3 & plus_dist_to_pole > 2),2]
  LD2ME <- data.frame(c(LD2ME),
                      c("3 - 2 um"))
  names(LD2ME)[1] <- "Length"
  names(LD2ME)[2] <- "Plus_end_distance_from_the_Spindle_Pole_Axis"
  LD3ME <- Data[with(Data, plus_dist_to_pole <= 2 & plus_dist_to_pole > 0),2]
  LD3ME <- data.frame(c(LD3ME),
                      c("2 - 0 um"))
  names(LD3ME)[1] <- "Length"
  names(LD3ME)[2] <- "Plus_end_distance_from_the_Spindle_Pole_Axis"
  LDME <- rbind(LD1ME, LD2ME, LD3ME)
  
  ggplot(LDME, aes(Length, colour = Plus_end_distance_from_the_Spindle_Pole_Axis)) + geom_freqpoly(binwidth = 0.25, size = 1) + theme_classic2()  + labs(x = "(-) end distance to the pole", y = "KMTs number") + xlim(c(0,5))
