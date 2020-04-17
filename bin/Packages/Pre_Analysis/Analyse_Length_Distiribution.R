#####################################
# Sort points in the individual KMT #
#####################################
## The output of this function are sorted points in each KMT
## After points are sorted. In each PoleX_YY_ZZ the fist Point ID correspond to the (+) end and last point to the (-) end. 

###########
# Setting #
###########

## Get ID for the ellipse Rx and Rz for 100%, 50% and 25%
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
Pole_avg <- rbind(Pole1, Pole2)
Pole_avg <- data.frame(X_Mean = c(mean(as.matrix(Pole_avg[1]))),
                       Y_Mean = c(mean(as.matrix(Pole_avg[2]))),
                       Z_Mean = c(mean(as.matrix(Pole_avg[3]))))
Rx100 <- data.frame()
Rx100[1,1] <- max(Kinetochore_Avg$X_Median)
Rx100[1,1] <- abs(Rx100[1,1] - Pole_avg$X_Mean)
Rx100[1,2] <- min(Kinetochore_Avg$X_Median)
Rx100[1,2] <- abs(Rx100[1,2] - Pole_avg$X_Mean)

Rx100 <- max(Rx100)
Rx50 <- Rx100*0.80
Rx25 <- Rx100*0.45

Rz100 <- data.frame()
Rz100[1,1] <- max(Kinetochore_Avg$Z_Median)
Rz100[1,1] <- abs(Rz100[1,1] - Pole_avg$Z_Mean)
Rz100[1,2] <- min(Kinetochore_Avg$Z_Median)
Rz100[1,2] <- abs(Rz100[1,2] - Pole_avg$Z_Mean)

Rz100 <- max(Rz100)
Rz50 <- Rz100*0.80
Rz25 <- Rz100*0.45

rm(Kinetochore_Avg, Pole_avg)

###########################
# Progress bar for Pole_1 #
###########################

total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)

##################################################
# Loop iterating through each KMT for the Pole_1 #
##################################################

for(i in which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  tryCatch({
    assign(paste(colnames(Segments)[i]),
           Analyse_LD(i, 
                      Pole1))
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Calcualting legnth and ends positions for Pole_1", 
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
} 
close(pb)

###########################
# Progress bar for Pole_2 #
###########################

total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
  as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
pb <- winProgressBar(min = 0,
                     max =  total,
                     width = 400)

##################################################
# Loop iterating through each KMT for the Pole_2 #
##################################################

for(i in as.numeric(which(colnames(Segments) == "Pole2_00")) : as.numeric(ncol(Segments) - 4)){
  j = 1
  tryCatch({
    assign(paste(colnames(Segments)[i]),
           Analyse_LD(i, 
                      Pole2))
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                    title = paste("Calcualting legnth and ends positions for Pole_2",
                                  round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                                        0), 
                                  "% Done"))
}
close(pb)

####################
# Generate data_P1 #
####################

LD_P1 <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["length"]
Plus_end <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["plus_dist_to_kinetochore_core"]
Dist_pole <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["plus_dist_to_pole"]
Elips <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["Elipse_Position"]
Minus_dist <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["minus_dist_to_pole"]
LD_P1 <- cbind(LD_P1,
               Plus_end,
               Dist_pole,
               Elips,
               Minus_dist)

for (i in as.numeric(which(colnames(Segments) == "Pole1_00")+1) : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  tryCatch({
  DF_LD <- get(paste(colnames(Segments)[i]))["length"]
  DF_Plus_end <- get(paste(colnames(Segments)[i]))["plus_dist_to_kinetochore_core"]
  DF_Dist_pole <- get(paste(colnames(Segments)[i]))["plus_dist_to_pole"]
  DF_Elips <- get(paste(colnames(Segments)[i]))["Elipse_Position"]
  Minus_dist <- get(paste(colnames(Segments)[i]))["minus_dist_to_pole"]
  DF <- cbind(DF_LD,
              DF_Plus_end,
              DF_Dist_pole,
              DF_Elips,
              Minus_dist)
  
  LD_P1 <- rbind(LD_P1, 
                 DF)
  },
  error = function(e){})
  
}

#############
# Save data #
#############

names(LD_P1)[1] <- "KMTs length"
write.xlsx(LD_P1, paste("bin/Output/", Data_label, "_LD_P1.xlsx", sep = ""), row.names = FALSE)

####################
# Generate data_P2 #
####################

  tryCatch({
    LD_P2 <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["length"]
    Minus_dist <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["minus_dist_to_pole"]
    Plus_end <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["plus_dist_to_kinetochore_core"]
    Dist_pole <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["plus_dist_to_pole"]
    Elips <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["Elipse_Position"]
    LD_P2 <- cbind(LD_P2,
                   Plus_end,
                   Dist_pole,
                   Elips,
                   Minus_dist)
  }, error = function(e){})


for (i in as.numeric(which(colnames(Segments) == "Pole2_00")+1) : as.numeric(ncol(Segments) - 4)){
  tryCatch({
    DF_LD <- get(paste(colnames(Segments)[i]))["length"]
    Minus_dist <- get(paste(colnames(Segments)[i]))["minus_dist_to_pole"]
    DF_Plus_end <- get(paste(colnames(Segments)[i]))["plus_dist_to_kinetochore_core"]
    DF_Dist_pole <- get(paste(colnames(Segments)[i]))["plus_dist_to_pole"]
    DF_Elips <- get(paste(colnames(Segments)[i]))["Elipse_Position"]
    DF <- cbind(DF_LD,
                DF_Plus_end,
                DF_Dist_pole,
                DF_Elips,
                Minus_dist)
    
    LD_P2 <- rbind(LD_P2, 
                   DF)
  },
  error = function(e){})
}

#############
# Save data #
#############

names(LD_P2)[1] <- "KMTs length"
write.xlsx(LD_P2, paste("bin/Output/", Data_label, "_LD_P2.xlsx", sep = ""), row.names = FALSE)


rm(DF,
   DF_LD,
   DF_Plus_end)
