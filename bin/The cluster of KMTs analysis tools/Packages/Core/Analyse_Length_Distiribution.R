#####################################
# Sort points in the individual KMT #
#####################################
## The output of this function are sorted points in each KMT
## After points are sorted. In each PoleX_YY_ZZ the fist Point ID correspond to the (+) end and last point to the (-) end. 

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

total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
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

#################
# Generate data #
#################

LD <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["length"]
Plus_end <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["plus_dist_to_kinetochore_core"]
LD <- cbind(LD, Plus_end)

for (i in as.numeric(which(colnames(Segments) == "Pole1_00")+1) : as.numeric(ncol(Segments) - 4)) {
  tryCatch({
    DF_LD <- get(paste(colnames(Segments)[i]))["length"]
  DF_Plus_end <- get(paste(colnames(Segments)[i]))["plus_dist_to_kinetochore_core"]
  DF <- cbind(DF_LD,
              DF_Plus_end)
  LD <- rbind(LD, 
              DF)
  },
  error = function(e){})
  
}

#############
# Save data #
#############

names(LD)[1] <- "KMTs length"
write.xlsx(LD, paste("Output/", Data_label, "_LD.xlsx", sep = ""), row.names = FALSE)

rm(DF,
   DF_LD,
   DF_Plus_end,
   Plus_end)
