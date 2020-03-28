######################################################
# The Package to calculate total and local curvature #
######################################################
## The output of this function is data.framed named:
## 'KMTs_total_Curvature' with total curvature ratio
## 'KMTs_local_Curvature' with local curvature, relative position, (+) end distance to the spindle pole axis

#######################################
# Progress bar for total curvature_P1 #
#######################################

total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)

###############################################################
# Loop iterating through each k-fiiber for total curvature_P1 #
###############################################################

KMTs_total_Curvature_P1 <- total_curvature(which(colnames(Segments) == "Pole1_00"))

for(i in as.numeric(which(colnames(Segments) == "Pole1_00")+1) : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  tryCatch({
    assign("DF",
           total_curvature(i))
    KMTs_total_Curvature_P1 <- rbind(KMTs_total_Curvature_P1,
                                       DF)
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Calculating total curvature of KMTs for Pole1...", 
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
}

#############
# Save Data #
#############

write.xlsx(KMTs_total_Curvature_P1, paste("Output/", Data_label, "_KMTs_total_Curvature_P1.xlsx", sep = ""), row.names = FALSE)

close(pb)
rm(DF)


#######################################
# Progress bar for total curvature_P2 #
#######################################

total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
pb <- winProgressBar(min = 0,
                     max =  total,
                     width = 400)

###############################################################
# Loop iterating through each k-fiiber for total curvature_P2 #
###############################################################

if(nrow(Pole2_00) == 0){
  KMTs_total_Curvature_P2 <- data.frame()
} else {
  KMTs_total_Curvature_P2 <- total_curvature(which(colnames(Segments) == "Pole2_00"))
}


for(i in as.numeric(which(colnames(Segments) == "Pole2_00")+1) : as.numeric(ncol(Segments) - 4)){
  tryCatch({
    assign("DF",
           total_curvature(i))
    KMTs_total_Curvature_P2 <- rbind(KMTs_total_Curvature_P2,
                                     DF)
  },
  error = function(e){})
  
  Sys.sleep(0.1)
  setWinProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                    title = paste("Calculating total curvature of KMTs for Pole2...", 
                                  round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                                        0),
                                  "% Done"))
}

#############
# Save Data #
#############
 
write.xlsx(KMTs_total_Curvature_P2, paste("Output/", Data_label, "_KMTs_total_Curvature_P2.xlsx", sep = ""), row.names = FALSE)


close(pb)

#######################################
# Progress bar for local curvature P1 #
#######################################

total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)

###############################################################
# Loop iterating through each k-fiiber for local curvature P1 #
###############################################################

tryCatch({
  KMTs_local_Curvature_P1 <- local_curvature(which(colnames(Segments) == "Pole1_00"))
}, error = function(e){})

for(i in as.numeric(which(colnames(Segments) == "Pole1_00")+1) : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  tryCatch({
    assign("DF",
           local_curvature(i))
    KMTs_local_Curvature_P1 <- rbind(KMTs_local_Curvature_P1,
                                     DF)
  },
  error = function(e){})
  
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Calculating local curvature of KMTs for the Pole1...",
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
}

#############
# Save Data #
#############

write.xlsx(KMTs_local_Curvature_P1, paste("Output/", Data_label, "_KMTs_local_Curvature_P1.xlsx", sep = ""), row.names = FALSE)

close(pb)
rm(DF)

#######################################
# Progress bar for local curvature_P2 #
#######################################

total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
pb <- winProgressBar(min = 0,
                     max =  total,
                     width = 400)

###############################################################
# Loop iterating through each k-fiiber for local curvature_P2 #
###############################################################

if(nrow(Pole2_00) == 0){
  KMTs_local_Curvature_P2 <- data.frame()
}else{
  KMTs_local_Curvature_P2 <- local_curvature(which(colnames(Segments) == "Pole2_00"))
}

for(i in as.numeric(which(colnames(Segments) == "Pole2_00")+1) : as.numeric(ncol(Segments) - 4)){
  tryCatch({
    assign("DF",
           local_curvature(i))
    KMTs_local_Curvature_P2 <- rbind(KMTs_local_Curvature_P2,
                                     DF)
  },
  error = function(e){})
  
  Sys.sleep(0.1)
  setWinProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                    title = paste("Calculating local curvature of KMTs for Pole2...", 
                                  round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                                        0),
                                  "% Done"))
}

#############
# Save Data #
#############

write.xlsx(KMTs_local_Curvature_P2, paste("Output/", Data_label, "_KMTs_local_Curvature_P2.xlsx", sep = ""), row.names = FALSE)
rm(DF)


close(pb)