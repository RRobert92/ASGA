######################################
# Count no. of KMTs at a kinetochore #
######################################
## The output of this function is a DF with number of KMTs

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

No_of_KMTs_at_kinetochore_P1 <- No_of_KMTs(2)
DF <- data.frame()

for(i in which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  tryCatch({
    assign("DF",
           No_of_KMTs(i))
    names(DF)[1] <- "No. of KMTs"
    No_of_KMTs_at_kinetochore_P1 <- rbind(No_of_KMTs_at_kinetochore_P1,
                                          DF)
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Counting no. of KMTs at each kinetochore from the Pole1...", 
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
}

No_of_KMTs_at_kinetochore_P1 <- data.frame(No_of_KMTs_at_kinetochore_P1[,1])
names(No_of_KMTs_at_kinetochore_P1)[1] <- "No. of KMTs"
write.xlsx(No_of_KMTs_at_kinetochore_P1, paste("Output/", Data_label, "_KMTs_no_P1.xlsx", sep = ""), row.names = FALSE)

rm(DF)
close(pb)

###########################
# Progress bar for Pole_2 #
###########################

total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
pb <- winProgressBar(min = 0,
                     max =  total,
                     width = 400)

###################################################
# Loop iterating through each KMT for the Pole_2 #
###################################################

No_of_KMTs_at_kinetochore_P2 <- No_of_KMTs(2)
DF <- data.frame()

for(i in which(colnames(Segments) == "Pole2_00") : as.numeric(ncol(Segments) - 4)){
  tryCatch({
    assign("DF",
           No_of_KMTs(i))
    names(DF)[1] <- "No. of KMTs"
    No_of_KMTs_at_kinetochore_P2 <- rbind(No_of_KMTs_at_kinetochore_P2,
                                          DF)
  },
  error = function(e){})
  
  Sys.sleep(0.1)
  setWinProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                    title = paste("Counting no. of KMTs at each kinetochore from the Pole2...",
                                  round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                                        0), 
                                  "% Done"))
}

No_of_KMTs_at_kinetochore_P2 <- data.frame(No_of_KMTs_at_kinetochore_P2[,1])
names(No_of_KMTs_at_kinetochore_P2)[1] <- "No. of KMTs"
write.xlsx(No_of_KMTs_at_kinetochore_P2, paste("Output/", Data_label, "_KMTs_no_P2.xlsx", sep = ""), row.names = FALSE)

rm(DF)
close(pb)