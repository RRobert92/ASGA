###################################################################
# The analysis tool count no. of KMTs with a (-) end at the pole1 #
###################################################################
## x variablein in the function is a number of a column contain KMTs information (e.g. 1, 2, 3...., etc.)
## Count how many KMTs with a minus end distance of "Minus_Threshold" is in the fiber

######################
# Setting for Pole_1 #
######################

KMTs_at_the_Pole1 <- KMTs_to_the_Pole(which(colnames(Segments_KMT) == "Pole1_00"))
names(KMTs_at_the_Pole1)[1] <- "No. of KMTs"

KMTs_to_the_Pole1_and_length <- KMTs_to_the_Pole_vs_length(which(colnames(Segments_KMT) == "Pole1_00"))
names(KMTs_to_the_Pole1_and_length)[1] <- "No. of KMTs"
names(KMTs_to_the_Pole1_and_length)[2] <- "KMTs length"
names(KMTs_to_the_Pole1_and_length)[3] <- "Minus end dist."
names(KMTs_to_the_Pole1_and_length)[4] <- "Plus end dist. to k-core"
names(KMTs_to_the_Pole1_and_length)[5] <- "Plus end dist. to pole"

###########################
# Progress bar for Pole_1 #
###########################

total <- as.numeric(length(which(colnames(Segments_KMT) == "Pole1_00") : as.numeric(which(colnames(Segments_KMT) == "Pole2_00"))) - 1)
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)

DF1 <- data.frame()
DF2 <- data.frame()

##################################################
# Loop iterating through each KMT for the Pole_1 #
##################################################

for (i in which(colnames(Segments_KMT) == "Pole1_00") : as.numeric(which(colnames(Segments_KMT) == "Pole2_00") - 1)) {
  tryCatch({
    assign("DF1",
           KMTs_to_the_Pole(i))
    names(DF1)[1] <- "No. of KMTs"
    KMTs_at_the_Pole1 <- rbind(KMTs_at_the_Pole1, 
                              DF1)
    
    assign("DF2",
           KMTs_to_the_Pole_vs_length(i))
    
    names(DF2)[1] <- "No. of KMTs"
    names(DF2)[2] <- "KMTs length"
    names(DF2)[3] <- "Minus end dist."
    names(DF2)[4] <- "Plus end dist. to k-core"
    names(DF2)[5] <- "Plus end dist. to pole"
    
    KMTs_to_the_Pole1_and_length <- rbind(KMTs_to_the_Pole1_and_length, 
                                         DF2)
    KMTs_to_the_Pole1_and_length <- na.omit(KMTs_to_the_Pole1_and_length)
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Calculating no. of KMTs reaching the pole1...", 
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
}

close(pb)

#############
# Save Data #
#############

tryCatch({
  KMTs_at_the_Pole1 <- data.frame(KMTs_at_the_Pole1[,1])
  names(KMTs_at_the_Pole1)[1] <- "No. of KMTs"
  write.xlsx(KMTs_at_the_Pole1, paste("bin/Output/", Data_label, "_KMTs_at_the_Pole1.xlsx", sep = ""), row.names = FALSE)
  write.xlsx(KMTs_to_the_Pole1_and_length, paste("bin/Output/", Data_label, "_Minus_end_position_P1.xlsx", sep = ""), row.names = FALSE)
},

error = function(e){})

rm(DF2)

######################
# Setting for Pole_2 #
######################

KMTs_at_the_Pole2 <- KMTs_to_the_Pole(which(colnames(Segments_KMT) == "Pole1_00"))
names(KMTs_at_the_Pole2)[1] <- "No. of KMTs"

KMTs_to_the_Pole2_and_length <- KMTs_to_the_Pole_vs_length(which(colnames(Segments_KMT) == "Pole1_00"))
names(KMTs_to_the_Pole2_and_length)[1] <- "No. of KMTs"
names(KMTs_to_the_Pole2_and_length)[2] <- "KMTs length"
names(KMTs_to_the_Pole2_and_length)[3] <- "Minus end dist."
names(KMTs_to_the_Pole2_and_length)[4] <- "Plus end dist. to k-core"
names(KMTs_to_the_Pole2_and_length)[5] <- "Plus end dist. to pole"

###########################
# Progress bar for Pole_2 #
###########################

total <- which(colnames(Segments_KMT) == colnames(Segments_KMT %>% select(starts_with("Pole")))[ncol(Segments_KMT %>% select(starts_with("Pole")))]) - 
  as.numeric(which(colnames(Segments_KMT) == "Pole2_00") - 1)

pb <- winProgressBar(min = 0,
                     max =  total,
                     width = 400)

DF1 <- data.frame()
DF2 <- data.frame()

##################################################
# Loop iterating through each KMT for the Pole_2 #
##################################################

for (i in which(colnames(Segments_KMT) == "Pole2_00") : as.numeric(ncol(Segments_KMT) - 4)) {
  tryCatch({
    assign("DF1",
           KMTs_to_the_Pole(i))
    names(DF1)[1] <- "No. of KMTs"
    KMTs_at_the_Pole2 <- rbind(KMTs_at_the_Pole2, 
                               DF1)
    
    assign("DF2",
           KMTs_to_the_Pole_vs_length(i))
    
    names(DF2)[1] <- "No. of KMTs"
    names(DF2)[2] <- "KMTs length"
    names(DF2)[3] <- "Minus end dist."
    names(DF2)[4] <- "Plus end dist. to k-core"
    names(DF2)[5] <- "Plus end dist. to pole"
    
    KMTs_to_the_Pole2_and_length <- rbind(KMTs_to_the_Pole2_and_length, 
                                          DF2)
    KMTs_to_the_Pole2_and_length <- na.omit(KMTs_to_the_Pole2_and_length)
  },
  error = function(e){})
  
  Sys.sleep(0.1)
  setWinProgressBar(pb, i - as.numeric(which(colnames(Segments_KMT) == "Pole2_00")), 
                    title = paste("Calculating no. of KMTs reaching the pole2...",
                                  round((i - as.numeric(which(colnames(Segments_KMT) == "Pole2_00") - 1)) / total * 100,
                                        0), 
                                  "% Done"))
}

close(pb)

#############
# Save Data #
#############

tryCatch({
KMTs_at_the_Pole2 <- data.frame(KMTs_at_the_Pole2[,1])
names(KMTs_at_the_Pole2)[1] <- "No. of KMTs"
write.xlsx(KMTs_at_the_Pole2, paste("bin/Output/", Data_label, "_KMTs_at_the_Pole2.xlsx", sep = ""), row.names = FALSE)
write.xlsx(KMTs_to_the_Pole2_and_length, paste("bin/Output/", Data_label, "_Minus_end_position_P2.xlsx", sep = ""), row.names = FALSE)
},

error = function(e){})

rm(DF2)