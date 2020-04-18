##################################################################################
# Package for calculate distribution of a (-) end in close proximity to the KMT #
##################################################################################

##################################
# Progress bar for fiber_area_P1 #
##################################

total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)


#####################################################
# Calculate which (-) ends are nucleated at the KMT #
#####################################################

KMTs_minus_seed_P1 <- Minus_end_seed(which(colnames(Segments) == "Pole1_00"))

for(m in as.numeric(which(colnames(Segments) == "Pole1_00")+1):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  tryCatch({
    assign("DF",
           Minus_end_seed(m))
    KMTs_minus_seed_P1 <- rbind(KMTs_minus_seed_P1,
                                DF)
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, m, 
                    title = paste("Calculating (-) nucleated at the KMT for Pole1...", 
                                  round((m - 1) / total * 100,
                                        0),
                                  "% Done"))
}

#############
# Save Data #
#############

write.xlsx(KMTs_minus_seed_P1, paste("bin/Output/", Data_label, "_KMTs_minus_seed_P1.xlsx", sep = ""), row.names = FALSE)

close(pb)

##################################
# Progress bar for fiber_area_P2 #
##################################

total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
  as.numeric(which(colnames(Segments) == "Pole2_00") - 1)

pb <- winProgressBar(min = 0,
                     max =  total,
                     width = 400)

#####################################################
# Calculate which (-) ends are nucleated at the KMT #
#####################################################

if(nrow(Pole2_00) == 0){
  KMTs_minus_seed_P2 <- data.frame()
  
} else {
  KMTs_minus_seed_P2 <- Minus_end_seed(which(colnames(Segments) == "Pole2_00"))
  
}

for(i in as.numeric(which(colnames(Segments) == "Pole2_00")+1) : as.numeric(ncol(Segments) - 4)){
  tryCatch({
    assign("DF",
           Minus_end_seed(i))
    KMTs_minus_seed_P2 <- rbind(KMTs_minus_seed_P2,
                                DF)
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                    title = paste("Calculating (-) nucleated at the KMT for Pole2...",
                                  round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                                        0),
                                  "% Done"))
}

#############
# Save Data #
#############

write.xlsx(KMTs_minus_seed_P2, paste("bin/Output/", Data_label, "_KMTs_minus_seed_P2.xlsx", sep = ""), row.names = FALSE)

close(pb)