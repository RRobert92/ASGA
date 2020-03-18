######################################################
# The Package to calculate total and local curvature #
######################################################
## The output of this function is data.framed named:
## 'KMTs_total_Curvature' with total curvature ratio
## 'KMTs_local_Curvature' with local curvature, relative position, (+) end distance to the spindle pole axis

####################################
# Progress bar for total curvature #
####################################

total <- as.numeric(ncol(Segments %>% select(starts_with("Pole"))))
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)

############################################################
# Loop iterating through each k-fiiber for total curvature #
############################################################

KMTs_total_Curvature <- total_curvature(2)

for(i in which(colnames(Segments) == "Pole1_01") : as.numeric(ncol(Segments) - 4)){
  tryCatch({
    assign("DF",
           total_curvature(i))
    KMTs_total_Curvature <- rbind(KMTs_total_Curvature,
                                       DF)
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Calculating total curvature of KMTs...", 
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
}

#############
# Save Data #
#############

write.xlsx(KMTs_total_Curvature, paste("Output/", Data_label, "_KMTs_total_Curvature.xlsx", sep = ""), row.names = FALSE)

close(pb)
rm(DF)

####################################
# Progress bar for local curvature #
####################################

total <- as.numeric(ncol(Segments %>% select(starts_with("Pole"))))
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)

############################################################
# Loop iterating through each k-fiiber for local curvature #
############################################################

KMTs_local_Curvature <- local_curvature(2)

for(i in which(colnames(Segments) == "Pole1_01") : as.numeric(ncol(Segments) - 4)){
  tryCatch({
    assign("DF",
           local_curvature(i))
    KMTs_local_Curvature <- rbind(KMTs_local_Curvature,
                                  DF)
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Calculating local curvature of KMTs...",
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
}

#############
# Save Data #
#############

write.xlsx(KMTs_local_Curvature, paste("Output/", Data_label, "_KMTs_local_Curvature.xlsx", sep = ""), row.names = FALSE)

close(pb)
rm(DF)
