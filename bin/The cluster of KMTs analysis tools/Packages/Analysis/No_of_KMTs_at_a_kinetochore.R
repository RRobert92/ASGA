######################################
# Count no. of KMTs at a kinetochore #
######################################
## The output of this function is a DF with number of KMTs

##########################
# Progress bar for Pole_1 #
###########################

total <- as.numeric(ncol(Segments %>% select(starts_with("Pole"))))
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)

##################################################
# Loop iterating through each KMT for the Pole_1 #
##################################################

No_of_KMTs_at_kinetochore <- No_of_KMTs(2)
DF <- data.frame()
for(i in which(colnames(Segments) == "Pole1_01") : as.numeric(ncol(Segments) - 4)){
  tryCatch({
    assign("DF",
           No_of_KMTs(i))
    names(DF)[1] <- "No. of KMTs"
    No_of_KMTs_at_kinetochore <- rbind(No_of_KMTs_at_kinetochore,
                                        DF)
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Counting no. of KMTs at each kinetochore...", 
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
}

No_of_KMTs_at_kinetochore <- data.frame(No_of_KMTs_at_kinetochore[,1])
names(No_of_KMTs_at_kinetochore)[1] <- "No. of KMTs"
rm(DF)

close(pb)