##################################################
# The Package to sort point in the individual KM #
##################################################
## The output of this function are sorted points in each KMT
## After points are sorted. In each PoleX_YY_ZZ the fist Point ID correspond to the (+) end and last point to the (-) end. 

###########################
# Progress bar for Pole_1 #
###########################
total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 420)

## Pole_1
Kinetochore_Pole1 <- data.frame()


for(i in which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  tryCatch({
    Kinetochore_Pole1[i,1:3] <- Kinetochore_XYZ(i)
    
  },
  error = function(e){}
  )
  
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Getting all Kinetochore positions for Pole_1...",
                                  round((i - 1) / total * 100, 
                                        0), 
                                  "% Done"))
}
close(pb)
Kinetochore_Pole1 <- na.omit(Kinetochore_Pole1)

###########################
# Progress bar for Pole_2 #
###########################

total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
pb <- winProgressBar(min = 0,
                     max =  total,
                     width = 420)

## Pole_2
Kinetochore_Pole2 <- data.frame()


for(i in as.numeric(which(colnames(Segments) == "Pole2_00")) : as.numeric(ncol(Segments) - 4)){
  tryCatch({
    Kinetochore_Pole2[i,1:3] <- Kinetochore_XYZ(i)
    
  },
  error = function(e){}
  )
  
  Sys.sleep(0.1)
  setWinProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                    title = paste("Getting all Kinetochore positions for Pole_2...",
                                  round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100, 
                                        0), 
                                  "% Done"))
}
close(pb)
Kinetochore_Pole2 <- na.omit(Kinetochore_Pole2)

## Find sister kinetochore and calculate distance between them
Inter_Kinetochore_Distance <- Inter_Kinetochore_Dist()
rm(Kinetochore_Pole1, Kinetochore_Pole2)