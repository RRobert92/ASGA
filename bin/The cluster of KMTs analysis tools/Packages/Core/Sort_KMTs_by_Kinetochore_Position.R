##################################################
# The Package to sort point in the individual KM #
##################################################
## The output of this function are sorted points in each KMT
## After points are sorted. In each PoleX_YY_ZZ the fist Point ID correspond to the (+) end and last point to the (-) end. 

###########################
# Progress bar for Pole_1 #
###########################

total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
pb <- tkProgressBar(title = "Sorting points based on (+) and (-) ends for the Pole_1",
                    min = 2,
                    max =  total,
                    width = 400)

## Pole_1

for(i in which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  tryCatch({
    j = 1
    while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
      assign(paste(colnames(Segments)[i], 
                   j, 
                   sep = "_"),
             Sort_by_distance_to_pole1(get(paste(colnames(Segments)[i], 
                                                 j, 
                                                 sep = "_"))))
      j = j + 1
    }
    },
  error = function(e){})
  Sys.sleep(0.1)
  setTkProgressBar(pb, i, 
                   label = paste(round((i - 1) / total * 100, 
                                       0), 
                                 "% Done"))
}
close(pb)

###########################
# Progress bar for Pole_2 #
###########################

total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
pb <- tkProgressBar(title = "Sorting points based on (+) and (-) ends for the Pole_2",
                    min = 0,
                    max =  total,
                    width = 400)
## Pole_2

for(i in as.numeric(which(colnames(Segments) == "Pole2_00")) : as.numeric(ncol(Segments) - 4)){
  j = 1
  tryCatch({
    while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
      assign(paste(colnames(Segments)[i], 
                   j, 
                   sep = "_"),
             Sort_by_distance_to_pole2(get(paste(colnames(Segments)[i], 
                                                 j, 
                                                 sep = "_"))))
      j = j + 1
      
    }
 },
  error = function(e){})
  Sys.sleep(0.1)
  setTkProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                   label = paste(round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100, 0), "% Done"))
}
close(pb)
