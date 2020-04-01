##########################################################
# Package to analyze fiber area and neighborhood density # 
##########################################################

##################################
# Progress bar for fiber_area_P1 #
##################################

total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)

## Find leading KMTs in the fiber for Pole1
for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
  tryCatch({
   assign(paste(colnames(Segments)[i]), 
         leading_KMTsv2(i, Pole1))
   assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         Leadig_Points(i))
   assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         find_polygon(i))
   assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         duplicated_points(i))
   assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         median_point(i))
   assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         find_polygon_for_all(i))
   assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         duplicated_points(i))
   assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         median_point(i))
   assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         polygon_area(i))
   assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
          relativ_pos_1_fiber(i))
  },
  error = function(e){},
  warning = function(w) {})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Calculating fiber area for Pole1...", 
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
}
close(pb)
##################################
# Progress bar for fiber_area_P2 #
##################################

total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
pb <- winProgressBar(min = 0,
                     max =  total,
                     width = 400)

## Find leading KMTs in the fiber for Pole2
for (i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments) - 4)) {
  tryCatch({
  assign(paste(colnames(Segments)[i]), 
         leading_KMTsv2(i, Pole1))
  assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         Leadig_Points(i))
  assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         find_polygon(i))
  assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         duplicated_points(i))
  assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         median_point(i))
  assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         find_polygon_for_all(i))
  assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         duplicated_points(i))
  assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         median_point(i))
  assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         polygon_area(i))
  assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
         relativ_pos_2_fiber(i))
  },
  error = function(e){},
  warning = function(w) {})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                    title = paste("Calculating fiber area for Pole2...", 
                                  round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                                        0),
                                  "% Done"))
}
close(pb)
