##########################################################
# Package to analyze fiber area and neighborhood density # 
##########################################################

##################################
# Progress bar for fiber_area_P1 #
##################################

total <- as.numeric(length(which(colnames(Segments_KMT) == "Pole1_00") : as.numeric(which(colnames(Segments_KMT) == "Pole2_00"))) - 1)
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)

## Find leading KMTs in the fiber for Pole1
for (i in which(colnames(Segments_KMT) == "Pole1_00"):as.numeric(which(colnames(Segments_KMT) == "Pole2_00") - 1)) {
  tryCatch({
   assign(paste(colnames(Segments_KMT)[i]), 
         leading_KMTsv2(i, Pole1))
   assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         Leadig_Points(i))
   assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         find_polygon(i))
   assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         duplicated_points(i))
   assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         median_point(i))
   assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         find_polygon_for_all(i))
   assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         duplicated_points(i))
   assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         median_point(i))
   assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         polygon_area(i))
   assign(paste(colnames(Segments_KMT)[i], "NDensity", sep = "_"),
          Neighorhood_densit(i))
   assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
          relativ_pos_1_fiber(i))
   assign(paste(colnames(Segments_KMT)[i], "NDensity", sep = "_"),
         cbind(get(paste(colnames(Segments_KMT)[i], "NDensity", sep = "_")),
               get(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"))[1]))
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Calculating fiber area for Pole1...", 
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
}
close(pb)

###############################
# Save data for fiber_area_P2 #
###############################

Fiber_area_P1 <- get(paste(colnames(Segments_KMT)[which(colnames(Segments_KMT) == "Pole1_00")], 
                           "fiber", 
                           sep = "_"))

for (i in as.numeric(which(colnames(Segments_KMT) == "Pole1_00")+1) : as.numeric(which(colnames(Segments_KMT) == "Pole2_00") - 1)) {
  tryCatch({
    Fiber_area_P1 <- rbind(Fiber_area_P1,
                           get(paste(colnames(Segments_KMT)[i], 
                                     "fiber", 
                                     sep = "_")))
  },
  error = function(e){})
}

N_density_P1 <- get(paste(colnames(Segments_KMT)[which(colnames(Segments_KMT) == "Pole1_00")], 
                          "NDensity",
                          sep = "_"))

for (i in as.numeric(which(colnames(Segments_KMT) == "Pole1_00")+1) : as.numeric(which(colnames(Segments_KMT) == "Pole2_00") - 1)) {
  tryCatch({
    N_density_P1 <- rbind(N_density_P1,
                          get(paste(colnames(Segments_KMT)[i], 
                                    "NDensity", 
                                    sep = "_")))
  },
  error = function(e){})
}

##################################
# Progress bar for fiber_area_P2 #
##################################

total <- which(colnames(Segments_KMT) == colnames(Segments_KMT %>% select(starts_with("Pole")))[ncol(Segments_KMT %>% select(starts_with("Pole")))]) - 
  as.numeric(which(colnames(Segments_KMT) == "Pole2_00") - 1)
pb <- winProgressBar(min = 0,
                     max =  total,
                     width = 400)

## Find leading KMTs in the fiber for Pole2
for (i in as.numeric(which(colnames(Segments_KMT) == "Pole2_00")) : as.numeric(ncol(Segments_KMT) - 4)) {
  tryCatch({
  assign(paste(colnames(Segments_KMT)[i]), 
         leading_KMTsv2(i, Pole1))
  assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         Leadig_Points(i))
  assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         find_polygon(i))
  assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         duplicated_points(i))
  assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         median_point(i))
  assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         find_polygon_for_all(i))
  assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         duplicated_points(i))
  assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         median_point(i))
  assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         polygon_area(i))
  assign(paste(colnames(Segments_KMT)[i], "NDensity", sep = "_"),
         Neighorhood_densit(i))
  assign(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"), 
         relativ_pos_2_fiber(i))
  assign(paste(colnames(Segments_KMT)[i], "NDensity", sep = "_"),
         cbind(get(paste(colnames(Segments_KMT)[i], "NDensity", sep = "_")),
               get(paste(colnames(Segments_KMT)[i], "fiber", sep = "_"))[1]))
  },
  error = function(e){})
  Sys.sleep(0.1)
  setWinProgressBar(pb, i - as.numeric(which(colnames(Segments_KMT) == "Pole2_00")), 
                    title = paste("Calculating fiber area for Pole2...", 
                                  round((i - as.numeric(which(colnames(Segments_KMT) == "Pole2_00") - 1)) / total * 100,
                                        0),
                                  "% Done"))
}
close(pb)

###############################
# Save data for fiber_area_P2 #
###############################

Fiber_area_P2 <- get(paste(colnames(Segments_KMT)[which(colnames(Segments_KMT) == "Pole2_00")], 
                           "fiber", 
                           sep = "_"))

for (i in as.numeric(which(colnames(Segments_KMT) == "Pole2_00")+1) : as.numeric(ncol(Segments_KMT) - 4))  {
  tryCatch({
    Fiber_area_P2 <- rbind(Fiber_area_P2,
                           get(paste(colnames(Segments_KMT)[i], 
                                     "fiber", 
                                     sep = "_")))
  },
  error = function(e){})
}

N_density_P2 <- get(paste(colnames(Segments_KMT)[which(colnames(Segments_KMT) == "Pole2_00")], 
                          "NDensity", 
                          sep = "_"))

for (i in as.numeric(which(colnames(Segments_KMT) == "Pole2_00")+1) : as.numeric(ncol(Segments_KMT) - 4))  {
  tryCatch({
    N_density_P2 <- rbind(N_density_P2,
                          get(paste(colnames(Segments_KMT)[i], 
                                    "NDensity", 
                                    sep = "_")))
  },
  error = function(e){})
}
