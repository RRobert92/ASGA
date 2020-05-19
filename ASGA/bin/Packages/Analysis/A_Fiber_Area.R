################################################################################
# Packages Fiber_Area
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-17 
################################################################################


# Set-up analysis --------------------------------------------------------------
A_Fiber_Area <- function (input, output, session){
  
# Analyze fiber area and fiber density for Pole1 -------------------------------
  total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
  
  progressSweetAlert(
    session = session, id = "P_fiber_area1",
    title = "Calculating fiber area for Pole1...",
    display_pct = TRUE, value = 0
  )
  
  for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
    tryCatch({
      assign(paste(colnames(Segments)[i]), 
             leading_KMTsv2(i, Pole1),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             Leadig_Points(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             find_polygon(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             duplicated_points(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             median_point(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             find_polygon_for_all(i))
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             duplicated_points(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             median_point(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             polygon_area(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "NDensity", sep = "_"),
             Neighorhood_densit(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             relativ_pos_1_fiber(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "NDensity", sep = "_"),
             cbind(get(paste(colnames(Segments)[i], "NDensity", sep = "_")),
                   get(paste(colnames(Segments)[i], "fiber", sep = "_"))[1]),
             envir = .GlobalEn)
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "P_fiber_area1",
      value = round((i - 1) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  
  Fiber_area_P1 <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")], 
                             "fiber", 
                             sep = "_"))
  
  
  N_density_P1 <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")], 
                            "NDensity",
                            sep = "_"))
  
  for (i in as.numeric(which(colnames(Segments) == "Pole1_00")+1) : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
    tryCatch({
      Fiber_area_P1 <- rbind(Fiber_area_P1,
                             get(paste(colnames(Segments)[i], 
                                       "fiber", 
                                       sep = "_")))
    },
    error = function(e){})
    
    N_density_P1 <- rbind(N_density_P1,
                          get(paste(colnames(Segments)[i], 
                                    "NDensity", 
                                    sep = "_")))
  }
  Fiber_area_P1 <<- Fiber_area_P1
  N_density_P1 <<- N_density_P1
  
# Analyze fiber area and fiber density for Pole2 -------------------------------
  total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
    as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
  
  progressSweetAlert(
    session = session, id = "P_fiber_area2",
    title = "Calculating fiber area for Pole2...",
    display_pct = TRUE, value = 0
  )
  
  for (i in as.numeric(which(colnames(Segments) == "Pole2_00")) : as.numeric(ncol(Segments) - 4)) {
    tryCatch({
      assign(paste(colnames(Segments)[i]), 
             leading_KMTsv2(i, Pole1),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             Leadig_Points(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             find_polygon(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             duplicated_points(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             median_point(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             find_polygon_for_all(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             duplicated_points(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             median_point(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             polygon_area(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "NDensity", sep = "_"),
             Neighorhood_densit(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             relativ_pos_2_fiber(i),
             envir = .GlobalEn)
      assign(paste(colnames(Segments)[i], "NDensity", sep = "_"),
             cbind(get(paste(colnames(Segments)[i], "NDensity", sep = "_")),
                   get(paste(colnames(Segments)[i], "fiber", sep = "_"))[1]),
             envir = .GlobalEn)
    },
    error = function(e){})
    updateProgressBar(
      session = session,
      id = "P_fiber_area2",
      value = round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  
  Fiber_area_P2 <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")], 
                            "fiber", 
                            sep = "_"))
  
  N_density_P2 <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")], 
                            "NDensity", 
                            sep = "_"))
  
  for (i in as.numeric(which(colnames(Segments) == "Pole2_00")+1) : as.numeric(ncol(Segments) - 4))  {
    tryCatch({
      Fiber_area_P2 <- rbind(Fiber_area_P2,
                             get(paste(colnames(Segments)[i], 
                                       "fiber", 
                                       sep = "_")))
    },
    error = function(e){})
    tryCatch({
      N_density_P2 <- rbind(N_density_P2,
                            get(paste(colnames(Segments)[i], 
                                      "NDensity", 
                                      sep = "_")))
    },
    error = function(e){})
  }
  Fiber_area_P2 <<- Fiber_area_P2
  N_density_P2 <<- N_density_P2
}