################################################################################
# Packages Fiber_AreaKinetochore area
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-17 
# Reviewed: Robert Kiewisz 19/07/2020
################################################################################


# Set-up analysis --------------------------------------------------------------
A_K_Core_Area <- function (input, output, session){
  
  # Analyze kinetochore area Pole1 ----------------------------------------------
  total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
  K_Core_Area <<- data.frame()
  
  progressSweetAlert(
    session = session, id = "P_k_core_area1",
    title = "Calculating kinetochore area for the Pole1...",
    display_pct = TRUE, value = 0
  )
  
  for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
    tryCatch({
      if("Leading" %in% colnames(get(paste(colnames(Segments)[i])))){
        
      }else{
        assign(paste(colnames(Segments)[i]), 
               leading_KMTsv2(i, Pole1),
               envir=.GlobalEnv)
      }
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             Leadig_Pointsv2(i),
             envir=.GlobalEnv)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             find_polygon(i),
             envir=.GlobalEnv)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             duplicated_points(i),
             envir=.GlobalEnv)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             median_point(i),
             envir=.GlobalEnv)
      assign("KCA",
             Kinetochore_Size(i),
             envir = .GlobalEnv)
      assign("K_Core_Area",
             rbind(K_Core_Area,
                   KCA),
             envir = .GlobalEnv)
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "P_k_core_area1",
      value = round((i - 1) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)
  
  K_Core_Area_P1 <<- K_Core_Area

  # Analyze kinetochore area Pole2 ----------------------------------------------  
  total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
    as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
  K_Core_Area <<- data.frame()
  
  progressSweetAlert(
    session = session, id = "P_k_core_area2",
    title = "Calculating kinetochore area for the Pole2...",
    display_pct = TRUE, value = 0
  )
  
  for (i in as.numeric(which(colnames(Segments) == "Pole2_00")) : as.numeric(ncol(Segments) - 4)) {
    tryCatch({
      if("Leading" %in% colnames(get(paste(colnames(Segments)[i])))){
        
      }else{
        assign(paste(colnames(Segments)[i]), 
               leading_KMTsv2(i, Pole2),
               envir=.GlobalEnv)
      }
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             Leadig_Pointsv2(i),
             envir=.GlobalEnv)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             find_polygon(i),
             envir=.GlobalEnv)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             duplicated_points(i),
             envir=.GlobalEnv)
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
             median_point(i),
             envir=.GlobalEnv)
      assign("KCA",
             Kinetochore_Size(i),
             envir = .GlobalEnv)
      assign("K_Core_Area",
             rbind(K_Core_Area,
                   KCA),
             envir = .GlobalEnv)
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "P_k_core_area2",
      value = round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  
  closeSweetAlert(session = session)
  
  K_Core_Area_P2 <<- K_Core_Area
}
