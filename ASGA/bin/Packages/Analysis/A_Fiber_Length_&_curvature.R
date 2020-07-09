################################################################################
# Packages Fiber_Fiber_Length_&_curvature
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-07-07
################################################################################


# Set-up analysis --------------------------------------------------------------
A_Fiber_Length_Curv <- function (input, output, session){
  
  Fiber_length <<- data.frame()
  Fiber_Total_Curv <<- data.frame()
  Fiber_Local_Curv <<- data.frame()
  
  # Analyze fiber length and curvature for Pole1 -------------------------------
  total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
  
  progressSweetAlert(
    session = session, id = "P_Curv&length1",
    title = "Calculating fiber length/Curvature for Pole1...",
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
      assign("Length_f",
             Fiber_Length(i),
             envir = .GlobalEnv)
      
      assign("Fiber_length",
             rbind(Fiber_length,
                   Length_f),
             envir = .GlobalEnv)
      
      assign("TC",
             Fiber_Total_Curvature(i),
             envir = .GlobalEnv)
      
      assign("Fiber_Total_Curv",
             rbind(Fiber_Total_Curv,
                   TC),
             envir = .GlobalEnv)
      
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
             relativ_pos_1_curv(i),
             envir = .GlobalEnv)
      assign("LC",
             Fiber_Local_Curvature(i),
             envir = .GlobalEnv)
      
      assign("Fiber_Local_Curv",
             rbind(Fiber_Local_Curv,
                   LC),
             envir = .GlobalEnv)
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "P_Curv&length1",
      value = round((i - 1) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)
  
  Fiber_Length_P1 <<- Fiber_length
  Fiber_Total_Curv_P1 <<- Fiber_Total_Curv
  Fiber_Local_Curv_P1 <<- Fiber_Local_Curv
  
  # Analyze fiber length and curvature for Pole2 ------------------------------- 
  Fiber_length <<- data.frame()
  Fiber_Total_Curv <<- data.frame()
  Fiber_Local_Curv <<- data.frame()
  
  total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
    as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
  
  progressSweetAlert(
    session = session, id = "P_Curv&length2",
    title = "Calculating fiber length/Curvature for Pole2...",
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
      assign("Length_f",
             Fiber_Length(i),
             envir = .GlobalEnv)
      
      assign("Fiber_length",
             rbind(Fiber_length,
                   Length_f),
             envir = .GlobalEnv)
      
      assign("TC",
             Fiber_Total_Curvature(i),
             envir = .GlobalEnv)
      
      assign("Fiber_Total_Curv",
             rbind(Fiber_Total_Curv,
                   TC),
             envir = .GlobalEnv)
      
      assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
             relativ_pos_2_curv(i),
             envir = .GlobalEnv)
      assign("LC",
             Fiber_Local_Curvature(i),
             envir = .GlobalEnv)
      
      assign("Fiber_Local_Curv",
             rbind(Fiber_Local_Curv,
                   LC),
             envir = .GlobalEnv)
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "P_Curv&length2",
      value = round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  
  closeSweetAlert(session = session)
  
  Fiber_Length_P2 <<- Fiber_length
  Fiber_Total_Curv_P2 <<- Fiber_Total_Curv
  Fiber_Local_Curv_P2 <<- Fiber_Local_Curv
}
