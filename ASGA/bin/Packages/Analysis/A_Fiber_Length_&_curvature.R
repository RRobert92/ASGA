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
  
  # Analyze fiber area and fiber density for Pole1 -------------------------------
  total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
  
  progressSweetAlert(
    session = session, id = "P_fiber_area1",
    title = "Calculating fiber length/Curvature for Pole1...",
    display_pct = TRUE, value = 0
  )
  Fiber_Length_P1 <<- data.frame(Fiber_legth = c())

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
      assign("Length",
             Fiber_Length(i))
      assign("TC",
             Fiber_Total_Curvature(i))
      assign(paste(colnames(Segments)[x], "fiber", sep = "_"),
             relativ_pos_1_curv(x, Pole1))
      assign("LC",
             Fiber_Local_Curvature(i))
      
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
  closeSweetAlert(session = session)
      