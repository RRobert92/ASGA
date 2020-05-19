################################################################################
# Packages Curvature
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-17 
################################################################################


# Set-up analysis --------------------------------------------------------------
A_Curvature <- function (input, output, session){
 
# Analyze total & local curvature for Pole1 ------------------------------------
  total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
  KMTs_total_Curvature_P1 <- total_curvature(which(colnames(Segments) == "Pole1_00"))
  
  tryCatch({
    KMTs_local_Curvature_P1 <- local_curvature(which(colnames(Segments) == "Pole1_00"))
  }, error = function(e){})
  
  progressSweetAlert(
    session = session, id = "P_TL_Curvature1",
    title = "Calculating total & local curvature of KMTs for Pole1...",
    display_pct = TRUE, value = 0
  )
  
  for(i in as.numeric(which(colnames(Segments) == "Pole1_00")+1) : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){

    tryCatch({
      assign("DF",
             total_curvature(i),
             envir = .GlobalEnv)
      KMTs_total_Curvature_P1 <- rbind(KMTs_total_Curvature_P1,
                                       DF)
    },
    error = function(e){}
    )
    
    tryCatch({
      assign("DF",
             local_curvature(i),
             envir = .GlobalEnv)
      KMTs_local_Curvature_P1 <- rbind(KMTs_local_Curvature_P1,
                                       DF)
    },
    error = function(e){}
    )
    
    updateProgressBar(
      session = session,
      id = "P_TL_Curvature1",
      value = round((i - 1) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)
  
  # Analyze total & local curvature for Pole2 ------------------------------------ 
  total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
    as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
  
  if(nrow(Pole2_00) == 0){
    KMTs_total_Curvature_P2 <- data.frame()
    KMTs_local_Curvature_P2 <- data.frame()
    
  } else {
    KMTs_total_Curvature_P2 <- total_curvature(which(colnames(Segments) == "Pole2_00"))
    KMTs_local_Curvature_P2 <- local_curvature(which(colnames(Segments) == "Pole2_00"))
    
  }
  
  progressSweetAlert(
    session = session, id = "P_TL_Curvature2",
    title = "Calculating total & local curvature of KMTs for Pole1...",
    display_pct = TRUE, value = 0
  )
  
  for(i in as.numeric(which(colnames(Segments) == "Pole2_00")+1) : as.numeric(ncol(Segments) - 4)){
    DF <- data.frame()
    tryCatch({
      assign("DF",
             total_curvature(i),
             envir = .GlobalEnv)
      KMTs_total_Curvature_P2 <- rbind(KMTs_total_Curvature_P2,
                                       DF)
    },
    error = function(e){}
    )
    
    DF <- data.frame()
    tryCatch({
      assign("DF",
             local_curvature(i),
             envir = .GlobalEnv)
      KMTs_local_Curvature_P2 <- rbind(KMTs_local_Curvature_P2,
                                       DF)
    },
    error = function(e){}
    )
    
    updateProgressBar(
      session = session,
      id = "P_TL_Curvature2",
      value = round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)
}