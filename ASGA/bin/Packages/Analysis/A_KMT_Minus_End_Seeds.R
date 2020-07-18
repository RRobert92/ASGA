################################################################################
# Packages KMT_Minus_End_Seeds
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-17
# Debugged/Reviewed: Robert Kiewisz 18/07/2020
################################################################################


# Set-up analysis --------------------------------------------------------------
A_KMT_Minus_End_Seeds <- function (input, output, session){
  
# Analyze (-) ends nucleation from the KMT for Pole1 ---------------------------
  total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
  
  progressSweetAlert(
    session = session, id = "P_nucleation1",
    title = "Calculating (-) nucleated from the KMT for the Pole1...",
    display_pct = TRUE, value = 0
  )
  
  assign("KMTs_minus_seed_P1",
         Minus_end_seed(which(colnames(Segments) == "Pole1_00")),
         envir=.GlobalEnv)
  
  for(i in as.numeric(which(colnames(Segments) == "Pole1_00")+1):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
    tryCatch({
      assign("DF",
             Minus_end_seed(i),
             envir=.GlobalEnv)
      assign("KMTs_minus_seed_P1",
             rbind(KMTs_minus_seed_P1,
                   DF),
             envir=.GlobalEnv)
    },
    error = function(e){})
   
    updateProgressBar(
      session = session,
      id = "P_nucleation1",
      value = round((i - 1) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)
  
# Analyze (-) ends nucleation from the KMT for Pole2 ---------------------------
  total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
    as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
  
  progressSweetAlert(
    session = session, id = "P_nucleation2",
    title = "Calculating (-) nucleated from the KMT for the Pole2...",
    display_pct = TRUE, value = 0
  )
  
  if(nrow(Pole2_00) == 0){
    assign("KMTs_minus_seed_P2",
           data.frame(),
           envir=.GlobalEnv)
    
  } else {
    assign("KMTs_minus_seed_P2",
           Minus_end_seed(which(colnames(Segments) == "Pole2_00")),
           envir=.GlobalEnv)
  }
  
  for(i in as.numeric(which(colnames(Segments) == "Pole2_00")+1) : as.numeric(ncol(Segments) - 4)){
    tryCatch({
      assign("DF",
             Minus_end_seed(i),
             envir=.GlobalEnv)
      assign("KMTs_minus_seed_P2",
             rbind(KMTs_minus_seed_P2,
                   DF),
             envir=.GlobalEnv)
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "P_nucleation2",
      value =  round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                     0)
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)
}