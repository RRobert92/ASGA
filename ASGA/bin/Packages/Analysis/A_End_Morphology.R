################################################################################
# Packages End_Morphology
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-17 
################################################################################


# Set-up analysis --------------------------------------------------------------
A_End_Morphology <- function (input, output, session){

  if(ncol(Nodes %>% select(starts_with("EndType"))) == 2){
    End_type_error <- End_Type_Error()
  }
  
if(ncol(Nodes %>% select(starts_with("EndType"))) >= 1){
  
# Analyze end morphology for Pole1 ---------------------------------------------
  total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
  
  assign("Plus_end_morphology_Pole1", 
         End_distribution_Plus(which(colnames(Segments) == "Pole1_00"),
                               1),
         envir = .GlobalEn)
  
  assign("Minus_end_morphology_Pole1", 
         End_distribution_Minus(which(colnames(Segments) == "Pole1_00"),
                                1),
         envir = .GlobalEn)
  progressSweetAlert(
    session = session, id = "P_End_Morphology1",
    title = "Calcualting (+) & (-) end morphology for Pole_1...",
    display_pct = TRUE, value = 0
  )
  
  for(i in as.numeric(which(colnames(Segments) == "Pole1_00")+1) : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
    tryCatch({
      DF <- data.frame()
      
      assign("DF",
             End_distribution_Plus(i,
                                   1),
             envir = .GlobalEn)
      Plus_end_morphology_Pole1 <- rbind(Plus_end_morphology_Pole1,
                                         DF)
      
      assign("DF",
             End_distribution_Minus(i,
                                    1),
             envir = .GlobalEn)
      Minus_end_morphology_Pole1 <- rbind(Minus_end_morphology_Pole1,
                                          DF)
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "P_End_Morphology1",
      value = round((i - 1) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  
  closeSweetAlert(session = session)
  
  
# Bin data for the Pole1 --------------------------------------------------------
  if(ncol(Nodes %>% select(starts_with("EndType"))) == 2){
    Plus_end_morphology_Pole1 <- Plus_end_morphology_Pole1 %>% select("Fiber",
                                                                      starts_with("EndType"),
                                                                      "Entype_Different",
                                                                      "Relative_plus_position")
    for (i in 2:as.numeric(ncol(Nodes %>% select(starts_with("EndType")))+1)){
      names(Plus_end_morphology_Pole1)[i] <- paste("EndType_", as.numeric(i-1), sep = "")
    }
    
    Minus_end_morphology_Pole1 <- Minus_end_morphology_Pole1 %>% select("Fiber",
                                                                        starts_with("EndType"),
                                                                        "Entype_Different",
                                                                        "Relative_minus_position")
    for (i in 2:as.numeric(ncol(Nodes %>% select(starts_with("EndType")))+1)){
      names(Minus_end_morphology_Pole1)[i] <- paste("EndType_", as.numeric(i-1), sep = "")
    }
    
  } else {
    Plus_end_morphology_Pole1 <- Plus_end_morphology_Pole1 %>% select("Fiber",
                                                                      starts_with("EndType"),
                                                                      "Relative_plus_position")
    names(Plus_end_morphology_Pole1)[2] <- "EndType_1"
    Minus_end_morphology_Pole1 <- Minus_end_morphology_Pole1 %>% select("Fiber",
                                                                        starts_with("EndType"),
                                                                        "Relative_minus_position")
    names(Minus_end_morphology_Pole1)[2] <- "EndType_1"
  }
  Plus_end_morphology_Pole1 <<- Plus_end_morphology_Pole1
  Minus_end_morphology_Pole1 <<- Minus_end_morphology_Pole1

# Analyze end morphology for Pole2 ---------------------------------------------
  total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
    as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
  
  tryCatch({
    assign("Plus_end_morphology_Pole2", 
           End_distribution_Plus(which(colnames(Segments) == "Pole2_00"),
                                 2),
           envir = .GlobalEn)
    
    assign("Minus_end_morphology_Pole2", 
           End_distribution_Minus(which(colnames(Segments) == "Pole2_00"),
                                  2),
           envir = .GlobalEn)
  },
  error = function(e){}
  )
  
  progressSweetAlert(
    session = session, id = "P_End_Morphology2",
    title = "Calcualting (+) & (-) end morphology for Pole_2...",
    display_pct = TRUE, value = 0
  )
  
  for(i in as.numeric(which(colnames(Segments) == "Pole2_00")+1) : as.numeric(ncol(Segments) - 4)){
    tryCatch({
      DF <- data.frame()
      
      assign("DF",
             End_distribution_Plus(i,
                                   2),
             envir = .GlobalEn)
      Plus_end_morphology_Pole2 <- rbind(Plus_end_morphology_Pole2,
                                         DF)
      
      assign("DF",
             End_distribution_Minus(i,
                                    2),
             envir = .GlobalEn)
      Minus_end_morphology_Pole2 <- rbind(Minus_end_morphology_Pole2,
                                          DF)
    },
    error = function(e){}
    )
    
    updateProgressBar(
      session = session,
      id = "P_End_Morphology2",
      value = round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  
  closeSweetAlert(session = session)
  
# Bin data for the Pole1 --------------------------------------------------------
  tryCatch({
    if(ncol(Nodes %>% select(starts_with("EndType"))) >= 2){
      Plus_end_morphology_Pole2 <- Plus_end_morphology_Pole2 %>% select("Fiber",
                                                                        starts_with("EndType"),
                                                                        "Entype_Different",
                                                                        "Relative_plus_position")
      for (i in 2:as.numeric(ncol(Nodes %>% select(starts_with("EndType")))+1)){
        names(Plus_end_morphology_Pole2)[i] <- paste("EndType_", as.numeric(i-1), sep = "")
      }
      
      Minus_end_morphology_Pole2 <- Minus_end_morphology_Pole2 %>% select("Fiber",
                                                                          starts_with("EndType"),
                                                                          "Entype_Different",
                                                                          "Relative_minus_position")
      for (i in 2:as.numeric(ncol(Nodes %>% select(starts_with("EndType")))+1)){
        names(Minus_end_morphology_Pole2)[i] <- paste("EndType_", as.numeric(i-1), sep = "")
      }
      
    } else if (ncol(Nodes %>% select(starts_with("EndType"))) == 1){
      Plus_end_morphology_Pole2 <- Plus_end_morphology_Pole2 %>% select("Fiber",
                                                                        starts_with("EndType"),
                                                                        "Relative_plus_position")
      names(Plus_end_morphology_Pole2)[2] <- "EndType_1"
      Minus_end_morphology_Pole2 <- Minus_end_morphology_Pole2 %>% select("Fiber",
                                                                          starts_with("EndType"),
                                                                          "Relative_minus_position")
      names(Minus_end_morphology_Pole2)[2] <- "EndType_1"
    }
  },
  error = function(e){})
  Plus_end_morphology_Pole2 <<- Plus_end_morphology_Pole2
  Minus_end_morphology_Pole2 <<- Minus_end_morphology_Pole2
}
}