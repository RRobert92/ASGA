#############################################
# The Package to define KMT end morphology #
#############################################

## Calculate % of similara cases for 2 measurements 
if(ncol(Nodes %>% select(starts_with("EndType"))) == 2){
  End_type_error <- End_Type_Error()
  write.xlsx(End_type_error, paste("Output/", Data_label, "_End_type_error.xlsx", sep = ""), row.names = FALSE)
}

if(ncol(Nodes %>% select(starts_with("EndType"))) >= 1){
  
  #####################################
  # Progress bar for (+) & (-) Pole_1 #
  #####################################
  
  total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
  
  pb <- winProgressBar(min = 2,
                       max =  total,
                       width = 400)
  
  ############################################################
  # Loop iterating through each KMT for the (+) & (-) Pole_1 #
  ############################################################
  
  assign("Plus_end_morphology_Pole1", 
         End_distribution_Plus(which(colnames(Segments) == "Pole1_00"),
                               1))
  
  assign("Minus_end_morphology_Pole1", 
         End_distribution_Minus(which(colnames(Segments) == "Pole1_00"),
                                1))
  
  for(i in as.numeric(which(colnames(Segments) == "Pole1_00")+1) : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
    tryCatch({
      DF <- data.frame()
      
      assign("DF",
             End_distribution_Plus(i,
                                   1))
      Plus_end_morphology_Pole1 <- rbind(Plus_end_morphology_Pole1,
                                         DF)
      
      assign("DF",
             End_distribution_Minus(i,
                                    1))
      Minus_end_morphology_Pole1 <- rbind(Minus_end_morphology_Pole1,
                                          DF)
    },
    error = function(e){})
    
    Sys.sleep(0.1)
    setWinProgressBar(pb, i, 
                      title = paste("Calcualting (+) & (-) end morphology for Pole_1", 
                                    round((i - 1) / total * 100,
                                          0),
                                    "% Done"))
  } 
  
  #################################
  # Bin data for (+) & (-) Pole_1 #
  #################################
  
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
  
  write.xlsx(Plus_end_morphology_Pole1, paste("bin/Output/", Data_label, "_(+)_morphology_P1.xlsx", sep = ""), row.names = FALSE)
  write.xlsx(Minus_end_morphology_Pole1, paste("bin/Output/", Data_label, "_(-)_morphology_P1.xlsx", sep = ""), row.names = FALSE)
  
  close(pb)
  rm(DF)

  
  #####################################
  # Progress bar for (+) & (-) Pole_2 #
  #####################################
  
  total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
    as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
  
  pb <- winProgressBar(min = 0,
                       max =  total,
                       width = 400)
  
  ############################################################
  # Loop iterating through each KMT for the (+) & (-) Pole_2 #
  ############################################################
  
  tryCatch({
    assign("Plus_end_morphology_Pole2", 
         End_distribution_Plus(which(colnames(Segments) == "Pole2_00"),
                               2))
  
  assign("Minus_end_morphology_Pole2", 
         End_distribution_Minus(which(colnames(Segments) == "Pole2_00"),
                                2))
  },
  error = function(e){}
  )
  
  for(i in as.numeric(which(colnames(Segments) == "Pole2_00")+1) : as.numeric(ncol(Segments) - 4)){
    tryCatch({
      DF <- data.frame()
      
      assign("DF",
             End_distribution_Plus(i,
                                   2))
      Plus_end_morphology_Pole2 <- rbind(Plus_end_morphology_Pole2,
                                         DF)
      
      assign("DF",
             End_distribution_Minus(i,
                                    2))
      Minus_end_morphology_Pole2 <- rbind(Minus_end_morphology_Pole2,
                                          DF)
    },
    error = function(e){}
    )
    
    Sys.sleep(0.1)
    setWinProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                      title = paste("Calcualting (+) & (-) end morphology for Pole_2",
                                    round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                                          0), 
                                    "% Done"))
  } 
  
  #################################
  # Bin data for (+) & (-) Pole_2 #
  #################################
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
    write.xlsx(Plus_end_morphology_Pole2, paste("bin/Output/", Data_label, "_(+)_morphology_P2.xlsx", sep = ""), row.names = FALSE)
    write.xlsx(Minus_end_morphology_Pole2, paste("bin/Output/", Data_label, "_(-)_morphology_P2.xlsx", sep = ""), row.names = FALSE)
  },
  error = function(e){})

  close(pb)
  rm(DF)
  
}
