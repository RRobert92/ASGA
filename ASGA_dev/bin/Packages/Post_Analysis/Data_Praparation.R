######################################
# Prepare data for the Post-Analysis #
######################################

############
# Settings #
############

if(No_of_Data == 1) {
DP <- dlg_input("
                                            You are in the final stage! 
                      The software will now Post-Analyse your one dataset.
                      
How would you like to analyze your data?
            1 - Analyse data that belong to the Pole_1 and Pole_2 TOGETHER
            2 - Analyse data that belong to the Pole_1 and Pole_2 SEPARATE*
            3 - Analyse ONLY data that belong to the Pole_1
                
                                                                                                *Not avaiable yet",1)$res
  
} else if (No_of_Data > 1) {
  DP <- dlg_input("                                            
                                            You are in the final stage! 
                      The software will now Post-Analyse your all dataset.
                      
How would you like to analyze your data?
            1 - Analyse data that belong to the Pole_1 and Pole_2 TOGETHER
            2 - Analyse data that belong to the Pole_1 and Pole_2 SEPARATE*
            3 - Analyse ONLY data that belong to the Pole_1
                
                                                                                                *Not avaiable yet",1)$res
  
}

DP <- as.numeric(DP)
###############################################
# Standardized imported data to post-analysis #
###############################################

if(DP == 1){
## Length Distribution
  tryCatch({
    assign(paste(Data_label,"_", 1,"_LD", sep = ""),
           rbind(get(paste(Data_label, "_", 1, "_LD_P1", sep = "")),
                 get(paste(Data_label, "_", 1, "_LD_P2", sep = ""))))
  
  rm(list = setdiff(ls(), 
                    setdiff(ls(), 
                            list(paste(Data_label, "_", 1, "_LD_P1", sep = ""), 
                                 paste(Data_label, "_", 1, "_LD_P2", sep = "")))))
  assign(paste(Data_label, "_LD_ALL", sep = ""),
         get(paste(Data_label,"_", 1,"_LD", sep = "")))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      assign(paste(Data_label,"_", i,"_LD", sep = ""),
             rbind(get(paste(Data_label, "_", i, "_LD_P1", sep = "")),
                   get(paste(Data_label, "_", i, "_LD_P2", sep = ""))))
      
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_LD_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_LD_P2", sep = "")))))
      assign(paste(Data_label, "_LD_ALL", sep = ""),
         rbind(get(paste(Data_label, "_LD_ALL", sep = "")), 
               get(paste(Data_label,"_", i,"_LD", sep = ""))))
    }, error = function(e){})
  }
  
## Inter-kinetochore distance
  tryCatch({
    assign(paste(Data_label, "_", 1, "_IKD", sep = ""),
           get(paste(Data_label, "_", 1, "_IKD", sep = "")))
    
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_IKD", sep = "")))))
    assign(paste(Data_label, "_IKD_ALL", sep = ""),
           get(paste(Data_label, "_", 1,"_IKD", sep = "")))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      assign(paste(Data_label,"_", i,"_IKD", sep = ""),
             get(paste(Data_label, "_", i, "_IKD", sep = "")))
      
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_IKD", sep = "")))))
      assign(paste(Data_label, "_IKD_ALL", sep = ""),
             rbind(get(paste(Data_label, "_IKD_ALL", sep = "")),
                   get(paste(Data_label,"_", i,"_IKD", sep = ""))))
    }, error = function(e){})
  }

## No of a KMTs at the kinetochore
  tryCatch({
    assign(paste(Data_label, "_", 1, "_KMTs_at_K", sep = ""),
           rbind(get(paste(Data_label, "_", 1, "_KMTs_at_K1", sep = "")),
                 get(paste(Data_label, "_", 1, "_KMTs_at_K2", sep = ""))))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_KMTs_at_K1", sep = ""), 
                                   paste(Data_label, "_", 1, "_KMTs_at_K2", sep = "")))))
    assign(paste(Data_label, "_KMTs_at_K_ALL", sep = ""),
           get(paste(Data_label, "_", 1, "_KMTs_at_K", sep = "")))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      assign(paste(Data_label, "_", i, "_KMTs_at_K", sep = ""),
             rbind(get(paste(Data_label, "_", i, "_KMTs_at_K1", sep = "")),
                   get(paste(Data_label, "_", i, "_KMTs_at_K2", sep = ""))))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_KMTs_at_K1", sep = ""), 
                                     paste(Data_label, "_", i, "_KMTs_at_K2", sep = "")))))
      assign(paste(Data_label, "_KMTs_at_K_ALL", sep = ""),
             rbind(get(paste(Data_label, "_KMTs_at_K_ALL", sep = "")), 
                   get(paste(Data_label, "_", i, "_KMTs_at_K", sep = ""))))
    }, error = function(e){})
  }

## No of a KMTs at the Pole1/2
  tryCatch({
    assign(paste(Data_label, "_", 1, "_KMTs_at_P", sep = ""),
           rbind(get(paste(Data_label, "_", 1, "_KMTs_at_P1", sep = "")),
                 get(paste(Data_label, "_", 1, "_KMTs_at_P2", sep = ""))))
    
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_KMTs_at_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_KMTs_at_P2", sep = "")))))
    assign(paste(Data_label, "_KMTs_at_P_ALL", sep = ""),
           get(paste(Data_label, "_", 1, "_KMTs_at_P", sep = "")))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      assign(paste(Data_label, "_", i, "_KMTs_at_P", sep = ""),
             rbind(get(paste(Data_label, "_", i, "_KMTs_at_P1", sep = "")),
                   get(paste(Data_label, "_", i, "_KMTs_at_P2", sep = ""))))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_KMTs_at_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_KMTs_at_P2", sep = "")))))
      assign(paste(Data_label, "_KMTs_at_P_ALL", sep = ""),
             rbind(get(paste(Data_label, "_KMTs_at_P_ALL", sep = "")),
                   get(paste(Data_label, "_", i, "_KMTs_at_P", sep = ""))))
    }, error = function(e){})
  }
  
## Minus end position
  tryCatch({
    assign(paste(Data_label, "_", 1, "_Minus_end", sep = ""),
           rbind(get(paste(Data_label, "_", 1, "_Minus_end_P1", sep = "")),
                 get(paste(Data_label, "_", 1, "_Minus_end_P2", sep = ""))))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_Minus_end_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_Minus_end_P2", sep = "")))))
    assign(paste(Data_label, "_Minus_end_ALL", sep = ""),
           get(paste(Data_label, "_", 1, "_Minus_end", sep = "")))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      assign(paste(Data_label, "_", i, "_Minus_end", sep = ""),
             rbind(get(paste(Data_label, "_", i, "_Minus_end_P1", sep = "")),
                   get(paste(Data_label, "_", i, "_Minus_end_P2", sep = ""))))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_Minus_end_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_Minus_end_P2", sep = "")))))
      assign(paste(Data_label, "_Minus_end_ALL", sep = ""),
             rbind(get(paste(Data_label, "_Minus_end_ALL", sep = "")),
                   get(paste(Data_label, "_", i, "_Minus_end", sep = ""))))
    }, error = function(e){})
  }
  
## Total curvature
  tryCatch({
    assign(paste(Data_label, "_", 1, "_Total_Curvature", sep = ""),
           rbind(get(paste(Data_label, "_", 1, "_total_curvature_P1", sep = "")),
                 get(paste(Data_label, "_", 1, "_total_curvature_P2", sep = ""))))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_total_curvature_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_total_curvature_P2", sep = "")))))
    assign(paste(Data_label, "_Total_Curvature_ALL", sep = ""),
           get(paste(Data_label, "_", 1, "_Total_Curvature", sep = "")))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      assign(paste(Data_label, "_", i, "_Total_Curvature", sep = ""),
             rbind(get(paste(Data_label, "_", i, "_total_curvature_P1", sep = "")),
                   get(paste(Data_label, "_", i, "_total_curvature_P2", sep = ""))))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_total_curvature_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_total_curvature_P2", sep = "")))))
      assign(paste(Data_label, "_Total_Curvature_ALL", sep = ""),
             rbind(get(paste(Data_label, "_Total_Curvature_ALL", sep = "")),
                   get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))))
    }, error = function(e){})
  }
  
## Local curvature
  tryCatch({
    assign(paste(Data_label, "_", 1, "_Local_Curvature", sep = ""),
           rbind(get(paste(Data_label, "_", 1, "_local_curvature_P1", sep = "")),
                 get(paste(Data_label, "_", 1, "_local_curvature_P2", sep = ""))))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_local_curvature_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_local_curvature_P2", sep = "")))))
    assign(paste(Data_label, "_Local_Curvature_ALL", sep = ""),
           get(paste(Data_label, "_", 1, "_Local_Curvature", sep = "")))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      assign(paste(Data_label, "_", i, "_Local_Curvature", sep = ""),
             rbind(get(paste(Data_label, "_", i, "_local_curvature_P1", sep = "")),
                   get(paste(Data_label, "_", i, "_local_curvature_P2", sep = ""))))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_local_curvature_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_local_curvature_P2", sep = "")))))
      assign(paste(Data_label, "_Local_Curvature_ALL", sep = ""),
             rbind(get(paste(Data_label, "_Local_Curvature_ALL", sep = "")),
                   get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))))
    }, error = function(e){})
  }
  
## (-) end seed
  tryCatch({
    assign(paste(Data_label, "_", 1, "_KMTs_minus_seed", sep = ""),
           rbind(get(paste(Data_label, "_", 1, "_KMTs_minus_seed_P1", sep = "")),
                 get(paste(Data_label, "_", 1, "_KMTs_minus_seed_P2", sep = ""))))
    
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_KMTs_minus_seed_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_KMTs_minus_seed_P2", sep = "")))))
    
    assign(paste(Data_label, "_KMT_(-)_seed_ALL", sep = ""),
           get(paste(Data_label, "_", 1, "_KMTs_minus_seed", sep = "")))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      assign(paste(Data_label, "_", i, "_KMTs_minus_seed", sep = ""),
             rbind(get(paste(Data_label, "_", i, "_KMTs_minus_seed_P1", sep = "")),
                   get(paste(Data_label, "_", i, "_KMTs_minus_seed_P2", sep = ""))))
      
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_KMTs_minus_seed_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_KMTs_minus_seed_P2", sep = "")))))
      
      assign(paste(Data_label, "_KMT_(-)_seed_ALL", sep = ""),
             rbind(get(paste(Data_label, "_KMT_(-)_seed_ALL", sep = "")), 
                   get(paste(Data_label, "_", i, "_KMTs_minus_seed", sep = ""))))
    }, error = function(e){})
  }
  
  
} else if (DP == 2){
  
  
} else if (DP == 3){
## Length Distribution
  tryCatch({
  LD <- get(paste(Data_label, "_", 1, "_LD_P1", sep = ""))
  rm(list = setdiff(ls(), 
                    setdiff(ls(), 
                            list(paste(Data_label, "_", 1, "_LD_P1", sep = ""), 
                                 paste(Data_label, "_", 1, "_LD_P2", sep = "")))))
  }, error = function(e){})

  for (i in 2:No_of_Data) {
    tryCatch({
       LD <- rbind(LD,
              get(paste(Data_label, "_", i, "_LD_P1", sep = "")))
       rm(list = setdiff(ls(), 
                         setdiff(ls(), 
                                 list(paste(Data_label, "_", i, "_LD_P1", sep = ""), 
                                      paste(Data_label, "_", i, "_LD_P2", sep = "")))))
    }, error = function(e){})
  }
  
## Inter-kinetochore distance
  tryCatch({
    IKD <- get(paste(Data_label, "_", 1, "_IKD", sep = ""))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_IKD", sep = "")))))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      IKD <- rbind(LD,
                  get(paste(Data_label, "_", i, "_IKD", sep = "")))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_IKD", sep = "")))))
    }, error = function(e){})
  }
  
## No of a KMTs at the kinetochore
  tryCatch({
    KMTs_at_K <- get(paste(Data_label, "_", 1, "_KMTs_at_K1", sep = ""))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_KMTs_at_K1", sep = ""), 
                                   paste(Data_label, "_", 1, "_KMTs_at_K2", sep = "")))))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      KMTs_at_K <- rbind(KMTs_at_K,
                         get(paste(Data_label, "_", i, "_KMTs_at_K1", sep = "")))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_KMTs_at_K1", sep = ""), 
                                     paste(Data_label, "_", i, "_KMTs_at_K2", sep = "")))))
    }, error = function(e){})
  }
  
## No of a KMTs at the Pole1
  tryCatch({
    KMTs_at_P <- get(paste(Data_label, "_", 1, "_KMTs_at_P1", sep = ""))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_KMTs_at_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_KMTs_at_P2", sep = "")))))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      KMTs_at_P <- rbind(KMTs_at_P,
                         get(paste(Data_label, "_", i, "_KMTs_at_P1", sep = "")))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_KMTs_at_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_KMTs_at_P2", sep = "")))))
    }, error = function(e){})
  }
  
## Minus end position
  tryCatch({
    Minus_end_pos <- get(paste(Data_label, "_", 1, "_Minus_end_P1", sep = ""))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_Minus_end_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_Minus_end_P2", sep = "")))))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      Minus_end_pos <- rbind(Minus_end_pos,
                             get(paste(Data_label, "_", i, "_Minus_end_P1", sep = "")))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_Minus_end_P1", sep = ""), 
                                    paste(Data_label, "_", i, "_Minus_end_P2", sep = "")))))
    }, error = function(e){})
  }
  
## Total curvature
  tryCatch({
    Total_Curvature <- get(paste(Data_label, "_", 1, "_total_curvature_P1", sep = ""))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_total_curvature_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_total_curvature_P2", sep = "")))))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      Total_Curvature <- rbind(Total_Curvature,
                               get(paste(Data_label, "_", i, "_total_curvature_P1", sep = "")))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_total_curvature_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_total_curvature_P2", sep = "")))))
    }, error = function(e){})
  }
  
## Local curvature
  tryCatch({
    Local_Curvature <- get(paste(Data_label, "_", 1, "_local_curvature_P1", sep = ""))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_local_curvature_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_local_curvature_P2", sep = "")))))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      Local_Curvature <- rbind(Local_Curvature,
                               get(paste(Data_label, "_", i, "_local_curvature_P1", sep = "")))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_local_curvature_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_local_curvature_P2", sep = "")))))
    }, error = function(e){})
  }
  
## End morphology error
  tryCatch({
    End_Type_Error <- get(paste(Data_label, "_", i, "_End_type_error", sep = ""))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_End_type_error", sep = "")))))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      End_Type_Error <- rbind(End_Type_Error,
                              get(paste(Data_label, "_", i, "_End_type_error", sep = "")))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_End_type_error", sep = "")))))
    }, error = function(e){})
  }
 
## Plus end morphology 
  tryCatch({
    Plus_End_Morphology <- get(paste(Data_label, "_", 1, "_(+)_end_morphology_P1", sep = ""))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_(+)_end_morphology_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_(+)_end_morphology_P2", sep = "")))))
  }, error = function(e){})
  
  for (i in 2:No_of_Data) {
    tryCatch({
      Plus_End_Morphology <- rbind(Plus_End_Morphology,
                                   get(paste(Data_label, "_", i, "_(+)_end_morphology_P1", sep = "")))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_(+)_end_morphology_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_(+)_end_morphology_P2", sep = "")))))
    }, error = function(e){})
  }
  
## Minus end morphology
  tryCatch({
    Minus_End_Morphology <- get(paste(Data_label, "_", 1, "_(-)_end_morphology_P1", sep = ""))
    rm(list = setdiff(ls(), 
                      setdiff(ls(), 
                              list(paste(Data_label, "_", 1, "_(-)_end_morphology_P1", sep = ""), 
                                   paste(Data_label, "_", 1, "_(-)_end_morphology_P2", sep = "")))))
  }, error = function(e){})

  for (i in 2:No_of_Data) {
    tryCatch({
      Minus_End_Morphology <- rbind(Minus_End_Morphology,
                                   get(paste(Data_label, "_", i, "_(-)_end_morphology_P1", sep = "")))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_(-)_end_morphology_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_(-)_end_morphology_P2", sep = "")))))
    }, error = function(e){})
  }
}