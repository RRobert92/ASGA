######################################
# Prepare data for the Post-Analysis #
######################################

############
# Settings #
############

if(No_of_Data = 1) {
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
  LD <- rbind(get(paste(Data_label, "_", 1, "_LD_P1", sep = "")),
              get(paste(Data_label, "_", 1, "_LD_P2", sep = "")))
  rm(list = setdiff(ls(), 
                    setdiff(ls(), 
                            list(paste(Data_label, "_", 1, "_LD_P1", sep = ""), 
                                 paste(Data_label, "_", 1, "_LD_P2", sep = "")))))
  
  for (i in 2:No_of_Data) {
    tryCatch({
      LD <- rbind(LD,
                  get(paste(Data_label, "_", i, "_LD_P1", sep = "")),
                  get(paste(Data_label, "_", i, "_LD_P2", sep = "")))
      rm(list = setdiff(ls(), 
                        setdiff(ls(), 
                                list(paste(Data_label, "_", i, "_LD_P1", sep = ""), 
                                     paste(Data_label, "_", i, "_LD_P2", sep = "")))))
    }, error = function(e){})
  }
  
} else if (DP == 2){
  
  
} else if (DP == 3){

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
  
  
  
}