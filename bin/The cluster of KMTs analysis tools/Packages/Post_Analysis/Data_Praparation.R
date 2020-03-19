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

###############################################
# Standardized imported data to post-analysis #
###############################################
