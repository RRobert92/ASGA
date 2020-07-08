################################################################################
# Module Save_Data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
################################################################################


Save_Data <- function (input, output, session){
  # Save Data for LA ------------------------------------------------------------
  tryCatch({
    assign(paste("Data",current_data, "LD_P1", sep = "_"),
           LD_P1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "LD_P2", sep = "_"),
           LD_P2,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "LD", sep = "_"),
           rbind(LD_P1, LD_P2),
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "LD_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_LD_P1.xlsx", sep = ""))
    write.xlsx(get(paste("Data",current_data, "LD_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_LD_P2.xlsx", sep = ""))
    write.xlsx(get(paste("Data",current_data, "LD", sep = "_")), 
               paste("Data/", "Data_",current_data, "_LD.xlsx", sep = "")) 
  },
  error = function(e){})
  
  # Save Data for KMT No --------------------------------------------------------
  tryCatch({
    assign(paste("Data",current_data, "KMT_No_P1", sep = "_"),
           No_of_KMTs_at_kinetochore_P1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "KMT_No_P2", sep = "_"),
           No_of_KMTs_at_kinetochore_P2,
           envir = .GlobalEnv)
    names(No_of_KMTs_at_kinetochore_P1)[1] <- "KMTs_per_kinetochore"
    names(No_of_KMTs_at_kinetochore_P2)[1] <- "KMTs_per_kinetochore"
    assign(paste("Data",current_data, "KMT_No", sep = "_"),
           rbind(No_of_KMTs_at_kinetochore_P1, No_of_KMTs_at_kinetochore_P2),
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "KMT_No_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_No_P1.xlsx", sep = ""))
    write.xlsx(get(paste("Data",current_data, "KMT_No_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_No_P2.xlsx", sep = ""))
    write.xlsx(get(paste("Data",current_data, "KMT_No", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_No.xlsx", sep = ""))  
  },error = function(e){})
  
  
  # Save Data for KMT at the Pole -----------------------------------------------
  tryCatch({
    assign(paste("Data",current_data, "KMT_Pole_P1", sep = "_"),
           KMTs_at_the_Pole1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "KMT_Pole_P2", sep = "_"),
           KMTs_at_the_Pole2,
           envir = .GlobalEnv)
    names(KMTs_at_the_Pole1)[1] <- "KMTs_at_the_Pole"
    names(KMTs_at_the_Pole2)[1] <- "KMTs_at_the_Pole"
    assign(paste("Data",current_data, "KMT_Pole", sep = "_"),
           rbind(KMTs_at_the_Pole1, KMTs_at_the_Pole2),
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "KMT_Pole_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_Pole_P1.xlsx", sep = ""))
    write.xlsx(get(paste("Data",current_data, "KMT_Pole_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_Pole_P2.xlsx", sep = ""))
    write.xlsx(get(paste("Data",current_data, "KMT_Pole", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_Pole.xlsx", sep = ""))  
  },error = function(e){})
  
  # Save Data for KMT at the Pole -----------------------------------------------
  tryCatch({
    assign(paste("Data",current_data, "KMT_Minus_Ends", sep = "_"),
           Minus_end_position,
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "KMT_Minus_Ends", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_Minus_Ends.xlsx", sep = "")) 
  },error = function(e){})
  
  # Save Data for end morphology Pole -------------------------------------------
  tryCatch({
    assign(paste("Data",current_data, "Minus_end_morphology_Pole1", sep = "_"),
           Minus_end_morphology_Pole1,
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "Minus_end_morphology_Pole1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Minus_end_morphology_Pole1.xlsx", sep = ""))
    
    assign(paste("Data",current_data, "Plus_end_morphology_Pole1", sep = "_"),
           Plus_end_morphology_Pole1,
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "Plus_end_morphology_Pole1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Plus_end_morphology_Pole1.xlsx", sep = ""))
    
    assign(paste("Data",current_data, "Minus_end_morphology_Pole2", sep = "_"),
           Minus_end_morphology_Pole2,
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "Minus_end_morphology_Pole2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Minus_end_morphology_Pole2.xlsx", sep = ""))
    
    assign(paste("Data",current_data, "Plus_end_morphology_Pole2", sep = "_"),
           Plus_end_morphology_Pole2,
           envir = .GlobalEnv)    
    write.xlsx(get(paste("Data",current_data, "Plus_end_morphology_Pole2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Plus_end_morphology_Pole2.xlsx", sep = ""))
    
    assign(paste("Data",current_data, "Plus_end_morphology", sep = "_"),
           rbind(Plus_end_morphology_Pole1, Plus_end_morphology_Pole2),
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "Minus_end_morphology", sep = "_"),
           rbind(Minus_end_morphology_Pole1, Minus_end_morphology_Pole2),
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "Plus_end_morphology", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Plus_end_morphology.xlsx", sep = ""))
    write.xlsx(get(paste("Data",current_data, "Minus_end_morphology", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Minus_end_morphology.xlsx", sep = ""))  
  },error = function(e){})
  
  # Save Data for LKD -----------------------------------------------------------
  tryCatch({
    assign(paste("Data",current_data, "IKD", sep = "_"),
           Inter_Kinetochore_Distance,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "IKD_KMT_Delta", sep = "_"),
           Inter_Kinetochore_Distance_KMTs_delta,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "IKD_KMT_No", sep = "_"),
           Inter_Kinetochore_Distance_KMTs_no,
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "IKD", sep = "_")), 
               paste("Data/", "Data_",current_data, "_IKD.xlsx", sep = ""))  
    write.xlsx(get(paste("Data",current_data, "IKD_KMT_Delta", sep = "_")), 
               paste("Data/", "Data_",current_data, "_IKD_KMT_Delta.xlsx", sep = "")) 
    write.xlsx(get(paste("Data",current_data, "IKD_KMT_No", sep = "_")), 
               paste("Data/", "Data_",current_data, "_IKD_KMT_No.xlsx", sep = "")) 
  },error = function(e){})
  
  # Save Data for total curvature -----------------------------------------------
  tryCatch({
    assign(paste("Data",current_data, "KMT_Total_Curv_P1", sep = "_"),
           KMTs_total_Curvature_P1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "KMT_Total_Curv_P2", sep = "_"),
           KMTs_total_Curvature_P2,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "KMT_Total_Curv", sep = "_"),
           rbind(KMTs_total_Curvature_P1, KMTs_total_Curvature_P2),
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "KMT_Total_Curv_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_Total_Curv_P1.xlsx", sep = ""))  
    write.xlsx(get(paste("Data",current_data, "KMT_Total_Curv_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_Total_Curv_P2.xlsx", sep = "")) 
    write.xlsx(get(paste("Data",current_data, "KMT_Total_Curv", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_Total_Curv.xlsx", sep = "")) 
  },error = function(e){})
  
  # Save Data for local curvature ----------------------------------------------
  tryCatch({
    assign(paste("Data",current_data, "KMT_Local_Curv_P1", sep = "_"),
           KMTs_local_Curvature_P1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "KMT_Local_Curv_P2", sep = "_"),
           KMTs_local_Curvature_P2,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "KMT_Local_Curv", sep = "_"),
           rbind(KMTs_local_Curvature_P1, KMTs_local_Curvature_P2),
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "KMT_Local_Curv_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_Local_Curv_P1.xlsx", sep = ""))  
    write.xlsx(get(paste("Data",current_data, "KMT_Local_Curv_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_Local_Curv_P2.xlsx", sep = "")) 
    write.xlsx(get(paste("Data",current_data, "KMT_Local_Curv", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMT_Local_Curv.xlsx", sep = "")) 
  },error = function(e){})
  
  # Save Data for Fiber area ----------------------------------------------------
  tryCatch({
    assign(paste("Data",current_data, "Fiber_Area_P1", sep = "_"),
           Fiber_area_P1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "Fiber_Area_P2", sep = "_"),
           Fiber_area_P2,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "Fiber_Area", sep = "_"),
           rbind(Fiber_area_P1, Fiber_area_P2),
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "Fiber_Area_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Area_P1.xlsx", sep = ""))  
    write.xlsx(get(paste("Data",current_data, "Fiber_Area_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Area_P2.xlsx", sep = "")) 
    write.xlsx(get(paste("Data",current_data, "Fiber_Area", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Area.xlsx", sep = "")) 
  },error = function(e){})
  
  # Save Data for Density fiber -------------------------------------------------
  tryCatch({
    assign(paste("Data",current_data, "N_Density_P1", sep = "_"),
           N_density_P1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "N_Density_P2", sep = "_"),
           N_density_P1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "N_Density", sep = "_"),
           rbind(N_density_P1, N_density_P2),
           envir = .GlobalEnv)
    write.xlsx(get(paste("Data",current_data, "N_Density_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_N_Density_P1.xlsx", sep = ""))  
    write.xlsx(get(paste("Data",current_data, "N_Density_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_N_Density_P2.xlsx", sep = "")) 
    write.xlsx(get(paste("Data",current_data, "N_Density", sep = "_")), 
               paste("Data/", "Data_",current_data, "_N_Density.xlsx", sep = ""))
  },error = function(e){})
  
  # Save Data for Fiber length and curvature fiber ------------------------------
  tryCatch({
    names(Fiber_Length_P1)[1] <- "Length"
    names(Fiber_Length_P2)[1] <- "Length"
    
    assign(paste("Data",current_data, "Fiber_Length_P1", sep = "_"),
           Fiber_Length_P1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "Fiber_Length_P2", sep = "_"),
           Fiber_Length_P2,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "Fiber_Length", sep = "_"),
           rbind(Fiber_Length_P1, Fiber_Length_P2),
           envir = .GlobalEnv)

    assign(paste("Data",current_data, "Fiber_Total_Curv_P1", sep = "_"),
           Fiber_Total_Curv_P1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "Fiber_Total_Curv_P2", sep = "_"),
           Fiber_Total_Curv_P2,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "Fiber_Total_Curv", sep = "_"),
           rbind(Fiber_Total_Curv_P1, Fiber_Total_Curv_P2),
           envir = .GlobalEnv)
    
    assign(paste("Data",current_data, "Fiber_Local_Curv_P1", sep = "_"),
           Fiber_Local_Curv_P1,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "Fiber_Local_Curv_P2", sep = "_"),
           Fiber_Local_Curv_P2,
           envir = .GlobalEnv)
    assign(paste("Data",current_data, "Fiber_Local_Curv", sep = "_"),
           rbind(Fiber_Local_Curv_P1, Fiber_Local_Curv_P2),
           envir = .GlobalEnv)
    
    write.xlsx(get(paste("Data",current_data, "Fiber_Length_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Length_P1.xlsx", sep = ""))  
    write.xlsx(get(paste("Data",current_data, "Fiber_Length_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Length_P2.xlsx", sep = "")) 
    write.xlsx(get(paste("Data",current_data, "Fiber_Length", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Length.xlsx", sep = ""))
    
    write.xlsx(get(paste("Data",current_data, "Fiber_Total_Curv_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Total_Curv_P1.xlsx", sep = ""))  
    write.xlsx(get(paste("Data",current_data, "Fiber_Total_Curv_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Total_Curv_P2.xlsx", sep = "")) 
    write.xlsx(get(paste("Data",current_data, "Fiber_Total_Curv", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Total_Curv.xlsx", sep = ""))
    
    write.xlsx(get(paste("Data",current_data, "Fiber_Local_Curv_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Local_Curv_P1.xlsx", sep = ""))  
    write.xlsx(get(paste("Data",current_data, "Fiber_Local_Curv_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Local_Curv_P2.xlsx", sep = "")) 
    write.xlsx(get(paste("Data",current_data, "Fiber_Local_Curv", sep = "_")), 
               paste("Data/", "Data_",current_data, "_Fiber_Local_Curv.xlsx", sep = ""))
    
  },error = function(e){})
  
  # Save Data for nucleation ----------------------------------------------------
  tryCatch({
    assign(paste("Data", current_data, "KMTs_minus_seed_P1", sep = "_"),
           KMTs_minus_seed_P1,
           envir = .GlobalEnv)
    assign(paste("Data", current_data, "KMTs_minus_seed_P2", sep = "_"),
           KMTs_minus_seed_P2,
           envir = .GlobalEnv)  
    assign(paste("Data",current_data, "KMTs_minus_seed", sep = "_"),
           rbind(KMTs_minus_seed_P1, KMTs_minus_seed_P2),
           envir = .GlobalEnv) 
    write.xlsx(get(paste("Data",current_data, "KMTs_minus_seed_P1", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMTs_minus_seed_P1.xlsx", sep = ""))
    write.xlsx(get(paste("Data",current_data, "KMTs_minus_seed_P2", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMTs_minus_seed_P2.xlsx", sep = ""))
    write.xlsx(get(paste("Data",current_data, "KMTs_minus_seed", sep = "_")), 
               paste("Data/", "Data_",current_data, "_KMTs_minus_seed.xlsx", sep = ""))   
  },error = function(e){})
  
  
  # Clean Environment -----------------------------------------------------------
  rm(list = ls(pattern = "Pole"))
  rm(list = ls(pattern = "DF"))
}