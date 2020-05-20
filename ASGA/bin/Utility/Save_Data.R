################################################################################
# Module Save_Data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-20
################################################################################


Save_Data <- function (input, output, session){
# Save Data for LA ------------------------------------------------------------
  assign(paste("Data",current_data, "LD_P1", sep = "_"),
         LD_P1,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "LD_P2", sep = "_"),
         LD_P2,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "LD", sep = "_"),
         rbind(LD_P1, LD_P2),
         envir = .GlobalEnv)

# Save Data for KMT No --------------------------------------------------------
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

# Save Data for KMT at the Pole -----------------------------------------------
  assign(paste("Data",current_data, "KMT_Pole_P1", sep = "_"),
         KMTs_at_the_Pole1,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "KMT_Pole_P2", sep = "_"),
         KMTs_at_the_Pole2,
         envir = .GlobalEnv)
  names(KMTs_at_the_Pole1)[1] <- "KMTs_at_the_Pole"
  names(KMTs_at_the_Pole2)[1] <- "KMTs_at_the_Pole"
  assign(paste("Data",current_data, "KMT_No", sep = "_"),
         rbind(KMTs_at_the_Pole1, KMTs_at_the_Pole2),
         envir = .GlobalEnv)

# Save Data for KMT at the Pole -----------------------------------------------
  assign(paste("Data",current_data, "KMT_Minus_Ends", sep = "_"),
         Minus_end_position,
         envir = .GlobalEnv)
  
# Save Data for LKD -----------------------------------------------------------
  assign(paste("Data",current_data, "IKD", sep = "_"),
         Inter_Kinetochore_Distance,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "IKD_KMT_Delta", sep = "_"),
         Inter_Kinetochore_Distance_KMTs_delta,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "IKD_KMT_No", sep = "_"),
         Inter_Kinetochore_Distance_KMTs_no,
         envir = .GlobalEnv)

# Save Data for total curvature -----------------------------------------------
  assign(paste("Data",current_data, "KMT_Total_Curv_P1", sep = "_"),
         KMTs_total_Curvature_P1,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "KMT_Total_Curv_P2", sep = "_"),
         KMTs_total_Curvature_P2,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "KMT_No", sep = "_"),
         rbind(KMTs_total_Curvature_P1, KMTs_total_Curvature_P2),
         envir = .GlobalEnv)

  # Save Data for local curvature ----------------------------------------------
  assign(paste("Data",current_data, "KMT_Local_Curv_P1", sep = "_"),
         KMTs_local_Curvature_P1,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "KMT_Local_Curv_P2", sep = "_"),
         KMTs_local_Curvature_P2,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "KMT_No", sep = "_"),
         rbind(KMTs_local_Curvature_P1, KMTs_local_Curvature_P2),
         envir = .GlobalEnv)

# Save Data for Fiber area ----------------------------------------------------
  assign(paste("Data",current_data, "Fiber_Area_P1", sep = "_"),
         Fiber_area_P1,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "Fiber_Area_P2", sep = "_"),
         Fiber_area_P2,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "Fiber_Area", sep = "_"),
         rbind(Fiber_area_P1, Fiber_area_P2),
         envir = .GlobalEnv)
  
# Save Data for Density fiber -------------------------------------------------
  assign(paste("Data",current_data, "N_Density_P1", sep = "_"),
         N_density_P1,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "N_Density_P2", sep = "_"),
         N_density_P1,
         envir = .GlobalEnv)
  assign(paste("Data",current_data, "N_Density", sep = "_"),
         rbind(N_density_P1, N_density_P2),
         envir = .GlobalEnv)
  
# Clean Environment -----------------------------------------------------------
  rm(list = ls(pattern = "Pole"))
  rm(list = ls(pattern = "DF"))
}