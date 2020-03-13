#####################################
# The cluster of KMTs analysis tools #
#####################################
rm(list = ls())
#######################################
# Library (load & install if missing) #
#######################################

source("Utility/Library.R")

############
# Settings #
############

source("Utility/Settings.R")

###########################
# !Check files structure! #
###########################

source("Utility/Check_Data.R")

#############
# Load Data #
#############

## Load data from the file and split for Segments, Points, and Nodes
source("Utility/Load_Data.r")

#############
# Functions #
#############

source("Tools/Core/Sort_by_Fiber.R")
source("Tools/Core/Select_Points.R")
source("Tools/Core/Find_XYZ.R")
source("Tools/Core/Kinetochore_Position.R")
source("Tools/Core/Sort_All_Points_to_Start_From_the_Kinetochore.R")
source("Tools/Core/T_Relative_position.R")
source("Tools/Analysis/Length_Distiribution.R")
source("Tools/Analysis/No_of_KMTs_connected_to_the_Pole.R")
source("Tools/Analysis/No_of_KMTs.R")
source("Tools/Analysis/T_Inter_Kinetochore_Dist.R")
source("Tools/Analysis/T_KMT_Curvature.R")

#############
# Read data #
#############

## Extract individual KMTs
source("Packages/Core/Get_Single_KMTs_From_Labels.R")

## Sort point in each KMT. Always the first point will be a point on a kinetochore
source("Packages/Core/Sort_KMTs_by_Kinetochore_Position.R")

## Core function to formated PoleX_YY DF: [1] Segment ID, [2] length, [3] minus_dist_to_pole, [4] plus_dist_to_pole
## Length distribution LD: [1] length
source("Packages/Core/Analyse_Length_Distiribution.R")

## Calculate relative position of each point on the kinetochore - centriol (Pole1/Pole2) axis
source("Packages/Core/P_Relative_position.R")
close(pb)

############
# Analysis #
############

## DF output of the functions:
## 0 - All below
## 1 - KMTs_at_the_Pole: [1] No. of KMTs
##   - KTMs_at_the_Pole_and_length: [1] No. of KMTs, [2] KMTs length, [3] Minus end dist., [4] Plus end dist.
## 2 - Inter_Kinetochore_Distance: [1] Inter-kinetochore distance
##   - Inter_Kinetochore_Distance_KMTs_no: [1] Inter-kinetochore distance, [2] KMTs no.
##   - Inter_Kinetochore_Distance_KTMs_delta: [1] Inter-kinetochore distance, [2] Delta of KMTs
## 3 - KMTs_total_Curvature: [1] Curvature, [2] k0fiber no.
## 4 - 

if(analysis == 0){
  source("Packages/Analysis/Analyse_No_of_KMTs_Reaching_the_Pole.R")
  source("Packages/Analysis/No_of_KMTs_at_a_kinetochore.R")
  source("Packages/Analysis/P_Inter_Kinetochore_Dist.R")
  source("Packages/Analysis/P_KMT_Curvature.R")
  
} else if (analysis == "1"){
  source("Packages/Analysis/Analyse_No_of_KMTs_Reaching_the_Pole.R")
  source("Packages/Analysis/No_of_KMTs_at_a_kinetochore.R")
           
} else if (analysis == "2"){
  source("Packages/Analysis/P_Inter_Kinetochore_Dist.R")
  
} else if (analysis == "3"){
  source("Packages/Analysis/P_KMT_Curvature.R")
  
} else if (analysis == "4"){
  source("Packages/Analysis/....R")
  
} else if (analysis == "5"){
  source("Packages/Analysis/....R")
  
} else if (analysis == "6"){
  source("Packages/Analysis/....R")
  
} else if (analysis == "7"){
  source("Packages/Analysis/....R")
  
}
rm(all, analysis, Data, i, j, ncol, Point_KMT, total, Poles)

##############
# Plots Data #
##############
source("Tools/Plots/PL_Local_Curvature.R")

#############
# Save Data #
#############
KMTs1 <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)

write.xlsx(LD[1:nrow(Segments_1),], paste("Output/", Data_label, "_LD.xlsx", sep = ""), row.names = FALSE)
write.xlsx(Inter_Kinetochore_Distance, paste("Output/", Data_label, "_Inter_Kinetochore_Distance.xlsx", sep = ""), row.names = FALSE)
write.xlsx(No_of_KMTs_at_kinetochore[1:KMTs1,], paste("Output/", Data_label, "_KMTs_no.xlsx", sep = ""), row.names = FALSE)
write.xlsx(KMTs_to_the_Pole1_and_length, paste("Output/", Data_label, "_Minus_end_position.xlsx", sep = ""), row.names = FALSE)
write.xlsx(KMTs_total_Curvature[1:nrow(Segments_1),], paste("Output/", Data_label, "_KMTs_total_Curvature.xlsx", sep = ""), row.names = FALSE)
