######################################
# The cluster of KMTs analysis tools #
######################################

############
# Settings #
############

source("bin/Utility/Settings.R")

###########################
# !Check files structure! #
###########################

source("bin/Utility/Check_Data.R")

#############
# Load Data #
#############

source("bin/Utility/Load_Data.R")

#############
# Functions #
#############

source("bin/Tools/Pre_Analysis/Sort_by_Fiber.R")
source("bin/Tools/Pre_Analysis/Select_Points.R")
source("bin/Tools/Pre_Analysis/Find_XYZ.R")
source("bin/Tools/Pre_Analysis/Kinetochore_Position.R")
source("bin/Tools/Pre_Analysis/Sort_All_Points_to_Start_From_the_Kinetochore.R")
source("bin/Tools/Pre_Analysis/T_Relative_position.R")
source("bin/Tools/Analysis/Length_Distiribution.R")
source("bin/Tools/Analysis/No_of_KMTs_connected_to_the_Pole.R")
source("bin/Tools/Analysis/No_of_KMTs.R")
source("bin/Tools/Analysis/T_Inter_Kinetochore_Dist.R")
source("bin/Tools/Analysis/T_KMT_Curvature.R")
source("bin/Tools/Analysis/T_End_Morphology.R")
source("bin/Tools/Analysis/T_Fiber_Area.R")
source("bin/Tools/Analysis/T_KMT_Minus_End_Seeds.R")

#############
# Read data #
#############

## Extract individual KMTs
source("bin/Packages/Pre_Analysis/Get_Single_KMTs_From_Labels.R")

## Sort point in each KMT. Always the first point will be a point on a kinetochore
source("bin/Packages/Pre_Analysis/Sort_KMTs_by_Kinetochore_Position.R")

## Core function to formats PoleX_YY DF: [1] Segment ID, [2] length, [3] minus_dist_to_pole, [4] plus_dist_to_pole
## Length distribution LD: [1] length
source("bin/Packages/Pre_Analysis/Analyse_Length_Distiribution.R")

## Calculate relative position of each point on the kinetochore - centriole (Pole1/Pole2) axis
source("bin/Packages/Pre_Analysis/P_Relative_position.R")

############
# Analysis #
############

## DF output of the functions:
## 0 - All below
## 1 - KMTs_at_the_Pole: [1] No. of KMTs
##   - KMTs_at_the_Pole_and_length: [1] No. of KMTs, [2] KMTs length, [3] Minus end dist., [4] Plus end dist., [5] Minus end relative position
## 2 - Inter_Kinetochore_Distance: [1] Inter-kinetochore distance
##   - Inter_Kinetochore_Distance_KMTs_no: [1] Inter-kinetochore distance, [2] KMTs no.
##   - Inter_Kinetochore_Distance_KMTs_delta: [1] Inter-kinetochore distance, [2] Delta of KMTs
## 3 - KMTs_total_Curvature: [1] Curvature, [2] k0fiber no.
## 4 - Plus/minus_end_morphology_Pole1/2: [1] Fiber, [2 - 3] EndType, [4] EndType Different (if exist), [5] Relative position

if(analysis == 0){
  source("bin/Packages/Analysis/Analyse_No_of_KMTs_Reaching_the_Pole.R")
  source("bin/Packages/Analysis/No_of_KMTs_at_a_kinetochore.R")
  source("bin/Packages/Analysis/P_Inter_Kinetochore_Dist.R")
  source("bin/Packages/Analysis/P_KMT_Curvature.R")
  source("bin/Packages/Analysis/P_End_Morphology.R")
  source("bin/Packages/Analysis/P_Fiber_Area.R")
  source("bin/Packages/Analysis/P_KMT_Minus_End_Seeds.R")
  
} else if (analysis == "1"){
  source("bin/Packages/Analysis/Analyse_No_of_KMTs_Reaching_the_Pole.R")
  source("bin/Packages/Analysis/No_of_KMTs_at_a_kinetochore.R")
           
} else if (analysis == "2"){
  source("bin/Packages/Analysis/P_Inter_Kinetochore_Dist.R")
  
} else if (analysis == "3"){
  source("bin/Packages/Analysis/P_KMT_Curvature.R")
  
} else if (analysis == "4"){
  source("bin/Packages/Analysis/P_End_Morphology.R")
  
} else if (analysis == "5"){
  source("bin/Packages/Analysis/P_Fiber_Area.R")
  
} else if (analysis == "6"){
  source("bin/Packages/Analysis/....R")
  
} else if (analysis == "7"){
  source("bin/Packages/Analysis/....R")
  
}
