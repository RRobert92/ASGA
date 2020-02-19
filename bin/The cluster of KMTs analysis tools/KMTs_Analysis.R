#####################################
# The cluster of KMTs analysis tools #
#####################################

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

source("Tools/Core/Sort_by_Fiber.r")
source("Tools/Core/Select_Points.r")
source("Tools/Core/Find_XYZ.r")
source("Tools/Core/Kinetochore_Position.r")
source("Tools/Core/Sort_All_Points_to_Start_From_the_Kinetochore.r")
source("Tools/Analysis/Length_Distiribution.R")
source("Tools/Analysis/No_of_KMTs_connected_to_the_Pole.R")

#############
# Read data #
#############

## Extract individual KMTs
source("Packages/Core/Get_Single_KMTs_From_Labels.r")

## Sort point in each KMT. Always the first point will be a point on a kinetochore
source("Packages/Core/Sort_KMTs_by_Kinetochore_Position.r")

############
# Analysis #
############

res <- dlg_message("Do you want to run full analysis?",
            "yesno")$res
if(res == "yes"){
  analysis <- 0
} else {
  analysis <- dlg_input("What analysis to run?
  Pick one from list: 
                        1 - No of KTMs at the pole
                        2 - Inter-kinetochore distance
                        3 - No. of KMTs per kinetochore
                        4 - Curvature of KMTs
                        5 - k-fiber area (polygon)
                        6 - Length of a k-fiber
                        7 - Curvature of k-fiber")$res
}
rm(res)

## Core function to formated PoleX_YY DF: [1] Segment ID, [2] length, [3] minus_dist_to_pole, [4] plus_dist_to_pole
source("Packages/Analysis/Analyse_Length_Distiribution.R")

## DF output of the functions:
## 0 - All below
## 1 - KMTs_at_the_Pole: [1] No. of KMTs
##   - KTMs_at_the_Pole_and_length: [1] No. of KMTs, [2] KMTs length, [3] Minus end dist., [4] Plus end dist.
## 2 - Inter_kinetochore_distance: [1]Inter_distance, [2] KMTs no., [3] Delta

if(analysis == 0){
  source("Packages/Analysis/Analyse_No_of_KMTs_Reaching_the_Pole.R")
  source("Packages/Analysis/....R")
  
} else if (analysis == "1"){
  source("Packages/Analysis/Analyse_No_of_KMTs_Reaching_the_Pole.R")
  
} else if (analysis == "2"){
  source("Packages/Analysis/....R")
  
} else if (analysis == "3"){
  source("Packages/Analysis/....R")
  
} else if (analysis == "4"){
  source("Packages/Analysis/....R")
  
} else if (analysis == "5"){
  source("Packages/Analysis/....R")
  
} else if (analysis == "6"){
  source("Packages/Analysis/....R")
  
} else if (analysis == "7"){
  source("Packages/Analysis/....R")
  
}

##############
# Plots Data #
##############
