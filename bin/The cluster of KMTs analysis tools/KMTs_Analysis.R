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
                        1 - No of KTMs at the Pole")$res
}
rm(res)

source("Packages/Analysis/Analyse_Length_Distiribution.R")

if(analysis == 0){
  source("Packages/Analysis/Analyse_No_of_KMTs_Reaching_the_Pole.R")
  source("Packages/Analysis/....R")
  
} else if (analysis == "1"){
  source("Packages/Analysis/Analyse_No_of_KMTs_Reaching_the_Pole.R")
  
} else if (analysis == "2"){
  source("Packages/Analysis/....R")
  
} else if (analysis == ""){
  source("Packages/Analysis/....R")
  
}

##############
# Plots Data #
##############
