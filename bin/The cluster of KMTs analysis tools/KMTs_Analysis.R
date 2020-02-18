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

#############
# Read data #
#############

## Extract individual KMTs
source("Packages/Get_Single_KMTs_From_Labels.r")

## Sort point in each KMT. Always the first point will be a point on a kinetochore
source("Packages/Sort_KMTs_by_Kinetochore_Position.r")

###########
#Analysis #
###########

res <- dlg_message("Do you want to run full analysis?",
            "yesno")$res
if(res == "yes"){
  analysis <- "all"
} else {
  analysis <- dlg_input("What analysis to run?\n Pick one from list:\n - LD")$res
}
rm(res)

if(analysis == "all"){
  source("Analysis/...")
  source("Analysis/...")
  source("Analysis/...")
} else if (analysis == "LD"){
  source("Analysis/...")
}

##############
# Plots Data #
##############