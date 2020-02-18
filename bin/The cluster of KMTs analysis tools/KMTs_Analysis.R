#####################################
# The cluster of KMTs analysis tools #
#####################################

#######################################
# Library (load & install if missing) #
#######################################

Required_Packages = c("readxl",
                      "tidyverse",
                      "plyr",
                      "ggplot2",
                      "xlsx",
                      "tcltk",
                      "svDialogs")

for (p in Required_Packages) {
  if (!require(p, 
               character.only = TRUE))
    install.packages(p, 
                     dependencies = TRUE)
  library(p,
          character.only = TRUE)
}
rm(Required_Packages, 
   p)

############
# Settings #
############

Data <- dlg_open(title = "Select data file", 
                 filters = dlg_filters[c("xlsx", "All")])$res

Segments <- read_excel(Data,
                       sheet = "Segments")
Nodes <- read_excel(Data,
                    sheet = "Nodes")
Points <- read_excel(Data,
                     sheet = "Points")

Poles <- dlg_message("Are the Poles labeled in the Node sheet as 'Pole1' and 'Pole2'?", 
                     "yesno")$res
if (Poles == "yes") {
  Pole1 <- "Pole1" ## Name of the label for the Pole1 in the Node section
  Pole2 <- "Pole2" ## Name of the label for the Pole2 in the Node section
} else {
  Pole1 <- dlg_input("What is a label for the Pole_1?", 
                     "Pole1")$res
  Pole2 <- dlg_input("What is a label for the Pole_2?", 
                     "Pole2")$res
}

Output <- "Output/"

Minus_Threshold <- 1

#############
# Functions #
#############

source("Functions/Sort_by_Fiber.r")
source("Functions/Select_Points.r")
source("Functions/Find_XYZ.r")
source("Functions/Kinetochore_Position.r")
source("Functions/Sort_All_Points_to_Start_From_the_Kinetochore.r")

###########################
# !Check files structure! #
###########################

Test_Segments <- colnames(Segments)[1] == "Segment ID" && colnames(Segments)[ncol(Segments)] == "Point IDs" && colnames(Segments)[ncol(Segments) - 3] == "length"
Test_Poles <- colnames(Nodes %>% select(Pole1)) == "Pole1" && colnames(Nodes %>% select(Pole2)) == "Pole2"

if (Test_Poles && Test_Segments == TRUE) {
  msg_box("The data structure looks great! Press Ok to analyze your awesome data!")
} else if (Test_Segments == FALSE && Test_Poles == TRUE) {
  quit(msg_box("The Segments data structure looks strange! Please check it with the guidelines and try again."))
} else if (Test_Poles == FALSE && Test_Segments == TRUE) {
  msg_box("Could not find 'Poles' coordinates in the Nodes excel sheet! Please check it with the guidelines and try again.")
  dlg_message("Are the 'Pole's' coordinates embedded in the raw data in the Node sheet?",
              "yesno")$res
  
  if (res == "Yes") {
    Pole1 <- dlg_input("Is this correct label name for the 'Pole_1'?", 
                      "Pole1")$res
    Pole2 <- dlg_input("Is this correct label name for the 'Pole_2'?", 
                       "Pole2")$res
  } else if (res == "No") {
    quit(msg_box("Please add 'Pole_1' and 'Pole_2' coordinate in the Nodes sheet and try again."))
  }
}
rm(Test_Segments, 
   Test_Poles, 
   Poles)

################
# Analyse Data #
################

## Load data from the file and split for Segments, Points, and Nodes
source("Functions/Load_Data.r")

## Extract individual KMTs
source("Analysis/Get_Single_KMTs_From_Labels.r")

## Sort point in each KMT. Always the first point will be a point on a kinetochore
source("Analysis/Sort_KMTs_by_Kinetochore_Position.r")

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