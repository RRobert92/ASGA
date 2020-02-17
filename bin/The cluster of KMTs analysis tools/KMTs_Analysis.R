#####################################
# The cluster of KMTs analysis tools #
#####################################

#######################################
# Library (load & install if missing) #
#######################################
Required_Packages=c("readxl", 
                    "tidyverse", 
                    "plyr", 
                    "ggplot2", 
                    "xlsx", 
                    "tcltk", 
                    "svDialogs");

for(p in Required_Packages){
  if(!require(p,character.only = TRUE))
    install.packages(p, dependencies = TRUE)
  library(p,character.only = TRUE)
}
rm(Required_Packages, p)

############
# Settings #
############
Data <- dlg_open(title = "Select data file", filters = dlg_filters[c("xlsx", "All")])$res

Segments <- read_excel(Data,
                       sheet = "Segments")
Nodes <- read_excel(Data,
                    sheet = "Nodes")
Points <- read_excel(Data,
                    sheet = "Points")

Poles <- dlg_message("Are the Poles labeled in the Node sheet as Pole1 and Pole2?", "yesno")$res
if (Poles == "yes"){
  Pole1 <- "Pole1" ## Name of the label for the Pole1 in the Node section
  Pole2 <- "Pole2" ## Name of the label for the Pole2 in the Node section
} else {
  Pole1 <- dlg_input("What is a label for the Pole_1?", "Pole1")$res
  Pole2 <- dlg_input("What is a label for the Pole_2?", "Pole2")$res
}

Output <- "Output/"

Minus_Threshold <- 1

#############
# Functions #
#############
## Function for sorting dataset
source("Functions/Sort_by_Fiber.r")
source("Functions/Select_Points.r")
source("Functions/Find_XYZ.r")
source("Functions/Kinetochore_Position.r")
###########################
# !Check files structure! #
###########################
Test_Segments <- colnames(Segments)[1] == "Segment ID" && colnames(Segments)[ncol(Segments)] == "Point IDs" && colnames(Segments)[ncol(Segments) - 3] == "length"
Test_Poles <- colnames(Nodes %>% select(Pole1)) == "Pole1" && colnames(Nodes %>% select(Pole2)) == "Pole2"
if(Test_Poles && Test_Segments == TRUE){
  msg_box("The data structure looks good! Press Ok to analyze your awesome data!")
} else if (Test_Segments == TRUE){
  quit(msg_box("The Segments data structure looks strange! Please check it with the guidelines and try again.")) 
} else if (Test_Poles == TRUE){
  quit(msg_box("Could not find Poles coordinates in Nodes excel sheet! Please check it with the guidelines and try again.")) 
}
rm(Test_Segments, Test_Poles, Poles)
################
# Analyse Data #
################
## Load data from the file and split for Segments, Points, and Nodes
source("Functions/Load_Data.r")

## Extract individual KMTs
total <- as.numeric(ncol(Segments %>% select(starts_with("Pole"))))
pb <- tkProgressBar(title = "Finding KMTs for each fiber...",
                    min = 0,
                    max =  total,
                    width = 300)
colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]
for (i in which(colnames(Segments) == "Pole1_00") : which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))])) {
  assign(colnames(Segments)[i], 
         Sort_by_fiber(colnames(Segments)[i]))
  
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"), 
           Select_Points(j, get(colnames(Segments)[i])))
    j = j + 1
  }
  
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"),
           Find_XYZ(get(paste(colnames(Segments)[i], j, sep = "_"))))
    j = j + 1
  }
  Sys.sleep(0.1)
  setTkProgressBar(pb, i, 
                   label = paste(round(i / total * 100, 0), "% Done"))
}
close(pb)

assign("Kinetochore_projected", 
       Kinetochore_position())

##############
# Plots Data #
##############
