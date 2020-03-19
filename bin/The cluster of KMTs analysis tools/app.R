#######################################
# Library (load & install if missing) #
#######################################

source("Utility/Core/Library.R")

################################
# Defin no. of data to analyse #
################################

No_of_Data <- dlg_input("How much data you want to analyzed and compared.",
                          1)$res
No_of_Data <- as.numeric(No_of_Data)
for(n in 1:No_of_Data){
  rm(list = setdiff(ls(), list("Data_label", "No_of_Data")))
  
  source("Utility/Analysis/KMTs_Analysis.R")
}
rm(list=setdiff(ls(), list("Data_label", "No_of_Data")))

##########################
# Load all analysed Data #
##########################
Data_label <- as.data.frame(str_split(Data_label, "_"))
Data_label <- as.character(Data_label[1,1])

## Load data from the soucre location Output/...
source("Utility/Analysis/Load_Analysis.R")

## Prepare data depending from specied need: bin all for P1 & P2 or analysed P1 & P2 separetly 
source("Utility/Analysis/..R")

##############
# Plots Data #
##############
source("Tools/Plots/PL_Length_Distirbution.R")
source("Tools/Plots/PL_Inter-Kinetochore_Distance.R")
source("Tools/Plots/PL_KMTs_at_the_Pole.R")
source("Tools/Plots/PL_(-)_End_Distribution.R")
source("Tools/Plots/PL_Total_Curvature.R")
source("Tools/Plots/PL_Local_Curvature.R")

