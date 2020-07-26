################################################################################
# Shiny Global
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-20
################################################################################

# Title of the app -------------------------------------------------------------
apptitle <- "ASGA v0.31.1"

# Global CSS  ------------------------------------------------------------------
includeCSS("www/css/style.css")

# Global HTML  -----------------------------------------------------------------
source("www/Home/index.R")
source("www/Get_Started/index.R")

# Global Utility  --------------------------------------------------------------
source("bin/Utility/Library.R")
source("bin/Utility/Check_Data.R")
source("bin/Utility/Upload_Data.R")
source("bin/Utility/Setting_Buttons.R")
source("bin/Utility/Pre_Analysis.R")
source("bin/Utility/Load_Data.R")
source("bin/Utility/Save_Data.R")

# Global Settings  -------------------------------------------------------------
options(shiny.maxRequestSize = 1024*1024^2)

DataTest <<- 0
numfiles <<- 0
Minus_Threshold <<- 1.68 # Minus end distance to the pole [um]
minus_distance <<- 0.035 # Minus end distance of any MT to the KMT [um]

# Global Functions  ------------------------------------------------------------
source("bin/Tools/Pre_Analysis/Sort_by_Fiber.R")
source("bin/Tools/Pre_Analysis/Select_Points.R")
source("bin/Tools/Pre_Analysis/Find_XYZ.R")
source("bin/Tools/Pre_Analysis/Kinetochore_Position.R")
source("bin/Tools/Pre_Analysis/Sort_All_Points_to_Start_From_the_Kinetochore.R")
source("bin/Tools/Pre_Analysis/T_Relative_Position.R")

source("bin/Tools/Analysis/Length_Distiribution.R")
source("bin/Tools/Analysis/No_of_KMTs_connected_to_the_Pole.R")
source("bin/Tools/Analysis/No_of_KMTs.R")
source("bin/Tools/Analysis/T_Inter_Kinetochore_Dist.R")
source("bin/Tools/Analysis/T_KMT_Curvature.R")
source("bin/Tools/Analysis/T_End_Morphology.R")
source("bin/Tools/Analysis/T_Fiber_Area.R")
source("bin/Tools/Analysis/T_KMT_Minus_End_Seeds.R")
source("bin/Tools/Analysis/T_Fiber_Length_Curvature.R")
source("bin/Tools/Analysis/T_Kinetochore_Area.R")
source("bin/Tools/Analysis/T_KMT_Torque.R")

source("bin/Utility/Report.R")

# Global Packages  -------------------------------------------------------------
source("bin/Packages/Analysis/A_KMT_number.R")
source("bin/Packages/Analysis/A_IKD.R")
source("bin/Packages/Analysis/A_Curvature.R")
source("bin/Packages/Analysis/A_End_Morphology.R")
source("bin/Packages/Analysis/A_Fiber_Area.R")
source("bin/Packages/Analysis/A_KMT_Minus_End_Seeds.R")
source("bin/Packages/Analysis/A_Fiber_Length_&_curvature.R")
source("bin/Packages/Analysis/A_Kinetochore_Area.R")
source("bin/Packages/Analysis/A_KMT_Torque.R")
