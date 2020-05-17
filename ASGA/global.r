################################################################################
# Shiny Global
#
# Author: Robert Kiewisz
# Created: 2020-05-16
################################################################################


# Global CSS  ------------------------------------------------------------------
includeCSS("www/css/style.css")

# Global HTML  ------------------------------------------------------------------
source("www/Home/index.R")
source("www/Get_Started/index.R")

# Global Utility  ---------------------------------------------------------------
source("bin/Utility/Library.R")
source("bin/Utility/Check_Data.R")
source("bin/Utility/Upload_Data.R")
source("bin/Utility/Setting_Buttons.R")
source("bin/Utility/Pre_Analysis.R")

# Global Settings  --------------------------------------------------------------
options(shiny.maxRequestSize = 1024*1024^2)

DataTest <<- 0

# Global Functions  -------------------------------------------------------------
source("bin/Tools/Pre_Analysis/Sort_by_Fiber.R")
source("bin/Tools/Pre_Analysis/Select_Points.R")
source("bin/Tools/Pre_Analysis/Find_XYZ.R")
source("bin/Tools/Pre_Analysis/Kinetochore_Position.R")
source("bin/Tools/Pre_Analysis/Sort_All_Points_to_Start_From_the_Kinetochore.R")
source("bin/Tools/Pre_Analysis/T_Relative_Position.R")