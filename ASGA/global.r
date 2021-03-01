################################################################################
# Shiny Global
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-20
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################
# Title of the app -------------------------------------------------------------
source("bin/Utility/Library.R")

APP_TITLE <- "ASGA v0.34"
CC <- paste("© Copyright GPL V3.0 2019-",
  str_split(Sys.Date(), pattern = "-")[[1]][1],
  ", Robert Kiewisz",
  sep = ""
)

# Global CSS  ------------------------------------------------------------------
includeCSS("www/css/style.css")

JS_CODE <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"

# Global HTML  -----------------------------------------------------------------
source("www/Home/index.R")
source("www/Get_Started/index.R")

# Global Utility  --------------------------------------------------------------
source("bin/Utility/Check_Data.R")
source("bin/Utility/Upload_Data.R")
source("bin/Utility/Setting_Buttons.R")
source("bin/Utility/Load_Data.R")
source("bin/Utility/Load_Amira.R")
source("bin/Utility/Save_Data.R")
source("bin/Utility/Save_Amira.R")
source("bin/Utility/Error_Messages.R")

# Global server settings  ------------------------------------------------------
options(shiny.maxRequestSize = 1024 * 1024^2)
options(shiny.host = "127.0.0.1")
options(shiny.port = 7878)

# Global constant settings  ----------------------------------------------------
SHINY_IO <<- TRUE # Constant defining if app is running locally or online
DATA_TEST <<- 0
NUM_FILES <<- 0
MINUS_THRESHOLD <<- 1.68 # Minus end distance to the pole [um]
MINUS_DISTANCE <<- 0.035 # Minus end distance of any MT to the KMT [um]
MT_POINT_CONFIG <<- 0.035 # Distance of any MT to the MT [um]
FIBER_AREA_CONFIG <<- 24 # 500 nm bin length
CURVATURE_CONFIG <<- 24 # 500 nm bin length

# Global Functions  ------------------------------------------------------------
# Pre_Analysis
source("bin/Tools/Pre_Analysis/Sort_by_Fiber.R")
source("bin/Tools/Pre_Analysis/Select_Points.R")
source("bin/Tools/Pre_Analysis/Find_XYZ.R")
source("bin/Tools/Pre_Analysis/Kinetochore_Position.R")
source("bin/Tools/Pre_Analysis/Sort_All_Points_to_Start_From_the_Kinetochore.R")
source("bin/Tools/Pre_Analysis/T_Relative_Position.R")

# Analysis for KMTs
source("bin/Tools/Analysis/KMTs/Length_Distiribution.R")
source("bin/Tools/Analysis/KMTs/No_of_KMTs_connected_to_the_Pole.R")
source("bin/Tools/Analysis/KMTs/No_of_KMTs.R")
source("bin/Tools/Analysis/KMTs/T_Inter_Kinetochore_Dist.R")
source("bin/Tools/Analysis/KMTs/T_KMT_Curvature.R")
source("bin/Tools/Analysis/KMTs/T_End_Morphology.R")
source("bin/Tools/Analysis/KMTs/T_Fiber_Area.R")
source("bin/Tools/Analysis/KMTs/T_KMT_Minus_End_Seeds.R")
source("bin/Tools/Analysis/KMTs/T_Fiber_Length_Curvature.R")
source("bin/Tools/Analysis/KMTs/T_Kinetochore_Area.R")
source("bin/Tools/Analysis/KMTs/T_KMT_Torque.R")

# Analysis for KMTs
source("bin/Tools/Analysis/SMTs/T_Bridging_MT.R")
source("bin/Tools/Analysis/SMTs/T_SMTs_Minus_Ends.R")
# Output report plots
source("bin/Utility/Report.R")
# Statistic analysis tools
source("bin/Tools/Analysis/Stat/FWHM.R")

# Global Packages  -------------------------------------------------------------
source("bin/Packages/Analysis/Pre_Analysis.R")
source("bin/Packages/Analysis/A_KMT_number.R")
source("bin/Packages/Analysis/A_IKD.R")
source("bin/Packages/Analysis/A_Curvature.R")
source("bin/Packages/Analysis/A_End_Morphology.R")
source("bin/Packages/Analysis/A_Fiber_Area.R")
source("bin/Packages/Analysis/A_KMT_Minus_End_Seeds.R")
source("bin/Packages/Analysis/A_Fiber_Length_&_curvature.R")
source("bin/Packages/Analysis/A_Kinetochore_Area.R")
source("bin/Packages/Analysis/A_KMT_Torque.R")
source("bin/Packages/Analysis/A_MT_Bridging.R")

# Test Unit --------------------------------------------------------------------
source("tests/Test_Output.R")
source("tests/Tests.R")
