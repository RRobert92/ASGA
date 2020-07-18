################################################################################
# Packages IKD
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-19
# Debugged/Reviewed: Robert Kiewisz 18/07/2020
################################################################################


# Set-up analysis --------------------------------------------------------------
A_IKD <- function (input, output, session){
  
  total <- 3
  
  progressSweetAlert(
    session = session, id = "P_IKD",
    title = "Calculating Inter-Kinetochore Distances...",
    display_pct = TRUE, value = 0
  )
  
  assign("Inter_Kinetochore_Distance",
         Inter_Kinetochore_Dist(),
         envir=.GlobalEnv)
  
  i=1
  updateProgressBar(
    session = session,
    id = "P_IKD",
    value = round(i / total * 100,
                  0)
  )
  Sys.sleep(0.1)
  
  assign("Inter_Kinetochore_Distance_KMTs_no",
         Compare_KMTs_no_for_sister(),
         envir=.GlobalEnv)
  
  i=2
  updateProgressBar(
    session = session,
    id = "P_IKD",
    value = round(i / total * 100,
                  0)
  )
  Sys.sleep(0.1)
  
  assign("Inter_Kinetochore_Distance_KMTs_delta",
         Compare_KMTs_delta_for_sister(),
         envir=.GlobalEnv)
  
  i=3
  updateProgressBar(
    session = session,
    id = "P_IKD",
    value = round(i / total * 100,
                  0)
  )
  Sys.sleep(0.1)
  closeSweetAlert(session = session)
}