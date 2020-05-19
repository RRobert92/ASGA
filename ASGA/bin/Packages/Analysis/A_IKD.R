################################################################################
# Packages IKD
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-19
################################################################################


# Set-up analysis --------------------------------------------------------------
A_IKD <- function (input, output, session){
  total <- 3
  
  progressSweetAlert(
    session = session, id = "P_IKD",
    title = "CalculatingInter-Kinetochore Distance...",
    display_pct = TRUE, value = 0
  )
  
  Inter_Kinetochore_Distance <<- Inter_Kinetochore_Dist()
  i=1
  updateProgressBar(
    session = session,
    id = "P_IKD",
    value = round((i - 1) / total * 100,
                  0)
  )
  Sys.sleep(0.1)
  
  Inter_Kinetochore_Distance_KMTs_no <<- Compare_KMTs_no_for_sister()
  i=2
  updateProgressBar(
    session = session,
    id = "P_IKD",
    value = round((i - 1) / total * 100,
                  0)
  )
  Sys.sleep(0.1)
  
  Inter_Kinetochore_Distance_KMTs_delta <<- Compare_KMTs_delta_for_sister()
  i=3
  updateProgressBar(
    session = session,
    id = "P_IKD",
    value = round((i - 1) / total * 100,
                  0)
  )
  Sys.sleep(0.1)
  closeSweetAlert(session = session)
}