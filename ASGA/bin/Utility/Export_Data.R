################################################################################
# Module Export_Data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-20
# Reviewed: Robert Kiewisz 19/07/2020
################################################################################

Export_Data <- function (input, output, session){
  # Set Data dir -----------------------------------------------------------------
  setwd("Data/")
  
  # Zip all file and cleand dir --------------------------------------------------
  Zip_Files <- list.files(path = getwd(), pattern = ".xlsx$")
  zip::zipr(zipfile = "ASGA_Data.zip", files = Zip_Files)
  
  file.remove(Zip_Files)
  # Back to main dir -------------------------------------------------------------
  setwd("../")
}