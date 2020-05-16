################################################################################
# Function Find_XYZ
#
# The core tool to assigned MT points with XYZ coordinates
#
# x variables in the function is a DF name with points assigned to the MT
#(e.g. Pole1_01_1)
#
# Author: Robert Kiewisz
# Created: 2020-05-16 
################################################################################


# Pick single KMT  -------------------------------------------------------------
Find_XYZ <- function(x) {
  points_MT <- x[1]
  names(points_MT)[1] <- "Point ID"
  
  joined_data <- join_all(list(points_MT, 
                               Points),
                          by = "Point ID")
  mutate_all(joined_data, 
             function(y) as.numeric(as.character(y)))
}
