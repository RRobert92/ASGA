#####################################################################################
# Tool No_of_KMTs
#
# The analysis tool to count no. of KMTs at each kinetochore
#
# x variable is a number of a column contain KMTs information (e.g. 1, 2, 3...., etc.)
# Count how many KMTs exist for each label
#
# Author: Robert Kiewisz
# Created: 2020-04-17
#####################################################################################


# Tool  -----------------------------------------------------------------------------
No_of_KMTs <- function(x){
  nrow(get(colnames(Segments)[x])) 
}
