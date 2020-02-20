###################################
# No. of KMTs at eahc kinetochore #
###################################
## x variable is a number of a column contain KMTs information (e.g. 1, 2, 3...., etc.)
## Count how many KMTs exist for each label

No_of_KMTs <- function(x){
  nrow(get(colnames(Segments)[x])) 
}
