##################################################################
# The analysis tool count no. of KMTs with a (-) end at the pole #
##################################################################
## x variablein in the function is a number of a column contain KMTs information (e.g. 1, 2, 3...., etc.)
## Count how many KMTs with a minus end distance of "Minus_Threshold" is in the fiber

## Include  only no of KMTs at a pole
KMTs_to_the_Pole <- function(x){
  No_of_KMTs <- data.frame()
  DF <- get(colnames(Segments)[x])[with(get(colnames(Segments)[x]), minus_dist_to_pole <= Minus_Threshold & minus_dist_to_pole > 0),]
  if (nrow(DF) == 0){
    No_of_KMTs <- 0
  } else {
    No_of_KMTs <- nrow(DF)
  }
  No_of_KMTs
}

## Include length, minus end distance and minus end distance
KMTs_to_the_Pole_vs_length <- function(x){
  No_of_KMTs <- data.frame()
  DF <- get(colnames(Segments)[x])[with(get(colnames(Segments)[x]), minus_dist_to_pole <= Minus_Threshold & minus_dist_to_pole > 0),]
  if (nrow(DF) == 0){
    No_of_KMTs <- 0
  } else {
    No_of_KMTs <- data.frame(c(nrow(DF)),
                             c(DF[2]),
                             c(DF[3]),
                             c(DF[4]))
  }
  No_of_KMTs
}