###############################################################
# The core tool to remove ',' and assigned points to each MTs #
###############################################################
  ## x variablein in the function is a number of the MTs in the fiber (e.g. 8, 10 etc.)
  ## y variablein in the function is a number of a column contain MTs information (e.g. 2, 3, 4 etc.)

Select_Points <- function(x, y) {
  selected_points <- data.frame(str_split(gsub("[^[:digit:]]", ",", y[x, 2]), pattern = ","))
  points <- data.frame(Point_ID = selected_points[, 1])
}