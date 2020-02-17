## remove "," and spread numbers in single cells
## x number of the KMTs in the fiber
## y is number of a column contain information about KTMs
Select_Points <- function(x, y) {
  selected_points <- data.frame(str_split(gsub("[^[:digit:]]", ",", y[x, 2]), pattern = ","))
  points <- data.frame(Point_ID = selected_points[, 1])
}