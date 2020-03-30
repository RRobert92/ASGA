###################################################################
# The core tool to search for all MTs that belonged to each label #
###################################################################
  ## x variablein in the function is a number of a column contain KMTs information

Sort_by_fiber <- function(x) {
  fiber <- Segments %>% filter_at(vars(starts_with(x)), any_vars(. >= 1))
  fiber %>% select(1, which(colnames(Segments) == "Point IDs"), which(colnames(Segments) == "length")) 
}