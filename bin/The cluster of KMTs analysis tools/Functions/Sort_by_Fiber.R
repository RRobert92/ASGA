## x is a number of a column contain KMTs information
Sort_by_fiber <- function(x) {
  fiber <- Segments %>% filter_at(vars(starts_with(x)), any_vars(. >= 1))
  fiber %>% select(1, which(colnames(Segments) == "Point IDs"))
}