# Declare packages
packages <- c("shiny", "shinyjs", "shinycssloaders", "shinyWidgets", "shinyBS", "shinyalert", "colourpicker", "stringr",
              "plyr", "tidyverse", "egg", "base", "alphashape3d","readxl","xlsx", "zip",
              "foreach", "doParallel", "Hmisc", "parallel", "iterators",
              "ggbeeswarm", "base",  "ggplot2",
              "rlang", "Rcpp", "Formula","survival", "lattice", "rgl", "geometry", "gridExtra",  "forcats", "dplyr", "purrr",
              "readr", "tidyr", "tibble", "stringr", "stats", "graphics", "grDevices", "utils", "datasets", "methods", "R6")

# Loop through each package
for (x in packages) {

  if(!is.element(x, installed.packages(lib.loc = "./R-Portable-Win/library/")[,1]))
    {install.packages(x, repos='http://cran.us.r-project.org', lib = "./R-Portable-Win/library/", dependencies = TRUE)
  } else {print(paste(x, " library already installed"))}
}
