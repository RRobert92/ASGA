# Declare packages
packages <- c("shiny", "shinyjs", "shinycssloaders", "shinyWidgets", "shinyBS", "shinyalert", "colourpicker", "stringr",
              "plyr", "tidyverse", "egg", "base", "alphashape3d","readxl","xlsx", "zip",
              "foreach", "doParallel", "Hmisc", "rlang", "Rcpp")

# Loop through each package
for (x in packages) {
  
  if(!is.element(x, installed.packages(lib.loc = "./R-Portable-Win/library/")[,1]))
    {install.packages(x, repos='http://cran.us.r-project.org', lib = "./R-Portable-Win/library/")
  } else {print(paste(x, " library already installed"))} 
}