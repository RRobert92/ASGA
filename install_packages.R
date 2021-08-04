# Declare packages
packages <- c("shiny", "shinyjs", "shinycssloaders", "shinyWidgets", "shinyBS", "shinyalert", "colourpicker", "stringr",
              "plyr", "tidyverse", "egg", "base", "alphashape3d","readxl","xlsx", "zip",
              "foreach", "doParallel", "Hmisc", "parallel", "iterators",
              "ggbeeswarm",  "ggplot2", "beeswarm",
              "rlang", "Rcpp", "Formula","survival", "webshot", "lattice", "rgl", "geometry", "gridExtra",  "forcats", "dplyr", "purrr",
              "readr", "tidyr", "tibble", "stringr", "stats", "graphics", "grDevices", "utils", "datasets", "methods", "R6",
			        'jsonlite', 'xtable', 'digest', 'sourcetools', 'htmltools', 'crayon', 'later', "cachem", "htmlwidgets",
			        "miniUI", "stringi", "broom", "pillar", "dbplyr", "gtable", "modelr", "cellranger", "reprex", "rvest", "magic",
			        "crosstalk", "manipulateWidget", "vipor", "latticeExtra", "htmlTable", "bslib", "magrittr", "vctrs", "utf8",
			        "pkgconfig", "tidyselect", "backports", "DBI", "scales", "haven", "abind", "rJava", "xlsxjars", "base64enc",
			        "png", "jpeg", "checkmate", "sass", "jquerylib")

# Loop through each package
for (x in packages) {

  if(!is.element(x, installed.packages(lib.loc = "./R-Portable-Win/library/")[,1]))
    {install.packages(x, repos='http://cran.us.r-project.org', lib = "./R-Portable-Win/library/", dependencies = TRUE, type = "binary")
  } else {print(paste(x, " library already installed"))}
}
