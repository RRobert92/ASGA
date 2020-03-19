Required_Packages = c("readxl",
                      "tidyverse",
                      "plyr",
                      "ggplot2",
                      "xlsx",
                      "tcltk",
                      "svDialogs")

for (p in Required_Packages) {
  if (!require(p, 
               character.only = TRUE))
    install.packages(p, 
                     dependencies = TRUE)
  library(p,
          character.only = TRUE)
}
rm(Required_Packages, 
   p)