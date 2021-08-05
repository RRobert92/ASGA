source("Library_List.R")
# Loop through each package
for (x in packages) {

    if (!is.element(x, installed.packages(lib.loc = "./R-Portable-Win/library/")[, 1])) {
        install.packages(x,
                         repos = 'http://cran.us.r-project.org',
                         lib = "./R-Portable-Win/library/",
                         dependencies = TRUE,
                         type = "binary")
    } else {
        print(paste(x, " library already installed"))
    }
}
