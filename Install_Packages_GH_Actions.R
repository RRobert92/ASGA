source("Library_List.R")
# Loop through each package
for (x in packages) {

    if (!is.element(x, installed.packages()[, 1])) {
        install.packages(x,
                         repos = 'http://cran.us.r-project.org',
                         dependencies = TRUE)
    } else {
        print(paste(x, " library already installed"))
    }
}
