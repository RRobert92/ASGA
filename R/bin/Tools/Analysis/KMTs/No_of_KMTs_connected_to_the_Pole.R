#####################################################################################
# Tool No_of_KMTs_connected_to_the_POle
#
# The analysis tool count no. of KMTs with a (-) end at the pole
#
# x variables in the function is a number of a column contain KMTs information
# Count how many KMTs with a minus end distance of "Minus_Threshold" is in the fiber
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-04-17
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
#####################################################################################

# Tool Include  only no of KMTs at a pole -------------------------------------------
KMTs_to_the_Pole <- function(x) {
    No_of_KMTs <- data.frame()
    DF <- get(colnames(Segments)[x])[with(get(colnames(Segments)[x]), minus_dist_to_pole <= MINUS_THRESHOLD & minus_dist_to_pole > 0),]

    if (nrow(DF) == 0) {
        No_of_KMTs <- 0
    } else {
        No_of_KMTs <- nrow(DF)
    }

    No_of_KMTs
}

# Tool Include length, minus end distance and minus end distance --------------------
KMTs_to_the_Pole_vs_length <- function(x) {
    No_of_KMTs <- data.frame()
    DF <- get(colnames(Segments)[x])[with(get(colnames(Segments)[x]), minus_dist_to_pole <= MINUS_THRESHOLD & minus_dist_to_pole > 0),]

    if (nrow(DF) == 0) {
        DF[1,] <- NA

        No_of_KMTs <- data.frame(
                c(nrow(DF)),
                c(DF[2]),
                c(DF[3]),
                c(DF[4]),
                c(DF[5]),
                c(DF[7])
        )
    } else {
        No_of_KMTs <- data.frame(
                c(nrow(DF)),
                c(DF[2]),
                c(DF[3]),
                c(DF[4]),
                c(DF[5]),
                c(DF[7])
        )
    }

    No_of_KMTs
}
