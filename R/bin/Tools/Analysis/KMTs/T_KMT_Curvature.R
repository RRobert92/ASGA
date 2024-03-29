#####################################################################################
# Tool KMT_Curvature
#
# The analysis tool to calculate total and local curvature of KMTs
#
# Count total curvature ration for each KMT.
# Ration is determined by dividing total KMT length and length
# of KMT between (+) and (-) end
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-04-17
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
#####################################################################################

# Function: Calculate total curvature ----------------------------------------------
total_curvature <- function(x) {
    curvarture <- data.frame()
    for (i in 1:nrow(get(paste(colnames(Segments)[x])))) {

        ## Virtuosity
        KMT <- get(paste(colnames(Segments)[x], i, sep = "_"))

        curv <- sqrt((KMT[1, 2] - KMT[nrow(KMT), 2])^2 +
                             (KMT[1, 3] - KMT[nrow(KMT), 3])^2 +
                             (KMT[1, 4] - KMT[nrow(KMT), 4])^2)

        total_length <- get(paste(colnames(Segments)[x]))[i, 2]

        ## Menger curvature
        Spline <- as.matrix(get(paste(colnames(Segments)[x], i, sep = "_"))[, 2:4])

        P0 <- Spline[as.numeric(round(nrow(Spline) / 2, 0)), 1:3]
        P1 <- Spline[1, 1:3]
        P2 <- Spline[nrow(Spline), 1:3]

        b <- sqrt((P2[1] - P1[1])^2 +
                          (P2[2] - P1[2])^2 +
                          (P2[3] - P1[3])^2)
        h <- abs((P2[1] - P1[1]) *
                         (P1[2] - P0[2]) *
                         (P1[3] - P0[3]) - (P1[1] - P0[1]) *
                (P2[2] - P1[2]) *
                (P2[3] - P1[3])) / b
        sideLength01 <- sqrt((P0[1] - P1[1])^2 +
                                     (P0[2] - P1[2])^2 +
                                     (P0[3] - P1[3])^2)
        sideLength12 <- b
        sideLength20 <- sqrt((P0[1] - P2[1])^2 +
                                     (P0[2] - P2[2])^2 +
                                     (P0[3] - P2[3])^2)
        triangleArea <- (h * b) / 2
        Menger_Curvature <- 4 * triangleArea / (sideLength01 * sideLength12 * sideLength20)

        ## Output

        curvarture[i, 1] <- total_length / curv
        curvarture[i, 2] <- x
        curvarture[i, 3] <- get(paste(colnames(Segments)[x]))[i, 2]
        curvarture[i, 4] <- get(paste(colnames(Segments)[x]))[i, 4]
        curvarture[i, 5] <- get(paste(colnames(Segments)[x]))[i, 5]
        curvarture[i, 6] <- get(paste(colnames(Segments)[x]))[i, 6]
        curvarture[i, 7] <- Menger_Curvature
        curvarture[i, 8] <- get(paste(colnames(Segments)[x]))[i, 1]

        names(curvarture)[1] <- "Curvature"
        names(curvarture)[2] <- "k-fiber no."
        names(curvarture)[3] <- "KMTs length"
        names(curvarture)[4] <- "(+) end position"
        names(curvarture)[5] <- "(+) Dist-to-Pole"
        names(curvarture)[6] <- "Elipse Position"
        names(curvarture)[7] <- "Menger_Curvature"
        names(curvarture)[8] <- "Segment_ID"
    }

    curvarture
}

# Function: Calculate local curvature -----------------------------------------------
# Count a distance between first point and first point + 25 (1+24) for data with 20nm step, curvature is count every 500 nm
# Count the curvature ratio for each step
local_curvature <- function(x) {
    full_data <- data.frame()
    for (i in 1:nrow(get(paste(colnames(Segments)[x])))) {
        KMT <- get(paste(colnames(Segments)[x], i, sep = "_"))

        # Get curve length ------------------------------------------------------------------
        output_curve <- data.frame(Curve = as.numeric())
        j <- 1

        while (j < nrow(KMT)) {
            output_curve[j, 1] <- sqrt((KMT[j, 2] - KMT[j + CURVATURE_CONFIG, 2])^2 +
                                               (KMT[j, 3] - KMT[j + CURVATURE_CONFIG, 3])^2 +
                                               (KMT[j, 4] - KMT[j + CURVATURE_CONFIG, 4])^2)

            j <- j + CURVATURE_CONFIG
        }
        output_curve <- na.omit(output_curve)

        # Get full length -------------------------------------------------------------------
        output_full <- data.frame(Full_L = as.numeric())
        j <- 1

        while (j < nrow(KMT)) {
            local_c <- data.frame()
            for (k in j:as.numeric(j + CURVATURE_CONFIG)) {
                local_c[k, 1] <- sqrt((KMT[k, 2] - KMT[k + 1, 2])^2 +
                                              (KMT[k, 3] - KMT[k + 1, 3])^2 +
                                              (KMT[k, 4] - KMT[k + 1, 4])^2)
            }
            local_c <- local_c[j:nrow(local_c), 1]

            output_full[j,] <- sum(local_c[1:CURVATURE_CONFIG])

            j <- j + CURVATURE_CONFIG
        }
        output_full <- na.omit(output_full)

        # get mean relative position --------------------------------------------------------
        output_mean <- data.frame(Mean_Position = as.numeric())
        j <- 1

        while (j < nrow(KMT)) {
            output_mean[j,] <- (KMT[j, 5] + KMT[j + CURVATURE_CONFIG, 5]) / 2

            j <- j + CURVATURE_CONFIG
        }
        output_mean <- na.omit(output_mean)

        if (nrow(output_curve) == 0) {

        } else {
            DF <- cbind.data.frame(
                    Curvature = output_full$Full_L / output_curve$Curve,
                    Relative_Position = output_mean$Mean_Position,
                    K_fiber_no = x,
                    KMT_no = i,
                    End_Position = get(paste(colnames(Segments)[x]))[i, 4],
                    End_to_Pole = get(paste(colnames(Segments)[x]))[i, 5],
                    Elipse_Position = get(paste(colnames(Segments)[x]))[i, 6]
            )
            full_data <- rbind(
                    full_data,
                    DF
            )
        }
    }

    full_data
}
