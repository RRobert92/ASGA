################################################################################################
# Tool Fiber_torque
#
# Set of functions to define fiber torque along the spindle pole axis
#
# !IMPOTENT! this tool is relaying on the Fiber_Area function and have to be run in
# conjunction with them. Advised to use it after final calculation of fiber area/density
# but before relative_pos_1_fiber.
# !Running this tool separately will required running polygon calculation!
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-08-05
################################################################################################

KMTs_torque_in_fiber <- function(x) {
    # Poygon center x,y,z
    Centers <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[, c("X_Coord", "Z_Coord")]
    angle <- tibble()

    for (j in seq(1,
                  ncol(select(get(paste(colnames(Segments)[x], "fiber", sep = "_")), starts_with("V"))))) {
        Positions <- select(
                get(paste(colnames(Segments)[x], "fiber", sep = "_")),
                starts_with("V")
        )

        Positions <- Positions[j]

        Pos_Coord <- Points[purrr::flatten_dbl(Positions + 1),
                            c("X Coord", "Z Coord")]

        angle_df <- tibble()
        for (i in seq(1,
                      nrow(Pos_Coord))) {
            Pos <- as.numeric(Pos_Coord[i, c("X Coord", "Z Coord")])
            #Pos_2 <- as.numeric(Pos_Coord[i + 1, c("X Coord", "Z Coord")])
            Center_Pos <- as.numeric(Centers[i,])

            angle_df[i, 1] <- atan2(Pos[2] - Center_Pos[2], Pos[1] - Center_Pos[1]) * (180 / pi)
        }

        # Zero out from first entry
        angle_df <- angle_df[, 1] - as.numeric(angle_df[1, 1])
        angle[1:nrow(Centers), j] <- angle_df
        names(angle)[j] <- paste0("V", j)
    }

    angle_df <- tibble(
            fiber = character(),
            mean = double(),
            STD = double()
    )
    for (i in seq(1, nrow(angle))) {
        angle_df[i, 2] <- mean(as.numeric(angle[i,]), na.rm = TRUE)
        angle_df[i, 3] <- sd(as.numeric(angle[i,]), na.rm = TRUE)
    }
    angle_df[, 1] <- colnames(Segments)[x]
    return(angle_df)
}

Fiber_torque_around_center <- function(x) {
    # Poygon center x,y,z
    Poses <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[, c("X_Coord", "Z_Coord")]
    Center <- as.numeric(Pole1[, c("X.Coord", "Z.Coord")])
    angle <- tibble()

    for (j in seq(1, nrow(Poses))) {
        Pos <- as.numeric(Poses[j,])
        angle[j, 1] <- atan2(Pos[2] - Center[2], Pos[1] - Center[1]) * (180 / pi)
    }

    # Zero out from first entry
    angle <- angle[, 1] - as.numeric(angle[1, 1])
    names(angle) <- "angle"
    angle[, 2] <- colnames(Segments)[x]

    return(angle)
}