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
# Created: 2020-05-29
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################################

# pick a point i and i + 1
# median as a zero, recalculate position of a points
#
KMTs_torque_in_fiber <- function(x) {
  angle1 <- data.frame()
  for (j in 1:ncol(select(
    get(paste(colnames(Segments)[x], "fiber", sep = "_")),
    starts_with("V")
  ))) {
    tryCatch(
      {
        angle <- data.frame()
        for (i in 2:as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
          df1 <- data.frame(
            get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, "X_Coord"],
            get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, "Z_Coord"]
          )
          df2 <- data.frame(
            get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i + 1, "X_Coord"],
            get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i + 1, "Z_Coord"]
          )
          DF1 <- select(
            get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, ],
            starts_with("V")
          )
          DF1 <- DF1[j]
          DF2 <- select(
            get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i + 1, ],
            starts_with("V")
          )
          DF2 <- DF2[j]

          DF1_Coord <- data.frame()
          DF1_Coord[1, 1] <- Points[as.numeric(DF1[1, 1] + 1), "X Coord"]
          DF1_Coord[1, 2] <- Points[as.numeric(DF1[1, 1] + 1), "Z Coord"]

          DF2_Coord <- data.frame()
          DF2_Coord[1, 1] <- Points[as.numeric(DF2[1, 1] + 1), "X Coord"]
          DF2_Coord[1, 2] <- Points[as.numeric(DF2[1, 1] + 1), "Z Coord"]


          tryCatch(
            {
              rad1 <- atan2(DF1_Coord[1, 2], DF1_Coord[1, 1])
              rad2 <- atan2(DF2_Coord[1, 2], DF2_Coord[1, 1])
              angle[i, 1] <- rad1 - rad2
            },
            error = function(e) {}
          )
        }
        angle <- sum(na.omit(angle))
        angle1[j, 1] <- angle * (180 / pi)
      },
      error = function(e) {}
    )
  }
  angle1 <- median(as.matrix(angle1))
  angle1
}
