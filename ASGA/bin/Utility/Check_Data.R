################################################################################
# Module Check_Data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-17
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# Check Data  ------------------------------------------------------------------
CheckData <- function(i) {
  tryCatch(
    {
      Test_Segments <- colnames(get(paste("Data", "Segments", i, sep = "_")))[1] == "Segment ID" &&
        colnames(get(paste("Data", "Segments", i, sep = "_")))[ncol(get(paste("Data", "Segments", i, sep = "_")))] == "Point IDs" &&
        colnames(get(paste("Data", "Segments", i, sep = "_")))[ncol(get(paste("Data", "Segments", i, sep = "_"))) - 3] == "length"
    },
    error = function(e) {}
  )

  if (!exists("Test_Segments")) {
    Test_Segments <- FALSE
  } else {}

  tryCatch(
    {
      Test_Pole1 <- colnames(get(paste("Data", "Nodes", i, sep = "_")) %>% select(Pole1)) == "Pole1"
    },
    error = function(e) {}
  )


  if (!exists("Test_Pole1")) {
    Test_Pole1 <- FALSE
  } else {}

  tryCatch(
    {
      Test_Pole2 <- colnames(get(paste("Data", "Nodes", i, sep = "_")) %>% select(Pole2)) == "Pole2"
    },
    error = function(e) {}
  )

  if (!exists("Test_Pole2")) {
    Test_Pole2 <- FALSE
  } else {}

  # State value of data check  --------------------------------------------------
  if (TestSegments == TRUE && TestPole1 == TRUE && TestPole2 == TRUE) {
    DataTest <<- 1
  } else if (TestSegments == TRUE && which(colnames(get(paste("Data", "Segments", i, sep = "_"))) == "Pole1_00") >
    which(colnames(get(paste("Data", "Segments", i, sep = "_"))) == "Pole2_00")) {
    DataTest <<- 2
  } else if (TestSegments == FALSE && TestPole1 == TRUE && TestPole2 == TRUE) {
    DataTest <<- 3
  } else if (TestSegments == TRUE && TestPole1 == FALSE) {
    DataTest <<- 4
  } else if (TestSegments == TRUE && TestPole2 == FALSE) {
    DataTest <<- 5
  } else if (TestSegments == TRUE && TestPole1 == TRUE && TestPole2 == TRUE) {
    DataTest <<- 6
  } else if (TestSegments == TRUE && TestPole1 == TRUE && TestPole2 == TRUE) {
    DataTest <<- 7
  } else if (!exists(get(paste("Data", "Segments", i, sep = "_")))) {
    DataTest <<- 0
  }
}
