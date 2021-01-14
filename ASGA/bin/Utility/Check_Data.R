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
Check_Data <- function(i) {
  tryCatch(
    {
      Test_Segments <<- colnames(get(paste("Data", "Segments", i, sep = "_")))[1] == "Segment ID" &&
        colnames(get(paste("Data", "Segments", i, sep = "_")))[ncol(get(paste("Data", "Segments", i, sep = "_")))] == "Point IDs" &&
        colnames(get(paste("Data", "Segments", i, sep = "_")))[ncol(get(paste("Data", "Segments", i, sep = "_"))) - 3] == "length"
    },
    error = function(e) {}
  )

  if (!exists("Test_Segments")) {
    Test_Segments <<- FALSE
  }

  tryCatch(
    {
      Test_Pole1 <<- colnames(get(paste("Data", "Nodes", i, sep = "_")) %>% select(Pole1)) == "Pole1"
    },
    error = function(e) {}
  )

  if (!exists("Test_Pole1")) {
    Test_Pole1 <<- FALSE
  }

  tryCatch(
    {
      Test_Pole2 <<- colnames(get(paste("Data", "Nodes", i, sep = "_")) %>% select(Pole2)) == "Pole2"
    },
    error = function(e) {}
  )

  if (!exists("Test_Pole2")) {
    Test_Pole2 <<- FALSE
  }

  tryCatch(
    {
      if (Test_Pole1 == FALSE && Test_Pole2 == FALSE) {
        Poles <- peakfinder(get(paste("Data", "Nodes", i, sep = "_")))
        Pole1 <<- Poles[1, ]
        Pole2 <<- Poles[2, ]
        rm(Poles)
        Test_Pole1 <<- TRUE
        Test_Pole2 <<- TRUE
      }
    },
    error = function(e) {}
  )

  tryCatch(
    {
      Test_Pole1_00 <<- which(colnames(get(paste("Data", "Segments", i, sep = "_"))) == "Pole1_00") <
        which(colnames(get(paste("Data", "Segments", i, sep = "_"))) == "Pole2_00")
    },
    error = function(e) {
      Test_Pole1_00 <<- tibble()
    }
  )
  if (length(Test_Pole1_00) == 0) {
    Test_Pole1_00 <<- FALSE
  }

  # State value of data check  --------------------------------------------------
  if (Test_Segments == TRUE && Test_Pole1 == TRUE && Test_Pole2 == TRUE && Test_Pole1_00 == TRUE) {
    DataTest <<- 1
  } else if (Test_Segments == TRUE && Test_Pole1_00 == FALSE) {
    DataTest <<- 2
  } else if (Test_Segments == FALSE && Test_Pole1 == TRUE && Test_Pole2 == TRUE) {
    DataTest <<- 3
  } else if (Test_Segments == TRUE && Test_Pole1 == FALSE) {
    DataTest <<- 4
  } else if (Test_Segments == TRUE && Test_Pole2 == FALSE) {
    DataTest <<- 5
  } else if (Test_Segments == TRUE && Test_Pole1 == FALSE && Test_Pole2 == FALSE) {
    DataTest <<- 6
  } else if (Test_Segments == FALSE && Test_Pole1 == FALSE && Test_Pole2 == FALSE) {
    DataTest <<- 7
  } else if (!exists(paste("Data", "Segments", i, sep = "_"))) {
    DataTest <<- 0
  }
}

Check_Analysis <- function(i) {
  tryCatch(
    {
      AnalysisTest <<- as.list(str_split(i, paste("Data_", "[:digit:]", "_", sep = ""))[[1]])
      if (length(AnalysisTest) == 2 &&
        AnalysisTest[1] == "") {
        AnalysisTest <<- 1
      } else {
        AnalysisTest <<- 0
      }
    },
    error = function(e) {
      AnalysisTest <<- 0
    }
  )

  if (!exists("AnalysisTest")) {
    AnalysisTest <<- 0
  }
}
