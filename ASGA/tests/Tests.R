################################################################################
# Module Test_Test
#
# Test analysis results
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-02-15
# Reviewed:
################################################################################

Test_Test <- function(input, output, session) {
  Test_df <<- data.frame()
  PRE_ANALYSIS_TEST <<- c(
    "Segment ID", "length", "minus_dist_to_pole", "plus_dist_to_kinetochore_core",
    "plus_dist_to_pole", "Elipse_Position", "Fiber_Name", "Relative_plus_position",
    "Relative_minus_position", "Leading"
  )
  EXPECTED_NCOL <<- 10
  EXPECTED_NROW <<- 6
  
  # Generate random number to test randomly selected fiber
  Random_Generator <- function() {
    RANDOM_NUMBER <- round(runif(1, which(colnames(Segments) == "Pole1_00"), as.numeric(ncol(Segments) - 4)), 0)
    RANDOM_TABLE <- round(runif(1, 1, 10), 0)

    return(tibble(RANDOM_NUMBER, RANDOM_TABLE))
  }

  # Standardized test to test if the output fit the requirements 
  Test_anaysis <- function(TESTED_TOOL, P1_P2 = TRUE, EXP_ncol = 6, EXP_nrow = c(24, 12, 12)) {
    Test_Analysis_Output <<- tibble()
    
    if (P1_P2 == TRUE) {
      Test_Analysis_Output[1, 1] <<- exists(TESTED_TOOL) && 
        exists(paste(TESTED_TOOL, "_P1", sep = "")) && 
        exists(paste(TESTED_TOOL, "_P2", sep = ""))

      Test_Analysis_Output[1, 2] <<- ncol(get(TESTED_TOOL)) == EXP_ncol[1] &&
        ncol(get(paste(TESTED_TOOL, "_P1", sep = ""))) == EXP_ncol[1] &&
        ncol(get(paste(TESTED_TOOL, "_P2", sep = ""))) == EXP_ncol[1]

      Test_Analysis_Output[1, 3] <<- nrow(get(TESTED_TOOL)) == EXP_nrow[1] &&
        nrow(get(paste(TESTED_TOOL, "_P1", sep = ""))) == EXP_nrow[2] &&
        nrow(get(paste(TESTED_TOOL, "_P2", sep = ""))) == EXP_nrow[3]
    } else {
      Test_Analysis_Output[1, 1] <<- exists(TESTED_TOOL)

      Test_Analysis_Output[1, 2] <<- ncol(get(TESTED_TOOL)) == EXP_ncol

      Test_Analysis_Output[1, 3] <<- nrow(get(TESTED_TOOL)) == EXP_nrow[1]
    }

    if (ncol(Test_Analysis_Output) == length((Test_Analysis_Output[Test_Analysis_Output == TRUE]))) {
      Test_Analysis_Output <<- TRUE
    } else {
      Test_Analysis_Output <<- FALSE
    }

    return(Test_Analysis_Output)
  }

  RANDOM_NUMBER <<- as.numeric(Random_Generator()[1])
  RANDOM_TABLE <<- as.numeric(Random_Generator()[2])


  Test_df[1, 1] <<- nrow(File_name) == 44 # Check if all data are properly saved and exported

  Test_df[1, 2] <<- ncol(Test_value) == length(Test_value[Test_value == TRUE]) # check if all analysis passed

  Test_df[1, 3] <<- table(startsWith(ls(envir = .GlobalEnv), "Pole"))["TRUE"] == 39 # Check if k-fiber were identified correctly

  Test_df[1, 4] <<- exists(colnames(Segments)[RANDOM_NUMBER]) &&
    ncol(get(colnames(Segments)[RANDOM_NUMBER])) == EXPECTED_NCOL &&
    nrow(get(colnames(Segments)[RANDOM_NUMBER])) == EXPECTED_NROW &&
    colnames(get(colnames(Segments)[RANDOM_NUMBER]))[RANDOM_TABLE] == PRE_ANALYSIS_TEST[RANDOM_TABLE]
  
  Test_df[1, 5] <<- Test_anaysis("Data_1_SMT_Ends", FALSE, 3, 20)
  Test_df[1, 6] <<- Test_anaysis("Data_1_LD", TRUE, 6, c(24, 12, 12))
  Test_df[1, 7] <<- Test_anaysis("Data_1_KMT_No", TRUE, 1, c(4,2,2))
  Test_df[1, 8] <<- Test_anaysis("Data_1_KMT_Pole", TRUE, 1, c(4,2,2))
  Test_df[1, 9] <<- Test_anaysis("Data_1_IKD", FALSE, 1, 2)
  Test_df[1, 10] <<- Test_anaysis("Data_1_KMT_Total_Curv", TRUE, 8, c(24,12,12))
  Test_df[1, 11] <<- Test_anaysis("Data_1_KMT_Local_Curv", TRUE, 7, c(190,122,68))
  Test_df[1, 12] <<- Test_anaysis("Data_1_Fiber_Torque", TRUE, 1, c(4,2,2))
  Test_df[1, 13] <<- Test_anaysis("Data_1_Fiber_Area", TRUE, 4, c(40,23,17))
  Test_df[1, 14] <<- Test_anaysis("Data_1_Fiber_Length", TRUE, 1, c(3,1,2))
  Test_df[1, 15] <<- Test_anaysis("Data_1_Fiber_Total_Curv", TRUE, 2, c(3,1,2))
  Test_df[1, 16] <<- Test_anaysis("Data_1_Fiber_Local_Curv", TRUE, 6, c(40,18,22))
  Test_df[1, 17] <<- Test_anaysis("Data_1_K_Core_Area", TRUE, 5, c(4,2,2))

  if (ncol(Test_df) == length((Test_df[Test_df == TRUE]))) {
    Test_df <<- TRUE
    Bad_funtions <<- 0
  } else {
    Test_df <<- FALSE
    Bad_funtions <<- which(Test_value[1, ] == FALSE, TRUE)
  }
}
