################################################################################
# Module Test_Result
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-08-29
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# Run analysis -----------------------------------------------------------------
Run_Test <- function(input, output, session) {
  withProgress(message = "Analyzing:", value = 1, {
    incProgress(1, detail = "Test in progress...")
    Sys.sleep(0.1)

    Test_value <<- tibble()
    current_data <<- 1

    tryCatch(
      {
        callModule(Pre_Analysis, "Home")
        Test_value[1, 1] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 1] <<- FALSE
      }
    )
    names(Test_value)[1] <<- "Pre_Analsis"

    tryCatch(
      {
        callModule(A_KMT_number, "Home")
        Test_value[1, 2] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 2] <<- FALSE
      }
    )
    names(Test_value)[2] <<- "KMT_number"

    tryCatch(
      {
        callModule(A_IKD, "Home")
        Test_value[1, 3] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 3] <<- FALSE
      }
    )
    names(Test_value)[3] <<- "IKD"

    tryCatch(
      {
        callModule(A_Curvature, "Home")
        Test_value[1, 4] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 4] <<- FALSE
      }
    )
    names(Test_value)[4] <<- "Curvature"

    tryCatch(
      {
        callModule(A_End_Morphology, "Home")
        Test_value[1, 5] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 5] <<- FALSE
      }
    )
    names(Test_value)[5] <<- "End_Morphology"

    tryCatch(
      {
        callModule(A_KMT_Torque, "Home")
        Test_value[1, 6] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 6] <<- FALSE
      }
    )
    names(Test_value)[6] <<- "KMT_Torque"

    tryCatch(
      {
        callModule(A_Fiber_Area, "Home")
        Test_value[1, 7] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 7] <<- FALSE
      }
    )
    names(Test_value)[7] <<- "Fiber_area"

    tryCatch(
      {
        callModule(A_Fiber_Length_Curv, "Home")
        Test_value[1, 8] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 8] <<- FALSE
      }
    )
    names(Test_value)[8] <<- "Fiber_L&C"

    tryCatch(
      {
        callModule(A_KMT_Minus_End_Seeds, "Home")
        Test_value[1, 9] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 9] <<- FALSE
      }
    )
    names(Test_value)[9] <<- "Nucleation"

    tryCatch(
      {
        callModule(A_K_Core_Area, "Home")
        Test_value[1, 10] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 10] <<- FALSE
      }
    )
    names(Test_value)[10] <<- "K_Core_Area"
    
    tryCatch(
      {
        callModule(Save_Data, "Home")
        Test_value[1, 11] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 11] <<- FALSE
      }
    )
    names(Test_value)[11] <<- "Save_data"
    File_name <<- as_tibble(ls(pattern = "Data_", envir = .GlobalEnv))
    File_name <<- na.omit(File_name)
  })
}

# Test analysis results --------------------------------------------------------
Test_Test <- function(input, output, session) {
  Test_df <<- data.frame()
  
  Test_df[1, 1] <<- nrow(File_name) == 43
  Test_df[1, 2] <<- ncol(Test_value) == length(Test_value[Test_value == TRUE])

  if (ncol(Test_df) == length((Test_df[Test_df == TRUE]))) {
    Test_df <<- TRUE
    Bad_funtions <<- 0
  } else {
    Test_df <<- FALSE
    Bad_funtions <<- which(Test_value[1, ] == FALSE, TRUE)
  }
}

# Show results  ----------------------------------------------------------------
Test_Result <- function(input, output, session) {
  if (Test_df == TRUE) {
    confirmSweetAlert(
      session,
      inputId = "Test_data_output",
      title = "Test result",
      text = paste("The test result was positive. All work as intended.", " Error no. ", as.character(Bad_funtions), sep = ""),
      type = "success",
      btn_labels = "Confirm",
      btn_colors = "#a5dc86"
    )
  } else {
    confirmSweetAlert(
      session,
      inputId = "Test_data_output",
      title = "Test result",
      text = paste("One of the test went wrong, check more infor in RStudio.
      Test results are included in `Test_value` data.frame.", "Error no. ", as.character(Bad_funtions[, 2]), sep = ""),
      type = "warning",
      btn_labels = "Confirm",
      btn_colors = "#f8bb86"
    )
  }
}
