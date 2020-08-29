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

# Test analysis results -------------------------------------------------------
Test_Test <- function(input, output, session) {
  Test_df <- data_frame()
  Test_df[1, 1] <- nrow(File_name) == 43
  if (Test_df[1, 1] == 1) {
    Test_df <- TRUE
  }
}

# Show results  --------------------------------------------------------------
Test_Result <- function(input, output, session) {
  confirmSweetAlert(
    session,
    inputId = "Test_data_output",
    title = "Test result",
    text = "The test result was positive. All work as intended.",
    type = "success",
    btn_labels = c("Confirm"),
    btn_colors = NULL
  )
}