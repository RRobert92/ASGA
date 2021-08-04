################################################################################
# Packages End_Morphology
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# Set-up analysis --------------------------------------------------------------
A_End_Morphology <- function(input, output, session) {
  if (ncol(Nodes %>% select(starts_with("EndType"))) == 2) {
    End_type_error <<- End_Type_Error()
  }

  if (ncol(Nodes %>% select(starts_with("EndType"))) >= 1) {

    # Analyze end morphology for Pole1 ---------------------------------------------
    total <- as.numeric(length(which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)

    Plus_end_morphology_Pole1 <<- End_distribution_Plus(which(colnames(Segments) == "Pole1_00"), 1)
    Minus_end_morphology_Pole1 <<- End_distribution_Minus(which(colnames(Segments) == "Pole1_00"), 1)

    progressSweetAlert(
      session = session,
      id = "P_End_Morphology1",
      title = "Calcualting (+) & (-) end morphology for the Pole_1...",
      display_pct = TRUE,
      value = 0
    )

    for (i in as.numeric(which(colnames(Segments) == "Pole1_00") + 1):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
      tryCatch(
      {
        DF <- data.frame()

        assign(
          "DF",
          End_distribution_Plus(i, 1)
        )

        assign(
          "Plus_end_morphology_Pole1",
          rbind(Plus_end_morphology_Pole1, DF)
        )

        assign(
          "DF",
          End_distribution_Minus(i, 1)
        )

        assign(
          "Minus_end_morphology_Pole1",
          rbind(Minus_end_morphology_Pole1, DF)
        )
      },
        error = function(e) { }
      )

      updateProgressBar(
        session = session,
        id = "P_End_Morphology1",
        value = round((i - 1) / total * 100, 0)
      )
      Sys.sleep(0.1)
    }

    closeSweetAlert(session = session)


    # Bin data for the Pole1 --------------------------------------------------------
    if (ncol(Nodes %>% select(starts_with("EndType"))) == 2) {
      Plus_end_morphology_Pole1 <- Plus_end_morphology_Pole1 %>% select(
        "Fiber",
        starts_with("EndType"),
        "Entype_Different",
        "Relative_plus_position",
        "Node_ID"
      )
      for (i in 2:as.numeric(ncol(Nodes %>% select(starts_with("EndType"))) + 1)) {
        names(Plus_end_morphology_Pole1)[i] <- paste0("EndType_", as.numeric(i - 1), sep = "")
      }

      Minus_end_morphology_Pole1 <- Minus_end_morphology_Pole1 %>% select(
        "Fiber",
        starts_with("EndType"),
        "Entype_Different",
        "Relative_minus_position",
        "Node_ID",
        "Minus_end_Distance"
      )
      for (i in 2:as.numeric(ncol(Nodes %>% select(starts_with("EndType"))) + 1)) {
        names(Minus_end_morphology_Pole1)[i] <- paste0("EndType_", as.numeric(i - 1), sep = "")
      }
    } else {
      Plus_end_morphology_Pole1 <- Plus_end_morphology_Pole1 %>% select(
        "Fiber",
        starts_with("EndType"),
        "Relative_plus_position",
        "Node_ID"
      )
      names(Plus_end_morphology_Pole1)[2] <- "EndType_1"
      Minus_end_morphology_Pole1 <- Minus_end_morphology_Pole1 %>% select(
        "Fiber",
        starts_with("EndType"),
        "Relative_minus_position",
        "Node_ID",
        "Minus_end_Distance"
      )
      names(Minus_end_morphology_Pole1)[2] <- "EndType_1"
    }

    Plus_end_morphology_Pole1 <<- Plus_end_morphology_Pole1
    Minus_end_morphology_Pole1 <<- Minus_end_morphology_Pole1

    # Analyze end morphology for Pole2 ---------------------------------------------
    total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) -
      as.numeric(which(colnames(Segments) == "Pole2_00") - 1)

    tryCatch(
    {
      Plus_end_morphology_Pole2 <<- End_distribution_Plus(which(colnames(Segments) == "Pole2_00"), 2)
      Minus_end_morphology_Pole2 <<- End_distribution_Minus(which(colnames(Segments) == "Pole2_00"), 2)
    },
      error = function(e) { }
    )

    progressSweetAlert(
      session = session,
      id = "P_End_Morphology2",
      title = "Calcualting (+) & (-) end morphology for the Pole_2...",
      display_pct = TRUE,
      value = 0
    )

    for (i in as.numeric(which(colnames(Segments) == "Pole2_00") + 1):as.numeric(ncol(Segments) - 4)) {
      tryCatch(
      {
        DF <- data.frame()

        assign(
          "DF",
          End_distribution_Plus(i, 2)
        )

        assign(
          "Plus_end_morphology_Pole2",
          rbind(Plus_end_morphology_Pole2, DF)
        )

        assign(
          "DF",
          End_distribution_Minus(i, 2)
        )

        assign(
          "Minus_end_morphology_Pole2",
          rbind(Minus_end_morphology_Pole2, DF)
        )
      },
        error = function(e) { }
      )

      updateProgressBar(
        session = session,
        id = "P_End_Morphology2",
        title = "Calcualting (+) & (-) end morphology for all other MTs...",
        value = round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100, 0)
      )
      Sys.sleep(0.1)
    }

    updateProgressBar(
      session = session,
      id = "P_End_Morphology2",
      value = 100
    )
    Sys.sleep(0.1)

    tryCatch({
      assign("MT_Ends_Type",
             MT_Ends_Distribution(),
             envir = .GlobalEnv)
    })
    closeSweetAlert(session = session)
    # Bin data for the Pole2 --------------------------------------------------------
    tryCatch(
    {
      if (ncol(Nodes %>% select(starts_with("EndType"))) >= 2) {
        Plus_end_morphology_Pole2 <- Plus_end_morphology_Pole2 %>% select(
          "Fiber",
          starts_with("EndType"),
          "Entype_Different",
          "Relative_plus_position",
          "Node_ID"
        )
        for (i in 2:as.numeric(ncol(Nodes %>% select(starts_with("EndType"))) + 1)) {
          names(Plus_end_morphology_Pole2)[i] <- paste0("EndType_", as.numeric(i - 1), sep = "")
        }

        Minus_end_morphology_Pole2 <- Minus_end_morphology_Pole2 %>% select(
          "Fiber",
          starts_with("EndType"),
          "Entype_Different",
          "Relative_minus_position",
          "Node_ID",
          "Minus_end_Distance"
        )
        for (i in 2:as.numeric(ncol(Nodes %>% select(starts_with("EndType"))) + 1)) {
          names(Minus_end_morphology_Pole2)[i] <- paste0("EndType_", as.numeric(i - 1), sep = "")
        }
      } else if (ncol(Nodes %>% select(starts_with("EndType"))) == 1) {
        Plus_end_morphology_Pole2 <- Plus_end_morphology_Pole2 %>% select(
          "Fiber",
          starts_with("EndType"),
          "Relative_plus_position",
          "Node_ID"
        )
        names(Plus_end_morphology_Pole2)[2] <- "EndType_1"
        Minus_end_morphology_Pole2 <- Minus_end_morphology_Pole2 %>% select(
          "Fiber",
          starts_with("EndType"),
          "Relative_minus_position",
          "Node_ID",
          "Minus_end_Distance"
        )
        names(Minus_end_morphology_Pole2)[2] <- "EndType_1"
      }
    },
      error = function(e) { }
    )

    tryCatch(
    {
      Plus_end_morphology_Pole2 <<- Plus_end_morphology_Pole2
      Minus_end_morphology_Pole2 <<- Minus_end_morphology_Pole2
    },
      error = function(e) { }
    )
  }
}
