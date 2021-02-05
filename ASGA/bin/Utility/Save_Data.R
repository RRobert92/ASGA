################################################################################
# Module Save_Data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

Save_Data <- function(input, output, session) {
  # Save Data for LA -----------------------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "LD_P1", sep = "_"),
        LD_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "LD_P2", sep = "_"),
        LD_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "LD", sep = "_"),
        rbind(LD_P1, LD_P2),
        envir = .GlobalEnv
      )
      write.xlsx(
        get(paste("Data", current_data, "LD_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_LD_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "LD_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_LD_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "LD", sep = "_")),
        paste("Data/", "Data_", current_data, "_LD.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for KMT No -------------------------------------------------------
  tryCatch(
    {
      names(No_of_KMTs_at_kinetochore_P1)[1] <- "KMTs_per_kinetochore"
      names(No_of_KMTs_at_kinetochore_P2)[1] <- "KMTs_per_kinetochore"

      assign(paste("Data", current_data, "KMT_No_P1", sep = "_"),
        No_of_KMTs_at_kinetochore_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMT_No_P2", sep = "_"),
        No_of_KMTs_at_kinetochore_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMT_No", sep = "_"),
        rbind(No_of_KMTs_at_kinetochore_P1, No_of_KMTs_at_kinetochore_P2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "KMT_No_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_No_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "KMT_No_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_No_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "KMT_No", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_No.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )


  # Save Data for KMT at the Pole ----------------------------------------------
  tryCatch(
    {
      names(KMTs_at_the_Pole1)[1] <- "KMTs_at_the_Pole"
      names(KMTs_at_the_Pole2)[1] <- "KMTs_at_the_Pole"

      assign(paste("Data", current_data, "KMT_Pole_P1", sep = "_"),
        KMTs_at_the_Pole1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMT_Pole_P2", sep = "_"),
        KMTs_at_the_Pole2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMT_Pole", sep = "_"),
        rbind(KMTs_at_the_Pole1, KMTs_at_the_Pole2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "KMT_Pole_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_Pole_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "KMT_Pole_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_Pole_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "KMT_Pole", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_Pole.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for KMT at the Pole ----------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "KMT_Minus_Ends", sep = "_"),
        Minus_end_position,
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "KMT_Minus_Ends", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_Minus_Ends.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for end morphology Pole ------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "Minus_end_morphology_Pole1", sep = "_"),
        Minus_end_morphology_Pole1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Plus_end_morphology_Pole1", sep = "_"),
        Plus_end_morphology_Pole1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Minus_end_morphology_Pole2", sep = "_"),
        Minus_end_morphology_Pole2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Plus_end_morphology_Pole2", sep = "_"),
        Plus_end_morphology_Pole2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Plus_end_morphology", sep = "_"),
        rbind(Plus_end_morphology_Pole1, Plus_end_morphology_Pole2),
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Minus_end_morphology", sep = "_"),
        rbind(Minus_end_morphology_Pole1, Minus_end_morphology_Pole2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "Minus_end_morphology_Pole1", sep = "_")),
        paste("Data/", "Data_", current_data, "_Minus_end_morphology_Pole1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Plus_end_morphology_Pole1", sep = "_")),
        paste("Data/", "Data_", current_data, "_Plus_end_morphology_Pole1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Minus_end_morphology_Pole2", sep = "_")),
        paste("Data/", "Data_", current_data, "_Minus_end_morphology_Pole2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Plus_end_morphology_Pole2", sep = "_")),
        paste("Data/", "Data_", current_data, "_Plus_end_morphology_Pole2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Plus_end_morphology", sep = "_")),
        paste("Data/", "Data_", current_data, "_Plus_end_morphology.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Minus_end_morphology", sep = "_")),
        paste("Data/", "Data_", current_data, "_Minus_end_morphology.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Amira file for end morphology -----------------------------------------
  tryCatch(
    {
      # Prepare data for saving in Amira ASCI file
      if (Amira == TRUE &&
        ncol(Nodes %>% select(starts_with("EndType"))) >= 1) {
        End_morpho <- rbind(
          tibble(X1 = Data_1_Minus_end_morphology$Node_ID, X2 = 0),
          tibble(X1 = Data_1_Plus_end_morphology$Node_ID, X2 = 1)
        )
        End_morpho <- End_morpho[order(End_morpho$X1), ]
        Poles <- Nodes[2] %>%
          rowid_to_column() %>%
          filter(`X Coord` == as.numeric(Pole1$X.Coord))
        if (length(Poles) != 2) {
          break
        }

        if (Poles[which.min(Poles$rowid), 1] == 0) {
          End_morpho <- rbind(
            tibble(
              X1 = Poles[which.min(Poles$rowid), 1] - 1,
              X2 = -1
            ),
            End_morpho[Poles[which.min(Poles$rowid), 1]:as.numeric(Poles[which.max(Poles$rowid), 1] - 2), ],
            tibble(
              X1 = Poles[which.max(Poles$rowid), 1] - 1,
              X2 = -1
            )
          )
        } else if (Poles[which.min(Poles$rowid), 1] == 0 &&
          abs(Poles[which.min(Poles$rowid), 1] - Poles[which.max(Poles$rowid), 1]) == 0) {
          End_morpho <- rbind(
            tibble(
              X1 = Poles[which.min(Poles$rowid), 1] - 1,
              X2 = -1
            ),
            tibble(
              X1 = Poles[which.max(Poles$rowid), 1] - 1,
              X2 = -1
            ),
            End_morpho[Poles[which.max(Poles$rowid), 1]:nrow(End_morpho), ]
          )
        } else if (abs(Poles[which.min(Poles$rowid), 1] - Poles[which.max(Poles$rowid), 1]) == 0) {
          End_morpho <- rbind(
            End_morpho[1:as.numeric(Poles[which.min(Poles$rowid), 1] - 1), ],
            tibble(
              X1 = Poles[which.min(Poles$rowid), 1] - 1,
              X2 = -1
            ),
            tibble(
              X1 = Poles[which.max(Poles$rowid), 1] - 1,
              X2 = -1
            )
          )
        } else {
          End_morpho <- rbind(
            End_morpho[1:as.numeric(Poles[which.min(Poles$rowid), 1] - 1), ],
            tibble(
              X1 = Poles[which.min(Poles$rowid), 1] - 1,
              X2 = -1
            ),
            End_morpho[Poles[which.min(Poles$rowid), 1]:as.numeric(Poles[which.max(Poles$rowid), 1] - 2), ],
            tibble(
              X1 = Poles[which.max(Poles$rowid), 1] - 1,
              X2 = -1
            )
          )
        }

        # save plus/minus association
        assign(
          paste("Amira", "Dataset", current_data, sep = "_"),
          Save_amira(End_morpho, 2, "Nodes", "int"),
          envir = .GlobalEnv
        )
      }
    },
    error = function(e) {}
  )

  # Save Data for KMT torque ---------------------------------------------------
  tryCatch(
    {
      names(Fiber_Torque_P1)[1] <- "Angle"
      names(Fiber_Torque_P2)[1] <- "Angle"

      assign(paste("Data", current_data, "Fiber_Torque_P1", sep = "_"),
        Fiber_Torque_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Torque_P2", sep = "_"),
        Fiber_Torque_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Torque", sep = "_"),
        rbind(Fiber_Torque_P1, Fiber_Torque_P2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "Fiber_Torque_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Torque_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Torque_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Torque_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Torque", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Torque.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for LKD ----------------------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "IKD", sep = "_"),
        Inter_Kinetochore_Distance,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "IKD_KMT_Delta", sep = "_"),
        Inter_Kinetochore_Distance_KMTs_delta,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "IKD_KMT_No", sep = "_"),
        Inter_Kinetochore_Distance_KMTs_no,
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "IKD", sep = "_")),
        paste("Data/", "Data_", current_data, "_IKD.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "IKD_KMT_Delta", sep = "_")),
        paste("Data/", "Data_", current_data, "_IKD_KMT_Delta.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "IKD_KMT_No", sep = "_")),
        paste("Data/", "Data_", current_data, "_IKD_KMT_No.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for total curvature ----------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "KMT_Total_Curv_P1", sep = "_"),
        KMTs_total_Curvature_P1[order(KMTs_total_Curvature_P1$Segment_ID), ],
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMT_Total_Curv_P2", sep = "_"),
        KMTs_total_Curvature_P2[order(KMTs_total_Curvature_P2$Segment_ID), ],
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMT_Total_Curv", sep = "_"),
        rbind(KMTs_total_Curvature_P1, KMTs_total_Curvature_P2),
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMT_Total_Curv", sep = "_"),
        get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))[order(get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))$Segment_ID), ],
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "KMT_Total_Curv_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_Total_Curv_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "KMT_Total_Curv_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_Total_Curv_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "KMT_Total_Curv", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_Total_Curv.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Amira file for curvature ----------------------------------------------
  tryCatch(
    {
      DF <- tibble(
        Tortuosity = as.character(),
        Menger_Curvature = as.character(),
        Segment_ID = as.numeric()
      )

      for (i in 1:nrow(Segments)) {
        if (i %in% get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))$Segment_ID) {
          Curv_data_colector <- get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))[
            which(get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))$Segment_ID == i),
            "Curvature"
          ]
          Menger_data_colector <- get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))[
            which(get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))$Segment_ID == i),
            "Menger_Curvature"
          ]

          DF[i, ] <- tibble(
            Tortuosity = as.character(Curv_data_colector),
            Menger_Curvature = as.character(Menger_data_colector),
            Segment_ID = i
          )
        } else {
          DF[i, ] <- tibble(
            Tortuosity = as.character("nan"),
            Menger_Curvature = as.character("nan"),
            Segment_ID = i
          )
        }
      }

      # save plus/minus association
      assign(
        paste("Amira", "Dataset", current_data, sep = "_"),
        Save_amira(
          DF,
          1, "Segments", "float"
        ),
        envir = .GlobalEnv
      )

      assign(
        paste("Amira", "Dataset", current_data, sep = "_"),
        Save_amira(
          DF,
          2, "Segments", "float"
        ),
        envir = .GlobalEnv
      )
      rm(DF, Curv_data_colector, Menger_data_colector)
    },
    error = function(e) {}
  )

  # Save Data for local curvature ----------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "KMT_Local_Curv_P1", sep = "_"),
        KMTs_local_Curvature_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMT_Local_Curv_P2", sep = "_"),
        KMTs_local_Curvature_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMT_Local_Curv", sep = "_"),
        rbind(KMTs_local_Curvature_P1, KMTs_local_Curvature_P2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "KMT_Local_Curv_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_Local_Curv_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "KMT_Local_Curv_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_Local_Curv_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "KMT_Local_Curv", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMT_Local_Curv.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for Fiber area ---------------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "Fiber_Area_P1", sep = "_"),
        Fiber_area_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Area_P2", sep = "_"),
        Fiber_area_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Area", sep = "_"),
        rbind(Fiber_area_P1, Fiber_area_P2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "Fiber_Area_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Area_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Area_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Area_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Area", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Area.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for Density fiber ------------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "N_Density_P1", sep = "_"),
        N_density_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "N_Density_P2", sep = "_"),
        N_density_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "N_Density", sep = "_"),
        rbind(N_density_P1, N_density_P2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "N_Density_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_N_Density_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "N_Density_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_N_Density_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "N_Density", sep = "_")),
        paste("Data/", "Data_", current_data, "_N_Density.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for Fiber length and curvature fiber -----------------------------
  tryCatch(
    {
      names(Fiber_Length_P1)[1] <- "Length"
      names(Fiber_Length_P2)[1] <- "Length"

      assign(paste("Data", current_data, "Fiber_Length_P1", sep = "_"),
        Fiber_Length_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Length_P2", sep = "_"),
        Fiber_Length_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Length", sep = "_"),
        rbind(Fiber_Length_P1, Fiber_Length_P2),
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Total_Curv_P1", sep = "_"),
        Fiber_Total_Curv_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Total_Curv_P2", sep = "_"),
        Fiber_Total_Curv_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Total_Curv", sep = "_"),
        rbind(Fiber_Total_Curv_P1, Fiber_Total_Curv_P2),
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Local_Curv_P1", sep = "_"),
        Fiber_Local_Curv_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Local_Curv_P2", sep = "_"),
        Fiber_Local_Curv_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Local_Curv", sep = "_"),
        rbind(Fiber_Local_Curv_P1, Fiber_Local_Curv_P2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "Fiber_Length_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Length_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Length_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Length_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Length", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Length.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Total_Curv_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Total_Curv_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Total_Curv_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Total_Curv_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Total_Curv", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Total_Curv.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Local_Curv_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Local_Curv_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Local_Curv_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Local_Curv_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Local_Curv", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Local_Curv.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for nucleation ---------------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "KMTs_minus_seed_P1", sep = "_"),
        KMTs_minus_seed_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMTs_minus_seed_P2", sep = "_"),
        KMTs_minus_seed_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "KMTs_minus_seed", sep = "_"),
        rbind(KMTs_minus_seed_P1, KMTs_minus_seed_P2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "KMTs_minus_seed_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMTs_minus_seed_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "KMTs_minus_seed_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMTs_minus_seed_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "KMTs_minus_seed", sep = "_")),
        paste("Data/", "Data_", current_data, "_KMTs_minus_seed.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for kinetochore area ---------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "K_Core_Area_P1", sep = "_"),
        K_Core_Area_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "K_Core_Area_P2", sep = "_"),
        K_Core_Area_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "K_Core_Area", sep = "_"),
        rbind(K_Core_Area_P1, K_Core_Area_P2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "K_Core_Area_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_K_Core_Area_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "K_Core_Area_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_K_Core_Area_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "K_Core_Area", sep = "_")),
        paste("Data/", "Data_", current_data, "_K_Core_Area.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Amira output
  if (exists("Amira") && Amira == TRUE) {
    write.table(get(paste("Amira", "Dataset", current_data, sep = "_")),
      paste("Data/", "Amira_", "Dataset_", current_data, ".am", sep = ""),
      quote = FALSE, row.names = FALSE, col.names = FALSE
    )
  }

  # Clean Environment ----------------------------------------------------------
  rm(list = ls(pattern = "Pole"))
  rm(list = ls(pattern = "DF"))
}
