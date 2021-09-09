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
    progressSweetAlert(
            session = session,
            id = "Saving_Data",
            title = "Saving analysed data...",
            display_pct = TRUE,
            value = 0
    )
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
                paste0("Data/", "Data_", current_data, "_LD_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "LD_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_LD_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "LD", sep = "_")),
                paste0("Data/", "Data_", current_data, "_LD.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Data for SMT ends -----------------------------------------------------
    tryCatch(
    {
        updateProgressBar(
                session = session,
                id = "Saving_Data",
                title = "Saving SMT ends data...",
                value = 10
        )
        Sys.sleep(0.1)

        assign(paste("Data", current_data, "SMT_Ends", sep = "_"),
               SMT_Ends,
               envir = .GlobalEnv
        )
        write.xlsx(
                get(paste("Data", current_data, "SMT_Ends", sep = "_")),
                paste0("Data/", "Data_", current_data, "_SMT_Ends.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Data for KMT No -------------------------------------------------------
    tryCatch(
    {
        updateProgressBar(
                session = session,
                id = "Saving_Data",
                title = "Saving KMT number data...",
                value = 20
        )
        Sys.sleep(0.1)

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
                paste0("Data/", "Data_", current_data, "_KMT_No_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "KMT_No_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_KMT_No_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "KMT_No", sep = "_")),
                paste0("Data/", "Data_", current_data, "_KMT_No.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Data for KMT at the Pole ----------------------------------------------
    tryCatch(
    {
        updateProgressBar(
                session = session,
                id = "Saving_Data",
                title = "Saving KMT position data...",
                value = 30
        )
        Sys.sleep(0.1)

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
                paste0("Data/", "Data_", current_data, "_KMT_Pole_P1.xlsx",)
        )
        write.xlsx(
                get(paste("Data", current_data, "KMT_Pole_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_KMT_Pole_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "KMT_Pole", sep = "_")),
                paste0("Data/", "Data_", current_data, "_KMT_Pole.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Data for KMT minus end interaction ------------------------------------
    tryCatch(
    {
        for (i in 1:length(Function_Scale_Seed)) {
            DF_Name <<- paste("KMT_Minus_End", Function_Scale_Seed[i], sep = "_")

            tryCatch(
            {
                if (exists(paste(DF_Name))) {
                    updateProgressBar(
                            session = session,
                            id = "Saving_Data",
                            title = "Saving KMT minus- ends interaction...",
                            value = 32
                    )
                    Sys.sleep(0.1)
                }

                assign(paste("Data", current_data, paste(DF_Name), sep = "_"),
                       get(paste(DF_Name)),
                       envir = .GlobalEnv
                )

                write.xlsx(
                        get(paste("Data", current_data, paste(DF_Name), sep = "_")),
                        paste0("Data/", "Data_", current_data, "_", paste(DF_Name), ".xlsx")
                )
            },
                    error = function(e) {}
            )
        }
    },
            error = function(e) {}
    )

    # Save Data for end morphology Pole ------------------------------------------
    tryCatch(
    {
        if (exists("Minus_end_morphology_Pole1")) {
            updateProgressBar(
                    session = session,
                    id = "Saving_Data",
                    title = "Saving KMT ends morphology data...",
                    value = 35
            )
            Sys.sleep(0.1)
        }

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
        assign(paste("Data", current_data, "MT_Ends_Type", sep = "_"),
               MT_Ends_Type,
               envir = .GlobalEnv)

        write.xlsx(
                get(paste("Data", current_data, "Minus_end_morphology_Pole1", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Minus_end_morphology_Pole1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Plus_end_morphology_Pole1", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Plus_end_morphology_Pole1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Minus_end_morphology_Pole2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Minus_end_morphology_Pole2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Plus_end_morphology_Pole2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Plus_end_morphology_Pole2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Plus_end_morphology", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Plus_end_morphology.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Minus_end_morphology", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Minus_end_morphology.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "MT_Ends_Type", sep = "_")),
                paste0("Data/", "Data_", current_data, "_MT_Ends_Type.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Amira file for end morphology -----------------------------------------
    tryCatch(
    {

        # Prepare data for saving in Amira ASCI file
        if (AMIRA == TRUE &&
                ncol(Nodes %>% select(starts_with("EndType"))) >= 1) {
            End_morpho <- rbind(
                    tibble(X1 = Data_1_Minus_end_morphology$Node_ID, X2 = 0),
                    tibble(X1 = Data_1_Plus_end_morphology$Node_ID, X2 = 1)
            )
            End_morpho <- End_morpho[order(End_morpho$X1),]
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
                        End_morpho[Poles[which.min(Poles$rowid), 1]:as.numeric(Poles[which.max(Poles$rowid), 1] - 2),],
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
                        End_morpho[Poles[which.max(Poles$rowid), 1]:nrow(End_morpho),]
                )
            } else if (abs(Poles[which.min(Poles$rowid), 1] - Poles[which.max(Poles$rowid), 1]) == 0) {
                End_morpho <- rbind(
                        End_morpho[1:as.numeric(Poles[which.min(Poles$rowid), 1] - 1),],
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
                        End_morpho[1:as.numeric(Poles[which.min(Poles$rowid), 1] - 1),],
                        tibble(
                                X1 = Poles[which.min(Poles$rowid), 1] - 1,
                                X2 = -1
                        ),
                        End_morpho[Poles[which.min(Poles$rowid), 1]:as.numeric(Poles[which.max(Poles$rowid), 1] - 2),],
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
        if (exists("Fiber_Torque_P1")) {
            updateProgressBar(
                    session = session,
                    id = "Saving_Data",
                    title = "Saving KMT torque data...",
                    value = 38
            )
            Sys.sleep(0.1)
        }

        assign(paste("Data", current_data, "Fiber_Torque_P1", sep = "_"),
               Fiber_Torque_P1,
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "Spindle_Torque_P1", sep = "_"),
               Spindle_Torque_P1,
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "Helicity_P1", sep = "_"),
               Helicity_P1,
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "Fiber_Torque_P2", sep = "_"),
               Fiber_Torque_P2,
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "Spindle_Torque_P2", sep = "_"),
               Spindle_Torque_P2,
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "Helicity_P2", sep = "_"),
               Helicity_P2,
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "Fiber_Torque", sep = "_"),
               rbind(Fiber_Torque_P1, Fiber_Torque_P2),
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "Spindle_Torque", sep = "_"),
               rbind(Spindle_Torque_P1, Spindle_Torque_P2),
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "Helicity", sep = "_"),
               rbind(Helicity_P1, Helicity_P2),
               envir = .GlobalEnv
        )

        write.xlsx(
                get(paste("Data", current_data, "Fiber_Torque_P1", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Torque_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Spindle_Torque_P1", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Spindle_Torque_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Helicity_P1", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Helicity_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Torque_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Torque_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Spindle_Torque_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Spindle_Torque_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Helicity_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Helicity_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Torque", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Torque.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Spindle_Torque", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Spindle_Torque.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Helicity", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Helicity.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Data for IKD ----------------------------------------------------------
    tryCatch(
    {
        updateProgressBar(
                session = session,
                id = "Saving_Data",
                title = "Saving IKD data...",
                value = 40
        )
        Sys.sleep(0.1)

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
                paste0("Data/", "Data_", current_data, "_IKD.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "IKD_KMT_Delta", sep = "_")),
                paste0("Data/", "Data_", current_data, "_IKD_KMT_Delta.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "IKD_KMT_No", sep = "_")),
                paste0("Data/", "Data_", current_data, "_IKD_KMT_No.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Data for total curvature ----------------------------------------------
    tryCatch(
    {
        if (exists("KMTs_total_Curvature_P1")) {
            updateProgressBar(
                    session = session,
                    id = "Saving_Data",
                    title = "Saving KMT curvature data...",
                    value = 42
            )
            Sys.sleep(0.1)
        }

        assign(paste("Data", current_data, "KMT_Total_Curv_P1", sep = "_"),
               KMTs_total_Curvature_P1[order(KMTs_total_Curvature_P1$Segment_ID),],
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "KMT_Total_Curv_P2", sep = "_"),
               KMTs_total_Curvature_P2[order(KMTs_total_Curvature_P2$Segment_ID),],
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "KMT_Total_Curv", sep = "_"),
               rbind(KMTs_total_Curvature_P1, KMTs_total_Curvature_P2),
               envir = .GlobalEnv
        )
        assign(paste("Data", current_data, "KMT_Total_Curv", sep = "_"),
               get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))[order(get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))$Segment_ID),],
               envir = .GlobalEnv
        )

        write.xlsx(
                get(paste("Data", current_data, "KMT_Total_Curv_P1", sep = "_")),
                paste0("Data/", "Data_", current_data, "_KMT_Total_Curv_P1.xlsx", sep = "")
        )
        write.xlsx(
                get(paste("Data", current_data, "KMT_Total_Curv_P2")),
                paste0("Data/", "Data_", current_data, "_KMT_Total_Curv_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "KMT_Total_Curv", sep = "_")),
                paste0("Data/", "Data_", current_data, "_KMT_Total_Curv.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Amira file for curvature ----------------------------------------------
    if (AMIRA == TRUE) {
        if (exists("KMTs_total_Curvature_P1")) {
            updateProgressBar(
                    session = session,
                    id = "Saving_Data",
                    title = "Saving KMT curvature Amira data...",
                    value = 45
            )
            Sys.sleep(0.1)
        }

        tryCatch(
        {
            DF <- tibble(
                    Tortuosity = as.character(),
                    Menger_Curvature = as.character(),
                    Segment_ID = as.numeric()
            )
            counter <- 1

            for (i in Segments$`Segment ID`) {
                if (i %in% get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))$Segment_ID) {
                    Curv_data_colector <- get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))[
                            which(get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))$Segment_ID == i),
                            "Curvature"
                    ]
                    Menger_data_colector <- get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))[
                            which(get(paste("Data", current_data, "KMT_Total_Curv", sep = "_"))$Segment_ID == i),
                            "Menger_Curvature"
                    ]

                    DF[counter,] <- tibble(
                            Tortuosity = as.character(Curv_data_colector),
                            Menger_Curvature = as.character(Menger_data_colector),
                            Segment_ID = i
                    )

                    if (DF[counter, 1] == "NaN") {
                        DF[counter, 1] <- tibble(Tortuosity = as.character("nan"))
                    }
                    if (DF[counter, 2] == "NaN") {
                        DF[counter, 2] <- tibble(Menger_Curvature = as.character("nan"))
                    }
                } else {
                    DF[counter,] <- tibble(
                            Tortuosity = as.character("nan"),
                            Menger_Curvature = as.character("nan"),
                            Segment_ID = i
                    )
                }
                counter <- counter + 1
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
    }
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
                paste0("Data/", "Data_", current_data, "_KMT_Local_Curv_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "KMT_Local_Curv_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_KMT_Local_Curv_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "KMT_Local_Curv", sep = "_")),
                paste0("Data/", "Data_", current_data, "_KMT_Local_Curv.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Data for Fiber area ---------------------------------------------------
    tryCatch(
    {
        if (exists("Fiber_area_P1")) {
            updateProgressBar(
                    session = session,
                    id = "Saving_Data",
                    title = "Saving KMT fiber area data...",
                    value = 55
            )
            Sys.sleep(0.1)
        }

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
                paste0("Data/", "Data_", current_data, "_Fiber_Area_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Area_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Area_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Area", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Area.xlsx")
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
                paste0("Data/", "Data_", current_data, "_N_Density_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "N_Density_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_N_Density_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "N_Density", sep = "_")),
                paste0("Data/", "Data_", current_data, "_N_Density.xlsx")
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
                paste0("Data/", "Data_", current_data, "_Fiber_Length_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Length_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Length_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Length", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Length.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Total_Curv_P1", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Total_Curv_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Total_Curv_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Total_Curv_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Total_Curv", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Total_Curv.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Local_Curv_P1", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Local_Curv_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Local_Curv_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Local_Curv_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "Fiber_Local_Curv", sep = "_")),
                paste0("Data/", "Data_", current_data, "_Fiber_Local_Curv.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Data for nucleation ---------------------------------------------------
    tryCatch(
    {

        for (i in 1:length(Function_Scale_Seed)) {
            DF_Name <<- paste("KMTs_minus_seed", Function_Scale_Seed[i], sep = "_")
            DF_Name_1 <<- paste("KMTs_minus_seed_P1", Function_Scale_Seed[i], sep = "_")
            DF_Name_2 <<- paste("KMTs_minus_seed_P2", Function_Scale_Seed[i], sep = "_")

            tryCatch(
            {
                if (exists(paste(DF_Name_1))) {
                    updateProgressBar(
                            session = session,
                            id = "Saving_Data",
                            title = "Saving MT and KMT nucleation data...",
                            value = 70
                    )
                    Sys.sleep(0.1)
                }

                assign(paste("Data", current_data, paste(DF_Name_1), sep = "_"),
                       get(paste(DF_Name_1)),
                       envir = .GlobalEnv
                )
                write.xlsx(
                        get(paste("Data", current_data, paste(DF_Name_1), sep = "_")),
                        paste0("Data/", "Data_", current_data, "_", paste(DF_Name_1), ".xlsx")
                )

                assign(paste("Data", current_data, paste(DF_Name_2), sep = "_"),
                       get(paste(DF_Name_2)),
                       envir = .GlobalEnv
                )
                write.xlsx(
                        get(paste("Data", current_data, paste(DF_Name_2), sep = "_")),
                        paste0("Data/", "Data_", current_data, "_", paste(DF_Name_2), ".xlsx")
                )

                assign(paste("Data", current_data, paste(DF_Name), sep = "_"),
                       rbind(get(paste("Data", current_data, paste(DF_Name_1), sep = "_")),
                             get(paste("Data", current_data, paste(DF_Name_2), sep = "_"))),
                       envir = .GlobalEnv
                )
                write.xlsx(
                        get(paste("Data", current_data, paste(DF_Name), sep = "_")),
                        paste0("Data/", "Data_", current_data, "_", paste(DF_Name), ".xlsx")
                )
            },
                    error = function(e) {}
            )
        }
    },
            error = function(e) {}
    )

    # Save Amira for KMT minus end interaction -----------------------------------
    tryCatch(
    {
        if (exists("KMT_Minus_End")) {
            updateProgressBar(
                    session = session,
                    id = "Saving_Data",
                    title = "Saving KMT minus end interaction data...",
                    value = 75
            )
            Sys.sleep(0.1)
        }

        for (j in 1:length(Function_Scale_Seed)) {
            DF_Name <<- paste("KMT_Minus_End", Function_Scale_Seed[j], sep = "_")

            tryCatch(
            {
                DF <- tibble(
                        Interaction_Type = as.numeric(),
                        Minus_Distance = as.character(),
                        Segment_ID = as.numeric()
                )
                counter <- 1

                for (i in Segments$`Segment ID`) {
                    if (i %in% get(paste("Data", current_data, DF_Name, sep = "_"))$KMT_ID) {
                        Minus_Interaction <- get(paste("Data", current_data, DF_Name, sep = "_"))[
                                which(get(paste("Data", current_data, DF_Name, sep = "_"))$KMT_ID == i),
                                "MT_type"
                        ]

                        if (Minus_Interaction == NaN) { # KMT with no interaction
                            Minus_Interaction <- 0
                        } else if (Minus_Interaction == "KMT") { # KMT with KMT interaction
                            Minus_Interaction <- 1
                        } else if (Minus_Interaction == "SMT") { # KMT with SMT interaction
                            Minus_Interaction <- 2
                        }

                        tryCatch(
                        {
                            Minus_distance <- get(paste("Data", current_data, DF_Name, sep = "_"))[
                                    which(get(paste("Data", current_data, DF_Name, sep = "_"))$KMT_ID == i),
                                    "KMT_Minus_Distance"
                            ]
                        },
                                error = function(e) {
                                    Minus_distance <<- NaN
                                }
                        )

                        DF[counter,] <- tibble(
                                Interaction_Type = as.numeric(Minus_Interaction),
                                Minus_Distance = as.character(Minus_distance),
                                Segment_ID = i
                        )

                        if (DF[counter, 2] == "NaN") {
                            DF[counter, 2] <- tibble(Minus_distance = as.character("nan"))
                        }
                        counter <- counter + 1
                    } else {
                        DF[counter,] <- tibble(
                                Interaction_Type = as.numeric(0),
                                Minus_Distance = as.character("nan"),
                                Segment_ID = i
                        )
                        counter <- counter + 1
                    }
                }
            },
                    error = function(e) {}
            )

            names(DF)[1] <- c(paste0("Interaction_Type_", Function_Scale_Seed[j]))
            assign(
                    paste("Amira", "Dataset", current_data, sep = "_"),
                    Save_amira(
                            DF,
                            1, "Segments", "int"
                    ),
                    envir = .GlobalEnv
            )
        }

        assign(
                paste("Amira", "Dataset", current_data, sep = "_"),
                Save_amira(
                        DF,
                        2, "Segments", "float"
                ),
                envir = .GlobalEnv
        )

        rm(DF, Minus_Interaction, Minus_distance)
    },
            error = function(e) {}
    )
    # Save Data for kinetochore area ---------------------------------------------
    tryCatch(
    {
        if (exists("K_Core_Area_P1")) {
            updateProgressBar(
                    session = session,
                    id = "Saving_Data",
                    title = "Saving kinetochore area data...",
                    value = 90
            )
            Sys.sleep(0.1)
        }

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
                paste0("Data/", "Data_", current_data, "_K_Core_Area_P1.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "K_Core_Area_P2", sep = "_")),
                paste0("Data/", "Data_", current_data, "_K_Core_Area_P2.xlsx")
        )
        write.xlsx(
                get(paste("Data", current_data, "K_Core_Area", sep = "_")),
                paste0("Data/", "Data_", current_data, "_K_Core_Area.xlsx")
        )
    },
            error = function(e) {}
    )

    # Save Data for MT interaction -----------------------------------------------
    tryCatch(
    {
        updateProgressBar(
                session = session,
                id = "Saving_Data",
                title = "Saving KMT interaction data...",
                value = 95
        )
        Sys.sleep(0.1)
        for (i in 1:length(Function_scale)) {
            DF_Name <<- paste("MT_Interaction", Function_scale[i], sep = "_")

            tryCatch(
            {
                if (exists(paste(DF_Name))) {
                }

                assign(paste("Data", current_data, paste(DF_Name), sep = "_"),
                       get(paste(DF_Name)),
                       envir = .GlobalEnv
                )

                write.xlsx(
                        get(paste("Data", current_data, paste(DF_Name), sep = "_")),
                        paste0("Data/", "Data_", current_data, "_", paste(DF_Name), ".xlsx")
                )
            },
                    error = function(e) {}
            )
        }
    },
            error = function(e) {}
    )

    # Save Amira for MT interaction -----------------------------------------------
    tryCatch(
    {
        for (j in 1:length(Function_scale)) {
            DF_Name <<- paste("MT_Interaction", Function_scale[j], sep = "_")

            tryCatch(
            {
                if (exists(paste(DF_Name))) {
                    updateProgressBar(
                            session = session,
                            id = "Saving_Data",
                            title = "Saving KMT interaction Amira data...",
                            value = 98
                    )
                    Sys.sleep(0.1)
                }

                Segment_DF <- tibble(
                        Segment_ID = as.numeric(),
                        Interaction_No = as.numeric()
                )
                counter <- 1

                for (i in Segments$`Segment ID`) {
                    if (i %in% get(paste("Data", current_data, paste(DF_Name), sep = "_"))$Segments_ID_1) {
                        Interaction_No <- get(paste("Data", current_data, paste(DF_Name), sep = "_"))[
                                which(get(paste("Data", current_data, paste(DF_Name), sep = "_"))$Segments_ID_1 == i),
                                1:2
                        ]

                        Interaction_No <- nrow(Interaction_No)
                        Segment_DF[counter, 1] <- as.numeric(i)
                        Segment_DF[counter, 2] <- as.numeric(Interaction_No)

                        counter <- counter + 1
                    } else {
                        Segment_DF[counter, 1] <- as.numeric(i)
                        Segment_DF[counter, 2] <- as.numeric(0)

                        counter <- counter + 1
                    }
                }
                names(Segment_DF)[2] <- paste("Interaction_No", Function_scale[j], sep = "_")

                Point_DF <- tibble(
                        Point_ID = as.numeric(),
                        Interaction_No_Point = as.numeric()
                )

                Point_DF[1:nrow(Points),] <- tibble(
                        Point_ID = as.numeric(0:as.numeric(nrow(Points) - 1)),
                        Interaction_No_Point = as.numeric(0)
                )

                for (i in 1:nrow(get(paste("Data", current_data, paste(DF_Name), sep = "_")))) {
                    S_1 <- as.numeric(get(paste("Data", current_data, paste(DF_Name), sep = "_"))[i, 4] + 1)
                    S_2 <- as.numeric(get(paste("Data", current_data, paste(DF_Name), sep = "_"))[i, 5] + 1)

                    if (S_1 < S_2) {
                        Point_DF[S_1:S_2, 2] <- Point_DF[S_1:S_2, 2] + 1
                    } else if (S_1 > S_2) {
                        Point_DF[S_2:S_1, 2] <- Point_DF[S_2:S_1, 2] + 1
                    }
                }
                names(Point_DF)[2] <- paste("Interaction_No_Point", Function_scale[j], sep = "_")

                assign(
                        paste("Amira", "Dataset", current_data, sep = "_"),
                        Save_amira(
                                Segment_DF,
                                2, "Segments", "int"
                        ),
                        envir = .GlobalEnv
                )

                assign(
                        paste("Amira", "Dataset", current_data, sep = "_"),
                        Save_amira(
                                Point_DF,
                                2, "Points", "int"
                        ),
                        envir = .GlobalEnv
                )
                rm(S_1, S_2, Point_DF, Segment_DF, Interaction_No)
            },
                    error = function(e) {}
            )
        }
    },
            error = function(e) {}
    )
    # Amira output ---------------------------------------------------------------
    if (exists("Amira_df") && AMIRA == TRUE) {
        updateProgressBar(
                session = session,
                id = "Saving_Data",
                title = "Saving finall Amira file...",
                value = 100
        )
        Sys.sleep(0.1)

        write.table(get(paste("Amira", "Dataset", current_data, sep = "_")),
                    paste0("Data/", "Amira_", "Dataset_", current_data, ".am"),
                    quote = FALSE, row.names = FALSE, col.names = FALSE
        )
    } else {
        updateProgressBar(
                session = session,
                id = "Saving_Data",
                title = "All Done!",
                value = 100
        )
        Sys.sleep(0.1)
    }

    # Clean Environment ----------------------------------------------------------
    rm(list = ls(pattern = "Pole"))
    rm(list = ls(pattern = "DF"))
    closeSweetAlert(session = session)
}
