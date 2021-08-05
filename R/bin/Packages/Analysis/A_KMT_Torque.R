################################################################################
# Packages Fiber Torque
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-07-07
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

A_KMT_Torque <- function(input, output, session) {
    # Set-up analysis --------------------------------------------------------------
    # Analyze KMT torque for Pole1 -------------------------------
    total <- as.numeric(length(which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)

    progressSweetAlert(
            session = session,
            id = "P_Torque1",
            title = "Calculating KMT torque for the Pole1...",
            display_pct = TRUE,
            value = 0
    )

    bin <<- data.frame()
    Torque <<- data.frame()
    for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
        tryCatch(
        {
            if ("Leading" %in% colnames(get(paste(colnames(Segments)[i])))) {

            } else {
                assign(paste(colnames(Segments)[i]),
                       leading_KMTsv2(i, Pole1),
                       envir = .GlobalEnv
                )
            }

            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   Leadig_Pointsv2(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   find_polygon(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   duplicated_points(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   median_point(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   find_polygon_for_all(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   duplicated_points(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   median_point(i),
                   envir = .GlobalEnv
            )
            assign("bin",
                   KMTs_torque_in_fiber(i),
                   envir = .GlobalEnv
            )
            assign("Torque",
                   rbind(Torque, bin),
                   envir = .GlobalEnv
            )
        },
                error = function(e) {}
        )

        updateProgressBar(
                session = session,
                id = "P_Torque1",
                value = round((i - 1) / total * 100, 0)
        )
        Sys.sleep(0.1)
    }
    closeSweetAlert(session = session)

    Fiber_Torque_P1 <<- Torque

    # Analyze KMT torque for Pole1 -------------------------------
    total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) -
            as.numeric(which(colnames(Segments) == "Pole2_00") - 1)

    progressSweetAlert(
            session = session,
            id = "P_Torque2",
            title = "Calculating KMT torque for the Pole2...",
            display_pct = TRUE,
            value = 0
    )

    bin <<- data.frame()
    Torque <<- data.frame()
    for (i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments) - 4)) {
        tryCatch(
        {
            if ("Leading" %in% colnames(get(paste(colnames(Segments)[i])))) {

            } else {
                assign(paste(colnames(Segments)[i]),
                       leading_KMTsv2(i, Pole2),
                       envir = .GlobalEnv
                )
            }

            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   Leadig_Pointsv2(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   find_polygon(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   duplicated_points(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   median_point(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   find_polygon_for_all(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   duplicated_points(i),
                   envir = .GlobalEnv
            )
            assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
                   median_point(i),
                   envir = .GlobalEnv
            )
            assign("bin",
                   KMTs_torque_in_fiber(i),
                   envir = .GlobalEnv
            )
            assign("Torque",
                   rbind(Torque, bin),
                   envir = .GlobalEnv
            )
        },
                error = function(e) {}
        )

        updateProgressBar(
                session = session,
                id = "P_Torque2",
                value = round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100, 0)
        )
        Sys.sleep(0.1)
    }
    closeSweetAlert(session = session)

    Fiber_Torque_P2 <<- Torque
}
