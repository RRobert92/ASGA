################################################################################
# Packages MT_Bridging
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz / Gunar Fabig
# Created: 2020-09-01
# Reviewed:
################################################################################

# Set-up analysis --------------------------------------------------------------

A_MT_Bridging <- function(input, output, session) {
    progressSweetAlert(
            session = session,
            id = "P_MT_Bridginig",
            title = "Calculating microtubule interaction...",
            display_pct = TRUE,
            value = round(0 / 4 * 100, 0)
    )

    # Define if analysis should be run as a function of distance or to run only for specified distance length
    if (MT_INT_AS_FUNCTION == TRUE) {
        Function_scale <<- list(0.025, 0.030, 0.035, 0.045, 0.050)
    } else {
        Function_scale <<- list(MT_POINT_CONFIG)
    }

    # Calculate all interaction --------------------------------------------------
    for (j in 1:length(Function_scale)) {
        updateProgressBar(
                session = session,
                id = "P_MT_Bridginig",
                title = paste("Calculating microtubule interaction for", Function_scale[j], "um...", sep = " "),
                value = round(as.numeric(j / length(Function_scale)) * 100, 0)
        )
        Sys.sleep(0.1)

        MT_POINT_CONFIG <<- as.numeric(Function_scale[j])

        cores <<- detectCores()
        cl <<- makeCluster(cores)
        registerDoParallel(cl)

        MT_Interaction <<- foreach(
                i = 1:nrow(Points), .combine = rbind, .inorder = FALSE,
                .export = c("Points", "MT_POINT_CONFIG", "Point_interaction")
        ) %dopar% {
            Point_interaction(i)
        }

        stopCluster(cl)

        assign(paste("MT_Interaction", MT_POINT_CONFIG, sep = "_"),
               MT_Interaction,
               envir = .GlobalEnv
        )
    }

    for (i in 1:length(Function_scale)) {
        MT_POINT_CONFIG <<- as.numeric(Function_scale[i])
        Counter <<- list(str_split((as.numeric(1:4) + (4 * (i - 1))), pattern = " "))
        MT_Interaction <<- get(paste("MT_Interaction", MT_POINT_CONFIG, sep = "_"))

        updateProgressBar(
                session = session,
                id = "P_MT_Bridginig",
                title = paste("Pre-sorting data of MT interaction for", Function_scale[i], "um...", sep = " "),
                value = round(((as.numeric(Counter[[1]][[1]])) / as.numeric(length(Function_scale) * 4)) * 100, 0)
        )
        Sys.sleep(0.1)

        assign("MT_Interaction",
               Segment_to_point(1),
               envir = .GlobalEnv
        )

        updateProgressBar(
                session = session,
                id = "P_MT_Bridginig",
                value = round(((as.numeric(Counter[[1]][[2]]) / as.numeric(length(Function_scale) * 4))) * 100, 0)
        )
        Sys.sleep(0.1)

        assign("MT_Interaction",
               Segment_to_point(2),
               envir = .GlobalEnv
        )

        names(MT_Interaction)[4:5] <<- c("Segments_ID_1", "Segments_ID_2")

        updateProgressBar(
                session = session,
                title = paste("Remove duplicated interactions for", Function_scale[i], "um...", sep = " "),
                id = "P_MT_Bridginig",
                value = round(((as.numeric(Counter[[1]][[3]]) / as.numeric(length(Function_scale) * 4))) * 100, 0)
        )
        Sys.sleep(0.1)

        assign("MT_Interaction",
               Remove_interaction_duplicates(),
               envir = .GlobalEnv
        )

        updateProgressBar(
                session = session,
                title = paste("Searching for unique interacting points for", Function_scale[i], "um...", sep = " "),
                id = "P_MT_Bridginig",
                value = round(((as.numeric(Counter[[1]][[4]]) / as.numeric(length(Function_scale) * 4))) * 100, 0)
        )
        Sys.sleep(0.1)

        assign("MT_Interaction",
               Unique_interaction(),
               envir = .GlobalEnv
        )

        updateProgressBar(
                session = session,
                title = paste("Saving final output for", Function_scale[i], "um...", sep = " "),
                id = "P_MT_Bridginig",
                value = round(((as.numeric(Counter[[1]][[4]]) / as.numeric(length(Function_scale) * 4))) * 100, 0)
        )
        Sys.sleep(0.1)

        assign(paste("MT_Interaction", MT_POINT_CONFIG, sep = "_"),
               MT_Interaction,
               envir = .GlobalEnv
        )
    }

    closeSweetAlert(session = session)
}
