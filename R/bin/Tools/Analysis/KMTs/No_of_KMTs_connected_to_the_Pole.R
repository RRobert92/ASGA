#####################################################################################
# Tool No_of_KMTs_connected_to_the_POle
#
# The analysis tool count no. of KMTs with a (-) end at the pole
#
# x variables in the function is a number of a column contain KMTs information
# Count how many KMTs with a minus end distance of "Minus_Threshold" is in the fiber
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-04-17
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
#####################################################################################

# Tool Include  only no of KMTs at a pole -------------------------------------------
KMTs_to_the_Pole <- function(x) {
    No_of_KMTs <- data.frame()
    seg_df <- tryCatch(get(colnames(Segments)[x]), error = function(e) NULL)

    if (is.null(seg_df) || !("minus_dist_to_pole" %in% colnames(seg_df))) {
        # minus_dist_to_pole not available (pre-analysis might have failed).
        # Record this situation in the global environment so upstream issues
        # can be diagnosed later instead of silently returning a fallback.
        tryCatch({
            if (!exists("MINUS_DIST_TO_POLE_MISSING", envir = .GlobalEnv)) {
                assign("MINUS_DIST_TO_POLE_MISSING", TRUE, envir = .GlobalEnv)
            } else {
                assign("MINUS_DIST_TO_POLE_MISSING", TRUE, envir = .GlobalEnv)
            }

            # Keep a list of segment names missing the column
            seg_name <- colnames(Segments)[x]
            if (!exists("MISSING_MINUS_DIST_SEGMENTS", envir = .GlobalEnv)) {
                assign("MISSING_MINUS_DIST_SEGMENTS", c(seg_name), envir = .GlobalEnv)
            } else {
                current <- get("MISSING_MINUS_DIST_SEGMENTS", envir = .GlobalEnv)
                assign("MISSING_MINUS_DIST_SEGMENTS", unique(c(current, seg_name)), envir = .GlobalEnv)
            }
        }, error = function(e) {})

        warning(paste0("minus_dist_to_pole column missing for segment: ", colnames(Segments)[x], ". Set 'MISSING_MINUS_DIST_SEGMENTS' in global env."))

        # Return 0 to keep downstream logic safe, but upstream flag records the problem.
        return(0)
    }

    DF <- seg_df[with(seg_df, minus_dist_to_pole <= MINUS_THRESHOLD & minus_dist_to_pole > 0), ]

    if (nrow(DF) == 0) {
        No_of_KMTs <- 0
    } else {
        No_of_KMTs <- nrow(DF)
    }

    No_of_KMTs
}

# Tool Include length, minus end distance and minus end distance --------------------
KMTs_to_the_Pole_vs_length <- function(x) {
    No_of_KMTs <- data.frame()
    seg_df <- tryCatch(get(colnames(Segments)[x]), error = function(e) NULL)

    if (is.null(seg_df) || !("minus_dist_to_pole" %in% colnames(seg_df))) {
        # Record and warn about the missing column, then provide an NA row
        tryCatch({
            if (!exists("MINUS_DIST_TO_POLE_MISSING", envir = .GlobalEnv)) {
                assign("MINUS_DIST_TO_POLE_MISSING", TRUE, envir = .GlobalEnv)
            } else {
                assign("MINUS_DIST_TO_POLE_MISSING", TRUE, envir = .GlobalEnv)
            }

            seg_name <- colnames(Segments)[x]
            if (!exists("MISSING_MINUS_DIST_SEGMENTS", envir = .GlobalEnv)) {
                assign("MISSING_MINUS_DIST_SEGMENTS", c(seg_name), envir = .GlobalEnv)
            } else {
                current <- get("MISSING_MINUS_DIST_SEGMENTS", envir = .GlobalEnv)
                assign("MISSING_MINUS_DIST_SEGMENTS", unique(c(current, seg_name)), envir = .GlobalEnv)
            }
        }, error = function(e) {})

        warning(paste0("minus_dist_to_pole column missing for segment: ", colnames(Segments)[x], ". Set 'MISSING_MINUS_DIST_SEGMENTS' in global env."))

        DF <- data.frame(matrix(NA, nrow = 1, ncol = max(7, if (!is.null(seg_df)) ncol(seg_df) else 7)))
        No_of_KMTs <- data.frame(
            c(nrow(DF)),
            c(DF[2]),
            c(DF[3]),
            c(DF[4]),
            c(DF[5]),
            c(DF[7])
        )
        return(No_of_KMTs)
    }

    DF <- seg_df[with(seg_df, minus_dist_to_pole <= MINUS_THRESHOLD & minus_dist_to_pole > 0), ]

    if (nrow(DF) == 0) {
        DF[1, ] <- NA

        No_of_KMTs <- data.frame(
            c(nrow(DF)),
            c(DF[2]),
            c(DF[3]),
            c(DF[4]),
            c(DF[5]),
            c(DF[7])
        )
    } else {
        No_of_KMTs <- data.frame(
            c(nrow(DF)),
            c(DF[2]),
            c(DF[3]),
            c(DF[4]),
            c(DF[5]),
            c(DF[7])
        )
    }

    No_of_KMTs
}
