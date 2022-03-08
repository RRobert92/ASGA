################################################################################
# Packages KMT_number
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-18
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# Set-up analysis --------------------------------------------------------------
A_KMT_number <- function(input, output, session) {

    # Set-up for Pole1  ------------------------------------------------------------
    KMTs_at_the_Pole1 <- KMTs_to_the_Pole(which(colnames(Segments) == "Pole1_00"))
    names(KMTs_at_the_Pole1)[1] <- "No. of KMTs"
    KMTs_at_the_Pole1 <- as.data.frame(KMTs_at_the_Pole1)

    KMTs_to_the_Pole1_and_length <- KMTs_to_the_Pole_vs_length(which(colnames(Segments) == "Pole1_00"))
    names(KMTs_to_the_Pole1_and_length)[1] <- "No. of KMTs"
    names(KMTs_to_the_Pole1_and_length)[2] <- "KMTs length"
    names(KMTs_to_the_Pole1_and_length)[3] <- "Minus end dist."
    names(KMTs_to_the_Pole1_and_length)[4] <- "Plus end dist. to k-core"
    names(KMTs_to_the_Pole1_and_length)[5] <- "Plus end dist. to pole"

    KMTs_to_the_Pole1_and_length <- as.data.frame(KMTs_to_the_Pole1_and_length)

    total <- as.numeric(length(which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)

    DF1 <- data.frame()
    DF2 <- data.frame()

    progressSweetAlert(
            session = session,
            id = "P_KMT_number1_1",
            title = "Calculating no. of KMTs reaching the pole1...",
            display_pct = TRUE,
            value = 0
    )

    # Analyze KMT at the Pole1 -----------------------------------------------------
    for (i in as.numeric(which(colnames(Segments) == "Pole1_00") + 1):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
        tryCatch(
        {
            assign(
                    "DF1",
                    KMTs_to_the_Pole(i)
            )
            names(DF1)[1] <- "No. of KMTs"
            KMTs_at_the_Pole1 <- rbind(KMTs_at_the_Pole1, DF1)

            assign(
                    "DF2",
                    KMTs_to_the_Pole_vs_length(i)
            )

            names(DF2)[1] <- "No. of KMTs"
            names(DF2)[2] <- "KMTs length"
            names(DF2)[3] <- "Minus end dist."
            names(DF2)[4] <- "Plus end dist. to k-core"
            names(DF2)[5] <- "Plus end dist. to pole"

            KMTs_to_the_Pole1_and_length <- rbind(KMTs_to_the_Pole1_and_length, DF2)
            KMTs_to_the_Pole1_and_length <- na.omit(KMTs_to_the_Pole1_and_length)
        },
                error = function(e) {}
        )

        updateProgressBar(
                session = session,
                id = "P_KMT_number1_1",
                value = round((i - 1) / total * 100, 0)
        )
        Sys.sleep(0.1)
    }
    closeSweetAlert(session = session)

    tryCatch(
    {
      KMTs_at_the_Pole1 <<- data.frame(KMTs_at_the_Pole1[, 1])
      names(KMTs_at_the_Pole1)[1] <- "No. of KMTs"
      KMTs_to_the_Pole1_and_length <<- KMTs_to_the_Pole1_and_length
    },
      error = function(e) {}
    )

    # Set-up for Pole2  ------------------------------------------------------------
    tryCatch(
      {
        KMTs_at_the_Pole2 <- KMTs_to_the_Pole(which(colnames(Segments) == "Pole2_00"))
        names(KMTs_at_the_Pole2)[1] <- "No. of KMTs"

        KMTs_to_the_Pole2_and_length <- KMTs_to_the_Pole_vs_length(which(colnames(Segments) == "Pole2_00"))
        names(KMTs_to_the_Pole2_and_length)[1] <- "No. of KMTs"
        names(KMTs_to_the_Pole2_and_length)[2] <- "KMTs length"
        names(KMTs_to_the_Pole2_and_length)[3] <- "Minus end dist."
        names(KMTs_to_the_Pole2_and_length)[4] <- "Plus end dist. to k-core"
        names(KMTs_to_the_Pole2_and_length)[5] <- "Plus end dist. to pole"

        total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) -
          as.numeric(which(colnames(Segments) == "Pole2_00") - 1)

        DF1 <- data.frame()
        DF2 <- data.frame()
      },
      error = function(e) {}
    )

    progressSweetAlert(
            session = session,
            id = "P_KMT_number2_1",
            title = "Calculating no. of KMTs reaching the pole2...",
            display_pct = TRUE,
            value = 0
    )

    # Analyze KMT at the Pole2 -----------------------------------------------------
    for (i in as.numeric(which(colnames(Segments) == "Pole2_00") + 1):as.numeric(ncol(Segments) - 4)) {
        tryCatch(
        {
            assign(
                    "DF1",
                    KMTs_to_the_Pole(i)
            )
            names(DF1)[1] <- "No. of KMTs"
            KMTs_at_the_Pole2 <- rbind(KMTs_at_the_Pole2, DF1)

            assign(
                    "DF2",
                    KMTs_to_the_Pole_vs_length(i)
            )

            names(DF2)[1] <- "No. of KMTs"
            names(DF2)[2] <- "KMTs length"
            names(DF2)[3] <- "Minus end dist."
            names(DF2)[4] <- "Plus end dist. to k-core"
            names(DF2)[5] <- "Plus end dist. to pole"

            KMTs_to_the_Pole2_and_length <- rbind(KMTs_to_the_Pole2_and_length, DF2)
            KMTs_to_the_Pole2_and_length <- na.omit(KMTs_to_the_Pole2_and_length)
        },
                error = function(e) {}
        )

        updateProgressBar(
                session = session,
                id = "P_KMT_number2_1",
                value = round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100, 0)
        )
        Sys.sleep(0.1)
    }
    closeSweetAlert(session = session)

    tryCatch(
    {
        KMTs_at_the_Pole2 <<- data.frame(KMTs_at_the_Pole2[, 1])
        names(KMTs_at_the_Pole2)[1] <- "No. of KMTs"
        KMTs_to_the_Pole2_and_length <<- KMTs_to_the_Pole2_and_length
    },
            error = function(e) {}
    )

    # Analyze KMT at the kinetochore Pole1 -------------------------------------------
    No_of_KMTs_at_kinetochore_P1 <- No_of_KMTs(which(colnames(Segments) == "Pole1_00"))
    DF <- data.frame()
    total <- as.numeric(length(which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)

    progressSweetAlert(
            session = session,
            id = "P_KMT_number1_2",
            title = "Counting no. of KMTs at each kinetochore from the Pole1...",
            display_pct = TRUE,
            value = 0
    )

    for (i in as.numeric(which(colnames(Segments) == "Pole1_00") + 1):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
        tryCatch(
        {
            assign(
                    "DF",
                    No_of_KMTs(i)
            )
            names(DF)[1] <- "No. of KMTs"
            No_of_KMTs_at_kinetochore_P1 <- rbind(No_of_KMTs_at_kinetochore_P1, DF)
        },
                error = function(e) {}
        )

        updateProgressBar(
                session = session,
                id = "P_KMT_number1_2",
                value = round((i - 1) / total * 100, 0)
        )
        Sys.sleep(0.1)
    }
    closeSweetAlert(session = session)

    No_of_KMTs_at_kinetochore_P1 <<- data.frame(No_of_KMTs_at_kinetochore_P1[, 1])
    names(No_of_KMTs_at_kinetochore_P1)[1] <- "No. of KMTs"

    # Analyze KMT at the kinetochore Pole2 -------------------------------------------
    tryCatch(
    {
        No_of_KMTs_at_kinetochore_P2 <- No_of_KMTs(which(colnames(Segments) == "Pole2_00"))
        DF <- data.frame()
    },
            error = function(e) {}
    )

    total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) -
            as.numeric(which(colnames(Segments) == "Pole2_00") - 1)

    progressSweetAlert(
            session = session,
            id = "P_KMT_number2_2",
            title = "Counting no. of KMTs at each kinetochore from the Pole2...",
            display_pct = TRUE,
            value = 0
    )

    for (i in as.numeric(which(colnames(Segments) == "Pole2_00") + 1):as.numeric(ncol(Segments) - 4)) {
        tryCatch(
        {
            assign(
                    "DF",
                    No_of_KMTs(i)
            )

            names(DF)[1] <- "No. of KMTs"

            No_of_KMTs_at_kinetochore_P2 <- rbind(No_of_KMTs_at_kinetochore_P2, DF)
        },
                error = function(e) {}
        )
        updateProgressBar(
                session = session,
                id = "P_KMT_number2_2",
                value = round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100, 0)
        )
        Sys.sleep(0.1)
    }
    closeSweetAlert(session = session)

    tryCatch(
    {
        No_of_KMTs_at_kinetochore_P2 <<- data.frame(No_of_KMTs_at_kinetochore_P2[, 1])
        names(No_of_KMTs_at_kinetochore_P2)[1] <- "No. of KMTs"
    },
            error = function(e) {}
    )
}
