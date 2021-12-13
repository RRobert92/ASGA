#####################################################################################
# Tool End_Morphology
#
# The analysis tools to defined end morphology
#
# Count the error rate between sample
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-04-21
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
#####################################################################################

# Tool End Morphology ---------------------------------------------------------------
End_Type_Error <- function() {
    if (ncol(Nodes %>% select(starts_with("EndType"))) == 2) {
        End_type_error <- data.frame(
                Correct = round(table(Nodes[, 7])["TRUE"] * 100 / nrow(Nodes), 2),
                Wrong = round(table(Nodes[, 7])["FALSE"] * 100 / nrow(Nodes), 2)
        )
    } else {
        break
    }

    End_type_error
}

# Analyze the end distribution according to the relative position of the KMT end ----
# X is here no. of column in the "Segment" ------------------------------------------
# Y is a no. of a Pole 1 or 2 -------------------------------------------------------
End_distribution_Plus <- function(x, y) {

    # Function setting ------------------------------------------------------------------
    if (y == 1) {
        y <- Pole1
        y_df <- 1
    } else {
        y <- Pole2
        y_df <- 2
    }

    Plus <- data.frame()

    # For x find Node ID that belong to Segment ID ---------------------------------------
    if (nrow(get(colnames(Segments)[x])) >= 1) {
        for (i in 1:nrow(get(colnames(Segments)[x]))) {
            S_ID <- get(colnames(Segments)[x])[i, 1]

            N_ID_1 <- as.numeric(Segments[as.numeric(S_ID + 1), "Node ID #1"])
            Node_1 <- Nodes[as.numeric(N_ID_1 + 1),]

            N_ID_2 <- as.numeric(Segments[as.numeric(S_ID + 1), "Node ID #2"])
            Node_2 <- Nodes[as.numeric(N_ID_2 + 1),]

            if (y_df == 1) {
                if (abs(Node_1["Y Coord"] - y["Y.Coord"]) > abs(Node_2["Y Coord"] - y["Y.Coord"])) {
                    Plus[i, 1:8] <- cbind(
                            Node_1,
                            get(colnames(Segments)[x])[i, "Relative_plus_position"]
                    )

                    Plus[i, 1] <- colnames(Segments)[x]
                    Plus[i, 9] <- as.numeric(Segments[as.numeric(S_ID + 1), "Node ID #1"])
                } else {
                    Plus[i, 1:8] <- cbind(
                            Node_2,
                            get(colnames(Segments)[x])[i, "Relative_plus_position"]
                    )

                    Plus[i, 1] <- colnames(Segments)[x]
                    Plus[i, 9] <- as.numeric(Segments[as.numeric(S_ID + 1), "Node ID #2"])
                }

            } else {
                if (abs(Node_1["Y Coord"] - y["Y.Coord"]) < abs(Node_2["Y Coord"] - y["Y.Coord"])) {
                    Plus[i, 1:8] <- cbind(
                            Node_1,
                            get(colnames(Segments)[x])[i, "Relative_plus_position"]
                    )

                    Plus[i, 1] <- colnames(Segments)[x]
                    Plus[i, 9] <- as.numeric(Segments[as.numeric(S_ID + 1), "Node ID #1"])
                } else {
                    Plus[i, 1:8] <- cbind(
                            Node_2,
                            get(colnames(Segments)[x])[i, "Relative_plus_position"]
                    )

                    Plus[i, 1] <- colnames(Segments)[x]
                    Plus[i, 9] <- as.numeric(Segments[as.numeric(S_ID + 1), "Node ID #2"])
                }

            }

        }
        names(Plus)[1] <- "Fiber"
        names(Plus)[8] <- "Relative_plus_position"
        names(Plus)[9] <- "Node_ID"

        rm(S_ID, N_ID_1, N_ID_2, Node_1, Node_2, y)

        Plus
    }
}

# Analyze the end distribution according to the relative position of the KMT end -----
# X is here no. of column in the "Segment" -------------------------------------------
# Y is a no. of a Pole 1 or 2 --------------------------------------------------------
End_distribution_Minus <- function(x, y) {

    # Function setting -------------------------------------------------------------------
    if (y == 1) {
        y <- Pole1
    } else {
        y <- Pole2
    }

    Minus <- data.frame()

    # For x find Node ID that belong to Segment ID ---------------------------------------
    if (nrow(get(colnames(Segments)[x])) >= 1) {
        for (i in 1:nrow(get(colnames(Segments)[x]))) {
            S_ID <- get(colnames(Segments)[x])[i, 1]

            N_ID_1 <- as.numeric(Segments[as.numeric(S_ID + 1), "Node ID #1"])
            Node_1 <- Nodes[as.numeric(N_ID_1 + 1),]

            N_ID_2 <- as.numeric(Segments[as.numeric(S_ID + 1), "Node ID #2"])
            Node_2 <- Nodes[as.numeric(N_ID_2 + 1),]

            if (abs(Node_1["Y Coord"] - y["Y.Coord"]) < abs(Node_2["Y Coord"] - y["Y.Coord"])) {
                Minus[i, 1:8] <- cbind(
                        Node_1,
                        get(colnames(Segments)[x])[i, "Relative_minus_position"]
                )

                Minus[i, 1] <- colnames(Segments)[x]
                Minus[i, 9] <- as.numeric(Segments[as.numeric(S_ID + 1), "Node ID #1"])
                Minus[i, 10] <- sqrt(
                        (Node_1["X Coord"] - y["X.Coord"])^2 +
                                (Node_1["Y Coord"] - y["Y.Coord"])^2 +
                                (Node_1["Z Coord"] - y["Z.Coord"])^2
                )
            } else {
                Minus[i, 1:8] <- cbind(
                        Node_2,
                        get(colnames(Segments)[x])[i, "Relative_minus_position"]
                )

                Minus[i, 1] <- colnames(Segments)[x]
                Minus[i, 9] <- as.numeric(Segments[as.numeric(S_ID + 1), "Node ID #2"])
                Minus[i, 10] <- sqrt(
                        (Node_2["X Coord"] - y["X.Coord"])^2 +
                                (Node_2["Y Coord"] - y["Y.Coord"])^2 +
                                (Node_2["Z Coord"] - y["Z.Coord"])^2
                )
            }
        }
        names(Minus)[1] <- "Fiber"
        names(Minus)[8] <- "Relative_minus_position"
        names(Minus)[9] <- "Node_ID"
        names(Minus)[10] <- "Minus_end_Distance"

        rm(S_ID, N_ID_1, N_ID_2, Node_1, Node_2, y)

        Minus
    }
}

MT_Ends_Distribution <- function(RP_Pole, kinetochore) {
    df <- tibble()

    for (i in 1:nrow(Segments)) {
        Node_1 <- as.numeric(Segments[i, "Node ID #1"])
        Node_1 <- filter(Nodes, `Node ID` == Node_1)[2:4]
        names(Node_1)[1:3] <- c("X.Coord", "Y.Coord", "Z.Coord")

        Node_2 <- as.numeric(Segments[i, "Node ID #2"])
        Node_2 <- filter(Nodes, `Node ID` == Node_2)[2:4]
        names(Node_2)[1:3] <- c("X.Coord", "Y.Coord", "Z.Coord")
        DF <- tibble()

        # Calculate distance to both poles for each Node_1 and _2
        DF[1, 1] <- as.numeric(dist(rbind(Node_1, Pole1), method = "euclidean")) # Pole1 Node1
        DF[2, 1] <- as.numeric(dist(rbind(Node_1, Pole2), method = "euclidean")) # Pole1 Node2

        DF[3, 1] <- as.numeric(dist(rbind(Node_2, Pole1), method = "euclidean")) # Pole2 Node1
        DF[4, 1] <- as.numeric(dist(rbind(Node_2, Pole2), method = "euclidean")) # Pole2 Node2
        Minus_end <- which.min(DF$...1)
        Distance <- DF[Minus_end, 1]

        # select closest as minus-end
        if (Minus_end == 1 || Minus_end == 3) {
          Pole = 1
          Minus_end_type <- "Pole1"
        }
        if (Minus_end == 2 || Minus_end == 4) {
          Pole = 2
          Minus_end_type <- "Pole2"
        }

        if (Minus_end == 1 || Minus_end == 2) {
            Minus_end <- tibble(
                    Node_1,
                    `Segment ID` = Segments[i, "Node ID #1"]
            )
            Plus_end <- tibble(
                    Node_2,
                    `Segment ID` = Segments[i, "Node ID #2"]
            )
        }
        if (Minus_end == 3 || Minus_end == 4) {
            Minus_end <- tibble(
                    Node_2,
                    `Node ID` = Segments[i, "Node ID #2"]
            )
            Plus_end <- tibble(
                    Node_1,
                    `Node ID` = Segments[i, "Node ID #1"]
            )
        }

        Minus_end_RK <- tibble(Minus_end,
                               Type = Nodes[as.numeric(Minus_end[1, 4] + 1), "EndType_RK"])
        Plus_End_RK <- tibble(Plus_end,
                              Type = Nodes[as.numeric(Plus_end[1, 4] + 1), "EndType_RK"])

        Minus_end_AL <- tibble(Minus_end,
                               Nodes[as.numeric(Minus_end[1, 4] + 1),] %>%
                                       select(starts_with("EndType_AL")))
        names(Minus_end_AL)[ncol(Minus_end_AL)] <- "Type"
        Plus_End_AL <- tibble(Plus_end,
                              Type = Nodes[as.numeric(Plus_end[1, 4] + 1),] %>%
                                      select(starts_with("EndType_AL")))
        names(Plus_End_AL)[ncol(Plus_End_AL)] <- "Type"

        # Define MT class
        MT_Class <- as_tibble(Segments[i,]) %>% filter_at(vars(starts_with("Pole")), any_vars(. > 0))
        if (nrow(MT_Class) != 0) {
            MT_Class <- "KMT"
        } else {
            MT_Class <- "NoN-KMT"
        }

        if (RP_Pole == TRUE) {
          if (Pole1[1, 2] < Pole2[1, 2]) {

              Relative_position <- (Minus_end$Y.Coord - Pole1[1, 2]) / (Pole2[1, 2] - Pole1[1, 2])
          } else {
              Relative_position <- (Minus_end$Y.Coord - Pole2[1, 2]) / (Pole1[1, 2] - Pole2[1, 2])
          }
        } else {
          if (Pole == 1) {
            Relative_position <- (Minus_end$Y.Coord - Pole1[1, 2]) / (kinetochore[1, 2] - Pole1[1, 2])
          } else {
            Relative_position <- (Minus_end$Y.Coord - kinetochore[1, 2]) / (Pole2[1, 2] - kinetochore[1, 2])
          }
        }

        MT <- tibble(
                `Segment ID` = as.numeric(Segments[i, 1]),
                Class = as.character(MT_Class),
                Plus_End_RK = as.numeric(Plus_End_RK[1, "Type"]),
                Minus_end_RK = as.numeric(Minus_end_RK[1, "Type"]),
                Plus_End_AL = as.numeric(Plus_End_AL[1, "Type"]),
                Minus_end_AL = as.numeric(Minus_end_AL[1, "Type"]),
                Minus_RP = as.numeric(Relative_position),
                Distance_to_Poel = as.numeric(Distance),
                Pole_association = as.character(Minus_end_type)
        )

        df <- rbind(df,
                    MT)
    }

    return(df)
}
