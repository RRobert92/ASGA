########################################################################################################
# Tool KMT_Minus_End_Seeds
#
# Function to calculate distribution of a (-) end in close proximity to the KMT
#
# The tool is calculating distance of each point on KMT (p) to every (-) end of SMT and KMT (P_S or P_K)
# Then we associate point_id to segment_id of P_S or P_K which are closer to the p then 55nm
# Multiple same segment_ID are marred to single entrance with their closes distance to the p
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-04-17
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
########################################################################################################

# Function: Calculate minus end distributions ----------------------------------------------------------
Minus_end_seed <- function(x) {
  Minus_end <- data.frame()

  for (i in 1:nrow(get(paste(colnames(Segments)[x])))) {
    DF <- data.frame()

    for (j in 1:nrow(get(paste(colnames(Segments)[x], i, sep = "_")))) {
      tryCatch(
        {
          p_to_P <- Nodes[with(Nodes, `X Coord` <= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 2] + as.numeric(MINUS_DISTANCE * 2)) &
            `X Coord` >= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 2] - as.numeric(MINUS_DISTANCE * 2))), ]
          p_to_P <- p_to_P[with(p_to_P, `Y Coord` <= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 3] + as.numeric(MINUS_DISTANCE * 2)) &
            `Y Coord` >= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 3] - as.numeric(MINUS_DISTANCE * 2))), ]
          p_to_P <- p_to_P[with(p_to_P, `Z Coord` <= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 4] + as.numeric(MINUS_DISTANCE * 2)) &
            `Z Coord` >= as.numeric(get(paste(colnames(Segments)[x], i, sep = "_"))[j, 4] - as.numeric(MINUS_DISTANCE * 2))), ]

          p_to_P[5:7] <- get(paste(colnames(Segments)[x], i, sep = "_"))[j, 2:4]

          p_to_P$dist <- apply(
            p_to_P[2:7],
            1,
            function(y) {
              dist(matrix(y, nrow = 2, byrow = TRUE))
            }
          )
          DF <- data.frame(
            p_to_P[with(p_to_P, dist <= MINUS_DISTANCE & dist >= 0), "Node ID"],
            p_to_P[with(p_to_P, dist <= MINUS_DISTANCE & dist >= 0), "dist"]
          )
        },
        warning = function(w) {}
      )

      if (nrow(DF) > 0) {
        all_end <- data.frame()
        defin_end <- data.frame()

        for (k in 1:nrow(DF)) {
          all_end <- Segments %>% filter_at(
            vars(starts_with("Node")),
            any_vars(. == DF[k, 1])
          )
          defin_end[k, 1:3] <- all_end %>% select(
            `Segment ID`,
            `Node ID #1`,
            `Node ID #2`
          )

          if (defin_end[k, 1] %in% Segments_SMT$`Segment ID`) {
            defin_end[k, 4] <- "SMT"
          } else {
            defin_end[k, 4] <- "KMT"
          }

          if (defin_end[k, 1] == get(paste(colnames(Segments)[x]))[i, 1]) {
            defin_end[k, 1:4] <- NA
            DF[k, 1:2] <- NA
          }
        }
        defin_end <- na.omit(defin_end)
        DF <- na.omit(DF)

        if (nrow(DF) > 0) {
          end_type <- data.frame()
          for (k in 1:nrow(defin_end)) {
            N1 <- Nodes %>% filter_at(vars(starts_with("Node")), any_vars(. == (defin_end[k, 2])))
            N2 <- Nodes %>% filter_at(vars(starts_with("Node")), any_vars(. == (defin_end[k, 3])))

            if (defin_end[k, 4] == "SMT") {
              N1_to_pole1 <- sqrt((Pole1[1, 1] - N1[1, 2])^2 +
                (Pole1[1, 2] - N1[1, 3])^2 +
                (Pole1[1, 3] - N1[1, 4])^2)
              N1_to_pole2 <- sqrt((Pole2[1, 1] - N1[1, 2])^2 +
                (Pole2[1, 2] - N1[1, 3])^2 +
                (Pole2[1, 3] - N1[1, 4])^2)
              N2_to_pole1 <- sqrt((Pole1[1, 1] - N2[1, 2])^2 +
                (Pole1[1, 2] - N2[1, 3])^2 +
                (Pole1[1, 3] - N2[1, 4])^2)
              N2_to_pole2 <- sqrt((Pole2[1, 1] - N2[1, 2])^2 +
                (Pole2[1, 2] - N2[1, 3])^2 +
                (Pole2[1, 3] - N2[1, 4])^2)

              Node_to_Pole <- rbind(
                N1_to_pole1,
                N1_to_pole2,
                N2_to_pole1,
                N2_to_pole2
              )
              if (which.min(as.matrix(Node_to_Pole)) == 1) {
                end_type[k, 1] <- "Minus"
                end_type[k, 2] <- "Plus"
              } else if (which.min(as.matrix(Node_to_Pole)) == 2) {
                end_type[k, 1] <- "Minus"
                end_type[k, 2] <- "Plus"
              } else if (which.min(as.matrix(Node_to_Pole)) == 3) {
                end_type[k, 1] <- "Plus"
                end_type[k, 2] <- "Minus"
              } else if (which.min(as.matrix(Node_to_Pole)) == 4) {
                end_type[k, 1] <- "Plus"
                end_type[k, 2] <- "Minus"
              } else {
                end_type[k, 1:2] <- NA
              }
            } else if (defin_end[k, 4] == "KMT") {
              if (nrow(Segments[as.numeric(defin_end[k, 1] + 1), ] %>% filter_at(vars(starts_with("Pole1")), any_vars(. == 1))) == 1) {

                # calculate (-) end for Pole 1 -------------------------------------------------------------------------
                N1_to_pole1 <- sqrt((Pole1[1, 1] - Nodes[as.numeric(defin_end[k, 2] + 1), 2])^2 +
                  (Pole1[1, 2] - Nodes[as.numeric(defin_end[k, 2] + 1), 3])^2 +
                  (Pole1[1, 3] - Nodes[as.numeric(defin_end[k, 2] + 1), 4])^2)
                N2_to_pole1 <- sqrt((Pole1[1, 1] - Nodes[as.numeric(defin_end[k, 3] + 1), 2])^2 +
                  (Pole1[1, 2] - Nodes[as.numeric(defin_end[k, 3] + 1), 3])^2 +
                  (Pole1[1, 3] - Nodes[as.numeric(defin_end[k, 3] + 1), 4])^2)

                Node_to_Pole <- rbind(N1_to_pole1, N2_to_pole1)

                if (which.min(as.matrix(Node_to_Pole)) == 1 && which.max(as.matrix(Node_to_Pole)) == 2) {
                  end_type[k, 1] <- "Minus"
                  end_type[k, 2] <- "Plus"
                } else if (which.min(as.matrix(Node_to_Pole)) == 2 && which.max(as.matrix(Node_to_Pole)) == 1) {
                  end_type[k, 1] <- "Plus"
                  end_type[k, 2] <- "Minus"
                }
              } else if (nrow(Segments[as.numeric(defin_end[k, 1] + 1), ] %>% filter_at(vars(starts_with("Pole2")), any_vars(. == 1))) == 1) {

                # calculate (-) end for Pole 2 -------------------------------------------------------------------------
                N1_to_pole2 <- sqrt((Pole2[1, 1] - Nodes[as.numeric(defin_end[k, 2] + 1), 2])^2 +
                  (Pole2[1, 2] - Nodes[as.numeric(defin_end[k, 2] + 1), 3])^2 +
                  (Pole2[1, 3] - Nodes[as.numeric(defin_end[k, 2] + 1), 4])^2)
                N2_to_pole2 <- sqrt((Pole2[1, 1] - Nodes[as.numeric(defin_end[k, 3] + 1), 2])^2 +
                  (Pole2[1, 2] - Nodes[as.numeric(defin_end[k, 3] + 1), 3])^2 +
                  (Pole2[1, 3] - Nodes[as.numeric(defin_end[k, 3] + 1), 4])^2)

                Node_to_Pole <- rbind(N1_to_pole2, N2_to_pole2)

                if (which.min(as.matrix(Node_to_Pole)) == 1 && which.max(as.matrix(Node_to_Pole)) == 2) {
                  end_type[k, 1] <- "Minus"
                  end_type[k, 2] <- "Plus"
                } else if (which.min(as.matrix(Node_to_Pole)) == 2 && which.max(as.matrix(Node_to_Pole)) == 1) {
                  end_type[k, 1] <- "Plus"
                  end_type[k, 2] <- "Minus"
                }
              }
            }
          }
          defin_end <- cbind(defin_end, end_type)

          for (k in 1:nrow(DF)) {
            if (is.na(defin_end[k, 5]) || is.na(defin_end[k, 6])) {
              defin_end[k, 1:6] <- NA
              DF[k, 1:2] <- NA
            } else {
              if (defin_end[k, 2] == DF[k, 1] && defin_end[k, 5] == "Plus" || defin_end[k, 3] == DF[k, 1] && defin_end[k, 6] == "Plus") {
                defin_end[k, 1:6] <- NA
                DF[k, 1:2] <- NA
              }
            }
          }
          defin_end <- na.omit(defin_end)
          DF <- na.omit(DF)
        }

        if (nrow(DF) > 0) {
          defin_end <- cbind(
            paste(colnames(Segments)[x], i, sep = "_"),
            defin_end[1],
            defin_end[4],
            round(DF[2], 4),
            get(paste(colnames(Segments)[x], i, sep = "_"))[j, 5],
            get(paste(colnames(Segments)[x]))[1, 5],
            get(paste(colnames(Segments)[x]))[1, 6],
            KMT_ends_density(
              get(paste(colnames(Segments)[x], i, sep = "_"))[j, 2:4],
              0.1, "point"
            )
          )
          names(defin_end)[1] <- "KMT_ID"
          names(defin_end)[2] <- "Interactor_ID"
          names(defin_end)[3] <- "I_class"
          names(defin_end)[4] <- "p_to_P_dist"
          names(defin_end)[5] <- "Relative_pos"
          names(defin_end)[6] <- "Plus_end_dist"
          names(defin_end)[7] <- "Ellipse"
          names(defin_end)[8] <- "Density"

          Minus_end <- rbind(Minus_end, defin_end)
        }
      }
    }

    list <- unique(Minus_end$Interactor_ID)
    Temp <- data.frame()
    idx <- 1
    for (k in list) {
      DF <- Minus_end %>% filter_at(vars("Interactor_ID"), any_vars(. == k))
      Temp[idx, 1:8] <- DF[which.min(DF$p_to_P_dist), 1:8]
      idx <- idx + 1
    }

    Minus_end <- na.omit(Temp)
  }
  Minus_end
}

KMT_Minus_End_Interaction <- function(x) {
  DF <- tibble(
    KMT_ID = as.numeric(),
    Interaction_ID = as.character(),
    KMT_Minus_Distance = as.numeric(),
    MT_type = as.character(),
    MT_distance = as.numeric()
  )

  Node_1 <- as.numeric(Segments_KMT[x, "Node ID #1"])
  Node_1 <- dplyr::filter(Nodes, `Node ID` == Node_1)[2:4]
  names(Node_1)[1:3] <- c("X.Coord", "Y.Coord", "Z.Coord")

  Node_2 <- as.numeric(Segments_KMT[x, "Node ID #2"])
  Node_2 <- dplyr::filter(Nodes, `Node ID` == Node_2)[2:4]
  names(Node_2)[1:3] <- c("X.Coord", "Y.Coord", "Z.Coord")
  End_Type <- tibble()

  # Calculate distance to both poles for each Node_1 and _2
  End_Type[1, 1] <- as.numeric(dist(rbind(Node_1, Pole1), method = "euclidean")) # Pole1 Node1
  End_Type[2, 1] <- as.numeric(dist(rbind(Node_1, Pole2), method = "euclidean")) # Pole1 Node2

  End_Type[3, 1] <- as.numeric(dist(rbind(Node_2, Pole1), method = "euclidean")) # Pole2 Node1
  End_Type[4, 1] <- as.numeric(dist(rbind(Node_2, Pole2), method = "euclidean")) # Pole2 Node2
  Minus_end <- which.min(End_Type$...1)
  Dist <- End_Type[Minus_end, 1]

  # select closest as minus-end
  if (Minus_end == 1 || Minus_end == 2) {
    if (Minus_end == 1) {
      Relative_Position <- (Node_1[2] - Pole1[2]) / (Node_2[2] - Pole1[2])
    }
    if (Minus_end == 2) {
      Relative_Position <- (Node_1[2] - Pole2[2]) / (Node_2[2] - Pole2[2])
    }
    Minus_end <- tibble(
      Node_1,
      KMT_ID = Segments_KMT[x, "Segment ID"],
      ID = Segments_KMT[x, "Node ID #1"],
      Distance = Dist[1, "...1"],
      Relative_Position = Relative_Position[1, "Y.Coord"]
    )
  }
  if (Minus_end == 3 || Minus_end == 4) {
    if (Minus_end == 3) {
      Relative_Position <- (Node_2[2] - Pole1[2]) / (Node_1[2] - Pole1[2])
    }
    if (Minus_end == 4) {
      Relative_Position <- (Node_2[2] - Pole2[2]) / (Node_1[2] - Pole2[2])
    }
    Minus_end <- tibble(
      Node_2,
      KMT_ID = Segments_KMT[x, "Segment ID"],
      ID = Segments_KMT[x, "Node ID #2"],
      Distance = Dist[1, "...1"],
      Relative_Position = Relative_Position[1, "Y.Coord"]
    )
  }

  # Check it there are MT around the KMT minus end
  p_to_P <- Points[with(Points, `X Coord` <= as.numeric(Minus_end[1, 1] + ((MINUS_DISTANCE * 2))) &
    `X Coord` >= as.numeric(Minus_end[1, 1] - ((MINUS_DISTANCE * 2)))), ]
  p_to_P <- p_to_P[with(p_to_P, `Y Coord` <= as.numeric(Minus_end[1, 2] + ((MINUS_DISTANCE * 2))) &
    `Y Coord` >= as.numeric(Minus_end[1, 2] - ((MINUS_DISTANCE * 2)))), ]
  p_to_P <- p_to_P[with(p_to_P, `Z Coord` <= as.numeric(Minus_end[1, 3] + ((MINUS_DISTANCE * 2))) &
    `Z Coord` >= as.numeric(Minus_end[1, 3] - ((MINUS_DISTANCE * 2)))), ]

  p_to_P[5:7] <- Minus_end[1, 1:3]

  p_to_P$dist <- apply(
    p_to_P[2:7],
    1,
    function(y) {
      dist(matrix(y, nrow = 2, byrow = TRUE))
    }
  )

  p_to_P <- data.frame(
    p_to_P[with(p_to_P, dist <= MINUS_DISTANCE & dist > 0), "Point_ID"],
    p_to_P[with(p_to_P, dist <= MINUS_DISTANCE & dist > 0), "dist"]
  )

  # Find type of MT e.g SMT or KMT
  for (i in 1:nrow(p_to_P)) {
    if_KMTs <- TRUE
    counter <- 1

    while (if_KMTs) {
      df <- stringr::str_split(Segments_KMT[counter, "Point IDs"], pattern = ",")
      if_KMTs <- as.numeric(table(as.numeric(df[[1]]) == as.numeric(p_to_P[i, 1]))[TRUE][2])

      if (!is.na(if_KMTs)) {
        if_KMTs <- FALSE
        p_to_P[i, 3] <- "KMT"
        p_to_P[i, 4] <- Segments_KMT[counter, "Segment ID"]
      } else {
        if_KMTs <- TRUE
        p_to_P[i, 3] <- "SMT"
        p_to_P[i, 4] <- 99999
        counter <- counter + 1
      }

      if (counter == nrow(Segments_KMT)) {
        if_KMTs <- FALSE
        p_to_P[i, 3] <- "SMT"
        p_to_P[i, 4] <- 99999
      }
    }
  }

  for (i in 1:nrow(p_to_P)) {
    if (Minus_end$KMT_ID == if (p_to_P[i, 4] == 99999) {
      FALSE
    } else {
      p_to_P[i, 4]
    }) {
      p_to_P[i, 1:4] <- NA
    }
  }
  p_to_P <- na.omit(p_to_P)

  if (nrow(p_to_P) > 0) {
    p_to_P <- p_to_P[which.min(as.matrix(p_to_P[2])), 1:4]

    j <- which.min(as.matrix(abs(LENGTH_ESTIMATION_DF[1] - p_to_P[1, 1]))) - 5

    if (j < 1) {
      j <- 1
    }

    test_condition <- TRUE
    while (test_condition == TRUE) {
      if (0 < gregexpr(paste(",", as.character(p_to_P[1, 1]), ",", sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1] ||
        0 < gregexpr(paste(as.character(p_to_P[1, 1]), ",", sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1] ||
        0 < gregexpr(paste(",", as.character(p_to_P[1, 1]), sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1]) {
        test_condition <- FALSE
      } else {
        j <- j + 1
      }
    }

    Segment_ID_DF <- j - 1

    DF <- tibble(
      KMT_ID = as.numeric(Segments_KMT[x, "Segment ID"]),
      Interaction_ID = as.character(Segment_ID_DF),
      KMT_Minus_Distance = as.numeric(Minus_end$Distance),
      MT_type = as.character(p_to_P[3]),
      MT_distance = as.numeric(p_to_P[2]),
      Relative_position = as.numeric(Minus_end$Relative_Position),
      Density = KMT_ends_density(
        Minus_end[1, 1:3],
        0.1, "point"
      )
    )
    return(DF)
  } else {
    DF <- tibble(
      KMT_ID = as.numeric(Segments_KMT[x, "Segment ID"]),
      Interaction_ID = as.character(NaN),
      KMT_Minus_Distance = as.numeric(Minus_end$Distance),
      MT_type = as.character(NaN),
      MT_distance = as.numeric(NaN),
      Relative_position = as.numeric(Minus_end$Relative_Position),
      Density = as.numeric(NaN)
    )
    return(DF)
  }
}
