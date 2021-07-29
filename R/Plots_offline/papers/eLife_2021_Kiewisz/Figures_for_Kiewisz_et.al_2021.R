###############################################################################
# Plots for eLife paper                                                       #
#                                                                             #
# (c) 2019-2021 Kiewisz                                                       #
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)  #
#                                                                             #
# Author: Robert Kiewisz                                                      #
# Created: 2021-07-13                                                         #
###############################################################################

###############################################################################
#                        Import all data using ASGA                           #
###############################################################################

source("bin/Utility/Library.R")
# Data as produced and imported from ASGA software v0.34 or newer

###############################################################################
#                                Figure 3B                                    #
###############################################################################
# Combined data for average #
No_KMTs_avg <- rbind(
  Data_1_KMT_No,
  Data_2_KMT_No,
  Data_3_KMT_No
)

# Plot #
ggplot(No_KMTs_avg, aes(KMTs_per_kinetochore)) +
  geom_histogram(bins = 12) +
  theme_classic() +
  ylab("Frequency") +
  xlab("No. of KMTs per kinetochore")

# Analysis of mean +/- STD #
Avg_KMT_no <- round(mean(No_KMTs_avg$KMTs_per_kinetochore), 1)
print(Avg_KMT_no)

Avg_KMT_no <- round(sd(No_KMTs_avg$KMTs_per_kinetochore), 1)
print(Avg_KMT_no)

###############################################################################
#                                Figure 3C                                    #
###############################################################################
# Combined data for average #
IKD_KMT_avg <- rbind(
  Data_1_IKD_KMT_No,
  Data_2_IKD_KMT_No,
  Data_3_IKD_KMT_No
)

# Plot #
IKD_KMT_NO <- ggplot(Data_1_IKD_KMT_No, aes(`Inter-kinetochore distance`, `KMTs no.`)) +
  geom_point(color = "brown1", shape = 15, size = 2) +
  theme_classic() +
  xlim(0.5, 1.75)
IKD_KMT_NO <- IKD_KMT_NO +
  geom_point(
    data = Data_2_IKD_KMT_No, aes(`Inter-kinetochore distance`, `KMTs no.`),
    color = "brown3", shape = 16, size = 2
  )
IKD_KMT_NO <- IKD_KMT_NO +
  geom_point(
    data = Data_3_IKD_KMT_No, aes(`Inter-kinetochore distance`, `KMTs no.`),
    color = "brown4", shape = 17, size = 2
  )

print(IKD_KMT_NO)

# Analysis of correlations #
Correlation <- tibble(
  Data_1 = cor(
    Data_1_IKD_KMT_No$`Inter-kinetochore distance`,
    Data_1_IKD_KMT_No$`KMTs no.`
  ),
  Data_2 = cor(
    Data_2_IKD_KMT_No$`Inter-kinetochore distance`,
    Data_2_IKD_KMT_No$`KMTs no.`
  ),
  Data_3 = cor(
    Data_3_IKD_KMT_No$`Inter-kinetochore distance`,
    Data_3_IKD_KMT_No$`KMTs no.`
  ),
  AVG = cor(
    IKD_KMT_avg$`Inter-kinetochore distance`,
    IKD_KMT_avg$`KMTs no.`
  )
)

print(Correlation)

###############################################################################
#                                Figure 3D                                    #
###############################################################################
# Combined data for average #
IKD_dekta_avg <- rbind(
  Data_1_IKD_KMT_Delta,
  Data_2_IKD_KMT_Delta,
  Data_3_IKD_KMT_Delta
)

# Plot #
IKD_delta_NO <- ggplot(Data_1_IKD_KMT_Delta, aes(`Inter-kinetochore distance`, `Delta of KMTs`)) +
  geom_point(color = "brown1", shape = 15, size = 2) +
  theme_classic() +
  xlim(0.5, 1.75)
IKD_delta_NO <- IKD_delta_NO +
  geom_point(
    data = Data_2_IKD_KMT_Delta, aes(`Inter-kinetochore distance`, `Delta of KMTs`),
    color = "brown3", shape = 16, size = 2
  )
IKD_delta_NO <- IKD_delta_NO +
  geom_point(
    data = Data_3_IKD_KMT_Delta, aes(`Inter-kinetochore distance`, `Delta of KMTs`),
    color = "brown4", shape = 17, size = 2
  )

print(IKD_delta_NO)

# Analysis of correlations #
Correlation <- tibble(
  Data_1 = cor(
    Data_1_IKD_KMT_Delta$`Inter-kinetochore distance`,
    Data_1_IKD_KMT_Delta$`Delta of KMTs`
  ),
  Data_2 = cor(
    Data_2_IKD_KMT_Delta$`Inter-kinetochore distance`,
    Data_2_IKD_KMT_Delta$`Delta of KMTs`
  ),
  Data_3 = cor(
    Data_3_IKD_KMT_Delta$`Inter-kinetochore distance`,
    Data_3_IKD_KMT_Delta$`Delta of KMTs`
  ),
  AVG = cor(
    IKD_dekta_avg$`Inter-kinetochore distance`,
    IKD_dekta_avg$`Delta of KMTs`
  )
)

print(Correlation)

###############################################################################
#                                Figure 4B                                    #
###############################################################################
# Analysis of Centrosome interaction area aka CIA #
Data_1 <- ggplot_build(ggplot(Data_1_SMT_Ends, aes(Distance_to_Pole)) +
  geom_density())$data[[1]]
Data_1 <- as.numeric(FWHM(Data_1$x, Data_1$y, 2)[2] / 2)

Data_2 <- ggplot_build(ggplot(Data_1_SMT_Ends, aes(Distance_to_Pole)) +
  geom_density())$data[[1]]
# Data show double pick to account for that FWHM is taken only for the first peak
y_max <- Data_2[which.max(Data_2$y), 1] / 1.2
Data_2 <- Data_2[as.numeric(which.max(Data_2$y) + 1):nrow(Data_2), ]
Data_2 <- as.numeric(Data_2[which(abs(Data_2$y - y_max) == min(abs(Data_2$y - y_max))), ]$x)

Data_3 <- ggplot_build(ggplot(Data_1_SMT_Ends, aes(Distance_to_Pole)) +
  geom_density())$data[[1]]
Data_3 <- as.numeric(FWHM(Data_3$x, Data_3$y, 2)[2] / 2)

# Centrosome Interaction Area for all data sets #
SMTs_avg <- rbind(Data_1_SMT_Ends, Data_2_SMT_Ends, Data_3_SMT_Ends)
Data_avg <- ggplot_build(ggplot(SMTs_avg, aes(Distance_to_Pole)) +
  geom_density())$data[[1]]

Data_FHWM <- as.numeric(FWHM(Data_avg$x, Data_avg$y, 1))
CIA <- (Data_FHWM[1] * 2) - Data_avg[1, 2]

print(CIA)

###############################################################################
#                                Figure 4C                                    #
###############################################################################
# Combined data for average #
LD_avg <- rbind(
  Data_1_LD,
  Data_2_LD,
  Data_3_LD
)

# Plot #
ggplot(LD_avg, aes(length)) +
  geom_histogram(bins = 30) +
  theme_classic() +
  ylab("No. of KMTs") +
  xlab("Length [um]")

# Analysis of mean +/- STD #
Avg_LD <- round(mean(LD_avg$length), 2)
print(Avg_LD)

Avg_LD <- round(sd(LD_avg$length), 2)
print(Avg_LD)

# Analysis of KMT length longer then 2 um #
mean_LD <- mean(c(
  nrow(filter(Data_1_LD, length <= 2)) / nrow(Data_1_LD),
  nrow(filter(Data_2_LD, length <= 2)) / nrow(Data_2_LD),
  nrow(filter(Data_3_LD, length <= 2)) / nrow(Data_3_LD)
))
print(round(mean_LD * 100, 1))
sd <- sd(c(
  nrow(filter(Data_1_LD, length <= 2)) / nrow(Data_1_LD),
  nrow(filter(Data_2_LD, length <= 2)) / nrow(Data_2_LD),
  nrow(filter(Data_3_LD, length <= 2)) / nrow(Data_3_LD)
))
print(round(sd * 100, 1))

# Analysis of KMT longer then half spindle length #
PTP <- tibble(
  X_Coord_P1 = c(51385.63281, 52571.19531, 80278.8125),
  Y_Coord_P1 = c(18100.44141, 113530.8203, 81406.53906),
  Z_Coord_P1 = c(25617.02148, 28686.98242, 17718.85156),
  X_Coord_P2 = c(51282.79297, 52565.24609, 80279.78906),
  Y_Coord_P2 = c(89695.32031, 9660.630859, -13390.78125),
  Z_Coord_P2 = c(24732.59766, 28693.30469, 17719.81055)
) / 10000
PTP$distance <- apply(
  PTP, 1,
  function(x) dist(matrix(x, nrow = 2, byrow = TRUE))
)

mean_LD <- mean(c(
  nrow(filter(Data_1_LD, length > PTP$distance[1] / 2)) / nrow(Data_1_LD),
  nrow(filter(Data_2_LD, length > PTP$distance[2] / 2)) / nrow(Data_2_LD),
  nrow(filter(Data_3_LD, length > PTP$distance[3] / 2)) / nrow(Data_3_LD)
))
print(round(mean_LD * 100, 1))
sd <- sd(c(
  nrow(filter(Data_1_LD, length > PTP$distance[1] / 2)) / nrow(Data_1_LD),
  nrow(filter(Data_2_LD, length > PTP$distance[2] / 2)) / nrow(Data_2_LD),
  nrow(filter(Data_3_LD, length > PTP$distance[3] / 2)) / nrow(Data_3_LD)
))
print(round(sd * 100, 1))

###############################################################################
#                                Figure 4D                                    #
###############################################################################
# Combined data for average #
LD_avg <- rbind(
  Data_1_LD,
  Data_2_LD,
  Data_3_LD
)

# Plot #
ggplot(LD_avg, aes(minus_dist_to_pole)) +
  geom_histogram(bins = 30) +
  theme_classic() +
  ylab("No. of KMTs") +
  xlab("Minus ends distance to the pole [um]")

# Analysis of KMT distribution at CIA #
mean_LD <- mean(c(
  (nrow(filter(Data_1_LD, minus_dist_to_pole <= CIA)) / nrow(Data_1_LD)) * 100,
  (nrow(filter(Data_2_LD, minus_dist_to_pole <= CIA)) / nrow(Data_2_LD)) * 100,
  (nrow(filter(Data_3_LD, minus_dist_to_pole <= CIA)) / nrow(Data_3_LD)) * 100
))
print(round(mean_LD, 1))
sd <- sd(c(
  (nrow(filter(Data_1_LD, minus_dist_to_pole <= CIA)) / nrow(Data_1_LD)) * 100,
  (nrow(filter(Data_2_LD, minus_dist_to_pole <= CIA)) / nrow(Data_2_LD)) * 100,
  (nrow(filter(Data_3_LD, minus_dist_to_pole <= CIA)) / nrow(Data_3_LD)) * 100
))
print(round(sd, 1))

###############################################################################
#                                Figure 4E                                    #
###############################################################################
# Combined data for average #
LD_avg <- rbind(
  Data_1_KMTs_minus_seed_0.1,
  Data_2_KMTs_minus_seed_0.1,
  Data_3_KMTs_minus_seed_0.1
)

# Plot #
ggplot(LD_avg, aes(Relative_pos)) +
  geom_histogram(bins = 30) +
  theme_classic() +
  ylab("No. of KMTs") +
  xlab("Minus ends distance to the pole [um]")

# Analysis of KMT distribution at CIA #
mean_LD <- mean(c(
  (nrow(filter(Data_1_KMTs_minus_seed_0.1, Relative_pos <= 0.2)) /
    nrow(Data_1_KMTs_minus_seed_0.1)) * 100,
  (nrow(filter(Data_2_KMTs_minus_seed_0.1, Relative_pos <= 0.2)) /
    nrow(Data_2_KMTs_minus_seed_0.1)) * 100,
  (nrow(filter(Data_3_KMTs_minus_seed_0.1, Relative_pos <= 0.2)) /
    nrow(Data_3_KMTs_minus_seed_0.1)) * 100
))
print(round(mean_LD, 2))
sd <- sd(c(
  (nrow(filter(Data_1_KMTs_minus_seed_0.1, Relative_pos <= 0.2)) /
    nrow(Data_1_KMTs_minus_seed_0.1)) * 100,
  (nrow(filter(Data_2_KMTs_minus_seed_0.1, Relative_pos <= 0.2)) /
    nrow(Data_2_KMTs_minus_seed_0.1)) * 100,
  (nrow(filter(Data_3_KMTs_minus_seed_0.1, Relative_pos <= 0.2)) /
    nrow(Data_3_KMTs_minus_seed_0.1)) * 100
))
print(round(sd, 2))

###############################################################################
#                                Figure 4F                                    #
###############################################################################
# Combined data for average #
LD_non_KMTs_avg <- rbind(
  Data_1_SMT_Ends,
  Data_1_SMT_Ends,
  Data_1_SMT_Ends
)

# Plot #
ggplot(LD_non_KMTs_avg, aes(Length)) +
  geom_histogram(bins = 30) +
  theme_classic() +
  ylab("No. of KMTs") +
  xlab("Length [um]")

# Analysis of mean +/- STD #
Avg_LD <- round(mean(LD_non_KMTs_avg$Length), 2)
print(Avg_LD)

Avg_LD <- round(sd(LD_non_KMTs_avg$Length), 2)
print(Avg_LD)

###############################################################################
#                                Figure 4G                                    #
###############################################################################
# Combined data for average #
LD_non_KMTs_avg <- rbind(
  Data_1_SMT_Ends,
  Data_2_SMT_Ends,
  Data_3_SMT_Ends
)

# Plot #
ggplot(LD_non_KMTs_avg, aes(Distance_to_Pole)) +
  geom_histogram(bins = 30) +
  theme_classic() +
  ylab("No. of KMTs") +
  xlab("Minus ends distance to the pole [um]")


# Analysis of KMT distribution at CIA #
mean_LD <- mean(c(
  (nrow(filter(Data_1_SMT_Ends, Distance_to_Pole <= CIA)) /
    nrow(Data_1_SMT_Ends)) * 100,
  (nrow(filter(Data_2_SMT_Ends, Distance_to_Pole <= CIA)) /
    nrow(Data_2_SMT_Ends)) * 100,
  (nrow(filter(Data_3_SMT_Ends, Distance_to_Pole <= CIA)) /
    nrow(Data_3_SMT_Ends)) * 100
))
print(round(mean_LD, 2))
sd <- sd(c(
  (nrow(filter(Data_1_SMT_Ends, Distance_to_Pole <= CIA)) /
    nrow(Data_1_SMT_Ends)) * 100,
  (nrow(filter(Data_2_SMT_Ends, Distance_to_Pole <= CIA)) /
    nrow(Data_2_SMT_Ends)) * 100,
  (nrow(filter(Data_3_SMT_Ends, Distance_to_Pole <= CIA)) /
    nrow(Data_3_SMT_Ends)) * 100
))
print(round(sd, 2))

###############################################################################
#                                Figure 4H                                    #
###############################################################################
# Combined data for average #
LD_non_KMTs_avg <- rbind(
  Data_1_SMT_Ends,
  Data_2_SMT_Ends,
  Data_3_SMT_Ends
)

# Plot #
ggplot(LD_non_KMTs_avg, aes(Relativ_Position)) +
  geom_histogram(bins = 30) +
  theme_classic() +
  ylab("No. of KMTs") +
  xlab("Minus ends distance to the pole [um]")


###############################################################################
#                                Figure 5F                                    #
###############################################################################
# Combined data for average #
Tortuosity_avg <- rbind(
  Data_1_KMT_Total_Curv,
  Data_2_KMT_Total_Curv,
  Data_3_KMT_Total_Curv
)

# Plot #
ggplot(Tortuosity_avg, aes(Curvature)) +
  geom_histogram(bins = 30) +
  theme_classic() +
  ylab("No. of KMTs") +
  xlab("Tortuosity of KMTs")

# Analysis of average KMT tortuosity #
print(round(mean(Tortuosity_avg$Curvature), 2))
print(round(sd(Tortuosity_avg$Curvature), 2))

# Analysis of KMT that had tortuosity greater then 1.1 #
mean_LD <- mean(c(
  (nrow(filter(Data_1_KMT_Total_Curv, Curvature > 1.1)) / nrow(Data_1_KMT_Total_Curv)) * 100,
  (nrow(filter(Data_2_KMT_Total_Curv, Curvature > 1.1)) / nrow(Data_2_KMT_Total_Curv)) * 100,
  (nrow(filter(Data_3_KMT_Total_Curv, Curvature > 1.1)) / nrow(Data_3_KMT_Total_Curv)) * 100
))
print(round(mean_LD, 2))
sd <- sd(c(
  (nrow(filter(Data_1_KMT_Total_Curv, Curvature > 1.1)) / nrow(Data_1_KMT_Total_Curv)) * 100,
  (nrow(filter(Data_2_KMT_Total_Curv, Curvature > 1.1)) / nrow(Data_2_KMT_Total_Curv)) * 100,
  (nrow(filter(Data_3_KMT_Total_Curv, Curvature > 1.1)) / nrow(Data_3_KMT_Total_Curv)) * 100
))
print(round(sd, 2))

###############################################################################
#                                Figure 5G                                    #
###############################################################################
# Combined data for average #
Tortuosity_avg <- rbind(
  Data_1_KMT_Total_Curv,
  Data_2_KMT_Total_Curv,
  Data_3_KMT_Total_Curv
)

# Plot #
Tortuosity_length <- ggplot(Data_1_KMT_Total_Curv, aes(`KMTs length`, Curvature)) +
  geom_jitter(shape = 15, color = "brown1") +
  theme_classic()
Tortuosity_length <- Tortuosity_length +
  geom_point(
    data = Data_2_KMT_Total_Curv, aes(`KMTs length`, Curvature),
    shape = 16, color = "brown3"
  )
Tortuosity_length <- Tortuosity_length +
  geom_point(
    data = Data_3_KMT_Total_Curv, aes(`KMTs length`, Curvature),
    shape = 17, color = "brown4"
  )
Tortuosity_length <- Tortuosity_length +
  geom_smooth(
    data = Tortuosity_avg, aes(`KMTs length`, Curvature),
    se = F, color = "grey20", size = 2, method = "loess"
  )

print(Tortuosity_length)

# Analysis of correlation #
Correlation <- tibble(
  Data_1 = cor(
    Data_1_KMT_Total_Curv$`Curvature`,
    Data_1_KMT_Total_Curv$`KMTs length`
  ),
  Data_2 = cor(
    Data_2_KMT_Total_Curv$`Curvature`,
    Data_2_KMT_Total_Curv$`KMTs length`
  ),
  Data_3 = cor(
    Data_3_KMT_Total_Curv$`Curvature`,
    Data_3_KMT_Total_Curv$`KMTs length`
  ),
  AVG = cor(
    Tortuosity_avg$Curvature,
    Tortuosity_avg$`KMTs length`
  )
)
print(Correlation)

# Analysis of KMT longer then half spindle length #
PTP <- tibble(
    X_Coord_P1 = c(51385.63281, 52571.19531, 80278.8125),
    Y_Coord_P1 = c(18100.44141, 113530.8203, 81406.53906),
    Z_Coord_P1 = c(25617.02148, 28686.98242, 17718.85156),
    X_Coord_P2 = c(51282.79297, 52565.24609, 80279.78906),
    Y_Coord_P2 = c(89695.32031, 9660.630859, -13390.78125),
    Z_Coord_P2 = c(24732.59766, 28693.30469, 17719.81055)
) / 10000
PTP$distance <- apply(
    PTP, 1,
    function(x) dist(matrix(x, nrow = 2, byrow = TRUE))
) / 2

Tortuosity_of_long <- tibble(
    Ratio = c(
        nrow(filter(Data_1_KMT_Total_Curv, `KMTs length` >= PTP$distance[1] & `Curvature` >= 1.1)) /
            nrow(filter(Data_1_KMT_Total_Curv, `KMTs length` >= PTP$distance[1])),
        nrow(filter(Data_2_KMT_Total_Curv, `KMTs length` >= PTP$distance[2] & `Curvature` >= 1.1)) /
            nrow(filter(Data_2_KMT_Total_Curv, `KMTs length` >= PTP$distance[2])),
        nrow(filter(Data_3_KMT_Total_Curv, `KMTs length` >= PTP$distance[3] & `Curvature` >= 1.1)) /
            nrow(filter(Data_3_KMT_Total_Curv, `KMTs length` >= PTP$distance[3]))
    )
)
paste(round(mean(Tortuosity_of_long$Ratio), 2),
      "+/-",
      round(sd(Tortuosity_of_long$Ratio), 2),
      sep = " ")

###############################################################################
#                                Figure 5I                                    #
###############################################################################
# Combined data for average #
Tortuosity_local_avg <- rbind(
  Data_1_KMT_Local_Curv,
  Data_2_KMT_Local_Curv,
  Data_3_KMT_Local_Curv
)

# Plot #
Tortuosity_local <- ggplot(Data_1_KMT_Local_Curv, aes(Relative_Position, Curvature)) +
  geom_jitter(shape = 15, color = "brown1") +
  theme_classic() +
  ylim(1, 1.1)
Tortuosity_local <- Tortuosity_local +
  geom_point(
    data = Data_2_KMT_Local_Curv, aes(Relative_Position, Curvature),
    shape = 16, color = "brown3"
  )
Tortuosity_local <- Tortuosity_local +
  geom_point(
    data = Data_3_KMT_Local_Curv, aes(Relative_Position, Curvature),
    shape = 17, color = "brown4"
  )
Tortuosity_local <- Tortuosity_local +
  geom_smooth(
    data = Tortuosity_local_avg, aes(Relative_Position, Curvature),
    se = F, color = "grey20", size = 2, method = "loess"
  )

print(Tortuosity_local)

# Analysis of correlation #
Correlation <- tibble(
  Data_1 = cor(
    Data_1_KMT_Local_Curv$`Curvature`,
    Data_1_KMT_Local_Curv$Relative_Position
  ),
  Data_2 = cor(
    Data_2_KMT_Local_Curv$`Curvature`,
    Data_2_KMT_Local_Curv$Relative_Position
  ),
  Data_3 = cor(
    Data_3_KMT_Local_Curv$`Curvature`,
    Data_3_KMT_Local_Curv$Relative_Position
  ),
  AVG = cor(
    Tortuosity_local_avg$Curvature,
    Tortuosity_local_avg$Relative_Position
  )
)
print(Correlation)

###############################################################################
#                                Figure 6B                                    #
###############################################################################
# Combined data for average #
Area_avg <- rbind(
  Data_1_Fiber_Area,
  Data_2_Fiber_Area,
  Data_3_Fiber_Area
)

# Plot #
Fiber_area <- ggplot(Data_1_Fiber_Area, aes(Relative_position, Alpha_area)) +
  geom_smooth(color = "brown1", se = F, method = "loess", formula = "y~x") +
  theme_classic()

Fiber_area <- Fiber_area +
  geom_smooth(
    data = Data_2_Fiber_Area, aes(Relative_position, Alpha_area),
    color = "brown3", se = F, method = "loess", formula = "y~x"
  )

Fiber_area <- Fiber_area +
  geom_smooth(
    data = Data_3_Fiber_Area, aes(Relative_position, Alpha_area),
    color = "brown4", se = F, method = "loess", formula = "y~x"
  )

Fiber_area <- Fiber_area +
  geom_smooth(
    data = Area_avg, aes(Relative_position, Alpha_area),
    color = "grey20", se = T, method = "loess", formula = "y~x"
  )

print(Fiber_area)

# Analysis of KMT that had tortuosity greater then 1.1 #
print(round(mean(Area_avg$Alpha_area), 2))
print(round(sd(Area_avg$Alpha_area), 2))

###############################################################################
#                                Figure 6D                                    #
###############################################################################
# Combined data for average #
Density_avg <- rbind(Data_1_N_Density, Data_2_N_Density, Data_3_N_Density)

# Plot #
Fiber_denisty <- ggplot(Data_1_N_Density, aes(Relative_position, `Focused KMTs %`)) +
  geom_smooth(color = "brown1", se = F, method = "loess", formula = "y~x") +
  theme_classic() +
  ylim(0, 100)

Fiber_denisty <- Fiber_denisty +
  geom_smooth(
    data = Data_2_N_Density, aes(Relative_position, `Focused KMTs %`),
    color = "brown3", se = F, method = "loess", formula = "y~x"
  )

Fiber_denisty <- Fiber_denisty +
  geom_smooth(
    data = Data_3_N_Density, aes(Relative_position, `Focused KMTs %`),
    color = "brown4", se = F, method = "loess", formula = "y~x"
  )

Fiber_denisty <- Fiber_denisty +
  geom_smooth(
    data = Density_avg, aes(Relative_position, `Focused KMTs %`),
    color = "grey20", se = T, method = "loess", formula = "y~x"
  )

print(Fiber_denisty)

Fiber_denisty_mean <- tibble(
    Mean = c(
        mean(Data_1_N_Density$`Focused KMTs %`),
        mean(Data_2_N_Density$`Focused KMTs %`),
        mean(Data_3_N_Density$`Focused KMTs %`)
    ),
    sd = c(
      sd(Data_1_N_Density$`Focused KMTs %`),
      sd(Data_2_N_Density$`Focused KMTs %`),
      sd(Data_3_N_Density$`Focused KMTs %`)
    )
)
paste(round(mean(Fiber_denisty_mean$Mean), 2),
      "+/-",
      round(sd(Fiber_denisty_mean$Mean), 2),
      sep = " ")

###############################################################################
#                                Figure 7B                                    #
###############################################################################
# Combined data for average #
KMT_Minus_End_Interaction_25 <- rbind(
  Data_1_KMT_Minus_End_0.025,
  Data_2_KMT_Minus_End_0.025,
  Data_3_KMT_Minus_End_0.025
)
KMT_Minus_End_Interaction_30 <- rbind(
  Data_1_KMT_Minus_End_0.03,
  Data_2_KMT_Minus_End_0.03,
  Data_3_KMT_Minus_End_0.03
)
KMT_Minus_End_Interaction_35 <- rbind(
  Data_1_KMT_Minus_End_0.035,
  Data_2_KMT_Minus_End_0.035,
  Data_3_KMT_Minus_End_0.035
)
KMT_Minus_End_Interaction_45 <- rbind(
  Data_1_KMT_Minus_End_0.045,
  Data_2_KMT_Minus_End_0.045,
  Data_3_KMT_Minus_End_0.045
)
KMT_Minus_End_Interaction_50 <- rbind(
  Data_1_KMT_Minus_End_0.05,
  ata_2_KMT_Minus_End_0.05,
  Data_3_KMT_Minus_End_0.05
)
KMT_Minus_End_Interaction_75 <- rbind(
  Data_1_KMT_Minus_End_0.075,
  Data_2_KMT_Minus_End_0.075,
  Data_3_KMT_Minus_End_0.075
)
KMT_Minus_End_Interaction_100 <- rbind(
  Data_1_KMT_Minus_End_0.1,
  Data_2_KMT_Minus_End_0.1,
  Data_3_KMT_Minus_End_0.1
)

# Analysis KMT minus end interaction #
KMT_Minus_End_Data_All <- tibble(
  Data = c(
    Interaction_25 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.025, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.025),
      nrow(filter(Data_2_KMT_Minus_End_0.025, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.025),
      nrow(filter(Data_3_KMT_Minus_End_0.025, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.025)
    )),
    Interaction_30 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.03, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.03),
      nrow(filter(Data_2_KMT_Minus_End_0.03, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.03),
      nrow(filter(Data_3_KMT_Minus_End_0.03, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.03)
    )),
    Interaction_35 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.035, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.035),
      nrow(filter(Data_2_KMT_Minus_End_0.035, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.035),
      nrow(filter(Data_3_KMT_Minus_End_0.035, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.035)
    )),
    Interaction_45 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.045, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.045),
      nrow(filter(Data_2_KMT_Minus_End_0.045, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.045),
      nrow(filter(Data_3_KMT_Minus_End_0.045, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.045)
    )),
    Interaction_50 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.05, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.05),
      nrow(filter(Data_2_KMT_Minus_End_0.05, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.05),
      nrow(filter(Data_3_KMT_Minus_End_0.05, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.05)
    )),
    Interaction_75 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.075, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.075),
      nrow(filter(Data_2_KMT_Minus_End_0.075, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.075),
      nrow(filter(Data_3_KMT_Minus_End_0.075, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.075)
    )),
    Interaction_100 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.1, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.1),
      nrow(filter(Data_2_KMT_Minus_End_0.1, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.1),
      nrow(filter(Data_3_KMT_Minus_End_0.1, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.1)
    ))
  ),
  Label = c(
    "All_25_A",
    "All_30_A",
    "All_35_A",
    "All_45_A",
    "All_50_A",
    "All_75_A",
    "All_100_A"
  ),
  STD = c(
    Interaction_25 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.025, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.025),
      nrow(filter(Data_2_KMT_Minus_End_0.025, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.025),
      nrow(filter(Data_3_KMT_Minus_End_0.025, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.025)
    )),
    Interaction_30 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.03, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.03),
      nrow(filter(Data_2_KMT_Minus_End_0.03, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.03),
      nrow(filter(Data_3_KMT_Minus_End_0.03, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.03)
    )),
    Interaction_35 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.035, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.035),
      nrow(filter(Data_2_KMT_Minus_End_0.035, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.035),
      nrow(filter(Data_3_KMT_Minus_End_0.035, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.035)
    )),
    Interaction_45 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.045, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.045),
      nrow(filter(Data_2_KMT_Minus_End_0.045, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.045),
      nrow(filter(Data_3_KMT_Minus_End_0.045, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.045)
    )),
    Interaction_50 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.05, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.05),
      nrow(filter(Data_2_KMT_Minus_End_0.05, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.05),
      nrow(filter(Data_3_KMT_Minus_End_0.05, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.05)
    )),
    Interaction_75 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.075, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.075),
      nrow(filter(Data_2_KMT_Minus_End_0.075, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.075),
      nrow(filter(Data_3_KMT_Minus_End_0.075, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.075)
    )),
    Interaction_100 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.1, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_1_KMT_Minus_End_0.1),
      nrow(filter(Data_2_KMT_Minus_End_0.1, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_2_KMT_Minus_End_0.1),
      nrow(filter(Data_3_KMT_Minus_End_0.1, MT_type == "KMT" | MT_type == "SMT")) /
        nrow(Data_3_KMT_Minus_End_0.1)
    ))
  )
)

# Plot #
ggplot(KMT_Minus_End_Data_All, aes(Label, weight = Data)) +
  geom_bar() +
  theme_classic() +
  ylim(0, 1) +
  geom_errorbar(aes(ymin = Data - STD, ymax = Data + STD),
    width = .2,
    position = position_dodge(.9)
  )

###############################################################################
#                                Figure 7D                                    #
###############################################################################
# Combined data for average #
KMT_Minus_End_Interaction_25 <- rbind(
  Data_1_KMT_Minus_End_0.025,
  Data_2_KMT_Minus_End_0.025,
  Data_3_KMT_Minus_End_0.025
)
KMT_Minus_End_Interaction_30 <- rbind(
  Data_1_KMT_Minus_End_0.03,
  Data_2_KMT_Minus_End_0.03,
  Data_3_KMT_Minus_End_0.03
)
KMT_Minus_End_Interaction_35 <- rbind(
  Data_1_KMT_Minus_End_0.035,
  Data_2_KMT_Minus_End_0.035,
  Data_3_KMT_Minus_End_0.035
)
KMT_Minus_End_Interaction_45 <- rbind(
  Data_1_KMT_Minus_End_0.045,
  Data_2_KMT_Minus_End_0.045,
  Data_3_KMT_Minus_End_0.045
)
KMT_Minus_End_Interaction_50 <- rbind(
  Data_1_KMT_Minus_End_0.05,
  Data_2_KMT_Minus_End_0.05,
  Data_3_KMT_Minus_End_0.05
)
KMT_Minus_End_Interaction_75 <- rbind(
  Data_1_KMT_Minus_End_0.075,
  Data_2_KMT_Minus_End_0.075,
  Data_3_KMT_Minus_End_0.075
)
KMT_Minus_End_Interaction_100 <- rbind(
  Data_1_KMT_Minus_End_0.1,
  Data_2_KMT_Minus_End_0.1,
  Data_3_KMT_Minus_End_0.1
)

# Analysis of KMT minus end interaction #
KMT_Minus_End_Data_Pole <- tibble(
  Data = c(
    Interaction_25 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.025, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.025),
      nrow(filter(Data_2_KMT_Minus_End_0.025, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.025),
      nrow(filter(Data_3_KMT_Minus_End_0.025, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.025)
    )),
    Interaction_30 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.03, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.03),
      nrow(filter(Data_2_KMT_Minus_End_0.03, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.03),
      nrow(filter(Data_3_KMT_Minus_End_0.03, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.03)
    )),
    Interaction_35 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.035, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.035),
      nrow(filter(Data_2_KMT_Minus_End_0.035, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.035),
      nrow(filter(Data_3_KMT_Minus_End_0.035, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.035)
    )),
    Interaction_45 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.045, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.045),
      nrow(filter(Data_2_KMT_Minus_End_0.045, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.045),
      nrow(filter(Data_3_KMT_Minus_End_0.045, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.045)
    )),
    Interaction_50 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.05, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.05),
      nrow(filter(Data_2_KMT_Minus_End_0.05, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.05),
      nrow(filter(Data_3_KMT_Minus_End_0.05, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.05)
    )),
    Interaction_75 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.075, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.075),
      nrow(filter(Data_2_KMT_Minus_End_0.075, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.075),
      nrow(filter(Data_3_KMT_Minus_End_0.075, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.075)
    )),
    Interaction_100 = mean(c(
      nrow(filter(Data_1_KMT_Minus_End_0.1, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.1),
      nrow(filter(Data_2_KMT_Minus_End_0.1, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.1),
      nrow(filter(Data_3_KMT_Minus_End_0.1, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.1)
    ))
  ),
  Label = c(
    "All_25_A",
    "All_30_A",
    "All_35_A",
    "All_45_A",
    "All_50_A",
    "All_75_A",
    "All_100_A"
  ),
  STD = c(
    Interaction_25 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.025, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.025),
      nrow(filter(Data_2_KMT_Minus_End_0.025, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.025),
      nrow(filter(Data_3_KMT_Minus_End_0.025, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.025)
    )),
    Interaction_30 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.03, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.03),
      nrow(filter(Data_2_KMT_Minus_End_0.03, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.03),
      nrow(filter(Data_3_KMT_Minus_End_0.03, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.03)
    )),
    Interaction_35 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.035, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.035),
      nrow(filter(Data_2_KMT_Minus_End_0.035, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.035),
      nrow(filter(Data_3_KMT_Minus_End_0.035, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.035)
    )),
    Interaction_45 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.045, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.045),
      nrow(filter(Data_2_KMT_Minus_End_0.045, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.045),
      nrow(filter(Data_3_KMT_Minus_End_0.045, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.045)
    )),
    Interaction_50 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.05, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.05),
      nrow(filter(Data_2_KMT_Minus_End_0.05, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.05),
      nrow(filter(Data_3_KMT_Minus_End_0.05, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.05)
    )),
    Interaction_75 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.075, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.075),
      nrow(filter(Data_2_KMT_Minus_End_0.075, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.075),
      nrow(filter(Data_3_KMT_Minus_End_0.075, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.075)
    )),
    Interaction_100 = sd(c(
      nrow(filter(Data_1_KMT_Minus_End_0.1, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_1_KMT_Minus_End_0.1),
      nrow(filter(Data_2_KMT_Minus_End_0.1, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_2_KMT_Minus_End_0.1),
      nrow(filter(Data_3_KMT_Minus_End_0.1, MT_type == "KMT" & KMT_Minus_Distance >= 1.53 |
        MT_type == "SMT" & KMT_Minus_Distance >= 1.53)) /
        nrow(Data_3_KMT_Minus_End_0.1)
    ))
  )
)

# Plot #
ggplot(KMT_Minus_End_Data_Pole, aes(Label, weight = Data)) +
  geom_bar() +
  theme_classic() +
  ylim(0, 1) +
  geom_errorbar(aes(ymin = Data - STD, ymax = Data + STD),
    width = .2,
    position = position_dodge(.9)
  )

###############################################################################
#                                Figure 7F                                    #
###############################################################################
# Combined data for average #
KMT_Minus_End_Interaction_35 <- rbind(
  Data_1_KMT_Minus_End_0.035,
  Data_2_KMT_Minus_End_0.035,
  Data_3_KMT_Minus_End_0.035
)

# Plot #
ggplot(filter(KMT_Minus_End_Interaction_35, MT_type == "KMT"), aes(Relative_position)) +
  geom_histogram(bins = 100) +
  ylim(0, 25) +
  theme_classic() +
  ylab("No. of KMT minus ends interacting with KMT lattices") +
  xlab("Relative position on the spindle axis")

# Analysis of KMT number in CIA with minus end associated to other MT #
KMT_no_with_Association <- filter(KMT_Minus_End_Interaction_35, MT_type == "KMT")
KMT_no_in_CIA <- KMT_no_with_Association %>%
  filter(KMT_Minus_Distance <= CIA)

print((nrow(KMT_no_in_CIA) / nrow(KMT_no_with_Association)) * 100)

###############################################################################
#                                Figure 7G                                    #
###############################################################################
# Combined data for average #
KMT_Minus_End_Interaction_35 <- rbind(
  Data_1_KMT_Minus_End_0.035,
  Data_2_KMT_Minus_End_0.035,
  Data_3_KMT_Minus_End_0.035
)

# Plot #
ggplot(filter(KMT_Minus_End_Interaction_35, MT_type == "SMT"), aes(Relative_position)) +
  geom_histogram(bins = 100) +
  ylim(0, 25) +
  theme_classic() +
  ylab("No. of KMT minus ends interacting with non-KMT lattices") +
  xlab("Relative position on the spindle axis")

print(KMT_Minus_End_Pos)

# Analysis non-KMTs number in CIA with minus end associated to other MT #
KMT_no_with_Association <- filter(KMT_Minus_End_Interaction_35, MT_type == "SMT")
KMT_no_in_CIA <- KMT_no_with_Association %>%
  filter(KMT_Minus_Distance <= CIA)

print((nrow(KMT_no_in_CIA) / nrow(KMT_no_with_Association)) * 100)

###############################################################################
#                                Figure 8B                                    #
###############################################################################
# Combined data for average #
Segments_KMT_1 <<- Data_Segments_1 %>% filter_at(
  vars(starts_with("Pole")),
  any_vars(. >= 1)
)
Segments_KMT_1 <<- Segments_KMT_1 %>% select(
  "Segment ID",
  "length",
  "Node ID #1",
  "Node ID #2",
  "Point IDs"
)
Segments_KMT_2 <<- Data_Segments_2 %>% filter_at(
  vars(starts_with("Pole")),
  any_vars(. >= 1)
)
Segments_KMT_2 <<- Segments_KMT_2 %>% select(
  "Segment ID",
  "length",
  "Node ID #1",
  "Node ID #2",
  "Point IDs"
)
Segments_KMT_3 <<- Data_Segments_3 %>% filter_at(
  vars(starts_with("Pole")),
  any_vars(. >= 1)
)
Segments_KMT_3 <<- Segments_KMT_3 %>% select(
  "Segment ID",
  "length",
  "Node ID #1",
  "Node ID #2",
  "Point IDs"
)

Assign_MT_Type <- function(name, ID) {
  Data <- get(name)
  Raw <- get(paste0("Segments_KMT_", ID))
  df <- tibble()
  for (i in seq_len(nrow(Data))) {
    if (Data$Segments_ID_1[i] %in% Raw$`Segment ID`) {
      df[i, 1] <- "KMT"
    } else {
      df[i, 1] <- "SMT"
    }
  }
  df <- assign(
    name,
    cbind(Data[, 1:7], df)
  )

  return(df)
}

List_Name <- c(
  "Data_1_MT_Interaction_0.025", "Data_1_MT_Interaction_0.03",
  "Data_1_MT_Interaction_0.035", "Data_1_MT_Interaction_0.045",
  "Data_1_MT_Interaction_0.05"
)
for (x in List_Name) {
  print(x)
  assign(
    x,
    Assign_MT_Type(x, 1)
  )
}

List_Name <- c(
  "Data_2_MT_Interaction_0.025", "Data_2_MT_Interaction_0.03",
  "Data_2_MT_Interaction_0.035", "Data_2_MT_Interaction_0.045",
  "Data_2_MT_Interaction_0.05"
)
for (x in List_Name) {
  print(x)
  assign(
    x,
    Assign_MT_Type(x, 2)
  )
}

List_Name <- c(
  "Data_3_MT_Interaction_0.025", "Data_3_MT_Interaction_0.03",
  "Data_3_MT_Interaction_0.035", "Data_3_MT_Interaction_0.045",
  "Data_3_MT_Interaction_0.05"
)
for (x in List_Name) {
  print(x)
  assign(
    x,
    Assign_MT_Type(x, 3)
  )
}

AVG_25 <- rbind(
  Data_1_MT_Interaction_0.025,
  Data_2_MT_Interaction_0.025,
  Data_3_MT_Interaction_0.025
)
AVG_30 <- rbind(
  Data_1_MT_Interaction_0.03,
  Data_2_MT_Interaction_0.03,
  Data_3_MT_Interaction_0.03
)
AVG_35 <- rbind(
  Data_1_MT_Interaction_0.035,
  Data_2_MT_Interaction_0.035,
  Data_3_MT_Interaction_0.035
)
AVG_45 <- rbind(
  Data_1_MT_Interaction_0.045,
  Data_2_MT_Interaction_0.045,
  Data_3_MT_Interaction_0.045
)
AVG_50 <- rbind(
  Data_1_MT_Interaction_0.05,
  Data_2_MT_Interaction_0.05,
  Data_3_MT_Interaction_0.05
)

AVG_MT_Interaction_25 <- tibble()
coutnter <- 1
for (i in unique(filter(AVG_25, `...1` == "KMT")$Segments_ID_1)) {
  AVG_MT_Interaction_25[coutnter, 1] <- mean(filter(AVG_25, `Segments_ID_1` == i)$Length)
  AVG_MT_Interaction_25[coutnter, 2] <- nrow(filter(AVG_25, `Segments_ID_1` == i))
  coutnter <- coutnter + 1
}

AVG_MT_Interaction_30 <- tibble()
coutnter <- 1
for (i in unique(filter(AVG_30, `...1` == "KMT")$Segments_ID_1)) {
  AVG_MT_Interaction_30[coutnter, 1] <- mean(filter(AVG_30, `Segments_ID_1` == i)$Length)
  AVG_MT_Interaction_30[coutnter, 2] <- nrow(filter(AVG_30, `Segments_ID_1` == i))
  coutnter <- coutnter + 1
}

AVG_MT_Interaction_35 <- tibble()
coutnter <- 1
for (i in unique(filter(AVG_35, `...1` == "KMT")$Segments_ID_1)) {
  AVG_MT_Interaction_35[coutnter, 1] <- mean(filter(AVG_35, `Segments_ID_1` == i)$Length)
  AVG_MT_Interaction_35[coutnter, 2] <- nrow(filter(AVG_35, `Segments_ID_1` == i))
  coutnter <- coutnter + 1
}

AVG_MT_Interaction_45 <- tibble()
coutnter <- 1
for (i in unique(filter(AVG_45, `...1` == "KMT")$Segments_ID_1)) {
  AVG_MT_Interaction_45[coutnter, 1] <- mean(filter(AVG_45, `Segments_ID_1` == i)$Length)
  AVG_MT_Interaction_45[coutnter, 2] <- nrow(filter(AVG_45, `Segments_ID_1` == i))
  coutnter <- coutnter + 1
}

AVG_MT_Interaction_50 <- tibble()
coutnter <- 1
for (i in unique(filter(AVG_50, `...1` == "KMT")$Segments_ID_1)) {
  AVG_MT_Interaction_50[coutnter, 1] <- mean(filter(AVG_50, `Segments_ID_1` == i)$Length)
  AVG_MT_Interaction_50[coutnter, 2] <- nrow(filter(AVG_50, `Segments_ID_1` == i))
  coutnter <- coutnter + 1
}

data <- tibble(
  value = c(
    AVG_MT_Interaction_25$...1, AVG_MT_Interaction_30$...1,
    AVG_MT_Interaction_35$...1, AVG_MT_Interaction_45$...1,
    AVG_MT_Interaction_50$...1
  ),
  type = c(
    rep("25", nrow(AVG_MT_Interaction_25)),
    rep("30", nrow(AVG_MT_Interaction_30)),
    rep("35", nrow(AVG_MT_Interaction_35)),
    rep("45", nrow(AVG_MT_Interaction_45)),
    rep("50", nrow(AVG_MT_Interaction_50))
  )
)
paste0(
  round(mean(filter(data, type == 25)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 25)$value), 1)
)
paste0(
  round(mean(filter(data, type == 30)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 30)$value), 1)
)
paste0(
  round(mean(filter(data, type == 35)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 35)$value), 1)
)
paste0(
  round(mean(filter(data, type == 45)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 45)$value), 1)
)
paste0(
  round(mean(filter(data, type == 50)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 50)$value), 1)
)

data <- tibble(
  value = c(
    AVG_MT_Interaction_25$...2, AVG_MT_Interaction_30$...2,
    AVG_MT_Interaction_35$...2, AVG_MT_Interaction_45$...2,
    AVG_MT_Interaction_50$...2
  ),
  type = c(
    rep("25", nrow(AVG_MT_Interaction_25)),
    rep("30", nrow(AVG_MT_Interaction_30)),
    rep("35", nrow(AVG_MT_Interaction_35)),
    rep("45", nrow(AVG_MT_Interaction_45)),
    rep("50", nrow(AVG_MT_Interaction_50))
  )
)
paste0(
  round(mean(filter(data, type == 25)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 25)$value), 1)
)
paste0(
  round(mean(filter(data, type == 30)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 30)$value), 1)
)
paste0(
  round(mean(filter(data, type == 35)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 35)$value), 1)
)
paste0(
  round(mean(filter(data, type == 45)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 45)$value), 1)
)
paste0(
  round(mean(filter(data, type == 50)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 50)$value), 1)
)

# Plot #
data %>%
  ggplot(aes(x = value, fill = type)) +
  geom_freqpoly() +
  scale_fill_manual(values = c("grey80", "grey65", "grey50", "grey40", "grey20")) +
  theme_classic() +
  labs(fill = "")

###############################################################################
#                                Figure 8D                                    #
###############################################################################
# Combined data for average #
AVG_MT_Interaction_25 <- tibble()
coutnter <- 1

for (i in unique(filter(AVG_25, `...1` == "SMT")$Segments_ID_1)) {
  AVG_MT_Interaction_25[coutnter, 1] <- mean(filter(AVG_25, `Segments_ID_1` == i)$Length)
  AVG_MT_Interaction_25[coutnter, 2] <- nrow(filter(AVG_25, `Segments_ID_1` == i))
  coutnter <- coutnter + 1
}

AVG_MT_Interaction_30 <- tibble()
coutnter <- 1
for (i in unique(filter(AVG_30, `...1` == "SMT")$Segments_ID_1)) {
  AVG_MT_Interaction_30[coutnter, 1] <- mean(filter(AVG_30, `Segments_ID_1` == i)$Length)
  AVG_MT_Interaction_30[coutnter, 2] <- nrow(filter(AVG_30, `Segments_ID_1` == i))
  coutnter <- coutnter + 1
}

AVG_MT_Interaction_35 <- tibble()
coutnter <- 1
for (i in unique(filter(AVG_35, `...1` == "SMT")$Segments_ID_1)) {
  AVG_MT_Interaction_35[coutnter, 1] <- mean(filter(AVG_35, `Segments_ID_1` == i)$Length)
  AVG_MT_Interaction_35[coutnter, 2] <- nrow(filter(AVG_35, `Segments_ID_1` == i))
  coutnter <- coutnter + 1
}

AVG_MT_Interaction_45 <- tibble()
coutnter <- 1
for (i in unique(filter(AVG_45, `...1` == "SMT")$Segments_ID_1)) {
  AVG_MT_Interaction_45[coutnter, 1] <- mean(filter(AVG_45, `Segments_ID_1` == i)$Length)
  AVG_MT_Interaction_45[coutnter, 2] <- nrow(filter(AVG_45, `Segments_ID_1` == i))
  coutnter <- coutnter + 1
}

AVG_MT_Interaction_50 <- tibble()
coutnter <- 1
for (i in unique(filter(AVG_50, `...1` == "SMT")$Segments_ID_1)) {
  AVG_MT_Interaction_50[coutnter, 1] <- mean(filter(AVG_50, `Segments_ID_1` == i)$Length)
  AVG_MT_Interaction_50[coutnter, 2] <- nrow(filter(AVG_50, `Segments_ID_1` == i))
  coutnter <- coutnter + 1
}

data <- tibble(
  value = c(
    AVG_MT_Interaction_25$...1, AVG_MT_Interaction_30$...1,
    AVG_MT_Interaction_35$...1, AVG_MT_Interaction_45$...1,
    AVG_MT_Interaction_50$...1
  ),
  type = c(
    rep("25", nrow(AVG_MT_Interaction_25)),
    rep("30", nrow(AVG_MT_Interaction_30)),
    rep("35", nrow(AVG_MT_Interaction_35)),
    rep("45", nrow(AVG_MT_Interaction_45)),
    rep("50", nrow(AVG_MT_Interaction_50))
  )
)
paste0(
  round(mean(filter(data, type == 25)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 25)$value), 1)
)
paste0(
  round(mean(filter(data, type == 30)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 30)$value), 1)
)
paste0(
  round(mean(filter(data, type == 35)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 35)$value), 1)
)
paste0(
  round(mean(filter(data, type == 45)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 45)$value), 1)
)
paste0(
  round(mean(filter(data, type == 50)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 50)$value), 1)
)

data <- tibble(
  value = c(
    AVG_MT_Interaction_25$...2, AVG_MT_Interaction_30$...2,
    AVG_MT_Interaction_35$...2, AVG_MT_Interaction_45$...2,
    AVG_MT_Interaction_50$...2
  ),
  type = c(
    rep("25", nrow(AVG_MT_Interaction_25)),
    rep("30", nrow(AVG_MT_Interaction_30)),
    rep("35", nrow(AVG_MT_Interaction_35)),
    rep("45", nrow(AVG_MT_Interaction_45)),
    rep("50", nrow(AVG_MT_Interaction_50))
  )
)
paste0(
  round(mean(filter(data, type == 25)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 25)$value), 1)
)
paste0(
  round(mean(filter(data, type == 30)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 30)$value), 1)
)
paste0(
  round(mean(filter(data, type == 35)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 35)$value), 1)
)
paste0(
  round(mean(filter(data, type == 45)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 45)$value), 1)
)
paste0(
  round(mean(filter(data, type == 50)$value), 1),
  " +/- ",
  round(sd(filter(data, type == 50)$value), 1)
)

# Plot #
data %>%
  ggplot(aes(x = value, fill = type)) +
  geom_freqpoly() +
  scale_fill_manual(values = c("grey80", "grey65", "grey50", "grey40", "grey20")) +
  theme_classic() +
  labs(fill = "")
