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
#                       Figure 2-figure supplement 1B                         #
###############################################################################
# Combined data for average #
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

# Plot #
Pole_to_Pole <- ggplot(PTP[1, ], aes("Metaphase #1", distance)) +
  geom_col(fill = "brown1") +
  theme_classic()
Pole_to_Pole <- Pole_to_Pole +
  geom_col(
    data = PTP[2, ], aes("Metaphase #2", distance),
    fill = "brown3"
  )
Pole_to_Pole <- Pole_to_Pole +
  geom_col(
    data = PTP[3, ], aes("Metaphase #3", distance),
    fill = "brown4"
  )
Pole_to_Pole <- Pole_to_Pole +
  geom_col(
    data = data.frame(distance = mean(PTP[, 7])), aes("Avg.", distance),
    fill = "darkred"
  ) +
  stat_summary(
    data = data.frame(distance = PTP[, 7]),
    aes("Avg.", distance),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )

print(Pole_to_Pole)

###############################################################################
#                       Figure 2-figure supplement 1D                         #
###############################################################################
# Combined data for average #
IKD_avg <- rbind(Data_1_IKD, Data_2_IKD, Data_3_IKD)

# Plot #
IKD <- ggplot(Data_1_IKD, aes("Metaphase #1", `Inter-kinetochore distance`)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 2
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  ylim(0.5, 1.8) +
  theme_classic()
IKD <- IKD +
  geom_quasirandom(
    data = Data_2_IKD, aes("Metaphase #2", `Inter-kinetochore distance`),
    color = "brown3", shape = 16, size = 1.5
  ) +
  stat_summary(
    data = Data_2_IKD,
    aes("Metaphase #2", `Inter-kinetochore distance`),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
IKD <- IKD +
  geom_quasirandom(
    data = Data_3_IKD, aes("Metaphase #3", `Inter-kinetochore distance`),
    color = "brown4", shape = 17, size = 1.5
  ) +
  stat_summary(
    data = Data_3_IKD,
    aes("Metaphase #3", `Inter-kinetochore distance`),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
IKD <- IKD +
  geom_quasirandom(
    data = IKD_avg, aes("Avg.", `Inter-kinetochore distance`),
    color = "grey20", shape = 16, size = 1.5
  ) +
  stat_summary(
    data = IKD_avg,
    aes("Avg.", `Inter-kinetochore distance`),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )

print(IKD)

###############################################################################
#                       Figure 3-figure supplement 1A                         #
###############################################################################
# Combined data for average #
No_KMTs_avg <- rbind(
  Data_1_KMT_No,
  Data_2_KMT_No,
  Data_3_KMT_No
)

# Plot #
No_KMTs <- ggplot(Data_1_KMT_No, aes("Metaphase #1", KMTs_per_kinetochore)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 1.5
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  theme_classic()
No_KMTs <- No_KMTs +
  geom_quasirandom(
    data = Data_2_KMT_No, aes("Metaphase #2", KMTs_per_kinetochore),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = Data_2_KMT_No,
    aes("Metaphase #2", KMTs_per_kinetochore),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
No_KMTs <- No_KMTs +
  geom_quasirandom(
    data = Data_3_KMT_No, aes("Metaphase #3", KMTs_per_kinetochore),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 1.5
  ) +
  stat_summary(
    data = Data_3_KMT_No,
    aes("Metaphase #3", KMTs_per_kinetochore),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
No_KMTs <- No_KMTs +
  geom_quasirandom(
    data = No_KMTs_avg, aes("Avg.", KMTs_per_kinetochore),
    method = "tukeyDense", color = "darkred",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = No_KMTs_avg,
    aes("Avg.", KMTs_per_kinetochore),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )

print(No_KMTs)

###############################################################################
#                       Figure 3-figure supplement 1A                         #
###############################################################################
KMTs_pole_1 <- tibble()
KMTs_pole_2 <- tibble()
KMTs_pole_3 <- tibble()
df <- filter(Data_1_LD, minus_dist_to_pole <= CIA)
j <- 1
for (i in unique(Data_1_LD$Fiber_Name)) {
    KMTs_pole_1[j, 1] <- nrow(filter(df, Fiber_Name == i))
    j <- j + 1
  }

df <- filter(Data_2_LD, minus_dist_to_pole <= CIA)
j <- 1
for (i in unique(Data_2_LD$Fiber_Name)) {
  KMTs_pole_2[j, 1] <- nrow(filter(df, Fiber_Name == i))
  j <- j + 1
}

df <- filter(Data_3_LD, minus_dist_to_pole <= CIA)
j <- 1
for (i in unique(Data_3_LD$Fiber_Name)) {
  KMTs_pole_3[j, 1] <- nrow(filter(df, Fiber_Name == i))
  j <- j + 1
}

AVG_kmt_pole <- rbind(
  KMTs_pole_1,
  KMTs_pole_2,
  KMTs_pole_3
)

# Plot #
No_KMTs <- ggplot(AVG_kmt_pole, aes("Avg.", ...1)) +
  geom_quasirandom(method = "tukeyDense", color = "brown1",
                   shape = 15, size = 1.5) +
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1),
               geom = "pointrange") +
  theme_classic()
No_KMTs <- No_KMTs +
  geom_quasirandom(data = KMTs_pole_1, aes("Metaphase #1", ...1),
                   method = "tukeyDense", color = "brown3",
                   shape = 16, size = 1.5) +
  stat_summary(data = KMTs_pole_1,
               aes("Metaphase #1", ...1),
               fun.data = "mean_sdl", fun.args = list(mult = 1),
               geom = "pointrange")
No_KMTs <- No_KMTs +
  geom_quasirandom(data = KMTs_pole_2, aes("Metaphase #2", ...1),
                   method = "tukeyDense", color = "brown4",
                   shape = 17, size = 1.5) +
  stat_summary(data = KMTs_pole_2,
               aes("Metaphase #2", ...1),
               fun.data = "mean_sdl", fun.args = list(mult = 1),
               geom = "pointrange")
No_KMTs <- No_KMTs +
  geom_quasirandom(data = KMTs_pole_3, aes("Metaphase #3", ...1),
                   method = "tukeyDense", color = "darkred",
                   shape = 16, size = 1.5) +
  stat_summary(data = KMTs_pole_3,
               aes("Metaphase #3", ...1),
               fun.data = "mean_sdl", fun.args = list(mult = 1),
               geom = "pointrange")

print(No_KMTs)

###############################################################################
#                       Figure 3-figure supplement 1D                         #
###############################################################################
# Combined data for average #
Kinetochores_size_avg <- rbind(
  Data_1_K_Core_Area,
  Data_2_K_Core_Area,
  Data_3_K_Core_Area
)

# Plot #
K_Core_size <- ggplot(Kinetochores_size_avg, aes(Kinetochore_area, KMT_no)) +
  stat_ellipse(color = "grey20", size = 1, level = 0.95) +
  theme_classic() + xlim(0, 0.3)
K_Core_size <- K_Core_size +
  geom_point(
    data = Data_1_K_Core_Area, aes(Kinetochore_area, KMT_no),
    color = "brown3", shape = 15, size = 3
  )
K_Core_size <- K_Core_size +
  geom_point(
    data = Data_2_K_Core_Area, aes(Kinetochore_area, KMT_no),
    color = "brown3", shape = 16, size = 3
  )
K_Core_size <- K_Core_size +
  geom_point(
    data = Data_3_K_Core_Area, aes(Kinetochore_area, KMT_no),
    color = "brown4", shape = 17, size = 3
  )

print(K_Core_size)

poisson.model <- glm(KMT_no ~ Kinetochore_area, Kinetochores_size_avg, family = poisson(link = "log"))
summary(poisson.model)

###############################################################################
#                       Figure 3-figure supplement 1E                         #
###############################################################################
KMT_density_all <- rbind(
  Data_1_K_Core_Area,
  Data_2_K_Core_Area,
  Data_3_K_Core_Area
)


# Plot #
KMT_density <- ggplot(Data_1_K_Core_Area, aes("Metaphase #1", KMT_density)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 3
  ) + ylim(0, 250) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  theme_classic()
KMT_density <- KMT_density +
  geom_quasirandom(
    data = Data_2_K_Core_Area, aes("Metaphase #2", KMT_density),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 3
  ) +
  stat_summary(
    data = Data_2_K_Core_Area,
    aes("Metaphase #2", KMT_density),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
KMT_density <- KMT_density +
  geom_quasirandom(
    data = Data_3_K_Core_Area, aes("Metaphase #3", KMT_density),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 3
  ) +
  stat_summary(
    data = Data_3_K_Core_Area,
    aes("Metaphase #3", KMT_density),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
KMT_density <- KMT_density +
  geom_quasirandom(
    data = KMT_density_all, aes("Avg.", KMT_density),
    method = "tukeyDense", color = "darkred",
    shape = 16, size = 3
  ) +
  stat_summary(
    data = KMT_density_all,
    aes("Avg.", KMT_density),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )

print(KMT_density)
paste0(
  "Average density ", round(mean(KMT_density_all$KMT_density), 3), " +/- ",
  round(sd(KMT_density_all$KMT_density), 3)
)

###############################################################################
#                       Figure 3-figure supplement 1F                         #
###############################################################################
KMT_density_all <- rbind(
  Data_1_K_Core_Area,
  Data_2_K_Core_Area,
  Data_3_K_Core_Area
)


# Plot #
KMT_density <- ggplot(Data_1_K_Core_Area, aes("Metaphase #1", neigbour_mean)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 1.5
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  theme_classic()
KMT_density <- KMT_density +
  geom_quasirandom(
    data = Data_2_K_Core_Area, aes("Metaphase #2", neigbour_mean),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = Data_2_K_Core_Area,
    aes("Metaphase #2", neigbour_mean),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
KMT_density <- KMT_density +
  geom_quasirandom(
    data = Data_3_K_Core_Area, aes("Metaphase #3", neigbour_mean),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 1.5
  ) +
  stat_summary(
    data = Data_3_K_Core_Area,
    aes("Metaphase #3", neigbour_mean),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
KMT_density <- KMT_density +
  geom_quasirandom(
    data = KMT_density_all, aes("Avg.", neigbour_mean),
    method = "tukeyDense", color = "darkred",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = KMT_density_all,
    aes("Avg.", neigbour_mean),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )

print(KMT_density)
paste0(
  "Average density ", round(mean(KMT_density_all$neigbour_mean), 3), " +/- ",
  round(mean(KMT_density_all$neigbour_std), 3)
)

###############################################################################
#                       Figure 3-figure supplement 2 - 1F                     #
###############################################################################
KMT_density_all <- rbind(
  Data_1_K_Core_Area,
  Data_2_K_Core_Area,
  Data_3_K_Core_Area
)

# Plot #
KMT_no_pos <- ggplot(filter(KMT_density_all, Elipse_position == "100%"), aes("outter", KMT_no)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 1.5
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  theme_classic()

KMT_no_pos <- KMT_no_pos +
  geom_quasirandom(
    data = filter(KMT_density_all, Elipse_position == "50%"),
    aes("center", KMT_no),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = filter(KMT_density_all, Elipse_position == "50%"),
    aes("center", KMT_no),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
KMT_no_pos <- KMT_no_pos +
  geom_quasirandom(
    data = filter(KMT_density_all, Elipse_position == "25%"),
    aes("inner", KMT_no),
    method = "tukeyDense", color = "brown4",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = filter(KMT_density_all, Elipse_position == "25%"),
    aes("inner", KMT_no),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(KMT_no_pos)

###############################################################################
#                       Figure 3-figure supplement 2                          #
###############################################################################
KMTs_pole_1 <- tibble()
KMTs_pole_2 <- tibble()
KMTs_pole_3 <- tibble()
df <- Data_1_LD
j <- 1
for (i in unique(Data_1_LD$Fiber_Name)) {
  KMTs_pole_1[j, 1] <- nrow(filter(df, Fiber_Name == i))
  KMTs_pole_1[j, 2] <- filter(df, Fiber_Name == i)$Elipse_Position[1]
  j <- j + 1
}

df <- Data_2_LD
j <- 1
for (i in unique(Data_2_LD$Fiber_Name)) {
  KMTs_pole_2[j, 1] <- nrow(filter(df, Fiber_Name == i))
  KMTs_pole_2[j, 2] <- filter(df, Fiber_Name == i)$Elipse_Position[1]
  j <- j + 1
}

df <- Data_3_LD
j <- 1
for (i in unique(Data_3_LD$Fiber_Name)) {
  KMTs_pole_3[j, 1] <- nrow(filter(df, Fiber_Name == i))
  KMTs_pole_3[j, 2] <- filter(df, Fiber_Name == i)$Elipse_Position[1]
  j <- j + 1
}

AVG_kmt_pole <- rbind(
  KMTs_pole_1,
  KMTs_pole_2,
  KMTs_pole_3
)

# Plot #
No_KMTs <- ggplot(filter(AVG_kmt_pole, ...2 == '100%'), aes("outer", ...1)) +
  geom_quasirandom(method = "tukeyDense", color = "brown1",
                   shape = 15, size = 1.5) +
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1),
               geom = "pointrange") +
  theme_classic()
No_KMTs <- No_KMTs +
  geom_quasirandom(data = filter(AVG_kmt_pole, ...2 == '50%'), aes("center", ...1),
                   method = "tukeyDense", color = "brown3",
                   shape = 16, size = 1.5) +
  stat_summary(data = filter(AVG_kmt_pole, ...2 == '50%'),
               aes("center", ...1),
               fun.data = "mean_sdl", fun.args = list(mult = 1),
               geom = "pointrange")
No_KMTs <- No_KMTs +
  geom_quasirandom(data = filter(AVG_kmt_pole, ...2 == '25%'), aes("inner", ...1),
                   method = "tukeyDense", color = "brown4",
                   shape = 17, size = 1.5) +
  stat_summary(data = filter(AVG_kmt_pole, ...2 == '25%'),
               aes("inner", ...1),
               fun.data = "mean_sdl", fun.args = list(mult = 1),
               geom = "pointrange")

print(No_KMTs)


###############################################################################
#                       Figure 3-figure supplement 2                          #
###############################################################################
KMT_density_all <- rbind(
  Data_1_K_Core_Area,
  Data_2_K_Core_Area,
  Data_3_K_Core_Area
)

# Plot #
KMT_no_pos <- ggplot(filter(KMT_density_all, Elipse_position == "100%"), aes("outter", KMT_density)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 1.5
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  theme_classic()

KMT_no_pos <- KMT_no_pos +
  geom_quasirandom(
    data = filter(KMT_density_all, Elipse_position == "50%"),
    aes("center", KMT_density),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = filter(KMT_density_all, Elipse_position == "50%"),
    aes("center", KMT_density),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
KMT_no_pos <- KMT_no_pos +
  geom_quasirandom(
    data = filter(KMT_density_all, Elipse_position == "25%"),
    aes("inner", KMT_density),
    method = "tukeyDense", color = "brown4",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = filter(KMT_density_all, Elipse_position == "25%"),
    aes("inner", KMT_density),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(KMT_no_pos)


###############################################################################
#                       Figure 3-figure supplement 2                          #
###############################################################################
KMT_density_all <- rbind(
  Data_1_K_Core_Area,
  Data_2_K_Core_Area,
  Data_3_K_Core_Area
)

# Plot #
KMT_no_pos <- ggplot(filter(KMT_density_all, Elipse_position == "100%"), aes("outter", neigbour_mean)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 1.5
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  theme_classic()

KMT_no_pos <- KMT_no_pos +
  geom_quasirandom(
    data = filter(KMT_density_all, Elipse_position == "50%"),
    aes("center", neigbour_mean),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = filter(KMT_density_all, Elipse_position == "50%"),
    aes("center", neigbour_mean),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
KMT_no_pos <- KMT_no_pos +
  geom_quasirandom(
    data = filter(KMT_density_all, Elipse_position == "25%"),
    aes("inner", neigbour_mean),
    method = "tukeyDense", color = "brown4",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = filter(KMT_density_all, Elipse_position == "25%"),
    aes("inner", neigbour_mean),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(KMT_no_pos)

###############################################################################
#                       Figure 4-figure supplement 1A                         #
###############################################################################
# Combined data for average #
LD_avg <- rbind(
  Data_1_LD,
  Data_2_LD,
  Data_3_LD
)

# Plot #
LD <- ggplot(Data_1_LD, aes("Metaphase #1", length)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 2
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  ylim(0, 15) +
  theme_classic()
LD <- LD +
  geom_quasirandom(
    data = Data_2_LD, aes("Metaphase #2", length),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = Data_2_LD,
    aes("Metaphase #2", length),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
LD <- LD +
  geom_quasirandom(
    data = Data_3_LD, aes("Metaphase #3", length),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 2
  ) +
  stat_summary(
    data = Data_3_LD,
    aes("Metaphase #3", length),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
LD <- LD +
  geom_quasirandom(
    data = LD_avg, aes("Avg.", length),
    method = "tukeyDense", color = "darkred",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = LD_avg,
    aes("Avg.", length),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(LD)

###############################################################################
#                       Figure 4-figure supplement                            #
###############################################################################
# Combined data for average #
LD_avg <- rbind(
  Data_1_LD,
  Data_2_LD,
  Data_3_LD
)

# Plot #
LD <- ggplot(filter(LD_avg, Elipse_Position == '100%'), aes("outer", length)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 2
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  ylim(0, 15) +
  theme_classic()
LD <- LD +
  geom_quasirandom(
    data = filter(LD_avg, Elipse_Position == '50%'), aes("center", length),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = filter(LD_avg, Elipse_Position == '50%'),
    aes("center", length),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
LD <- LD +
  geom_quasirandom(
    data = filter(LD_avg, Elipse_Position == '25%'), aes("inner", length),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 2
  ) +
  stat_summary(
    data = filter(LD_avg, Elipse_Position == '25%'),
    aes("inner", length),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(LD)

###############################################################################
#                       Figure 4-figure supplement 1B                         #
###############################################################################
# Combined data for average #
LD_avg <- rbind(
  Data_1_LD,
  Data_2_LD,
  Data_3_LD
)

# Plot #
LD <- ggplot(Data_1_LD, aes("Metaphase #1", minus_dist_to_pole)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 2
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  ylim(0, 8) +
  theme_classic()
LD <- LD +
  geom_quasirandom(
    data = Data_2_LD, aes("Metaphase #2", minus_dist_to_pole),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = Data_2_LD,
    aes("Metaphase #2", minus_dist_to_pole),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
LD <- LD +
  geom_quasirandom(
    data = Data_3_LD, aes("Metaphase #3", minus_dist_to_pole),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 2
  ) +
  stat_summary(
    data = Data_3_LD,
    aes("Metaphase #3", minus_dist_to_pole),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
LD <- LD +
  geom_quasirandom(
    data = LD_avg, aes("Avg.", minus_dist_to_pole),
    method = "tukeyDense", color = "darkred",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = LD_avg,
    aes("Avg.", minus_dist_to_pole),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(LD)

###############################################################################
#                       Figure 4-figure supplement                            #
###############################################################################
# Combined data for average #
LD_avg <- rbind(
  Data_1_LD,
  Data_2_LD,
  Data_3_LD
)

# Plot #
LD <- ggplot(filter(LD_avg, Elipse_Position == '100%'), aes("outer", minus_dist_to_pole)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 2
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  ylim(0, 15) +
  theme_classic()
LD <- LD +
  geom_quasirandom(
    data = filter(LD_avg, Elipse_Position == '50%'), aes("center", minus_dist_to_pole),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = filter(LD_avg, Elipse_Position == '50%'),
    aes("center", minus_dist_to_pole),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
LD <- LD +
  geom_quasirandom(
    data = filter(LD_avg, Elipse_Position == '25%'), aes("inner", minus_dist_to_pole),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 2
  ) +
  stat_summary(
    data = filter(LD_avg, Elipse_Position == '25%'),
    aes("inner", minus_dist_to_pole),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(LD)

###############################################################################
#                       Figure 4-figure supplement 1C                         #
###############################################################################
# Combined data for average #
Minus_Ends_avg <- rbind(
  Data_1_KMT_Minus_End_0.1,
  Data_2_KMT_Minus_End_0.1,
  Data_3_KMT_Minus_End_0.1
)

# Plot #
MInus_Ends <- ggplot(Data_1_KMT_Minus_End_0.1, aes("Metaphase #1", Relative_position)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 2
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  ylim(-0.5, 1) +
  theme_classic()
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Data_2_KMT_Minus_End_0.1, aes("Metaphase #2", Relative_position),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = Data_2_KMT_Minus_End_0.1,
    aes("Metaphase #2", Relative_position),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Data_3_KMT_Minus_End_0.1, aes("Metaphase #3", Relative_position),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 2
  ) +
  stat_summary(
    data = Data_3_KMT_Minus_End_0.1,
    aes("Metaphase #3", Relative_position),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Minus_Ends_avg, aes("Avg.", Relative_position),
    method = "tukeyDense", color = "darkred",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = Minus_Ends_avg,
    aes("Avg.", Relative_position),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(MInus_Ends)

###############################################################################
#                       Figure 4-figure supplement 1C                         #
###############################################################################
# Combined data for average #
Minus_Ends_avg <- rbind(
  Data_1_KMT_Minus_End_0.1,
  Data_2_KMT_Minus_End_0.1,
  Data_3_KMT_Minus_End_0.1
)

# Plot #
MInus_Ends <- ggplot(Data_1_KMT_Minus_End_0.1, aes("Metaphase #1", Relative_position)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 2
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  ylim(-0.5, 1) +
  theme_classic()
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Data_2_KMT_Minus_End_0.1, aes("Metaphase #2", Relative_position),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = Data_2_KMT_Minus_End_0.1,
    aes("Metaphase #2", Relative_position),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Data_3_KMT_Minus_End_0.1, aes("Metaphase #3", Relative_position),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 2
  ) +
  stat_summary(
    data = Data_3_KMT_Minus_End_0.1,
    aes("Metaphase #3", Relative_position),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Minus_Ends_avg, aes("Avg.", Relative_position),
    method = "tukeyDense", color = "darkred",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = Minus_Ends_avg,
    aes("Avg.", Relative_position),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(MInus_Ends)

###############################################################################
#                       Figure 4-figure supplement 1D                         #
###############################################################################
# Combined data for average #
LD_non <- rbind(
  Data_1_SMT_Ends,
  Data_2_SMT_Ends,
  Data_3_SMT_Ends
)

# Plot #
LD_non <- ggplot(Data_1_SMT_Ends, aes("Metaphase #1", Length)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 2
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  ylim(0, 15) +
  theme_classic()
LD_non <- LD_non +
  geom_quasirandom(
    data = Data_2_SMT_Ends, aes("Metaphase #2", Length),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = Data_2_SMT_Ends,
    aes("Metaphase #2", Length),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
LD_non <- LD_non +
  geom_quasirandom(
    data = Data_3_SMT_Ends, aes("Metaphase #3", Length),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 2
  ) +
  stat_summary(
    data = Data_3_SMT_Ends,
    aes("Metaphase #3", Length),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
LD_non <- LD_non +
  geom_quasirandom(
    data = LD_non, aes("Avg.", Length),
    method = "tukeyDense", color = "darkred",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = LD_non,
    aes("Avg.", Length),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(LD_non)

###############################################################################
#                       Figure 4-figure supplement 1E                         #
###############################################################################
# Combined data for average #
Minus_Ends_avg <- rbind(
  Data_1_SMT_Ends,
  Data_2_SMT_Ends,
  Data_3_SMT_Ends
)

# Plot #
MInus_Ends <- ggplot(Data_1_SMT_Ends, aes("Metaphase #1", Distance_to_Pole)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 2
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  ylim(0, 8) +
  theme_classic()
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Data_2_SMT_Ends, aes("Metaphase #2", Distance_to_Pole),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = Data_2_SMT_Ends,
    aes("Metaphase #2", Distance_to_Pole),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Data_3_SMT_Ends, aes("Metaphase #3", Distance_to_Pole),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 2
  ) +
  stat_summary(
    data = Data_3_SMT_Ends,
    aes("Metaphase #3", Distance_to_Pole),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Minus_Ends_avg, aes("Avg.", Distance_to_Pole),
    method = "tukeyDense", color = "darkred",
    shape = 16, size = 2
  ) +
  stat_summary(
    data = Minus_Ends_avg,
    aes("Avg.", Distance_to_Pole),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(MInus_Ends)

###############################################################################
#                       Figure 4-figure supplement 1F                         #
###############################################################################
# Combined data for average #
for (i in c("Data_1_SMT_Ends", "Data_2_SMT_Ends", "Data_3_SMT_Ends")) {
  print(i)
  data <- get(i)

  for (j in seq(nrow(data))) {
    RP <- as.numeric(data[j, "Relativ_Position"])

    if (RP > 0.5) {
      RP <- -RP + 1

      data[j, "Relativ_Position"] <- RP
    }
  }
  assign(
    i,
    data
  )
}

Minus_Ends_avg <- rbind(
  Data_1_SMT_Ends,
  Data_2_SMT_Ends,
  Data_3_SMT_Ends
)

# Plot #
MInus_Ends <- ggplot(Data_1_SMT_Ends, aes("Metaphase #1", Relativ_Position)) +
  geom_quasirandom(
    method = "tukeyDense", color = "brown1",
    shape = 15, size = 1.5
  ) +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  ) +
  ylim(-0.25, 0.5) +
  theme_classic()
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Data_2_SMT_Ends, aes("Metaphase #2", Relativ_Position),
    method = "tukeyDense", color = "brown3",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = Data_2_SMT_Ends,
    aes("Metaphase #2", Relativ_Position),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Data_3_SMT_Ends, aes("Metaphase #3", Relativ_Position),
    method = "tukeyDense", color = "brown4",
    shape = 17, size = 1.5
  ) +
  stat_summary(
    data = Data_3_SMT_Ends,
    aes("Metaphase #3", Relativ_Position),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
MInus_Ends <- MInus_Ends +
  geom_quasirandom(
    data = Minus_Ends_avg, aes("Avg.", Relativ_Position),
    method = "tukeyDense", color = "darkred",
    shape = 16, size = 1.5
  ) +
  stat_summary(
    data = Minus_Ends_avg,
    aes("Avg.", Relativ_Position),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange"
  )
print(MInus_Ends)

###############################################################################
#                       Figure 5-figure supplement 1A                         #
###############################################################################
# Combined data for average #
Tortuosity_avg <- rbind(
  Data_1_KMT_Total_Curv,
  Data_2_KMT_Total_Curv,
  Data_3_KMT_Total_Curv
)
Tortuosity_avg_inner <- Tortuosity_avg %>% filter(`Elipse Position` == "25%")
Tortuosity_avg_middle <- Tortuosity_avg %>% filter(`Elipse Position` == "50%")
Tortuosity_avg_outer <- Tortuosity_avg %>% filter(`Elipse Position` == "100%")



###############################################################################
#                       Figure 5-figure supplement 1N                         #
###############################################################################
# Combined data for average #
Tortuosity_avg <- rbind(
  Data_1_KMT_Total_Curv,
  Data_2_KMT_Total_Curv,
  Data_3_KMT_Total_Curv
)
Tortuosity_avg_inner <- Tortuosity_avg %>% filter(`Elipse Position` == "25%")
Tortuosity_avg_middle <- Tortuosity_avg %>% filter(`Elipse Position` == "50%")
Tortuosity_avg_outer <- Tortuosity_avg %>% filter(`Elipse Position` == "100%")


# Plot #
P1 <- ggplot(Tortuosity_avg_inner, aes(Curvature)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  theme_classic() +
  ylab("No. of KMTs") +
  xlab("Tortuosity of KMTs")
P1 <- P1 + geom_histogram(
  data = Tortuosity_avg_middle, aes(Curvature),
  bins = 30, fill = "blue", color = "black"
)
P1 <- P1 + geom_histogram(
  data = Tortuosity_avg_outer, aes(Curvature),
  bins = 30, fill = "purple", color = "black"
)
print(P1)
paste("Mean: ", median(Tortuosity_avg_inner$Curvature), "n=", nrow(Tortuosity_avg_inner))
paste("Mean: ", median(Tortuosity_avg_middle$Curvature), "n=", nrow(Tortuosity_avg_middle))
paste("Mean: ", median(Tortuosity_avg_outer$Curvature), "n=", nrow(Tortuosity_avg_outer))

###############################################################################
#                       Figure 7-figure supplement 1A                         #
###############################################################################
# Combined data for average #
KMT_Minus_seed <- tibble(
  Data = c(
    Interaction_25 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.025, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.025, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.025, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_30 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.03, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.03, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.03, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_35 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.035, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.035, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.035, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_45 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.045, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.045, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.045, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_50 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.05, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.05, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.05, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_75 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.075, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.075, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.075, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_100 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.1, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.1, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.1, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    )
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
    Interaction_25 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.025, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.025, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.025, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_30 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.03, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.03, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.03, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_35 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.035, I_class == "KMT"))) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.035, I_class == "KMT"))) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.035, I_class == "KMT"))) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_45 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.045, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.045, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.045, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_50 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.05, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.05, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.05, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_75 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.075, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.075, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.075, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_100 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.1, I_class == "KMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.1, I_class == "KMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.1, I_class == "KMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    )
  )
)

# Plot #
ggplot(KMT_Minus_seed, aes(Label, weight = Data)) +
  geom_bar() +
  theme_classic() +
  ylim(0, 1) +
  geom_errorbar(aes(ymin = Data - STD, ymax = Data + STD),
    width = .2,
    position = position_dodge(.9)
  )

###############################################################################
#                       Figure 7-figure supplement 1B                         #
###############################################################################
# Combined data for average #
KMT_seed <- rbind(
  Data_1_KMTs_minus_seed_0.035,
  Data_2_KMTs_minus_seed_0.035,
  Data_3_KMTs_minus_seed_0.035
)

# Plot #
ggplot(filter(KMT_seed, I_class == "KMT"), aes(Relative_pos)) +
  geom_histogram(bins = 75) +
  ylim(0, 50) +
  theme_classic() +
  ylab("No. of KMT minus ends interacting with non-KMT lattices") +
  xlab("Relative position on the spindle axis")

data <- tibble()
data[1, 1] <- 0 # Normalized number
data[1, 2] <- 1 # Relative potions
kmt_association <- filter(KMT_seed, I_class == "KMT")
df_min <- min(kmt_association$Density)
df_max <- max(kmt_association$Density)
for (i in 1:nrow(kmt_association)){
  kmt_association[i, 'Density'] <- (as.numeric(kmt_association[i, 'Density']) - df_min)/(df_max - df_min)
}

red <- 0.05
df_max <- 1
df_min <- df_max - red
for (i in 1:(1.25/red)){
  df <- kmt_association %>% filter(Relative_pos > df_min & Relative_pos <= df_max)
  data[i+1, 1] <- sum(1 - df[, 'Density'])
  data[i+1, 2] <- df_max

  df_max <- df_max - red
  df_min <- df_min - red
}

df_min <- min(data$...1)
df_max <- max(data$...1)
for (i in 1:nrow(data)){
  data[i, 1] <- (as.numeric(data[i, 1]) - df_min)/(df_max - df_min)
}
library(zoo)
i#Plot
ggplot(data, aes(...2, ...1)) +
  geom_col() +
  ylim(0, 1) +
  xlim(-0.25, 1) +
  theme_classic() +
  ylab("No. of KMT minus ends interacting with KMT lattices") +
  xlab("Relative position on the spindle axis") +
  geom_line(aes(y=rollmean(...1, 3, na.pad=TRUE)))

# Analysis of KMT number in CIA #
KMT_no_with_Association <- filter(Data_1_KMTs_minus_seed_0.035, I_class == "KMT")
KMT_no_in_CIA <- KMT_no_with_Association %>%
  filter(Relative_pos < 0.2 & Relative_pos > -0.2)

print((nrow(KMT_no_in_CIA) / nrow(KMT_no_with_Association)) * 100)

###############################################################################
#                       Figure 7-figure supplement 1C                         #
###############################################################################
# Combined data for average #
KMT_Minus_seed <- tibble(
  Data = c(
    Interaction_25 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.025, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.025, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.025, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_30 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.03, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.03, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.03, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_35 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.035, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.035, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.035, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_45 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.045, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.045, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.045, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_50 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.05, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.05, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.05, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_75 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.075, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.075, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.075, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_100 = mean(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.1, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.1, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.1, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    )
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
    Interaction_25 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.025, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.025, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.025, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_30 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.03, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.03, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.03, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_35 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.035, I_class == "SMT"))) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.035, I_class == "SMT"))) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.035, I_class == "SMT"))) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_45 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.045, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.045, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.045, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_50 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.05, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.05, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.05, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_75 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.075, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.075, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.075, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    ),
    Interaction_100 = sd(
      c(
        (length(unique(filter(Data_1_KMTs_minus_seed_0.1, I_class == "SMT")$KMT_ID)) /
          nrow(Data_1_LD)),
        (length(unique(filter(Data_2_KMTs_minus_seed_0.1, I_class == "SMT")$KMT_ID)) /
          nrow(Data_2_LD)),
        (length(unique(filter(Data_3_KMTs_minus_seed_0.1, I_class == "SMT")$KMT_ID)) /
          nrow(Data_3_LD))
      )
    )
  )
)

# Plot #
ggplot(KMT_Minus_seed, aes(Label, weight = Data)) +
  geom_bar() +
  theme_classic() +
  ylim(0, 1) +
  geom_errorbar(aes(ymin = Data - STD, ymax = Data + STD),
    width = .2,
    position = position_dodge(.9)
  )

###############################################################################
#                       Figure 7-figure supplement 1D                         #
###############################################################################
# Combined data for average #
KMT_seed <- rbind(
  Data_1_KMTs_minus_seed_0.035,
  Data_2_KMTs_minus_seed_0.035,
  Data_3_KMTs_minus_seed_0.035
)

# Plot #
ggplot(filter(KMT_seed, I_class == "SMT"), aes(Relative_pos)) +
  geom_histogram(bins = 75) +
  ylim(0, 50) +
  theme_classic() +
  ylab("No. of KMT minus ends interacting with non-KMT lattices") +
  xlab("Relative position on the spindle axis")


data <- tibble()
data[1, 1] <- 0 # Normalized number
data[1, 2] <- 1 # Relative potions
kmt_association <- filter(KMT_seed, I_class == "SMT")
df_min <- min(kmt_association$Density)
df_max <- max(kmt_association$Density)
for (i in 1:nrow(kmt_association)){
  kmt_association[i, 'Density'] <- (as.numeric(kmt_association[i, 'Density']) - df_min)/(df_max - df_min)
}

red <- 0.05
df_max <- 1
df_min <- df_max - red
for (i in 1:(1.25/red)){
  df <- kmt_association %>% filter(Relative_pos > df_min & Relative_pos <= df_max)
  data[i+1, 1] <- sum(1 - df[, 'Density'])
  data[i+1, 2] <- df_max

  df_max <- df_max - red
  df_min <- df_min - red
}

df_min <- min(data$...1)
df_max <- max(data$...1)
for (i in 1:nrow(data)){
  data[i, 1] <- (as.numeric(data[i, 1]) - df_min)/(df_max - df_min)
}

#Plot
ggplot(data, aes(...2, ...1)) +
  geom_col() +
  theme_classic() +
  ylab("No. of SMT minus ends interacting with non-KMT lattices") +
  xlab("Relative position on the spindle axis") +
  geom_line(aes(y=rollmean(...1, 3, na.pad=TRUE)))

# Analysis of non-KMT number at CIA #
KMT_no_with_Association <- filter(Data_1_KMTs_minus_seed_0.035, I_class == "SMT")
KMT_no_in_CIA <- KMT_no_with_Association %>%
  filter(Relative_pos < 0.2 & Relative_pos > -0.2)

print((nrow(KMT_no_in_CIA) / nrow(KMT_no_with_Association)) * 100)

###############################################################################
#                       Figure 8-figure supplement 1A                         #
###############################################################################
# Combined data for average #
Segments_KMT_1 <<- Data_Segments_1 %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))
Segments_KMT_1 <<- Segments_KMT_1 %>% select(
  "Segment ID",
  "length",
  "Node ID #1",
  "Node ID #2",
  "Point IDs"
)
Segments_KMT_2 <<- Data_Segments_2 %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))
Segments_KMT_2 <<- Segments_KMT_2 %>% select(
  "Segment ID",
  "length",
  "Node ID #1",
  "Node ID #2",
  "Point IDs"
)
Segments_KMT_3 <<- Data_Segments_3 %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))
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
  "Data_1_MT_Interaction_0.025", "Data_1_MT_Interaction_0.03", "Data_1_MT_Interaction_0.035",
  "Data_1_MT_Interaction_0.045", "Data_1_MT_Interaction_0.05"
)
for (x in List_Name) {
  print(x)
  assign(
    x,
    Assign_MT_Type(x, 1)
  )
}

List_Name <- c(
  "Data_2_MT_Interaction_0.025", "Data_2_MT_Interaction_0.03", "Data_2_MT_Interaction_0.035",
  "Data_2_MT_Interaction_0.045", "Data_2_MT_Interaction_0.05"
)
for (x in List_Name) {
  print(x)
  assign(
    x,
    Assign_MT_Type(x, 2)
  )
}

List_Name <- c(
  "Data_3_MT_Interaction_0.025", "Data_3_MT_Interaction_0.03", "Data_3_MT_Interaction_0.035",
  "Data_3_MT_Interaction_0.045", "Data_3_MT_Interaction_0.05"
)
for (x in List_Name) {
  print(x)
  assign(
    x,
    Assign_MT_Type(x, 3)
  )
}

# Analysis of KMT-MT interaction distributions #
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
#                       Figure 8-figure supplement 1B                         #
###############################################################################
# Combined data for average #
Segments_KMT_1 <<- Data_Segments_1 %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))
Segments_KMT_1 <<- Segments_KMT_1 %>% select(
  "Segment ID",
  "length",
  "Node ID #1",
  "Node ID #2",
  "Point IDs"
)
Segments_KMT_2 <<- Data_Segments_2 %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))
Segments_KMT_2 <<- Segments_KMT_2 %>% select(
  "Segment ID",
  "length",
  "Node ID #1",
  "Node ID #2",
  "Point IDs"
)
Segments_KMT_3 <<- Data_Segments_3 %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))
Segments_KMT_3 <<- Segments_KMT_3 %>% select(
  "Segment ID",
  "length",
  "Node ID #1",
  "Node ID #2",
  "Point IDs"
)

# Analysis of non-KMT-MT interaction distribution #
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
  "Data_1_MT_Interaction_0.025", "Data_1_MT_Interaction_0.03", "Data_1_MT_Interaction_0.035",
  "Data_1_MT_Interaction_0.045", "Data_1_MT_Interaction_0.05"
)
for (x in List_Name) {
  print(x)
  assign(
    x,
    Assign_MT_Type(x, 1)
  )
}

List_Name <- c(
  "Data_2_MT_Interaction_0.025", "Data_2_MT_Interaction_0.03", "Data_2_MT_Interaction_0.035",
  "Data_2_MT_Interaction_0.045", "Data_2_MT_Interaction_0.05"
)
for (x in List_Name) {
  print(x)
  assign(
    x,
    Assign_MT_Type(x, 2)
  )
}

List_Name <- c(
  "Data_3_MT_Interaction_0.025", "Data_3_MT_Interaction_0.03", "Data_3_MT_Interaction_0.035",
  "Data_3_MT_Interaction_0.045", "Data_3_MT_Interaction_0.05"
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

# Plot #
data %>%
  ggplot(aes(x = value, fill = type)) +
  geom_freqpoly() +
  scale_fill_manual(values = c("grey80", "grey65", "grey50", "grey40", "grey20")) +
  theme_classic() +
  labs(fill = "")

###############################################################################
#                       Figure 8-figure supplement 1C                         #
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
    AVG_MT_Interaction_25$...1, AVG_MT_Interaction_30$...1, AVG_MT_Interaction_35$...1,
    AVG_MT_Interaction_45$...1, AVG_MT_Interaction_50$...1
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

# Correlation Analysis #
library(ggpubr)
data <- rbind(
  AVG_MT_Interaction_25, AVG_MT_Interaction_30,
  AVG_MT_Interaction_35, AVG_MT_Interaction_45,
  AVG_MT_Interaction_50
)
names(data)[1:2] <- c("length", "No")
ggscatter(data,
  x = "No", y = "length", add = "reg.line", conf.int = T,
  cor.coef = TRUE, cor.method = "spearman"
)

###############################################################################
#                       Figure 8-figure supplement 1D                         #
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

# Analysis #
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

# Correlation Analysis #
library(ggpubr)
data <- rbind(
  AVG_MT_Interaction_25, AVG_MT_Interaction_30,
  AVG_MT_Interaction_35, AVG_MT_Interaction_45,
  AVG_MT_Interaction_50
)
names(data)[1:2] <- c("length", "No")
ggscatter(data,
  x = "No", y = "length", add = "reg.line", conf.int = T,
  cor.coef = TRUE, cor.method = "spearman"
)
