###########################################################################################################################
# Plots for k-fiber based on ellipse
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
###########################################################################################################################

# Number of KMTs per kinetochore ------------------------------------------------------------------------------------------
Fiber_1 <- Data_1_KMT_Total_Curv %>% distinct(`k-fiber no.`, `Elipse Position`)
Fiber_2 <- Data_2_KMT_Total_Curv %>% distinct(`k-fiber no.`, `Elipse Position`)
Fiber_3 <- Data_3_KMT_Total_Curv %>% distinct(`k-fiber no.`, `Elipse Position`)

Data_1_KMT_No <- cbind(Data_1_KMT_No[2], Fiber_1[2])
Data_2_KMT_No <- cbind(Data_2_KMT_No[2], Fiber_2[2])
Data_3_KMT_No <- cbind(Data_3_KMT_No[2], Fiber_3[2])



All_KMT_No <- rbind(Data_1_KMT_No, Data_2_KMT_No, Data_3_KMT_No)


P1 <- ggplot(All_KMT_No[with(All_KMT_No, `Elipse Position` == "100%"), ], aes("Outer", KMTs_per_kinetochore)) +
  geom_boxplot(fill = "violetred4", color = "black", outlier.alpha = 0) +
  theme_classic() +
  xlab("Data-set names") +
  ylab("Number of KMTs per kinetochore") +
  geom_jitter(data = All_KMT_No[with(All_KMT_No, `Elipse Position` == "100%"), ], aes("Outer", KMTs_per_kinetochore), alpha = 0.2, size = 1, width = 0.25)

P1 <- P1 + geom_boxplot(
  data = All_KMT_No[with(All_KMT_No, `Elipse Position` == "50%"), ], aes("Middle", KMTs_per_kinetochore),
  fill = "royalblue4", color = "black",
  outlier.alpha = 0
) +
  geom_jitter(data = All_KMT_No[with(All_KMT_No, `Elipse Position` == "50%"), ], aes("Middle", KMTs_per_kinetochore), alpha = 0.2, size = 1, width = 0.25)

P1 <- P1 + geom_boxplot(
  data = All_KMT_No[with(All_KMT_No, `Elipse Position` == "25%"), ], aes("Inner", KMTs_per_kinetochore),
  fill = "springgreen4", color = "black",
  outlier.alpha = 0
) +
  geom_jitter(data = All_KMT_No[with(All_KMT_No, `Elipse Position` == "25%"), ], aes("Inner", KMTs_per_kinetochore), alpha = 0.2, size = 1, width = 0.25)

print(P1)

# Length distribution -----------------------------------------------------------------------------------------------------

All_LD <- rbind(Data_1_LD, Data_2_LD, Data_3_LD)

P2 <- ggplot(All_LD[with(All_LD, `Elipse_Position` == "100%"), ], aes(length)) +
  geom_density(kernel = "gaussian", size = 1, color = "violetred4") +
  theme_classic() +
  xlab("KMT lengths") +
  ylab("KMT density [Gaussian Kernal density]") +
  geom_vline(
    data = All_LD[with(All_LD, `Elipse_Position` == "100%"), ],
    aes(xintercept = mean(length)), color = "violetred4", linetype = "dashed", size = 1
  )

P2 <- P2 + geom_density(
  data = All_LD[with(All_LD, `Elipse_Position` == "50%"), ],
  aes(length), kernel = "gaussian", size = 1, color = "royalblue4"
) +
  geom_vline(
    data = All_LD[with(All_LD, `Elipse_Position` == "50%"), ],
    aes(xintercept = mean(length)), color = "royalblue4", linetype = "dashed", size = 1
  )

P2 <- P2 + geom_density(
  data = All_LD[with(All_LD, `Elipse_Position` == "25%"), ],
  aes(length), kernel = "gaussian", size = 1, color = "springgreen4"
) +
  geom_vline(
    data = All_LD[with(All_LD, `Elipse_Position` == "25%"), ],
    aes(xintercept = mean(length)), color = "springgreen4", linetype = "dashed", size = 1
  )

print(P2)


# Total curvature  -------------------------------------------------------------------------------------------------------

All_T_Curv <- rbind(Data_1_KMT_Total_Curv, Data_2_KMT_Total_Curv, Data_3_KMT_Total_Curv)

P3 <- ggplot(All_T_Curv[with(All_T_Curv, `Elipse Position` == "100%"), ], aes(Curvature)) +
  geom_density(kernel = "gaussian", size = 1, color = "violetred4") +
  theme_classic() +
  xlab("KMT curvatrure") +
  ylab("KMT density [Gaussian Kernal density]") +
  xlim(1, 1.3) +
  geom_vline(
    data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "100%"), ],
    aes(xintercept = mean(Curvature)), color = "violetred4", linetype = "dashed", size = 1
  )

P3 <- P3 + geom_density(
  data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "50%"), ],
  aes(Curvature), kernel = "gaussian", size = 1, color = "royalblue4"
) +
  geom_vline(
    data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "50%"), ],
    aes(xintercept = mean(Curvature)), color = "royalblue4", linetype = "dashed", size = 1
  )
P3 <- P3 + geom_density(
  data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "25%"), ],
  aes(Curvature), kernel = "gaussian", size = 1, color = "springgreen4"
) +
  geom_vline(
    data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "25%"), ],
    aes(xintercept = mean(Curvature)), color = "springgreen4", linetype = "dashed", size = 1
  )

print(P3)

P3.1 <- ggplot(All_T_Curv[with(All_T_Curv, `Elipse Position` == "100%"), ], aes(`KMTs length`, Curvature)) +
  geom_smooth(size = 1, color = "violetred4", se = F) +
  theme_classic()

P3.1 <- P3.1 + geom_smooth(
  data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "50%"), ], aes(`KMTs length`, Curvature),
  size = 1, color = "royalblue4", se = F
)
P3.1 <- P3.1 + geom_smooth(
  data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "25%" & `KMTs length` <= 3), ], aes(`KMTs length`, Curvature),
  size = 1, color = "springgreen4", se = F
)

print(P3.1)

# Local curvature  -------------------------------------------------------------------------------------------------------

All_L_Curv <- rbind(Data_1_KMT_Local_Curv, Data_2_KMT_Local_Curv, Data_3_KMT_Local_Curv)
LC_bin_100 <- data.frame()
LC_bin_80 <- data.frame()
LC_bin_45 <- data.frame()

i <- 1
j <- 1
while (i >= -0.2) {
  LC_bin_100[j, 1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"), ][with(
    All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))
  LC_bin_80[j, 1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"), ][with(
    All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))
  LC_bin_45[j, 1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"), ][with(
    All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))

  LC_bin_100[j, 2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"), ][with(
    All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))
  LC_bin_80[j, 2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"), ][with(
    All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))
  LC_bin_45[j, 2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"), ][with(
    All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))

  LC_bin_100[j, 3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"), ][with(
    All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][2]))
  LC_bin_80[j, 3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"), ][with(
    All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][2]))
  LC_bin_45[j, 3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"), ][with(
    All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][2]))

  j <- j + 1
  i <- i - 0.1
}

LC_bin_45 <- LC_bin_45[1:10, 1:3]

P4 <- ggplot(LC_bin_100, aes(V3, V1)) +
  geom_smooth(color = "violetred4", se = F) +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT Curvature ratio") +
  xlim(-0.25, 1) +
  ylim(1, 1.02)
P4 <- P4 + geom_smooth(
  data = LC_bin_80,
  aes(V3, V1), color = "royalblue4", se = F
)
P4 <- P4 + geom_smooth(
  data = LC_bin_45,
  aes(V3, V1), color = "springgreen4", se = F
)
print(P4)


P4.1 <- ggplot(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"), ], aes(Curvature)) +
  geom_density(kernel = "gaussian", size = 1, color = "violetred4") +
  theme_classic() +
  xlab("KMT curvatrure") +
  ylab("KMT density [Gaussian Kernal density]") +
  xlim(1, 1.05) +
  geom_vline(
    data = All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"), ],
    aes(xintercept = mean(Curvature)), color = "violetred4", linetype = "dashed", size = 1
  )

P4.1 <- P4.1 + geom_density(
  data = All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"), ],
  aes(Curvature), kernel = "gaussian", size = 1, color = "royalblue4"
) +
  geom_vline(
    data = All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"), ],
    aes(xintercept = mean(Curvature)), color = "royalblue4", linetype = "dashed", size = 1
  )

P4.1 <- P4.1 + geom_density(
  data = All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"), ],
  aes(Curvature), kernel = "gaussian", size = 1, color = "springgreen4"
) +
  geom_vline(
    data = All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"), ],
    aes(xintercept = mean(Curvature)), color = "springgreen4", linetype = "dashed", size = 1
  )

print(P4.1)


# Fiber area  -------------------------------------------------------------------------------------------------------

Data_1_Fiber_Area <- cbind(Data_1_Fiber_Area, Data_1_N_Density["Elipse_Position"])
Data_2_Fiber_Area <- cbind(Data_2_Fiber_Area, Data_2_N_Density["Elipse_Position"])
Data_3_Fiber_Area <- cbind(Data_3_Fiber_Area, Data_3_N_Density["Elipse_Position"])

FA_bin_100 <- data.frame()
FA_bin_80 <- data.frame()
FA_bin_45 <- data.frame()
All_fiber_area <- rbind(Data_1_Fiber_Area, Data_2_Fiber_Area, Data_3_Fiber_Area)

i <- 1
j <- 1
while (i >= -0.2) {
  FA_bin_100[j, 1] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Elipse_Position` == "100%"), ][with(
    All_fiber_area[with(All_fiber_area, `Elipse_Position` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))
  FA_bin_80[j, 1] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Elipse_Position` == "50%"), ][with(
    All_fiber_area[with(All_fiber_area, `Elipse_Position` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))
  FA_bin_45[j, 1] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Elipse_Position` == "25%"), ][with(
    All_fiber_area[with(All_fiber_area, `Elipse_Position` == "25%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))

  FA_bin_100[j, 2] <- sd(as.matrix(All_fiber_area[with(All_fiber_area, `Elipse_Position` == "100%"), ][with(
    All_fiber_area[with(All_fiber_area, `Elipse_Position` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))
  FA_bin_80[j, 2] <- sd(as.matrix(All_fiber_area[with(All_fiber_area, `Elipse_Position` == "50%"), ][with(
    All_fiber_area[with(All_fiber_area, `Elipse_Position` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))
  FA_bin_45[j, 2] <- sd(as.matrix(All_fiber_area[with(All_fiber_area, `Elipse_Position` == "25%"), ][with(
    All_fiber_area[with(All_fiber_area, `Elipse_Position` == "25%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))

  FA_bin_100[j, 3] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Elipse_Position` == "100%"), ][with(
    All_fiber_area[with(All_fiber_area, `Elipse_Position` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][1]))
  FA_bin_80[j, 3] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Elipse_Position` == "50%"), ][with(
    All_fiber_area[with(All_fiber_area, `Elipse_Position` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][1]))
  FA_bin_45[j, 3] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Elipse_Position` == "25%"), ][with(
    All_fiber_area[with(All_fiber_area, `Elipse_Position` == "25%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][1]))
  j <- j + 1
  i <- i - 0.1
}

FA_bin_100 <- round(FA_bin_100, 3)
FA_bin_100[3] <- round(FA_bin_100[3], 2)
FA_bin_80 <- round(FA_bin_80, 3)
FA_bin_80[3] <- round(FA_bin_80[3], 2)
FA_bin_45 <- round(FA_bin_45, 3)
FA_bin_45[3] <- round(FA_bin_45[3], 2)

names(FA_bin_100)[1] <- "Area"
names(FA_bin_100)[2] <- "SD"
names(FA_bin_100)[3] <- "Relative_Position"
names(FA_bin_80)[1] <- "Area"
names(FA_bin_80)[2] <- "SD"
names(FA_bin_80)[3] <- "Relative_Position"
names(FA_bin_45)[1] <- "Area"
names(FA_bin_45)[2] <- "SD"
names(FA_bin_45)[3] <- "Relative_Position"

P5 <- ggplot(FA_bin_100, aes(Relative_Position, Area)) +
  geom_smooth(method = "gam", color = "violetred4", se = FALSE, linetype = "solid") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT polygon area") +
  xlim(-0.2, 1)
P5 <- P5 + geom_smooth(data = FA_bin_80, aes(Relative_Position, Area), color = "royalblue4", se = FALSE, linetype = "solid")
P5 <- P5 + geom_smooth(data = FA_bin_45, aes(Relative_Position, Area), color = "springgreen4", se = FALSE, linetype = "solid")
print(P5)

# Neighborhood density  -------------------------------------------------------------------------------------------------------

All_fiber_focus <- rbind(Data_1_N_Density, Data_2_N_Density, Data_3_N_Density)

ND_bin_100 <- data.frame()
ND_bin_80 <- data.frame()
ND_bin_45 <- data.frame()

i <- 1
j <- 1
while (i >= -0.2) {
  ND_bin_100[j, 1] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "100%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))
  ND_bin_80[j, 1] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "50%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))
  ND_bin_45[j, 1] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "25%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "25%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))

  ND_bin_100[j, 2] <- sd(as.matrix(All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "100%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))
  ND_bin_80[j, 2] <- sd(as.matrix(All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "50%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))
  ND_bin_45[j, 2] <- sd(as.matrix(All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "25%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "25%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))

  ND_bin_100[j, 3] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "100%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][6]))
  ND_bin_80[j, 3] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "50%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][6]))
  ND_bin_45[j, 3] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "25%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Elipse_Position` == "25%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][6]))
  j <- j + 1
  i <- i - 0.1
}

ND_bin_100 <- round(ND_bin_100, 3)
ND_bin_100[3] <- round(ND_bin_100[3], 2)
ND_bin_80 <- round(ND_bin_80, 3)
ND_bin_80[3] <- round(ND_bin_80[3], 2)
ND_bin_45 <- round(ND_bin_45, 3)
ND_bin_45[3] <- round(ND_bin_45[3], 2)

names(ND_bin_100)[1] <- "Density"
names(ND_bin_100)[2] <- "SD"
names(ND_bin_100)[3] <- "Relative_Position"
names(ND_bin_80)[1] <- "Density"
names(ND_bin_80)[2] <- "SD"
names(ND_bin_80)[3] <- "Relative_Position"
names(ND_bin_45)[1] <- "Density"
names(ND_bin_45)[2] <- "SD"
names(ND_bin_45)[3] <- "Relative_Position"

P6 <- ggplot(ND_bin_100, aes(Relative_Position, `Density`)) +
  geom_smooth(color = "violetred4", se = FALSE, linetype = "solid") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT focusing factor [%]") +
  xlim(-0.2, 1)
P6 <- P6 + geom_smooth(data = ND_bin_80, aes(Relative_Position, `Density`), color = "royalblue4", se = FALSE, linetype = "solid")
P6 <- P6 + geom_smooth(data = ND_bin_45, aes(Relative_Position, `Density`), color = "springgreen4", se = FALSE, linetype = "solid")
print(P6)

P7 <- ggplot(ND_bin_100, aes(`Density`)) +
  geom_density(kernel = "gaussian", color = "violetred4", linetype = "solid", size = 1) +
  theme_classic() +
  xlab("KMT focusing factor [%]") +
  ylab("No. of k-fibers [%]")
P7 <- P7 + geom_density(data = ND_bin_80, aes(`Density`), kernel = "gaussian", color = "royalblue4", linetype = "solid", size = 1)
P7 <- P7 + geom_density(data = ND_bin_45, aes(`Density`), kernel = "gaussian", color = "springgreen4", linetype = "solid", size = 1)
print(P7)
