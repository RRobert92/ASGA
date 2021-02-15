###########################################################################################################################
# Plots for k-fiber based on unified position model
#
# Model is based on ellipse and PED models. Both models are applied and checked which fiber fit to the prediction of both
# models, and a fiber which shows different prediction are evaluated. The is based primaly on the ellipse model, and corrected
# using the PED model by evaluating the position of a kinetochore in the PED model.
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
###########################################################################################################################

# Number of KMTs per kinetochore ------------------------------------------------------------------------------------------
Fiber_1 <- Data_1_KMT_Total_Curv %>% distinct(`k-fiber no.`, `Elipse Position`, `(+) Dist-to-Pole`)
Fiber_2 <- Data_2_KMT_Total_Curv %>% distinct(`k-fiber no.`, `Elipse Position`, `(+) Dist-to-Pole`)
Fiber_3 <- Data_3_KMT_Total_Curv %>% distinct(`k-fiber no.`, `Elipse Position`, `(+) Dist-to-Pole`)

Data_1_KMT_No <- cbind(Data_1_KMT_No["KMTs_per_kinetochore"], Fiber_1["Elipse Position"], Fiber_1["(+) Dist-to-Pole"])
Data_2_KMT_No <- cbind(Data_2_KMT_No["KMTs_per_kinetochore"], Fiber_2["Elipse Position"], Fiber_2["(+) Dist-to-Pole"])
Data_3_KMT_No <- cbind(Data_3_KMT_No["KMTs_per_kinetochore"], Fiber_3["Elipse Position"], Fiber_3["(+) Dist-to-Pole"])

for (j in 1:NUM_FILES) {
  LD100max <- max(get(paste("Data", j, "KMT_No", sep = "_"))$`(+) Dist-to-Pole`)
  LD100min <- min(get(paste("Data", j, "KMT_No", sep = "_"))$`(+) Dist-to-Pole`)
  LD80 <- ((70 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  LD45 <- ((50 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  df <- data.frame()

  for (i in 1:nrow(get(paste("Data", j, "KMT_No", sep = "_")))) {
    df[i, 1] <- round(((get(paste("Data", j, "KMT_No", sep = "_"))[i, 3] - LD100min) / (LD100max - LD100min)) * 100, 0)
  }
  names(df)[1] <- "PED"

  assign(
    paste("Data", j, "KMT_No", sep = "_"),
    cbind(
      get(paste("Data", j, "KMT_No", sep = "_")),
      df
    )
  )

  df <- data.frame()
  test <- data.frame()
  for (i in 1:nrow(get(paste("Data", j, "KMT_No", sep = "_")))) {
    if (get(paste("Data", j, "KMT_No", sep = "_"))[i, "PED"] <= 50) {
      test <- "25%"
    } else if (get(paste("Data", j, "KMT_No", sep = "_"))[i, "PED"] >= 75) {
      test <- "100%"
    } else {
      test <- "50%"
    }

    if (test == get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"]) {
      df[i, 1] <- get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"]
    } else if (test != get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"] &&
      get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"] == "25%"&&
      get(paste("Data", j, "KMT_No", sep = "_"))[i, "PED"] >= 45) {
      df[i, 1] <- "50%"
    } else if (test != get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"] &&
      get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"] == "50%"&&
      get(paste("Data", j, "KMT_No", sep = "_"))[i, "PED"] >= 65){
      df[i, 1] <- "100%"
    }  else if (test != get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"] &&
      get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"] == "50%"&&
      get(paste("Data", j, "KMT_No", sep = "_"))[i, "PED"] <= 45){
      df[i, 1] <- "25%"
    } else if (test != get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"] &&
      get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"] == "100%" &&
      get(paste("Data", j, "KMT_No", sep = "_"))[i, "PED"] <= 50) {
      df[i, 1] <- "50%"
    } else {
      df[i, 1] <- get(paste("Data", j, "KMT_No", sep = "_"))[i, "Elipse Position"]
    }

    names(df)[1] <- "Uni_Model"
  }

  assign(
    paste("Data", j, "KMT_No", sep = "_"),
    cbind(
      get(paste("Data", j, "KMT_No", sep = "_")),
      df
    )
  )
}
rm(Fiber_1, Fiber_2, Fiber_3, df)


All_KMT_No <- rbind(Data_1_KMT_No, Data_2_KMT_No, Data_3_KMT_No)
sd(All_KMT_No[with(All_KMT_No, `Uni_Model` == "25%"), ]$KMTs_per_kinetochore)
sd(All_KMT_No[with(All_KMT_No, `Uni_Model` == "50%"), ]$KMTs_per_kinetochore)
sd(All_KMT_No[with(All_KMT_No, `Uni_Model` == "100%"), ]$KMTs_per_kinetochore)
nrow(All_KMT_No[with(All_KMT_No, `Uni_Model` == "25%"), ])
nrow(All_KMT_No[with(All_KMT_No, `Uni_Model` == "50%"), ])
nrow(All_KMT_No[with(All_KMT_No, `Uni_Model` == "100%"), ])

P1 <- ggplot(All_KMT_No[with(All_KMT_No, `Uni_Model` == "100%"), ], aes("Outer", KMTs_per_kinetochore)) +
  geom_quasirandom(color = "violetred4", method = "tukeyDense", size = 1) +
  theme_classic() +
  xlab("Data-set names") +
  ylab("Number of KMTs per kinetochore") +
  ylim(0, 20)

P1 <- P1 + geom_quasirandom(
  data = All_KMT_No[with(All_KMT_No, `Uni_Model` == "50%"), ], aes("Middle", KMTs_per_kinetochore),
  color = "royalblue4",method = "tukeyDense", size = 1)

P1 <- P1 + geom_quasirandom(
  data = All_KMT_No[with(All_KMT_No, `Uni_Model` == "25%"), ], aes("Inner", KMTs_per_kinetochore),
  color = "springgreen4", method = "tukeyDense", size = 1)
print(P1)

t <- rbind(All_KMT_No[with(All_KMT_No, Uni_Model == "100%"), ], All_KMT_No[with(All_KMT_No, Uni_Model == "25%"), ])
t_anova <- aov(KMTs_per_kinetochore ~ Uni_Model, t)
summary(t_anova)

for (i in 1:NUM_FILES) {
  assign(
    "df",
    cbind(
      get(paste("Data_", i, "_KMT_Pole", sep = ""))[1],
      get(paste("Data_", i, "_KMT_No", sep = ""))["Uni_Model"]
    )
  )
  names(df)[2] <- "Uni_Model"
  assign(
    paste("Data_", i, "_KMT_Pole", sep = ""),
    df
  )
}

All_KMT_Pole <- rbind(Data_1_KMT_Pole, Data_2_KMT_Pole, Data_3_KMT_Pole)
P1.1 <- ggplot(All_KMT_Pole[with(All_KMT_Pole, `Uni_Model` == "100%"), ], aes("Outer", KMTs_at_the_Pole)) +
  geom_quasirandom(color = "violetred4", method = "tukeyDense", size = 1) +
  theme_classic() +
  xlab("Data-set names") +
  ylab("Number of KMTs per kinetochore") +
  ylim(0, 20) 
P1.1 <- P1.1 + geom_quasirandom(
  data = All_KMT_Pole[with(All_KMT_Pole, `Uni_Model` == "50%"), ], aes("Middle", KMTs_at_the_Pole),
  color = "royalblue4", method = "tukeyDense", size = 1) 
P1.1 <- P1.1 + geom_quasirandom(
  data = All_KMT_Pole[with(All_KMT_Pole, `Uni_Model` == "25%"), ], aes("Inner", KMTs_at_the_Pole),
  color = "springgreen4", method = "tukeyDense", size = 1)

print(P1.1)

t <- rbind(All_KMT_Pole[with(All_KMT_Pole, Uni_Model == "100%"), ], All_KMT_Pole[with(All_KMT_Pole, Uni_Model == "25%"), ])
t_anova <- aov(KMTs_at_the_Pole ~ Uni_Model, t)
summary(t_anova)
sd(All_KMT_Pole[with(All_KMT_Pole, `Uni_Model` == "25%"), ]$KMTs_at_the_Pole)
sd(All_KMT_Pole[with(All_KMT_Pole, `Uni_Model` == "50%"), ]$KMTs_at_the_Pole)
sd(All_KMT_Pole[with(All_KMT_Pole, `Uni_Model` == "100%"), ]$KMTs_at_the_Pole)
nrow(All_KMT_Pole[with(All_KMT_Pole, `Uni_Model` == "25%"), ])
nrow(All_KMT_Pole[with(All_KMT_Pole, `Uni_Model` == "50%"), ])
nrow(All_KMT_Pole[with(All_KMT_Pole, `Uni_Model` == "100%"), ])
# Length distribution -----------------------------------------------------------------------------------------------------

for (j in 1:NUM_FILES) {
  LD100max <- max(get(paste("Data", j, "LD", sep = "_"))$`plus_dist_to_pole`)
  LD100min <- min(get(paste("Data", j, "LD", sep = "_"))$`plus_dist_to_pole`)
  LD80 <- ((70 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  LD45 <- ((50 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  df <- data.frame()

  for (i in 1:nrow(get(paste("Data", j, "LD", sep = "_")))) {
    df[i, 1] <- round(((get(paste("Data", j, "LD", sep = "_"))[i, "plus_dist_to_pole"] - LD100min) / (LD100max - LD100min)) * 100, 0)
  }
  names(df)[1] <- "PED"

  assign(
    paste("Data", j, "LD", sep = "_"),
    cbind(
      get(paste("Data", j, "LD", sep = "_")),
      df
    )
  )

  df <- data.frame()
  test <- data.frame()
  for (i in 1:nrow(get(paste("Data", j, "LD", sep = "_")))) {
    if (get(paste("Data", j, "LD", sep = "_"))[i, "PED"] <= 50) {
      test <- "25%"
    } else if (get(paste("Data", j, "LD", sep = "_"))[i, "PED"] >= 70) {
      test <- "100%"
    } else {
      test <- "50%"
    }

    if (test == get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"]) {
      df[i, 1] <- get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"]
    } else if (test != get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"] == "25%" &&
      get(paste("Data", j, "LD", sep = "_"))[i, "PED"] >= 45) {
      df[i, 1] <- "50%"
    } else if (test != get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"] == "50%" &&
      get(paste("Data", j, "LD", sep = "_"))[i, "PED"] >= 65) {
      df[i, 1] <- "100%"
    } else if (test != get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"] == "50%" &&
      get(paste("Data", j, "LD", sep = "_"))[i, "PED"] <= 45) {
      df[i, 1] <- "25%"
    }else if (test != get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"] == "100%" &&
      get(paste("Data", j, "LD", sep = "_"))[i, "PED"] <= 50) {
      df[i, 1] <- "50%"
    } else {
      df[i, 1] <- get(paste("Data", j, "LD", sep = "_"))[i, "Elipse_Position"]
    }

    names(df)[1] <- "Uni_Model"
  }

  assign(
    paste("Data", j, "LD", sep = "_"),
    cbind(
      get(paste("Data", j, "LD", sep = "_")),
      df
    )
  )
}


All_LD <- rbind(Data_1_LD, Data_2_LD, Data_3_LD)

P2 <- ggplot(All_LD[with(All_LD, `Uni_Model` == "100%"), ], aes("Outer", length)) +
  geom_quasirandom(method = "tukeyDense", size = 1, color = "violetred4") +
  theme_classic() +
  xlab("KMT lengths") +
  ylab("KMT density [Gaussian Kernal density]") + ylim(0,10)

P2 <- P2 + geom_quasirandom(
  data = All_LD[with(All_LD, `Uni_Model` == "50%"), ],
  aes("Middle", length), method = "tukeyDense", size = 1, color = "royalblue4")

P2 <- P2 + geom_quasirandom(
  data = All_LD[with(All_LD, `Uni_Model` == "25%"), ],
  aes("Inner", length), method = "tukeyDense", size = 1, color = "springgreen4")
print(P2)

P2 <- ggplot(All_LD[with(All_LD, `Uni_Model` == "100%"), ], aes(length)) +
  geom_density(size = 1, color = "violetred4") +
  theme_classic() +
  xlab("KMT lengths") +
  ylab("KMT density [Gaussian Kernal density]")

P2 <- P2 + geom_density(
  data = All_LD[with(All_LD, `Uni_Model` == "50%"), ],
  aes(length), size = 1, color = "royalblue4")

P2 <- P2 + geom_density(
  data = All_LD[with(All_LD, `Uni_Model` == "25%"), ],
  aes(length), size = 1, color = "springgreen4")
print(P2)

sd(All_LD[with(All_LD, `Uni_Model` == "25%"), ]$length)
sd(All_LD[with(All_LD, `Uni_Model` == "50%"), ]$length)
sd(All_LD[with(All_LD, `Uni_Model` == "100%"), ]$length)
nrow(All_LD[with(All_LD, `Uni_Model` == "25%"), ])
nrow(All_LD[with(All_LD, `Uni_Model` == "50%"), ])
nrow(All_LD[with(All_LD, `Uni_Model` == "100%"), ])

t <- rbind(All_LD[with(All_LD, Uni_Model == "50%"), ], All_LD[with(All_LD, Uni_Model == "25%"), ])
t_anova <- aov(length ~ Uni_Model, t)
summary(t_anova)

# Total curvature  -------------------------------------------------------------------------------------------------------


for (j in 1:NUM_FILES) {
  LD100max <- max(get(paste("Data", j, "KMT_Total_Curv", sep = "_"))$`(+) Dist-to-Pole`)
  LD100min <- min(get(paste("Data", j, "KMT_Total_Curv", sep = "_"))$`(+) Dist-to-Pole`)
  LD80 <- ((70 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  LD45 <- ((50 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  df <- data.frame()

  for (i in 1:nrow(get(paste("Data", j, "KMT_Total_Curv", sep = "_")))) {
    df[i, 1] <- round(((get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "(+) Dist-to-Pole"] - LD100min) / (LD100max - LD100min)) * 100, 0)
  }
  names(df)[1] <- "PED"

  assign(
    paste("Data", j, "KMT_Total_Curv", sep = "_"),
    cbind(
      get(paste("Data", j, "KMT_Total_Curv", sep = "_")),
      df
    )
  )

  df <- data.frame()
  test <- data.frame()
  for (i in 1:nrow(get(paste("Data", j, "KMT_Total_Curv", sep = "_")))) {
    if (get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "PED"] <= 50) {
      test <- "25%"
    } else if (get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "PED"] >= 70) {
      test <- "100%"
    } else {
      test <- "50%"
    }

    if (test == get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"]) {
      df[i, 1] <- get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"]
    } else if (test != get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"] &&
      get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"] == "25%" &&
      get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "PED"] >= 45) {
      df[i, 1] <- "50%"
    } else if (test != get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"] &&
      get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"] == "50%" &&
      get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "PED"] >= 65) {
      df[i, 1] <- "100%"
    } else if (test != get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"] &&
      get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"] == "50%" &&
      get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "PED"] <= 45) {
      df[i, 1] <- "25%"
    } else if (test != get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"] &&
      get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"] == "100%" &&
      get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "PED"] <= 50) {
      df[i, 1] <- "50%"
    } else {
      df[i, 1] <- get(paste("Data", j, "KMT_Total_Curv", sep = "_"))[i, "Elipse Position"]
    }

    names(df)[1] <- "Uni_Model"
  }

  assign(
    paste("Data", j, "KMT_Total_Curv", sep = "_"),
    cbind(
      get(paste("Data", j, "KMT_Total_Curv", sep = "_")),
      df
    )
  )
}

All_T_Curv <- rbind(Data_1_KMT_Total_Curv, Data_2_KMT_Total_Curv, Data_3_KMT_Total_Curv)

P3 <- ggplot(All_T_Curv[with(All_T_Curv, `Uni_Model` == "100%"), ], aes("Outer", Curvature)) +
  geom_quasirandom(method = "tukeyDense", size = 1, color = "violetred4") +
  theme_classic() +
  xlab("KMT curvatrure") +
  ylab("KMT density [Gaussian Kernal density]")

P3 <- P3 + geom_quasirandom(
  data = All_T_Curv[with(All_T_Curv, `Uni_Model` == "50%"), ],
  aes("Middle", Curvature), method = "tukeyDense", size = 1, color = "royalblue4"
)
P3 <- P3 + geom_quasirandom(
  data = All_T_Curv[with(All_T_Curv, `Uni_Model` == "25%"), ],
  aes("Inner", Curvature), method = "tukeyDense", size = 1, color = "springgreen4"
)

print(P3)

P3.1 <- ggplot(All_T_Curv[with(All_T_Curv, `Uni_Model` == "100%"), ], aes(Curvature)) +
  geom_density(size = 1, color = "violetred4") +
  theme_classic()

P3.1 <- P3.1 + geom_density(
  data = All_T_Curv[with(All_T_Curv, `Uni_Model` == "50%"), ], aes(Curvature),
  size = 1, color = "royalblue4"
)
P3.1 <- P3.1 + geom_density(
  data = All_T_Curv[with(All_T_Curv, `Uni_Model` == "25%"), ], aes(Curvature),
  size = 1, color = "springgreen4"
)

print(P3.1)

P3.2 <- ggplot(All_T_Curv[with(All_T_Curv, `Uni_Model` == "100%"), ], aes(`KMTs length` ,Curvature)) +
  geom_smooth(size = 1, color = "violetred4", method = "loess") +
  theme_classic()

P3.2 <- P3.2 + geom_smooth(
  data = All_T_Curv[with(All_T_Curv, `Uni_Model` == "50%"), ], aes(`KMTs length`, Curvature),
  size = 1, color = "royalblue4", method = "loess"
)
P3.2 <- P3.2 + geom_smooth(
  data = All_T_Curv[with(All_T_Curv, `Uni_Model` == "25%" & `KMTs length` < 5), ], aes(`KMTs length`, Curvature),
  size = 1, color = "springgreen4", method = "loess"
)

print(P3.2)

t <- rbind(All_T_Curv[with(All_T_Curv, Uni_Model == "50%"), ], All_T_Curv[with(All_T_Curv, Uni_Model == "25%"), ])
t_anova <- aov(Curvature ~ Uni_Model, t)
summary(t_anova)

sd(All_T_Curv[with(All_T_Curv, `Uni_Model` == "25%"), ]$Curvature)
sd(All_T_Curv[with(All_T_Curv, `Uni_Model` == "50%"), ]$Curvature)
sd(All_T_Curv[with(All_T_Curv, `Uni_Model` == "100%"), ]$Curvature)
nrow(All_T_Curv[with(All_T_Curv, `Uni_Model` == "25%"), ])
nrow(All_T_Curv[with(All_T_Curv, `Uni_Model` == "50%"), ])
nrow(All_T_Curv[with(All_T_Curv, `Uni_Model` == "100%"), ])

# Local curvature  -------------------------------------------------------------------------------------------------------

for (j in 1:NUM_FILES) {
  LD100max <- max(get(paste("Data", j, "KMT_Local_Curv", sep = "_"))$`End_to_Pole`)
  LD100min <- min(get(paste("Data", j, "KMT_Local_Curv", sep = "_"))$`End_to_Pole`)
  LD80 <- ((70 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  LD45 <- ((50 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  df <- data.frame()

  for (i in 1:nrow(get(paste("Data", j, "KMT_Local_Curv", sep = "_")))) {
    df[i, 1] <- round(((get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "End_to_Pole"] - LD100min) / (LD100max - LD100min)) * 100, 0)
  }
  names(df)[1] <- "PED"

  assign(
    paste("Data", j, "KMT_Local_Curv", sep = "_"),
    cbind(
      get(paste("Data", j, "KMT_Local_Curv", sep = "_")),
      df
    )
  )

  df <- data.frame()
  test <- data.frame()
  for (i in 1:nrow(get(paste("Data", j, "KMT_Local_Curv", sep = "_")))) {
    if (get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "PED"] <= 50) {
      test <- "25%"
    } else if (get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "PED"] >= 75) {
      test <- "100%"
    } else {
      test <- "50%"
    }

    if (test == get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"]) {
      df[i, 1] <- get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"]
    } else if (test != get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"] == "25%" &&
      get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "PED"] >= 75) {
      df[i, 1] <- "50%"
    } else if (test != get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"] == "25%" &&
      get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "PED"] < 75) {
      df[i, 1] <- "25%"
    } else if (test != get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"] == "50%" &&
      get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "PED"] >= 75) {
      df[i, 1] <- "100%"
    } else if (test != get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"] == "50%" &&
      get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "PED"] < 45) {
      df[i, 1] <- "25%"
    } else if (test != get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"] == "100%" &&
      get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "PED"] <= 50) {
      df[i, 1] <- "50%"
    } else {
      df[i, 1] <- get(paste("Data", j, "KMT_Local_Curv", sep = "_"))[i, "Elipse_Position"]
    }
    names(df)[1] <- "Uni_Model"
  }

  assign(
    paste("Data", j, "KMT_Local_Curv", sep = "_"),
    cbind(
      get(paste("Data", j, "KMT_Local_Curv", sep = "_")),
      df
    )
  )
}

All_L_Curv <- rbind(Data_1_KMT_Local_Curv, Data_2_KMT_Local_Curv, Data_3_KMT_Local_Curv)

LC_bin_100 <- data.frame()
LC_bin_80 <- data.frame()
LC_bin_45 <- data.frame()

i <- 1
j <- 1

while (i >= -0.2) {
  LC_bin_100[j, 1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Uni_Model` == "100%"), ][with(
    All_L_Curv[with(All_L_Curv, `Uni_Model` == "100%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))
  LC_bin_80[j, 1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Uni_Model` == "50%"), ][with(
    All_L_Curv[with(All_L_Curv, `Uni_Model` == "50%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))
  LC_bin_45[j, 1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Uni_Model` == "25%"), ][with(
    All_L_Curv[with(All_L_Curv, `Uni_Model` == "25%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))

  LC_bin_100[j, 2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `Uni_Model` == "100%"), ][with(
    All_L_Curv[with(All_L_Curv, `Uni_Model` == "100%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))
  LC_bin_80[j, 2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `Uni_Model` == "50%"), ][with(
    All_L_Curv[with(All_L_Curv, `Uni_Model` == "50%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))
  LC_bin_45[j, 2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `Uni_Model` == "25%"), ][with(
    All_L_Curv[with(All_L_Curv, `Uni_Model` == "25%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][1]))

  LC_bin_100[j, 3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Uni_Model` == "100%"), ][with(
    All_L_Curv[with(All_L_Curv, `Uni_Model` == "100%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][2]))
  LC_bin_80[j, 3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Uni_Model` == "50%"), ][with(
    All_L_Curv[with(All_L_Curv, `Uni_Model` == "50%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][2]))
  LC_bin_45[j, 3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Uni_Model` == "25%"), ][with(
    All_L_Curv[with(All_L_Curv, `Uni_Model` == "25%"), ],
    Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)
  ), ][2]))

  j <- j + 1
  i <- i - 0.1
}

LC_bin_100 <- na.omit(LC_bin_100)
LC_bin_80 <- na.omit(LC_bin_80)
LC_bin_45 <- na.omit(LC_bin_45[1:9, ])

P4 <- ggplot(LC_bin_100, aes(V3, V1)) +
  geom_smooth(color = "violetred4", se = F) +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT Curvature ratio") +
  P4 <- P4 + geom_smooth(
  data = LC_bin_80,
  aes(V3, V1), color = "royalblue4", se = F
)
P4 <- P4 + geom_smooth(
  data = LC_bin_45,
  aes(V3, V1), color = "springgreen4", se = F
)
print(P4)


P4.1 <- ggplot(All_L_Curv[with(All_L_Curv, `Uni_Model` == "100%"), ], aes(Curvature)) +
  geom_density(kernel = "gaussian", size = 1, color = "violetred4") +
  theme_classic() +
  xlim(1, 1.1)
xlab("KMT curvatrure") + ylab("KMT density [Gaussian Kernal density]") +
  geom_vline(
    data = All_L_Curv[with(All_L_Curv, `Uni_Model` == "100%"), ],
    aes(xintercept = mean(Curvature)), color = "violetred4", linetype = "dashed", size = 1
  )

P4.1 <- P4.1 + geom_density(
  data = All_L_Curv[with(All_L_Curv, `Uni_Model` == "50%"), ],
  aes(Curvature), kernel = "gaussian", size = 1, color = "royalblue4"
) +
  geom_vline(
    data = All_L_Curv[with(All_L_Curv, `Uni_Model` == "50%"), ],
    aes(xintercept = mean(Curvature)), color = "royalblue4", linetype = "dashed", size = 1
  )

P4.1 <- P4.1 + geom_density(
  data = All_L_Curv[with(All_L_Curv, `Uni_Model` == "25%"), ],
  aes(Curvature), kernel = "gaussian", size = 1, color = "springgreen4"
) +
  geom_vline(
    data = All_L_Curv[with(All_L_Curv, `Uni_Model` == "25%"), ],
    aes(xintercept = mean(Curvature)), color = "springgreen4", linetype = "dashed", size = 1
  )

print(P4.1)

# Fiber area  -------------------------------------------------------------------------------------------------------

Data_1_Fiber_Area <- cbind(Data_1_Fiber_Area, Data_1_N_Density["Elipse_Position"], Data_1_N_Density["plus_dist_to_pole"])
Data_2_Fiber_Area <- cbind(Data_2_Fiber_Area, Data_2_N_Density["Elipse_Position"], Data_2_N_Density["plus_dist_to_pole"])
Data_3_Fiber_Area <- cbind(Data_3_Fiber_Area, Data_3_N_Density["Elipse_Position"], Data_3_N_Density["plus_dist_to_pole"])

for (j in 1:NUM_FILES) {
  LD100max <- max(get(paste("Data", j, "Fiber_Area", sep = "_"))$`plus_dist_to_pole`)
  LD100min <- min(get(paste("Data", j, "Fiber_Area", sep = "_"))$`plus_dist_to_pole`)
  LD80 <- ((70 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  LD45 <- ((50 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  df <- data.frame()

  for (i in 1:nrow(get(paste("Data", j, "Fiber_Area", sep = "_")))) {
    df[i, 1] <- round(((get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "plus_dist_to_pole"] - LD100min) / (LD100max - LD100min)) * 100, 0)
  }
  names(df)[1] <- "PED"

  assign(
    paste("Data", j, "Fiber_Area", sep = "_"),
    cbind(
      get(paste("Data", j, "Fiber_Area", sep = "_")),
      df
    )
  )

  df <- data.frame()
  test <- data.frame()
  for (i in 1:nrow(get(paste("Data", j, "Fiber_Area", sep = "_")))) {
    if (get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "PED"] <= 50) {
      test <- "25%"
    } else if (get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "PED"] >= 75) {
      test <- "100%"
    } else {
      test <- "50%"
    }
    
    if (test == get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"]) {
      df[i, 1] <- get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"]
    } else if (test != get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"] == "25%" &&
      get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "PED"] >= 45) {
      df[i, 1] <- "50%"
    } else if (test != get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"] == "50%" &&
      get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "PED"] >= 65) {
      df[i, 1] <- "100%"
    } else if (test != get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"] == "50%" &&
      get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "PED"] <= 45) {
      df[i, 1] <- "25%"
    } else if (test != get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"] &&
      get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"] == "100%" &&
      get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "PED"] <= 50) {
      df[i, 1] <- "50%"
    } else {
      df[i, 1] <- get(paste("Data", j, "Fiber_Area", sep = "_"))[i, "Elipse_Position"]
    }
    names(df)[1] <- "Uni_Model"
  }

  assign(
    paste("Data", j, "Fiber_Area", sep = "_"),
    cbind(
      get(paste("Data", j, "Fiber_Area", sep = "_")),
      df
    )
  )
}

FA_bin_100 <- data.frame()
FA_bin_80 <- data.frame()
FA_bin_45 <- data.frame()
All_fiber_area <- rbind(Data_1_Fiber_Area, Data_2_Fiber_Area, Data_3_Fiber_Area)


PX <- ggplot(All_fiber_area[with(All_fiber_area, Uni_Model == "100%"), ], aes(Relative_position, Alpha_area)) +
  geom_smooth(method = "loess", color = "violetred4") +
  theme_classic()
PX <- PX + geom_smooth(data = All_fiber_area[with(All_fiber_area, Uni_Model == "50%"), ], aes(Relative_position, Alpha_area), method = "loess", color = "royalblue4")
PX <- PX + geom_smooth(data = All_fiber_area[with(All_fiber_area, Uni_Model == "25%"), ], aes(Relative_position, Alpha_area), method = "loess", color = "springgreen4")
print(PX)

PX <- ggplot(All_fiber_area[with(All_fiber_area, Uni_Model == "100%"), ], aes(Relative_position, Roundness)) +
  geom_smooth(method = "loess", color = "violetred4") +
  theme_classic()
PX <- PX + geom_smooth(data = All_fiber_area[with(All_fiber_area, Uni_Model == "50%"), ], aes(Relative_position, Roundness), method = "loess", color = "royalblue4")
PX <- PX + geom_smooth(data = All_fiber_area[with(All_fiber_area, Uni_Model == "25%"), ], aes(Relative_position, Roundness), method = "loess", color = "springgreen4")
print(PX)

t <- rbind(All_fiber_area[with(All_fiber_area, Uni_Model == "50%"), ], All_fiber_area[with(All_fiber_area, Uni_Model == "25%"), ])
t_anova <- aov(Roundness ~ Uni_Model, t)
summary(t_anova)

sd(All_fiber_area[with(All_fiber_area, Uni_Model == "25%"), ]$Alpha_area)
sd(All_fiber_area[with(All_fiber_area, Uni_Model == "50%"), ]$Alpha_area)
sd(All_fiber_area[with(All_fiber_area, Uni_Model == "100%"), ]$Alpha_area)
nrow(All_fiber_area[with(All_fiber_area, Uni_Model == "25%"), ])
nrow(All_fiber_area[with(All_fiber_area, Uni_Model == "50%"), ])
nrow(All_fiber_area[with(All_fiber_area, Uni_Model == "100%"), ])

i <- 1
j <- 1
while (i >= -0.2) {
  FA_bin_100[j, 1] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Uni_Model` == "100%"), ][with(
    All_fiber_area[with(All_fiber_area, `Uni_Model` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))
  FA_bin_80[j, 1] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Uni_Model` == "50%"), ][with(
    All_fiber_area[with(All_fiber_area, `Uni_Model` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))
  FA_bin_45[j, 1] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Uni_Model` == "25%"), ][with(
    All_fiber_area[with(All_fiber_area, `Uni_Model` == "25%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))

  FA_bin_100[j, 2] <- sd(as.matrix(All_fiber_area[with(All_fiber_area, `Uni_Model` == "100%"), ][with(
    All_fiber_area[with(All_fiber_area, `Uni_Model` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))
  FA_bin_80[j, 2] <- sd(as.matrix(All_fiber_area[with(All_fiber_area, `Uni_Model` == "50%"), ][with(
    All_fiber_area[with(All_fiber_area, `Uni_Model` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))
  FA_bin_45[j, 2] <- sd(as.matrix(All_fiber_area[with(All_fiber_area, `Uni_Model` == "25%"), ][with(
    All_fiber_area[with(All_fiber_area, `Uni_Model` == "25%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][2]))

  FA_bin_100[j, 3] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Uni_Model` == "100%"), ][with(
    All_fiber_area[with(All_fiber_area, `Uni_Model` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][1]))
  FA_bin_80[j, 3] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Uni_Model` == "50%"), ][with(
    All_fiber_area[with(All_fiber_area, `Uni_Model` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][1]))
  FA_bin_45[j, 3] <- median(as.matrix(All_fiber_area[with(All_fiber_area, `Uni_Model` == "25%"), ][with(
    All_fiber_area[with(All_fiber_area, `Uni_Model` == "25%"), ],
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

FA_bin_100 <- FA_bin_100
FA_bin_80 <- FA_bin_80
FA_bin_45 <- FA_bin_45

P5 <- ggplot(FA_bin_100, aes(Relative_Position, Area)) +
  geom_smooth(method = "gam", color = "violetred4", se = T, linetype = "solid") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT polygon area") +
  xlim(-0.2, 1)
P5 <- P5 + geom_smooth(data = FA_bin_80, aes(Relative_Position, Area), color = "royalblue4", se = T, linetype = "solid")
P5 <- P5 + geom_smooth(data = FA_bin_45, aes(Relative_Position, Area), color = "springgreen4", se = T, linetype = "solid")
print(P5)

# Neighborhood density  -------------------------------------------------------------------------------------------------------

Data_1_N_Density <- cbind(Data_1_N_Density, Data_1_Fiber_Area["Uni_Model"])
Data_2_N_Density <- cbind(Data_2_N_Density, Data_2_Fiber_Area["Uni_Model"])
Data_3_N_Density <- cbind(Data_3_N_Density, Data_3_Fiber_Area["Uni_Model"])

All_fiber_focus <- rbind(Data_1_N_Density, Data_2_N_Density, Data_3_N_Density)

PX <- ggplot(All_fiber_focus[with(All_fiber_focus, Uni_Model == "100%"), ], aes(Relative_position, `Focused KMTs %`)) +
  geom_smooth(method = "loess", color = "violetred4") +
  theme_classic()
PX <- PX + geom_smooth(data = All_fiber_focus[with(All_fiber_focus, Uni_Model == "50%"), ], aes(Relative_position, `Focused KMTs %`), method = "loess", color = "royalblue4")
PX <- PX + geom_smooth(data = All_fiber_focus[with(All_fiber_focus, Uni_Model == "25%"), ], aes(Relative_position, `Focused KMTs %`), method = "loess", color = "springgreen4")
print(PX)

t <- rbind(All_fiber_focus[with(All_fiber_focus, Uni_Model == "100%"), ], All_fiber_focus[with(All_fiber_focus, Uni_Model == "50%"), ])
t_anova <- aov(`Focused KMTs %` ~ Uni_Model, t)
summary(t_anova)

ND_bin_100 <- data.frame()
ND_bin_80 <- data.frame()
ND_bin_45 <- data.frame()

sd(All_fiber_focus[with(All_fiber_focus, Uni_Model == "25%"), ]$`Focused KMTs %`)
sd(All_fiber_focus[with(All_fiber_focus, Uni_Model == "50%"), ]$`Focused KMTs %`)
sd(All_fiber_focus[with(All_fiber_focus, Uni_Model == "100%"), ]$`Focused KMTs %`)
nrow(All_fiber_area[with(All_fiber_area, Uni_Model == "25%"), ])
nrow(All_fiber_area[with(All_fiber_area, Uni_Model == "50%"), ])
nrow(All_fiber_area[with(All_fiber_area, Uni_Model == "100%"), ])

i <- 1
j <- 1
while (i >= -0.2) {
  ND_bin_100[j, 1] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "100%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))
  ND_bin_80[j, 1] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "50%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))
  ND_bin_45[j, 1] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "25%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "25%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))

  ND_bin_100[j, 2] <- sd(as.matrix(All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "100%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))
  ND_bin_80[j, 2] <- sd(as.matrix(All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "50%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))
  ND_bin_45[j, 2] <- sd(as.matrix(All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "25%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "25%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][3]))

  ND_bin_100[j, 3] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "100%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "100%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][6]))
  ND_bin_80[j, 3] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "50%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "50%"), ],
    Relative_position <= i & Relative_position >= as.numeric(i - 0.1)
  ), ][6]))
  ND_bin_45[j, 3] <- median(as.matrix(All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "25%"), ][with(
    All_fiber_focus[with(All_fiber_focus, `Uni_Model` == "25%"), ],
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

ND_bin_100 <- ND_bin_100[1:11, 1:3]
ND_bin_80 <- ND_bin_80[1:11, 1:3]
ND_bin_45 <- ND_bin_45[1:11, 1:3]

P6 <- ggplot(ND_bin_100, aes(Relative_Position, `Density`)) +
  geom_smooth(color = "violetred4", se = T, linetype = "solid") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT focusing factor [%]") +
  xlim(-0.1, 1) +
  ylim(50, 110)
P6 <- P6 + geom_smooth(data = ND_bin_80, aes(Relative_Position, `Density`), color = "royalblue4", se = T, linetype = "solid")
P6 <- P6 + geom_smooth(data = ND_bin_45, aes(Relative_Position, `Density`), color = "springgreen4", se = T, linetype = "solid")
print(P6)

P7 <- ggplot(ND_bin_100, aes(`Density`)) +
  geom_density(kernel = "gaussian", color = "violetred4", linetype = "solid", size = 1) +
  theme_classic() +
  xlab("KMT focusing factor [%]") +
  ylab("No. of k-fibers [%]")
P7 <- P7 + geom_density(data = ND_bin_80, aes(`Density`), kernel = "gaussian", color = "royalblue4", linetype = "solid", size = 1)
P7 <- P7 + geom_density(data = ND_bin_45, aes(`Density`), kernel = "gaussian", color = "springgreen4", linetype = "solid", size = 1)
print(P7)

# Nucleation  -------------------------------------------------------------------------------------------------------

for (j in 1:NUM_FILES) {
  LD100max <- max(get(paste("Data", j, "KMTs_minus_seed", sep = "_"))$`Plus_end_dist`)
  LD100min <- min(get(paste("Data", j, "KMTs_minus_seed", sep = "_"))$`Plus_end_dist`)
  LD80 <- ((70 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  LD45 <- ((50 * (LD100max - LD100min)) + (100 * LD100min)) / 100
  df <- data.frame()

  for (i in 1:nrow(get(paste("Data", j, "KMTs_minus_seed", sep = "_")))) {
    df[i, 1] <- round(((get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Plus_end_dist"] - LD100min) / (LD100max - LD100min)) * 100, 0)
  }
  names(df)[1] <- "PED"

  assign(
    paste("Data", j, "KMTs_minus_seed", sep = "_"),
    cbind(
      get(paste("Data", j, "KMTs_minus_seed", sep = "_")),
      df
    )
  )

  df <- data.frame()
  test <- data.frame()
  for (i in 1:nrow(get(paste("Data", j, "KMTs_minus_seed", sep = "_")))) {
    if (get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "PED"] < 50) {
      test <- "25%"
    } else if (get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "PED"] > 75) {
      test <- "100%"
    } else {
      test <- "50%"
    }

    if (test == get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"]) {
      df[i, 1] <- get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"]
    } else if (test != get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"] &&
      get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"] == "25%" &&
      get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "PED"] >= 75) {
      df[i, 1] <- "50%"
    } else if (test != get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"] &&
      get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"] == "25%" &&
      get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "PED"] < 75) {
      df[i, 1] <- "25%"
    } else if (test != get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"] &&
      get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"] == "50%" &&
      get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "PED"] >= 75) {
      df[i, 1] <- "100%"
    } else if (test != get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"] &&
      get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"] == "50%" &&
      get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "PED"] < 45) {
      df[i, 1] <- "25%"
    } else if (test != get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"] &&
      get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"] == "100%" &&
      get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "PED"] <= 50) {
      df[i, 1] <- "50%"
    } else {
      df[i, 1] <- get(paste("Data", j, "KMTs_minus_seed", sep = "_"))[i, "Ellipse"]
    }
    names(df)[1] <- "Uni_Model"
  }

  assign(
    paste("Data", j, "KMTs_minus_seed", sep = "_"),
    cbind(
      get(paste("Data", j, "KMTs_minus_seed", sep = "_")),
      df
    )
  )
}

All_KMTs_minus_seed <- rbind(Data_1_KMTs_minus_seed, Data_2_KMTs_minus_seed, Data_3_KMTs_minus_seed)

PX <- ggplot(All_KMTs_minus_seed[with(All_KMTs_minus_seed, Uni_Model == "100%"), ], aes(Relative_pos, colour = I_class)) +
  geom_density(kernel = "gaussian", size = 1) +
  theme_classic()
PX <- PX + geom_density(data = All_KMTs_minus_seed[with(All_KMTs_minus_seed, Uni_Model == "50%"), ], aes(Relative_pos, colour = I_class), kernel = "gaussian", size = 1)
PX <- PX + geom_density(data = All_KMTs_minus_seed[with(All_KMTs_minus_seed, Uni_Model == "25%"), ], aes(Relative_pos, colour = I_class), kernel = "gaussian", size = 1)
print(PX)

t <- rbind(All_KMTs_minus_seed[with(All_KMTs_minus_seed, Uni_Model == "50%" & I_class == "SMT"), ], All_KMTs_minus_seed[with(All_KMTs_minus_seed, Uni_Model == "25%" & I_class == "SMT"), ])
t_anova <- aov(Relative_pos ~ Uni_Model, t)
summary(t_anova)
