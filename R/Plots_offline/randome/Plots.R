###########################################################################################################################
# Plots for all clollected data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
###########################################################################################################################

# Number of KMTs per kinetochore ------------------------------------------------------------------------------------------

P1 <- ggplot(Data_1_KMT_No, aes("Metaphase #1", KMTs_per_kinetochore)) +
  geom_boxplot(fill = "grey20", color = "black") +
  theme_classic() +
  xlab("Data-set names") +
  ylab("Number of KMTs per kinetochore") +
  ylim(0, 20)
P1 <- P1 + geom_boxplot(data = Data_2_KMT_No, aes("Metaphase #2", KMTs_per_kinetochore), fill = "grey30", color = "black")
P1 <- P1 + geom_boxplot(data = Data_3_KMT_No, aes("Metaphase #3", KMTs_per_kinetochore), fill = "grey40", color = "black")
P1 <- P1 + geom_boxplot(data = Data_4_KMT_No, aes("RPE-1 wt #4", KMTs_per_kinetochore), fill = "grey60", color = "black")
P1 <- P1 + geom_boxplot(data = Data_5_KMT_No, aes("RPE-1 wt #5", KMTs_per_kinetochore), fill = "grey70", color = "black")
P1 <- P1 + geom_boxplot(data = Data_6_KMT_No, aes("U2OS wt #6", KMTs_per_kinetochore), fill = "grey80", color = "black")
P1 <- P1 + geom_boxplot(data = Data_7_KMT_No, aes("U2OS wt #7", KMTs_per_kinetochore), fill = "grey90", color = "black")

All_KMT_No <- rbind(Data_1_KMT_No, Data_2_KMT_No, Data_3_KMT_No)
P1 <- P1 + geom_boxplot(data = All_KMT_No, aes("Avg. HeLa", KMTs_per_kinetochore), fill = "darkred", color = "black", outlier.alpha = 0) +
  geom_jitter(data = All_KMT_No, aes("Avg. HeLa", KMTs_per_kinetochore), alpha = 0.1, size = 1, width = 0.25)

All_KMT_No_RPE <- rbind(Data_4_KMT_No, Data_5_KMT_No)
P1 <- P1 + geom_boxplot(data = All_KMT_No_RPE, aes("Avg. RPE-1", KMTs_per_kinetochore), fill = "darkgreen", color = "black", outlier.alpha = 0) +
  geom_jitter(data = All_KMT_No_RPE, aes("Avg. RPE-1", KMTs_per_kinetochore), alpha = 0.1, size = 1, width = 0.25)

All_KMT_No_U2OS <- rbind(Data_6_KMT_No, Data_7_KMT_No)
P1 <- P1 + geom_boxplot(data = All_KMT_No_U2OS, aes("Avg. U2OS", KMTs_per_kinetochore), fill = "darkorange", color = "black", outlier.alpha = 0) +
  geom_jitter(data = All_KMT_No_U2OS, aes("Avg. U2OS", KMTs_per_kinetochore), alpha = 0.1, size = 1, width = 0.25)

print(P1)

All_KMT_No <- as.data.frame(c(All_KMT_No, 1))
names(All_KMT_No)[2] <- "No"
All_KMT_No_RPE <- as.data.frame(c(All_KMT_No_RPE, 2))
names(All_KMT_No_RPE)[2] <- "No"
All_KMT_No_U2OS <- as.data.frame(c(All_KMT_No_U2OS, 3))
names(All_KMT_No_U2OS)[2] <- "No"

t <- rbind(All_KMT_No_RPE, All_KMT_No_U2OS)
t_anova <- aov(KMTs_per_kinetochore ~ No, t)
summary(t_anova)

ggsave(file = "KMT_no.svg", plot = P1)

# Number of KMTs at the pole ----------------------------------------------------------------------------------------------
P1.1 <- ggplot(Data_1_KMT_Pole, aes("Metaphase #1", KMTs_at_the_Pole)) +
  geom_boxplot(fill = "grey20", color = "black") +
  theme_classic() +
  xlab("Data-set names") +
  ylab("Number of KMTs per kinetochore") +
  ylim(0, 20)
P1.1 <- P1.1 + geom_boxplot(data = Data_2_KMT_Pole, aes("Metaphase #2", KMTs_at_the_Pole), fill = "grey30", color = "black")
P1.1 <- P1.1 + geom_boxplot(data = Data_3_KMT_Pole, aes("Metaphase #3", KMTs_at_the_Pole), fill = "grey40", color = "black")
All_KMT_Pole <- rbind(Data_1_KMT_Pole, Data_2_KMT_Pole, Data_3_KMT_Pole)
P1.1 <- P1.1 + geom_boxplot(data = All_KMT_Pole, aes("Avg. HeLa", KMTs_at_the_Pole), fill = "darkred", color = "black", outlier.alpha = 0) +
  geom_jitter(data = All_KMT_Pole, aes("Avg. HeLa", KMTs_at_the_Pole), alpha = 0.1, size = 1, width = 0.25)
P1.1 <- P1.1 + geom_boxplot(data = Data_5_KMT_Pole_P1, aes("Avg. RPE-1", KMTs_at_the_Pole), fill = "darkgreen", color = "black", outlier.alpha = 0) +
  geom_jitter(data = Data_5_KMT_Pole_P1, aes("Avg. RPE-1", KMTs_at_the_Pole), alpha = 0.1, size = 1, width = 0.25)
P1.1 <- P1.1 + geom_boxplot(data = Data_7_KMT_Pole, aes("Avg. U2OS", KMTs_at_the_Pole), fill = "darkorange", color = "black", outlier.alpha = 0) +
  geom_jitter(data = Data_7_KMT_Pole, aes("Avg. U2OS", KMTs_at_the_Pole), alpha = 0.1, size = 1, width = 0.25)
print(P1.1)

All_KMT_Pole <- as.data.frame(c(All_KMT_Pole, 1))
names(All_KMT_Pole)[2] <- "No"
Data_5_KMT_Pole_P1 <- as.data.frame(c(Data_5_KMT_Pole_P1, 2))
names(Data_5_KMT_Pole_P1)[2] <- "No"
Data_7_KMT_Pole <- as.data.frame(c(Data_7_KMT_Pole, 3))
names(Data_7_KMT_Pole)[2] <- "No"

t <- rbind(All_KMT_Pole, Data_7_KMT_Pole)
t_anova <- aov(KMTs_at_the_Pole ~ No, t)
summary(t_anova)
# Inter kinetochore distance ----------------------------------------------------------------------------------------------

P0 <- ggplot(Data_1_IKD, aes("Metaphase #1", `Inter-kinetochore distance`)) +
  geom_boxplot(fill = "grey20", color = "black") +
  theme_classic() +
  xlab("Data-set names") +
  ylab("IKD (um)")
P0 <- P0 + geom_boxplot(data = Data_2_IKD, aes("Metaphase #2", `Inter-kinetochore distance`), fill = "grey30", color = "black")
P0 <- P0 + geom_boxplot(data = Data_3_IKD, aes("Metaphase #3", `Inter-kinetochore distance`), fill = "grey40", color = "black")
P0 <- P0 + geom_boxplot(data = Data_4_IKD, aes("Metaphase #4", `Inter-kinetochore distance`), fill = "grey50", color = "black")
P0 <- P0 + geom_boxplot(data = Data_5_IKD, aes("Metaphase #5", `Inter-kinetochore distance`), fill = "grey60", color = "black")
P0 <- P0 + geom_boxplot(data = Data_6_IKD, aes("Metaphase #6", `Inter-kinetochore distance`), fill = "grey70", color = "black")
P0 <- P0 + geom_boxplot(data = Data_7_IKD, aes("Metaphase #7", `Inter-kinetochore distance`), fill = "grey80", color = "black")
ALL_IKD <- rbind(Data_1_IKD, Data_2_IKD, Data_3_IKD)
P0 <- P0 + geom_boxplot(data = ALL_IKD, aes("HeLa average", `Inter-kinetochore distance`), fill = "darkred", color = "black")
ALL_IKD <- rbind(Data_4_IKD, Data_5_IKD)
P0 <- P0 + geom_boxplot(data = ALL_IKD, aes("RPE-1 average", `Inter-kinetochore distance`), fill = "darkgreen", color = "black")
ALL_IKD <- rbind(Data_6_IKD, Data_7_IKD)
P0 <- P0 + geom_boxplot(data = ALL_IKD, aes("U2OS average", `Inter-kinetochore distance`), fill = "orange", color = "black")
print(P0)


# Length distribution -----------------------------------------------------------------------------------------------------

P2 <- ggplot(Data_1_LD, aes(length)) +
  geom_density(kernel = "gaussian", size = 1, color = "black", linetype = "dashed") +
  theme_classic() +
  xlab("KMT lengths") +
  ylab("KMT density [Gaussian Kernal density]")
P2 <- P2 + geom_density(data = Data_2_LD, aes(length), kernel = "gaussian", size = 1, color = "black", linetype = "dotdash")
P2 <- P2 + geom_density(data = Data_3_LD, aes(length), kernel = "gaussian", size = 1, color = "black", linetype = "dotted")
All_LD <- rbind(Data_1_LD, Data_2_LD, Data_3_LD)
P2 <- P2 + geom_density(data = All_LD, aes(length), kernel = "gaussian", size = 1, color = "darkred", linetype = "solid")
print(P2)

P2.1 <- ggplot(Data_1_LD, aes("Metaphase #1", length)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "grey20", color = "black") +
  theme_classic() +
  ylab("KMT lengths [um]") +
  ylim(0, 10)
P2.1 <- P2.1 + geom_violin(data = Data_2_LD, aes("Metaphase #2", length), draw_quantiles = c(0.25, 0.5, 0.75), fill = "grey30", color = "black")
P2.1 <- P2.1 + geom_violin(data = Data_3_LD, aes("Metaphase #3", length), draw_quantiles = c(0.25, 0.5, 0.75), fill = "grey40", color = "black")
P2.1 <- P2.1 + geom_violin(data = All_LD, aes("Average", length), draw_quantiles = c(0.25, 0.5, 0.75), fill = "darkred", color = "black")

print(P2.1)

P2.2 <- ggplot(All_LD, aes(length)) +
  geom_density(kernel = "gaussian", size = 1, color = "darkred", linetype = "solid") +
  theme_classic() +
  xlab("KMT lengths") +
  ylab("KMT density [Gaussian Kernal density]")
P2.2 <- P2.2 + geom_density(data = Data_5_LD_P1, aes(length), kernel = "gaussian", size = 1, color = "darkgreen", linetype = "solid")
P2.2 <- P2.2 + geom_density(data = Data_7_LD, aes(length), kernel = "gaussian", size = 1, color = "darkorange", linetype = "solid")
print(P2.2)

P2.3 <- ggplot(All_LD, aes("Average HeLa", length)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "darkred", color = "black") +
  theme_classic() +
  ylab("KMT lengths [um]") +
  ylim(0, 10)
P2.3 <- P2.3 + geom_violin(data = Data_5_LD_P1, aes("RPE-1 wt #5", length), draw_quantiles = c(0.25, 0.5, 0.75), fill = "darkgreen", color = "black")
P2.3 <- P2.3 + geom_violin(data = Data_7_LD, aes("U2OS wt #7", length), draw_quantiles = c(0.25, 0.5, 0.75), fill = "darkorange", color = "black")
print(P2.3)

for (i in 1:NUM_FILES) {
  assign(
    paste("Data_", i, "_LD", sep = ""),
    data.frame(
      Data = as.factor(paste("Metaphase #", i, sep = "")),
      get(paste("Data_", i, "_LD", sep = ""))
    )
  )
}

t <- data.frame(Pole = "Pole")
t <- All_LD %>% filter(Uni_Model == "100%")
t2 <- All_LD %>% filter(Uni_Model == "50%")
t3 <- All_LD %>% filter(Uni_Model == "25%")
t <- data.frame(
  Data = "100%",
  t
)
t2 <- data.frame(
  Data = "50%",
  t2
)
t3 <- data.frame(
  Data = "25%",
  t3
)
t <- rbind(t, t2, t3)

ggscatter(t,
  x = "length", y = "minus_dist_to_pole",
  color = "Data", palette = "jco", add = "reg.line", conf.int = TRUE, alpha = 0.1
)
ggsave(file = "LD.svg", plot = P2)

All_LD <- as.data.frame(c(All_LD, 1))
names(All_LD)[7] <- "No"
Data_5_LD_P1 <- as.data.frame(c(Data_5_LD_P1, 2))
names(Data_5_LD_P1)[7] <- "No"
Data_7_LD <- as.data.frame(c(Data_7_LD, 3))
names(Data_7_LD)[7] <- "No"

t <- rbind(Data_7_LD, Data_5_LD_P1)
t_anova <- aov(length ~ No, t)
summary(t_anova)

# Minus end distribution --------------------------------------------------------------------------------------------------

All_KMT_Minus_Ends <- rbind(Data_1_KMT_Minus_Ends, Data_2_KMT_Minus_Ends, Data_3_KMT_Minus_Ends)

P2.4 <- ggplot(Data_1_KMT_Minus_Ends, aes(Relative_minus_position)) +
  geom_density(kernel = "gaussian", size = 1, linetype = "dashed", color = "black") +
  theme_classic() +
  xlab("KMT minus-end distance from the pole") +
  ylab("KMT density [Gaussian Kernel density]") +
  xlim(-0.2, 1)+ ylim(0, 3)
P2.4 <- P2.4 + geom_density(data = Data_2_KMT_Minus_Ends, aes(Relative_minus_position), linetype = "dotdash", color = "black", kernel = "gaussian", size = 1)
P2.4 <- P2.4 + geom_density(data = Data_3_KMT_Minus_Ends, aes(Relative_minus_position), linetype = "dotdash", color = "black", kernel = "gaussian", size = 1)
P2.4 <- P2.4 + geom_density(data = All_KMT_Minus_Ends, aes(Relative_minus_position), linetype = "solid", color = "red3", kernel = "gaussian", size = 1) +
  geom_vline(data = All_KMT_Minus_Ends, aes(xintercept = median(Relative_minus_position)), color = "red3", linetype = "dashed", size = 1)
print(P2.4)


P2.5 <- ggplot(All_KMT_Minus_Ends, aes(Relative_minus_position)) +
  geom_density(kernel = "gaussian", size = 1, color = "red3", linetype = "solid") +
  theme_classic() +
  xlab("KMT minus-end distance from the pole") +
  ylab("KMT density [Gaussian Kernel density]") +
  geom_vline(data = All_KMT_Minus_Ends, aes(xintercept = median(Relative_minus_position)), color = "red3", linetype = "dashed", size = 1) +
  xlim(-0.2, 1) + ylim(0, 3)
P2.5 <- P2.5 + geom_density(data = Data_5_KMT_Minus_Ends[1:496,], aes(Relative_minus_position), kernel = "gaussian", size = 1, color = "goldenrod2") +
  geom_vline(data = Data_5_KMT_Minus_Ends[1:496,], aes(xintercept = median(Relative_minus_position)), color = "goldenrod2", linetype = "dashed", size = 1)
P2.5 <- P2.5 + geom_density(data = Data_7_KMT_Minus_Ends[1:nrow(Data_7_KMT_Total_Curv_P1),], aes(Relative_minus_position), kernel = "gaussian", size = 1, color = "royalblue2") +
  geom_vline(data = Data_7_KMT_Minus_Ends[1:nrow(Data_7_KMT_Total_Curv_P1),], aes(xintercept = median(Relative_minus_position)), color = "royalblue2", linetype = "dashed", size = 1)
print(P2.5)

Data_5_KMT_Minus_Ends <- Data_5_KMT_Minus_Ends[1:496, ]

P2.5.1 <- ggplot(All_KMT_Minus_Ends, aes(minus_dist_to_pole)) +
  geom_density(kernel = "gaussian", size = 1, color = "darkred", linetype = "solid") +
  theme_classic() +
  xlab("KMT minus-end distance from the pole") +
  ylab("KMT density [Gaussian Kernel density]") +
  geom_vline(data = All_KMT_Minus_Ends, aes(xintercept = median(minus_dist_to_pole)), color = "red", linetype = "dashed", size = 1)
P2.5.1 <- P2.5.1 + geom_density(data = Data_5_KMT_Minus_Ends, aes(minus_dist_to_pole), kernel = "gaussian", size = 1, color = "darkgreen") +
  geom_vline(data = Data_5_KMT_Minus_Ends, aes(xintercept = median(minus_dist_to_pole)), color = "green", linetype = "dashed", size = 1)
P2.5.1 <- P2.5.1 + geom_density(data = Data_7_KMT_Minus_Ends[1:nrow(Data_7_KMT_Total_Curv_P1)], aes(minus_dist_to_pole), kernel = "gaussian", size = 1, color = "darkorange") +
  geom_vline(data = Data_7_KMT_Minus_Ends[1:nrow(Data_7_KMT_Total_Curv_P1)], aes(xintercept = median(minus_dist_to_pole)), color = "orange", linetype = "dashed", size = 1)
print(P2.5.1)

All_KMT_Minus_Ends <- as.data.frame(c(All_KMT_Minus_Ends, 1))
names(All_KMT_Minus_Ends)[3] <- "No"
Data_5_KMT_Minus_Ends <- as.data.frame(c(Data_5_KMT_Minus_Ends, 2))
names(Data_5_KMT_Minus_Ends)[3] <- "No"
Data_7_KMT_Minus_Ends <- as.data.frame(c(Data_7_KMT_Minus_Ends, 3))
names(Data_7_KMT_Minus_Ends)[3] <- "No"

t <- rbind(All_KMT_Minus_Ends, All_KMT_Minus_Ends)
t_anova <- aov(minus_dist_to_pole ~ No, t)
summary(t_anova)

# Total curvature  --------------------------------------------------------------------------------------------------------

P3 <- ggplot(Data_1_KMT_Total_Curv, aes(Curvature)) +
  geom_density(kernel = "gaussian", size = 1, color = "black", linetype = "dashed") +
  theme_classic() +
  xlab("KMT curvatrure") +
  ylab("KMT density [Gaussian Kernal density]") +
  xlim(1, 1.3)
P3 <- P3 + geom_density(data = Data_2_KMT_Total_Curv, aes(Curvature), kernel = "gaussian", size = 1, color = "black", linetype = "dotdash")
P3 <- P3 + geom_density(data = Data_3_KMT_Total_Curv, aes(Curvature), kernel = "gaussian", size = 1, color = "black", linetype = "dotted")
All_T_Curv <- rbind(Data_1_KMT_Total_Curv, Data_2_KMT_Total_Curv, Data_3_KMT_Total_Curv)
P3 <- P3 + geom_density(data = All_T_Curv, aes(Curvature), kernel = "gaussian", size = 1, color = "darkred", linetype = "solid") +
  geom_vline(data = All_T_Curv, aes(xintercept = median(Curvature)), color = "blue", linetype = "dashed", size = 1)

print(P3)

P3 <- ggplot(All_T_Curv, aes(Curvature)) +
  geom_density(kernel = "gaussian", size = 1, color = "darkred", linetype = "solid") +
  theme_classic() +
  xlab("KMT curvatrure") +
  ylab("KMT density [Gaussian Kernal density]") +
  xlim(1, 1.3) +
  geom_vline(data = All_T_Curv, aes(xintercept = median(Curvature)), color = "red", linetype = "dashed", size = 1)
P3 <- P3 + geom_density(data = Data_5_KMT_Total_Curv_P1, aes(Curvature), kernel = "gaussian", size = 1, color = "darkblue", linetype = "solid") +
  geom_vline(data = Data_5_KMT_Total_Curv_P1, aes(xintercept = median(Curvature)), color = "blue", linetype = "dashed", size = 1)
P3 <- P3 + geom_density(data = Data_7_KMT_Total_Curv, aes(Curvature), kernel = "gaussian", size = 1, color = "darkorange", linetype = "solid") +
  geom_vline(data = Data_7_KMT_Total_Curv, aes(xintercept = median(Curvature)), color = "orange", linetype = "dashed", size = 1)

print(P3)

ggsave(file = "Total_curv.svg", plot = P3)

P3 <- ggplot(Data_1_KMT_Total_Curv, aes(`KMTs length`, Curvature)) +
  geom_smooth(method = "loess", size = 1, color = "black", linetype = "dashed", se = F) +
  theme_classic() +
  xlab("KMT length") +
  ylab("KMT curvature ratio")
P3 <- P3 + geom_smooth(data = Data_2_KMT_Total_Curv, aes(`KMTs length`, Curvature), method = "loess", size = 1, color = "black", linetype = "dotdash", se = F)
P3 <- P3 + geom_smooth(data = Data_3_KMT_Total_Curv, aes(`KMTs length`, Curvature), method = "loess", size = 1, color = "black", linetype = "dotted", se = F)
All_T_Curv <- rbind(Data_1_KMT_Total_Curv, Data_2_KMT_Total_Curv, Data_3_KMT_Total_Curv)
P3 <- P3 + geom_smooth(data = All_T_Curv, aes(`KMTs length`, Curvature), method = "loess", size = 1, color = "darkred", linetype = "solid")

print(P3)

P3.1 <- ggplot(All_T_Curv, aes(`KMTs length`, Curvature)) +
  geom_smooth(method = "loess", size = 1, color = "darkred", linetype = "solid", se = T) +
  theme_classic() +
  xlab("KMT length") +
  ylab("KMT curvature ratio") +
  stat_cor()
P3.1 <- P3.1 + geom_smooth(data = Data_5_KMT_Total_Curv_P1, aes(`KMTs length`, Curvature), method = "loess", size = 1, color = "darkblue", linetype = "solid")
P3.1 <- P3.1 + geom_smooth(data = Data_7_KMT_Total_Curv, aes(`KMTs length`, Curvature), method = "loess", size = 1, color = "darkorange", linetype = "solid")
print(P3.1)

All_T_Curv <- as.data.frame(c(All_T_Curv, 1))
names(All_T_Curv)[7] <- "No"
Data_5_KMT_Total_Curv_P1 <- as.data.frame(c(Data_5_KMT_Total_Curv_P1, 2))
names(Data_5_KMT_Total_Curv_P1)[7] <- "No"
Data_7_KMT_Total_Curv <- as.data.frame(c(Data_7_KMT_Total_Curv, 3))
names(Data_7_KMT_Total_Curv)[7] <- "No"

t <- rbind(Data_5_KMT_Total_Curv_P1, Data_7_KMT_Total_Curv)
t_anova <- aov(Curvature ~ No, t)
summary(t_anova)


# Local curvature  -------------------------------------------------------------------------------------------------------

LC_bin_Data_1 <- data.frame()
LC_bin_Data_2 <- data.frame()
LC_bin_Data_3 <- data.frame()
LC_bin_All <- data.frame()
All_L_Curv <- rbind(Data_1_KMT_Local_Curv, Data_2_KMT_Local_Curv, Data_3_KMT_Local_Curv)
LC_bin_Data_5 <- data.frame()
LC_bin_Data_7 <- data.frame()

i <- 1
j <- 1
while (i >= -0.2) {
  LC_bin_Data_1[j, 1] <- median(as.matrix(Data_1_KMT_Local_Curv[with(Data_1_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))
  LC_bin_Data_2[j, 1] <- median(as.matrix(Data_2_KMT_Local_Curv[with(Data_2_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))
  LC_bin_Data_3[j, 1] <- median(as.matrix(Data_3_KMT_Local_Curv[with(Data_3_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))
  LC_bin_Data_5[j, 1] <- median(as.matrix(Data_5_KMT_Local_Curv[with(Data_5_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))
  LC_bin_Data_7[j, 1] <- median(as.matrix(Data_7_KMT_Local_Curv[with(Data_7_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))
  LC_bin_All[j, 1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))

  LC_bin_Data_1[j, 2] <- sd(as.matrix(Data_1_KMT_Local_Curv[with(Data_1_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))
  LC_bin_Data_2[j, 2] <- sd(as.matrix(Data_2_KMT_Local_Curv[with(Data_2_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))
  LC_bin_Data_3[j, 2] <- sd(as.matrix(Data_3_KMT_Local_Curv[with(Data_3_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))
  LC_bin_Data_5[j, 2] <- sd(as.matrix(Data_5_KMT_Local_Curv[with(Data_5_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))
  LC_bin_Data_7[j, 2] <- sd(as.matrix(Data_7_KMT_Local_Curv[with(Data_7_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))
  LC_bin_All[j, 2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][1]))

  LC_bin_Data_1[j, 3] <- median(as.matrix(Data_1_KMT_Local_Curv[with(Data_1_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][2]))
  LC_bin_Data_2[j, 3] <- median(as.matrix(Data_2_KMT_Local_Curv[with(Data_2_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][2]))
  LC_bin_Data_3[j, 3] <- median(as.matrix(Data_3_KMT_Local_Curv[with(Data_3_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][2]))
  LC_bin_Data_5[j, 3] <- median(as.matrix(Data_5_KMT_Local_Curv[with(Data_5_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][2]))
  LC_bin_Data_7[j, 3] <- median(as.matrix(Data_7_KMT_Local_Curv[with(Data_7_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][2]))
  LC_bin_All[j, 3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i - 0.1)), ][2]))

  j <- j + 1
  i <- i - 0.1
}

LC_bin_Data_1 <- round(LC_bin_Data_1, 4)
LC_bin_Data_1[3] <- round(LC_bin_Data_1[3], 2)
LC_bin_Data_2 <- round(LC_bin_Data_2, 4)
LC_bin_Data_2[3] <- round(LC_bin_Data_2[3], 2)
LC_bin_Data_3 <- round(LC_bin_Data_3, 4)
LC_bin_Data_3[3] <- round(LC_bin_Data_3[3], 2)
LC_bin_Data_5 <- round(LC_bin_Data_5, 4)
LC_bin_Data_5[3] <- round(LC_bin_Data_5[3], 2)
LC_bin_Data_7 <- round(LC_bin_Data_7, 4)
LC_bin_Data_7[3] <- round(LC_bin_Data_7[3], 2)
LC_bin_All <- round(LC_bin_All, 4)
LC_bin_All[3] <- round(LC_bin_All[3], 2)

names(LC_bin_Data_1)[1] <- "Curvature"
names(LC_bin_Data_1)[2] <- "SD"
names(LC_bin_Data_1)[3] <- "Relative_Position"
names(LC_bin_Data_2)[1] <- "Curvature"
names(LC_bin_Data_2)[2] <- "SD"
names(LC_bin_Data_2)[3] <- "Relative_Position"
names(LC_bin_Data_3)[1] <- "Curvature"
names(LC_bin_Data_3)[2] <- "SD"
names(LC_bin_Data_3)[3] <- "Relative_Position"
names(LC_bin_Data_5)[1] <- "Curvature"
names(LC_bin_Data_5)[2] <- "SD"
names(LC_bin_Data_5)[3] <- "Relative_Position"
names(LC_bin_Data_7)[1] <- "Curvature"
names(LC_bin_Data_7)[2] <- "SD"
names(LC_bin_Data_7)[3] <- "Relative_Position"
names(LC_bin_All)[1] <- "Curvature"
names(LC_bin_All)[2] <- "SD"
names(LC_bin_All)[3] <- "Relative_Position"

P4 <- ggplot(Data_1_KMT_Local_Curv, aes(Relative_Position, Curvature)) +
  geom_smooth(method = "gam", color = "black", se = FALSE, linetype = "dashed") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT Curvature ratio")
P4 <- P4 + geom_smooth(data = Data_2_KMT_Local_Curv, aes(Relative_Position, Curvature), color = "black", se = FALSE, linetype = "dotdash")
P4 <- P4 + geom_smooth(data = Data_3_KMT_Local_Curv, aes(Relative_Position, Curvature), color = "black", se = FALSE, linetype = "dotted")
P4 <- P4 + geom_smooth(data = All_L_Curv, aes(Relative_Position, Curvature), color = "darkred", se = T, linetype = "solid") + stat_cor()

print(P4)

P4 <- ggplot(All_L_Curv, aes(Relative_Position, Curvature)) +
  geom_smooth(method = "gam", color = "darkred", se = T, linetype = "solid") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT Curvature ratio") +
  xlim(-0.2, 1)
P4 <- P4 + geom_smooth(data = Data_5_KMT_Local_Curv_P1, aes(Relative_Position, Curvature), color = "blue", se = T, linetype = "solid") 
P4 <- P4 + geom_smooth(data = Data_7_KMT_Local_Curv, aes(Relative_Position, Curvature), color = "orange", se = T, linetype = "solid")+ stat_cor()
print(P4)

All_L_Curv <- as.data.frame(c(All_L_Curv, 1))
names(All_L_Curv)[8] <- "No"
Data_5_KMT_Local_Curv_P1 <- as.data.frame(c(Data_5_KMT_Local_Curv_P1, 2))
names(Data_5_KMT_Local_Curv_P1)[8] <- "No"
Data_7_KMT_Local_Curv <- as.data.frame(c(Data_7_KMT_Local_Curv, 3))
names(Data_7_KMT_Local_Curv)[8] <- "No"

t <- rbind(Data_5_KMT_Local_Curv, Data_7_KMT_Local_Curv)
t_anova <- aov(Curvature ~ No, t)
summary(t_anova)

P4 <- ggplot(LC_bin_Data_1, aes(Relative_Position, Curvature)) +
  geom_smooth(method = "gam", color = "black", se = FALSE, linetype = "dashed") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT Curvature ratio") +
  geom_point()
P4 <- P4 + geom_smooth(data = LC_bin_Data_2, aes(Relative_Position, Curvature), color = "black", se = FALSE, linetype = "dotdash")
P4 <- P4 + geom_smooth(data = LC_bin_Data_3, aes(Relative_Position, Curvature), color = "black", se = FALSE, linetype = "dotted")
P4 <- P4 + geom_smooth(data = LC_bin_All, aes(Relative_Position, Curvature), color = "darkred", se = T, linetype = "solid") + stat_cor()

print(P4)

P4 <- ggplot(LC_bin_All, aes(Relative_Position, Curvature)) +
  geom_smooth(method = "gam", color = "darkred", se = T, linetype = "solid") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT Curvature ratio") +
  xlim(-0.2, 1)
P4 <- P4 + geom_smooth(data = LC_bin_Data_5, aes(Relative_Position, Curvature), color = "blue", se = FALSE, linetype = "solid")
P4 <- P4 + geom_smooth(data = LC_bin_Data_7, aes(Relative_Position, Curvature), color = "orange", se = FALSE, linetype = "solid")
print(P4)

ggsave(file = "Local_curv.svg", plot = P4)

# Fiber area  ------------------------------------------------------------------------------------------------------------

All_fiber_area <- rbind(Data_1_Fiber_Area, Data_2_Fiber_Area, Data_3_Fiber_Area)

P5 <- ggplot(Data_1_Fiber_Area, aes(Relative_position, Alpha_area)) +
  geom_smooth(method = "gam", color = "black", se = FALSE, linetype = "dashed") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT polygon area") +
  xlim(-0.2, 1)
P5 <- P5 + geom_smooth(data = Data_2_Fiber_Area, aes(Relative_position, Alpha_area), color = "black", se = FALSE, linetype = "dotdash")
P5 <- P5 + geom_smooth(data = Data_3_Fiber_Area, aes(Relative_position, Alpha_area), color = "black", se = FALSE, linetype = "dotted")
P5 <- P5 + geom_smooth(data = All_fiber_area, aes(Relative_position, Alpha_area), color = "darkred", se = T, linetype = "solid")
print(P5)

P5 <- ggplot(All_fiber_area, aes(Relative_position, Alpha_area)) +
  geom_smooth(method = "gam", color = "darkred", se = T, linetype = "solid") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT polygon area") +
  xlim(-0.2, 1)
P5 <- P5 + geom_smooth(data = Data_5_Fiber_Area_P1, aes(Relative_position, Alpha_area), color = "blue", se = T, linetype = "solid")
P5 <- P5 + geom_smooth(data = Data_7_Fiber_Area, aes(Relative_position, Alpha_area), color = "orange", se = T, linetype = "solid")
print(P5)

P7.1 <- ggplot(All_fiber_area, aes(Relative_position, Roundness)) +
  geom_smooth(method = "loess", colour = "darkred") +
  stat_cor() +
  theme_classic2() +
  ylim(0, 0.6)
P7.1 <- P7.1 + geom_smooth(data = Data_1_Fiber_Area, aes(Relative_position, Roundness), method = "loess", color = "black", se = FALSE, linetype = "dashed")
P7.1 <- P7.1 + geom_smooth(data = Data_2_Fiber_Area, aes(Relative_position, Roundness), method = "loess", color = "black", se = FALSE, linetype = "dotdash")
P7.1 <- P7.1 + geom_smooth(data = Data_3_Fiber_Area, aes(Relative_position, Roundness), method = "loess", color = "black", se = FALSE, linetype = "dotdash")
print(P7.1)

ggsave(file = "Fiber_area.svg", plot = P5)

All_fiber_area <- as.data.frame(c(All_fiber_area, 1))
names(All_fiber_area)[4] <- "No"
Data_5_Fiber_Area_P1 <- as.data.frame(c(Data_5_Fiber_Area_P1, 2))
names(Data_5_Fiber_Area_P1)[4] <- "No"
Data_7_Fiber_Area <- as.data.frame(c(Data_7_Fiber_Area, 3))
names(Data_7_Fiber_Area)[4] <- "No"

t <- rbind(Data_5_Fiber_Area_P1, Data_7_Fiber_Area)
t_anova <- aov(Curvature ~ No, t)
summary(t_anova)

# Fiber focused  ---------------------------------------------------------------------------------------------------------

All_fiber_focus <- rbind(Data_1_N_Density, Data_2_N_Density, Data_3_N_Density)

P6 <- ggplot(Data_1_N_Density, aes(Relative_position, `Focused KMTs %`)) +
  geom_smooth(color = "black", se = FALSE, linetype = "dashed") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT focusing factor [%]") +
  xlim(-0.2, 1)
P6 <- P6 + geom_smooth(data = Data_2_N_Density, aes(Relative_position, `Focused KMTs %`), color = "black", se = FALSE, linetype = "dotdash")
P6 <- P6 + geom_smooth(data = Data_3_N_Density, aes(Relative_position, `Focused KMTs %`), color = "black", se = FALSE, linetype = "dotted")
P6 <- P6 + geom_smooth(data = All_fiber_focus, aes(Relative_position, `Focused KMTs %`), color = "darkred", se = T, linetype = "solid")
print(P6)

P6 <- ggplot(All_fiber_focus, aes(Relative_position, Focused.KMTs..)) +
  geom_smooth(color = "darkred", se = T, linetype = "solid") +
  theme_classic() +
  xlab("KMT Relative Position") +
  ylab("KMT focusing factor [%]") +
  xlim(-0.2, 1)
P6 <- P6 + geom_smooth(data = Data_5_N_Density_P1, aes(Relative_position, `Focused KMTs %`), color = "blue", se = T, linetype = "solid")
P6 <- P6 + geom_smooth(data = Data_7_N_Density, aes(Relative_position, `Focused KMTs %`), color = "orange", se = T, linetype = "solid")
print(P6)

ggsave(file = "Fiber_focus_distribution.svg", plot = P6)

P7 <- ggplot(Data_1_N_Density, aes(`Focused KMTs %`)) +
  geom_density(kernel = "gaussian", color = "black", linetype = "dashed", size = 1) +
  theme_classic() +
  xlab("KMT focusing factor [%]") +
  ylab("No. of k-fibers [%]")
P7 <- P7 + geom_density(data = Data_2_N_Density, aes(`Focused KMTs %`), kernel = "gaussian", color = "black", linetype = "dotdash", size = 1)
P7 <- P7 + geom_density(data = Data_3_N_Density, aes(`Focused KMTs %`), kernel = "gaussian", color = "black", linetype = "dotted", size = 1)
P7 <- P7 + geom_density(data = All_fiber_focus, aes(`Focused KMTs %`), kernel = "gaussian", color = "darkred", linetype = "solid", size = 1)
print(P7)

P7 <- ggplot(All_fiber_focus, aes(Focused.KMTs..)) +
  geom_density(kernel = "gaussian", color = "darkred", linetype = "solid", size = 1) +
  theme_classic() +
  xlab("KMT focusing factor [%]") +
  ylab("No. of k-fibers [%]")
P7 <- P7 + geom_density(data = Data_5_N_Density_P1, aes(`Focused KMTs %`), kernel = "gaussian", color = "blue", linetype = "solid", size = 1)
P7 <- P7 + geom_density(data = Data_7_N_Density, aes(`Focused KMTs %`), kernel = "gaussian", color = "orange", linetype = "solid", size = 1)
print(P7)
ggsave(file = "Fiber_focus_no.svg", plot = P7)

All_fiber_focus <- as.data.frame(c(All_fiber_focus, 1))
names(All_fiber_focus)[7] <- "No"
names(All_fiber_focus)[3] <- "Focused KMTs %"
Data_5_N_Density_P1 <- as.data.frame(c(Data_5_N_Density_P1, 2))
names(Data_5_N_Density_P1)[7] <- "No"
names(Data_5_N_Density_P1)[3] <- "Focused KMTs %"
Data_7_N_Density <- as.data.frame(c(Data_7_N_Density, 3))
names(Data_7_N_Density)[7] <- "No"
names(Data_7_N_Density)[3] <- "Focused KMTs %"

t <- rbind(Data_5_N_Density_P1, Data_7_N_Density)
t_anova <- aov(`Focused KMTs %` ~ No, t)
summary(t_anova)

# MT nucleation from KMTs  ---------------------------------------------------------------------------------------------------------

All_minus_seed <- rbind(Data_1_KMTs_minus_seed, Data_2_KMTs_minus_seed, Data_3_KMTs_minus_seed)

P8 <- ggplot(All_minus_seed, aes(Relative_pos, colour = I_class)) +
  geom_density(kernel = "gaussian", size = 1) +
  theme_classic()
P8 <- P8 + geom_density(data = Data_1_KMTs_minus_seed, aes(Relative_pos, colour = I_class), kernel = "gaussian", size = 1)
P8 <- P8 + geom_density(data = Data_2_KMTs_minus_seed, aes(Relative_pos, colour = I_class), kernel = "gaussian", size = 1)
P8 <- P8 + geom_density(data = Data_3_KMTs_minus_seed, aes(Relative_pos, colour = I_class), kernel = "gaussian", size = 1)

ggplot(All_minus_seed, aes("1", Relative_pos, colour = I_class)) +
  geom_quasirandom(method = "tukeyDense", size = 1) +
  theme_classic()
print(P8)
