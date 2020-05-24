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

Data_1_KMT_No <- cbind(Data_1_KMT_No, Fiber_1[2])
Data_2_KMT_No <- cbind(Data_2_KMT_No, Fiber_2[2])
Data_3_KMT_No <- cbind(Data_3_KMT_No, Fiber_3[2])
All_KMT_No <- rbind(Data_1_KMT_No, Data_2_KMT_No, Data_3_KMT_No)


P1 <- ggplot(All_KMT_No[with(All_KMT_No, `Elipse Position` == "100%"),], aes("Outer", KMTs_per_kinetochore)) + 
  geom_boxplot(fill = "violetred4", color = "black", outlier.alpha = 0) + theme_classic() +
  xlab("Data-set names") + ylab("Number of KMTs per kinetochore") + 
  geom_jitter(data = All_KMT_No[with(All_KMT_No, `Elipse Position` == "100%"),], aes("Outer", KMTs_per_kinetochore), alpha = 0.1, size = 1, width = 0.25)

P1 <- P1 + geom_boxplot(data = All_KMT_No[with(All_KMT_No, `Elipse Position` == "50%"),], aes("Middle", KMTs_per_kinetochore),
                        fill = "royalblue4", color = "black",
                        outlier.alpha = 0) +
  geom_jitter(data = All_KMT_No[with(All_KMT_No, `Elipse Position` == "50%"),], aes("Middle", KMTs_per_kinetochore), alpha = 0.1, size = 1, width = 0.25)

P1 <- P1 + geom_boxplot(data = All_KMT_No[with(All_KMT_No, `Elipse Position` == "25%"),], aes("Inner", KMTs_per_kinetochore),
                        fill = "springgreen4", color = "black",
                        outlier.alpha = 0) +
  geom_jitter(data = All_KMT_No[with(All_KMT_No, `Elipse Position` == "25%"),], aes("Inner", KMTs_per_kinetochore), alpha = 0.1, size = 1, width = 0.25)

print(P1)

ggsave(file="KMT_no_Ellpise.svg", plot = P1)

# Length distribution -----------------------------------------------------------------------------------------------------

All_LD <- rbind(Data_1_LD, Data_2_LD, Data_3_LD)

P2 <- ggplot(All_LD[with(All_LD, `Elipse_Position` == "100%"),], aes(length)) + 
  geom_density(kernel = "gaussian", size = 1, color = "violetred4") + theme_classic() +
  xlab("KMT lengths") + ylab("KMT density [Gaussian Kernal density]") +
  geom_vline(data = All_LD[with(All_LD, `Elipse_Position` == "100%"),], 
             aes(xintercept = mean(length)), color = "violetred4", linetype = "dashed", size = 1)

P2 <- P2  + geom_density(data = All_LD[with(All_LD, `Elipse_Position` == "50%"),], 
                         aes(length), kernel = "gaussian", size = 1, color = "royalblue4") +
  geom_vline(data = All_LD[with(All_LD, `Elipse_Position` == "50%"),], 
             aes(xintercept = mean(length)), color = "royalblue4", linetype = "dashed", size = 1)

P2 <- P2  + geom_density(data = All_LD[with(All_LD, `Elipse_Position` == "25%"),], 
                         aes(length), kernel = "gaussian", size = 1, color = "springgreen4") +
  geom_vline(data = All_LD[with(All_LD, `Elipse_Position` == "25%"),], 
             aes(xintercept = mean(length)), color = "springgreen4", linetype = "dashed", size = 1)

print(P2)

ggsave(file="LD_Ellpise.svg", plot = P2)

# Total curvature  -------------------------------------------------------------------------------------------------------

All_T_Curv <- rbind(Data_1_KMT_Total_Curv, Data_2_KMT_Total_Curv, Data_3_KMT_Total_Curv)

P3 <- ggplot(All_T_Curv[with(All_T_Curv, `Elipse Position` == "100%"),], aes(Curvature)) + 
  geom_density(kernel = "gaussian", size = 1, color = "violetred4") + theme_classic() +
  xlab("KMT curvatrure") + ylab("KMT density [Gaussian Kernal density]") + xlim(1,1.3) +
  geom_vline(data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "100%"),], 
             aes(xintercept = mean(Curvature)), color = "violetred4", linetype = "dashed", size = 1)

P3 <- P3 + geom_density(data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "50%"),], 
                        aes(Curvature), kernel = "gaussian", size = 1, color = "royalblue4") +
  geom_vline(data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "50%"),], 
             aes(xintercept = mean(Curvature)), color = "royalblue4", linetype = "dashed", size = 1)
P3 <- P3 + geom_density(data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "25%"),], 
                        aes(Curvature), kernel = "gaussian", size = 1, color = "springgreen4") +
  geom_vline(data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "25%"),], 
             aes(xintercept = mean(Curvature)), color = "springgreen4", linetype = "dashed", size = 1)

print(P3)

ggsave(file="Total_curv_Ellipse.svg", plot = P3)

P3.1 <- ggplot(All_T_Curv[with(All_T_Curv, `Elipse Position` == "100%"),], aes(`KMTs length`, Curvature)) + 
  geom_smooth( size = 1, color = "violetred4", se = F) + theme_classic()

P3.1 <- P3.1 + geom_smooth(data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "50%"),], aes(`KMTs length`, Curvature),
                           size = 1, color = "royalblue4", se = F)
P3.1 <- P3.1 + geom_smooth(data = All_T_Curv[with(All_T_Curv, `Elipse Position` == "25%"),], aes(`KMTs length`, Curvature),
                           size = 1, color = "springgreen4", se = F)

print(P3.1)
ggsave(file="Total_curv_length_Ellipse.svg", plot = P3.1)

# Local curvature  -------------------------------------------------------------------------------------------------------

All_L_Curv <- rbind(Data_1_KMT_Local_Curv, Data_2_KMT_Local_Curv, Data_3_KMT_Local_Curv)
LC_bin_100 <- data.frame()
LC_bin_80 <- data.frame()
LC_bin_45 <- data.frame()

i = 1
j = 1
while (i >= -0.2){
  LC_bin_100[j,1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"),][with(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"),],
                                                                                                      Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  LC_bin_80[j,1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"),][with(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"),],
                                                                                                      Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  LC_bin_45[j,1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"),][with(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"),],
                                                                                                    Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  
  LC_bin_100[j,2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"),][with(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"),],
                                                                                                  Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  LC_bin_80[j,2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"),][with(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"),],
                                                                                                      Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  LC_bin_45[j,2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"),][with(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"),],
                                                                                                Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  
  LC_bin_100[j,3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"),][with(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"),],
                                                                                                      Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  LC_bin_80[j,3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"),][with(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"),],
                                                                                                      Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  LC_bin_45[j,3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"),][with(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"),],
                                                                                                Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  
  j = j + 1
  i = i - 0.1
}

LC_bin_45 <- LC_bin_45[1:10, 1:3]

P4 <- ggplot(LC_bin_100, aes(V3, V1)) + 
  geom_smooth(color = "violetred4", se = F) + theme_classic() +
  xlab("KMT Relative Position") + ylab("KMT Curvature ratio") + xlim(-0.3, 1) +
P4 <- P4 + geom_smooth(data = LC_bin_80, 
                       aes(V3, V1), color = "royalblue4", se = F)
P4 <- P4 + geom_smooth(data = LC_bin_45, 
                       aes(V3, V1), color = "springgreen4", se = F)
print(P4)

ggsave(file="Local_curv_Ellipse.svg", plot = P4)

P4.1 <- ggplot(All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"),], aes(Curvature)) + geom_density(kernel = "gaussian", size = 1, color = "violetred4") + 
  theme_classic() +
  xlab("KMT curvatrure") + ylab("KMT density [Gaussian Kernal density]") + xlim(1,1.05) +
  geom_vline(data = All_L_Curv[with(All_L_Curv, `Elipse_Position` == "100%"),], 
             aes(xintercept = mean(Curvature)), color = "violetred4", linetype = "dashed", size = 1)

P4.1 <- P4.1 + geom_density(data = All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"),], 
                        aes(Curvature), kernel = "gaussian", size = 1, color = "royalblue4") +
  geom_vline(data = All_L_Curv[with(All_L_Curv, `Elipse_Position` == "50%"),], 
             aes(xintercept = mean(Curvature)), color = "royalblue4", linetype = "dashed", size = 1)

P4.1 <- P4.1 + geom_density(data = All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"),], 
                            aes(Curvature), kernel = "gaussian", size = 1, color = "springgreen4") +
  geom_vline(data = All_L_Curv[with(All_L_Curv, `Elipse_Position` == "25%"),], 
             aes(xintercept = mean(Curvature)), color = "springgreen4", linetype = "dashed", size = 1)

print(P4.1)
