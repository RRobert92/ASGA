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
  geom_boxplot(fill = "grey20", color = "black") + theme_classic() +
  xlab("Data-set names") + ylab("Number of KMTs per kinetochore")
P1<- P1 + geom_boxplot(data = Data_2_KMT_No, aes("Metaphase #2", KMTs_per_kinetochore), fill = "grey30", color = "black")
P1<- P1 + geom_boxplot(data = Data_3_KMT_No, aes("Metaphase #3", KMTs_per_kinetochore), fill = "grey40", color = "black")
All_KMT_No <- rbind(Data_1_KMT_No, Data_2_KMT_No, Data_3_KMT_No)
P1<- P1 + geom_boxplot(data = All_KMT_No, aes("Avg.", KMTs_per_kinetochore), fill = "darkred", color = "black", outlier.alpha = 0) + 
  geom_jitter(data = All_KMT_No, aes("Avg.", KMTs_per_kinetochore), alpha = 0.1, size = 1, width = 0.25)

print(P1)

ggsave(file="KMT_no.svg", plot = P1)

# Length distribution -----------------------------------------------------------------------------------------------------

P2 <- ggplot(Data_1_LD, aes(length)) + geom_density(kernel = "gaussian", size = 1, color = "black", linetype = "dashed") + theme_classic() +
  xlab("KMT lengths") + ylab("KMT density [Gaussian Kernal density]")
P2 <- P2  + geom_density(data = Data_2_LD, aes(length), kernel = "gaussian", size = 1, color = "black", linetype = "dotdash")
P2 <- P2  + geom_density(data = Data_3_LD, aes(length), kernel = "gaussian", size = 1, color = "black", linetype = "dotted")
All_LD <- rbind(Data_1_LD, Data_2_LD, Data_3_LD)
P2 <- P2  + geom_density(data = All_LD, aes(length), kernel = "gaussian", size = 1, color = "darkred", linetype = "solid")
print(P2)

ggsave(file="LD.svg", plot = P2)

# Minus end distribution --------------------------------------------------------------------------------------------------

All_KMT_Minus_Ends <- rbind(Data_1_KMT_Minus_Ends, Data_2_KMT_Minus_Ends, Data_3_KMT_Minus_Ends)

P2.1 <- ggplot(Data_1_KMT_Minus_Ends, aes(Relative_minus_position, minus_dist_to_pole)) + geom_smooth(method = "loess", linetype = "dashed", color = "black", se = F) + 
  theme_classic() + xlab("KMT Relative Position") + ylab("KMT minus-end distance from the pole") + xlim(-0.2, 1)
P2.1 <- P2.1 + geom_smooth(data = Data_2_KMT_Minus_Ends, aes(Relative_minus_position, minus_dist_to_pole), linetype = "dotdash", color = "black", se = F)
P2.1 <- P2.1 + geom_smooth( data = Data_3_KMT_Minus_Ends, aes(Relative_minus_position, minus_dist_to_pole), linetype = "dotdash", color = "black", se = F)
P2.1 <- P2.1 + geom_smooth( data = All_KMT_Minus_Ends, aes(Relative_minus_position, minus_dist_to_pole), linetype = "solid", color = "darkred")
print(P2.1)

# Total curvature  --------------------------------------------------------------------------------------------------------

P3 <- ggplot(Data_1_KMT_Total_Curv, aes(Curvature)) + geom_density(kernel = "gaussian", size = 1, color = "black", linetype = "dashed") + theme_classic() +
  xlab("KMT curvatrure") + ylab("KMT density [Gaussian Kernal density]") + xlim(1,1.3)
P3 <- P3 + geom_density(data = Data_2_KMT_Total_Curv, aes(Curvature), kernel = "gaussian", size = 1, color = "black", linetype = "dotdash") 
P3 <- P3 + geom_density(data = Data_3_KMT_Total_Curv, aes(Curvature), kernel = "gaussian", size = 1, color = "black", linetype = "dotted") 
All_T_Curv <- rbind(Data_1_KMT_Total_Curv, Data_2_KMT_Total_Curv, Data_3_KMT_Total_Curv)
P3 <- P3 + geom_density(data = All_T_Curv, aes(Curvature), kernel = "gaussian", size = 1, color = "darkred", linetype = "solid") + 
  geom_vline(data = All_T_Curv, aes(xintercept = median(Curvature)), color = "blue", linetype = "dashed", size = 1)

print(P3)

ggsave(file="Total_curv.svg", plot = P3)

P3 <- ggplot(Data_1_KMT_Total_Curv, aes(`KMTs length`, Curvature)) + geom_smooth(method = 'loess', size = 1, color = "black", linetype = "dashed", se = F) + theme_classic() +
  xlab("KMT length") + ylab("KMT curvature ratio") 
P3 <- P3 + geom_smooth(data = Data_2_KMT_Total_Curv, aes(`KMTs length`, Curvature), method = 'loess', size = 1, color = "black", linetype = "dotdash", se = F) 
P3 <- P3 + geom_smooth(data = Data_3_KMT_Total_Curv, aes(`KMTs length`, Curvature), method = 'loess', size = 1, color = "black", linetype = "dotted", se = F) 
All_T_Curv <- rbind(Data_1_KMT_Total_Curv, Data_2_KMT_Total_Curv, Data_3_KMT_Total_Curv)
P3 <- P3 + geom_smooth(data = All_T_Curv, aes(`KMTs length`, Curvature), method = 'loess', size = 1, color = "darkred", linetype = "solid")

print(P3)

# Local curvature  -------------------------------------------------------------------------------------------------------

LC_bin_Data_1 <- data.frame()
LC_bin_Data_2 <- data.frame()
LC_bin_Data_3 <- data.frame()
LC_bin_All <- data.frame()
All_L_Curv <- rbind(Data_1_KMT_Local_Curv, Data_2_KMT_Local_Curv, Data_3_KMT_Local_Curv)

i = 1
j = 1
while (i >= -0.2){
  LC_bin_Data_1[j,1] <- median(as.matrix(Data_1_KMT_Local_Curv[with(Data_1_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  LC_bin_Data_2[j,1] <- median(as.matrix(Data_2_KMT_Local_Curv[with(Data_2_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  LC_bin_Data_3[j,1] <- median(as.matrix(Data_3_KMT_Local_Curv[with(Data_3_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  LC_bin_All[j,1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  
  LC_bin_Data_1[j,2] <- sd(as.matrix(Data_1_KMT_Local_Curv[with(Data_1_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  LC_bin_Data_2[j,2] <- sd(as.matrix(Data_2_KMT_Local_Curv[with(Data_2_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  LC_bin_Data_3[j,2] <- sd(as.matrix(Data_3_KMT_Local_Curv[with(Data_3_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  LC_bin_All[j,2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][1]))
  
  LC_bin_Data_1[j,3] <- median(as.matrix(Data_1_KMT_Local_Curv[with(Data_1_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  LC_bin_Data_2[j,3] <- median(as.matrix(Data_2_KMT_Local_Curv[with(Data_2_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  LC_bin_Data_3[j,3] <- median(as.matrix(Data_3_KMT_Local_Curv[with(Data_3_KMT_Local_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  LC_bin_All[j,3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  
    j = j + 1
  i = i - 0.1
}

LC_bin_Data_1 <- round(LC_bin_Data_1, 4)
LC_bin_Data_1[3] <- round(LC_bin_Data_1[3], 2)
LC_bin_Data_2 <- round(LC_bin_Data_2, 4)
LC_bin_Data_2[3] <- round(LC_bin_Data_2[3], 2)
LC_bin_Data_3 <- round(LC_bin_Data_3, 4)
LC_bin_Data_3[3] <- round(LC_bin_Data_3[3], 2)
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
names(LC_bin_All)[1] <- "Curvature"
names(LC_bin_All)[2] <- "SD"
names(LC_bin_All)[3] <- "Relative_Position"

P4 <- ggplot(Data_1_KMT_Local_Curv, aes(Relative_Position, Curvature)) + geom_smooth(method = "gam", color = "black", se = FALSE, linetype = "dashed") + theme_classic() +
  xlab("KMT Relative Position") + ylab("KMT Curvature ratio") + xlim(-0.2, 1) 
P4 <- P4 + geom_smooth(data = Data_2_KMT_Local_Curv, aes(Relative_Position, Curvature), color = "black", se = FALSE, linetype = "dotdash")
P4 <- P4 + geom_smooth(data = Data_2_KMT_Local_Curv, aes(Relative_Position, Curvature), color = "black", se = FALSE, linetype = "dotted")
P4 <- P4 + geom_smooth(data = All_L_Curv, aes(Relative_Position, Curvature), color = "darkred", se = T, linetype = "solid")
print(P4)

ggsave(file="Local_curv.svg", plot = P4)

# Fiber area  ------------------------------------------------------------------------------------------------------------

FA_bin_Data_1 <- data.frame()
FA_bin_Data_2 <- data.frame()
FA_bin_Data_3 <- data.frame()
FA_bin_All <- data.frame()
All_fiber_area <- rbind(Data_1_Fiber_Area, Data_2_Fiber_Area, Data_3_Fiber_Area)

i = 1
j = 1 
while (i >= -0.2){
  FA_bin_Data_1[j,1] <- median(as.matrix(Data_1_Fiber_Area[with(Data_1_Fiber_Area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][2]))
  FA_bin_Data_2[j,1] <- median(as.matrix(Data_2_Fiber_Area[with(Data_2_Fiber_Area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][2]))
  FA_bin_Data_3[j,1] <- median(as.matrix(Data_3_Fiber_Area[with(Data_3_Fiber_Area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][2]))
  FA_bin_All[j,1] <- median(as.matrix(All_fiber_area[with(All_fiber_area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][2]))
  
  FA_bin_Data_1[j,2] <- sd(as.matrix(Data_1_Fiber_Area[with(Data_1_Fiber_Area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][2]))
  FA_bin_Data_2[j,2] <- sd(as.matrix(Data_2_Fiber_Area[with(Data_2_Fiber_Area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][2]))
  FA_bin_Data_3[j,2] <- sd(as.matrix(Data_3_Fiber_Area[with(Data_3_Fiber_Area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][2]))
  FA_bin_All[j,2] <- sd(as.matrix(All_fiber_area[with(All_fiber_area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][2]))
 
  FA_bin_Data_1[j,3] <- median(as.matrix(Data_1_Fiber_Area[with(Data_1_Fiber_Area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][1]))
  FA_bin_Data_2[j,3] <- median(as.matrix(Data_2_Fiber_Area[with(Data_2_Fiber_Area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][1]))
  FA_bin_Data_3[j,3] <- median(as.matrix(Data_3_Fiber_Area[with(Data_3_Fiber_Area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][1]))
  FA_bin_All[j,3] <- median(as.matrix(All_fiber_area[with(All_fiber_area, Relative_position <= i & Relative_position >= as.numeric(i-0.1)),][1]))
  
  j = j + 1
  i = i - 0.1
}

FA_bin_Data_1 <- round(FA_bin_Data_1, 3)
FA_bin_Data_1[3] <- round(FA_bin_Data_1[3], 2)
FA_bin_Data_2 <- round(FA_bin_Data_2, 3)
FA_bin_Data_2[3] <- round(FA_bin_Data_2[3], 2)
FA_bin_Data_3 <- round(FA_bin_Data_3, 3)
FA_bin_Data_3[3] <- round(FA_bin_Data_3[3], 2)
FA_bin_All <- round(FA_bin_All, 3)
FA_bin_All[3] <- round(FA_bin_All[3], 2)

names(FA_bin_Data_1)[1] <- "Area"
names(FA_bin_Data_1)[2] <- "SD"
names(FA_bin_Data_1)[3] <- "Relative_Position"
names(FA_bin_Data_2)[1] <- "Area"
names(FA_bin_Data_2)[2] <- "SD"
names(FA_bin_Data_2)[3] <- "Relative_Position"
names(FA_bin_Data_3)[1] <- "Area"
names(FA_bin_Data_3)[2] <- "SD"
names(FA_bin_Data_3)[3] <- "Relative_Position"
names(FA_bin_All)[1] <- "Area"
names(FA_bin_All)[2] <- "SD"
names(FA_bin_All)[3] <- "Relative_Position"

P5 <- ggplot(Data_1_Fiber_Area, aes(Relative_position, Alpha_area)) + geom_smooth(method = "gam", color = "black", se = FALSE, linetype = "dashed") + theme_classic() +
  xlab("KMT Relative Position") + ylab("KMT polygon area") + xlim(-0.2, 1)
P5 <- P5 + geom_smooth(data = Data_2_Fiber_Area, aes(Relative_position, Alpha_area), color = "black", se = FALSE, linetype = "dotdash")
P5 <- P5 + geom_smooth(data = Data_3_Fiber_Area, aes(Relative_position, Alpha_area), color = "black", se = FALSE, linetype = "dotted")
P5 <- P5 + geom_smooth(data = All_fiber_area, aes(Relative_position, Alpha_area), color = "darkred", se = T, linetype = "solid")
print(P5)

ggsave(file="Fiber_area.svg", plot = P5)

# Fiber focused  ---------------------------------------------------------------------------------------------------------

All_fiber_focus <- rbind(Data_1_N_Density, Data_2_N_Density, Data_3_N_Density)

P6 <- ggplot(Data_1_N_Density, aes(Relative_position, `Focused KMTs %`)) + geom_smooth(color = "black", se = FALSE, linetype = "dashed") + theme_classic() +
  xlab("KMT Relative Position") + ylab("KMT focusing factor [%]") + xlim(-0.2, 1)
P6 <- P6 + geom_smooth(data = Data_2_N_Density, aes(Relative_position, `Focused KMTs %`), color = "black", se = FALSE, linetype = "dotdash")
P6 <- P6 + geom_smooth(data = Data_3_N_Density, aes(Relative_position, `Focused KMTs %`), color = "black", se = FALSE, linetype = "dotted")
P6 <- P6 + geom_smooth(data = All_fiber_focus, aes(Relative_position, `Focused KMTs %`), color = "darkred", se = T, linetype = "solid")
print(P6)

ggsave(file="Fiber_focus_distribution.svg", plot = P6)

P7 <-  ggplot(Data_1_N_Density, aes(`Focused KMTs %`)) + geom_density(kernel = "gaussian", color = "black", linetype = "dashed", size = 1) + theme_classic() + 
  xlab("KMT focusing factor [%]") + ylab("No. of k-fibers [%]")
P7 <-  P7 + geom_density(data = Data_2_N_Density, aes(`Focused KMTs %`), kernel = "gaussian", color = "black", linetype = "dotdash", size = 1)
P7 <-  P7 + geom_density(data = Data_3_N_Density, aes(`Focused KMTs %`), kernel = "gaussian", color = "black", linetype = "dotted", size = 1)
P7 <-  P7 + geom_density(data = All_fiber_focus, aes(`Focused KMTs %`), kernel = "gaussian", color = "darkred", linetype = "solid", size = 1)
print(P7)
ggsave(file="Fiber_focus_no.svg", plot = P7)
