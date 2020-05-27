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
Fiber_1 <- Data_1_KMT_Total_Curv %>% distinct(`k-fiber no.`, `(+) Dist-to-Pole`)
Fiber_2 <- Data_2_KMT_Total_Curv %>% distinct(`k-fiber no.`, `(+) Dist-to-Pole`)
Fiber_3 <- Data_3_KMT_Total_Curv %>% distinct(`k-fiber no.`, `(+) Dist-to-Pole`)

Data_1_KMT_No <- cbind(Data_1_KMT_No[2], Fiber_1[2])
Data_2_KMT_No <- cbind(Data_2_KMT_No[2], Fiber_2[2])
Data_3_KMT_No <- cbind(Data_3_KMT_No[2], Fiber_3[2])

LD100max <- max(Data_1_KMT_No$`(+) Dist-to-Pole`)
LD100min <- min(Data_1_KMT_No$`(+) Dist-to-Pole`)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100

df<-data.frame()
for (i in 1:nrow(Data_1_KMT_No)){
  if(Data_1_KMT_No[i,2] > LD80_1){
df[i,1] <- "100%" 
}else if (Data_1_KMT_No[i,2] <= LD45_1){
  df[i,1] <- "25%" 
} else{
  df[i,1] <- "50%" 
}
}
Data_1_KMT_No <- cbind(Data_1_KMT_No, df)

LD100max <- max(Data_2_KMT_No$`(+) Dist-to-Pole`)
LD100min <- min(Data_2_KMT_No$`(+) Dist-to-Pole`)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100

df<-data.frame()
for (i in 1:nrow(Data_2_KMT_No)){
  if(Data_2_KMT_No[i,2] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_2_KMT_No[i,2] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_2_KMT_No <- cbind(Data_2_KMT_No, df)

LD100max <- max((Data_3_KMT_No$`(+) Dist-to-Pole`)
LD100min <- min(Data_3_KMT_No$`(+) Dist-to-Pole`)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
df<-data.frame()
for (i in 1:nrow(Data_3_KMT_No)){
  if(Data_3_KMT_No[i,2] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_3_KMT_No[i,2] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_3_KMT_No <- cbind(Data_3_KMT_No, df)

All_KMT_No <- rbind(Data_1_KMT_No, Data_2_KMT_No, Data_3_KMT_No)


P1 <- ggplot(All_KMT_No[with(All_KMT_No, `V1` == "100%"),], aes("Outer", KMTs_per_kinetochore)) + 
  geom_boxplot(fill = "violetred4", color = "black", outlier.alpha = 0) + theme_classic() +
  xlab("Data-set names") + ylab("Number of KMTs per kinetochore") + 
  geom_jitter(data = All_KMT_No[with(All_KMT_No, `V1` == "100%"),], aes("Outer", KMTs_per_kinetochore), alpha = 0.2, size = 1, width = 0.25)

P1 <- P1 + geom_boxplot(data = All_KMT_No[with(All_KMT_No, `V1` == "50%"),], aes("Middle", KMTs_per_kinetochore),
                        fill = "royalblue4", color = "black",
                        outlier.alpha = 0) +
  geom_jitter(data = All_KMT_No[with(All_KMT_No, `V1` == "50%"),], aes("Middle", KMTs_per_kinetochore), alpha = 0.2, size = 1, width = 0.25)

P1 <- P1 + geom_boxplot(data = All_KMT_No[with(All_KMT_No, `V1` == "25%"),], aes("Inner", KMTs_per_kinetochore),
                        fill = "springgreen4", color = "black",
                        outlier.alpha = 0) +
  geom_jitter(data = All_KMT_No[with(All_KMT_No, `V1` == "25%"),], aes("Inner", KMTs_per_kinetochore), alpha = 0.2, size = 1, width = 0.25)

print(P1)

ggsave(file="KMT_no_dist.svg", plot = P1)

# Length distribution -----------------------------------------------------------------------------------------------------

LD100max <- max(Data_1_LD$plus_dist_to_pole)
LD100min <- min(Data_1_LD$plus_dist_to_pole)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100

df<-data.frame()
for (i in 1:nrow(Data_1_LD)){
  if(Data_1_LD[i,4] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_1_LD[i,4] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_1_LD <- cbind(Data_1_LD, df)

LD100max <- max(Data_2_LD$plus_dist_to_pole)
LD100min <- min(Data_2_LD$plus_dist_to_pole)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
df<-data.frame()
for (i in 1:nrow(Data_2_LD)){
  if(Data_2_LD[i,4] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_2_LD[i,4] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_2_LD <- cbind(Data_2_LD, df)

LD100max <- max(Data_3_LD$plus_dist_to_pole)
LD100min <- min(Data_3_LD$plus_dist_to_pole)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
df<-data.frame()
for (i in 1:nrow(Data_3_LD)){
  if(Data_3_LD[i,4] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_3_LD[i,4] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_3_LD <- cbind(Data_3_LD, df)

All_LD <- rbind(Data_1_LD, Data_2_LD, Data_3_LD)

P2 <- ggplot(All_LD[with(All_LD, `V1` == "100%"),], aes(length)) + 
  geom_density(kernel = "gaussian", size = 1, color = "violetred4") + theme_classic() +
  xlab("KMT lengths") + ylab("KMT density [Gaussian Kernal density]") +
  geom_vline(data = All_LD[with(All_LD, `V1` == "100%"),], 
             aes(xintercept = mean(length)), color = "violetred4", linetype = "dashed", size = 1)

P2 <- P2  + geom_density(data = All_LD[with(All_LD, `V1` == "50%"),], 
                         aes(length), kernel = "gaussian", size = 1, color = "royalblue4") +
  geom_vline(data = All_LD[with(All_LD, `V1` == "50%"),], 
             aes(xintercept = mean(length)), color = "royalblue4", linetype = "dashed", size = 1)

P2 <- P2  + geom_density(data = All_LD[with(All_LD, `V1` == "25%"),], 
                         aes(length), kernel = "gaussian", size = 1, color = "springgreen4") +
  geom_vline(data = All_LD[with(All_LD, `V1` == "25%"),], 
             aes(xintercept = mean(length)), color = "springgreen4", linetype = "dashed", size = 1)

print(P2)

ggsave(file="LD_Ellpise.svg", plot = P2)

# Total curvature  -------------------------------------------------------------------------------------------------------

LD100max <- max(Data_1_KMT_Total_Curv$`(+) Dist-to-Pole`)
LD100min <- min(Data_1_KMT_Total_Curv$`(+) Dist-to-Pole`)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100

df<-data.frame()
for (i in 1:nrow(Data_1_KMT_Total_Curv)){
  if(Data_1_KMT_Total_Curv[i,6] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_1_KMT_Total_Curv[i,6] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_1_KMT_Total_Curv <- cbind(Data_1_KMT_Total_Curv, df)

LD100max <- max(Data_2_KMT_Total_Curv$`(+) Dist-to-Pole`)
LD100min <- min(Data_2_KMT_Total_Curv$`(+) Dist-to-Pole`)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
df<-data.frame()
for (i in 1:nrow(Data_2_KMT_Total_Curv)){
  if(Data_2_KMT_Total_Curv[i,6] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_2_KMT_Total_Curv[i,6] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_2_KMT_Total_Curv <- cbind(Data_2_KMT_Total_Curv, df)

LD100max <- max(Data_3_KMT_Total_Curv$`(+) Dist-to-Pole`)
LD100min <- min(Data_3_KMT_Total_Curv$`(+) Dist-to-Pole`)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
df<-data.frame()
for (i in 1:nrow(Data_3_KMT_Total_Curv)){
  if(Data_3_KMT_Total_Curv[i,6] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_3_KMT_Total_Curv[i,6] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_3_KMT_Total_Curv <- cbind(Data_3_KMT_Total_Curv, df)

All_T_Curv <- rbind(Data_1_KMT_Total_Curv, Data_2_KMT_Total_Curv, Data_3_KMT_Total_Curv)

P3 <- ggplot(All_T_Curv[with(All_T_Curv, `V1` == "100%"),], aes(Curvature)) + 
  geom_density(kernel = "gaussian", size = 1, color = "violetred4") + theme_classic() +
  xlab("KMT curvatrure") + ylab("KMT density [Gaussian Kernal density]") + xlim(1,1.3) +
  geom_vline(data = All_T_Curv[with(All_T_Curv, `V1` == "100%"),], 
             aes(xintercept = mean(Curvature)), color = "violetred4", linetype = "dashed", size = 1)

P3 <- P3 + geom_density(data = All_T_Curv[with(All_T_Curv, `V1` == "50%"),], 
                        aes(Curvature), kernel = "gaussian", size = 1, color = "royalblue4") +
  geom_vline(data = All_T_Curv[with(All_T_Curv, `V1` == "50%"),], 
             aes(xintercept = mean(Curvature)), color = "royalblue4", linetype = "dashed", size = 1)
P3 <- P3 + geom_density(data = All_T_Curv[with(All_T_Curv, `V1` == "25%"),], 
                        aes(Curvature), kernel = "gaussian", size = 1, color = "springgreen4") +
  geom_vline(data = All_T_Curv[with(All_T_Curv, `V1` == "25%"),], 
             aes(xintercept = mean(Curvature)), color = "springgreen4", linetype = "dashed", size = 1)

print(P3)

ggsave(file="Total_curv_Ellipse.svg", plot = P3)

P3.1 <- ggplot(All_T_Curv[with(All_T_Curv, `V1` == "100%"),], aes(`KMTs length`, Curvature)) + 
  geom_smooth( size = 1, color = "violetred4", se = F) + theme_classic()

P3.1 <- P3.1 + geom_smooth(data = All_T_Curv[with(All_T_Curv, `V1` == "50%"),], aes(`KMTs length`, Curvature),
                           size = 1, color = "royalblue4", se = F)
P3.1 <- P3.1 + geom_smooth(data = All_T_Curv[with(All_T_Curv, `V1` == "25%" & `KMTs length` <= 3),], aes(`KMTs length`, Curvature),
                           size = 1, color = "springgreen4", se = F)

print(P3.1)
ggsave(file="Total_curv_length_Ellipse.svg", plot = P3.1)

# Local curvature  -------------------------------------------------------------------------------------------------------

LD100max <- max(Data_1_KMT_Local_Curv$End_to_Pole)
LD100min <- min(Data_1_KMT_Local_Curv$End_to_Pole)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
df<-data.frame()
for (i in 1:nrow(Data_1_KMT_Local_Curv)){
  if(Data_1_KMT_Local_Curv[i,7] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_1_KMT_Local_Curv[i,7] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_1_KMT_Local_Curv <- cbind(Data_1_KMT_Local_Curv, df)

LD100max <- max(Data_2_KMT_Local_Curv$End_to_Pole)
LD100min <- min(Data_2_KMT_Local_Curv$End_to_Pole)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
df<-data.frame()
for (i in 1:nrow(Data_2_KMT_Local_Curv)){
  if(Data_2_KMT_Local_Curv[i,7] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_2_KMT_Local_Curv[i,7] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_2_KMT_Local_Curv <- cbind(Data_2_KMT_Local_Curv, df)

LD100max <- max(Data_3_KMT_Local_Curv$End_to_Pole)
LD100min <- min(Data_3_KMT_Local_Curv$End_to_Pole)
LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
df<-data.frame()
for (i in 1:nrow(Data_3_KMT_Local_Curv)){
  if(Data_3_KMT_Local_Curv[i,7] > LD80_1){
    df[i,1] <- "100%" 
  }else if (Data_3_KMT_Local_Curv[i,7] <= LD45_1){
    df[i,1] <- "25%" 
  } else{
    df[i,1] <- "50%" 
  }
}
Data_3_KMT_Local_Curv <- cbind(Data_3_KMT_Local_Curv, df)

All_L_Curv <- rbind(Data_1_KMT_Local_Curv, Data_2_KMT_Local_Curv, Data_3_KMT_Local_Curv)
LC_bin_100 <- data.frame()
LC_bin_80 <- data.frame()
LC_bin_45 <- data.frame()

i = 1
j = 1
while (i >= -0.2){
  LC_bin_100[j,1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `V1` == "100%"),][with(All_L_Curv[with(All_L_Curv, `V1` == "100%"),],
                                                                                                      Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  LC_bin_80[j,1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `V1` == "50%"),][with(All_L_Curv[with(All_L_Curv, `V1` == "50%"),],
                                                                                                      Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  LC_bin_45[j,1] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `V1` == "25%"),][with(All_L_Curv[with(All_L_Curv, `V1` == "25%"),],
                                                                                                    Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  
  LC_bin_100[j,2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `V1` == "100%"),][with(All_L_Curv[with(All_L_Curv, `V1` == "100%"),],
                                                                                                  Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  LC_bin_80[j,2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `V1` == "50%"),][with(All_L_Curv[with(All_L_Curv, `V1` == "50%"),],
                                                                                                      Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  LC_bin_45[j,2] <- sd(as.matrix(All_L_Curv[with(All_L_Curv, `V1` == "25%"),][with(All_L_Curv[with(All_L_Curv, `V1` == "25%"),],
                                                                                                Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][2]))
  
  LC_bin_100[j,3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `V1` == "100%"),][with(All_L_Curv[with(All_L_Curv, `V1` == "100%"),],
                                                                                                      Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][3]))
  LC_bin_80[j,3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `V1` == "50%"),][with(All_L_Curv[with(All_L_Curv, `V1` == "50%"),],
                                                                                                      Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][3]))
  LC_bin_45[j,3] <- median(as.matrix(All_L_Curv[with(All_L_Curv, `V1` == "25%"),][with(All_L_Curv[with(All_L_Curv, `V1` == "25%"),],
                                                                                                Relative_Position <= i & Relative_Position >= as.numeric(i-0.1)),][3]))
  
  j = j + 1
  i = i - 0.1
}

LC_bin_45 <- LC_bin_45[1:10, 1:3]

P4 <- ggplot(LC_bin_100, aes(V3, V1)) + 
  geom_smooth(color = "violetred4", se = F) + theme_classic() +
  xlab("KMT Relative Position") + ylab("KMT Curvature ratio") + xlim(-0.25, 1) + ylim(1, 1.02)
P4 <- P4 + geom_smooth(data = LC_bin_80, 
                       aes(V3, V1), color = "royalblue4", se = F)
P4 <- P4 + geom_smooth(data = LC_bin_45, 
                       aes(V3, V1), color = "springgreen4", se = F)
print(P4)


ggsave(file="Local_curv_Ellipse.svg", plot = P4)

P4.1 <- ggplot(All_L_Curv[with(All_L_Curv, `V1` == "100%"),], aes(Curvature)) + geom_density(kernel = "gaussian", size = 1, color = "violetred4") + 
  theme_classic() +
  xlab("KMT curvatrure") + ylab("KMT density [Gaussian Kernal density]") + xlim(1,1.05) +
  geom_vline(data = All_L_Curv[with(All_L_Curv, `V1` == "100%"),], 
             aes(xintercept = mean(Curvature)), color = "violetred4", linetype = "dashed", size = 1)

P4.1 <- P4.1 + geom_density(data = All_L_Curv[with(All_L_Curv, `V1` == "50%"),], 
                        aes(Curvature), kernel = "gaussian", size = 1, color = "royalblue4") +
  geom_vline(data = All_L_Curv[with(All_L_Curv, `V1` == "50%"),], 
             aes(xintercept = mean(Curvature)), color = "royalblue4", linetype = "dashed", size = 1)

P4.1 <- P4.1 + geom_density(data = All_L_Curv[with(All_L_Curv, `V1` == "25%"),], 
                            aes(Curvature), kernel = "gaussian", size = 1, color = "springgreen4") +
  geom_vline(data = All_L_Curv[with(All_L_Curv, `V1` == "25%"),], 
             aes(xintercept = mean(Curvature)), color = "springgreen4", linetype = "dashed", size = 1)

print(P4.1)

ggsave(file="Local_curv_distrib_Ellipse.svg", plot = P4.1)