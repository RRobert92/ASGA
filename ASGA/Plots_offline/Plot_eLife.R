###########################################################################################################################
# Plots for eLife paper
#
# (c) 2019-2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-02-02
###########################################################################################################################
source("bin/Utility/Library.R")

# 3D Euclidean distance between spindle poles from tomographic data
PTP <- tibble(X_Coord_P1 = c(51385.63281, 52571.19531, 80278.8125),	
              Y_Coord_P1 = c(18100.44141, 113530.8203, 81406.53906),	
              Z_Coord_P1 = c(25617.02148, 28686.98242, 17718.85156),
              X_Coord_P2 = c(51282.79297, 52565.24609, 80279.78906),	
              Y_Coord_P2 = c(89695.32031, 9660.630859, -13390.78125),	
              Z_Coord_P2 = c(24732.59766, 28693.30469, 17719.81055))/10000
PTP$distance <- apply(PTP, 1, 
                      function(x) dist(matrix(x, nrow = 2, byrow = TRUE)))

Pole_to_Pole <- ggplot(PTP[1,], aes("Metaphase #1", distance)) + geom_col(fill = "brown1") + theme_classic()
Pole_to_Pole <- Pole_to_Pole + geom_col(data = PTP[2,], aes("Metaphase #2", distance), fill = "brown2")
Pole_to_Pole <- Pole_to_Pole + geom_col(data = PTP[3,], aes("Metaphase #3", distance), fill = "brown3")
Pole_to_Pole <- Pole_to_Pole + geom_col(data = data.frame(distance = mean(PTP[,7])), aes("Avg.", distance), fill = "darkred") + 
  stat_summary(data = data.frame(distance =PTP[,7]), 
               aes("Avg.", distance),
               fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
print(Pole_to_Pole)

# 3D Euclidean distance between sister-kinetochore (IKD)
IKD_avg <- rbind(Data_1_IKD, Data_2_IKD, Data_3_IKD)

IKD <- ggplot(Data_1_IKD, aes("Metaphase #1", `Inter-kinetochore distance`)) + 
       geom_quasirandom(method = "tukeyDense", color = "brown1") + theme_classic() + 
       stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange") +
       ylim(0, 2)
IKD <- IKD + 
       geom_quasirandom(data = Data_2_IKD, aes("Metaphase #2", `Inter-kinetochore distance`), color = "brown2") + 
       stat_summary(data = Data_2_IKD, 
                    aes("Metaphase #2", `Inter-kinetochore distance`), 
                    fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
IKD <- IKD + 
       geom_quasirandom(data = Data_3_IKD, aes("Metaphase #3", `Inter-kinetochore distance`), color = "brown3") +
       stat_summary(data = Data_3_IKD, 
                    aes("Metaphase #3", `Inter-kinetochore distance`), 
                    fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
IKD <- IKD + 
       geom_quasirandom(data = IKD_avg, aes("Avg.", `Inter-kinetochore distance`), color = "darkred") + 
       stat_summary(data = IKD_avg, 
                   aes("Avg.", `Inter-kinetochore distance`), 
                   fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
print(IKD)

# No. of KMTs per k-fiber
No_KMTs_avg <- rbind(Data_1_KMT_No, Data_2_KMT_No, Data_3_KMT_No)

No_KMTs <- ggplot(Data_1_KMT_No, aes("Metaphase #1", KMTs_per_kinetochore)) + 
           geom_quasirandom(method = "tukeyDense", color = "brown1") + theme_classic() + 
           stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
No_KMTs <- No_KMTs + 
           geom_quasirandom(data = Data_2_KMT_No, aes("Metaphase #2", KMTs_per_kinetochore), 
                            method = "tukeyDense", color = "brown2") + 
           stat_summary(data = Data_2_KMT_No,
                        aes("Metaphase #2", KMTs_per_kinetochore), 
                        fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
No_KMTs <- No_KMTs + 
  geom_quasirandom(data = Data_3_KMT_No, aes("Metaphase #3", KMTs_per_kinetochore), 
                   method = "tukeyDense", color = "brown3") + 
  stat_summary(data = Data_3_KMT_No,
               aes("Metaphase #3", KMTs_per_kinetochore), 
               fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
No_KMTs <- No_KMTs + 
  geom_quasirandom(data = No_KMTs_avg, aes("Avg.", KMTs_per_kinetochore), 
                   method = "tukeyDense", color = "darkred") + 
  stat_summary(data = No_KMTs_avg,
               aes("Avg.", KMTs_per_kinetochore), 
               fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
print(No_KMTs)

IKD_KMT_NO <- ggplot(Data_1_IKD_KMT_No, aes(`KMTs no.`, `Inter-kinetochore distance`)) + 
              geom_jitter(color = "brown1", shape = 15) + theme_classic() + 
              geom_smooth(method = "lm", color = "brown1", se = F)
IKD_KMT_NO <- IKD_KMT_NO +
              geom_jitter(data = Data_2_IKD_KMT_No, aes(`KMTs no.`, `Inter-kinetochore distance`), color = "brown2", shape = 16) +
              geom_smooth(data = Data_2_IKD_KMT_No, aes(`KMTs no.`, `Inter-kinetochore distance`), method = "lm", color = "brown2", se = F)
IKD_KMT_NO <- IKD_KMT_NO +
  geom_jitter(data = Data_2_IKD_KMT_No, aes(`KMTs no.`, `Inter-kinetochore distance`), color = "brown3", shape = 17) +
  geom_smooth(data = Data_3_IKD_KMT_No, aes(`KMTs no.`, `Inter-kinetochore distance`), method = "lm", color = "brown3", se = F)
print(IKD_KMT_NO)

# Length distribution
LD_avg <- rbind(Data_1_LD, Data_2_LD, Data_3_LD)

LD <- ggplot(Data_1_LD, aes("Metaphase #1", length)) + 
      geom_quasirandom(method = "tukeyDense", color = "brown1") + 
      stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange") + theme_classic()
LD <- LD + 
      geom_quasirandom(data = Data_2_LD, aes("Metaphase #2", length), 
                       method = "tukeyDense", color = "brown2") + 
      stat_summary(data = Data_2_LD,
                   aes("Metaphase #2", length), 
                   fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
LD <- LD + 
      geom_quasirandom(data = Data_3_LD, aes("Metaphase #3", length), 
                   method = "tukeyDense", color = "brown3") + 
      stat_summary(data = Data_3_LD,
               aes("Metaphase #3", length), 
               fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
LD <- LD + 
      geom_quasirandom(data = LD_avg, aes("Avg.", length), 
                   method = "tukeyDense", color = "darkred") + 
      stat_summary(data = LD_avg,
               aes("Avg.", length), 
               fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
print(LD)

# Minus ends distance and distribution

Data_1 <- ggplot_build(ggplot(Data_1_KMT_Minus_Ends, aes(minus_dist_to_pole)) + geom_density())$data[[1]]
Data_1 <- as.numeric(FWHM(Data_1$x, Data_1$y, 2)[2])

Data_2 <- ggplot_build(ggplot(Data_2_KMT_Minus_Ends, aes(minus_dist_to_pole)) + geom_density())$data[[1]]
y_max <- Data_2[which.max(Data_2$y),1]/1.2   # Data show double pick to account for that FWHM is taken only for the first peak
Data_2 <- Data_2[as.numeric(which.max(Data_2$y)+1):nrow(Data_2),]
Data_2 <- as.numeric(Data_2[which(abs(Data_2$y - y_max) == min(abs(Data_2$y - y_max))),]$x)

Data_3 <- Data_3[as.numeric(which.max(Data_3$y)+1):nrow(Data_3),]
Data_3 <- as.numeric(Data_3[which(abs(Data_3$y - y_max) == min(abs(Data_3$y - y_max))),]$x)

Minus_Ends_avg <- rbind(Data_1_KMT_Minus_Ends, Data_2_KMT_Minus_Ends, Data_3_KMT_Minus_Ends)

MInus_Ends <- ggplot(Data_1_KMT_Minus_Ends, aes("Metaphase #1", minus_dist_to_pole))+ 
              geom_quasirandom(method = "tukeyDense", color = "brown1") + 
              stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange") + theme_classic()
MInus_Ends <- MInus_Ends + 
              geom_quasirandom(data = Data_2_KMT_Minus_Ends, aes("Metaphase #2", minus_dist_to_pole), 
                               method = "tukeyDense", color = "brown2") + 
              stat_summary(data = Data_2_KMT_Minus_Ends,
                           aes("Metaphase #2", minus_dist_to_pole), 
                           fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
MInus_Ends <- MInus_Ends + 
              geom_quasirandom(data = Data_3_KMT_Minus_Ends, aes("Metaphase #3", minus_dist_to_pole), 
                               method = "tukeyDense", color = "brown3") + 
              stat_summary(data = Data_3_KMT_Minus_Ends,
                          aes("Metaphase #3", minus_dist_to_pole), 
                          fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
MInus_Ends <- MInus_Ends + 
              geom_quasirandom(data = Minus_Ends_avg, aes("Avg.", minus_dist_to_pole), 
                               method = "tukeyDense", color = "darkred") + 
              stat_summary(data = Minus_Ends_avg,
                           aes("Avg.", minus_dist_to_pole), 
                           fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
print(MInus_Ends)

Minus_Position <- ggplot(Data_1_KMT_Minus_Ends, aes("Metaphase #1", Relative_minus_position))+ 
                  geom_quasirandom(method = "tukeyDense", color = "brown1") + 
                  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange") + theme_classic()
Minus_Position <- Minus_Position + 
                  geom_quasirandom(data = Data_2_KMT_Minus_Ends, aes("Metaphase #2", Relative_minus_position), 
                                   method = "tukeyDense", color = "brown2") + 
                  stat_summary(data = Data_2_KMT_Minus_Ends,
                               aes("Metaphase #2", Relative_minus_position), 
                               fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
Minus_Position <- Minus_Position + 
                  geom_quasirandom(data = Data_3_KMT_Minus_Ends, aes("Metaphase #3", Relative_minus_position), 
                                   method = "tukeyDense", color = "brown3") + 
                  stat_summary(data = Data_3_KMT_Minus_Ends,
                               aes("Metaphase #3", Relative_minus_position), 
                               fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
Minus_Position <- Minus_Position + 
                  geom_quasirandom(data = Minus_Ends_avg, aes("Avg.", Relative_minus_position), 
                                   method = "tukeyDense", color = "darkred") + 
                  stat_summary(data = Minus_Ends_avg,
                               aes("Avg.", Relative_minus_position), 
                               fun.data="mean_sdl", fun.args = list(mult=1), geom="pointrange")
print(Minus_Position)