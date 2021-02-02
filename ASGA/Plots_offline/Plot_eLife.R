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
Pole_to_Pole <- Pole_to_Pole + geom_col(data = data.frame(distance = mean(PTP[,7])), aes("Avg.", distance), fill = "darkred")
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

