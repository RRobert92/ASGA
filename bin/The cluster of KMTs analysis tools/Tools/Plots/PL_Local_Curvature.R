local_full_1.0 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 1.0 & Relative_Position > 0.899),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 1.0 & Relative_Position > 0.899),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 1.0 & Relative_Position > 0.899),][,1]))

local_full_0.9 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.9 & Relative_Position > 0.799),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.9 & Relative_Position > 0.799),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.9 & Relative_Position > 0.799),][,1]))

local_full_0.8 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.8 & Relative_Position > 0.699),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.8 & Relative_Position > 0.699),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.8 & Relative_Position > 0.699),][,1]))

local_full_0.7 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.7 & Relative_Position > 0.599),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.7 & Relative_Position > 0.599),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.7 & Relative_Position > 0.599),][,1]))

local_full_0.6 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.6 & Relative_Position > 0.499),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.6 & Relative_Position > 0.499),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.6 & Relative_Position > 0.499),][,1]))

local_full_0.5 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.5 & Relative_Position > 0.399),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.5 & Relative_Position > 0.399),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.5 & Relative_Position > 0.399),][,1]))

local_full_0.4 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.4 & Relative_Position > 0.299),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.4 & Relative_Position > 0.299),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.4 & Relative_Position > 0.299),][,1]))

local_full_0.3 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.3 & Relative_Position > 0.199),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.3 & Relative_Position > 0.199),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.3 & Relative_Position > 0.199),][,1]))

local_full_0.2 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.2 & Relative_Position > 0.099),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.2 & Relative_Position > 0.099),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.2 & Relative_Position > 0.099),][,1]))

local_full_0.1 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.1 & Relative_Position > 0.000),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.1 & Relative_Position > 0.000),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.1 & Relative_Position > 0.000),][,1]))

local_full_0.0 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.0 & Relative_Position > -0.099),][,1]),
                             To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.0 & Relative_Position > -0.099),][,2]),
                             SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < 0.0 & Relative_Position > -0.099),][,1]))

local_full_m0.1 <- data.frame(To_1.0_c = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < -0.1 & Relative_Position > -0.199),][,1]),
                              To_1.0_p = median(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < -0.1 & Relative_Position > -0.199),][,2]),
                              SD = sd(KMTs_local_Curvature[with(KMTs_local_Curvature, Relative_Position < -0.1 & Relative_Position > -0.199),][,1]))

Local_Full <- rbind(local_full_1.0,
                    local_full_0.9,
                    local_full_0.8,
                    local_full_0.7,
                    local_full_0.6,
                    local_full_0.5,
                    local_full_0.4,
                    local_full_0.3,
                    local_full_0.2,
                    local_full_0.1,
                    local_full_0.0,
                    local_full_m0.1)

rm(local_full_1.0,local_full_0.9,local_full_0.8,local_full_0.7, local_full_0.6,local_full_0.5,
   local_full_0.4,local_full_0.3,local_full_0.2, local_full_0.1,local_full_0.0,local_full_m0.1)