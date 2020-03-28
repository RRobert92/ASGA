local_full <- data.frame()
local_full[1,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 1.0 & Relative_Position > 0.899),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 1.0 & Relative_Position > 0.899),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 1.0 & Relative_Position > 0.899),][,1])))

local_full[2,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.9 & Relative_Position > 0.799),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.9 & Relative_Position > 0.799),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.9 & Relative_Position > 0.799),][,1])))

local_full[3,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.8 & Relative_Position > 0.699),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.8 & Relative_Position > 0.699),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.8 & Relative_Position > 0.699),][,1])))

local_full[4,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.7 & Relative_Position > 0.599),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.7 & Relative_Position > 0.599),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.7 & Relative_Position > 0.599),][,1])))

local_full[5,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.6 & Relative_Position > 0.499),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.6 & Relative_Position > 0.499),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.6 & Relative_Position > 0.499),][,1])))

local_full[6,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.5 & Relative_Position > 0.399),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.5 & Relative_Position > 0.399),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.5 & Relative_Position > 0.399),][,1])))

local_full[7,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.4 & Relative_Position > 0.299),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.4 & Relative_Position > 0.299),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.4 & Relative_Position > 0.299),][,1])))

local_full[8,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.3 & Relative_Position > 0.199),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.3 & Relative_Position > 0.199),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.3 & Relative_Position > 0.199),][,1])))

local_full[9,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.2 & Relative_Position > 0.099),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.2 & Relative_Position > 0.099),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.2 & Relative_Position > 0.099),][,1])))

local_full[9,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.1 & Relative_Position > 0.000),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.1 & Relative_Position > 0.000),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.1 & Relative_Position > 0.000),][,1])))

local_full[10,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.0 & Relative_Position > -0.099),][,1])),
                             To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.0 & Relative_Position > -0.099),][,2])),
                             SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < 0.0 & Relative_Position > -0.099),][,1])))

local_full[11,1:3] <- data.frame(To_1.0_c = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < -0.1 & Relative_Position > -0.199),][,1])),
                              To_1.0_p = median(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < -0.1 & Relative_Position > -0.199),][,2])),
                              SD = sd(unlist(Local_Curvature[with(Local_Curvature, Relative_Position < -0.1 & Relative_Position > -0.199),][,1])))

print(ggplot(local_full, aes(To_1.0_p, To_1.0_c)) + geom_point() + ylim(1, 1.01) + xlim(-0.3, 1))

PL <- ggplot(Local_Curvature, aes(Relative_Position, Curvature)) + 
  geom_smooth(size = 1, method = "gam") +
  xlim(-0.3, 1) + ylim(1, 1.03) +
  theme_classic()
PL <- PL + geom_point(data = local_full, aes(To_1.0_p, To_1.0_c))
print(PL)

write.xlsx(Local_Curvature, paste("Output/", Data_label, "_Local_Curvature.xlsx", sep = ""))
write.xlsx(local_full, paste("Output/", Data_label, "_Local_C_bin.xlsx", sep = ""))