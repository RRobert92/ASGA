TC_0_1.5 <- data.frame(F0_T1.5_C = KMTs_total_Curvature[with(KMTs_total_Curvature, `(+) end position` <= 1.5 & `(+) end position` > 0),][,1],
                       F0_T1.5_L = KMTs_total_Curvature[with(KMTs_total_Curvature, `(+) end position` <= 1.5 & `(+) end position` > 0),][,3])
P <-  ggplot(TC_0_1.5, aes(x = F0_T1.5_L, F0_T1.5_C)) + geom_smooth(colour = "red", size = 1, method = "loess") + theme_classic() + 
  ggtitle(paste(Data_label, "Length Distribution")) +
  xlab("KMTs length (um)") + 
  ylab("Total KMT curvature")

TC_1.5_3 <- data.frame(F1.5_T3_C = KMTs_total_Curvature[with(KMTs_total_Curvature, `(+) end position` <= 3 & `(+) end position` > 1.5),][,1],
                       F1.5_T3_L = KMTs_total_Curvature[with(KMTs_total_Curvature, `(+) end position` <= 3 & `(+) end position` > 1.5),][,3])
P <- P + geom_smooth(data = TC_1.5_3, aes(F1.5_T3_L, F1.5_T3_C), colour = "blue", size = 1) + theme_classic()

TC_3_4.5 <- data.frame(F3_T4.5_C = KMTs_total_Curvature[with(KMTs_total_Curvature, `(+) end position` <= 10 & `(+) end position` > 3),][,1],
                       F3_T4.5_L = KMTs_total_Curvature[with(KMTs_total_Curvature, `(+) end position` <= 10 & `(+) end position` > 3),][,3])
P <- P + geom_smooth(data = TC_3_4.5, aes(F3_T4.5_L, F3_T4.5_C), colour = "green", size = 1) + theme_classic()

print(P)
