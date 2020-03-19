TC_0_1.5 <- data.frame(Total_Curvature[with(Total_Curvature, `(+) end position` <= 1.5 & `(+) end position` > 0),][,1],
                       Total_Curvature[with(Total_Curvature, `(+) end position` <= 1.5 & `(+) end position` > 0),][,3])

P_TC <-  ggplot(TC_0_1.5, aes(x = KMTs.length, Curvature)) + geom_smooth(colour = "red", size = 1, method = "gam") + theme_classic() + 
  ggtitle(paste(Data_label, "Total curvature based on fiber position")) +
  xlab("KMTs length (um)") + 
  ylab("Total KMT curvature")

TC_1.5_3 <- data.frame(Total_Curvature[with(Total_Curvature, `(+) end position` <= 3 & `(+) end position` > 1.5),][,1],
                       Total_Curvature[with(Total_Curvature, `(+) end position` <= 3 & `(+) end position` > 1.5),][,3])
P_TC <- P_TC + geom_smooth(data = TC_1.5_3, aes(x = KMTs.length, Curvature), colour = "blue", size = 1, method = "gam") + theme_classic()

TC_3_4.5 <- data.frame(Total_Curvature[with(Total_Curvature, `(+) end position` <= 10 & `(+) end position` > 3),][,1],
                       Total_Curvature[with(Total_Curvature, `(+) end position` <= 10 & `(+) end position` > 3),][,3])
P_TC <- P_TC + geom_smooth(data = TC_3_4.5, aes(x = KMTs.length, Curvature), colour = "green", size = 1, method = "gam") + theme_classic()

print(P_TC)

print(ggplot(Total_Curvature, aes(x = `KMTs length`, Curvature)) + geom_smooth(colour = "red", size = 1, method = "gam") + theme_classic() + 
        ggtitle(paste(Data_label, "Total Curvature overall")) + geom_jitter(alpha = 0.1) +
        xlab("KMTs length (um)") + 
        ylab("Total KMT curvature"))

write.xlsx(Total_Curvature, paste("Output/", Data_label, "_Total_Curvature.xlsx", sep = ""))
write.xlsx(TC_0_1.5, paste("Output/", Data_label, "_TC_0_1.5.xlsx", sep = ""))
write.xlsx(TC_1.5_3, paste("Output/", Data_label, "_TC_1.5_3.xlsx", sep = ""))
write.xlsx(TC_3_4.5, paste("Output/", Data_label, "_TC_3_4.5.xlsx", sep = ""))