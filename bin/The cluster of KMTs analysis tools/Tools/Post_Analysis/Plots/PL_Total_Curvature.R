
write.xlsx(Total_Curvature, paste("Output/", Data_label, "_Total_Curvature.xlsx", sep = ""))
write.xlsx(TC_0_1.5, paste("Output/", Data_label, "_TC_0_1.5.xlsx", sep = ""))
write.xlsx(TC_1.5_3, paste("Output/", Data_label, "_TC_1.5_3.xlsx", sep = ""))
write.xlsx(TC_3_4.5, paste("Output/", Data_label, "_TC_3_4.5.xlsx", sep = ""))

for(i in 1:No_of_Data){
  assign(paste(Data_label, "_", i, "_TC_0_1.5", sep = ""),
         data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                      `(+) end position` <= 1.5 & `(+) end position` > 0),][,1],
                    get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")),
                                                                                      `(+) end position` <= 1.5 & `(+) end position` > 0),][,3]))
  assign(paste(Data_label, "_", i, "_TC_1.5_3", sep = ""),
         data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                      `(+) end position` <= 3 & `(+) end position` > 1.5),][,1],
                    get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")),
                                                                                      `(+) end position` <= 3 & `(+) end position` > 1.5),][,3]))
  assign(paste(Data_label, "_", i, "_TC_3_4.5", sep = ""),
         data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                      `(+) end position` <= 10 & `(+) end position` > 3),][,1],
                    get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")),
                                                                                      `(+) end position` <= 10 & `(+) end position` > 3),][,3]))
}

for(i in 1:No_of_Data){
P1 <- ggplot(get(paste(Data_label, "_", i, "_TC_0_1.5", sep = "")), aes(`KMTs.length`, Curvature)) + 
  geom_smooth(method = "loess", color = "blue") + geom_point(color = "blue", alpha = 0.2) +
  ylim(1, 1.6) + xlim(0, 10) + theme_classic()
P1 <- P1 + geom_smooth(data = get(paste(Data_label, "_", i, "_TC_1.5_3", sep = "")), aes(`KMTs.length`, Curvature), color = "green", method = "loess") +
  geom_point(data = get(paste(Data_label, "_", i, "_TC_1.5_3", sep = "")), aes(`KMTs.length`, Curvature), color = "green", alpha = 0.2)
P1 <- P1 + geom_smooth(data = get(paste(Data_label, "_", i, "_TC_3_4.5", sep = "")), aes(`KMTs.length`, Curvature), color = "red", method = "loess") +
  geom_point(data = get(paste(Data_label, "_", i, "_TC_3_4.5", sep = "")), aes(`KMTs.length`, Curvature), color = "red", alpha = 0.2)
print(P1)
}

assign(paste(Data_label, "_", "_TC_0_1.5_ALL", sep = ""),
       rbind(get(paste(Data_label, "_", 1, "_TC_0_1.5", sep = "")),
             get(paste(Data_label, "_", 2, "_TC_0_1.5", sep = "")),
             get(paste(Data_label, "_", 3, "_TC_0_1.5", sep = ""))))
assign(paste(Data_label, "_", "_TC_1.5_3_ALL", sep = ""),
       rbind(get(paste(Data_label, "_", 1, "_TC_1.5_3", sep = "")),
             get(paste(Data_label, "_", 2, "_TC_1.5_3", sep = "")),
             get(paste(Data_label, "_", 3, "_TC_1.5_3", sep = ""))))
assign(paste(Data_label, "_", "_TC_3_4.5_ALL", sep = ""),
       rbind(get(paste(Data_label, "_", 1, "_TC_3_4.5", sep = "")),
             get(paste(Data_label, "_", 2, "_TC_3_4.5", sep = "")),
             get(paste(Data_label, "_", 3, "_TC_3_4.5", sep = ""))))

P2 <- ggplot(get(paste(Data_label, "_", "_TC_0_1.5_ALL", sep = "")), aes(`KMTs.length`, Curvature)) + geom_smooth(method = "loess", color = "blue") + ylim(1, 1.6) + xlim(0, 10) +
  theme_classic()
P2 <- P2 + geom_smooth(data = get(paste(Data_label, "_", "_TC_1.5_3_ALL", sep = "")), aes(`KMTs.length`, Curvature), color = "green", method = "loess")

P2 <- P2 + geom_smooth(data = get(paste(Data_label, "_", "_TC_3_4.5_ALL", sep = "")), aes(`KMTs.length`, Curvature), color = "red", method = "loess")
print(P2)

TC_ALL <- rbind(WT_1_Total_Curvature,
                WT_2_Total_Curvature,
                WT_3_Total_Curvature)
P3 <- ggplot(get(paste(Data_label, "_", 1, "_Total_Curvature", sep = "")), aes(`KMTs length`, Curvature)) + geom_smooth(method = "loess", color = "grey25") + ylim(1, 1.6) + xlim(0, 10) +
  theme_classic()
P3 <- P3 + geom_smooth(data = get(paste(Data_label, "_", 2, "_Total_Curvature", sep = "")), aes(`KMTs length`, Curvature), color = "grey45", method = "loess")

P3 <- P3 + geom_smooth(data = get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), aes(`KMTs length`, Curvature), color = "grey65", method = "loess")

P3 <- P3 + geom_smooth(data = TC_ALL, aes(`KMTs length`, Curvature), color = "black", method = "loess")
print(P3)
