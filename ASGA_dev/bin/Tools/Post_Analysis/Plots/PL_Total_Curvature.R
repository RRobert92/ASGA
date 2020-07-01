for(i in 1:No_of_Data){
  LD100max <- max(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[5])
  LD100min <- min(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[5])
  LD100 <- LD100max
  LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
  LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
  
  LD_45 <- data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")),
                                                                                        `(+) Dist-to-Pole` <= LD45 & `(+) Dist-to-Pole` > 0),][,1],
                      get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                        `(+) Dist-to-Pole` <= LD45 & `(+) Dist-to-Pole` > 0),][,3])
  LD_80 <- data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                        `(+) Dist-to-Pole` <= LD80 & `(+) Dist-to-Pole` > LD45),][,1],
                      get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")),
                                                                                        `(+) Dist-to-Pole` <= LD80 & `(+) Dist-to-Pole` > LD45),][,3])
  LD_100 <- data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                         `(+) Dist-to-Pole` <= LD100 & `(+) Dist-to-Pole` > LD80),][,1],
                       get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                         `(+) Dist-to-Pole` <= LD100 & `(+) Dist-to-Pole` > LD80),][,3])
  P <- ggplot(LD_45, aes(KMTs.length, Curvature)) + geom_smooth(fill = "dodgerblue4", method = "gam") + 
    geom_jitter( alpha = 0.2, color = "dodgerblue4") + theme_classic()
  P <- P + geom_smooth(data = LD_80, aes(KMTs.length, Curvature), fill = "olivedrab4", method = "gam") +
    geom_jitter(data = LD_80, aes(KMTs.length, Curvature), color = "olivedrab4", alpha = 0.2)
  P <- P + geom_smooth(data = LD_100, aes(KMTs.length, Curvature), fill = "firebrick4", method = "gam") + 
    geom_jitter(data = LD_100, aes(KMTs.length, Curvature), color = "firebrick4", alpha = 0.2)
  print(P)
  
  write.xlsx(LD_45, paste("bin/Output/", Data_label, "_", i, "_TC_Pole_45.xlsx" ,sep = ""))
  write.xlsx(LD_80, paste("bin/Output/", Data_label, "_", i, "_TC_Pole_80.xlsx" ,sep = ""))
  write.xlsx(LD_100, paste("bin/Output/", Data_label, "_", i, "_TC_Pole_100.xlsx" ,sep = ""))
}

for(i in 1:No_of_Data){
  LD100max <- max(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[4])
  LD100min <- min(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[4])
  LD100 <- LD100max
  LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
  LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
  
  LD_45 <- data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                           `(+) end position` <= LD45 & `(+) end position` > 0),][,1],
                      get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                           `(+) end position` <= LD45 & `(+) end position` > 0),][,3])
  LD_80 <- data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                           `(+) end position` <= LD80 & `(+) end position` > LD45),][,1],
                      get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")),
                                                                           `(+) end position` <= LD80 & `(+) end position` > LD45),][,3])
  LD_100 <- data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                            `(+) end position` <= LD100 & `(+) end position` > LD80),][,1],
                       get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                            `(+) end position` <= LD100 & `(+) end position` > LD80),][,3])
  
  P <- ggplot(LD_45, aes(KMTs.length, Curvature)) + geom_smooth(fill = "dodgerblue4", method = "gam") + 
    geom_jitter( alpha = 0.2, color = "dodgerblue4") + theme_classic()
  P <- P + geom_smooth(data = LD_80, aes(KMTs.length, Curvature), fill = "olivedrab4", method = "gam") + 
    geom_jitter(data = LD_80, aes(KMTs.length, Curvature), color = "olivedrab4", alpha = 0.2)
  P <- P + geom_smooth(data = LD_100, aes(KMTs.length, Curvature), fill = "firebrick4", method = "gam") + 
    geom_jitter(data = LD_100, aes(KMTs.length, Curvature), color = "firebrick4", alpha = 0.2)
  print(P)
  
  write.xlsx(LD_45, paste("bin/Output/", Data_label, "_", i, "_TC_Kcore_45.xlsx" ,sep = ""))
  write.xlsx(LD_80, paste("bin/Output/", Data_label, "_", i, "_TC_Kcore_80.xlsx" ,sep = ""))
  write.xlsx(LD_100, paste("bin/Output/", Data_label, "_", i, "_TC_Kcore_100.xlsx" ,sep = ""))
}

for(i in 1:No_of_Data){
  LD_45 <- data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                        `Elipse Position` == "25%" ),][,1],
                      get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                        `Elipse Position` == "25%"),][,3])
  LD_80 <- data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                        `Elipse Position` == "50%"),][,1],
                      get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")),
                                                                                        `Elipse Position` == "50%"),][,3])
  LD_100 <- data.frame(get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                         `Elipse Position` == "100%"),][,1],
                       get(paste(Data_label, "_", i, "_Total_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Total_Curvature", sep = "")), 
                                                                                         `Elipse Position` == "100%"),][,3])
  
  P <- ggplot(LD_45, aes(KMTs.length, Curvature)) + geom_smooth(fill = "dodgerblue4", method = "gam") + 
    geom_jitter( alpha = 0.2, color = "dodgerblue4") + theme_classic()
  P <- P + geom_smooth(data = LD_80, aes(KMTs.length, Curvature), fill = "olivedrab4", method = "gam") + 
    geom_jitter(data = LD_80, aes(KMTs.length, Curvature), color = "olivedrab4", alpha = 0.2)
  P <- P + geom_smooth(data = LD_100, aes(KMTs.length, Curvature), fill = "firebrick4", method = "gam") + 
    geom_jitter(data = LD_100, aes(KMTs.length, Curvature), color = "firebrick4", alpha = 0.2)
  print(P)
  
  write.xlsx(LD_45, paste("bin/Output/", Data_label, "_", i, "_TC_E_45.xlsx" ,sep = ""))
  write.xlsx(LD_80, paste("bin/Output/", Data_label, "_", i, "_TC_E_80.xlsx" ,sep = ""))
  write.xlsx(LD_100, paste("bin/Output/", Data_label, "_", i, "_TC_E_100.xlsx" ,sep = ""))
}