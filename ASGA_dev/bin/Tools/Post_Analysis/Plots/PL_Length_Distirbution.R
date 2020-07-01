for(i in 1:No_of_Data){
  LD100max <- max(get(paste(Data_label, "_", i, "_LD", sep = ""))[3])
  LD100min <- min(get(paste(Data_label, "_", i, "_LD", sep = ""))[3])
  LD100 <- LD100max
  LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
  LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
  
  LD_45 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_pole <= LD45 & plus_dist_to_pole > 0),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_pole <= LD45 & plus_dist_to_pole > 0),][,2])
  LD_80 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_pole <= LD80 & plus_dist_to_pole > LD45),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")),
                                                                              plus_dist_to_pole <= LD80 & plus_dist_to_pole > LD45),][,2])
  LD_100 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_pole <= LD100 & plus_dist_to_pole > LD80),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_pole <= LD100 & plus_dist_to_pole > LD80),][,2])
  
  P <- ggplot(LD_45, aes("1", KMTs.length)) + geom_violin(fill = "dodgerblue4") + geom_jitter(width = 0.2) + theme_classic()
  P <- P + geom_violin(data = LD_80, aes("2", KMTs.length), fill = "olivedrab4") + geom_jitter(data = LD_80, aes("2", KMTs.length), width = 0.2)
  P <- P + geom_violin(data = LD_100, aes("3", KMTs.length), fill = "firebrick4") + geom_jitter(data = LD_100, aes("3", KMTs.length), width = 0.2)
  print(P)
  
  write.xlsx(LD_45, paste("bin/Output/", Data_label, "_", i, "_LD_Pole_45.xlsx" ,sep = ""))
  write.xlsx(LD_80, paste("bin/Output/", Data_label, "_", i, "_LD_Pole_80.xlsx" ,sep = ""))
  write.xlsx(LD_100, paste("bin/Output/", Data_label, "_", i, "_LD_Pole_100.xlsx" ,sep = ""))
}

for(i in 1:No_of_Data){
  LD100max <- max(get(paste(Data_label, "_", i, "_LD", sep = ""))[2])
  LD100min <- min(get(paste(Data_label, "_", i, "_LD", sep = ""))[2])
  LD100 <- LD100max
  LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
  LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
  
  LD_45 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= LD45 & plus_dist_to_kinetochore_core > 0),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= LD45 & plus_dist_to_kinetochore_core > 0),][,2])
  LD_80 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= LD80 & plus_dist_to_kinetochore_core > LD45),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")),
                                                                              plus_dist_to_kinetochore_core <= LD80 & plus_dist_to_kinetochore_core > LD45),][,2])
  LD_100 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= LD100 & plus_dist_to_kinetochore_core > LD80),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= LD100 & plus_dist_to_kinetochore_core > LD80),][,2])
  
  P <- ggplot(LD_45, aes("1", KMTs.length)) + geom_violin(fill = "dodgerblue4") + geom_jitter(width = 0.2) + theme_classic()
  P <- P + geom_violin(data = LD_80, aes("2", KMTs.length), fill = "olivedrab4") + geom_jitter(data = LD_80, aes("2", KMTs.length), width = 0.2)
  P <- P + geom_violin(data = LD_100, aes("3", KMTs.length), fill = "firebrick4") + geom_jitter(data = LD_100, aes("3", KMTs.length), width = 0.2)
  print(P)
  
  write.xlsx(LD_45, paste("bin/Output/", Data_label, "_", i, "_LD_Kcore_45.xlsx" ,sep = ""))
  write.xlsx(LD_80, paste("bin/Output/", Data_label, "_", i, "_LD_Kcore_80.xlsx" ,sep = ""))
  write.xlsx(LD_100, paste("bin/Output/", Data_label, "_", i, "_LD_Kcore_100.xlsx" ,sep = ""))
}

for(i in 1:No_of_Data){
  LD_45 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              Elipse_Position == "25%" ),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              Elipse_Position == "25%"),][,2])
  LD_80 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              Elipse_Position == "50%"),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")),
                                                                              Elipse_Position == "50%"),][,2])
  LD_100 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              Elipse_Position == "100%"),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              Elipse_Position == "100%"),][,2])
  
  P <- ggplot(LD_45, aes("1", KMTs.length)) + geom_violin(fill = "dodgerblue4") + geom_jitter(width = 0.2) + theme_classic()
  P <- P + geom_violin(data = LD_80, aes("2", KMTs.length), fill = "olivedrab4") + geom_jitter(data = LD_80, aes("2", KMTs.length), width = 0.2)
  P <- P + geom_violin(data = LD_100, aes("3", KMTs.length), fill = "firebrick4") + geom_jitter(data = LD_100, aes("3", KMTs.length), width = 0.2)
  print(P)
  
  write.xlsx(LD_45, paste("bin/Output/", Data_label, "_", i, "_LD_E_45.xlsx" ,sep = ""))
  write.xlsx(LD_80, paste("bin/Output/", Data_label, "_", i, "_LD_E_80.xlsx" ,sep = ""))
  write.xlsx(LD_100, paste("bin/Output/", Data_label, "_", i, "_LD_E_100.xlsx" ,sep = ""))
}