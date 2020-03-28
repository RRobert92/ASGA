for(i in 1:No_of_Data){
  LD100max <- max(get(paste(Data_label, "_", i, "_LD", sep = ""))[3])
  LD100min <- min(get(paste(Data_label, "_", i, "_LD", sep = ""))[3])
  LD100 <- LD100max
  LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
  LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
  
  LD_0_1.5 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_pole <= LD45 & plus_dist_to_pole > 0),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_pole <= LD45 & plus_dist_to_pole > 0),][,2])
  LD_1.5_3 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_pole <= LD80 & plus_dist_to_pole > LD45),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")),
                                                                              plus_dist_to_pole <= LD80 & plus_dist_to_pole > LD45),][,2])
  LD_3_4.5 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_pole <= LD100 & plus_dist_to_pole > LD80),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_pole <= LD100 & plus_dist_to_pole > LD80),][,2])
  
  P <- ggplot(LD_0_1.5, aes("1", KMTs.length)) + geom_violin(fill = "dodgerblue4") + geom_jitter(width = 0.2) + theme_classic()
  P <- P + geom_violin(data = LD_1.5_3, aes("2", KMTs.length), fill = "olivedrab4") + geom_jitter(data = LD_1.5_3, aes("2", KMTs.length), width = 0.2)
  P <- P + geom_violin(data = LD_3_4.5, aes("3", KMTs.length), fill = "firebrick4") + geom_jitter(data = LD_3_4.5, aes("3", KMTs.length), width = 0.2)
  print(P)
}

for(i in 1:No_of_Data){
  LD100max <- max(get(paste(Data_label, "_", i, "_LD", sep = ""))[2])
  LD100min <- min(get(paste(Data_label, "_", i, "_LD", sep = ""))[2])
  LD100 <- LD100max
  LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
  LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
  
 LD_0_1.5 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= LD45 & plus_dist_to_kinetochore_core > 0),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= LD45 & plus_dist_to_kinetochore_core > 0),][,2])
  LD_1.5_3 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= LD80 & plus_dist_to_kinetochore_core > LD45),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")),
                                                                              plus_dist_to_kinetochore_core <= LD80 & plus_dist_to_kinetochore_core > LD45),][,2])
  LD_3_4.5 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= LD100 & plus_dist_to_kinetochore_core > LD80),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= LD100 & plus_dist_to_kinetochore_core > LD80),][,2])
  
  P <- ggplot(LD_0_1.5, aes("1", KMTs.length)) + geom_violin(fill = "dodgerblue4") + geom_jitter(width = 0.2) + theme_classic()
  P <- P + geom_violin(data = LD_1.5_3, aes("2", KMTs.length), fill = "olivedrab4") + geom_jitter(data = LD_1.5_3, aes("2", KMTs.length), width = 0.2)
  P <- P + geom_violin(data = LD_3_4.5, aes("3", KMTs.length), fill = "firebrick4") + geom_jitter(data = LD_3_4.5, aes("3", KMTs.length), width = 0.2)
  print(P)
}

for(i in 1:No_of_Data){
  LD_0_1.5 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              Elipse_Position == "25%" ),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              Elipse_Position == "25%"),][,2])
  LD_1.5_3 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              Elipse_Position == "50%"),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")),
                                                                              Elipse_Position == "50%"),][,2])
  LD_3_4.5 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              Elipse_Position == "100%"),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              Elipse_Position == "100%"),][,2])
  
  P <- ggplot(LD_0_1.5, aes("1", KMTs.length)) + geom_violin(fill = "dodgerblue4") + geom_jitter(width = 0.2) + theme_classic()
  P <- P + geom_violin(data = LD_1.5_3, aes("2", KMTs.length), fill = "olivedrab4") + geom_jitter(data = LD_1.5_3, aes("2", KMTs.length), width = 0.2)
  P <- P + geom_violin(data = LD_3_4.5, aes("3", KMTs.length), fill = "firebrick4") + geom_jitter(data = LD_3_4.5, aes("3", KMTs.length), width = 0.2)
  print(P)
}