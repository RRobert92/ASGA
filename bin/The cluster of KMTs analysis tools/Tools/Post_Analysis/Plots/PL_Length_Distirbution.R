for(i in 1:No_of_Data){
  LD_0_1.5 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= 1.5 & plus_dist_to_kinetochore_core > 0),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= 1.5 & plus_dist_to_kinetochore_core > 0),][,2])
  LD_1.5_3 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= 3 & plus_dist_to_kinetochore_core > 1.5),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")),
                                                                              plus_dist_to_kinetochore_core <= 3 & plus_dist_to_kinetochore_core > 1.5),][,2])
  LD_3_4.5 <- data.frame(get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= 10 & plus_dist_to_kinetochore_core > 3),][,1],
                         get(paste(Data_label, "_", i, "_LD", sep = ""))[with(get(paste(Data_label, "_", i, "_LD", sep = "")), 
                                                                              plus_dist_to_kinetochore_core <= 10 & plus_dist_to_kinetochore_core > 3),][,2])
  write.xlsx(LD_0_1.5, paste("Output/", Data_label, "_", i, "_LD_0_1.5.xlsx", sep = ""))
  write.xlsx(LD_1.5_3, paste("Output/", Data_label, "_", i, "_LD_1.5_3.xlsx", sep = ""))
  write.xlsx(LD_3_4.5, paste("Output/", Data_label, "_", i, "_LD_3_4.5.xlsx", sep = ""))
  
  P <- ggplot(LD_0_1.5, aes("1", KMTs.length)) + geom_violin(fill = "dodgerblue4") + geom_jitter(width = 0.2) + theme_classic()
  P <- P + geom_violin(data = LD_1.5_3, aes("2", KMTs.length), fill = "olivedrab4") + geom_jitter(data = LD_1.5_3, aes("2", KMTs.length), width = 0.2)
  P <- P + geom_violin(data = LD_3_4.5, aes("3", KMTs.length), fill = "firebrick4") + geom_jitter(data = LD_3_4.5, aes("3", KMTs.length), width = 0.2)
  print(P)
  
  
}
