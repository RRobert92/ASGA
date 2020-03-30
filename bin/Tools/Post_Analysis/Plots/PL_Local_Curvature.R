for(i in 1:No_of_Data){
  LD100max <- max(get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[6])
  LD100min <- min(get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[6])
  LD100 <- LD100max
  LD80 <- ((80*(LD100max-LD100min))+(100*LD100min))/100
  LD45 <- ((45*(LD100max-LD100min))+(100*LD100min))/100
  
  LD_45 <- data.frame(get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")),
                                                                                        End_to_Pole <= LD45 & End_to_Pole > 0),][,1],
                      get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")), 
                                                                                        End_to_Pole <= LD45 & End_to_Pole > 0),][,2])
  LD_80 <- data.frame(get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")), 
                                                                                        End_to_Pole <= LD80 & End_to_Pole > LD45),][,1],
                      get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")),
                                                                                        End_to_Pole <= LD80 & End_to_Pole > LD45),][,2])
  LD_100 <- data.frame(get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")), 
                                                                                         End_to_Pole <= LD100 & End_to_Pole > LD80),][,1],
                       get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")), 
                                                                                         End_to_Pole <= LD100 & End_to_Pole > LD80),][,2])
  
  local_full <- data.frame()
  n = 1
  for(j in 1:11){
    local_full[j,1:3] <- data.frame(Curvature = median(LD_45[with(LD_45, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]),
                                    Relative_Position = median(LD_45[with(LD_45, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,2]),
                                    SD_error = sd(LD_45[with(LD_45, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]) / 
                                      sqrt(length(LD_45[with(LD_45, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1])))
    n = n - 0.1
  }
  LD_45_median <- local_full
  
  local_full <- data.frame()
  n = 1
  for(j in 1:11){
    local_full[j,1:3] <- data.frame(Curvature = median(LD_80[with(LD_80, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]),
                                    Relative_Position = median(LD_80[with(LD_80, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,2]),
                                    SD_error = sd(LD_80[with(LD_80, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]) / 
                                      sqrt(length(LD_80[with(LD_80, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1])))
    n = n - 0.1
  }
  LD_80_median <- local_full
  
  local_full <- data.frame()
  n = 1
  for(j in 1:11){
    local_full[j,1:3] <- data.frame(Curvature = median(LD_100[with(LD_100, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]),
                                    Relative_Position = median(LD_100[with(LD_100, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,2]),
                                    SD_error = sd(LD_100[with(LD_100, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]) / 
                                      sqrt(length(LD_100[with(LD_100, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1])))
    n = n - 0.1
  }
  LD_100_median <- local_full
  
  
  
  write.xlsx(LD_45, paste("bin/Output/", Data_label, "_", i, "_LC_Pole_45.xlsx" ,sep = ""))
  write.xlsx(LD_45_median, paste("bin/Output/", Data_label, "_", i, "_LC_Pole_m_45.xlsx" ,sep = ""))
  write.xlsx(LD_80, paste("bin/Output/", Data_label, "_", i, "_LC_Pole_80.xlsx" ,sep = ""))
  write.xlsx(LD_80_median, paste("bin/Output/", Data_label, "_", i, "_LC_Pole_m_80.xlsx" ,sep = ""))
  write.xlsx(LD_100, paste("bin/Output/", Data_label, "_", i, "_LC_Pole_100.xlsx" ,sep = ""))
  write.xlsx(LD_100_median, paste("bin/Output/", Data_label, "_", i, "_LC_Pole_m_100.xlsx" ,sep = ""))
}


for(i in 1:No_of_Data){
  LD_45 <- data.frame(get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")), 
                                                                                        Elipse_Position == "25%" ),][,1],
                      get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")), 
                                                                                        Elipse_Position == "25%"),][,2])
  LD_80 <- data.frame(get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")), 
                                                                                        Elipse_Position == "50%"),][,1],
                      get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")),
                                                                                        Elipse_Position == "50%"),][,2])
  LD_100 <- data.frame(get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")), 
                                                                                         Elipse_Position == "100%"),][,1],
                       get(paste(Data_label, "_", i, "_Local_Curvature", sep = ""))[with(get(paste(Data_label, "_", i, "_Local_Curvature", sep = "")), 
                                                                                         Elipse_Position == "100%"),][,2])
  
  local_full <- data.frame()
  n = 1
  for(j in 1:11){
    local_full[j,1:3] <- data.frame(Curvature = median(LD_45[with(LD_45, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]),
                                    Relative_Position = median(LD_45[with(LD_45, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,2]),
                                    SD_error = sd(LD_45[with(LD_45, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]) / 
                                      sqrt(length(LD_45[with(LD_45, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1])))
    n = n - 0.1
  }
  LD_45_median <- local_full
  
  local_full <- data.frame()
  n = 1
  for(j in 1:11){
    local_full[j,1:3] <- data.frame(Curvature = median(LD_80[with(LD_80, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]),
                                    Relative_Position = median(LD_80[with(LD_80, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,2]),
                                    SD_error = sd(LD_80[with(LD_80, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]) / 
                                      sqrt(length(LD_80[with(LD_80, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1])))
    n = n - 0.1
  }
  LD_80_median <- local_full
  
  local_full <- data.frame()
  n = 1
  for(j in 1:11){
    local_full[j,1:3] <- data.frame(Curvature = median(LD_100[with(LD_100, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]),
                                    Relative_Position = median(LD_100[with(LD_100, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,2]),
                                    SD_error = sd(LD_100[with(LD_100, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1]) / 
                                      sqrt(length(LD_100[with(LD_100, Relative_Position < n & Relative_Position > as.numeric(n-0.101)),][,1])))
    n = n - 0.1
  }
  LD_100_median <- local_full
  
  write.xlsx(LD_45, paste("bin/Output/", Data_label, "_", i, "_LC_E_45.xlsx" ,sep = ""))
  write.xlsx(LD_45_median, paste("bin/Output/", Data_label, "_", i, "_LC_E_m_45.xlsx" ,sep = ""))
  write.xlsx(LD_80, paste("bin/Output/", Data_label, "_", i, "_LC_E_80.xlsx" ,sep = ""))
  write.xlsx(LD_80_median, paste("bin/Output/", Data_label, "_", i, "_LC_E_m_80.xlsx" ,sep = ""))
  write.xlsx(LD_100, paste("bin/Output/", Data_label, "_", i, "_LC_E_100.xlsx" ,sep = ""))
  write.xlsx(LD_100_median, paste("bin/Output/", Data_label, "_", i, "_LC_E_m_100.xlsx" ,sep = ""))
}