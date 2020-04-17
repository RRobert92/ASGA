###################################################
# The Package to count Inter-kinetochore distance #
###################################################
## The output of this function is data,framed named 'Inter_Kinetochore_Distance' with a distance in [um] for each pair.

if(IKD == "yes" && nrow(Pole2_00) > 0){
  Inter_Kinetochore_Distance <- Inter_Kinetochore_Dist()
  Inter_Kinetochore_Distance_KMTs_no <- Compare_KMTs_no_for_sister()
  Inter_Kinetochore_Distance_KMTs_delta <- Compare_KMTs_delta_for_sister()
  
  write.xlsx(Inter_Kinetochore_Distance, paste("bin/Output/", Data_label, "_Inter_Kinetochore_Distance.xlsx", sep = ""), row.names = FALSE)
  rm(pb, DF1)
}
