###################################################
# The Package to count Inter-kinetochore idstance #
###################################################
## The output of this function is data,framed named 'Inter_Kinetochore_Distance' with a distance in [um] for each pair.

if(IKD == "yes"){
  Inter_Kinetochore_Distance <- Inter_Kinetochore_Dist()
  Inter_Kinetochore_Distance_KMTs_no <- Compare_KMTs_no_for_sister()
  Inter_Kinetochore_Distance_KMTs_delta <- Compare_KMTs_delta_for_sister()
  
} else if(IKD == "no"){
 
}
rm(pb, DF1)
