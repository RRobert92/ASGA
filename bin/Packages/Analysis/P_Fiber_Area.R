##########################################################
# Package to analyze fiber area and neighborhood density # 
##########################################################

## Find leading KMTs in the fiber for Pole1
for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
  assign(paste(colnames(Segments)[i]), 
         leading_KMTsv2(i, Pole1))
}

## Find leading KMTs in the fiber for Pole2
for (i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments) - 4)) {
  assign(paste(colnames(Segments)[i]), 
         leading_KMTsv2(i, Pole2))
}