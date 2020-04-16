#############################################################
# Set of functions to extract the single KMTs from raw data #
#############################################################
## The output of this function is two set of data
## First one is named PoleX_YY
## Second is named PoleX_YY_ZZ
## X <- Pole number
## YY <- Number of a fiber. Numbering is 00, 01,... etc.
## ZZ <- Number of a KMT in a fiber. Numbering is 1, 2,..., 10, 11,... etc.

################
# Progress bar #
################

total <- as.numeric(ncol(Segments_KMT %>% select(starts_with("Pole"))))
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 300)

#####################################
# Loop iterating through each label #
#####################################

for (i in which(colnames(Segments_KMT) == "Pole1_00") : which(colnames(Segments_KMT) == colnames(Segments_KMT %>% select(starts_with("Pole")))[ncol(Segments_KMT %>% select(starts_with("Pole")))])) {
  assign(colnames(Segments_KMT)[i],
         Sort_by_fiber(colnames(Segments_KMT)[i]))
  
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments_KMT)[i])))) {
    assign(paste(colnames(Segments_KMT)[i], 
                 j, 
                 sep = "_"),
           Select_Points(j, 
                         get(colnames(Segments_KMT)[i])))
    j = j + 1
  }
  
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments_KMT)[i])))) {
    assign(paste(colnames(Segments_KMT)[i], 
                 j, 
                 sep = "_"),
           Find_XYZ(get(paste(
             colnames(Segments_KMT)[i], 
             j, 
             sep = "_"))))
    j = j + 1
  }
  
  Sys.sleep(0.1)
  setWinProgressBar(pb, 
                    i,
                    title = paste("Finding KMTs for each fiber...",
                                  round(i / total * 100, 
                                        0), 
                                  "% Done"))
}
close(pb)
