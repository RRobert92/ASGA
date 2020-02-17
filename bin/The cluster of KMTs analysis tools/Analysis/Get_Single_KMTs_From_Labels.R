total <- as.numeric(ncol(Segments %>% select(starts_with("Pole"))))
pb <- tkProgressBar(title = "Finding KMTs for each fiber...",
                    min = 0,
                    max =  total,
                    width = 300)
colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]
for (i in which(colnames(Segments) == "Pole1_00") : which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))])) {
  assign(colnames(Segments)[i], 
         Sort_by_fiber(colnames(Segments)[i]))
  
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"), 
           Select_Points(j, get(colnames(Segments)[i])))
    j = j + 1
  }
  
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"),
           Find_XYZ(get(paste(colnames(Segments)[i], j, sep = "_"))))
    j = j + 1
  }
  Sys.sleep(0.1)
  setTkProgressBar(pb, i, 
                   label = paste(round(i / total * 100, 0), "% Done"))
}
close(pb)

assign("Kinetochore_projected", 
       Kinetochore_position())