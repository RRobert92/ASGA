##################################################################
# The analysis tool count no. of KMTs with a (-) end at the pole #
##################################################################
## x variablein in the function is a number of a column contain KMTs information (e.g. 1, 2, 3...., etc.)
## Count how many KMTs with a minus end distance of "Minus_Threshold" is in the fiber

KMTs_at_the_Pole <- KMTs_to_the_Pole(which(colnames(Segments) == "Pole1_00"))
names(KMTs_at_the_Pole)[1] <- "No. of KMTs"

KMTs_to_the_Pole_and_length <- KMTs_to_the_Pole_vs_length(which(colnames(Segments) == "Pole1_00"))
names(KMTs_to_the_Pole_and_length)[1] <- "No. of KMTs"
names(KMTs_to_the_Pole_and_length)[2] <- "KMTs length"
names(KMTs_to_the_Pole_and_length)[3] <- "Minus end dist."
names(KMTs_to_the_Pole_and_length)[4] <- "Plus end dist. to k-core"
names(KMTs_to_the_Pole_and_length)[5] <- "Plus end dist. to pole"

total <- as.numeric(ncol(Segments %>% select(starts_with("Pole"))))
pb <- tkProgressBar(title = "Calculating no. of KMTs reaching the pole...",
                    min = 2,
                    max =  total,
                    width = 400)

DF1 <- data.frame()
DF2 <- data.frame()

for (i in which(colnames(Segments) == "Pole1_01") : which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))])) {
  tryCatch({
    assign("DF1",
           KMTs_to_the_Pole(i))
    names(DF1)[1] <- "No. of KMTs"
    KMTs_at_the_Pole <- rbind(KMTs_at_the_Pole, 
                              DF1)
    
    assign("DF2",
           KMTs_to_the_Pole_vs_length(i))
    
    names(DF2)[1] <- "No. of KMTs"
    names(DF2)[2] <- "KMTs length"
    names(DF2)[3] <- "Minus end dist."
    names(DF2)[4] <- "Plus end dist. to k-core"
    names(DF2)[5] <- "Plus end dist. to pole"
    
    KMTs_to_the_Pole_and_length <- rbind(KMTs_to_the_Pole_and_length, 
                                         DF2)
  },
  error = function(e){})
  Sys.sleep(0.1)
  setTkProgressBar(pb, i, 
                   label = paste(round((i - 1) / total * 100, 
                                       0), 
                                 "% Done"))
}

close(pb)

KMTs_at_the_Pole <- data.frame(KMTs_at_the_Pole[,1])
names(KMTs_at_the_Pole)[1] <- "No. of KMTs"

rm(DF, DF2)
