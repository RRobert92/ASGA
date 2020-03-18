#########################################
# Count relative position for each KMTs #
#########################################
## The output of this function is added column [5] with Relative position at each KMTs data.frame
## At the end of the function, to each PoleX_YY new column is added with [6]: Relative (-) end position and [7]: Relative (+) end position

###########################
# Progress bar for Pole_1 #
###########################

total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
pb <- winProgressBar(min = 2,
                     max =  total,
                     width = 400)

##################################################
# Loop iterating through each KMT for the Pole_1 #
##################################################

  for(i in which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
    tryCatch({
       ## find KMTs closest to the kinetochore for y
    Point_KMT <- data.frame()
    for(j in 1:nrow(get(colnames(Segments)[i]))){
      Point_KMT[j,1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[1,3]
    } 
    Point_KMT <- which(Point_KMT[1] == min(Point_KMT))
    longest <- get(paste(colnames(Segments)[i], Point_KMT, sep = "_"))

    for(j in 1:nrow(get(paste(colnames(Segments)[i])))){
          assign(paste(colnames(Segments)[i], j, sep = "_"),
           relativ_pos_1(longest,
                         get(paste(colnames(Segments)[i], j, sep = "_"))))
    }
    Point_minus <- data.frame()
    for(j in 1:nrow(get(colnames(Segments)[i]))){
      Point_minus[j,1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[nrow(get(paste(colnames(Segments)[i], j, sep = "_"))),5]
    }
    names(Point_minus)[1] <- "Relative_minus_position"
    
    Point_plus <- data.frame()
    for(j in 1:nrow(get(colnames(Segments)[i]))){
      Point_plus[j,1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[1,5]
    }
    names(Point_plus)[1] <- "Relative_plus_position"
    
    assign(paste(colnames(Segments)[i]),
           cbind(get(paste(colnames(Segments)[i])),
                 Point_plus,
                 Point_minus))
    },
    error = function(e){})
     
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    title = paste("Calcualting relative position for Pole_1", 
                                  round((i - 1) / total * 100,
                                        0),
                                  "% Done"))
} 
close(pb)

###########################
# Progress bar for Pole_2 #
###########################

total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
pb <- winProgressBar(min = 0,
                     max =  total,
                     width = 400)

###################################################
# Loop iterating through each KMT for the Pole_12 #
###################################################

for(i in which(colnames(Segments) == "Pole2_00") : as.numeric(ncol(Segments) - 4)){
  tryCatch({
      ## find KMTs closest to the kinetochore for y
  Point_KMT <- data.frame()
  for(j in 1:nrow(get(colnames(Segments)[i]))){
    Point_KMT[j,1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[1,3]
  } 
  Point_KMT <- which(Point_KMT[1] == max(Point_KMT))
  longest <- get(paste(colnames(Segments)[i], Point_KMT, sep = "_"))
  
  for(j in 1:nrow(get(paste(colnames(Segments)[i])))){
    assign(paste(colnames(Segments)[i], j, sep = "_"),
           relativ_pos_2(longest,
                         get(paste(colnames(Segments)[i], j, sep = "_"))))
  }
  Point_minus <- data.frame()
  for(j in 1:nrow(get(colnames(Segments)[i]))){
    Point_minus[j,1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[nrow(get(paste(colnames(Segments)[i], j, sep = "_"))),5]
  }
  names(Point_minus)[1] <- "Relative_minus_position"
  
  Point_plus <- data.frame()
  for(j in 1:nrow(get(colnames(Segments)[i]))){
    Point_plus[j,1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[1,5]
  }
  names(Point_plus)[1] <- "Relative_plus_position"
  
  assign(paste(colnames(Segments)[i]),
         cbind(get(paste(colnames(Segments)[i])),
               Point_plus,
               Point_minus))

  },
  error = function(e){})
Sys.sleep(0.1)
  setWinProgressBar(pb, i - as.numeric(which(colnames(Segments) == "Pole2_00")), 
                    title = paste("Calcualting relative position for Pole_2",
                                  round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                                        0), 
                                  "% Done"))
} 
close(pb)

Minus_end_position <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["minus_dist_to_pole"]
Relative_position <- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["Relative_minus_position"]
Minus_end_position <- cbind(Minus_end_position, Relative_position)

for (i in as.numeric(which(colnames(Segments) == "Pole1_00")+1) : as.numeric(ncol(Segments) - 4)) {
  tryCatch({
  DF_Minus_end_position <- get(paste(colnames(Segments)[i]))["minus_dist_to_pole"]
  DF_Relative_position <- get(paste(colnames(Segments)[i]))["Relative_minus_position"]
  DF <- cbind(DF_Minus_end_position,
              DF_Relative_position)
  Minus_end_position <- rbind(Minus_end_position,
                              DF)
  },
  error = function(e){})
  
}

rm(DF,
   DF_Minus_end_position,
   DF_Relative_position,
   longest,
   Relative_position,
   Point_minus,
   Point_plus)