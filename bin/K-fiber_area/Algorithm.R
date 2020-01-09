##Load data, bin of 1000A or 0.1um
library(readxl)
library(tidyverse)

Points <- read_excel("Pulpit/Metaphase_1_KMTs.resampled.rotated.2_75.0.-9_55.am.xlsx", 
                     sheet = "Points")
Points <- data.frame(Point_ID = c(Points$`Point ID`),
                     X_Coord = c(Points$`X Coord`)/10000,
                     Y_Coord = c(Points$`Y Coord`)/10000,
                     Z_Coord = c(Points$`Z Coord`)/10000)

Segments <- read_excel("Pulpit/Metaphase_1_KMTs.resampled.rotated.2_75.0.-9_55.am.xlsx",
                       sheet = "Segments")

##Define Pole1 and Pole2 position in um
Nodes <- read_excel("Pulpit/Metaphase_1_KMTs.resampled.rotated.2_75.0.-9_55.am.xlsx",
                    sheet = "Nodes")
Pole1 <- data.frame(X = c(Pole1 %>% select("X Coord")/10000), 
                    Y = c(Pole1 %>% select("Y Coord")/10000), 
                    Z = c(Pole1 %>% select("Z Coord")/10000))
Pole2 <- Nodes %>% filter_at(vars("Pole2"),
                             any_vars(.>=1))
Pole2 <- data.frame(X = c(Pole2 %>% select("X Coord")/10000), 
                    Y = c(Pole2 %>% select("Y Coord")/10000), 
                    Z = c(Pole2 %>% select("Z Coord")/10000))

##Select one fiber
Sort_by_fiber <- function(x) {
  fiber <- Segments %>% filter_at(vars(starts_with(x)), any_vars(. >= 1))
  fiber %>% select(1, ncol(fiber))
}

## remove "," and spread numbers in single cells
Select_Points <- function(x, y) {
  selected_points <- data.frame(str_split(gsub("[^[:digit:]]", ",", y[x, 2]), pattern = ","))
  points <- data.frame(Point_ID = selected_points[, 1])
}

## combin point_id with xyz
nrow_1 <- seq(from = 1,
              to = nrow(Segments),
              by = 1)

library(plyr)
Find_XYZ <- function(x) {
  joined_data <- join_all(list(x, Points),
                          by = "Point_ID")
  mutate_all(joined_data, 
             function(y) as.numeric(as.character(y)))
}

##sort point by there position in spindle pole axis first point is kinetochore
Sort_by_distance_to_pole1 <- function(y){
  position <- data.frame(x = c(Pole1[1,1], Pole1[1,1]),
                         y = c(Pole1[1,2], Pole1[1,2]),
                         z = c(Pole1[1,3], Pole1[1,3]),
                         x1 = c(y[1,2], y[nrow(y),2]),
                         y1 = c(y[1,3], y[nrow(y),3]),
                         z1 = c(y[1,4], y[nrow(y),4]))
  
  position$distance <- apply(position, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))
  
  if(position[1,7] < position[2,7]){
    y %>% arrange(desc(Point_ID))
  } else {
    y %>% arrange(Point_ID)
  }
}

##sort point by there position in spindle pole axis first point is kinetochore
Sort_by_distance_to_pole2 <- function(y){
  position <- data.frame(x = c(Pole2[1,1], Pole2[1,1]),
                         y = c(Pole2[1,2], Pole2[1,2]),
                         z = c(Pole2[1,3], Pole2[1,3]),
                         x1 = c(y[1,2], y[nrow(y),2]),
                         y1 = c(y[1,3], y[nrow(y),3]),
                         z1 = c(y[1,4], y[nrow(y),4]))
  
  position$distance <- apply(position, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))
  
  if(position[1,7] < position[2,7]){
    y %>% arrange(desc(Point_ID))
  } else {
    y %>% arrange(Point_ID)
  }
}

##Calculate ratio of KMT_length / (-) end distance to the pole
leading_KMTsv2 <- function(x, y) {
  j = 1
  leading <- data.frame(Leading = as.numeric())
  while (j <= as.numeric(nrow(get(colnames(Segments)[x])))) {
    KMT_lenght <- Segments[as.numeric(get(colnames(Segments)[x])[j, 1] + 1), as.numeric(ncol(Segments) - 3)] / 10000
    m_end_to_pole <- data.frame(x = c(y[1, 1]),
                                y = c(y[1, 2]),
                                z = c(y[1, 3]),
                                x1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 2]),
                                y1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 3]),
                                z1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 4]))
    m_end_to_pole$distance <- apply(m_end_to_pole, 1, function(z) dist(matrix(z,
                                                                              nrow = 2,
                                                                              byrow = TRUE)))
    leading[j, ] <- KMT_lenght[1, 1] / m_end_to_pole$distance[1]
    j = j + 1
  }
  bind_cols(get(paste(colnames(Segments)[x])), leading)
}

##find point for lading KMTS, for i = i+5 == 0.5um
Leadig_Points <- function(x) {
  i = 1
  leading_points <- data.frame(Leading_ID = as.numeric())
  while (i <= nrow(get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])[3])), sep = "_")))) {
    leading_points[i, ] <- get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])[3])), sep = "_"))[i, 1]
    i = i + 5
  }
  leading_points <- na.omit(leading_points)
}

##find all points which corespond to the slice of the fiber
find_polygon <- function(x) {
  i = 1
  lead_points_id <- data.frame(Distance = as.numeric())
  Distance <- data.frame(V1 = as.numeric())
  
  while (i <= as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    lead_points_id <- data.frame(X_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i, ] + 1), 2],
                                 Y_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i, ] + 1), 3],
                                 Z_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i, ] + 1), 4])
    j = 1
    lead_points_id_full <- data.frame(t = as.numeric())
    
    for (j in 1:as.numeric(nrow(get(paste(colnames(Segments)[x]))))) {
      lead_points_id_full <- apply(lead_points_id, MARGIN = 2,
                                   function(y) rep(y, as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_"))))))
      
      lead_points_id_full <- cbind(get(paste(colnames(Segments)[x], j, sep = "_"))[1], lead_points_id,
                                   get(paste(colnames(Segments)[x], j, sep = "_"))[2:4])
      
      lead_points_id_full$distance <- apply(lead_points_id_full[2:7], 1, 
                                            function(x) dist(matrix(x, 
                                                                    nrow = 2, 
                                                                    byrow = TRUE)))
      
      Distance[i, j] <- lead_points_id_full[as.numeric(which.min(lead_points_id_full$distance)), 1]
      lead_points_id_full <- data.frame(t = as.numeric())
    }
    i = i + 1
  }
  bind_cols(get(paste(colnames(Segments)[x], "fiber", sep = "_")), Distance)
}
library(base)
##remove duplicated points
duplicated_points <- function(x){
  DF <- get(paste(colnames(Segments)[x], "fiber",  sep = "_"))
  
  for(i in 1:ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_")))){
    DF[,i][duplicated(DF[,i])] <- NA
  }
  ##check if there is no hole in dataset if yes remove 
  for(i in 1:ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_")))){
    for(j in 2:nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_")))){
      if(is.na(DF[j-1,i]) && is.na(DF[j+2,i])){
      DF[j,i] <- NA
    }
    else{}
    }
  }
  DF
}

##get a median point for each positin and put cbin in first col
median_point <- function(x){
  ##for looop to creat df of x y z coord for eahc position
  ##mediana of x y z coord
  ## writ it in a table
  ##cbin with Pole1_00_fiber
  Median_id <- data.frame(X_Coord = as.numeric(),
                          Y_Coord = as.numeric(),
                          Z_Coord = as.numeric())
  
  for (i in 1:as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    DF <- data.frame(X_Coord = as.numeric(),
                     Y_Coord = as.numeric(),
                     Z_Coord = as.numeric())
    
    for (j in 1:as.numeric(ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
      DF[j,1] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),2]
      DF[j,2] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),3]
      DF[j,3] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),4]
    }
    
    Median_id[i,1] <- median(na.omit(DF$X_Coord))
    Median_id[i,2] <- median(na.omit(DF$Y_Coord))
    Median_id[i,3] <- median(na.omit(DF$Z_Coord))
  }
  cbind(Median_id, get(paste(colnames(Segments)[x], "fiber", sep = "_")))
}

##count geometry and find circular area
circular_area <- function(x){
  area <- data.frame(Circular_area = as.numeric())
  i=1
  for (i in 1:as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    DF <- data.frame(X_Coord = as.numeric(),
                     Y_Coord = as.numeric(),
                     Z_Coord = as.numeric())
    j=5
    for (j in 5:as.numeric(ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
      DF[j,1] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),2]
      DF[j,2] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),3]
      DF[j,3] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),4]
    }
    DF <- na.omit(DF)
    median_point <- as.data.frame(lapply(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,1:3], 
                                         rep, as.numeric(nrow(DF))))
    DF <- cbind(DF, median_point)
    DF$distance <- apply(DF,1,
                         function(y) dist(matrix(y,
                                                 nrow = 2,
                                                 byrow = TRUE)))
    area[i,] <- round(pi * DF[as.numeric(which.max(DF$distance)), as.numeric(ncol(DF))]^2, 3)
  }
  cbind(area, 
        get(paste(colnames(Segments)[x], "fiber", sep = "_")))
}

##count geometry and find polygon area
library(alphashape3d)
polygon_area <- function(x){
  area <- data.frame(Alpha_area = as.numeric())
  i=1
  for (i in 1:as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    DF <- data.frame(X_Coord = as.numeric(),
                     Y_Coord = as.numeric(),
                     Z_Coord = as.numeric())
    j=6
    for (j in 6:as.numeric(ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
      DF[j,1] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),2]
      DF[j,2] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),3]
      DF[j,3] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i,j]+1),4]
    }
    DF1 <- as.matrix(na.omit(DF))
    DF2 <- as.matrix(DF1 + 1)
    DF3 <- as.matrix(merge(DF1, DF2, all=TRUE))
    if(as.numeric(nrow(DF1)) < 3) {
      area[i,] <- 0
    } else {
      alphashape.obj <- ashape3d(DF3,pert = TRUE, alpha = 10)
      area[i,] <- round(volume_ashape3d(alphashape.obj) / 1, 3)
    }
  }
  cbind(area, 
        get(paste(colnames(Segments)[x], "fiber",  sep = "_")))
}

## Relative postion of points between kinetochore and the pole1
relativ_pos_1 <- function(x){
  relativ_pos_part1 <- lapply(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[4], 
                              function(y){get(paste(colnames(Segments)[x], "fiber", sep = "_"))[4] - Pole1[1,2]})
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y_Coord"]][["Y_Coord"]])
  relativ_pos_part2 <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[which.min(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1,4]),4] - Pole1[1,2]
  relativ_positon <- lapply(relativ_pos_part1, function(y){round(relativ_pos_part1[1] / relativ_pos_part2, 2)})
  relat_pos = data.frame(Relative_Position = relativ_positon[["relativ_pos_part1...Y_Coord......Y_Coord..."]])
  cbind(relat_pos,
        get(paste(colnames(Segments)[x], "fiber", sep = "_")))
}

## Relative postion of points between kinetochore and the pole2
relativ_pos_2 <- function(x){
  relativ_pos_part1 <- lapply(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[4], 
                              function(y){get(paste(colnames(Segments)[x], "fiber", sep = "_"))[4] - Pole2[1,2]})
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y_Coord"]][["Y_Coord"]])
  relativ_pos_part2 <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[which.max(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1,4]),4] - Pole2[1,2]
  relativ_positon <- lapply(relativ_pos_part1, function(y){round(relativ_pos_part1[1] / relativ_pos_part2, 2)})
  relat_pos = data.frame(Relative_Position = relativ_positon[["relativ_pos_part1...Y_Coord......Y_Coord..."]])
  cbind(relat_pos, 
        get(paste(colnames(Segments)[x], "fiber", sep = "_")))
}

##KMTs denisty
Density_of_KMTs <- function(x){
  
}

library(tcltk)
total <- as.numeric(ncol(Segments) - 4)
pb <- winProgressBar(title = "Progress",
                     min = 0,
                     max =  total,
                     width = 300)

for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(ncol(Segments) - 4)) {
  ##find individual fiber
  assign(colnames(Segments)[i], 
         Sort_by_fiber(colnames(Segments)[i]))
  ##select individual KMTs from fiber naming it POleX_YY_ZZ
  ##X  - pole 1 or 3
  ##YY - number of fiber
  ##ZZ - number of KMTs in the fiber
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"), 
           Select_Points(j, get(colnames(Segments)[i])))
    j = j + 1
  }
  
  j = 1
  while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
    assign(paste(colnames(Segments)[i], j, sep = "_"),
           Find_XYZ(get(paste(colnames(Segments)[i], j,sep = "_"))))
    j = j + 1
  }
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    label = paste(round(i / total * 100, 0), "% Done"))
}
close(pb)

##Sort points in the individual fiber, make 1 point in df corespond to (+) end
for(i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  j = 1
  tryCatch({
    while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
      assign(paste(colnames(Segments)[i], j, sep = "_"),
             Sort_by_distance_to_pole1(get(paste(colnames(Segments)[i], j, sep = "_"))))
      j = j + 1
    }
  },
  error = function(e){})
}
for(i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments) - 4)){
  j = 1
  tryCatch({
    while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
      assign(paste(colnames(Segments)[i], j, sep = "_"),
             Sort_by_distance_to_pole2(get(paste(colnames(Segments)[i], j, sep = "_"))))
      j = j + 1
    }
  },
  error = function(e){}
  )
}

##find leading KMTs in the fiber ..... 1->49
for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
  assign(paste(colnames(Segments)[i]), 
         leading_KMTsv2(i, Pole1))
}
for (i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments) - 4)) {
  assign(paste(colnames(Segments)[i]), 
         leading_KMTsv2(i, Pole2))
}

pb <- winProgressBar(title = "Progress",
                     min = 0,
                     max =  total,
                     width = 300)

for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(ncol(Segments) - 4)) {
  tryCatch({
    assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
           Leadig_Points(i))
    
    ##find points which correspond to the leading fier
    assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
           find_polygon(i))
    
    ##Remove all duplicates
    assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
           duplicated_points(i))
    
    ##for each position find medan point
    assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
           median_point(i))
  },
  error = function(e){}
  )
  
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, 
                    label = paste(round(i / total * 100, 0), "% Done"))
}
close(pb)

## Calculate circular area
for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(ncol(Segments) - 4)) {
  tryCatch({
    assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
           circular_area(i))   
  },
  error = function(e){}
  )
}

## Calculate polygone area
for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(ncol(Segments) - 4)) {
  tryCatch({
    assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
           polygon_area(i))
  },
  error = function(e){}
  )
}

## Ralative position for Pole1
for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
  tryCatch({
    assign(paste(colnames(Segments)[i], "fiber", sep = "_"), 
           relativ_pos_1(i))
    },
    error = function(e){}
    )
  
}

## Ralative position for Pole2
for (i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments) - 4)) {
  tryCatch({
    assign(paste(colnames(Segments)[i], "fiber", sep = "_"),
           relativ_pos_2(i))
  },
  error = function(e){}
  )
}

##Marge data into one file
##External funciton cbind.na by http://www.dr-spiess.de/Rscripts.html
cbind.na <- function (..., deparse.level = 1) 
{
  na <- nargs() - (!missing(deparse.level))    
  deparse.level <- as.integer(deparse.level)
  stopifnot(0 <= deparse.level, deparse.level <= 2)
  argl <- list(...)   
  while (na > 0 && is.null(argl[[na]])) {
    argl <- argl[-na]
    na <- na - 1
  }
  if (na == 0) 
    return(NULL)
  if (na == 1) {         
    if (isS4(..1)) 
      return(cbind2(..1))
    else return(matrix(...))  ##.Internal(cbind(deparse.level, ...)))
  }
  if (deparse.level) {       
    symarg <- as.list(sys.call()[-1L])[1L:na]
    Nms <- function(i) {
      if (is.null(r <- names(symarg[i])) || r == "") {
        if (is.symbol(r <- symarg[[i]]) || deparse.level == 
            2) 
          deparse(r)
      }
      else r
    }
  }   
  ## deactivated, otherwise no fill in with two arguments
  if (na == 0) {
    r <- argl[[2]]
    fix.na <- FALSE
  }
  else {
    nrs <- unname(lapply(argl, nrow))
    iV <- sapply(nrs, is.null)
    fix.na <- identical(nrs[(na - 1):na], list(NULL, NULL))
    ## deactivated, otherwise data will be recycled
    #if (fix.na) {
    #    nr <- max(if (all(iV)) sapply(argl, length) else unlist(nrs[!iV]))
    #    argl[[na]] <- cbind(rep(argl[[na]], length.out = nr), 
    #        deparse.level = 0)
    #}       
    if (deparse.level) {
      if (fix.na) 
        fix.na <- !is.null(Nna <- Nms(na))
      if (!is.null(nmi <- names(argl))) 
        iV <- iV & (nmi == "")
      ii <- if (fix.na) 
        2:(na - 1)
      else 2:na
      if (any(iV[ii])) {
        for (i in ii[iV[ii]]) if (!is.null(nmi <- Nms(i))) 
          names(argl)[i] <- nmi
      }
    }
    
    ## filling with NA's to maximum occuring nrows
    nRow <- as.numeric(sapply(argl, function(x) NROW(x)))
    maxRow <- max(nRow, na.rm = TRUE)  
    argl <- lapply(argl, function(x)  if (is.null(nrow(x))) c(x, rep(NA, maxRow - length(x)))
                   else rbind.na(x, matrix(, maxRow - nrow(x), ncol(x))))
    r <- do.call(cbind, c(argl[-1L], list(deparse.level = deparse.level)))
  }
  d2 <- dim(r)
  r <- cbind2(argl[[1]], r)
  if (deparse.level == 0) 
    return(r)
  ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2L
  ism2 <- !is.null(d2) && length(d2) == 2L && !fix.na
  if (ism1 && ism2) 
    return(r)
  Ncol <- function(x) {
    d <- dim(x)
    if (length(d) == 2L) 
      d[2L]
    else as.integer(length(x) > 0L)
  }
  nn1 <- !is.null(N1 <- if ((l1 <- Ncol(..1)) && !ism1) Nms(1))
  nn2 <- !is.null(N2 <- if (na == 2 && Ncol(..2) && !ism2) Nms(2))
  if (nn1 || nn2 || fix.na) {
    if (is.null(colnames(r))) 
      colnames(r) <- rep.int("", ncol(r))
    setN <- function(i, nams) colnames(r)[i] <<- if (is.null(nams)) 
      ""
    else nams
    if (nn1) 
      setN(1, N1)
    if (nn2) 
      setN(1 + l1, N2)
    if (fix.na) 
      setN(ncol(r), Nna)
  }
  r
}
rbind.na <- function (..., deparse.level = 1) 
{
  na <- nargs() - (!missing(deparse.level))
  deparse.level <- as.integer(deparse.level)
  stopifnot(0 <= deparse.level, deparse.level <= 2)
  argl <- list(...)
  while (na > 0 && is.null(argl[[na]])) {
    argl <- argl[-na]
    na <- na - 1
  }    
  if (na == 0) 
    return(NULL)
  if (na == 1) {
    if (isS4(..1)) 
      return(rbind2(..1))
    else return(matrix(..., nrow = 1)) ##.Internal(rbind(deparse.level, ...)))
  }
  if (deparse.level) {
    symarg <- as.list(sys.call()[-1L])[1L:na]
    Nms <- function(i) {
      if (is.null(r <- names(symarg[i])) || r == "") {
        if (is.symbol(r <- symarg[[i]]) || deparse.level == 
            2) 
          deparse(r)
      }
      else r
    }
  }
  
  ## deactivated, otherwise no fill in with two arguments
  if (na == 0) {
    r <- argl[[2]]
    fix.na <- FALSE
  }
  else {
    nrs <- unname(lapply(argl, ncol))
    iV <- sapply(nrs, is.null)
    fix.na <- identical(nrs[(na - 1):na], list(NULL, NULL))
    ## deactivated, otherwise data will be recycled
    #if (fix.na) {
    #    nr <- max(if (all(iV)) sapply(argl, length) else unlist(nrs[!iV]))
    #    argl[[na]] <- rbind(rep(argl[[na]], length.out = nr), 
    #        deparse.level = 0)
    #}
    if (deparse.level) {
      if (fix.na) 
        fix.na <- !is.null(Nna <- Nms(na))
      if (!is.null(nmi <- names(argl))) 
        iV <- iV & (nmi == "")
      ii <- if (fix.na) 
        2:(na - 1)
      else 2:na
      if (any(iV[ii])) {
        for (i in ii[iV[ii]]) if (!is.null(nmi <- Nms(i))) 
          names(argl)[i] <- nmi
      }
    }
    
    ## filling with NA's to maximum occuring ncols
    nCol <- as.numeric(sapply(argl, function(x) if (is.null(ncol(x))) length(x)
                              else ncol(x)))
    maxCol <- max(nCol, na.rm = TRUE)  
    argl <- lapply(argl, function(x)  if (is.null(ncol(x))) c(x, rep(NA, maxCol - length(x)))
                   else cbind(x, matrix(, nrow(x), maxCol - ncol(x))))  
    
    ## create a common name vector from the
    ## column names of all 'argl' items
    namesVEC <- rep(NA, maxCol)  
    for (i in 1:length(argl)) {
      CN <- colnames(argl[[i]])          
      m <- !(CN %in% namesVEC)
      namesVEC[m] <- CN[m]          
    }  
    
    ## make all column names from common 'namesVEC'
    for (j in 1:length(argl)) {    
      if (!is.null(ncol(argl[[j]]))) colnames(argl[[j]]) <- namesVEC
    }
    
    r <- do.call(rbind, c(argl[-1L], list(deparse.level = deparse.level)))        
  }
  
  d2 <- dim(r)
  
  ## make all column names from common 'namesVEC'
  colnames(r) <- colnames(argl[[1]])
  
  r <- rbind2(argl[[1]], r)
  
  if (deparse.level == 0) 
    return(r)
  ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2L
  ism2 <- !is.null(d2) && length(d2) == 2L && !fix.na
  if (ism1 && ism2) 
    return(r)
  Nrow <- function(x) {
    d <- dim(x)
    if (length(d) == 2L) 
      d[1L]
    else as.integer(length(x) > 0L)
  }
  nn1 <- !is.null(N1 <- if ((l1 <- Nrow(..1)) && !ism1) Nms(1))
  nn2 <- !is.null(N2 <- if (na == 2 && Nrow(..2) && !ism2) Nms(2))
  if (nn1 || nn2 || fix.na) {
    if (is.null(rownames(r))) 
      rownames(r) <- rep.int("", nrow(r))
    setN <- function(i, nams) rownames(r)[i] <<- if (is.null(nams)) 
      ""
    else nams
    if (nn1) 
      setN(1, N1)
    if (nn2) 
      setN(1 + l1, N2)
    if (fix.na) 
      setN(nrow(r), Nna)
  }
  r
}

Data <- Pole1_00_fiber[1:3]
for (i in as.numeric(which(colnames(Segments) == "Pole1_00")+1):as.numeric(ncol(Segments) - 4)){
  tryCatch({
    Data <- rbind(Data,
                   get(paste(colnames(Segments)[i], "fiber", sep = "_"))[1:3])
  },
  error = function(e){}
  )
}

names(Data)[1] <- "Relativ_pos" 
library(xlsx)
write.xlsx(Data, "Data#1.xlsx")

##Spread data for bins_circular
Data_full_1.0 <- data.frame(To_1.0_RP = Data[with(Data, Relativ_pos <= 1.0 & Relativ_pos > 0.899),][,1],
                             To_1.0_Area = Data[with(Data, Relativ_pos <= 1.0 & Relativ_pos > 0.899),][,3])
Data_full_0.9 <- data.frame(To_0.9_c = Data[with(Data, Relativ_pos < 0.9 & Relativ_pos > 0.799),][,1],
                             To_0.9_p = Data[with(Data, Relativ_pos < 0.9 & Relativ_pos > 0.799),][,3])
Data_full_0.8 <- data.frame(To_0.8_c = Data[with(Data, Relativ_pos < 0.8 & Relativ_pos > 0.699),][,1],
                             To_0.8_p = Data[with(Data, Relativ_pos < 0.8 & Relativ_pos > 0.699),][,3])
Data_full_0.7 <- data.frame(To_0.7_c = Data[with(Data, Relativ_pos < 0.7 & Relativ_pos > 0.599),][,1],
                             To_0.7_p = Data[with(Data, Relativ_pos < 0.7 & Relativ_pos > 0.599),][,3])
Data_full_0.6 <- data.frame(To_0.6_c = Data[with(Data, Relativ_pos < 0.6 & Relativ_pos > 0.499),][,1],
                             To_0.6_p = Data[with(Data, Relativ_pos < 0.6 & Relativ_pos > 0.499),][,3])
Data_full_0.5 <- data.frame(To_0.5_c = Data[with(Data, Relativ_pos < 0.5 & Relativ_pos > 0.399),][,1],
                             To_0.5_p = Data[with(Data, Relativ_pos < 0.5 & Relativ_pos > 0.399),][,3])
Data_full_0.4 <- data.frame(To_0.4_c = Data[with(Data, Relativ_pos < 0.4 & Relativ_pos > 0.299),][,1],
                             To_0.4_p = Data[with(Data, Relativ_pos < 0.4 & Relativ_pos > 0.299),][,3])
Data_full_0.3 <- data.frame(To_0.3_c = Data[with(Data, Relativ_pos < 0.3 & Relativ_pos > 0.199),][,1],
                             To_0.3_p = Data[with(Data, Relativ_pos < 0.3 & Relativ_pos > 0.199),][,3])
Data_full_0.2 <- data.frame(To_0.2_c = Data[with(Data, Relativ_pos < 0.2 & Relativ_pos > 0.099),][,1],
                             To_0.2_p = Data[with(Data, Relativ_pos < 0.2 & Relativ_pos > 0.099),][,3])
Data_full_0.1 <- data.frame(To_0.1_c = Data[with(Data, Relativ_pos < 0.1 & Relativ_pos > 0.000),][,1],
                             To_0.1_p = Data[with(Data, Relativ_pos < 0.1 & Relativ_pos > 0.000),][,3])
Data_full_0.0 <- data.frame(To_0.0_c = Data[with(Data, Relativ_pos < 0.0 & Relativ_pos > -0.101),][,1],
                             To_0.0_p = Data[with(Data, Relativ_pos < 0.0 & Relativ_pos > -0.101),][,3])
Data_full_m0.1 <- data.frame(To_m0.1_c = Data[with(Data, Relativ_pos < -0.1 & Relativ_pos > -0.201),][,1],
                              To_m0.1_p = Data[with(Data, Relativ_pos < -0.1 & Relativ_pos > -0.201),][,3])
Data_full_m0.2 <- data.frame(To_m0.2_c = Data[with(Data, Relativ_pos < -0.2 & Relativ_pos > -0.301),][,1],
                              To_m0.2_p = Data[with(Data, Relativ_pos < -0.2 & Relativ_pos > -0.301),][,3])
Data_full_m0.3 <- data.frame(To_m0.3_c = Data[with(Data, Relativ_pos < -0.3),][,1],
                              To_m0.3_p = Data[with(Data, Relativ_pos < -0.3),][,3])

Data_circular_bins <- cbind.na(Data_full_1.0,
                               Data_full_0.9,
                               Data_full_0.8,
                               Data_full_0.7,
                               Data_full_0.6,
                               Data_full_0.5,
                               Data_full_0.4,
                               Data_full_0.3,
                               Data_full_0.2,
                               Data_full_0.1,
                               Data_full_0.0,
                               Data_full_m0.1,
                               Data_full_m0.2,
                               Data_full_m0.3)

##Spread data for bins_alpha
Data_full_1.0 <- data.frame(To_1.0_c = Data[with(Data, Relativ_pos <= 1.0 & Relativ_pos > 0.899),][,1],
                            To_1.0_p = Data[with(Data, Relativ_pos <= 1.0 & Relativ_pos > 0.899),][,2])
Data_full_0.9 <- data.frame(To_0.9_c = Data[with(Data, Relativ_pos < 0.9 & Relativ_pos > 0.799),][,1],
                            To_0.9_p = Data[with(Data, Relativ_pos < 0.9 & Relativ_pos > 0.799),][,2])
Data_full_0.8 <- data.frame(To_0.8_c = Data[with(Data, Relativ_pos < 0.8 & Relativ_pos > 0.699),][,1],
                            To_0.8_p = Data[with(Data, Relativ_pos < 0.8 & Relativ_pos > 0.699),][,2])
Data_full_0.7 <- data.frame(To_0.7_c = Data[with(Data, Relativ_pos < 0.7 & Relativ_pos > 0.599),][,1],
                            To_0.7_p = Data[with(Data, Relativ_pos < 0.7 & Relativ_pos > 0.599),][,2])
Data_full_0.6 <- data.frame(To_0.6_c = Data[with(Data, Relativ_pos < 0.6 & Relativ_pos > 0.499),][,1],
                            To_0.6_p = Data[with(Data, Relativ_pos < 0.6 & Relativ_pos > 0.499),][,2])
Data_full_0.5 <- data.frame(To_0.5_c = Data[with(Data, Relativ_pos < 0.5 & Relativ_pos > 0.399),][,1],
                            To_0.5_p = Data[with(Data, Relativ_pos < 0.5 & Relativ_pos > 0.399),][,2])
Data_full_0.4 <- data.frame(To_0.4_c = Data[with(Data, Relativ_pos < 0.4 & Relativ_pos > 0.299),][,1],
                            To_0.4_p = Data[with(Data, Relativ_pos < 0.4 & Relativ_pos > 0.299),][,2])
Data_full_0.3 <- data.frame(To_0.3_c = Data[with(Data, Relativ_pos < 0.3 & Relativ_pos > 0.199),][,1],
                            To_0.3_p = Data[with(Data, Relativ_pos < 0.3 & Relativ_pos > 0.199),][,2])
Data_full_0.2 <- data.frame(To_0.2_c = Data[with(Data, Relativ_pos < 0.2 & Relativ_pos > 0.099),][,1],
                            To_0.2_p = Data[with(Data, Relativ_pos < 0.2 & Relativ_pos > 0.099),][,2])
Data_full_0.1 <- data.frame(To_0.1_c = Data[with(Data, Relativ_pos < 0.1 & Relativ_pos > 0.000),][,1],
                            To_0.1_p = Data[with(Data, Relativ_pos < 0.1 & Relativ_pos > 0.000),][,2])
Data_full_0.0 <- data.frame(To_0.0_c = Data[with(Data, Relativ_pos < 0.0 & Relativ_pos > -0.101),][,1],
                            To_0.0_p = Data[with(Data, Relativ_pos < 0.0 & Relativ_pos > -0.101),][,2])
Data_full_m0.1 <- data.frame(To_m0.1_c = Data[with(Data, Relativ_pos < -0.1 & Relativ_pos > -0.201),][,1],
                             To_m0.1_p = Data[with(Data, Relativ_pos < -0.1 & Relativ_pos > -0.201),][,2])
Data_full_m0.2 <- data.frame(To_m0.2_c = Data[with(Data, Relativ_pos < -0.2 & Relativ_pos > -0.301),][,1],
                             To_m0.2_p = Data[with(Data, Relativ_pos < -0.2 & Relativ_pos > -0.301),][,2])
Data_full_m0.3 <- data.frame(To_m0.3_c = Data[with(Data, Relativ_pos < -0.3),][,1],
                             To_m0.3_p = Data[with(Data, Relativ_pos < -0.3),][,2])

Data_alpha_bins <- cbind.na(Data_full_1.0,
                               Data_full_0.9,
                               Data_full_0.8,
                               Data_full_0.7,
                               Data_full_0.6,
                               Data_full_0.5,
                               Data_full_0.4,
                               Data_full_0.3,
                               Data_full_0.2,
                               Data_full_0.1,
                               Data_full_0.0,
                               Data_full_m0.1,
                               Data_full_m0.2,
                               Data_full_m0.3)
write.xlsx(Data_alpha_bins, "Data#1_alpha.xlsx")
write.xlsx(Data_circular_bins, "Data#1_circular.xlsx")
