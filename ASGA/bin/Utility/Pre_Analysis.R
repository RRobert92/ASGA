################################################################################
# Module Pre_Analysis
#
# Author: Robert Kiewisz
# Created: 2020-05-17 
################################################################################


PreAnalysis <- function (input, output, session){
 
# Set-up analysis --------------------------------------------------------------
  Segments <<- Data_Segments_1
  Points <<- Data_Points_1
  Nodes <<- Data_Nodes_1
  Pole1 <<- "Pole1"
  Pole2 <<- "Pole2"
  Minus_Threshold <<- 1.2
  minus_distance <<- 0.055
  
  # Load Segments --------------------------------------------------------------
  ncolumn <<- ncol(Segments)
  Segments_1_KMT <<- Segments %>% filter_at(vars(starts_with(Pole1)),
                                           any_vars(.>= 1))
  Segments_1_KMT <<- Segments_1_KMT %>% select("Segment ID",
                                              "length",
                                              "Node ID #1",
                                              "Node ID #2",
                                              "Point IDs")
  
  Segments_2_KMT <<- Segments %>% filter_at(vars(starts_with(Pole2)),
                                           any_vars(.>= 1))
  Segments_2_KMT <<- Segments_2_KMT %>% select("Segment ID",
                                              "length",
                                              "Node ID #1",
                                              "Node ID #2",
                                              "Point IDs")
  
  Segments_KMT <<- Segments %>% filter_at(vars(starts_with("Pole")),
                                         any_vars(.>=1))
  Segments_KMT <<- Segments_KMT %>% select("Segment ID",
                                          "length",
                                          "Node ID #1",
                                          "Node ID #2",
                                          "Point IDs")
  
  Segments_SMT <<- Segments %>% filter_at(vars(starts_with("Pole")),
                                         all_vars(.< 1))
  Segments_SMT <<- Segments_SMT %>% select("Segment ID",
                                          "length",
                                          "Node ID #1",
                                          "Node ID #2",
                                          "Point IDs")
  
  # Load Poles -----------------------------------------------------------------
  Pole1 <<- Nodes %>% filter_at(vars(Pole1),
                               any_vars(.>=1))
  Pole1 <<- data.frame(X = c(Pole1 %>% select("X Coord")/10000),
                      Y = c(Pole1 %>% select("Y Coord")/10000),
                      Z = c(Pole1 %>% select("Z Coord")/10000))
  Pole2 <<- Nodes %>% filter_at(vars(Pole2),
                               any_vars(.>=1))
  Pole2 <<- data.frame(X = c(Pole2 %>% select("X Coord")/10000),
                      Y = c(Pole2 %>% select("Y Coord")/10000),
                      Z = c(Pole2 %>% select("Z Coord")/10000))
  
  # Load Nodes -----------------------------------------------------------------
  if(ncol(Nodes %>% select(starts_with("EndType"))) == 1){
    Nodes <<- Nodes %>% select("Node ID", 
                              "X Coord",
                              "Y Coord",
                              "Z Coord",
                              starts_with("EndType"))
    
  } else if (ncol(Nodes %>% select(starts_with("EndType"))) == 2){
    Nodes <<- Nodes %>% select("Node ID", 
                              "X Coord",
                              "Y Coord",
                              "Z Coord",
                              starts_with("EndType"))
    
    compare <<- data.frame()
    
    for(i in 1:nrow(Nodes %>% select(starts_with("EndType")))){
      compare[i,1] <- Nodes[i,5] == Nodes[i,6]
    }
    Nodes <<- cbind(Nodes,
                   compare)
    names(Nodes)[7] <- "Entype_Different"
    rm(compare)
    
  } else {
    Nodes <<- Nodes %>% select("Node ID", 
                              "X Coord",
                              "Y Coord",
                              "Z Coord")
  }
  
  Nodes[2:4] <<- Nodes[2:4]/10000
  
  # Load Points ----------------------------------------------------------------
  Points <<- Points %>% select("Point ID", 
                              "X Coord",
                              "Y Coord",
                              "Z Coord")
  
  Points[2:4] <<- Points[2:4]/10000
  names(Points)[1] <<- "Point_ID"
  
# Sort single KMT --------------------------------------------------------------
  progressSweetAlert(
    session = session, id = "SingleKMT",
    title = "Sorting data for single KMT",
    display_pct = TRUE, value = 0
  )
  
  total <- as.numeric(ncol(Segments %>% select(starts_with("Pole"))))
  
  for (i in which(colnames(Segments) == "Pole1_00") : which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))])) {
    assign(paste(colnames(Segments)[i]),
           Sort_by_fiber(paste(colnames(Segments)[i])),
           envir=.GlobalEnv)
    
    j = 1
    while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
      assign(paste(colnames(Segments)[i], 
                   j, 
                   sep = "_"),
             Select_Points(j, 
                           get(colnames(Segments)[i])),
             envir=.GlobalEnv)
      j = j + 1
    }
    
    j = 1
    while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
      assign(paste(colnames(Segments)[i], 
                   j, 
                   sep = "_"),
             Find_XYZ(get(paste(
               colnames(Segments)[i], 
               j, 
               sep = "_"))),
             envir=.GlobalEnv)
      j = j + 1
    }
    
    updateProgressBar(
      session = session,
      id = "SingleKMT",
      value = i/total * 100
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)

# Sort Points in KMT at a Pole1 ------------------------------------------------
  progressSweetAlert(
    session = session, id = "SinglePoints1",
    title = "Sorting points based on (+) and (-) ends for the Pole_1...",
    display_pct = TRUE, value = 0
  )
  total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)

  for(i in which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
    tryCatch({
      j = 1
      while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
        assign(paste(colnames(Segments)[i], 
                     j, 
                     sep = "_"),
               Sort_by_distance_to_pole1(get(paste(colnames(Segments)[i], 
                                                   j, 
                                                   sep = "_"))),
               envir=.GlobalEnv)
        
        j = j + 1
      }
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "SinglePoints1",
      value = round((i - 1) / total * 100, 
                    0)
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)

# Sort Points in KMT at a Pole2 ------------------------------------------------
  progressSweetAlert(
    session = session, id = "SinglePoints2",
    title = "Sorting points based on (+) and (-) ends for the Pole_2...",
    display_pct = TRUE, value = 0)
  
  total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
    as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
  
  for(i in as.numeric(which(colnames(Segments) == "Pole2_00")) : as.numeric(ncol(Segments) - 4)){
    j = 1
    tryCatch({
      while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
        assign(paste(colnames(Segments)[i], 
                     j, 
                     sep = "_"),
               Sort_by_distance_to_pole2(get(paste(colnames(Segments)[i], 
                                                   j, 
                                                   sep = "_"))),
               envir=.GlobalEnv)
        
        j = j + 1
        
      }
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "SinglePoints2",
      value =  round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100, 
                     0)
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)

# Get ID for the ellipse Rx and Rz for 100%, 50% and 25% -----------------------
  Plus_end <<- data.frame()
  Kinetochore_Avg <<- data.frame()

  assign("Kinetochore_projected",
         Kinetochore_position(),
         envir = .GlobalEnv)
  
  for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(ncol(Segments) - 4)){
    j = 1
    tryCatch({
      while (j <= as.numeric(nrow(get(paste(colnames(Segments)[i]))))) {
        Plus_end[j,1:3] <<- get(paste(colnames(Segments)[i], j, sep = "_"))[1,2:4]
        j = j + 1
      }
      Plus_end <<- data.frame(X_Median = c(median(as.matrix(Plus_end[1]))),
                             Y_Median = c(median(as.matrix(Plus_end[2]))),
                             Z_Median = c(median(as.matrix(Plus_end[3]))))
      Kinetochore_Avg[i,1:3] <<- Plus_end
    },
    error = function(e){
      Kinetochore_Avg[i,1:3] <<- NA
    })
  }
  
  Kinetochore_Avg <<- na.omit(Kinetochore_Avg)
  Pole_avg <<- rbind(Pole1, Pole2)
  Pole_avg <<- data.frame(X_Mean = c(mean(as.matrix(Pole_avg[1]))),
                         Y_Mean = c(mean(as.matrix(Pole_avg[2]))),
                         Z_Mean = c(mean(as.matrix(Pole_avg[3]))))
  Rx100 <<- data.frame()
  Rx100[1,1] <<- max(Kinetochore_Avg$X_Median)
  Rx100[1,1] <<- abs(Rx100[1,1] - Pole_avg$X_Mean)
  Rx100[1,2] <<- min(Kinetochore_Avg$X_Median)
  Rx100[1,2] <<- abs(Rx100[1,2] - Pole_avg$X_Mean)
  
  Rx100 <<- max(Rx100)
  Rx50 <<- Rx100*0.80
  Rx25 <<- Rx100*0.45
  
  Rz100 <<- data.frame()
  Rz100[1,1] <<- max(Kinetochore_Avg$Z_Median)
  Rz100[1,1] <<- abs(Rz100[1,1] - Pole_avg$Z_Mean)
  Rz100[1,2] <<- min(Kinetochore_Avg$Z_Median)
  Rz100[1,2] <<- abs(Rz100[1,2] - Pole_avg$Z_Mean)
  
  Rz100 <<- max(Rz100)
  Rz50 <<- Rz100*0.80
  Rz25 <<- Rz100*0.45
 
# Analyze Length Distribution for Pole1 --------------------------------------------------
  progressSweetAlert(
    session = session, id = "AnalyseLD1",
    title = "Calcualting legnth and ends positions for Pole_1",
    display_pct = TRUE, value = 0)
  
  total <- as.numeric(length(which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00"))) - 1)
  
  for(i in which(colnames(Segments) == "Pole1_00") : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
    tryCatch({
      assign(paste(colnames(Segments)[i]),
             Analyse_LD(i, 
                        Pole1),
             envir=.GlobalEnv)
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "AnalyseLD1",
      value = round((i - 1) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)
  
# Analyze Length Distribution for Pole2 --------------------------------------------------
  progressSweetAlert(
    session = session, id = "AnalyseLD2",
    title = "Calcualting legnth and ends positions for Pole_2",
    display_pct = TRUE, value = 0)
  
  total <- which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))]) - 
    as.numeric(which(colnames(Segments) == "Pole2_00") - 1)
  
  for(i in as.numeric(which(colnames(Segments) == "Pole2_00")) : as.numeric(ncol(Segments) - 4)){
    j = 1
    tryCatch({
      assign(paste(colnames(Segments)[i]),
             Analyse_LD(i, 
                        Pole2),
             envir=.GlobalEnv)
    },
    error = function(e){})
    
    updateProgressBar(
      session = session,
      id = "AnalyseLD2",
      value = round((i - as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) / total * 100,
                    0)
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)
  
# Collect Length Distribution Data for Pole1 --------------------------------------------------
  LD_P1 <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["length"]
  Plus_end <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["plus_dist_to_kinetochore_core"]
  Dist_pole <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["plus_dist_to_pole"]
  Elips <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["Elipse_Position"]
  Minus_dist <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["minus_dist_to_pole"]
  LD_P1 <<- cbind(LD_P1,
                 Plus_end,
                 Dist_pole,
                 Elips,
                 Minus_dist)
  
  for (i in as.numeric(which(colnames(Segments) == "Pole1_00")+1) : as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
    tryCatch({
      DF_LD <<- get(paste(colnames(Segments)[i]))["length"]
      DF_Plus_end <<- get(paste(colnames(Segments)[i]))["plus_dist_to_kinetochore_core"]
      DF_Dist_pole <<- get(paste(colnames(Segments)[i]))["plus_dist_to_pole"]
      DF_Elips <<- get(paste(colnames(Segments)[i]))["Elipse_Position"]
      Minus_dist <<- get(paste(colnames(Segments)[i]))["minus_dist_to_pole"]
      DF <<- cbind(DF_LD,
                  DF_Plus_end,
                  DF_Dist_pole,
                  DF_Elips,
                  Minus_dist)
      
      LD_P1 <<- rbind(LD_P1, 
                     DF)
    },
    error = function(e){})
    
  }
  
# Collect Length Distribution Data for Pole1 --------------------------------------------------
  tryCatch({
    LD_P2 <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["length"]
    Minus_dist <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["minus_dist_to_pole"]
    Plus_end <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["plus_dist_to_kinetochore_core"]
    Dist_pole <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["plus_dist_to_pole"]
    Elips <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["Elipse_Position"]
    LD_P2 <<- cbind(LD_P2,
                   Plus_end,
                   Dist_pole,
                   Elips,
                   Minus_dist)
  }, error = function(e){})
  
  
  for (i in as.numeric(which(colnames(Segments) == "Pole2_00")+1) : as.numeric(ncol(Segments) - 4)){
    tryCatch({
      DF_LD <<- get(paste(colnames(Segments)[i]))["length"]
      Minus_dist <<- get(paste(colnames(Segments)[i]))["minus_dist_to_pole"]
      DF_Plus_end <<- get(paste(colnames(Segments)[i]))["plus_dist_to_kinetochore_core"]
      DF_Dist_pole <<- get(paste(colnames(Segments)[i]))["plus_dist_to_pole"]
      DF_Elips <- get(paste(colnames(Segments)[i]))["Elipse_Position"]
      DF <<- cbind(DF_LD,
                  DF_Plus_end,
                  DF_Dist_pole,
                  DF_Elips,
                  Minus_dist)
      
      LD_P2 <<- rbind(LD_P2, 
                     DF)
    },
    error = function(e){})
  }
}