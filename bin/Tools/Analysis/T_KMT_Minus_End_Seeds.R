##################################################################################
# Function to calculate distribution of a (-) end in close proximity to the KMT #
##################################################################################
# The tool is calculating distance of each point on KMT (p) to every (-) end of SMT and KMT (P_S or P_K)
# Then we associate point_id to segment_id of P_S or P_K which are closer to the p then 100nm 
# Multiple same segment_ID are marred to single entrance with their closes distance to the p
# The final output is data file for each KMT with: 
# KMT_ID, 
# Interactor_ID, 
# class of interactor, 
# p to P distance
# Relative position,
# Plus end distance to the pole,
# Ellipse position

Minus_end_seed <- function(x){
  Minus_end <- data.frame()
  for(i in 1:nrow(get(paste(colnames(Segments)[x])))){
    DF <- data.frame()
    Minus_seed <- data.frame()
    for(j in 1:nrow(get(paste(colnames(Segments)[x], i, sep = "_")))){
      p_to_P <- Nodes
      p_to_P[5:7] <- get(paste(colnames(Segments)[x],i, sep = "_"))[j,2:4] 
      
      p_to_P$dist <- apply(p_to_P[2:7], 
                           1, 
                           function(y) dist(matrix(y, 
                                                   nrow = 2, 
                                                   byrow = TRUE)))
      DF <- data.frame(p_to_P[with(p_to_P, dist <= minus_distance & dist >= 0),"Node ID"],
                       p_to_P[with(p_to_P, dist <= minus_distance & dist >= 0), "dist"])
      if(nrow(DF) > 0){
        all_end <- data.frame()
        defin_end <- data.frame()
        
        for (k in 1:nrow(DF)){
          all_end <- Segments %>% filter_at(vars(starts_with("Node")),
                                            any_vars(. == DF[k,1]))
          defin_end[k,1:3] <- all_end %>% select(`Segment ID`,
                                                 `Node ID #1`,
                                                 `Node ID #2`)
          
          if(defin_end[1,1] %in% Segments_SMT$`Segment ID`){
            defin_end[k,4] <-"SMT"
          } else {
            defin_end[k,4] <-"KMT"
          }
          
          if(defin_end[k,1] == get(paste(colnames(Segments)[x]))[i,1]){
            defin_end[k,1:4] <- NA
            DF[k,1:2] <- NA
          }
        }
        defin_end <- na.omit(defin_end)
        DF <- na.omit(DF)
        
        if(nrow(DF) > 0){
          end_type <- data.frame()
          for(k in 1:nrow(defin_end)){
            N1_to_pole1 <- sqrt((Pole1[1,1] - Nodes[as.numeric(defin_end[k,2]+1),2])^2 + 
                                (Pole1[1,2] - Nodes[as.numeric(defin_end[k,2]+1),3])^2 + 
                                (Pole1[1,3] - Nodes[as.numeric(defin_end[k,2]+1),4])^2)
            N1_to_pole2 <- sqrt((Pole2[1,1] - Nodes[as.numeric(defin_end[k,2]+1),2])^2 + 
                                (Pole2[1,2] - Nodes[as.numeric(defin_end[k,2]+1),3])^2 + 
                                (Pole2[1,3] - Nodes[as.numeric(defin_end[k,2]+1),4])^2)
            N2_to_pole1 <- sqrt((Pole1[1,1] - Nodes[as.numeric(defin_end[k,3]+1),2])^2 + 
                                (Pole1[1,2] - Nodes[as.numeric(defin_end[k,3]+1),3])^2 + 
                                (Pole1[1,3] - Nodes[as.numeric(defin_end[k,3]+1),4])^2)
            N2_to_pole2 <- sqrt((Pole2[1,1] - Nodes[as.numeric(defin_end[k,3]+1),2])^2 + 
                                (Pole2[1,2] - Nodes[as.numeric(defin_end[k,3]+1),3])^2 + 
                                (Pole2[1,3] - Nodes[as.numeric(defin_end[k,3]+1),4])^2)
            Node_to_Pole <- rbind(N1_to_pole1,
                                  N1_to_pole2,
                                  N2_to_pole1,
                                  N2_to_pole2)
            if(which.min(as.matrix(Node_to_Pole)) == 1 && which.max(as.matrix(Node_to_Pole)) == 2){
              end_type[k,1] <- "Plus"
              end_type[k,2] <- "Minus"
            } else if(which.min(as.matrix(Node_to_Pole)) == 2 && which.max(as.matrix(Node_to_Pole)) == 1){
              end_type[k,1] <- "Minus"
              end_type[k,2] <- "Plus"
            } else if(which.min(as.matrix(Node_to_Pole)) == 3 && which.max(as.matrix(Node_to_Pole)) == 4){
              end_type[k,1] <- "Plus"
              end_type[k,2] <- "Minus"
            } else if(which.min(as.matrix(Node_to_Pole)) == 4 && which.max(as.matrix(Node_to_Pole)) == 3){
              end_type[k,1] <- "Minus"
              end_type[k,2] <- "Plus"
            } else {
              end_type[k,1:2] <- NA
            }
          }
          defin_end <- cbind(defin_end,
                             end_type)
          
          for (k in 1:nrow(DF)){
            if(is.na(defin_end[k,5]) || is.na(defin_end[k,6])){
              defin_end[k,1:6] <- NA
              DF[k,1:2] <- NA
            } else {
              if(defin_end[k,2] == DF[k,1] && defin_end[k,5] =="Minus" || defin_end[k,3] == DF[k,1] && defin_end[k,6] =="Minus"){
                defin_end[k,1:6] <- NA
                DF[k,1:2] <- NA
              }
            }
          }
          defin_end <- na.omit(defin_end)
          DF <- na.omit(DF)
        }
        
        if(nrow(DF) > 0){
          defin_end <- cbind(paste(colnames(Segments)[x], i, sep = "_"),
                             defin_end[1],
                             defin_end[4],
                             round(DF[2],4),
                             get(paste(colnames(Segments)[x], i, sep = "_"))[j,5],
                             get(paste(colnames(Segments)[x]))[1,5],
                             get(paste(colnames(Segments)[x]))[1,6])
          names(defin_end)[1] <- "KMT_ID"
          names(defin_end)[2] <- "Interactor_ID"
          names(defin_end)[3] <- "I_class"
          names(defin_end)[4] <- "p_to_P_dist"
          names(defin_end)[5] <- "Relative_pos"
          names(defin_end)[6] <- "Plus_end_dist"
          names(defin_end)[7] <- "Ellipse"
          
          Minus_end <- rbind(Minus_end,
                             defin_end)
        }
      }
    }
    
    list <- unique(Minus_end$Interactor_ID)
    Temp <- data.frame()
    for(k in list){
      DF <- Minus_end %>% filter_at(vars("Interactor_ID"),
                                    any_vars(. == k))
      Temp[k,1:7] <- DF[which.min(DF$p_to_P_dist), 1:7]
    }
    Minus_end <- na.omit(Temp)
    Minus_seed <- rbind(Minus_seed,
                        Minus_end)
  }
}