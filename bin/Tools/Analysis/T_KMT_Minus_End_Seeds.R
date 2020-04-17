##################################################################################
# Function to calculate distribution of a (-) end in close proximity to the KMT #
##################################################################################
# The tool is calculating distance of each point on KMT (p) to every (-) end of SMT and KMT (P_S or P_K)
# Then we associate point_id to segment_id of P_S or P_K which are closer to the p then 55nm 
# Multiple same segment_ID are marred to single entrance with their closes distance to the p
# The final output is data file for each KMT with: 
# KMT_ID, 
# Interactor_ID, 
# class of interactor, 
# p to P distance
# Position at the spindle pole axis a.k.a. pole to pole axis,
# Relative position,
# Plus end distance to the pole,
# Ellipse position
Minus_end_seed <- function(x){
  for(i in 1:nrow(get(paste(colnames(Segments)[x])))){
    DF <- data.frame()
    for(j in 1:nrow(get(paste(colnames(Segments)[x], j, sep = "_")))){
      p_to_P <- Nodes
      p_to_P[5:7] <- get(paste(colnames(Segments)[x],j, sep = "_"))[j,2:4] 
      
      p_to_P$dist <- apply(p_to_P[2:7], 
                       1, 
                       function(y) dist(matrix(y, 
                                               nrow = 2, 
                                               byrow = TRUE)))
      DF <- data.frame(p_to_P[with(p_to_P, dist <= 0.1 & dist >= 0),"Node ID"],
                       p_to_P[with(p_to_P, dist <= 0.1 & dist >= 0), "dist"])
      for (i in 1:nrow(DF)){
        all_end <- Segments %>% filter_at(vars(starts_with("Node")),
                                          any_vars(. == DF[i,1]))
        defin_end[i,1:3] <- all_end %>% select(`Segment ID`,
                                               `Node ID #1`,
                                               `Node ID #2`)
      }
      
    }
  }
  
}
