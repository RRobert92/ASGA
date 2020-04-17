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

