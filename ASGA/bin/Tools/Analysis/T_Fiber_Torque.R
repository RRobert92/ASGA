################################################################################################
# Tool Fiber_torque
#
# Set of functions to define fiber torque along the spindle pole axis
#
# !IMPOTENT! this tool is relaying on the Fiber_Area function and have to be run in 
# conjunction with them. Advised to use it after final calculation of fiber area/density
# but before relative_pos_1_fiber. 
# !Running this tool separately will required running polygon calculation!
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-29
################################################################################################


# Function: ... ------------------------------------------------------------------

# pick a point i and i + 1
# median as a zero, recalculate position of a points 
# 
angle1 <- data.frame()
for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)){
  tryCatch({
    df <- data.frame(get(paste(colnames(Segments)[i], "fiber", sep = "_"))["X_Coord"] - mean(as.matrix(get(paste(colnames(Segments)[i], "fiber", sep = "_"))["X_Coord"])),
                     get(paste(colnames(Segments)[i], "fiber", sep = "_"))["Z_Coord"] - mean(as.matrix(get(paste(colnames(Segments)[i], "fiber", sep = "_"))["Z_Coord"])))
    P12 <- sqrt((0 - df[1,1])^2 + (0 - df[1,2])^2)
    P13 <- sqrt((0 - df[nrow(df),1])^2 + (0 - df[nrow(df),2])^2)
    P23 <- sqrt((df[nrow(df),1] - df[1,1])^2 + (df[nrow(df),2] - df[1,2])^2)
    
    angle1[i,1] <- acos((P12^2 + P13^2 - P23^2)/(2*P12*P13))   #atan2((df[nrow(df),2] - df[1,2]), (df[nrow(df),1]) - df[1,1])
    angle1[i,1] <- angle1[i,1] * 180/pi
    angle1[i,2] <- angle1[i,1] / get(colnames(Segments)[i])$plus_dist_to_pole[1]
  },
  error = function(e){})
}

angle1 <- na.omit(angle1)

median(as.matrix(angle[2]))

ggplot(df, aes(X_Coord, Z_Coord)) + geom_line() + theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)

for(j in 1:nrow(Pole1_32_fiber)){
df <- data.frame()
df[1, 1:3] <- Pole1_32_fiber[j, 1:3]
for(i in 7){
  df[i,1:3] <- Points[as.numeric(Pole1_32_fiber[j,i] + 1), 2:4]
}
df <- na.omit(df)
base <- data.frame()
base[1:nrow(df),1:3] <- df[1,1:3]
df <- df-base
print(ggplot(df, aes(X_Coord, Z_Coord)) + geom_point() +theme_classic() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0))

 
}
