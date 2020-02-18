############################################################
# The core tool to assigned MT points with XYZ coordinates #
############################################################
## x variablein in the function is a DF name with points assinged to the MT (e.g. Pole1_01_1)


Find_XYZ <- function(x) {
  joined_data <- join_all(list(x, Points),
                          by = "Point_ID")
  mutate_all(joined_data, 
             function(y) as.numeric(as.character(y)))
}
