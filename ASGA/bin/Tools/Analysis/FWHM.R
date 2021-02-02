################################################################################
# Module Gaussian fit
#
# (c) 2020 Schwarz/Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-12-25
#
# Usage example: FWHM(x, y, Output_ID)
#
# Where x - X axis data
# Where y - Y axis data
# Where Output_ID: 1 for FWHM coordinates
#                  2 for peak distribution coordinates
#                  3 for length of of FWHM
#                  4 for Gaussian fit
################################################################################

FWHM <- function(x, y, Output_ID) {
  
  x <- as.numeric(as.matrix(x))
  y <- as.numeric(as.matrix(y))
  
  #Generate fitted line for given data
  Fit1000 <- round(as.numeric(lm(y ~ splines::ns(x, 1000))[["fitted.values"]]), 2)
  bin <- as.data.frame(cbind(Fit1000, x, y))
  
  # Calculate peak of the distribution
  Peak <- data.frame(
    X = bin[which(abs(y - max(bin$Fit1000)) == min(abs(y - max(bin$Fit1000)))), "x"],
    Y = max(bin$Fit1000)
  )
  
  # Calculate Half distribution
  Half_max <- as.numeric(max(bin$Fit1000) / 2)
  d <- as.data.frame(as.numeric(y - Half_max))
  bin <- as.data.frame(cbind(bin, d))
  names(bin)[4] <- "value"
  # Calculate FWHM
  x1 <- data.frame(
    X = bin[which.min(abs(bin[1:which(bin$x == as.numeric(Peak$X)), ]$value)), ]["x"],
    Y = bin[which.min(abs(bin[1:which(bin$x == as.numeric(Peak$X)), ]$value)), ]["y"]
  )
  x2 <- data.frame(
    X = bin[as.numeric(which(bin$x == as.numeric(Peak$X)) + 
                         which.min(abs(bin[which(bin$x == as.numeric(Peak$X)):nrow(bin), ]$value)) - 1), ]["x"],
    Y = bin[as.numeric(which(bin$x == as.numeric(Peak$X)) + 
                         which.min(abs(bin[which(bin$x == as.numeric(Peak$X)):nrow(bin), ]$value)) - 1), ]["y"]
  )
  
  # FWHM
  df <- data.frame(
    X1 = x1$x,
    X2 = x2$x,
    Y = mean(as.numeric(x1$y), as.numeric(x2$y))
  )
  
  # Output: Coordinates for peak
  if (Output_ID == 1) {
    names(Peak)[1] <- "X"
    names(Peak)[2] <- "Y"
    return(Peak)
  }
  
  # Output: Coordinates for FWHM
  if (Output_ID == 2) {
    return(df)
  }
  
  # Output: Length of of FWHM
  if (Output_ID == 3) {
    return(abs(as.numeric(df$X1) - as.numeric(df$X2)))
  }
  
  # Output: Fit
  if (Output_ID == 4) {
    return(as.data.frame(Fit1000))
  }
}

FWHM(x$x, x$y, 1)
