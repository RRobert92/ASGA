##################################################################
# Set of functions to define fiber area and neighborhood density # 
##################################################################
s
## Calculate ratio of KMT_length / (-) end distance to the pole. Lower ratio, define leading KMTs

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
  while (i <= nrow(get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])["Leading"])), sep = "_")))) {
    leading_points[i, ] <- get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])["Leading"])), sep = "_"))[i, 1]
    i = i + 5
  }
  leading_points <- na.omit(leading_points)
}ss