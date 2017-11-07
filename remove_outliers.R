remove.outliers <- function(x, multiple, na.rm = TRUE) {
  "
  Removes outliers beyond a specified interquartile multiple and prints the outliers.
  
  :param x: 1D vector; values
  :param times: numeric; the amount times the interquartile range you want to remove. Outliers are found beyond 1.5 * IQR and extreme outliers 3 * IQR.
  :param na.rm: boolean; Typically do not change. Removes NA values from calculations
  :return y: vector; Contains the input x with outliers set to NA.
  "
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  iqr <- IQR(x, na.rm = na.rm)
  H <- multiple * iqr
  
  outs <- x[which((x > (qnt[2] + H)) | (x < (qnt[1] - H)))]
  print(paste('outliers removed: ', length(outs[is.finite(outs)]), sep = ''))
  print(outs[is.finite(outs)])
  
  y <- x
  y[which((x > (qnt[2] + H)) | (x < (qnt[1] - H)))] <- NA
  return(y)
}
