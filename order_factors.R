orderFactors.acending <- function(dfFactorColumn) {
  "
  Reorders a dataframe based on a column composed of factors in ascending order.
  Used if data frame factors could be numeric. Implementation ex: df$factorColumn <- orderFactors.acending(df$factorColumn)

  :param dfFactorColumn: dataframe column; ex: df$exampleColumn
  :return dfOrderedColumn: dataframe; cos
  "

  dfFactorColumn <- factor(dfFactorColumn, levels(dfFactorColumn)[c(as.numeric(order(as.numeric(levels(t$bufferzone)))))])
  return(dfFactorColumn)
}

orderFactors.decending <- function(dfFactorColumn) {
  "
  Reorders a dataframe based on a column composed of factors in ascending order.
  Used if data frame factors could be numeric. Implementation ex: df$factorColumn <- orderFactors.acending(df$factorColumn)
  
  :param dfFactorColumn: dataframe column; ex: df$exampleColumn
  :return dfOrderedColumn: dataframe; cos
  "
  dfFactorColumn <- factor(dfFactorColumn, levels(dfFactorColumn)[c(as.numeric(order(as.numeric(levels(t$bufferzone)), decreasing = T)))])
  return(dfFactorColumn)
}
