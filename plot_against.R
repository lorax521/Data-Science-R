plot.against <- function(against, dataframe) {
  "
  Plots one variable in a dataframe against all other variables in the dataframe.

  :param against: dataframe column; ex: df$againstData
  :param dataframe: dataframe; ex: df, or df@data
  "
  par(mfrow=c(floor(sqrt(ncol(dataframe))), ceiling(sqrt(ncol(dataframe)))))
    
  for(i in 1:(ncol(dataframe))) {
    tryCatch({
      plot(against~unlist(dataframe[c(i)]), xlab=paste(names(dataframe[c(i)])), ylab=deparse(substitute(against)))
      mod <- lm(against~unlist(dataframe[c(i)]))
      abline(mod)
    }, error=function(e){print(paste('error:', names(dataframe[c(i)]), sep=' '))},
    warning=function(e){print(paste('categorical variable', names(dataframe[c(i)]), 'only used the first 2 regression coefs for abline', sep=' '))})
  }
  
  par(mfrow=c(1,1))
}
