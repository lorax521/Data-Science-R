rank_3covariates <- function(dep, inds) {
  "
  Ranks permutations of any three covariates for a multivariate linear regression model.
  The dependent and indepedent variables should be stored as seperate columns in a single dataframe.
  
  :param dep: dataframe index; the column index of the dependent variable. Ex: df[11] 
  :param inds: dataframe indicies; the indicies of the independent variables. Ex: df[c(4:9)]
  :return df: dataframe; a dataframe containing all permutations of covariates ranked by AIC decending.
  :dependencies: library(stats), library(combinat)
  "
  ifTrue <- function(dat) {
    if (any(duplicated(c(dat)) == T)) {
      return (F)
    } else {
      return (T)
    }
  }
  
  testMVLM <- function(dat) {
    dep <- grep(as.character(unlist(dat[1])), names(vote))
    ind1 <- grep(as.character(unlist(dat[2])), names(vote))
    ind2 <- grep(as.character(unlist(dat[3])), names(vote))
    ind3 <- grep(as.character(unlist(dat[4])), names(vote))
    model.mvlm <- glm(unlist(vote@data[dep])~unlist(vote@data[ind1])+unlist(vote@data[ind2])+unlist(vote@data[ind3]))
    s <- summary(model.mvlm, corr=T)
    dfupdate <- rbind(s$aic, as.character(unlist(dat[2])), as.character(unlist(dat[3])), as.character(unlist(dat[4])))
    return(dfupdate)
  }
  
  dat <- expand.grid(names(dep), names(inds), names(inds), names(inds))
  dat.sort <- t(apply(dat, 1, sort))
  dat2 <- dat[!duplicated(dat.sort),]
  dat3 <- dat2[apply(dat2, 1, ifTrue),]
  
  dfout <- data.frame()
  
  dfout <- rbind(dfout, t(apply(dat3, 1, testMVLM)))
  colnames(dfout)<- c('aic', 'covariate1', 'covariate2', 'covariate3')
  df <- dfout[order(dfout$aic),]
  return(df)
}
