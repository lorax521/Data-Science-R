mvlm_rank3 <- function(dep, inds) {
  "
  Ranks permutations of any three covariates for a multivariate linear regression model.
  The dependent and indepedent variables should be stored as seperate columns in a single dataframe.
  
  :param dep: dataframe index; the column index of the dependent variable. Ex: df[11] 
  :param inds: dataframe indicies; the indicies of the independent variables. Ex: df[c(4:9)]
  :return df: dataframe; a dataframe containing all permutations of covariates ranked by AIC decending.
  :dependencies: library(stats)
  "
  ifTrue <- function(dat) {
    if (any(duplicated(c(dat)) == T)) {
      return (F)
    } else {
      return (T)
    }
  }
  
  testMVLM <- function(dat) {
    dep1 <- grep(as.character(unlist(dat[1])), names(dep))
    ind1 <- grep(as.character(unlist(dat[2])), names(inds))
    ind2 <- grep(as.character(unlist(dat[3])), names(inds))
    ind3 <- grep(as.character(unlist(dat[4])), names(inds))
    model.mvlm <- glm(unlist(dep[dep1])~unlist(inds[ind1])+unlist(inds[ind2])+unlist(inds[ind3]))
    s <- summary(model.mvlm, corr=T)
    dfupdate <- rbind(s$aic, as.character(unlist(dat[2])), as.character(unlist(dat[3])), as.character(unlist(dat[4])))
    return(dfupdate)
  }
  
  dat <- expand.grid(names(dep), names(inds), names(inds), names(inds))
  dat.sort <- t(apply(dat, 1, sort))
  dat2 <- dat[!duplicated(dat.sort),]
  dat3 <- dat2[apply(dat2, 1, ifTrue),]
  
  df <- data.frame()
  df <- rbind(df, t(apply(dat3, 1, testMVLM)))
  colnames(df)<- c('aic', 'covariate1', 'covariate2', 'covariate3')
  dfout <- df[order(df$aic),]
  return(dfout)
}
