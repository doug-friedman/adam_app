#  Computes the variance of a weighted mean following Cochran 1977 definition
#  https://stats.stackexchange.com/questions/25895/computing-standard-error-in-weighted-mean-estimation
weighted.var.se <- function(x, w, na.rm=FALSE)
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  return(out)
}

wt.mean.br = function(x, ind, wts){ return(wt.mean(x[ind],wts)) }

boot.wse = function(x, wts){ sd(boot(x, statistic=wt.mean.br, R=1000, wts=wts)$t) }
