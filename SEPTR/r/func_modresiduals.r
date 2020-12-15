modresiduals<-function(guess, ranges, d, model, rows=NULL) {
  
  #rows=NULL <- consider all rows for the calculations
  if (!is.null(rows))
    d <- d[rows, ]
  lower=ranges["min",]
  upper=ranges["max",]
  # Define objective function
  objFunc <- function(p) {
    # Next gives a 3D array [strain, obs/sim, case]
    z <- modelApply(p=p, d=d, model=model)
    # Next gives residuals as a matrix [case, strain]
    resid <- t(z[,"sim",] - z[,"obs",])
    return(as.vector(resid))
  }
  
  # Minimize with FME
  P <- FME::modFit(f=objFunc, p=guess, lower=lower, upper=upper)
  return(P)
}
