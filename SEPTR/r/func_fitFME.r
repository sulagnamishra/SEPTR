fitFME <- function(guess, lower, upper, d, model, rows=NULL) {

  #rows=NULL <- consider all rows for the calculations
  if (!is.null(rows))
    d <- d[rows, ]

  # Define objective function
  objFunc <- function(p) {
    # Next gives a 3D array [strain, obs/sim, case]
    z <- modelApply(p=p, d=d, model=model)
    # Next gives residuals as a matrix [case, strain]
    resid <- t(z[,"sim",] - z[,"obs",])
    return(as.vector(resid))
  }
  
  # Minimize with FME
  opt <- FME::modFit(f=objFunc, p=guess)

  # Compute prediction
  fitted <- modelApply(opt$par, d=d, model=model)  #n observation data computed against the predicted parameter value

  return(list(par=summary(opt)$par, fitted=fitted))
}

