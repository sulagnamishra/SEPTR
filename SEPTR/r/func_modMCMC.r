#modMCMC
modmcmc<-function(ranges,d,model,guess,runs)
 { # Define objective function
  objFunc <- function(p) {
    # Next gives a 3D array [strain, obs/sim, case]
    z <- modelApply(p=p, d=d, model=model)
    # Next gives residuals as a matrix [case, strain]
    resid <- t(z[,"sim",] - z[,"obs",])
    return(as.vector(resid))
  }
  
  # Minimize with FME
  P <- FME::modFit(f=objFunc, p=guess, lower=ranges["min",],
                   upper=ranges["max",])
  
  # summary of fit
  sP    <- summary(P)
  sP[]
  #print(sP)
  
  # Running an MCMC
  #---------------------
  # estimate parameter covariances
  # (to efficiently generate new parameter values)
  Covar   <- sP$cov.scaled 
  Covar <-sP$cov.unscaled
  
  # the model variance
  s2prior <- sP$modVariance
  
  MCMC <- modMCMC(f = objFunc, p = P$par,jump = Covar, niter = runs, lower=ranges["min",],
                  upper=ranges["max",],
                  var0 = s2prior, wvar0 = NULL, updatecov = 100)
  #updatecov: updates the proposal every 100 runs - Adaptive Metropolis algoritm
  #ntrydr
  return(MCMC)
}
