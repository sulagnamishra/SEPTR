
# Estimate parameters by analyzing the performance of random parameter sets
fitLHS <- function(ranges, runs, d, model, plot=TRUE) {

  # Create latin hypercube sample of parameters (matrix)
  sample <- lhs::improvedLHS(n=runs, k=ncol(ranges))
  colnames(sample) <- colnames(ranges)
  for (i in 1:ncol(ranges)) {
    sample[,i] <- min(ranges[,i]) + sample[,i] *
      (max(ranges[,i]) - min(ranges[,i]))
  }
  
  #changing the names of the parameters to greek characters for plots
  names_plot<-colnames(sample)
  location_gamma<-which(names_plot=="logGamma")
  location_theta<-which(names_plot=="theta")
  names_plot[location_gamma]<-expression(paste("log",gamma))
  names_plot[location_theta]<-expression(theta)
  
  # Process all parameter sets; the result is a 4D array with dimensions
  # [parameterSet, strain, obs/sim, case]
  z <- plyr::aaply(sample, 1, modelApply, d=d, model=model)  #(Run the modelapply () individually for each parameter set)
  dimnames(z)[[1]] <- paste0("pars.",1:(dim(z)[1]))
  names(dimnames(z))[1]= "pars"

  # Compute mismatch between sim and obs, averaged over cases;
  # the result is a matrix [parameterSet, strain]
  resid <- z[,,"sim",] - z[,,"obs",]
  errorsForStrains <- plyr::aaply(resid, c(1,2), function(x){ sqrt(mean(x^2))}) #matrix -> pars v/s D,T,R

  # Plot error over individual parameters (marginal distributions)
  if (plot) {
    par(mfcol=c(ncol(errorsForStrains),ncol(sample)))
    for (k in 1:ncol(sample)) {
      for (i in 1:ncol(errorsForStrains)) {
        plot(sample[,k], errorsForStrains[,i], pch=20, cex=0.5,
          xlab=if (i == ncol(errorsForStrains)) names_plot[k] else "",
          ylab=if (k == 1) paste("RMSE,",colnames(errorsForStrains)[i],"strain") else "")
      }
    }
    par(mfcol=c(1,1))
  }

  # Compute total errors; the result is a vector with 1 value per parameter set
  errorsTotal <- apply(errorsForStrains, 1, mean)
  mergeparerror <-cbind(sample,errorsTotal)
  mergeparerror<-mergeparerror[order(mergeparerror[,"errorsTotal"]),]
  save(mergeparerror,file=paste0("ParLHSerror",".RData"))
  # Return best-performing set
  return(sample[which.min(errorsTotal),])
}

