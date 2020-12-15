# Runs the model for a single vector of parameter values;
# The result is a 3D array [strain, obs/sim, case]
modelApply <- function(p, model, d) {

  # Helper function taking logs
  takeLog <- function(x) {log10(x + 1)}   #+1 to avoid negative values

  # Helper function for use with apply (x = one record of d)
  f <- function(x, p) {
    # print(x)
    ini <- setNames(x[paste0(model$namesVars(),".ini")], model$namesVars())
    model$setVars(ini)
    p <- c(p, x)  # to pass experimental conditions - (1 set of parameter value and 1 set of observed data)
    # print(p)
    model$setPars(p[model$namesPars()])
    sim <- model$step(t0=0, h=x["to"]-x["from"], check=TRUE)     #simulated value from time to-from
##    sim <- model$dynamics(times=c(0, x["to"]-x["from"]), rtol=1e-6, atol=0.1)
##    sim <- sim[2,-1]
    sim <- takeLog(sim[model$namesVars()]) - takeLog(ini)   #log simulated values - log original values (state variables)     
    obs <- takeLog(x[paste0(model$namesVars(),".end")]) - takeLog(ini) #log observed final values - log orginal init values
    matrix(c(obs, sim), nrow=length(obs), ncol=2,
     dimnames=list(model$namesVars(), c("obs","sim")))
  }
  z <- apply(as.matrix(d[,sapply(d, is.numeric)]), 1, f, p=p)  #sapply is to check and run the as.matrix only for numerical values
  z <- array(z, dim=c(model$lenVars(), 2, nrow(d)), dimnames=list(
    strain=model$namesVars(), origin=c("obs","sim"), case=paste0("case.",1:nrow(d))))
  z
}
