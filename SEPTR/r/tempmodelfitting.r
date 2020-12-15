rm(list=ls())

# Example data set
data <- cbind(t=c(20,30,37), lg=c(-14.5, -12, -12.5))

# formulation for logGamma with temperature dependence;
# The temperature of the optimum (topt) is fixed so as to
# restrict the number of fitted parameters (2 only). One can
# use an estimate of topt from another paper.
# The fitted parameters are:
#   lg_topt: logGamma at t=topt
#   theta:   controls temperature dependence of logGamma
model <- function(p, data, fix) {
  p["lg_topt"] / (p["theta"] * (data[,"t"] - fix["topt"])^2 + 1)
}

# the objective function
ssr <- function(p, data, fix) {
  sum((data[,"lg"] - model(p, data, fix))^2)
}

# value of fixed parameter (might be changed based on reference)
fix <- c(topt=35)

# initial guess for fitted parameters
init <- c(lg_topt=-12, theta=0)

# fit and plot
opt <- optim(init, ssr, data=data, fix=fix)
plot(data, pch=20, xlab = "Temp in C", ylab = )
temps <- seq(min(data[,"t"]), max(data[,"t"]), 0.5)
lines(temps, model(opt$par, data=data.frame(t=temps), fix=fix))

