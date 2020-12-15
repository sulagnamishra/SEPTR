#Model initialization for single temp, Cost not included (LVM)
modelInit_uni_nocost<- function(dir) {
  rd <- function(f, ...) {
    read.table(paste0(dir,"/",f), header=TRUE, sep="\t", ...)
  }
  model <- rodeo$new(vars=rd("vars.txt"), pars=rd("pars_uni_nocost.txt"),
                     funs=rd("funs.txt"), 
                     pros=rd("pros_uni_nocost.txt"),
                     stoi=as.matrix(rd("stoi.txt", row.names=1)), asMatrix=TRUE, dim=c(1))
  model$initStepper(sources=paste0(dir,"/functions.f95"), method="rk5")
  return(model)
}