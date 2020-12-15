#Main executable file
rm(list=ls())
options(warn=1)
setwd('/path/to/folder/SEPTR/r')        #path to folder
ifile <- "../in/data_20170929.csv"      #test data   
odir <- "../out"                        #Stores the output files
combinations <- "../in/combination.txt" #Defines the various test cases : Eg - all temperatures - PRM or 20C - LVM, etc
suffix<-format(Sys.time(),"%d-%b-%Y-%H.%M.%S") #Stores the main outputs with unique names for every run
# packages
library("deSolve")
library("rodeo")
library("lhs")
library("plyr")
library("reshape2")
library("FME")
# load utility functions
sources <- list.files(path=".", pattern="^func_.+[.]r$", full.names=TRUE)

for (src in sources) {
  source(src)
  print(paste0("loaded '",src,"'"))
}

#considercost? and Subsetting temp?
#.........................................
Runloop<-FALSE #run the original code single time for all the data points, model cost, no end point calculations
runMCMC<-FALSE #when true runs the MCMC analysis for given number of runs
mcmcruns<-50000

#default case when runloop is False. Default case is all temp - PRM model
default<-data.frame()
default[1,"considercost"]<-TRUE        #consider cost in the numerical method
default[1,"considertheta"]<-TRUE       #consider temperature dependency in the numerical method (monod function - (temp/(theta+temp))
#default[1,"considercost_endpoint"]<-FALSE   #consider cost in the growth rate (reduced growth rate) in the end point method
default[1,"subsettemp"]<-c("NULL")           #subset temperature
default[1,"subsetD2R"]<-c("NULL")            #subset of D2R ratio

#.........................................

print("pre-processing data")
observed <- dataPreprocess(file=ifile, sep=",")
observed <- dataTransform(observed)

#subsetting data set according to end point method
y<-which(observed[,"T.ini"]==0)  #running all methods for the same data set in end point method
#observed<-observed[y,]   #uncomment for end point method
#subsetting data set to remove records where T.end = 0
y<-which(observed[,"T.end"]==0)  #running all methods for the same data set
observed<-observed[-y,]


print("creating data summary")
tmp <- aggregate(data.frame(count=1:nrow(observed)),
  by=observed[,c("D2R_desired","phase","temp")], FUN=length)
tmp <- dcast(data=tmp, phase + D2R_desired ~ temp,
  value.var="count", fill=0)
write.table(tmp, file=paste0("../out/dataSummary","_",
                             suffix,".txt"), sep="\t", quote=FALSE,row.names=FALSE, col.names=TRUE)

#running the conjugation model either for the loop to compare the final results or for single original setting
if(Runloop==TRUE)
{
  options<-read.table(combinations,sep = ",",header=TRUE)
  for(combinationnumber in 1:nrow(options))
  {
    guess_best<-conjugationmodel(combination=options[combinationnumber,],
                                 subsetD2R=default$subsetD2R,
                     considertheta=default$considertheta,runMCMC=runMCMC,
                     mcmcruns=mcmcruns,suffix=suffix)
  }
}else {combinationnumber=0                                                    #default case when runloop is FALSE
guess_best<-conjugationmodel(combination=default,subsetD2R=default$subsetD2R,
                             considertheta=default$considertheta,
                 runMCMC=runMCMC,mcmcruns=mcmcruns,suffix=suffix)
}              
#################################################################################################  
#                      Plotting the End point Vs Numerical solution(PRM) 
#################################################################################################
if(Runloop==TRUE)
{
  print("plotting end point log gamma vs. numerical log gamma")
  pdf(file=paste0("../out/endpointcomparision","_",suffix,".pdf"), 
      width=6, height=4)
  endptcomparision<-read.table(file=paste0("../out/gamma_endpt","_",
                                           suffix,".txt"), sep="\t", header=TRUE)
  par(mfcol=c(1,1))
  par(mar = c(5,5,2,2))
  loggamma<-endptcomparision[which(endptcomparision$tempconsidered!=0 & 
                                     endptcomparision$costconsideration_numericalmodel==TRUE),]
  comp1<-data.frame(-loggamma$logGamma,-log10(loggamma$rategamma_t))
  barplot(t(as.matrix(comp1)), xlab="Temperature",col=c("grey","black"),
          names.arg=loggamma$temp,cex.names=1.4,beside=TRUE,
          ylab=expression(paste("-log",gamma)),cex.axis = 1.5,cex.lab=1.5,ylim = c(11,14),xpd=F)
  legend("topright",legend=c("PRM","EPM"),
         cex=1.1,fill = c("grey","black"),ncol=2, bty="n")
  graphics.off()
}
#################################################################################################  
#                   Run modMCMC for uncertainities in the parameter estimation        
#################################################################################################
if(runMCMC==TRUE){
  print("Finding best fit parameter using advanced parameter fitting: MCMC")
  model_ranges<-runmodel(subsettemp=NULL,considercost=TRUE)
  MCMC<-modmcmc(ranges=model_ranges$ranges, 
                d=observed, model=model_ranges$model,guess=guess_best$guess,
                runs=mcmcruns)
  save(MCMC,file=paste0("test",combinationnumber,"_",mcmcruns,"_",suffix,
                        ".RData"))
  print("plotting pairwise coorelation of the parameter")
  filename<-paste0("../out/MCMC",combinationnumber,"_",mcmcruns,"_",
                   suffix,".pdf")
  pdf(file=filename, width=12, height=12)
  plot(MCMC, Full = TRUE)
  par(mfcol=c(1,1))
  graphics.off()
  pairs(MCMC)
  # function from the coda package.
  raftery.diag(as.mcmc(MCMC$pars))
  cor(MCMC$pars)
  cov(MCMC$pars)   # covariances by MCMC
  #sP$cov.scaled    # covariances by Hessian of fit
}

