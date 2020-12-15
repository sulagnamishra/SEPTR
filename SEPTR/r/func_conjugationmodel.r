conjugationmodel<-function(combination,subsetD2R,considertheta,runMCMC,mcmcruns,suffix)
{ 
 
#################################################################################################  
#                   subsetting the data set according to the cases under study            
#################################################################################################  
  considercost<-combination$considercost
  #considercost_endpoint<-combination$considercost_endpoint
  subsettemp<-as.vector(combination$subsettemp)
  if(subsettemp==c("NULL"))
    subsettemp=NULL
  if(subsetD2R==c("NULL"))
    subsetD2R=NULL
  if (is.null(subsetD2R)==FALSE)
  {
    print(paste("calculating for D2R =",subsetD2R))
    observed<-observed[which(observed$D2R%in%subsetD2R),]
  }
  #subset the simulation for temperature
if (is.null(subsettemp)==FALSE)
  {
    print(paste("calculating for temp =",subsettemp))
    observed<-observed[which(observed$temp%in%subsettemp),]
  }
  #################################################################################################  
  #       Choose the model setup and parameters to be considered according to the case           
  #################################################################################################      
  print("initializing model")
  
model_ranges<-runmodel(subsettemp,considercost)
model<-model_ranges$model
model$plotStoichiometry(box=1)
  
#################################################################################################  
#                       LHS for initial guess value of the parameters          
#################################################################################################
  
print("running the model with random parameter sets")
runsLHS <- 600
ranges<-model_ranges$ranges

filename<-paste0("../out/marginals",combinationnumber,"_",suffix,".pdf")
pdf(file=filename, width=6, height=5)
omar <- par("mar")
par(mar=c(4.2, 4.2, 0.2, 0.2))
parsLHS <- fitLHS(ranges=ranges, runs=runsLHS, d=observed,
                  model=model, plot=TRUE)
par(mar=omar)
graphics.off()


#################################################################################################  
#                       Best fit parameter value using FME - modFit         
#################################################################################################
print("finding best-fit parameters")
parsFME <- fitFME(guess=parsLHS, lower=ranges["min",],
                  upper=ranges["max",], d=observed, model=model, rows=NULL)
best <- cbind(data.frame(Parameter=rownames(parsFME$par)),   #to add the rownames of the returned df as column 
              as.data.frame(parsFME$par))
for (i in 1:ncol(best)) {
  if (is.numeric(best[,i]))
    best[,i] <- signif(best[,i], 3)
}
filename<-paste0("../out/best",combinationnumber,"_",suffix,".txt")
write.table(best, file=filename, sep="\t", quote=FALSE,
            row.names=FALSE, col.names=TRUE)

print("plotting predicted vs. observed")
filename<-paste0("../out/validation",combinationnumber,"_",suffix,".pdf")
pdf(file=filename, width=6, height=2.5)
strains <- dimnames(parsFME$fitted)[["strain"]]
par(mfcol=c(1,length(strains)))
for (str in strains) {
  x <- parsFME$fitted[str,"obs",]
  y <- parsFME$fitted[str,"sim",]
  lim <- c(-0.1, max(c(x,y)))
  plot(x, y, xlim=lim, ylim=lim, cex=0.75, pch=21,
       col="grey20", bg="lightblue",
       xlab="Observed increase", ylab="Predicted increase")
  abline(0, 1, lty=3)
  mtext(side=3, strainNames(str), cex=par("cex"))
}
par(mfcol=c(1,1))
graphics.off()



#################################################################################################  
#                       End point calculations according to levin's method         
#################################################################################################
#NOTE:
#rategamma_t <- calculated with growth rate for R, 
#rategamma_t_withcost <- calculated with growth rate for D/T
#..............................................................................................

#The end point calculations are executed for all the possible combinations and the comparision plots are 
#generated 
if(Runloop==TRUE){
  print("Starting end point calculations")
  gamma_endpoint<-endpt(d=observed,strains=strains,states=c(".ini",".end"),
                        gt=subset(best, Parameter=="b"| Parameter=="cost",select = c("Parameter","Estimate")),suffix=suffix)
  print("Add additional columns (compile data) in the end-point method output file")
  gamma_endpoint[,"cost"]<-best["cost","Estimate"]
  gamma_endpoint[,"logGamma"]<-best["logGamma","Estimate"]
  gamma_endpoint[,"costconsideration_numericalmodel"]<-considercost
  gamma_endpoint[,"tempconsidered"]<-subsettemp
  if(is.null(subsettemp)==TRUE){
    gamma_endpoint[,"tempconsidered"]<-0}
  #gamma_endpoint[,"costconsideration_endpointmethod"]<-considercost_endpoint
  #write.table(gamma_endpoint, file="../out/gamma_endpt.txt", sep="\t", quote=FALSE,
  #            row.names=FALSE, col.names=TRUE)
  write.table(gamma_endpoint, file = paste0("../out/gamma_endpt","_",suffix,".txt"), sep = "\t", quote = FALSE,
              row.names = FALSE, col.names = ifelse(paste0("gamma_endpt","_",suffix,".txt")%in%list.files("../out/"),FALSE,TRUE),
              qmethod = "double", append = TRUE)
}

if(combinationnumber==1|combinationnumber==0)
{
guess_best<-list(parsLHS,best)
names(guess_best)<-c("guess","bestfit")
}
return(guess_best)
}

