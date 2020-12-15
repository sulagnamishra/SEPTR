#The function calculates plasmid transfer rate w.r.t temperature. Growth constant 
#is taken from the modfit() in the previous step. It is assumed at all clones grow at a constant rate
#ref - L.Simonsen et al.
#gt is the mean growth rate taken over all strains, for each temperature. 
endpt<-function(d,strains,states,gt,suffix)
{
  # Helper function taking logs
  takeLog <- function(x) {log10(x + 1)}   #+1 to avoid negative values
  #Helper function to calculate growth rate
  growthrate<-function(x)   #based on growth rate of Recipient cells
  {
   reductionduetocost=1
   growthrate1=reductionduetocost*(gt["b","Estimate"]*(x-4))**2
   return(growthrate1)
  }
  
  growthrate_withcost<-function(x)   #based on growthrate of donor/Transconjugant cells
  {
    reductionduetocost=1-gt["cost","Estimate"]
    growthrate_withcost1=reductionduetocost*(gt["b","Estimate"]*(x-4))**2
    return(growthrate_withcost1)
  }
  
    for(j in states)
    {
      d[,paste0("N",j)]<-apply(d[,paste0(strains,j)],1,sum,na.rm=TRUE)
      d[,paste0("S",j)]<-d[,paste0("D",j)]/d[,paste0("N",j)]
      d[,paste0("Y",j)]<-d[,paste0("T",j)]/d[,paste0("R",j)]
      }
#rategamma<-gt*(log(d[,"S.end"]+d[,"Y.end"])-log(d[,"S.ini"]+d[,"Y.ini"]))/(d[,"N.end"]-d[,"N.ini"])

#calculating phi or mu for D and R under proposed and Levin's model condition
d[,"gt"]<-sapply(d$temp,growthrate)
d[,"gt_withcost"]<-sapply(d$temp,growthrate_withcost) 

#with only Y.end (the simplified equation)
t1<-which(d[,"Y.ini"]==0) #discarding all the records wth T.ini != 0 (ref: Simonen 1990)
d_y_end<-d[t1,]
d_y_end[,"rategamma_t"]<-d_y_end[,"gt"]*(log(1+d_y_end[,"Y.end"]/d_y_end[,"S.end"] )/(d_y_end[,"N.end"]-d_y_end[,"N.ini"]))
d_y_end[,"rategamma_t_withcost"]<-d_y_end[,"gt_withcost"]*(log(1+d_y_end[,"Y.end"]/d_y_end[,"S.end"] )/(d_y_end[,"N.end"]-d_y_end[,"N.ini"]))

#d <- with all records
d[,"rategamma_t"]<-d[,"gt"]*(log(d[,"Y.end"]+d[,"S.end"])-log(d[,"Y.ini"]+d[,"S.ini"]))/(d[,"N.end"]-d[,"N.ini"])
d[,"rategamma_t_withcost"]<-d[,"gt_withcost"]*(log(d[,"Y.end"]+d[,"S.end"])-log(d[,"Y.ini"]+d[,"S.ini"]))/(d[,"N.end"]-d[,"N.ini"])

#gamma calculations
gammaendpt_y_end <- aggregate(data.frame(rategamma_t=d_y_end[,"rategamma_t"],
                                         rategamma_t_withcost=d_y_end[,"rategamma_t_withcost"],
                                         gt<-d_y_end[,"gt"], gt_withcost<-d_y_end[,"gt_withcost"]),by=list(temp=d_y_end[,c("temp")]), FUN=mean)
SD_gamma <- aggregate(data.frame(SD_gamma=d_y_end[,"rategamma_t"]),by=list(temp=d_y_end[,c("temp")]), FUN=sd) 
  gammaendpt_y_end<-merge(gammaendpt_y_end,SD_gamma,by="temp")
  
  ##################### negative values returned under normal conditions and no transformation
gammaendpt <- aggregate(data.frame(rategamma_t=d[,"rategamma_t"],
                                   rategamma_t_withcost=d[,"rategamma_t_withcost"],
                                   gt<-d[,"gt"], gt_withcost<-d[,"gt_withcost"]),by=list(temp=d[,c("temp")]), FUN=mean)
SD_gamma <- aggregate(data.frame(SD_gamma=d[,"rategamma_t"]),by=list(temp=d[,c("temp")]), FUN=sd) 
gammaendpt<-merge(gammaendpt,SD_gamma,by="temp")
###################################################################
write.table(d_y_end, file = paste0("../out/endpoint_full","_",suffix,".txt"), sep = "\t", quote = FALSE,
            row.names = FALSE, col.names = ifelse(paste0("endpoint_full","_",suffix,".txt")%in%list.files("../out/"),FALSE,TRUE),
            qmethod = "double", append = TRUE)
return(gammaendpt_y_end)
}
