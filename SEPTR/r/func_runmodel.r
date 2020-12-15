#function to choose which model to run and the range of the parameters accordingly
runmodel<-function(subsettemp,considercost)
{
  ranges_master <- rbind(
    min = c(b=0.035, cost=0.1,theta = -0.001,logGamma = -16),
    max = c(b=0.050, cost=0.9,theta = -0.00001,logGamma = -11))
  
  if (is.null(subsettemp)==FALSE)
{
  print(paste("calculating for temp =",subsettemp))
  observed<-observed[which(observed$temp%in%subsettemp),]
  print("initializing model")
  if(considercost==TRUE)
  {
  print("one temp and cost included")
  model <- modelInit_uni(dir="../rodeo")                  #one temperature, cost included
  ranges<-ranges_master[,c("b","cost","logGamma")]
  }          
  else{
    print("one temp and no cost")
    model <- modelInit_uni_nocost(dir="../rodeo")        #one temperature, no cost included
    ranges<-ranges_master[,c("b","logGamma")]
  }}
if (is.null(subsettemp)==TRUE) 
{  
if(considercost==TRUE)
{ 
  print(" all temp and cost included")                    #all temperature,  cost included
  model <- modelInit(dir="../rodeo")
  ranges<-ranges_master[,c("b","cost","theta","logGamma")]
}
  else{ 
    print(" all temp included and cost excluded")
    model <- modelInit_nocost(dir="../rodeo")           #all temperature, no cost included
    ranges<-ranges_master[,c("b","theta","logGamma")]}
}
  model_ranges<-list(model<-model, 
       ranges<-ranges)
  names(model_ranges)<-c("model","ranges")
return(model_ranges)
}

