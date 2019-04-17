RealFease <- function(Data=NULL){
  if(is.null(Data))stop('no data provided')
  if(class(Data)!="Data")stop('object not of class Data')
  MPs <- avail('MP')
  nMPs<-length(MPs)
  Err<-rep(TRUE,nMPs)
  
  for(i in 1:nMPs){
    
    tryCatch({
      test<-do.call(MPs[i],list(x=1,Data=Data))
      Err[i]=FALSE
    },
    error = function(e){
      #print(paste(i,MPs[i]))
    })
    
  }
  MPs[!Err]
  
}
