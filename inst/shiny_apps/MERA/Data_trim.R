

Data_trimer<-function(Data){

  if(is.na(Data@LHYear)|is.null(Data@LHYear)|Data@LHYear==Data@Year[length(Data@Year)]){
    message("Data could not be trimmed, make sure LHYear is less than max(Year)")
    return(NA)
  }else if(Data@LHYear>(Data@Year[length(Data@Year)]-3)){
    return(NA)
  }else{
    DataT<-Data
    orglength<-length(Data@Year)
    ind<-(1:length(Data@Year))[Data@Year<(Data@LHYear+1)]
    newlength<-length(ind)
    slots<-slotNames(Data)

    for(i in 1:length(slots)){
      temp<-slot(Data,slots[i])
      if(orglength%in%dim(temp)|length(temp)==orglength){
        dims<-dim(temp)
        ndim<-length(dims)
        if(ndim==2){
          slot(DataT,slots[i])<-array(slot(Data,slots[i])[,ind],c(dim(temp)[1],newlength))
        }else if(ndim==3){
          slot(DataT,slots[i])<-array(slot(Data,slots[i])[,ind,],c(dim(temp)[1],newlength,dim(temp)[3]))
        }else{
          slot(DataT,slots[i])<-slot(Data,slots[i])[ind]
        }
      }
    }
    return(DataT)
  }

}

CALsimp<-function(Data,nbins=10,simno=1){

  oldbins<-Data@CAL_bins
  nold<-length(oldbins)
  ind<-rep((1:nold),each=floor(nold/nbins))[1:nold]
  maxbin<-max(ind)
  newCAL_bins<-c(Data@CAL_bins[match(1:maxbin,ind)],Data@CAL_bins[nold])
  ny<-dim(Data@CAL)[2]
  newCAL<-array(0,c(1,ny,maxbin))
  for(b in 1:(nold-1)) newCAL[1,,ind[b]]<-newCAL[1,,ind[b]]+Data@CAL[simno,,b]

  Data@CAL_bins<-newCAL_bins
  Data@CAL<-newCAL
  Data
}
