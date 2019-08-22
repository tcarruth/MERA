
getCodes<-function(dat,maxtest=6){
  
  codes<-Detect_scope(dat,eff=NA)                            # what scoping methods are possible?
  ord<-order(nchar(codes),decreasing = T)
  codes[ord][1:min(maxtest,length(codes))]
  
}

DataStrip<-function(dat,eff,code,simno=1){

  datTypes<-c("C","I","A","L","M")
  slotnams<-c("Cat","Ind","CAA","CAL","ML")
  listnams<-c("Chist","Index","CAA","CAL","ML")
  nD<-length(datTypes)
  outlist<-list()

  for(i in 1:nD){

    if(grepl(datTypes[i],code)){

      temp<-slot(dat,slotnams[i])

      if(length(dim(temp))==2){
        outlist[[listnams[i]]]<-slot(dat,slotnams[i])[simno,]
      }else{
        outlist[[listnams[i]]]<-slot(dat,slotnams[i])[simno,,]
      }

    }else{

      outlist[[listnams[i]]]<-NULL

    }

  }

  if(grepl("E",code)){
    outlist[['Ehist']]<-eff[simno,]
    outlist[['condition']]<-"effort"
  }else{
    if(!grepl("C",code)){
      outlist[['Ehist']]<-rep(1,length(dat@Year))
      outlist[['condition']]<-"effort"
    }else{
      outlist[['condition']]<-"catch"
    }

  }

  if(length(dat@CAL_bins)>1){
    NL<-length(dat@CAL_bins)
    outlist[['length_bin']]<-(dat@CAL_bins[1:(NL-1)]+dat@CAL_bins[2:NL])/2
  }

  outlist

}


makesimsamOM<-function(OM,ndeps=20,DepLB=0.05, DepUB=0.8){
  OM_s<-trimOM(OM,ndeps)
  OM_s@cpars$D<-seq(DepLB,DepUB,length.out=ndeps)
  OM_s@proyears<-5
  OM_s@interval<-5
  OM_s
}

goodslot<-function(x,LHy){

  good<-FALSE
  if(length(x)>0){
    if(dim(x)[2]>=LHy)good=TRUE

  }
  good

}

Detect_scope<-function(dat,eff=NA,simno=1,minndat=20){

  eff<-matrix(eff,nrow=1)
  ny<-ncol(dat@Cat)

  if(length(dat@LHYear)>0){
    if(is.na(dat@LHYear)){
      LHy<-ny
    }else{
      LHy<-dat@LHYear
    }
  }else{
    LHy<-ny
  }

  if(LHy>1900) LHy<- LHy-dat@Year[1]+1 # if calendar years are specified
  if(length(LHy)==0){
    LHy=ny
  }else if(is.na(LHy)){
    LHy=ny
  }
  yind<-1:LHy

  Cat<-Ind<-Eff<-CAA<-CAL<-ML<-NA
  nL<-length(dat@CAL_bins)-1

  if(goodslot(dat@Cat,LHy)) Cat<-dat@Cat[simno,yind]
  if(goodslot(dat@Ind,LHy)) Ind <-dat@Ind[simno,yind]
  if(goodslot(eff,LHy))     Eff<-eff[yind]

  # need to make sure CAL data is the right dimensions
  CAL<-array(0,c(1,ny,nL))
  if(length(dat@CAL)>0){
    if(sum(!is.na(dat@CAL))>minndat){
      nyCAL<-dim(dat@CAL)[2]
      nLCAL<-dim(dat@CAL)[3]

      CAL[1,(ny-nyCAL+1):ny,1:nLCAL]<-dat@CAL[1,,] # assumes they are reporting the most recent years for the shortest length classes
    }else{
      CAL<-NA
    }
  }else{
    CAL<-NA
  }

  # need to make sure CAA data is the right dimensions
  if(length(dat@MaxAge)>0){
    if(is.na(dat@MaxAge)){
      na<-ceiling(-log(0.01)/dat@Mort)
    }else{
      na<-dat@MaxAge
    }
  }else{
    na<-ceiling(-log(0.01)/dat@Mort)
  }

  CAA<-array(0,c(1,ny,na))
  if(length(dat@CAA)>0){
    if(sum(!is.na(dat@CAA))>minndat){
      nyCAA<-dim(dat@CAA)[2]
      naCAA<-dim(dat@CAA)[3]

      CAA[1,(ny-nyCAA+1):ny,1:naCAA]<-dat@CAA[1,,] # assumes they are reporting the most recent years for the youngest age classes
    }else{
      CAA<-NA
    }
  }else{
    CAA<-NA
  }

  if(goodslot(dat@ML,LHy))  ML<-dat@ML[simno,yind]

  condC<-sum(!is.na(Cat))==LHy
  condE<-sum(!is.na(Eff))==LHy
  condI<-sum(!is.na(Ind))>1
  condA<-sum(!is.na(CAA))>minndat
  condL<-sum(!is.na(CAL))>minndat
  condM<-sum(!is.na(ML))>minndat

  # Possible data combinations
  datTypes<-c("C","E","I","A","L","M")
  #Tcond<-expand.grid(rep(list(c("TRUE","FALSE")),length(datTypes)))
  #for(c in 1:ncol(Tcond))Tcond[,c]<-as.logical(Tcond[,c])
  #names(Tcond)<-datTypes
  #Tcond<-Tcond[!((Tcond$C|Tcond$E) & apply(Tcond,1,sum)==1),] # remove only Catch and only Effort

  # condC<- condI<- condA<- condL <- FALSE
  # condI<- condA<- condL <- FALSE
  # condA<- condL <- FALSE

  # Available data combinations
  Alist<-list()
  Alist[[1]]<-unique(c(condC,FALSE))
  Alist[[2]]<-unique(c(condE,FALSE))
  Alist[[3]]<-unique(c(condI,FALSE))
  Alist[[4]]<-unique(c(condA,FALSE))
  Alist[[5]]<-unique(c(condL,FALSE))
  Alist[[6]]<-unique(c(condM,FALSE))

  Acond<-expand.grid(Alist)
  names(Acond)<-datTypes
  Acond<-Acond[!((Acond$C|Acond$E) & apply(Acond,1,sum)==1),] # remove only Catch and only Effort

  nA<-nrow(Acond)
  DataCode<-rep(NA,nA)
  for(i in 1:(nA-1)){
    DataCode[i]<-paste(datTypes[unlist(Acond[i,])],collapse="_")
  }
  DataCode[nA]<-"None"

  DataCode

}



getOMsim<-function(OM,simno=1,silent=T){
  
  
  if(length(OM@cpars)==0){
    
    if(!silent) message("There is no cpars slot in this OM object, only the nsim slot has been modified")
  }else{
    
    for(i in 1:length(OM@cpars)){
      
      dims<-dim(OM@cpars[[i]])
      ndim<-length(dims)
      
      if(ndim==0){
        OM@cpars[[i]]<-OM@cpars[[i]][simno]
      }else if(ndim==2){
        OM@cpars[[i]]<-matrix(OM@cpars[[i]][simno,],nrow=1)
      }else if(ndim==3){
        OM@cpars[[i]]<-array(OM@cpars[[i]][simno,,],c(1,dims[2:3]))
      }else if(ndim==4){
        OM@cpars[[i]]<-array(OM@cpars[[i]][simno,,,],c(1,dims[2:4]))
      }else if(ndim==5){  
        OM@cpars[[i]]<-array(OM@cpars[[i]][simno,,,,],c(1,dims[2:5]))
      }
      
    }
    
  }
  
  OM@nsim<-1
  
  OM
}


Scoping_parallel<-function(x,OM,dat,eff,code){
  
  outlist<-DataStrip(dat,eff,code,simno=x)
  
  OMp<-getOMsim(OM,simno=x)
  
  out<-SRA_scope(OM=OMp,
                 Chist = outlist$Chist,
                 Ehist = outlist$Ehist,
                 condition = outlist$condition,
                 Index= outlist$Index,
                 CAA = outlist$CAA,
                 CAL = outlist$CAL,
                 ML = outlist$ML,
                 length_bin = outlist$length_bin,
                 report=F,
                 cores=1)
  out[[1]]
  
}


SimSam<-function(OM,dat,code){
  
  sfExport('DataStrip','getOMsim')
  scoped<-sfSapply(1:OM@nsim,Scoping_parallel,OM=OM,dat=dat,code=code)
  deps<-lapply(scoped, function(x)x@cpars$D)
  list(Sim=OM@cpars$D, Sam=unlist(deps))
  
}


GetDep<-function(OM,dat,eff=NA,code,cores=4){
  if(is.na(eff[1])){
    eff<-dat@Cat/dat@Ind
  }  
  
  eff<-eff/apply(eff,1,mean)
  outlist<-DataStrip(dat,eff,code,simno=1)
  
  out<-SRA_scope(OM=OM,
                 Chist = outlist$Chist,
                 Ehist = outlist$Ehist,
                 condition = outlist$condition,
                 Index= outlist$Index,
                 CAA = outlist$CAA,
                 CAL = outlist$CAL,
                 ML = outlist$ML,
                 length_bin = outlist$length_bin,
                 report=F,
                 cores=cores)
  
  #saveRDS(OM,"C:/temp/OM.Rdata")
  #saveRDS(outlist,"C:/temp/outlist.Rdata")
  
  out[[1]]@cpars$D[out$output$conv]
  
}

getCodes<-function(dat,maxtest=6){
  
  codes<-Detect_scope(dat,eff=NA)                            # what scoping methods are possible?
  ord<-order(nchar(codes),decreasing = T)
  codes[ord][1:min(maxtest,length(codes))]
  
}



