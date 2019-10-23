


SimTest<-function(OM,code, ndeps=40, DepLB=0.05, DepUB=0.8){
  
  ndeps<-min(ndeps,OM@nsim)
  OMc<-makesimsamOM(OM,ndeps=ndeps,DepLB=DepLB,DepUB=DepUB)  # Convert Operating model to simsam OM (depletion range in cpars)
  SimMSE<-runMSE(OMc,MPs="curE",PPD=TRUE)                    # Simulate historical data for each depletion level
  dat<-SimMSE@Misc[[4]][[1]]                                 # Extract the posterior predicted data
  SimSam(OMc,dat,code)                                       # Get the depletion from the conditioned operating model
  
}

fitdep<-function(out,dEst=0.5,plot=T){
  
  fitdat<-data.frame(Sim=out$Sim,Sam=out$Sam)      # Summarize these data (simulated versus assessed)
  opt<-optim(par=  c(-5,0,0), fitdep_int,
             #method="L-BFGS-B",
             #lower=c(-1, -20, -2),
             #upper=c(1,   2,  2),
             method="Nelder-Mead",
             x=fitdat$Sam,y=fitdat$Sim,
             hessian=T,
             control=list(trace=6,REPORT=1,maxit=20))
  
  fitted<-fitdep_int(par=opt$par,x=fitdat$Sam,y=fitdat$Sim,mode=2)
  ord<-order(-fitdat$Sam)
  # dEst<-rlnorm(10,log(0.3),0.1)
  
  nsim<-length(dEst)
  varcov<-solve(opt$hessian)
  
  totsamp<-nsim*2
  samps<-rmvnorm(totsamp,mean=opt$par,sigma=varcov)
  
  nobs<-nrow(fitdat)
  stoch<-array(NA,c(totsamp,nobs))
 
  for(i in 1:totsamp)   stoch[i,]<-fitdep_int(samps[i,],x=fitdat$Sam,y=fitdat$Sim,mode=2)
 
  sums<-apply(stoch,1,function(x,sim=fitdat$Sim)sum((x-sim)<0))
  tokeep<-((1:totsamp)[sums>(nobs*0.15)&sums<(nobs*0.85)])[1:nsim]
  samps<-matrix(samps[tokeep,],nrow=nsim)
  stoch<-matrix(stoch[tokeep,],nrow=nsim)
  
  biascor<-rep(NA,nsim)
  
  for(i in 1:nsim)biascor[i]<-fitdep_int(samps[i,],x=dEst[i],y=dEst[i],mode=2)
  #fitout= 
  list(biascor=biascor,samps=samps,stoch=stoch,opt=opt,dEst=dEst,Sam=fitdat$Sam,Sim=fitdat$Sim,fitted=fitted)
  
}


fitdep_int<-function(par,x,y,mode=1){
  # par<-c(0,0,0); x = fitdat$Sam; y=fitdat$Sim # inverted because you wish to predict 'real' / simulated depletion
  print(par)
  yest<-exp(par[1])+exp(par[2])*x^exp(par[3])   # exponential model
  rat<-log(yest/y)
  sdEmp<-min(0.5,sd(rat))
  print(yest)
  print("----")
  
  nLLdat<-(-dnorm(0,rat,sd=sdEmp,log=TRUE))
  nLLprior<-(-dnorm(par,c(-5,0,0),sd=c(10,10,10),log=TRUE))
  #sdEmp<-0.2# empirical sd from fit
  #sum(-dnorm(yest,y,sd=sdEmp,log=TRUE)) # return sum of neg LL
  if(mode==1){
    return(sum(c(nLLdat,nLLprior))) #return(sum((yest-y)^2))
  }else{
    return(yest)
  }
  
}



biasplot<-function(fitout,lab=""){
  
  dEst<-fitout$dEst
  biascor<-fitout$biascor
  
  xlim<-c(0,max(c(fitout$dEst,1)))
  ylim<-c(0,max(c(fitout$biascor,1)))
  plot(fitout$Sam,fitout$Sim,xlab="Assessed status",ylab="Simulated (bias corrected) status",xlim=xlim,ylim=ylim)
  lines(c(-1,10),c(-1,10),col="#99999950",lwd=2)
  ord<-order(-fitout$Sam)
  lines(fitout$Sam[ord],fitout$fitted[ord],col='red',lwd=2)
  nsim<-length(fitout$dEst)
   
  for(i in 1:nsim){
    
    lines(fitout$Sam[ord],fitout$stoch[i,ord],col="#ff000050")
    lines(c(fitout$dEst[i],fitout$dEst[i]),c(0,fitout$biascor[i]),col="#0000ff50")
    lines(c(0,fitout$dEst[i]),c(fitout$biascor[i],fitout$biascor[i]),col="#00ff0050")
    
  }
  
  dAss<-density(fitout$dEst,adjust=1.5,from=0)
  dBC<-density(fitout$biascor,adjust=1.5,from=0)
  
  polygon(c(0,dAss$x),c(0,dAss$y/max(dAss$y)*0.1),col="#0000ff50",border="#0000ff50")
  polygon(c(0,dBC$y/max(dBC$y)*0.08),c(0,dBC$x),col="#00ff0050",border="#00ff0050")
  mtext(lab,side=3,line=0.4)

}




getCodes<-function(dat,maxtest=6){
  
  codes<-Detect_scope(dat)                             # what scoping methods are possible?
  ord<-order(nchar(codes),decreasing = T)
  codes[ord][1:min(maxtest,length(codes))]
  
}

DataStrip<-function(dat,code,simno=1){

  datTypes<-c("C","E","I","A","L","M")
  slotnams<-c("Cat","Effort","Ind","CAA","CAL","ML")
  listnams<-c("Chist","Ehist","Index","CAA","CAL","ML")
  slotnams2<-c("Cat","Effort","Ind","CAA","ML","ML")  # hack for current SRAscope limitation
  listnams2<-c("Chist","Ehist","Index","CAA","ML","ML") # hack for current SRAscope limitation
  
  nD<-length(datTypes)
  outlist<-list()
  
  dat@ML<-dat@ML/dat@vbLinf*100 # ML conversion

  for(i in 1:nD){
    
    outlist[[listnams[i]]]<-NULL
    
  }

  for(i in 1:nD){

    if(grepl(datTypes[i],code)){

      temp<-slot(dat,slotnams[i])

      if(length(dim(temp))==2){
        outlist[[listnams[i]]]<-slot(dat,slotnams[i])[simno,]
        
      }else{
        outlist[[listnams[i]]]<-slot(dat,slotnams[i])[simno,,]
       
      }
      
      #temp<-slot(dat,slotnams2[i])
      
      #if(length(dim(temp))==2){
        
       # outlist[[listnams2[i]]]<-slot(dat,slotnams2[i])[simno,]
      #}else{
       
      #  outlist[[listnams2[i]]]<-slot(dat,slotnams2[i])[simno,,]
      #}

    }

  }

  if(grepl("E",code)){
     outlist[['condition']]<-"effort"
  }else{
    if(!grepl("C",code)){
      outlist[['Ehist']]<-rep(1,length(dat@Year))
      outlist[['condition']]<-"effort"
    }else{
      outlist[['condition']]<-"catch"
    }

  }
  
  if(code=="C"){
    outlist[['Ehist']]<-rep(1,length(dat@Year))
    outlist[['condition']]<-"effort" # exception for catch only methods
  } 

  if(length(dat@CAL_bins)>1){
    NL<-length(dat@CAL_bins)
    outlist[['length_bin']]<-(dat@CAL_bins[1:(NL-1)]+dat@CAL_bins[2:NL])/2
  }
  
  # Cut out projection PPD years
  #yrs<-1:OM@nyears
  
  #for(i in 1:length(outlist)){
    
   # if(names(outlist)[i]%in%c("Chist","Ehist","Index","ML")){
    #  outlist[[i]]<-outlist[[i]][yrs]
      
    #}else if(names(outlist)[i]%in%c("CAA","CAL")))

    #}
    
  #}
  
  outlist

}


makesimsamOM<-function(OM,ndeps=40,DepLB=0.05, DepUB=0.8){
  OM_s<-trimOM(OM,ndeps)
  OM_s@cpars$D<-seq(DepLB,DepUB,length.out=ndeps)
  OM_s@interval<-100
  OM_s
}

goodslot<-function(x,LHy){

  good<-FALSE
  if(length(x)>0){
    if(dim(x)[2]>=LHy)good=TRUE

  }
  good

}

Detect_scope<-function(dat,simno=1,minndat=20){

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
  if(goodslot(dat@Effort,LHy))     Eff<-dat@Effort[simno,yind]

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
  Acond<-Acond[!(Acond$E & apply(Acond,1,sum)==1),]    # remove only effort 
  Acond<-Acond[!(Acond$A&Acond$L),]                    # remove age + length conditioning
  Acond<-Acond[!(Acond$L&Acond$M),]                    # remove length + mean length conditioning

  nA<-nrow(Acond)
  DataCode<-rep(NA,nA-1)
  for(i in 1:(nA-1)){
    DataCode[i]<-paste(datTypes[unlist(Acond[i,])],collapse="_")
  }
  #DataCode[nA]<-"None"

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


Scoping_parallel<-function(x,OMc,dat,code){
  
  outlist<-DataStrip(dat,code,simno=x)
  
  OMp<-getOMsim(OMc,simno=x)
  #loadRDS(OMp,"C:/temp/OMp.rds")
  #loadRDS(outlist,"C:/temp/outlist.rds")
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
  out@OM
  
}


SimSam<-function(OMc,dat,code){
  
  sfExport('DataStrip','getOMsim')
  scoped<-sfSapply(1:OMc@nsim,Scoping_parallel,OMc=OMc,dat=dat,code=code)
  deps<-lapply(scoped, function(x)x@cpars$D)
  list(Sim=OMc@cpars$D, Sam=unlist(deps))
  
}


GetDep<-function(OM,dat,code,cores=4){
  
  outlist<-DataStrip(dat,code,simno=1)
  #saveRDS(OM,"C:/temp/OM")
  #saveRDS(outlist,"C:/temp/outlist")
    
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
                 mean_fit = TRUE,
                 cores=cores)
  
  out
  
}

getCodes<-function(dat,maxtest=6){
  
  codes<-Detect_scope(dat)                            # what scoping methods are possible?
  ord<-order(nchar(codes),decreasing = T)
  codes[ord][1:min(maxtest,length(codes))]
  
}


DataTrim<-function(dat,OM,startyr=1,endyr=NA){
  
  if(is.na(endyr)){
    stop("You need to specify an endyr to cut the data")
  }else if(endyr > length(dat@Year)){
    stop("You specified an end yr that is greater than the length of the Year slot")
  }else{
    yind<-1:OM@nyears
    dat@Year<-dat@Year[yind]
    dat@Cat<-dat@Cat[,yind]
    dat@Effort<-dat@Effort[,yind]
    dat@Ind<-dat@Ind[,yind]
    dat@ML<-dat@ML[,yind]
    dat@Rec<-dat@Rec[,yind]
    dat@Lc<-dat@Lc[,yind]
    dat@Lbar<-dat@Lbar[,yind]
    dat@CAL<-dat@CAL[,yind,]
    dat@CAA<-dat@CAA[,yind,]
  }
  
  dat
  
}


