


SimTest<-function(OMsimsam,code, ndeps=40, DepLB=0.05, DepUB=0.8){
  
  ndeps<-min(ndeps,OMsimsam@nsim)
  OMc<-makesimsamOM(OMsimsam,ndeps=ndeps,DepLB=DepLB,DepUB=DepUB)  # Convert Operating model to simsam OM (depletion range in cpars)
  SimMSE<-runMSE(OMc,MPs="curE",PPD=TRUE)                    # Simulate historical data for each depletion level
  dat<-SimMSE@Misc[[4]][[1]]                                 # Extract the posterior predicted data
  SimSam(OMc,dat,code)                                       # Get the depletion from the conditioned operating model
  
}

fitdep<-function(out,dEst=0.5){
  
  fitdat<-data.frame(Sim=out$Sim,Sam=out$Sam)      # Summarize these data (simulated versus assessed)
  opt<-optim(par=  c(-5,0,0), fitdep_int,
             # method="L-BFGS-B",
             # lower=c(-1, -20, -2),
             # upper=c(1,   2,  2),
             method="Nelder-Mead",
             x=fitdat$Sam,y=fitdat$Sim,
             hessian=T,
             control=list(trace=0,REPORT=0,maxit=500))
  
  posdef<-sum(eigen(solve(opt$hessian))$values>0)==3  # is the var covar matrix invertible?
  
  fitted<-fitdep_int(par=opt$par,x=fitdat$Sam,y=fitdat$Sim,mode=2)
  ord<-order(-fitdat$Sam)
  # dEst <- rlnorm(10,log(0.3),0.1)
 
  if(posdef){
    
    nsim<-length(dEst)
    varcov<-solve(opt$hessian)
    
    totsamp<-nsim*2
    samps<-rmvnorm(totsamp,mean=opt$par,sigma=varcov)
    nobs<-nrow(fitdat)
    stoch<-array(NA,c(totsamp,nobs))
    
    for(i in 1:totsamp)   stoch[i,]<-fitdep_int(samps[i,],x=fitdat$Sam,y=fitdat$Sim,mode=2)
    
    sums<-apply(stoch,1,function(x,sim=fitdat$Sim)sum((x-sim)<0,na.rm=T))
    tokeep<-((1:totsamp)[sums>(nobs*0.15)&sums<(nobs*0.85)])[1:nsim]
    samps<-matrix(samps[tokeep,],nrow=nsim)
    stoch<-matrix(stoch[tokeep,],nrow=nsim)
    
    biascor<-rep(NA,nsim)
    
    for(i in 1:nsim)biascor[i]<-fitdep_int(samps[i,],x=dEst[i],y=dEst[i],mode=2)
    
  }else{
    
    samps<-NULL
    biascor<-NULL
    stoch<-NULL
    
  }
  
  #fitout= 
  list(biascor=biascor,samps=samps,stoch=stoch,opt=opt,dEst=dEst,Sam=fitdat$Sam,Sim=fitdat$Sim,fitted=fitted,posdef=posdef)
  
}


fitdep_int<-function(par,x,y,mode=1){
  # par<-c(0,0,0); x = fitdat$Sam; y=fitdat$Sim # inverted because you wish to predict 'real' / simulated depletion
 # print(par)
  yest<-exp(par[1])+exp(par[2])*x^exp(par[3])   # exponential model
  rat<-log(yest/y)
  sdEmp<-min(0.5,sd(rat),na.rm=T)
  #print(yest)
  #print("----")
  
  nLLdat<-(-dnorm(0,rat,sd=sdEmp,log=TRUE))
  nLLprior<-(-dnorm(par,c(-5,0,0),sd=c(10,10,10),log=TRUE))
  #sdEmp<-0.2# empirical sd from fit
  #sum(-dnorm(yest,y,sd=sdEmp,log=TRUE)) # return sum of neg LL
  if(mode==1){
    return(sum(c(nLLdat,nLLprior),na.rm=T)) #return(sum((yest-y)^2))
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
  codes<-codes[order(codes)]
  ord<-order(nchar(codes),decreasing = T)
  codes[ord][1:min(maxtest,length(codes))]
  
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

Detect_scope<-function(dat,simno=1,minndat=5){

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

  Cat<-Ind<-Eff<-CAA<-CAL<-ML<-NULL
  nL<-length(dat@CAL_bins)-1

  if(goodslot(dat@Cat,LHy)) Cat<-dat@Cat[simno,yind]
  if(goodslot(dat@Ind,LHy)) Ind <-c(Ind,dat@Ind[simno,yind])
  if(goodslot(dat@SpInd,LHy)) Ind <-c(Ind,dat@SpInd[simno,yind])
  if(goodslot(dat@VInd,LHy)) Ind <-c(Ind,dat@VInd[simno,yind])
  if(!all(is.na(dat@AddInd))) Ind <-c(Ind,as.vector(dat@AddInd[simno,,]))
  #if(goodslot(dat@Effort,LHy))     Eff<-dat@Effort[simno,yind]

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
  condE<-TRUE#sum(!is.na(Eff))==LHy # always possible from sketched effort  
  condI<-sum(!is.na(Ind))>1
  condA<-sum(!is.na(CAA))>minndat
  condL<-sum(!is.na(CAL))>minndat
  condM<-sum(!is.na(ML))>minndat

  # Possible data combinations
  datTypes<-c("C","E","I","A","L","M")
  
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
  Acond<-Acond[apply(Acond,1,sum)!=1,]    # remove any single data type run
  #Acond<-Acond[!((Acond$E & Acond$I) & apply(Acond,1,sum)>2),] # remove anything with E and I that has something else
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


SimSam<-function(OMc,dat,code){
  
  #sfExport('getOMsim')
  scoped<-sfSapply(1:OMc@nsim, Scoping_parallel, OMc=OMc, dat=dat, code=code, DataStrip=DataStrip, getOMsim=getOMsim)
  deps<-lapply(scoped, function(x)x@cpars$D)
  deps[deps<0.01]<-NA
  list(Sim=OMc@cpars$D, Sam=unlist(deps))
  
}


GetDep<-function(OM,dat,code){
  
  #saveRDS(dat,"C:/temp/dat.rda") # 
  #saveRDS(OM,"C:/temp/OM.rda")     
  #saveRDS(code,"C:/temp/code.rda")
  
  datS<-DataStrip2(dat,OM,code,simno=1)
  
  nsim<-input$nsim
  if(input$Parallel){
     if(nsim>8){
        parallel=T
        setup(cpus=ncpus)
        AM(paste0("Using parallel processing for conditioning (",ncpus,") cpus"))
     }
  }
  
  selectivity="dome"
  if(all(PanelState[[1]][[10]]==c(T,F,F,F))) selectivity='logistic'; AM("Assuming logistic selectivity according to MERA question F11")
  
  condition="catch2"
  if(grepl("E",code))  condition<-"effort"; AM("Conditioning on effort")
  
  #saveRDS(datS,"C:/temp/datS.rda") # 
  #saveRDS(OM,"C:/temp/OM.rda")      #
  #saveRDS(code,"C:/temp/code.rda") # 
  
  out<-RCM(OM,datS,
           ESS = rep(input$ESS,2),
           LWT = list(CAA=input$Wt_comp,CAL=input$Wt_comp),
           max_F = input$max_F,
           C_eq=input$C_eq_val,
           selectivity=selectivity,
           condition=condition,
           cores = ncpus,
           mean_fit=TRUE,
           control=list(eval.max=1E4, iter.max=1E4, abs.tol=1e-6))
  
  out
  
}




DataStrip2<-function(dat,OM,code,simno=1){
  
  # OM<-testOM; code="C_I"; simno=1; dat<-new('Data',"C:/Users/tcar_/Dropbox/MSC Data Limited Methods Project - Japan/MERA_Japan_workshop/ys_flounder2_TC/Yellow_striped_flounder2.csv")
 
  # Add effort to the data template
  datS<-dat

  # If there is no effort data in the imported dat file - file this with the effort from the OM
  if(sum(is.na(dat@Effort))>1){
    AM("Effort data not reported in imported data file - using effort sketched in MERA question F5")
    datS@Effort <- OM@cpars$Find
    
  }  
  
  datTypes<-c("C","E","A","L","M")
  slotnams<-c("Cat","Effort","CAA","CAL","ML")
  nD<-length(datTypes)
  
  for(i in 1:nD) if(!(grepl(datTypes[i],code)))slot(datS,slotnams[i])[]<-NA
  
  if(!(grepl("I",code))){
    datS@Ind[]<-NA
    datS@Abun[]<-NA
    datS@SpAbun[]<-NA
    datS@SpInd[]<-NA
    datS@VInd[]<-NA
  }
  
  datS
  
}

getCodes<-function(dat,maxtest=6){
  
  codes<-Detect_scope(dat)                            # what scoping methods are possible?
  ord<-order(codes,decreasing = F)
  codes<-codes[ord]
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


