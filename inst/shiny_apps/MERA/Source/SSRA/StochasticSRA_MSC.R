
SRAsim<-function(OM,qmult=0.5,CAApatchy=0.4,Cpatchy=1,Ipatchy=0.4,MLpatchy=0.4,nCAA=100,nL=200,sigmaE=0.25,sigmaI=0.1){

  maxage<-OM@maxage
  nyears<-OM@nyears

  M<-mean(OM@M)          # Natural mortality rate
  h<-mean(OM@h)          # Steepness
  Linf<-mean(OM@Linf)
  K<-mean(OM@K)         # Maximum growth rate
  t0<-mean(OM@t0)       # Theorectical length at age zero

  lenM <- mean(OM@L50)
  L50_90<-mean(OM@L50_95)
  len95 <- lenM+L50_90

  ageM <- -((log(1-lenM/Linf))/K) + t0 # calculate ageM from L50 and growth parameters (non-time-varying)
  age95 <- -((log(1-len95/Linf))/K) + t0

  Mat_age <- 1/(1 + exp(-log(19) * (((1:maxage) - ageM)/(age95 - ageM))))

  Len_age<-Linf*(1-exp(-K*((1:maxage)-t0)))
  Wt_age<-OM@a*Len_age^OM@b
  M_age<-rep(M,maxage)

  sel<-Mat_age
  eff<-(sin((nyears:1)/18)+1)*exp(rnorm(nyears,1,sigmaE))
  apFM<-eff/mean(eff)*qmult*M

  N<-CAA<-array(NA,c(nyears,maxage))
  Chist<-SSB<-ML<-rep(NA,nyears)

  FM<-array(rep(apFM,maxage)*rep(sel,each=nyears),c(nyears,maxage))
  Z<-array(FM+rep(M_age,each=nyears),c(nyears,maxage))

  R0<-10000
  sigma<-mean(OM@Perr)
  Recdevs<-trlnorm(nyears-1+maxage,1,sigma)
  Recdevs<-Recdevs/mean(Recdevs)

  N0<-R0*exp(-M*(0:(maxage-1)))
  SSB0<-sum(N0*Mat_age*Wt_age)
  SSBpR<-SSB0/R0

  N1<-N0*Recdevs[maxage:1]

  for(y in 1:nyears){

    if(y==1){
      N[1,]<-N1
    }else{
      N[y,]<-N[y-1,]
    }
    SSB[y]<-sum(N[y,]*Mat_age*Wt_age)

    CAA[y,]<-N[y,]*exp(-M_age/2)*(1-exp(-FM[y,]))
    ML[y]<-mean(sample(rep(Len_age,CAA[y,]),nL))
    Chist[y]<-sum(CAA[y,]*Wt_age)

    N[y,]<-N[y,]*exp(-Z[y,])
    N[y,2:maxage]<-N[y,1:(maxage-1)]
    N[y,1]<-Recdevs[maxage+y]*(0.8 * R0 * h * SSB[y])/
      (0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB[y])

  }

  for(y in 1:nyears)CAA[y,]<-rmultinom(1,nCAA,CAA[y,])

  CAA[sample(1:nyears,size=nyears*(1-Cpatchy),replace=F),]<-NA

  par(mfrow=c(4,2),mai=c(0.8,0.8,0.05,0.05))
  plot(SSB,type="l",xlab="Year",ylim=c(0,max(SSB)))
  plot(Chist,type="l",xlab="Year",ylim=c(0,max(Chist)))

  plot(apFM,type="l",xlab="Year",ylab="ApicalF")
  plot(sel,type="l",xlab="Age",ylab="Selectivity")

  plot((-maxage+2):nyears,Recdevs,xlab="Year")
  abline(v=0.5,col='blue')
  plot(c(1,nyears),c(1,maxage),col='white',xlab="Year",ylab="Age")
  legend("top",legend="Catch composition data",bty='n')
  points(rep(1:nyears,maxage),rep(1:maxage,each=nyears),cex=CAA^0.5/max(CAA^0.5,na.rm=T)*2.5)

  Ind<-trlnorm(nyears,1,sigmaI)*(SSB/SSB[1])
  Ind[sample(1:nyears,size=nyears*(1-Ipatchy),replace=F)]<-NA
  Ind<-Ind/mean(Ind,na.rm=T)
  ML[sample(1:nyears,size=nyears*(1-MLpatchy),replace=F)]<-NA

  plot(1:nyears,Ind,xlab="Year",ylim=c(0,max(Ind,na.rm=T)))
  plot(1:nyears,ML,xlab="Year")

  return(list(Chist=Chist,Recdevs=Recdevs,CAA=CAA,Ind=Ind,ML=ML,N=N,SSB=SSB,FM=FM,M=M,SSB0=SSB0,sel=sel))

}



StochasticSRA_MSC<-function(OM,CAA,Chist,Ind=NA,ML=NA,CAL=NA,mulen=NA,wts=c(1,1,0.5,0.1,1),
                        Jump_fac=1,nits=4000, burnin=500,thin=10,ESS=300,MLsd=0.1,
                        ploty=T,nplot=6,SRAdir=NA,shiny=T){

  OM <- updateMSE(OM) # Check that all required slots in OM object contain values

  nyears<-length(Chist)
  if(class(Chist)=="matrix")nyears<-nrow(Chist)
  maxage<-OM@maxage

  if(length(Ind)==1){
    Ind<-rep(NA,nyears)
  }else{
    if(sum(is.na(Ind))<nyears)Ind<-Ind/mean(Ind,na.rm=T) # normalize Ind to mean 1
  }
  if(length(ML)==1)ML<-rep(NA,nyears)


  Umax<-1-exp(-OM@maxF) # get SRA umax from OM
  Imiss<-is.na(Ind)     # which SSB index observations are missing?
  proyears<-OM@proyears
  nsim<-OM@nsim

  if(!is.na(dat@CV_Cat[1])){
     Cobs=rep(dat@CV_Cat[1],nsim)  # use the OM obs error for index
  }else{
     Cobs<-runif(nsim,OM@Cobs[1],OM@Cobs[2]) # sample observation error
  }

  if(!is.na(dat@CV_Ind[1])){
    Iobs=rep(dat@CV_Ind[1],nsim)  # use the OM obs error for index
  }else{
    Iobs<-runif(nsim,OM@Iobs[1],OM@Iobs[2]) # sample observation error
  }

  if (OM@nyears != nyears) {
    message("OM@nyears being updated to length Chist: ", nyears)
    OM@nyears <- nyears
  }

  if(sum(is.na(Chist))>0){
    message("One or more of the historical annual catch observations is missing. Linear interpolation has been used to fill these data")
    Chistold<-Chist
    Chist<-approx(Chist)$y
    cond<-!is.na(Chistold)
    Chist[(1:nyears)[cond]]<-Chistold[(1:nyears)[cond]]
    print(data.frame("Catches entered" = Chistold, "Catches interpolated"=Chist))
  }

  if(length(as.vector(CAA))==1){
    CAAswitch=F # don't do CAA calcs
    CAALH<-0 # likelihood contribution is nil

  }else{
    if (dim(CAA)[1] != nyears) stop("Number of CAA rows (", dim(CAA)[1], ") does not equal nyears (", nyears, "). NAs are acceptable")

    if (dim(CAA)[2] != OM@maxage) {
      message("Number of CAA columns (", dim(CAA)[2], ") does not equal OM@maxage (",  OM@maxage, ")")
      message("Assuming no CAA for ages greater than ", dim(CAA)[2], ' and filling with 0s')
      addages <- OM@maxage-dim(CAA)[2]
      CAA2 <- matrix(0, nrow=nrow(CAA), ncol=addages)
      CAA <- cbind(CAA, CAA2)
    }
  }

  if(length(as.vector(CAL))==1){
    CALswitch=F # don't do CAL calcs
    CALLH<-0 # likelihood contribution is nil

  }else{
    if (dim(CAL)[1] != nyears) stop("Number of CAL rows (", dim(CAL)[1], ") does not equal nyears (", nyears, "). NAs are acceptable")

    if(is.na(mulen[1])) stop("You must specify the argument mulen, which is the mean length of each length bin (columns) of the CAL data")

    if (dim(CAL)[2] != length(mulen)) {
      stop("The argument mulen (the mean length of each length bin) should be of the same length as the number of columns of the CAL data")
    }
    issomething<-function(x)(sum(is.na(x))<ncol(CAL))&(sum(x,na.rm=T)>0)
    CALyrs<-(1:nrow(CAL))[apply(CAL,1,issomething)]
    CALswitch=T
  }

  if(!CALswitch&!CAAswitch)stop('You need at least some catch at age data (slot CAA) or catch at length data (CAL) to use Stochastic SRA')

  nlen<-length(mulen)

  if (burnin < 0.05*nits) burnin <- 0.05 * nits

  if("nsim"%in%slotNames(OM))nsim<-OM@nsim
  if("proyears"%in%slotNames(OM))proyears<-OM@proyears
  OM@nsim<-nsim
  OM@proyears<-proyears

  # Sample custom parameters
  SampCpars <- list() # empty list
  # custom parameters exist - sample and write to list
  if(length(OM@cpars)>0){
    # ncparsim<-cparscheck(OM@cpars)   # check each list object has the same length and if not stop and error report
    SampCpars <- SampleCpars(OM@cpars, nsim)
  }

  # Sample Stock Parameters
  options(warn=-1)
  StockPars <- SampleStockPars(OM, nsim, nyears, proyears, SampCpars)
  options(warn=1)

  # Assign Stock pars to function environment
  for (X in 1:length(StockPars)) assign(names(StockPars)[X], StockPars[[X]])
  agearr<-array(rep(1:maxage,each=nsim),c(nsim,maxage))
  Wt_age <- Wt_age[,,nyears] # no time-varying growth
  Mat_age<- Mat_age[,,nyears]
  Len_age<-Len_age[,,nyears]


  # iALK script =================================================
  if(CALswitch){
    lvar<-runif(nsim,OM@LenCV[1],OM@LenCV[2])
    iALK<-array(NA,c(nsim,maxage,nlen))
    ind<-as.matrix(expand.grid(1:nsim,1:maxage,1:nlen))
    Lind<-ind[,c(1,2)]
    iALK[ind]<-dnorm(mulen[ind[,3]],Len_age[Lind],lvar[ind[,1]]*Len_age[Lind])
    sums<-apply(iALK,1:2,sum)
    sind<-ind[,1:2]
    iALK<-iALK/sums[sind]
    #contour(x=1:maxage,y=1:nlen,iALK[3,,],nlevels=10)
  }
  # Sample Fleet Parameters
  options(warn=-1)
  FleetPars <- SampleFleetPars(SubOM(OM, "Fleet"), Stock=StockPars, nsim,
                               nyears, proyears, cpars=SampCpars)
  options(warn=1)
  # Assign Fleet pars to function environment
  for (X in 1:length(FleetPars)) assign(names(FleetPars)[X], FleetPars[[X]])

  # Sampled arrays
  Chist_a<-array(trlnorm(nyears*nsim,1,Cobs)*rep(Chist,each=nsim),c(nsim,nyears)) # Historical catch

  # set up mcmc
  lnR0<-lninfl<-lnslp<-array(NA,c(nsim,nits))
  lnRD<-array(0,c(nsim,nyears+maxage,nits))

  LHD<-array(NA,c(nsim,nits))

  # if(sfIsRunning())sfExport(list=c("Chist_a"))

  if(sfIsRunning()){
    R0LB<-sfSapply(1:nsim,LSRA,FF=M*4,Chist_arr=Chist_a,M=M,Mat_age=Mat_age,Wt_age=Wt_age,
                   sel=Mat_age,Recdevs=array(1,c(nsim,nyears+maxage)),h=hs)

    R0UB<-sfSapply(1:nsim,LSRA,FF=M/10,Chist_arr=Chist_a,M=M,Mat_age=Mat_age,Wt_age=Wt_age,
                   sel=Mat_age,Recdevs=array(1,c(nsim,nyears+maxage)),h=hs)
  }else{

    R0LB<-sapply(1:nsim,LSRA,FF=M*4,Chist_arr=Chist_a,M=M,Mat_age=Mat_age,Wt_age=Wt_age,
                 sel=Mat_age,Recdevs=array(1,c(nsim,nyears+maxage)),h=hs)

    R0UB<-sapply(1:nsim,LSRA,FF=M/10,Chist_arr=Chist_a,M=M,Mat_age=Mat_age,Wt_age=Wt_age,
                 sel=Mat_age,Recdevs=array(1,c(nsim,nyears+maxage)),h=hs)

  }

  R0b=cbind(R0LB-1,R0UB+1)
  inflb<-log(c(0.5,maxage*0.5))
  slpb<-log(exp(inflb)*c(0.1,2))#c(-3,3)
  RDb<-c(-2,2)

  # initial guesses
  lnR0[,1]<-R0UB#log(apply(Chist_a,1,mean))
  lninfl[,1]<-log(maxage/4)
  lnslp[,1]<-log(exp(lninfl[,1])*0.2)
  lnRD[,,1]<-0

  # parameter vector
  pars<-c(lnR0[,1],lninfl[,1],lnslp[,1],lnRD[,,1])
  npars<-length(pars)

  # parameter store
  LHstr<-array(NA,c(nsim,nits))
  parstr<-array(NA,c(npars,nits))

  # parameter indexes
  R0ind<-1:nsim
  inflind<-(1*nsim)+(1:nsim)
  slpind<-(2*nsim)+(1:nsim)
  RDind<-(3*nsim+1):length(pars)

  # Parameter jumping
  JumpCV<-rep(0.05,npars) # R0
  JumpCV[inflind]<-0.05
  JumpCV[slpind]<-0.05
  JumpCV[RDind]<-0.1*mean(procsd) # a function of sigmaR to provide reasonable acceptance rate
  JumpCV<-JumpCV*Jump_fac

  # parameter censorship
  parLB<-parUB<-rep(NA,length(pars))

  parLB[R0ind]<-R0b[,1]
  parLB[inflind]<-inflb[1]
  parLB[slpind]<-slpb[1]
  parLB[RDind]<-RDb[1]

  parUB[R0ind]<-R0b[,2]
  parUB[inflind]<-inflb[2]
  parUB[slpind]<-slpb[2]
  parUB[RDind]<-RDb[2]

  CAAadj=sum(CAA,na.rm=T)/ESS # ESS adjustment to low sample sizes
  CALadj=sum(CAL,na.rm=T)/ESS # ESS adjustment to low sample sizes

  update<-(1:50)*(nits/50)
  adapt<-c(rep(5,100),rep(2.5,100),rep(1,nits-200))

  CAA_pred<-array(NA,c(nsim,nyears,maxage))

  if(CALswitch){
    CAL_pred<-array(NA,c(nsim,nyears,nlen))
    CALtemp<-array(NA,c(nsim,maxage,nlen))
    CAAind<-cbind(ind[,1],rep(1,nsim*maxage),ind[,2]) # sim, year, age
  }

  PredF<-MLpred<-SSB<-array(NA,c(nsim,nyears))

  upprog<-floor((1:20)*(nits/20))
  for(i in 1:nits){

    if(i %in% update){
      cat(".")
      flush.console()
    }

    #i<-i+1# debugging

    Reject<-rep(FALSE,nsim)

    nupars<-rnorm(npars,pars,JumpCV*adapt[i])
    nupars[nupars<parLB]<-parLB[nupars<parLB]
    nupars[nupars>parUB]<-parUB[nupars>parUB]

    if(i==1)nupars=pars

    R0<-exp(nupars[R0ind])
    infl<-exp(nupars[inflind])
    #infl<-(0.05+(infl/(1+infl))*0.45)*maxage
    slp<-exp(nupars[slpind])
    #slp<-(0.02+(slp/(1+slp))*0.98)*infl
    RD<-exp(array(nupars[RDind],c(nsim,nyears+maxage)))
    RD<-RD/apply(RD,1,mean)
    sel<-1/(1+exp((infl-(agearr))/slp))

    # calcs (getting pen as a zero or a 1)

    N<-R0*exp(-M*(agearr-1))
    SSB0<-apply(N*Mat_age*Wt_age,1,sum)
    SSBpR<-SSB0/R0

    CAA_pred[]<-NA
    if(CALswitch)CAL_pred[]<-0

    PredF[]<-NA
    MLpred[]<-NA
    SSB[]<-NA

    for(y in 1:nyears){  # M - F - aging / recruitment

      if(y==1)N<-N*RD[,maxage:1]

      SSB[,y]<-apply(N*Mat_age*Wt_age,1,sum)

      PredN<-N*exp(-M/2)
      PredVN<-PredN*sel
      CAA_pred[,y,]<-PredVN/apply(PredVN,1,sum)

      if(CALswitch){
        if(y%in%CALyrs){
          CAAind[,2]<-y
          CALtemp[ind]<-iALK[ind]*CAA_pred[CAAind]
          CAL_pred[,y,]<-CAL_pred[,y,]+apply(CALtemp,c(1,3),sum)
        }
      }

      MLpred[,y]<-apply(CAA_pred[,y,]*Len_age,1,sum)/apply(CAA_pred[,y,],1,sum)

      PredVW<-PredVN*Wt_age                   # Predicted vulnerable weight
      Predfrac<-PredVW/apply(PredVW,1,sum)    # Catch weight distribution over ages

      Cat<-(Chist_a[,y]*Predfrac)/Wt_age      # Guess at catch numbers by age
      predU<-Cat/PredN                        # Which means this harvest rate
      predU[!is.finite(predU)] <- Inf
      cond<-predU>Umax                        # Check max U
      Reject[apply(cond,1,sum)>0]<-TRUE       # Reject sims where U > Umax for any age class
      Cat[cond]<-Cat[cond]/(predU[cond]/Umax) # Set catch to Umax

      PredF[,y]<--log(1-apply(Cat/PredN,1,max)) # apical F
      N<-N*exp(-M)-Cat #PredF[,y]*sel)

      N[,2:maxage]<-N[,1:(maxage-1)] # aging
      N[,1]<-RD[,maxage+y]*(0.8*R0*hs*SSB[,y])/(0.2*SSBpR*R0*(1-hs)+(hs-0.2)*SSB[,y])
      N[N<0] <- tiny


    }

    Ipred<-SSB
    Ipred[matrix(rep(Imiss,each=nsim),nrow=nsim)]<-NA
    Ipred<-Ipred/apply(Ipred,1,mean,na.rm=T)
    Ires<-Ipred/matrix(rep(Ind,each=nsim),nrow=nsim)
    MLres<-MLpred/matrix(rep(ML,each=nsim),nrow=nsim)

    Ires[Ires<(1E-10)]<-(1E-10)
    Ires[Ires>1E10]<-1E10
    MLres[MLres<(1E-10)]<-(1E-10)
    MLres[MLres>1E10]<-1E10

    if(CAAswitch)CAA_pred[CAA_pred<1E-15]<-1E-15

    if(CALswitch){
      CAL_pred<-CAL_pred/array(apply(CAL_pred,1:2,sum),dim(CAL_pred))
      CAL_pred[CAL_pred<1E-15]<-1E-15
    }

    if(CAAswitch){
      CAALH<-apply(log(CAA_pred)*
                   array(rep(CAA,each=nsim)/CAAadj,c(nsim,nyears,maxage)),
                 1,sum,na.rm=T)
    }

    if(CALswitch){
      CALLH<-apply(log(CAL_pred[,CALyrs,])*
                     array(rep(CAL[CALyrs,],each=nsim)/CALadj,c(nsim,length(CALyrs),nlen)),
                   1,sum,na.rm=T)
    }

    RDLH<-apply(matrix(dnorm(nupars[RDind],-(procsd^2)/2,procsd,log=T),nrow=nsim),1,sum)
    ILH<-apply(dnorm(log(Ires),-(Iobs^2)/2,Iobs,log=T),1,sum,na.rm=T)
    MLLH<-apply(dnorm(log(MLres),-(MLsd^2)/2,MLsd,log=T),1,sum,na.rm=T)

    LH<-wts[1]*CAALH+wts[2]*RDLH+wts[3]*ILH+wts[4]*MLLH+wts[5]*CALLH

    # Reject / accept (cond)
    if(i > 1){
      Accept<-runif(nsim)<exp(LH-LHstr[,i-1])
      Accept[Reject]<-FALSE
      LHstr[,i]<-LHstr[,i-1]
      LHstr[Accept,i]<-LH[Accept]
      Aind<-rep(Accept,npars/nsim) # The correct index in the pars vector for the accepted simulations
      parstr[,i]<-parstr[,i-1]
      parstr[Aind,i]<-nupars[Aind]
      pars<-parstr[,i]
      # print(rbind(Reject,Accept))

    }else{
      parstr[,i]<-pars
      LHstr[,i]<-LH
    }

    if(shiny)if(i %in% upprog)incProgress(1/20, detail = round(i*100/nits))

  } # End of MCMC

  dep<-SSB[,nyears]/SSB0
  procsd<-apply(RD,1,sd,na.rm=T)
  procmu <- -0.5 * (procsd)^2  # adjusted log normal mean
  OM@D<-quantile(dep,c(0.05,0.95))
  OM@Perr<-quantile(procsd,c(0.025,0.975))

  getAC<-function(recdev)acf(recdev,plot=F)$acf[2,1,1]
  AC<-apply(RD,1,getAC)
  OM@AC<-quantile(AC,c(0.05,0.95))

  A5<--(slp*log(1/0.05-1)-infl)
  A5[A5<0]<-0.01
  A95<--(slp*log(1/0.95-1)-infl)
  L5<-Linf*(1-exp(-K*(A5-t0)))
  L95<-Linf*(1-exp(-K*(A95-t0)))

  OM@L5<-quantile(L5,c(0.05,0.95))
  OM@LFS<-quantile(L95,c(0.05,0.95))
  OM@nyears<-nyears
  OM@EffYears<-1:OM@nyears

  OM@EffLower<-apply(PredF,2,quantile,p=0.05)
  OM@EffUpper<-apply(PredF,2,quantile,p=0.95)
  OM@nyears<-nyears

  Perr<-array(NA,c(nsim,maxage+nyears+proyears-1))
  Perr[,1:(nyears+maxage-1)]<-log(RD[,2:(maxage+nyears)])
  Perr[,(nyears+maxage):(nyears+maxage+proyears-1)]<-matrix(rnorm(nsim*(proyears),rep(procmu,proyears),rep(procsd,proyears)),nrow=nsim)

  for (y in (maxage+nyears):(nyears + proyears+maxage-1)) Perr[, y] <- AC * Perr[, y - 1] +   Perr[, y] * (1 - AC * AC)^0.5
  Perr<-exp(Perr)

  PredF<-PredF/apply(PredF,1,mean) # Find should be mean 1 so qs optimizers are standardized

  Wt_age <- array(Wt_age, dim=c(dim=c(nsim, maxage, nyears+proyears)))
  Len_age <- array(Len_age, dim=c(nsim, maxage, nyears+proyears))
  Marray <- matrix(M, nrow=nsim, ncol=proyears+nyears)
  OM@cpars<-list(D=dep,M=M,procsd=procsd,AC=AC,hs=hs,Linf=Linf,
                 Wt_age=Wt_age, Len_age=Len_age, Marray=Marray,
                 K=K,t0=t0,L50=L50,
                 L5=L5,LFS=L95,Find=PredF,
                 V=array(sel,c(nsim,maxage,nyears)),Perr=Perr,R0=R0,
                 Iobs=apply(Ires,1,sd,na.rm=T),
                 SSB=SSB,SSB0=SSB0,RD=RD) # not valid for runMSE code but required
  #params<-
  list(OM=OM,SRAinfo=list(nits=nits,nsim=nsim,thin=thin,nyears=nyears,maxage=maxage,nages=maxage,parstr=parstr,burnin=burnin,SSB=SSB,
                PredF=PredF,RD=RD,CAA=CAA,CAL=CAL,CAA_pred=CAA_pred,CAL_pred=CAL_pred,Ind=Ind,mulen=mulen,
                CAAswitch=CAAswitch,CALswitch=CALswitch,sel=sel,SSB0=SSB0))
  #save(params,file="C:/temp/params")

}



#' Generic comparison plot for simulation testing of Stochastic SRA method
#'
#' @description Plots simulation variables versus estimation variables for Stochastic SRA methods of conditioning operating models.
#' @param simy The simulated time series
#' @param samy The matrix of estimated time series from of StochasticSRA() function.
#' @param xlab The x axis label for the plot
#' @param ylab The y axis label for the plot
#' @param maxplot The total number of individual simulations to be plotted in the first plot
#' @param type Should a line 'l' or points 'p' be plotted?
#' @return A plot
#' @author T. Carruthers (Canadian DFO grant)
#' @export compplot
#' @examples
#' nyears<-100
#' nsims<-200
#' simy<-sin(seq(0,2,length.out=nyears))
#' samy<-array(rep(simy,each=nsims)*rnorm(nsims,1,0.2)*rnorm(nsims*nyears,1,0.1),c(nsims,nyears))
#' par(mfrow=c(1,2))
#' compplot(simy,samy,xlab="Year",ylab="Some time varying parameter")
compplot<-function(simy,samy,xlab="",ylab="",maxplot=10,type="l"){

  col<-rep(c("blue","red","green","orange","grey","brown","pink","yellow","dark red","dark blue","dark green"),100)

  nsim<-dim(samy)[1]
  xs<-dim(samy)[2]
  qq<-apply(samy,2,quantile,p=c(0.05,0.25,0.5,0.75,0.95))
  ylim<-c(0,max(simy,qq))

  plot(simy,ylim=ylim,type=type,xlab=xlab,ylab=ylab)
  for(i in 1:min(nsim,maxplot))lines(samy[i,],col=col[i])
  if(type=="l")lines(simy,lwd=3)
  if(type=="p")points(simy,pch=19)

  plot(simy,ylim=ylim,type='l',xlab=xlab,ylab=ylab)
  polygon(c(1:xs,xs:1),c(qq[1,],qq[5,xs:1]),border=NA,col='light grey')
  polygon(c(1:xs,xs:1),c(qq[2,],qq[4,xs:1]),border=NA,col='dark grey')
  lines(qq[3,],lwd=1,col="white")

  if(type=="l")lines(simy,lwd=3)
  if(type=="p")points(simy,pch=19)

}


#' Plot simulation test of Stochastic SRA method
#'
#' @description Plots simulation variables versus estimation variables for Stochastic SRA methods of conditioning operating models.
#' @param sim The output list object of SRAsim() function.
#' @param OM The output object of StochasticSRA() function.
#' @param outfile The name of the figure (something.jpg) you wish to make using SRAcomp
#' @param maxplot The maximum number of simulations to plot
#' @author T. Carruthers (Canadian DFO grant)
#' @export SRAcomp
#' @examples
#' \dontrun{
#' sim<-SRAsim(testOM,qmult=1,patchy=0.8)
#' CAA<-sim$CAA
#' Chist<-sim$Chist
#' testOM<-StochasticSRA(testOM,CAA,Chist,nsim=30,nits=500)
#' SRAcomp(sim,testOM)
#' }
SRAcomp<-function(sim,OM,outfile=NA,maxplot=10){

  sam<-OM@cpars
  if(!is.na(outfile))jpeg(outfile,width=7,height=9,units='in',res=400)

  nsim<-dim(sam$SSB)[1]
  nyears<-dim(sam$SSB)[2]
  maxage<- dim(sim$CAA)[2]

  PEest<-"Perr"%in%names(sam)
  if(PEest)par(mfrow=c(4,2),mai=c(0.5,0.7,0.05,0.05))
  if(!PEest)par(mfrow=c(3,2),mai=c(0.5,0.7,0.05,0.05))


  # SSB
  compplot(sim$SSB,sam$SSB,xlab="Year",ylab="SSB",maxplot=maxplot)

  # Depletion
  compplot(sim$SSB/sim$SSB0,sam$SSB/sam$SSB0,xlab="Year",ylab="Depletion",maxplot=maxplot)

  # Recdevs
  if(PEest)compplot(sim$Recdevs,sam$RD,xlab="Year",ylab="log recruitment",type="l",maxplot=maxplot)

  # Selectivity
  compplot(sim$sel,sam$V[,,1],xlab="Age",ylab="Selectivity",type="l",maxplot=maxplot)

  legend('bottomright',legend=c("Simulated","Estimated 90% PI","Estimated 50% PI"),bty='n',text.col=c("black","grey","dark grey"),text.font=rep(2,3))

  if(!is.na(outfile))dev.off()

}




#' Estimates R0 using SRA to match current F estimates and avoid penalities for low stock sizes
#'
#' @param x a position in the various arrays and vectors that corresponds with a simulation (for use with sapply)
#' @param FF a vector of recent fishign mortality rates (apical Fs)
#' @param Chist_arr a vector of historical catch observations `[nyears]`
#' @param M a vector of natural mortality rates `[nsim]`
#' @param Mat_age a matrix of maturity at age `[nsim x nage]`
#' @param Wt_age a matrix of weight at age `[nsim x nage]`
#' @param sel a matrix of selectivity at age `[nsim x nage]`
#' @param Recdevs a matrix of recruitment deviations `[nsim x nyears]`
#' @param h a vector of steepness values of the Bev-Holt Stock-Recruitment relationship
#' @return all package data objects are placed in the global namespace \code{dir}
#' @export LSRA
#' @author T. Carruthers
LSRA<-function(x,FF,Chist_arr,M,Mat_age,Wt_age,sel,Recdevs,h){

  maxage<-ncol(Mat_age)

  SSB0guess<-sum(Chist_arr[x,])*c(0.05,100)
  SSBpR<-sum(exp(-M[x]*(0:(maxage-1)))*Mat_age[x,]*Wt_age[x,])
  R0range=SSB0guess/SSBpR

  # Modes 1:obj  2:Fpred  3:depletion  4:R0   5:Ffit
  opt<-optimize(LSRA_opt,interval=log(R0range),
                FF_a=FF[x],
                Chist=Chist_arr[x,],
                M_a=M[x],
                Mat_age_a=Mat_age[x,],
                Wt_age_a=Wt_age[x,],
                sel_a=sel[x,],
                Recdevs_a=Recdevs[x,],
                h_a=h[x])

  opt$minimum
}


#' Alternative version of LSRA that's a wrapper for LSRA_opt to return the right type of output (mode) using sapply
#'
#' @param x a position in the various arrays and vectors that corresponds with a simulation (for use with sapply)
#' @param lnR0s a vector nsim long that are estimated R0 values
#' @param FF a vector of recent fishign mortality rates (apical Fs)
#' @param Chist a vector of historical catch observations `[nyears]`
#' @param M a vector of natural mortality rates `[nsim]`
#' @param Mat_age a matrix of maturity at age `[nsim x nage]`
#' @param Wt_age a matrix of weight at age `[nsim x nage]`
#' @param sel a matrix of selectivity at age `[nsim x nage]`
#' @param Recdevs a matrix of recruitment deviations `[nsim x nyears]`
#' @param h a vector of steepness values of the Bev-Holt Stock-Recruitment relationship
#' @param mode optimization or plotting
#' @return all package data objects are placed in the global namespace \code{dir}
#' @export LSRA2
#' @author T. Carruthers
LSRA2<-function(x,lnR0s,FF,Chist,M,Mat_age,Wt_age,sel,Recdevs,h,mode=2){

  LSRA_opt(lnR0s[x], FF_a=FF[x], Chist=Chist[x,], M_a=M[x],
           Mat_age_a=Mat_age[x,],Wt_age_a=Wt_age[x,],
           sel_a=sel[x,],Recdevs_a=Recdevs[x,],h_a=h[x],mode=mode)

}


#' Internal estimation function for LSRA and LSRA2 functions
#'
#' @param param a numeric value representing log(R0)
#' @param FF_a numeric value, recent fishign mortality rate (apical F)
#' @param Chist a vector of historical catch observations `[nyears]`
#' @param M_a numeric value, natural mortality rate
#' @param Mat_age_a a vector of maturity at age `[nage]`
#' @param Wt_age_a a vector of weight at age `[nage]`
#' @param sel_a a vector of selectivity at age `[nage]`
#' @param Recdevs_a a vector of recruitment deviations `[nyears]`
#' @param h_a a numeric value of steepness values of the Bev-Holt Stock-Recruitment relationship
#' @param Umax maximum harvest rate per year
#' @param mode 1-5 see below
#' @return depends on mode but could be 1:objective function 2:trajectory of Fs 3: SSB depletion 4:log(R0) 5:diagnostic plots
#' @export LSRA_opt
#' @author T. Carruthers
LSRA_opt<-function(param,FF_a,Chist,M_a,Mat_age_a,Wt_age_a,sel_a,Recdevs_a,h_a,Umax=0.5,mode=1){

  nyears<-length(Chist)
  maxage<-length(Mat_age_a)
  R0<-exp(param)
  N<-R0*exp(-M_a*(0:(maxage-1)))
  Nstr<-array(NA,c(nyears,maxage))
  SSB0<-sum(N*Mat_age_a*Wt_age_a)
  SSBpR<-SSB0/R0
  pen<-0

  PredF<-SSB<-rep(NA,nyears)

  for(y in 1:nyears){

    SSB[y]<-sum(N*Mat_age_a*Wt_age_a)
    PredN<-N*exp(-M_a/2)
    PredVN<-PredN*sel_a
    PredVW<-PredVN*Wt_age_a
    Predfrac<-PredVW/sum(PredVW)
    Cat<-Chist[y]*Predfrac
    predU<-Cat/(PredN*Wt_age_a)
    cond<-predU>Umax

    if(sum(cond)>0){
      pen<-pen+sum(abs(predU[cond]-Umax)^2)
      Cat[cond]<-Cat[cond]/(predU[cond]/Umax)
    }

    PredF[y]<--log(1-max(Cat/(N*Wt_age_a)))
    N<-N*exp(-M_a-PredF[y]*sel_a)

    N[2:maxage]<-N[1:(maxage-1)] # aging
    N[1]<-Recdevs_a[y]*(0.8*R0*h_a*SSB[y])/(0.2*SSBpR*R0*(1-h_a)+(h_a-0.2)*SSB[y])
    Nstr[y,]<-N

  }

  mupredF<-mean(PredF[(nyears-15):(nyears-5)])

  if(mode==1){

    return(pen+(log(mupredF)-log(FF_a))^2)

  }else if(mode==2){

    return(PredF)

  }else if(mode==3){

    return(SSB/SSB0)

  }else if(mode==4){

    return(param)

  }else if(mode==5){

    return(SSB)

  }else{

    par(mfrow=c(2,1))
    plot(PredF)
    abline(h=FF_a,col="red")
    abline(h=mupredF,col="blue")
    plot(Chist)

  }
}








