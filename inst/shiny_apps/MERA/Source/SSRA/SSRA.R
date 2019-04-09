
# Stochastic SRA wrapper

SSRA_wrap<-function(OM,dat,simno=1){

  dat<-CALsimp(dat)
  CAA<-dat@CAA[simno,,]
  Chist<-dat@Cat[simno,]
  Ind<-dat@Ind[simno,]
  CAL=dat@CAL[simno,,]
  ML<-dat@ML[simno,]
  nl<-length(dat@CAL_bins)
  mulen<-(dat@CAL_bins[1:(nl-1)]+dat@CAL_bins[2:nl])/2

  StochasticSRA_MSC(OM,CAA,Chist,Ind,ML,CAL,mulen,wts=c(1,1,0.5,0.1,1),
                     Jump_fac=1,nits=1000, burnin=200,thin=10,ESS=300,MLsd=0.1,
                     ploty=F,nplot=6,SRAdir=NA)

}


