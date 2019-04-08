P1_LTY_plot<<-function(MSEobj,burnin,MPcols){

  Eyr<-10
  rnd<-0
  #MPcols[MPcols=='green']<-'darkgreen'
  #MPcols[feasible!=""]<-makeTransparent(MPcols[feasible!=""],70)
  PI.111.a<-round(apply(MSEobj@B_BMSY[,,1:burnin]>0.5,2,mean)*100,rnd)
  #refY<-sum(MSEobj@C[,1,11:50])
  LTY<-apply(MSEobj@C[,,11:50],2,sum)
  refY<-max(LTY,na.rm=T)
  LTY<-round(LTY/refY*100,rnd)
  MP<-MSEobj@MPs
  par(mai=c(0.8,0.8,0.1,0.1))
  ylim<-c(0,max(LTY))
  plot(c(-10,110),ylim,col='white',xlab="",ylab="")
  mtext(paste0("Prob. Biomass > 0.5 BMSY, yrs 1-",burnin," (PI.1.1.1a)"),1,line=2.5,cex=1.2)
  mtext("Long term yield",2,line=2.5,cex=1.2)
  abline(v=c(0,100),col="#99999950")
  abline(h=c(0,100),col="#99999950")

  text(PI.111.a,LTY,MSEobj@MPs,col=MPcols,cex=1.2)

}


P1b_LTY_plot<<-function(MSEobj,burnin,MPcols){

  Eyr<-10
  rnd<-0
  #MPcols[MPcols=='green']<-'darkgreen'
  #MPcols[feasible!=""]<-makeTransparent(MPcols[feasible!=""],70)
  PI.111.b<-round(apply(MSEobj@B_BMSY[,,1:burnin]>1,2,mean)*100,rnd)
  #refY<-sum(MSEobj@C[,1,11:50])
  LTY<-apply(MSEobj@C[,,11:50],2,sum)
  refY<-max(LTY,na.rm=T)
  LTY<-round(LTY/refY*100,rnd)
  MP<-MSEobj@MPs
  par(mai=c(0.8,0.8,0.1,0.1))
  ylim<-c(0,max(LTY))
  plot(c(-10,110),ylim,col='white',xlab="",ylab="")
  mtext(paste0("Prob. Biomass > BMSY, yrs 1-",burnin," (PI.1.1.1b)"),1,line=2.5,cex=1.2)
  mtext("Long term yield",2,line=2.5,cex=1.2)
  abline(v=c(0,100),col="#99999950")
  abline(h=c(0,100),col="#99999950")

  text(PI.111.b,LTY,MSEobj@MPs,col=MPcols,cex=1.2)

}

P2_LTY_plot<<-function(MSEobj,MPcols){

  rnd<-0
  MPcols[MPcols=='green']<-'darkgreen'
  PI.121.a<-round(apply(MSEobj@B_BMSY[,,11:50]>0.5,2,mean)*100,rnd)
  LTY<-apply(MSEobj@C[,,11:50],2,sum)
  refY<-max(LTY,na.rm=T)
  LTY<-round(LTY/refY*100,rnd)
  MP<-MSEobj@MPs
  par(mai=c(0.8,0.8,0.1,0.1))
  ylim<-c(0,max(LTY))
  plot(c(-10,110),ylim,col='white',xlab="",ylab="")
  mtext("Prob. Biomass > 0.5 BMSY, yrs 11-50 (PI.1.2.1a)",1,line=2.5,cex=1.2)
  mtext("Long term yield",2,line=2.5,cex=1.2)
  abline(v=c(0,100),col="#99999950")
  abline(h=c(0,100),col="#99999950")

  text(PI.121.a,LTY,MSEobj@MPs,col=MPcols,cex=1.2)

}

P3_LTY_plot<<-function(MSEobj,MSEobj_reb,MPcols){

  rnd<-4
  MPcols[MPcols=='green']<-'darkgreen'
  MGT2<-ceiling(MSEobj@OM$MGT*2)
  MGT2[MGT2<5]<-5
  MGT2[MGT2>20]<-20

  Bind<-cbind(as.matrix(expand.grid(1:MSEobj@nsim,1:MSEobj@nMPs)),rep(MGT2,MSEobj@nMPs))
  Bmat<-array(MSEobj_reb@B_BMSY[Bind],c(MSEobj_reb@nsim,MSEobj_reb@nMPs))
  PI.112<-round(apply(Bmat>1,2,mean)*100,rnd)

  LTY<-apply(MSEobj@C[,,11:50],2,sum)
  refY<-max(LTY,na.rm=T)
  LTY<-round(LTY/refY*100,rnd)
  MP<-MSEobj@MPs
  par(mai=c(0.8,0.8,0.1,0.1))
  ylim<-c(0,max(LTY))
  plot(c(-10,110),ylim,col='white',xlab="",ylab="")
  mtext("Prob. Rebuilding to BMSY over 2MGT (PI.1.1.2)",1,line=2.5,cex=1.2)
  mtext("Relative yield",2,line=2.5,cex=1.2)
  abline(v=c(0,100),col="#99999950")
  abline(h=c(0,100),col="#99999950")

  text(PI.112,LTY,MSEobj@MPs,col=MPcols,cex=1.2)

}
