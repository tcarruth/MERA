
MSC_PMs<-function(MSEobj,MSEobj_reb,curyr=2018,MGTmult=2,MPcols=NA){

  if(is.na(MPcols[1]))MPcols=rep('black',MSEobj@nMPs)

  MPcols[MPcols=="green"]<-"darkgreen"

  layout(matrix(c(1,2,3,1,2,4,1,2,5),nrow=3),heights=c(1,1,0.7))

  par(mai=c(0.8,0.3,0.1,0.1),omi=c(0.001,0.4,0.3,0.01))
  yrs<-curyr+(1:MSEobj@proyears)

  Brel<-MSEobj@B_BMSY[,1,]*100
  ylim<-c(0,quantile(Brel,0.92))
  ylim[2]<-max(200,ylim[2])
  matplot(yrs,t(Brel),type='l',xlim=c(curyr+1,curyr+61),col=makeTransparent("#00ff00",60),lty=1,lwd=3,ylim=ylim,yaxs="i",xlab="",ylab="")


  MGTline=mean(MSEobj@OM$MGT)*2

  abline(v=curyr+1+c(burnin,MSEobj@proyears),lwd=3,lty=1,col="#99999980")
  #abline(v=curyr+1+MGTline,lwd=3,lty=1,col="#00ff0080")

  abline(h=c(50,100),lwd=3,lty=1,col="#99999980")
  yra<-array(rep(yrs,each=MSEobj@nsim),c(MSEobj@nsim,MSEobj@proyears))
  MGTa<-curyr+array(MGTmult*MSEobj@OM$MGT,dim=dim(yra))

  col1<-'blue'
  col2<-'red'
  col3<-'orange'

  cond1<-Brel>50 & yra < (curyr+burnin)+0.5
  cond2<-Brel>100 & yra < (curyr+burnin)+0.5
  points(yra[cond2],Brel[cond2],pch=19,cex=1.4,col=col2)
  points(yra[cond1],Brel[cond1],pch=3,cex=0.88,col=col1)

  cond4=Brel>50 & yra > (curyr+burnin)+0.5
  cond5<-Brel>100 & yra > (curyr+burnin)+0.5
  points(yra[cond5],Brel[cond5],cex=1.4,col=col2)
  points(yra[cond4],Brel[cond4],pch=4,cex=1,col=col1)


  text(curyr+burnin/2,10,paste0("Burn-in years (",burnin,"-",MSEobj@proyears,")"),col='dark grey',font=2,cex=1.2)

  text(curyr+47,10,"Year 50",col='dark grey',font=2,cex=1.2)

  P111a<-round(100*sum(cond1)/(burnin*MSEobj@nsim),0)
  P111b<-round(100*sum(cond2)/(burnin*MSEobj@nsim),0)

  P121a<-round(100*sum(cond4)/((MSEobj@proyears-burnin)*MSEobj@nsim),0)
  P121b<-round(100*sum(cond5)/((MSEobj@proyears-burnin)*MSEobj@nsim),0)


  legend('topright',legend=c(paste0("1.1.1a (",P111a,"%)"),
                             paste0("1.1.1b (",P111b,"%)"),
                             paste0("1.2.1a (",P121a,"%)"),
                             paste0("1.2.1b (",P121b,"%)")),
         col=c(col1,col2,col1,col2),text.col=c(col1,col2,col1,col2),bty='n',pch=c(3,19,4,1),text.font=2)

  #legend('right',legend="Specified OM", bty='n',text.font=2)

  mtext("Biomass relative to BMSY (%)",2,line=2.8)
  mtext("MSE Projection Year",1,line=2.8)

  Brel2<-MSEobj_reb@B_BMSY[,1,]*100
  matplot(yrs,t(Brel2),type='l',col=makeTransparent("#00ff00",60),lty=1,lwd=3,ylim=ylim,xlim=c(curyr+1,curyr+61),yaxs="i",ylab="",xlab="")
  cond3<-Brel2>100 & yra > MGTa+0.5 & yra < 1.5+MGTa
  cond3x<-Brel2<100 & yra > MGTa+0.5 & yra < 1.5+MGTa
  points(yra[cond3],Brel2[cond3],pch=17,cex=1.7,col=col3)
  points(yra[cond3x],Brel2[cond3x],pch=2,cex=1.7,col=col3)
  abline(h=100,lwd=3,lty=1,col="#99999980")
  P112<-round(100*sum(cond3)/MSEobj@nsim,0)
  text(curyr+MGTline+3,ylim[2]*0.95,"2MGT",col='orange',font=2,cex=1.2)
  legend('topright',legend=paste0("1.1.2 (",P112,"%)"),col=col3,text.col=col3,bty='n',pch=17,text.font=2)

  legend('right',legend="Rebuilding analysis (1.1.2)",bty='n',text.font=2)


  mtext("Biomass relative to BMSY (%)",2,line=2.8)
  mtext("MSE Projection Year",1,line=2.8)

  labline=0.3
  labcex=1

  xvar<-Brel[,1:burnin]
  xlim<-c(0,quantile(xvar,0.98))
  dens<-density(xvar,from=0)
  plot(dens,main="",xlab="",ylab="",xlim=xlim,col='green')
  abline(v=c(50,100),lwd=2,lty=1,col="#99999980")
  subdens<-getsegment(dens,50,lower=F)
  polygon(y=subdens$x,x=subdens$y,col="blue",border=NA)
  subdens<-getsegment(dens,100,lower=F)
  polygon(y=subdens$x,x=subdens$y,col="red",border=NA)
  lines(dens,col="green")
  mtext("Posterior density",2,line=2.8)
  legend('topright',legend=c("1.1.1a","1.1.1b"),text.col=c("blue","red"),bty='n',text.font=2)
  mtext(paste0("Burn-in years (",0,"-",burnin,")"),3,line=labline,cex=labcex,col='dark grey',font=2)

  xvar<-Brel[,(burnin+1):MSEobj@proyears]
  xlim<-c(0,quantile(xvar,0.98))
  dens<-density(xvar,from=0)
  plot(dens,main="",xlab="",ylab="",xlim=xlim,col='green')
  abline(v=c(50,100),lwd=2,lty=1,col="#99999980")
  subdens<-getsegment(dens,50,lower=F)
  polygon(y=subdens$x,x=subdens$y,col="blue",border=NA)
  subdens<-getsegment(dens,100,lower=F)
  polygon(y=subdens$x,x=subdens$y,col="red",border=NA)
  legend('topright',legend=c("1.2.1a","1.2.1b"),text.col=c("blue","red"),bty='n',text.font=2)
  mtext(paste0("Years ",burnin,"-",MSEobj@proyears),3,line=labline,cex=labcex,col='dark grey',font=2)
  lines(dens,col="green")
  mtext("Biomass relative to BMSY",1,line=2.8)
  mtext(MSEobj@MPs[1],3,line=0.3,col=MPcols[1],outer=T,font=2)

  cond3b<-yra > MGTa+0.5 & yra < 1.5+MGTa
  xvar<-Brel2[cond3b]
  xlim<-c(0,quantile(xvar,0.98))
  dens<-density(xvar,from=0)
  plot(dens,main="",xlab="",ylab="",xlim=xlim,col='green')
  abline(v=100,lwd=2,lty=1,col="#99999980")
  subdens<-getsegment(dens,100,lower=F)
  polygon(y=subdens$x,x=subdens$y,col="orange",border=NA)
  lines(dens,col='green')

  legend('topright',legend="1.1.2",text.col="orange",bty='n',text.font=2)
  mtext("After 2MGT",3,line=labline,cex=labcex,col='dark grey',font=2)
}

plotquant<-function(x,p=c(0.05,0.25,0.75,0.95),yrs,qcol,lcol,addline=T,ablines=NA){
  ny<-length(yrs)
  qs<-apply(x,2,quantile,p=p[c(1,4)])
  qsi<-apply(x,2,quantile,p=p[2:3])
  polygon(c(yrs,yrs[ny:1]),c(qs[1,],qs[2,ny:1]),border=NA,col='#b3ecff')

  polygon(c(yrs,yrs[ny:1]),c(qsi[1,],qsi[2,ny:1]),border=NA,col=qcol)
  if(!is.na(ablines[1]))abline(h=ablines,col='#99999980')

  if(addline)for(i in 1:2)lines(yrs,x[i,],col=lcol,lty=i)
  lines(yrs,apply(x,2,quantile,p=0.5),lwd=2,col="white")
}

Pplot3<-function(MSEobj,maxcol=6,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",curyr=2018,quants=c(0.05,0.25,0.75,0.95),MPcols,maxrow=NA){

  if(is.na(maxcol))maxcol=ceiling(length(MSEobj@MPs)/0.5) # defaults to portrait 1:2
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  yrs<-curyr+(1:MSEobj@proyears)

  MPcols[MPcols=="green"]<-"darkgreen"

  plots<-split(1:nMPs, ceiling(seq_along(1:nMPs)/maxcol))

  nr<-length(plots)*2
  if(!is.na(maxrow))nr=max(nr,maxrow)
  nc<-maxcol

  mat<-array(0,c(nc,nr*1.5))
  ind<-floor(0.5+(1:nr)*1.5)
  mat[,ind]<-1:(nr*nc)
  mat<-t(mat)
  ht<-rep(0.2,nr*1.5)
  ht[ind]<-1
  layout(mat,heights=ht)
  par(mai=c(0.3,0.3,0.01,0.01),omi=c(0.5,0.5,0.05,0.05))

  B_BMSY<-MSEobj@B_BMSY
  Yd<-MSEobj@C/ array(rep(MSEobj@C[,,1],MSEobj@proyears),dim(MSEobj@C))#MSEobj@OM$RefY
  Yd[is.na(Yd)]<-0

  Blims <- c(0,quantile(B_BMSY,0.95))
  Ylims<- c(0,quantile(Yd,0.95))

  for(pp in 1:length(plots)){

    toplot<-unlist(plots[pp])
    nt<-length(toplot)

    for(i in toplot){
      plot(range(yrs),Blims,col="white")
      plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
      mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
      if(i==toplot[1])mtext("B/BMSY",2,line=2.3)
    }
    if(nt<maxcol)for(i in 1:(maxcol-nt))plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel",axes=F)

    for(i in toplot){
      plot(range(yrs),Ylims,col="white")
      plotquant(Yd[,i,],p=quants,yrs,qcol,lcol,ablines=1)
      if(i==toplot[1])mtext("Yield relative to today",2,line=2.3)
    }
    if(nt<maxcol)for(i in 1:(maxcol-nt))plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel",axes=F)

  }

  mtext("Projection Year",1,line=0.7,outer=T)

}



Pplot4<-function(MSEobj,maxcol=6,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",curyr=2018,quants=c(0.05,0.25,0.75,0.95),MPcols,maxrow=NA){

  if(is.na(maxcol))maxcol=ceiling(length(MSEobj@MPs)/0.5) # defaults to portrait 1:2
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  yrs<-curyr+(1:MSEobj@proyears)

  MPcols[MPcols=="green"]<-"darkgreen"

  plots<-split(1:nMPs, ceiling(seq_along(1:nMPs)/maxcol))

  nr<-length(plots)*2
  if(!is.na(maxrow))nr=max(nr,maxrow)
  nc<-maxcol

  mat<-array(0,c(nc,nr*1.5))
  ind<-floor(0.5+(1:nr)*1.5)
  mat[,ind]<-1:(nr*nc)
  mat<-t(mat)
  ht<-rep(0.2,nr*1.5)
  ht[ind]<-1
  layout(mat,heights=ht)
  par(mai=c(0.3,0.3,0.01,0.01),omi=c(0.5,0.5,0.05,0.05))

  F_FMSY<-MSEobj@F_FMSY
  Yd<-MSEobj@C/MSEobj@OM$RefY
  Yd[is.na(Yd)]<-0

  Flims <- c(0,quantile(F_FMSY,0.95))
  Ylims<- c(0,quantile(Yd,0.95))

  for(pp in 1:length(plots)){

    toplot<-unlist(plots[pp])
    nt<-length(toplot)

    for(i in toplot){
      plot(range(yrs),Flims,col="white")
      plotquant(F_FMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
      mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
      if(i==toplot[1])mtext("F/FMSY",2,line=2.3)
    }
    if(nt<maxcol)for(i in 1:(maxcol-nt))plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel",axes=F)

    for(i in toplot){
      plot(range(yrs),Ylims,col="white")
      plotquant(Yd[,i,],p=quants,yrs,qcol,lcol,ablines=1)
      if(i==toplot[1])mtext("Yield relative to MSY",2,line=2.3)
    }
    if(nt<maxcol)for(i in 1:(maxcol-nt))plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel",axes=F)

  }

  mtext("Projection Year",1,line=0.7,outer=T)

}


Rplot<-function(MSEobj,maxcol=6,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",curyr=2018,quants=c(0.05,0.25,0.75,0.95),MPcols,maxrow=NA){

  if(is.na(maxcol))maxcol=ceiling(length(MSEobj@MPs)/0.5) # defaults to portrait 1:2
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  yrs<-curyr+(1:MSEobj@proyears)

  MPcols[MPcols=="green"]<-"darkgreen"

  plots<-split(1:nMPs, ceiling(seq_along(1:nMPs)/maxcol))

  nr<-length(plots)*2
  if(!is.na(maxrow))nr=max(nr,maxrow)
  nc<-maxcol

  mat<-array(0,c(nc,nr*1.5))
  ind<-floor(0.5+(1:nr)*1.5)
  mat[,ind]<-1:(nr*nc)
  mat<-t(mat)
  ht<-rep(0.2,nr*1.5)
  ht[ind]<-1
  layout(mat,heights=ht)
  par(mai=c(0.3,0.3,0.01,0.01),omi=c(0.5,0.5,0.05,0.05))

  B_BMSY<-MSEobj@B_BMSY
  B_B0<-MSEobj@B_BMSY*MSEobj@OM$SSBMSY_SSB0#<-MSEobj@C/ array(rep(MSEobj@C[,,1],MSEobj@proyears),dim(MSEobj@C))#MSEobj@OM$RefY

  Blims <- c(0,quantile(B_BMSY,0.95))
  B2lims<- c(0,quantile(B_B0,0.95))


  for(pp in 1:length(plots)){

    toplot<-unlist(plots[pp])
    nt<-length(toplot)

    for(i in toplot){
      plot(range(yrs),Blims,col="white")
      plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
      mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
      if(i==toplot[1])mtext("B/BMSY",2,line=2.3)
    }
    if(nt<maxcol)for(i in 1:(maxcol-nt))plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel",axes=F)

    for(i in toplot){
      plot(range(yrs),B2lims,col="white")
      plotquant(B_B0[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.4,1))
      if(i==toplot[1])mtext("B/B0",2,line=2.3)
    }
    if(nt<maxcol)for(i in 1:(maxcol-nt))plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel",axes=F)

  }

  mtext("Projection Year",1,line=0.7,outer=T)

}

