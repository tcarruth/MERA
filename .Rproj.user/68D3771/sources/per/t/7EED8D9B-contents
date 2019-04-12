BMSYproj<-function(MSEobj,MSEobj_reb,options=list(),maxcol=6,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),maxrow=NA){
  
  MPcols<-rep('purple',MSEobj@nMPs)
  
  if(is.na(maxcol))maxcol=ceiling(length(MSEobj@MPs)/0.5) # defaults to portrait 1:2
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  yrs<-Current_Year+(1:MSEobj@proyears)
  
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

