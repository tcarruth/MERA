
LT_HCR<-function(MSEobj, MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),maxrow=NA){
  
  MPcols<-rep('purple',MSEobj_reb@nMPs)
  
  if(is.na(maxcol))maxcol=ceiling(length(MSEobj_reb@MPs)/0.5) # defaults to portrait 1:2
  MPs<-MSEobj_reb@MPs
  nMPs<-length(MPs)
  yrs<-Current_Year+(1:MSEobj_reb@proyears)
  
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
  
  B_BMSY<-MSEobj_reb@B_BMSY
  B_B0<-MSEobj_reb@B_BMSY*MSEobj_reb@OM$SSBMSY_SSB0#<-MSEobj_reb@C/ array(rep(MSEobj_reb@C[,,1],MSEobj_reb@proyears),dim(MSEobj_reb@C))#MSEobj_reb@OM$RefY
  
  Blims <- c(0,quantile(B_BMSY,0.95))
  B2lims<- c(0,quantile(B_B0,0.95))
  
  for(pp in 1:length(plots)){
    
    toplot<-unlist(plots[pp])
    nt<-length(toplot)
    
    for(i in toplot){
      
      plot(range(yrs),Blims,col="white")
      plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
      mtext(MSEobj_reb@MPs[i],3,line=0.2,font=2,col=MPcols[i])
      if(i==toplot[1])mtext("B/BMSY",2,line=2.3)
      if(pp==1&i==1){
        Bdeps<-MSEobj_reb@OM$D/MSEobj_reb@OM$SSBMSY_SSB0#MSEobj_reb@B_BMSY[,1,1]#
        legend('topleft',legend=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" ),bty='n')
      }
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


