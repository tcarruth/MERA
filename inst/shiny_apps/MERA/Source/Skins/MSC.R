
# ====================================================================================================================================
# ====  MSC Skin  ====================================================================================================================
# ====================================================================================================================================

# Reused Figure code

BMSYproj<-function(MSEobj,MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),vline=NA,fease=F){
  
  if(fease){
    MPcols=MPcols#FeaseLabs(MSEobj@MPs)$MPcols
  }else{
    MPcols<-rep('black',MSEobj@nMPs)
  }
  
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  
  if("YIU"%in%names(options)){
    yrs<-Current_Year+(1:MSEobj_reb@proyears)-options$YIU
  }else{
    yrs<-Current_Year+(1:MSEobj_reb@proyears)
  }
  
  nc<-maxcol
  nr<-ceiling(nMPs/nc)
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  B_BMSY<-MSEobj@SB_SBMSY
  Blims <- c(0,quantile(B_BMSY,0.95))

  for(i in 1:nMPs){
    plot(range(yrs),Blims,col="white",yaxs="i")
    plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
    mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
    
    if(i==1){
      Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0#MSEobj_reb@SB_SBMSY[,1,1]#
      legend('topleft',legend=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" ),bty='n')
    }
    if(!is.na(vline))abline(v=yrs[vline],lwd=2)
    if("YIU"%in%names(options))abline(v=yrs[options$YIU],lwd=2) #polygon(yrs[c(1,options$burnin,options$burnin,1)],c(-10,-10,10,10),col='lightgrey',border=NA)
    
  }
  
  mtext("B/BMSY",2,line=0.7,outer=T)
  mtext("Year",1,line=0.7,outer=T)
  
}

B0proj<-function(MSEobj,MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),vline=NA,fease=F){
  
  if(fease){
    MPcols=MPcols#FeaseLabs(MSEobj@MPs)$MPcols
  }else{
    MPcols<-rep('black',MSEobj@nMPs)
  }
  
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  
  if("YIU"%in%names(options)){
    yrs<-Current_Year+(1:MSEobj_reb@proyears)-options$YIU
  }else{
    yrs<-Current_Year+(1:MSEobj_reb@proyears)
  }
  
  nc<-maxcol
  nr<-ceiling(nMPs/nc)
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  B_B0<-MSEobj@SSB/MSEobj@OM$SSB0
  Blims <- c(0,quantile(B_B0,0.95))
  
  for(i in 1:nMPs){
    plot(range(yrs),Blims,col="white")
    plotquant(B_B0[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.2))
    mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
    
    if(i==1){
      Bdeps<-MSEobj@OM$D#MSEobj_reb@SB_SBMSY[,1,1]#
      legend('topleft',legend=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% unfished SSB" ),bty='n')
    }
    if(!is.na(vline))abline(v=yrs[vline],lwd=2)
    if("YIU"%in%names(options))abline(v=yrs[options$YIU],lwd=2) #polygon(yrs[c(1,options$burnin,options$burnin,1)],c(-10,-10,10,10),col='lightgrey',border=NA)
    
  }
  
  mtext("SSB / SSB0",2,line=0.7,outer=T)
  mtext("Year",1,line=0.7,outer=T)
  
}

plotquant<-function(x,p=c(0.05,0.25,0.75,0.95),yrs,qcol,lcol,addline=T,ablines=NA){
  #plot(range(yrs),Ylims,col="white")
  
  ny<-length(yrs)
  x[x==Inf]<-NA
  qs<-apply(x,2,quantile,p=p[c(1,4)],na.rm=T,type=3)
  qsi<-apply(x,2,quantile,p=p[2:3],na.rm=T,type=3)
  polygon(c(yrs,yrs[ny:1]),c(qs[1,],qs[2,ny:1]),border=NA,col='#b3ecff')
  
  polygon(c(yrs,yrs[ny:1]),c(qsi[1,],qsi[2,ny:1]),border=NA,col=qcol)
  if(!is.na(ablines[1]))abline(h=ablines,col='#99999980')
  
  if(addline)for(i in 1:2)lines(yrs,x[i,],col=lcol,lty=i)
  lines(yrs,apply(x,2,quantile,p=0.5,na.rm=T),lwd=2,col="white")
}

LT_HCR<-function(MSEobj, MSEobj_reb,options=list(),maxcol=6,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),vline=NA,fease=F){
  
  if(fease){
    MPcols=MPcols#FeaseLabs(MSEobj@MPs)$MPcols
  }else{
    MPcols<-rep('black',MSEobj@nMPs)
  }
  
  MPs<-MSEobj_reb@MPs
  nMPs<-length(MPs)
  if("YIU"%in%names(options)){
    yrs<-Current_Year+(1:MSEobj_reb@proyears)-options$YIU
  }else{
    yrs<-Current_Year+(1:MSEobj_reb@proyears)
  }
  nr<-ceiling(nMPs/maxcol)
  nc<-maxcol
  
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  B_BMSY<-MSEobj_reb@SB_SBMSY
  B_B0<-MSEobj_reb@SB_SBMSY*MSEobj_reb@OM$SSBMSY_SSB0 #<-MSEobj_reb@C/ array(rep(MSEobj_reb@C[,,1],MSEobj_reb@proyears),dim(MSEobj_reb@C))#MSEobj_reb@OM$RefY
  
  Blims <- c(0,quantile(B_BMSY,0.95))
  B2lims<- c(0,quantile(B_B0,0.95))
  
  for(i in 1:nMPs){
    
    plot(range(yrs),Blims,col="white")
    plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
    mtext(MSEobj_reb@MPs[i],3,line=0.2,font=2,col=MPcols[i])
    
    if(i==1){
      Bdeps<-MSEobj_reb@OM$D/MSEobj_reb@OM$SSBMSY_SSB0#MSEobj_reb@SB_SBMSY[,1,1]#
      legend('topleft',legend=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" ),bty='n')
    }
    
    if("YIU"%in%names(options))abline(v=yrs[options$YIU],lwd=2) #polygon(yrs[c(1,options$burnin,options$burnin,1)],c(-10,-10,10,10),col='lightgrey',border=NA)
    
    if(!is.na(vline))abline(v=yrs[vline])
  }
  
  mtext("B/BMSY",2,line=0.7,outer=T)
  mtext("Projection Year",1,line=0.7,outer=T)
  
}

ST_HCR<-function(MSEobj, MSEobj_reb,options=list(),maxcol=6,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),plotMGT=FALSE, fease=F){
  
  if(fease){
    MPcols=MPcols#FeaseLabs(MSEobj@MPs)$MPcols
  }else{
    MPcols<-rep('black',MSEobj@nMPs)
  }
  
  MPs<-MSEobj_reb@MPs
  nMPs<-length(MPs)
 
  if("YIU"%in%names(options)){
    yrs<-Current_Year+(1:20)-options$YIU
  }else{
    yrs<-Current_Year+(1:20)
  }
  nr<-ceiling(nMPs/maxcol)
  nc<-maxcol
  
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  B_BMSY<-MSEobj_reb@SB_SBMSY[,,1:20,drop=F]
  B_B0<-MSEobj_reb@SB_SBMSY*MSEobj_reb@OM$SSBMSY_SSB0#<-MSEobj_reb@C/ array(rep(MSEobj_reb@C[,,1],MSEobj_reb@proyears),dim(MSEobj_reb@C))#MSEobj_reb@OM$RefY
  
  Blims <- c(0,quantile(B_BMSY,0.95))
 
  for(i in 1:nMPs){
    
    plot(range(yrs),Blims,col="white")
     if(plotMGT){
      MGT2<-2* MSEobj_reb@OM$MGT
      polygon(yrs[1]+c(min(MGT2),max(MGT2),max(MGT2),min(MGT2)),c(-10,-10,10,10),col='lightgrey',border=NA)
      #legend('bottomright',legend="Two mean generation times",text.col='grey',bty='n')
     }
    
    plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
    mtext(MSEobj_reb@MPs[i],3,line=0.2,font=2,col=MPcols[i])
    
    if(i==1){
      Bdeps<-MSEobj_reb@OM$D/MSEobj_reb@OM$SSBMSY_SSB0#MSEobj_reb@SB_SBMSY[,1,1]#
      legend('topleft',legend=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" ),bty='n')
    }
    
    if("YIU"%in%names(options)) abline(v=yrs[options$YIU],lwd=2) #polygon(yrs[c(1,options$burnin,options$burnin,1)],c(-10,-10,10,10),col='lightgrey',border=NA)
    
  }
  
  mtext("B/BMSY",2,line=0.7,outer=T)
  mtext("Projection Year",1,line=0.7,outer=T)
  
}


CCU_plot<-function(MSEobj,MSEobj_reb,options=list(),maxrow=1,maxcol=3,fease=F){

  if(fease){
    MPcols=MPcols#FeaseLabs(MSEobj@MPs)$MPcols
  }else{
    MPcols<-rep('black',MSEobj@nMPs)
  }
  
  if(MSEobj@nsim>23){
  
    #          F2   F3           F4     F5      F6      F7        F8       F9      F10         F11        F12    F13    F14       F15      F16      F17     F18      F19 
    opt1<-  c("M",  "D", "hs",  "Ftype","Esd",  "qhssim", "qinc",  "L50",  "Sel50sim", "Vmaxlen", "DR",  "Fdisc", "procsd", "Ahsim", "Vhsim", "Asim", "Vsim", "initD",
              "TACFrac", "TACSD", "TAEFrac","TAESD", "SizeLimFrac","SizeLimSD","Cbias","betas","RefY")
    
    MSEtemp<-MSEobj
    MSEtemp@OM<-cbind(MSEtemp@OM,betas=MSEtemp@Obs$betas,MSEtemp@Misc[[4]])
    MSEtemp@OM<-MSEtemp@OM[,names(MSEtemp@OM)%in%opt1]
    VOIout<-VOI(MSEtemp,ncomp=15,nbins=6,plot=F)[[1]]
    
                
    qno<-   c("F2",       "F3",             "F4",       "F5",            "F6",         "F7",               "F8",                  "F9",                "F10",        "F11",      "F12",        "F13",              "F14",      "F15",            "F16",         "F17",         "F18",           "F19",
              "M2",       "M3",          "M4",         "M5",     "M6",               "M7", "D2",    "D3")
    qtext<- c("Longevity","Stock depletion","Steepness","Effort Pattern","Effort Var.","Hist. catchability","Future catchability","Length at Maturity","Selectivity","Dome Sel.","Discard rate","Post. Rel. Mort.","Rec. Var.","Hist. MPA size", "Hist. Mixing", "Future MPA", "Future Mixing", "Initial Dep.",
              "TAC offset", "TAC Var.", "TAE offset", "TAE Var", "Size Lim. offset", "Size Lim. Var.", "Cat. Rep. Bias.","Hyperstability")
    nMPs<-MSEobj@nMPs
    
    nrow=ceiling(nMPs/maxcol)
    par(mfrow=c(max(maxrow,nrow),maxcol),mai=c(2.4,0.4,0.2,0.01),omi=c(0.3,0.3,0.05,0.01))
    
    for(i in 1:MSEobj@nMPs){
      
      MP<-MSEobj@MPs[i]
      dat<-VOIout[match(MP,VOIout[,1])+0:1,2:16]
      lab1<-qno[match(as.factor(unlist(dat[1,])),opt1)]
      dat2<-aggregate(as.numeric(as.character(unlist(dat[2,]))),by=list(lab1),max)
      dat2<-dat2[order(dat2$x,decreasing=T),]
      labs<-paste(qno,qtext,sep=" - ")
      
      barplot(dat2[,2],names.arg=labs[match(dat2[,1],qno)], las=2,col=rgb(0.4,0.8,0.95),border=NA,cex.axis=1.4,cex.names=1.3)
      mtext(MP,3,adj=0.8,font=2,cex=1,col=MPcols[i])
      
    }
  
    mtext("Question / operating model characteristic",1,outer=T,line=0.5)
    mtext("Variability in Long Term Yield (% LTY)",2,outer=T,line=0.5)
  }else{
    plot(1,axes=F,col="white",xlab="",ylab="")
    legend('center',legend='< Requires at least 24 simulations >',bty="n",text.col="dark grey")
  }  
  
}


VOI_plot<-function(MSEobj,MSEobj_reb,options=list(),maxcol=6,fease=F){
  
  if(fease){
    MPcols=MPcols#FeaseLabs(MSEobj@MPs)$MPcols
  }else{
    MPcols<-rep('black',MSEobj@nMPs)
  }
  Obsnam<-c("Abias",       "Aerr",        "betas",       "BMSY_B0bias", "Brefbias",    "CAA_ESS",     "CAL_ESS",    "Cbias",      
            "Crefbias",    "Csd",         "Dbias" ,      "Derr",        "FMSY_Mbias",  "hbias",       "Irefbias",    "Isd",         "Kbias",       "lenMbias",   
            "LFCbias",     "LFSbias",     "Linfbias",    "Mbias",       "Recsd",       "t0bias")
  
  MSEobj@Obs<-MSEobj@Obs[,names(MSEobj@Obs)%in%Obsnam]
  VOIout<-VOI(MSEobj,ncomp=12,nbins=8,plot=F)[[2]]
  
  Obstext<- c("Abs. biomass bias","Abs. biomass Err.","Hyperstability","BMSY_B0 bias","BMSY bias","CAA n samps","CAL n samps","Catch bias",
              "MSY bias","Catch Err.","Depletion bias","Depletion Err.","FMSY_M bias","Steepness bias", "Index Targ. bias", "Index Err.", "V.B. K bias", "Len. Mat. bias",
              "Small Sel. bias", "Large Sel. bias", "V.B. Linf bias", "M bias", "Recruit. err", "VB t0 bias")
  
  
  
  MPplot<-rep(F,MSEobj@nMPs)
  
  for(i in 1:MSEobj@nMPs){
    
    MP<-MSEobj@MPs[i]
   
    dat<-VOIout[match(MP,VOIout[,1])+0:1,2:13]
    ind<-dat[2,]!=""&!is.na(dat[2,])
    MPplot[i]<-sum(ind[1:length(ind)])>0
  }  
    
  nMPs<-sum(MPplot)+as.integer(sum(!MPplot)>0) # plus the list of prescriptive MPs
  nrow=ceiling(nMPs/maxcol)
  par(mfrow=c(nrow,maxcol),mai=c(1.4,0.4,0.2,0.01),omi=c(0.3,0.3,0.05,0.01))
 
  for(i in (1:MSEobj@nMPs)[MPplot]){

    MP<-MSEobj@MPs[i]
    dat<-VOIout[match(MP,VOIout[,1])+0:1,2:13]
    ind<-dat[2,]!=""&!is.na(dat[2,])
    dat<-as.matrix(dat[,ind],ncol=sum(ind))  
    Obsgot<-as.character(unlist(dat[1,]))
   
    lab1<-Obstext[match(Obsgot,Obsnam)]
    dat2<-aggregate(as.numeric(as.character(unlist(dat[2,]))),by=list(lab1),max)
    dat2<-dat2[order(dat2$x,decreasing=T),]
    labs<-dat2[,1]
    barplot(dat2[,2],names.arg=labs, las=2,col=rgb(0.4,0.8,0.95),border=NA,cex.axis=1.4,cex.names=1.3)
    mtext(MP,3,font=2,cex=1,col=MPcols[i])
    
  }
  if(sum(!MPplot)>0){
    plot(c(0,1),axes=F,xlab="",ylab="",main='',col='white')
    prescriptive<-MSEobj@MPs[!MPplot]
    legend('center',legend=prescriptive,title="'Zero data' MPs:",cex=1.3,bty='n')
  }
  mtext("Question / operating model characteristic",1,outer=T,line=0.5)
  mtext("Variability in Long Term Yield (% LTY)",2,outer=T,line=0.5)
  
}

Yproj<-function(MSEobj,MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),fease=F){
  
  if(fease){
    MPcols=MPcols#FeaseLabs(MSEobj@MPs)$MPcols
  }else{
    MPcols<-rep('black',MSEobj@nMPs)
  }
  
  maxcol=5
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  yrs<-Current_Year+(1:MSEobj@proyears)
  
  nc<-maxcol
  nr<-ceiling(nMPs/nc)
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  Yd<-MSEobj@Catch/ array(rep(MSEobj@Catch[,,1],MSEobj@proyears),dim(MSEobj@Catch))#MSEobj@OM$RefY
  #Yd[is.na(Yd)]<-0
  Yd[Yd==Inf]<-NA
  Yd[Yd==NaN]<-NA
  
  Ylims<- c(0,min(10,quantile(Yd,0.95,na.rm=T)))
  
  for(i in 1:nMPs){
    plot(range(yrs),Ylims,col="white")
    
    plotquant(Yd[,i,],p=quants,yrs,qcol,lcol,ablines=1)
    mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
  }
 
  mtext("Yield relative to today",2,line=0.7,outer=T)
  mtext("Projection Year",1,line=0.7,outer=T)
  
}

F_FMSYproj<-function(MSEobj,MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),fease=F){
  
  if(fease){
    MPcols=MPcols#FeaseLabs(MSEobj@MPs)$MPcols
  }else{
    MPcols<-rep('black',MSEobj@nMPs)
  }
  
  maxcol=5
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  yrs<-Current_Year+(1:MSEobj@proyears)
  
  nc<-maxcol
  nr<-ceiling(nMPs/nc)
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  F_FMSY<-MSEobj@F_FMSY
  Flims <- c(0,quantile(F_FMSY,0.95))
  
  for(i in 1:nMPs){
    plot(range(yrs),Flims,col="white")
    plotquant(F_FMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
    mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
   
  }
  mtext("F/FMSY",2,line=0.7,outer=T)
  mtext("Projection Year",1,line=0.7,outer=T)
  
}

PB100<<-function (MSEobj = NULL, Ref = 1, Yrs = -5) 
{
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Probability long-term biomass is greater than BMSY"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. B > ", Ref, " BMSY (Year ", 
                            Yrs[1],"-",Yrs[2],")")
  }
  else {
    PMobj@Caption <- paste0("Prob. B > BMSY (Years ", 
                            Yrs[1], ")")
  }
  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(PB100)<<-'PM'

PB50<<-function (MSEobj = NULL, Ref = 0.5, Yrs = -5) 
{
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Probability long-term biomass is greater than 50% BMSY"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. B > ", Ref, " 50% BMSY (Year ", 
                            Yrs[1],"-",Yrs[2],")")
  }
  else {
    PMobj@Caption <- paste0("Prob. B > 50% BMSY (Years ", 
                            Yrs[1], ")")
  }
  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(PB50)<<-'PM'

LTY2<<-function (MSEobj = NULL, Ref = 0.5, Yrs = -5) 
{
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Average Yield relative to Reference Yield (Years ", 
                       Yrs[1], "-", Yrs[2], ")")
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. Yield > ", Ref, 
                            " Ref. Yield (Years ", Yrs[1], "-", Yrs[2], 
                            ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. Yield > Ref. Yield (Years ", 
                            Yrs[1], "-", Yrs[2], ")")
  }
  RefYd <- array(MSEobj@OM$RefY, dim = dim(MSEobj@Catch[, , Yrs[1]:Yrs[2]]))
  PMobj@Stat <- MSEobj@Catch[, , Yrs[1]:Yrs[2]]/RefYd
  PMobj@Ref <- 0.5
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(LTY2)<<-'PM'

Tplot<-function(MSEobj, MSEobj_reb, controls=list()){
 
  TradePlot(MSEobj,PMlist=list('PB50', 'LTY2', 'PB100','LTY2'),fill=rgb(0.4,0.8,0.95),Show='plots') #TradePlotMERA
}


# generic functions


FeaseLabs<-function(MPs,dat=NULL){
  
  nMPs<-length(MPs) 
  
  if(is.null(FeaseMPs)){
    DFeasible<-MPs
  }else{
    DFeasible<-FeaseMPs # these are calculated when data are loaded
  }

  tempdat0<-MSEtool::SimulatedData
  tempdat0@Misc<-rep(list(0),2)
  tempdat0@AddInd<-array(NA,c(dim(tempdat0@Ind)[1],5,dim(tempdat0@Ind)[2]))
  for(i in 1:5)tempdat0@AddInd[,i,]<-tempdat0@Ind
  
  # TAC TAE Feasibility
  cond<-unlist(PanelState[[2]][1]) # cond=rep(T,4) cond=c(F,T,T,T)
  runMPs <- applyMP(tempdat0, MPs, reps = 2, nsims=1, silent=TRUE)
  recs <- runMPs[[1]]
  type <- matrix(0, nrow=length(MPs),ncol=4) # TAC TAE SL MPA
  
  for (mm in seq_along(recs)) {
    type[mm,1] <- as.integer(length(recs[[mm]]$TAC) > 0)
    type[mm,2] <- as.integer(length(recs[[mm]]$Effort)>0)
    type[mm,3] <- as.integer(length(recs[[mm]]$LR5)>0)
    type[mm,4] <- as.integer(!is.na(recs[[mm]]$Spatial[1,1]))
  }
  
  DFeasible<-unique(c(DFeasible,MPs[(type[,4]==1|type[,3]==1) & apply(type,1,sum)==1])) # Size limits and area closures might not need data
  
  totneeded<-apply(type,1,sum)
  speced<-matrix(rep(as.integer(cond),each=length(MPs)),nrow=length(MPs))
  MFeasible<-MPs[apply(speced*type,1,sum)==totneeded]
  
  MP_Type<-rep("TAC",length(MPs))
  MP_Type[type[,2]==1]<-"TAE"
  MP_Type[type[,3]==1]<-"SzLim"
  MP_Type[type[,4]==1]<-"MPA"
  MP_Type[totneeded>1]<-"Mixed"
  
  cols<-rep('black',length(MPs))
  cols[!MPs%in%MFeasible | !MPs%in%DFeasible]<-'red'
  
  feasible<-rep("",length(MPs))
  condD<-!MPs%in%DFeasible
  condM<-!MPs%in%MFeasible
  condDM<-condD&condM
  feasible[condD]<-"D"
  feasible[condM]<-"M"
  feasible[condDM]<-"D/M"
  
  # Rankings
  rnkscore<-rep(0,nMPs)
  rnkscore[cols=="red"]=rnkscore[cols=="red"]+1000
  ord<-order(rnkscore,decreasing = T)
  
  list(feasible=feasible,MPcols=cols,MPs=MPs,MP_Type=MP_Type,ord=ord)
  
}


plotInd<-function(MSEobj_Eval,dat,dat_ind,pCC=TRUE){
  
  styr=max(dat@Year)-min(dat@Year)+1
  PPD<-MSEobj_Eval@Misc$Data[[1]]
  
  # Standardization
  #PPD@Cat<-PPD@Cat/PPD@Cat[,styr]
  #PPD@Ind<-PPD@Ind/PPD@Ind[,styr]
  #PPD@ML<-PPD@ML/PPD@ML[,styr]
  
  tsd= c("Cat","Cat","Cat","Ind","Ind","ML")
  stat=c("slp","AAV","mu","slp","mu", "slp")
  res<-max(dat_ind@Year-max(dat@Year))
  datayears<-dim(dat_ind@Cat)[2]
  
  indPPD<-getinds(PPD,styr=styr,res=res,tsd=tsd,stat=stat)
  
  # Standardization
  #dat_ind@Cat<-dat_ind@Cat/dat_ind@Cat[,styr]
  #dat_ind@Ind<-dat_ind@Ind/dat_ind@Ind[,styr]
  #dat_ind@ML<-dat_ind@ML/dat_ind@ML[,styr]
  
  indData<-getinds(dat_ind,styr=styr,res=res,tsd=tsd,stat=stat)
  
  if(pCC)CC(indPPD,indData,pp=1,res=res)
  if(!pCC)plot_mdist(indPPD,indData,alpha=0.05)
  
}



# ============= Risk Assessment ==================

  Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')
  Fig_title <- Tab_title <- rep(list(""), 10)
  #for(i in 1:10)Fig_dim[[i]]<-function(dims)list(height=1,width=1)
  # These are the names of widgets and their values to display in this skin / mode
  #             years in projection,  year resolution of reporting  rounding of digits
  options<-list(res=5)
  
  Intro_title[[1]] <- "Introduction"
  Intro_text[[1]] <- "Status quo fishing effort and catches are projected to evaluate biological risk. 
                      Zero catch and FMSY fishing are also projected to frame performance."
  
  # --- Tables --- 
  Tab_title[[1]] <- "Table 1. Projected biomass relative 50% BMSY"
  Tab_text[[1]] <-"The probability that projected biomass exceeds 50% BMSY. Probabilities below 50% are colored red, those above 90% are colored green."
  
  Tabs[[1]]<-function(MSEobj,MSEobj_reb,options=list(res=5),rnd=1){
    
    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    ind<-1+(0:1000*options$res)
    ind<-ind[ind<=proyears]
    
    LRP<-round(apply(MSEobj@SB_SBMSY>0.5,2:3,mean)*100,rnd)[,ind]
    Tab1<-as.data.frame(cbind(c("Current effort", "Current catches", "FMSY fishing", "Zero fishing"),LRP),stringsAsFactors = F)
    for(i in 2:ncol(Tab1))Tab1[,i]<-as.numeric(Tab1[,i])
    colnams<-c("MP",ind+Current_Year)
    names(Tab1)<-colnams
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@SB_SBMSY[,1,1]#
    caption=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab1,caption=caption,
              extensions = 'Buttons',
              options=list(buttons = 
                             list('copy', list(
                               extend = 'collection',
                               buttons = c('csv', 'excel', 'pdf'),
                               text = 'Download'
                             )),
                           dom = 'Brti')
                           )%>%
      formatStyle(columns = 2:ncol(Tab1), valueColumns = 2:ncol(Tab1), color = styleInterval(c(50,90),c('red','orange','green')))
    
  }
  
  Tab_title[[2]] <- "Table 2. Projected biomass relative to BMSY"
  Tab_text[[2]] <-"The probability that projected biomass is above BMSY. Probabilities below 40% are colored red, those above 60% are colored green."
  
  Tabs[[2]]<-function(MSEobj,MSEobj_reb,options=list(res=5),rnd=1){
    
    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    ind<-1+(0:1000*options$res)
    ind<-ind[ind<=proyears]
    
    TRP<-round(apply(MSEobj@SB_SBMSY>1,2:3,mean)*100,rnd)[,ind]
    Tab1<-as.data.frame(cbind(c("Current effort", "Current catches", "FMSY fishing", "Zero fishing"),TRP),stringsAsFactors = F)
    for(i in 2:ncol(Tab1)) Tab1[,i]<-as.numeric(Tab1[,i])
    colnams<-c("MP",ind+Current_Year)
    names(Tab1)<-colnams
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@SB_SBMSY[,1,1]#
    caption=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab1,caption=caption,
              extensions = 'Buttons',
              options=list(buttons = 
                             list('copy', list(
                               extend = 'collection',
                               buttons = c('csv', 'excel', 'pdf'),
                               text = 'Download'
                             )),
                           dom = 'Brti'))%>%
      formatStyle(columns = 2:ncol(Tab1), valueColumns = 2:ncol(Tab1), color = styleInterval(c(40,60),c('red','orange','green')))
    
  }
  
  #Tab_title[[3]] <- Tab_title[[4]] <- Tab_title[[5]] <- Tab_title[[6]] <- Tab_title[[7]] <- Tab_title[[8]] <- Tab_title[[9]] <- "" # make extras empty

 
  
  
  Fig_title[[2]] <- "Figure 1. Risk Assessment. B/BMSY and Yield (relative to today) projection plots"
  Fig_text[[2]] <-  "Figure 1. Risk assessment text. Projections of biomass and yield relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points." 
  
  Figs[[2]]<-function(MSEobj,MSEobj_reb,options=list()){
    MSEobj@MPs<-c("Current effort", "Current catches", "FMSY fishing", "Zero fishing")
    BMSYproj(MSEobj,MSEobj,options,maxcol=4)
  } 
  Fig_dim[[2]]<-function(dims)list(height=400,width=1200)
  
  
  Risk_Assessment<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
                        Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)
  
  
  
  
# ============= Status Determination ==================
  
  Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')
  Fig_title <- Tab_title <- rep(list(""), 10)
  #for(i in 1:10)Fig_dim[[i]]<-function(dims)list(height=1,width=1)
 
  # These are the names of widgets and their values to display in this skin / mode
  #             years in projection,  year resolution of reporting  rounding of digits
  options<-list()
  
  Intro_title[[1]] <- "Introduction"
  Intro_text[[1]] <- "Status determination provides estimates of spawning stock biomass relative to asymptotic unfished conditions."
  
  # --- Tables --- 
  
  Tab_title[[1]] <- "Table 1. Depletion estimates (SSB relative to unfished)"
  Tab_text[[1]] <-"Quantiles of the depletion estimates of various methods. Method refers to a stochastic 
  stock reduction analysis fitted to various combinations of data types (C = Catch, I = Index, M = mean length, 
  A = Catch at age composition, L = Catch at length composition). 'Conv' is the fraction of runs that converged."
  
  Tabs[[1]]<-function(Status,options=list()){
    
    ncode<-length(Status$codes)
    qs<-matrix(NA,nrow=ncode,ncol=5)
    for(i in 1:ncode){
      if(length(Status$Est[[i]])>2){
        qs[i,]<-round(quantile(Status$Est[[i]]*100,c(0.025,0.05,0.5,0.95,0.975)),2)
        
      }else{
        qs[i,]<-NA
      }
    }
    
    conv<-round(sum(Status$Fit[[1]]@conv)/length(Status$Fit[[1]]@conv)*100,2)
    tab<-as.data.frame(matrix(c(Status$codes,qs,conv),nrow=1))
    names(tab)<-c("Method","2.5%","5%","Median","95%","97.5%","Conv %")
    datatable(tab,caption=paste("Stock status estimates (SSB relative to 'unfished') in",Lyear),
              extensions = 'Buttons',
              options=list(buttons = 
                             list('copy', list(
                               extend = 'collection',
                               buttons = c('csv', 'excel', 'pdf'),
                               text = 'Download'
                             )),
                           dom = 'Brti')
    )
    
  }
  
  Tab_title[[2]] <- "Table 2. Probability of SSB exceeding various fractions of SSB0"
  Tab_text[[2]] <-"Probability of spawning stock biomass (SSB) in the most recent year, exceeding various fractions of unfished spawnign biomass (SSB0). Method refers to a stochastic 
  stock reduction analysis fitted to various combinations of data types (C = Catch, I = Index, M = mean length, 
  A = Catch at age composition, L = Catch at length composition). 'Conv' is the fraction of runs that converged."
  
  
  Tabs[[2]]<-function(Status,options=list()){
    
    ncode<-length(Status$codes)
    
    ps<-c(sum(Status$Est[[1]]>0.1),sum(Status$Est[[1]]>0.2),sum(Status$Est[[1]]>0.3),sum(Status$Est[[1]]>0.4),sum(Status$Est[[1]]>0.5))/length(Status$Est[[1]])
    
    tab<-as.data.frame(matrix(c(Status$codes,round(ps,3)),nrow=1))
    names(tab)<-c("Method","P(SSB > 0.1 SSB0)","P(SSB > 0.2 SSB0)","P(SSB > 0.3 SSB0)","P(SSB > 0.4 SSB0)","P(SSB > 0.5 SSB0)")
    datatable(tab,caption=paste("Probability that SSB exceeds various fractions of SSB0 in ",Lyear),
              extensions = 'Buttons',
              options=list(buttons = 
                             list('copy', list(
                               extend = 'collection',
                               buttons = c('csv', 'excel', 'pdf'),
                               text = 'Download'
                             )),
                           dom = 'Brti')
    )
    
  }
  
  
  
  
  # --- Figures --- 
  Fig_title[[1]] <- "Figure 1. Estimated depletion (SSB relative to unfished) expressed as a percentage"
  Fig_text[[1]] <-"The median, interquartile range and 95% interval of estimated spawning stock depletion."
  
  Figs[[1]]<-function(Status,options=list()){
    
    keep<-unlist(lapply(Status$Est,length))>3
    Est<-Status$Est[keep]
    nEst<-sum(keep)
    
    cols<-rep(rgb(0.4,0.8,0.95),nEst)#c('darkgrey','lightgrey',rainbow(nEst-1))
    
    SDdat<-data.frame(y=unlist(Est),x=rep(Status$codes[keep],unlist(lapply(Est,length))))
    
    boxplot(y~x,SDdat,col=cols,xlab="Status Determination Method",yaxs='i',ylab=paste("Estimated Status (%, SSB relative to unfished) in ",Lyear))
    #legend('topright',legend=Status$codes[keep],text.col=cols,bty='n',cex=0.9)
    abline(h=seq(0.1,1,length.out=10),col="grey")
    boxplot(y~x,SDdat,col=cols,xlab=Status$codes,yaxs='n',
            ylab="Estimated Status (SSB relative to unfished)",add=T)
    
    
  }
  Fig_dim[[1]]<-function(dims)list(height=500,width=400)
  
  Fig_title[[2]] <- "Figure 2. Spawning stock depletion relative to equilibrium SSB in initial year "
  Fig_text[[2]] <-"Estimated spawning stock depletion trend. Plotted are the 90th (light blue), 50th (dark blue) and median estimates (white line)."
  
  Figs[[2]]<-function(Status,options=list()){
    
    ntot<-length(Status$Fit)
    keep<-unlist(lapply(Status$Est,length))>3 # keep estimates from one of the methods of estimation
    nmods<-sum(keep)
    keep_ind<-(1:ntot)[keep]
    nplots<-nmods+1 # add the overall mean plot
    cols<-c('darkgrey','lightgrey',rainbow(nmods-1))
    
    nc<-5
    nr<-ceiling(nplots/nc)
    
    procdeps_inst<-function(x){  # Instantaneous version
      t(sapply(1:length(x@Misc),function(X,listy)listy[[X]]$E[1:(length(listy[[X]]$E)-1)],listy=x@Misc)
        / sapply(1:length(x@Misc),function(X,listy)listy[[X]]$E0,listy=x@Misc))
    } 
    
    procdeps<-function(x){
      t(sapply(1:length(x@Misc),function(X,listy)listy[[X]]$E[1:(length(listy[[X]]$E)-1)],listy=x@Misc))/
        sapply(1:length(x@Misc),function(X,listy)listy[[X]]$E0[1],listy=x@Misc)
    } 
    
    deps<-lapply(Status$Fit,procdeps) # ntot matrices of depletion (nsim x nyears)
    
    getquants<-function(x)  apply(x,2,quantile,p=c(0.05,0.25,0.5,0.75,0.95),na.rm=T)
    Dqs<-lapply(deps,getquants)
    meds<-matrix(unlist(lapply(Dqs,function(x)x[3,])),ncol=ntot)[,keep,drop=F]
    ny<-nrow(meds)
    par(mfrow=c(1,1),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
    
    #plot(c(0,ny),c(0,1),col="white",yaxs='i',ylab="",xlab="")
    #abline(h=seq(0.1,1,length.out=10),col="light grey")
    #matplot(meds,type='l',col=cols,add=T,lty=1) 
    
    #legend('topright',legend=Status$codes[keep],text.col=cols,bty='n',cex=0.9)
    #mtext("Median trend, all methods",3,line=0.2,font=2)
    
    qplot<-function(mat,xlab=1:ny,main=""){ #qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4"
      
      plot(range(xlab),c(0,1),col="white",yaxs='i',ylab="",xlab="")
      abline(h=seq(0.1,1,length.out=10),col="light grey")
      
      polygon(c(xlab,xlab[length(xlab):1]),c(mat[1,],mat[5,ncol(mat):1]),col=rgb(0.4,0.8,0.95),border=rgb(0.4,0.8,0.95))
      polygon(c(xlab,xlab[length(xlab):1]),c(mat[2,],mat[4,ncol(mat):1]),col="dodgerblue4",border="dodgerblue4")
      lines(xlab,mat[3,],col='white',lwd=1)
      mtext(main,3,line=0.2,font=2)
      
    }
    
    for(i in keep_ind){
      
      qplot(Dqs[[i]],xlab=Syear:Lyear,main=Status$codes[[i]])
      
    }
    
    mtext("Year",1,line=0.5,outer=T)
    mtext("Stock Depletion (SSB relative to unfished)",2,line=0.5,outer=T)
    
  }
  Fig_dim[[2]]<-function(dims)list(height=500*ceiling(dims$nmeth/5),width=700)
  
  
  
  SD<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
           Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)
  
    
# ============= Planning =========================
  
  Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')
  Fig_title <- Tab_title <- rep(list(""), 10)
  #for(i in 1:10)Fig_dim[[i]]<-function(dims)list(height=1,width=1)
  # These are the names of widgets and their values to display in this skin / mode
  #             years in projection,  year resolution of reporting  rounding of digits
  options<-list(burnin=10,            res=1)
  
  Intro_title[[1]] <- "Introduction"
  Intro_text[[1]] <- "Planning mode projects multiple MPs to evaluate their absolute and relative performance with respect to target and limit reference points."
  
  # --- Tables --- 
  Tab_title[[1]] <- "Table 1. Projected biomass relative to the LRP"
  Tab_text[[1]] <-"The probability that projected biomass is above 50% BMSY. Probabilities of 50% or lower are shaded red. Probabilities over 90% are shaded green. "
  
  Tabs[[1]]<-function(MSEobj,MSEobj_reb,options=list(burnin=10,res=1),rnd=1){
    
    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    burnin<-options$burnin
    ind<-1+(0:1000*options$res)
    ind<-ind[ind<=min(burnin,proyears)]
    
    LRP<-round(apply(MSEobj@SB_SBMSY[,,1:burnin,drop=FALSE]>0.5,2:3,mean)*100,rnd)#[,ind]
    
    FT<<-FeaseLabs(MPs=MSEobj@MPs,dat=dat)
    MPcols<<-FT$MPcols  # just do FeaseLabs once or else this computationally costly code has to be reused
    
    if(MSEobj@nMPs==1){
      temp<-c(MSEobj@MPs, FT$MP_Type, FT$feasible, LRP)
      Tab1<-as.data.frame(matrix(temp,nrow=1),stringsAsFactors=F)
    }else{
      Tab1<-as.data.frame(cbind(MSEobj@MPs, FT$MP_Type, FT$feasible, LRP),stringsAsFactors = F)
    }
    for(i in 4:ncol(Tab1))Tab1[,i]<-as.numeric(Tab1[,i])
    colnams<-c("MP","MP type","Feasibility",ind+Current_Year)
    names(Tab1)<-colnams
    
    URLs <- sapply(Tab1$MP, MPurl) %>% unlist()
    MPwithurl <- !is.na(URLs) 
    Tab1$MP[MPwithurl] <- paste0("<a href='", URLs[MPwithurl]," ' target='_blank'>", Tab1$MP[MPwithurl],"</a>")
    
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@SB_SBMSY[,1,1]#
    caption=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab1,caption=caption, extensions = 'Buttons',class = 'display',rownames=FALSE,escape=FALSE,
                   options=list(buttons = 
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                  )),
                               dom = 'Blfrtip'
                               )
              )%>%
      formatStyle(columns = 4:ncol(Tab1), valueColumns = 4:ncol(Tab1), color = styleInterval(c(50,90),c('red','orange','green')))  %>%
      formatStyle(columns=1, valueColumns=3, color = styleEqual(c("","M","D"),c("black","red","red")))%>%
      formatStyle(columns=2, valueColumns=3, color = styleEqual(c("","M","D"),c("black","red","red")))%>%
      formatStyle(columns=3, valueColumns=3, color = styleEqual(c("","M","D"),c("black","red","red")))  
      
  }
  
  Tab_title[[2]] <- "Table 2. Projected biomass relative to the TRP"
  Tab_text[[2]] <-"The probability that projected biomass is above BMSY. Probabilities of 40% or lower are shaded red. Probabilities over 60% are shaded green."
  
  Tabs[[2]]<-function(MSEobj,MSEobj_reb,options=list(res=1),rnd=1){
    
    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    burnin<-options$burnin
    ind<-1+(0:1000*options$res)
    ind<-ind[ind<=min(burnin,proyears)]
    
    TRP<-round(apply(MSEobj@SB_SBMSY[,,1:burnin,drop=FALSE]>1,2:3,mean)*100,rnd)[,ind]
    #FT<-FeaseLabs(MPs=MSEobj@MPs,dat=NA)
    if(MSEobj@nMPs==1){
      temp<-c(MSEobj@MPs, FT$MP_Type, FT$feasible, TRP)
      Tab1<-as.data.frame(matrix(temp,nrow=1),stringsAsFactors=F)
    }else{
      Tab1<-as.data.frame(cbind(MSEobj@MPs, FT$MP_Type, FT$feasible, TRP),stringsAsFactors = F)
    }
    for(i in 4:ncol(Tab1))Tab1[,i]<-as.numeric(Tab1[,i])
    colnams<-c("MP","MP type","Feasibility",ind+Current_Year)
    names(Tab1)<-colnams
    
    URLs <- sapply(Tab1$MP, MPurl) %>% unlist()
    MPwithurl <- !is.na(URLs) 
    Tab1$MP[MPwithurl] <- paste0("<a href='", URLs[MPwithurl]," ' target='_blank'>", Tab1$MP[MPwithurl],"</a>")
    
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@SB_SBMSY[,1,1]#
    caption=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab1,caption=caption, extensions = 'Buttons',class = 'display',rownames=FALSE,escape=FALSE,
              options=list(buttons = 
                             list('copy', list(
                               extend = 'collection',
                               buttons = c('csv', 'excel', 'pdf'),
                               text = 'Download'
                             )),
                           dom = 'Blfrtip'
                      )
      )%>%
      formatStyle(columns = 4:ncol(Tab1), valueColumns = 4:ncol(Tab1), color = styleInterval(c(40,60),c('red','orange','green')))  %>%
      formatStyle(columns=1, valueColumns=3, color = styleEqual(c("","M","D"),c("black","red","red")))%>%
      formatStyle(columns=2, valueColumns=3, color = styleEqual(c("","M","D"),c("black","red","red")))%>%
      formatStyle(columns=3, valueColumns=3, color = styleEqual(c("","M","D"),c("black","red","red")))       
  }
  
  
  # --- Figures ---
  
  Fig_title[[2]] <- "Figure 1. Biomass projection relative to the Target and Limit Reference Points"
  Fig_text[[2]] <- "Projections of biomass and yield relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, 
                    the white solid line is the median and the dark blue lines are two example simulations.
                    Grey horizontal lines denote the Target (BMSY) and Limit (50% BMSY) Reference Points." 
  Figs[[2]]<-function(MSEobj,MSEobj_reb,options=list()) BMSYproj(MSEobj,MSEobj_reb,options,fease=T)
  Fig_dim[[2]]<-function(dims)list(height=ceiling(dims$nMPs/5)*250,width=1100)
  
  Fig_title[[3]] <- "Figure 2. Long-term HCR"
  Fig_text[[3]] <- "Projections of biomass relative to MSY and unfished (B0) levels given a starting depletion of half BMSY. The rebuilding analysis simulates the fishery currently in a depleted state even if the user-specified depletion in the operating model is higher.
  In these cases, the rebuilding analysis provides added assurance whether a particular management procedure would be likely to rebuild the stock if the user-specified depletion level is overly optimistic and in need of rebuilding.
  The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the limit and target reference points" 
  Figs[[3]]<-function(MSEobj,MSEobj_reb,options=list()) LT_HCR(MSEobj,MSEobj_reb,options,fease=T)
  Fig_dim[[3]]<-function(dims)list(height=ceiling(dims$nMPs/6)*250,width=1200)
  
  Fig_title[[4]] <- "Figure 3. Short-term HCR"
  Fig_text[[4]] <- "As Figure 2 but over the first 10 years." 
  Figs[[4]] <- function(MSEobj,MSEobj_reb,options=list()) ST_HCR(MSEobj,MSEobj_reb,options,fease=T)
  Fig_dim[[4]]<-function(dims)list(height=ceiling(dims$nMPs/6)*250,width=1200)
  
  Fig_title[[5]] <- "Figure 4. Evaluation of current uncertainties"
  Fig_text[[5]] <- "This figure identifies those questions across which there is the highest variability in long term yield (average yield over last 10 years of the projection). This figures identifies which elements of the questionnaire (Step A) are the most consequential uncertainties." 
  Figs[[5]] <- function(MSEobj,MSEobj_reb,options=list()) CCU_plot(MSEobj,MSEobj_reb,options,fease=T)
  Fig_dim[[5]]<-function(dims)list(height=ceiling(dims$nMPs/3)*350,width=1300)
  
  Fig_title[[6]] <- "Figure 5. Value of information"
  Fig_text[[6]] <- "This figure identifies the key observation uncertainties (biases and errors) in determing the long-term yield performance of MPs (average yield over last 10 years of the projection)." 
  Figs[[6]] <- function(MSEobj,MSEobj_reb,options=list()) VOI_plot(MSEobj,MSEobj_reb,options,fease=T)
  Fig_dim[[6]]<-function(dims)list(height=ceiling(dims$nMPs/6)*350,width=1300)
  
  Fig_title[[7]] <- "Figure 6. Yield projection"
  Fig_text[[7]] <- "Future yield as a fraction of current yield" 
  Figs[[7]] <- function(MSEobj,MSEobj_reb,options=list()) Yproj(MSEobj,MSEobj_reb,options,fease=T)
  Fig_dim[[7]]<-function(dims)list(height=ceiling(dims$nMPs/5)*350,width=1300)
  
  Fig_title[[8]] <- "Figure 7. Fishing mortality rate projection"
  Fig_text[[8]] <- "Future fishing mortality rate relative to FMSY" 
  Figs[[8]] <- function(MSEobj,MSEobj_reb,options=list()) F_FMSYproj(MSEobj,MSEobj_reb,options,fease=T)
  Fig_dim[[8]]<-function(dims)list(height=ceiling(dims$nMPs/5)*350,width=1300)
  
  Fig_title[[9]] <- "Figure 8. Yield - Biomass trade-offs"
  Fig_text[[9]] <- "Trade-off between yield and biomass risks" 
  Figs[[9]] <- function(MSEobj,MSEobj_reb,options=list()) Tplot(MSEobj,MSEobj_reb,options)
  Fig_dim[[9]]<-function(dims)list(height=650,width=1300)
  
 
  Planning<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
                 Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)
  
  
 

# ============= Evaluation =======================

  Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')
  Fig_title <- Tab_title <- rep(list(""), 10)
  #for(i in 1:10)Fig_dim[[i]]<-function(dims)list(height=1,width=1)
  # These are the names of widgets and their values to display in this skin / mode
  #             years in projection,  year resolution of reporting  rounding of digits
  options<-list()

  Intro_title[[1]] <- "Introduction"
  Intro_text[[1]] <- "A single MP is projected to infer future stock status and determine whether the data observed are consistent with those that were projected"
  
  
  # --- Tables --- 
  Tab_title[[1]] <- "Table 1. Biomass relative to 50% BMSY"
  Tab_text[[1]] <-"The biomass projection for the interim years that an MP has been in use."

  Tabs[[1]]<-function(MSEobj_Eval,dat,dat_ind,options=list(res=1),res=5,rnd=1){
    
    YIU<-length(dat_ind@Year)-length(dat@Year)
    nMPs<-MSEobj_Eval@nMPs
    proyears<-MSEobj_Eval@proyears
    ind<-1:min(YIU,proyears)
    
    LRP<-matrix(round(apply(MSEobj_Eval@SB_SBMSY[,,1:YIU,drop=FALSE]>0.5,2:3,mean)*100,rnd)[,ind],nrow=nMPs)
    Tab1<-as.data.frame(cbind(MSEobj_Eval@MPs,LRP))
   
    colnams<-c("MP",max(dat@Year)+(1:YIU))
    names(Tab1)<-colnams
    Tab1$MP<-as.character(Tab1$MP)
    
    URLs <- MPurl(as.character(Tab1$MP))
    MPwithurl <- !is.na(URLs) 
    Tab1$MP[MPwithurl] <- paste0("<a href='", URLs[MPwithurl]," ' target='_blank'>", Tab1$MP[MPwithurl],"</a>")
    
    
    Bdeps<-MSEobj_Eval@OM$D/MSEobj_Eval@OM$SSBMSY_SSB0 #MSEobj_reb@SB_SBMSY[,1,1]#
    caption=paste0("Simulations start between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab1,caption=caption,extensions = 'Buttons',class = 'display',rownames=FALSE,escape=FALSE,
              options=list(buttons = 
                             list('copy', list(
                               extend = 'collection',
                               buttons = c('csv', 'excel', 'pdf'),
                               text = 'Download'
                             )),
                           dom = 'Brti', 
                           ordering=F
              )
    )%>%
      formatStyle(columns = 2:ncol(Tab1), valueColumns = 2:ncol(Tab1), color = styleInterval(c(50,100),c('red','orange','green')))
  
  }
  
  Tab_title[[2]] <- "Table 2. Biomass relative to BMSY"
  Tab_text[[2]] <-"The biomass projection for the interim years that an MP has been in use."
  
  Tabs[[2]]<-function(MSEobj_Eval, dat,dat_ind,options=list(burnin=10,res=1),rnd=1){
    
    YIU<-length(dat_ind@Year)-length(dat@Year)
    nMPs<-MSEobj_Eval@nMPs
    proyears<-MSEobj_Eval@proyears
    ind<-1:min(YIU,proyears)
    
    TRP<-matrix(round(apply(MSEobj_Eval@SB_SBMSY[,,ind,drop=FALSE]>1,2:3,mean)*100,rnd)[,ind],nrow=nMPs)
    Tab2<-as.data.frame(cbind(MSEobj_Eval@MPs,TRP))
    colnams<-c("MP",max(dat@Year)+(1:YIU))
    names(Tab2)<-colnams
    Tab2$MP<-as.character(Tab2$MP)
    
    URLs <- sapply(Tab2$MP, MPurl) %>% unlist()
    MPwithurl <- !is.na(URLs) 
    Tab2$MP[MPwithurl] <- paste0("<a href='", URLs[MPwithurl]," ' target='_blank'>", Tab2$MP[MPwithurl],"</a>")
    
    Bdeps<-MSEobj_Eval@OM$D/MSEobj_Eval@OM$SSBMSY_SSB0 #MSEobj_reb@SB_SBMSY[,1,1]#
    caption=paste0("Simulations start between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab2,caption=caption, extensions = 'Buttons',class = 'display',rownames=FALSE,escape=FALSE,
                   options=list(buttons = 
                                  list('copy', list(
                                      extend = 'collection',
                                      buttons = c('csv', 'excel', 'pdf'),
                                      text = 'Download'
                                  )),
                                dom = 'Brti', 
                                ordering=F
                               )
    )%>%
      formatStyle(columns = 2:ncol(Tab2), valueColumns = 2:ncol(Tab2), color = styleInterval(c(25,50,100),c('red','orange','green','darkgreen')))
    
  }
  
  Tab_title[[3]] <- "Table 3. Spawning biomass relative to 20% of SSB unfished"
  Tab_text[[3]] <-"Probability of biomass exceeding 20% unfished levels in the years since MP adoption."
  
  Tabs[[3]]<-function(MSEobj_Eval,dat,dat_ind,options=list(burnin=10,res=1),rnd=1){
    
    YIU<-length(dat_ind@Year)-length(dat@Year)
    B_B0<-MSEobj_Eval@SSB/MSEobj_Eval@OM$SSB0
    nMPs<-MSEobj_Eval@nMPs
    proyears<-MSEobj_Eval@proyears
    ind<-1:min(YIU,proyears)
    RP<-matrix(round(apply(B_B0[,,ind,drop=F]>0.2,2:3,mean)*100,rnd),nrow=nMPs)
    Tab3<-as.data.frame(cbind(MSEobj_Eval@MPs,RP))
    colnams<-c("MP",max(dat@Year)+(1:YIU))
    names(Tab3)<-colnams
    Tab3$MP<-as.character(Tab3$MP)
    
    URLs <- sapply(Tab3$MP, MPurl) %>% unlist()
    MPwithurl <- !is.na(URLs) 
    Tab3$MP[MPwithurl] <- paste0("<a href='", URLs[MPwithurl]," ' target='_blank'>", Tab3$MP[MPwithurl],"</a>")
    
    Bdeps<-MSEobj_Eval@OM$D#MSEobj_reb@SB_SBMSY[,1,1]#
    caption=paste0("Simulations start between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% of unfished SSB" )
    datatable(Tab3,caption=caption,extensions = 'Buttons',class = 'display',rownames=FALSE,escape=FALSE,
                options=list(buttons = 
                               list('copy', list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel', 'pdf'),
                                 text = 'Download'
                               )),
                             dom = 'Brti', 
                             ordering=F
                )
      )%>%
      formatStyle(columns = 2:ncol(Tab3), valueColumns = 2:ncol(Tab3), color = styleInterval(c(25,50,100),c('red','orange','green','darkgreen')))
    
  }

  # --- Figures ---

  Fig_title[[2]] <- "Figure 1. Biomass projected since MP adoption"
  Fig_text[[2]] <- "Projections of biomass relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points. The bold black vertical line is the current year." 
  
  Figs[[2]]<-function(MSEobj_Eval,dat,dat_ind,options=list()) BMSYproj(MSEobj_Eval,MSEobj_Eval,options=list( YIU=length(dat_ind@Year)-length(dat@Year)),maxcol=1)
  Fig_dim[[2]] <- function(dims)list(height=420,width=600)
  
  Fig_title[[3]] <- "Figure 2. Biomass projected since MP adoption relative to unfished SSB"
  Fig_text[[3]] <- "Projections of biomass relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points. The bold black vertical line is the current year." 
  
  Figs[[3]]<-function(MSEobj_Eval,dat,dat_ind,options=list()) B0proj(MSEobj_Eval,MSEobj_Eval,options=list( YIU=length(dat_ind@Year)-length(dat@Year)),maxcol=1)
  Fig_dim[[3]] <- function(dims)list(height=420,width=600)
 
  Fig_title[[4]] <- "Figure 3. Posterior predicted data versus those observed"
  Fig_text[[4]] <- "The 'cloud' of posterior predicted data are represented by the grey shaded areas. Points are observed data since an MP has been in use and are color-coded according to their agreement with posterior predictions."
  
  Figs[[4]]<-  function(MSEobj_Eval,dat,dat_ind,options=list())post_marg_plot(MSEobj_Eval,dat,dat_ind,options=list())
  Fig_dim[[4]] <- function(dims)list(height=800,width=800)
 
  Evaluation<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
                   Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)
 

# ========== Build ============================= 
  
MSC<-list(Risk_Assessment=Risk_Assessment,SD=SD,Planning=Planning,Evaluation=Evaluation) 
None<-list()



