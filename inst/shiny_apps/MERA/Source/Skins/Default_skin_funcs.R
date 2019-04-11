
# ====================================================================================================================================
# ====  Skins  =======================================================================================================================
# ====================================================================================================================================




Skins<-list()

# ==== MSC =============================================================================

# Reused Table code





# Reused Figure code

BMSYproj<-function(MSEobj,MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),vline=NA){
  
  MPcols<-rep('purple',MSEobj@nMPs)
  
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  
  if("burnin"%in%names(options)){
    yrs<-Current_Year+(1:MSEobj_reb@proyears)-options$burnin
  }else{
    yrs<-Current_Year+(1:MSEobj_reb@proyears)
  }
  
  nc<-maxcol
  nr<-ceiling(nMPs/nc)
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  B_BMSY<-MSEobj@B_BMSY
  Blims <- c(0,quantile(B_BMSY,0.95))

  for(i in 1:nMPs){
    plot(range(yrs),Blims,col="white")
    plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
    mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
    
    if(i==1){
      Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0#MSEobj_reb@B_BMSY[,1,1]#
      legend('topleft',legend=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" ),bty='n')
    }
    if(!is.na(vline))abline(v=yrs[vline],lwd=2)
    if("burnin"%in%names(options))abline(v=yrs[options$burnin],lwd=2) #polygon(yrs[c(1,options$burnin,options$burnin,1)],c(-10,-10,10,10),col='lightgrey',border=NA)
    
  }
  
  
  mtext("B/BMSY",2,line=0.7,outer=T)
  mtext("Year",1,line=0.7,outer=T)
  
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

LT_HCR<-function(MSEobj, MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),vline=NA){
  
  MPcols<-rep('purple',MSEobj_reb@nMPs)
  
  MPs<-MSEobj_reb@MPs
  nMPs<-length(MPs)
  if("burnin"%in%names(options)){
    yrs<-Current_Year+(1:MSEobj_reb@proyears)-options$burnin
  }else{
    yrs<-Current_Year+(1:MSEobj_reb@proyears)
  }
  nr<-ceiling(nMPs/maxcol)
  nc<-maxcol
  
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  B_BMSY<-MSEobj_reb@B_BMSY
  B_B0<-MSEobj_reb@B_BMSY*MSEobj_reb@OM$SSBMSY_SSB0#<-MSEobj_reb@C/ array(rep(MSEobj_reb@C[,,1],MSEobj_reb@proyears),dim(MSEobj_reb@C))#MSEobj_reb@OM$RefY
  
  Blims <- c(0,quantile(B_BMSY,0.95))
  B2lims<- c(0,quantile(B_B0,0.95))
  
  for(i in 1:nMPs){
    
    plot(range(yrs),Blims,col="white")
    plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
    mtext(MSEobj_reb@MPs[i],3,line=0.2,font=2,col=MPcols[i])
    
    if(i==1){
      Bdeps<-MSEobj_reb@OM$D/MSEobj_reb@OM$SSBMSY_SSB0#MSEobj_reb@B_BMSY[,1,1]#
      legend('topleft',legend=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" ),bty='n')
    }
    
    if("burnin"%in%names(options))abline(v=yrs[options$burnin],lwd=2) #polygon(yrs[c(1,options$burnin,options$burnin,1)],c(-10,-10,10,10),col='lightgrey',border=NA)
    
    if(!is.na(vline))abline(v=yrs[vline])
  }
  
  mtext("B/BMSY",2,line=0.7,outer=T)
  mtext("Projection Year",1,line=0.7,outer=T)
  
}

ST_HCR<-function(MSEobj, MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),plotMGT=FALSE){
  
  MPcols<-rep('purple',MSEobj_reb@nMPs)
  
  MPs<-MSEobj_reb@MPs
  nMPs<-length(MPs)
 
  if("burnin"%in%names(options)){
    yrs<-Current_Year+(1:20)-options$burnin
  }else{
    yrs<-Current_Year+(1:20)
  }
  nr<-ceiling(nMPs/maxcol)
  nc<-maxcol
  
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  B_BMSY<-MSEobj_reb@B_BMSY[,,1:20,drop=F]
  B_B0<-MSEobj_reb@B_BMSY*MSEobj_reb@OM$SSBMSY_SSB0#<-MSEobj_reb@C/ array(rep(MSEobj_reb@C[,,1],MSEobj_reb@proyears),dim(MSEobj_reb@C))#MSEobj_reb@OM$RefY
  
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
      Bdeps<-MSEobj_reb@OM$D/MSEobj_reb@OM$SSBMSY_SSB0#MSEobj_reb@B_BMSY[,1,1]#
      legend('topleft',legend=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" ),bty='n')
    }
    
    if("burnin"%in%names(options)) abline(v=yrs[options$burnin],lwd=2) #polygon(yrs[c(1,options$burnin,options$burnin,1)],c(-10,-10,10,10),col='lightgrey',border=NA)
    
  }
  
  mtext("B/BMSY",2,line=0.7,outer=T)
  mtext("Projection Year",1,line=0.7,outer=T)
  
}


CCU_plot<-function(MSEobj,MSEobj_reb,options=list(),maxrow=1,maxcol=3){

  MPcols<-rep('purple',MSEobj@nMPs)
  
  #          F2   F3           F4     F5      F6      F7        F8       F9      F10         F11        F12    F13    F14       F15      F16      F17     F18      F19 
  opt1<-  c("M",  "D", "hs",  "Ftype","Esd",  "qhssim", "qinc",  "L50",  "Sel50sim", "Vmaxlen", "DR",  "Fdisc", "procsd", "Ahsim", "Vhsim", "Asim", "Vsim", "initD",
            "TACFrac", "TACSD", "TAEFrac","TAESD", "SizeLimFrac","SizeLimSD","Cbias","betas","RefY")
  
  MSEtemp<-MSEobj
  MSEtemp@OM<-cbind(MSEtemp@OM,betas=MSEtemp@Obs$betas,MSEtemp@Misc[[4]])
  MSEtemp@OM<-MSEtemp@OM[,names(MSEtemp@OM)%in%opt1]
  VOIout<-VOI(MSEtemp,ncomp=15,nbins=8,plot=F)[[1]]
  
              
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
  
}


VOI_plot<-function(MSEobj,MSEobj_reb,options=list(),maxcol=6){
  
  MPcols<-rep('purple',MSEobj@nMPs)

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
    ind<-dat[2,]!=""
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
    legend('center',legend=prescriptive,title="MPs without data requirements:",cex=1.3,bty='n')
  }
  mtext("Question / operating model characteristic",1,outer=T,line=0.5)
  mtext("Variability in Long Term Yield (% LTY)",2,outer=T,line=0.5)
  
}

Yproj<-function(MSEobj,MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95)){
  
  MPcols<-rep('purple',MSEobj@nMPs)
  
  maxcol=5
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  yrs<-Current_Year+(1:MSEobj@proyears)
  
  nc<-maxcol
  nr<-ceiling(nMPs/nc)
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  Yd<-MSEobj@C/ array(rep(MSEobj@C[,,1],MSEobj@proyears),dim(MSEobj@C))#MSEobj@OM$RefY
  Yd[is.na(Yd)]<-0
  
  Ylims<- c(0,min(10,quantile(Yd,0.95)))
  
  for(i in 1:nMPs){
    plot(range(yrs),Ylims,col="white")
    
    plotquant(Yd[,i,],p=quants,yrs,qcol,lcol,ablines=1)
    mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
  }
 
  mtext("Yield relative to today",2,line=0.7,outer=T)
  mtext("Projection Year",1,line=0.7,outer=T)
  
}

F_FMSYproj<-function(MSEobj,MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95)){
  
  MPcols<-rep('purple',MSEobj@nMPs)
  
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
  PMobj@Stat <- MSEobj@B_BMSY[, , Yrs[2]]
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
  PMobj@Stat <- MSEobj@B_BMSY[, , Yrs[2]]
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
  RefYd <- array(MSEobj@OM$RefY, dim = dim(MSEobj@C[, , Yrs[1]:Yrs[2]]))
  PMobj@Stat <- MSEobj@C[, , Yrs[1]:Yrs[2]]/RefYd
  PMobj@Ref <- 0.5
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(LTY2)<<-'PM'

Tplot<-function(MSEobj, MSEobj_reb, controls=list()){
 
  TradePlot(MSEobj,'PB50', 'LTY2', 'PB100','LTY2',fill=rgb(0.4,0.8,0.95))
}

# ============= Risk Assessment ==================

  Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- list()
  
  # These are the names of widgets and their values to display in this skin / mode
  #             years in projection,  year resolution of reporting  rounding of digits
  options<-list()#burnin=10,            res=1)
  
  
  # --- Tables --- 
  Tab_title[[1]] <- "Table 1. Risk assessment. Projected biomass relative to the LRP"
  Tab_text[[1]] <-"Table 1. Risk assessment text. The probability that projected biomass is above 50% BMSY. "
  
  Tabs[[1]]<-function(MSEobj,MSEobj_reb,options=list(),rnd=1){
    
    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    burnin<-10
    ind<-1+(0:1000*options$res)
    ind<-ind[ind<=min(burnin,proyears)]
    
    LRP<-round(apply(MSEobj@B_BMSY[,,1:burnin,drop=FALSE]>0.5,2:3,mean)*100,rnd)[,ind]
    Tab1<-as.data.frame(cbind(MSEobj@MPs,LRP))
    colnams<-c("MP",ind+Current_Year)
    names(Tab1)<-colnams
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
    caption=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab1,caption=caption)%>%
      formatStyle(columns = 2:ncol(Tab1), valueColumns = 2:ncol(Tab1), color = styleInterval(c(50,100),c('red','orange','green')))
    
  }
  Tab_title[[2]] <- Tab_title[[3]] <- "" # make extras empty
  
  
  Fig_title[[1]] <- "Figure 1. Risk Assessment. B/BMSY and Yield (relative to today) projection plots"
  Fig_text[[1]] <-  "Figure 1. Risk assessment text. Projections of biomass and yield relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points." 
  
  Figs[[1]]<-function(MSEobj,MSEobj_reb,options=list())plot(1:10)#BMSYproj(MSEobj,MSEobj_reb,options)
  Fig_dim[[1]]<-function(dims)list(height=350,width=1000)
  
  Fig_title[[2]] <- Fig_title[[3]] <- "" # make extras empty
  
  
  
  Risk_Assessment<-list(Tabs=Tabs,Figs=Figs,Tab_title=Tab_title,Tab_text=Tab_text, Fig_title=Fig_title,Fig_text=Fig_text,Fig_dim=Fig_dim,options=options)
  
  
# ============= Planning =========================
  
  Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- list()
  
  # These are the names of widgets and their values to display in this skin / mode
  #             years in projection,  year resolution of reporting  rounding of digits
  options<-list(burnin=10,            res=1)
  
  # --- Tables --- 
  Tab_title[[1]] <- "Table 1. Projected biomass relative to the LRP"
  Tab_text[[1]] <-"The probability that projected biomass is above 50% BMSY. Probabilities of 50% or lower are shaded red. Probabilities over 90% are shaded green. "
  
  Tabs[[1]]<-function(MSEobj,MSEobj_reb,options=list(res=1),rnd=1){
    
    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    burnin<-10
    ind<-1+(0:1000*options$res)
    ind<-ind[ind<=min(burnin,proyears)]
    
    LRP<-round(apply(MSEobj@B_BMSY[,,1:burnin,drop=FALSE]>0.5,2:3,mean)*100,rnd)[,ind]
    Tab1<-as.data.frame(cbind(MSEobj@MPs,LRP))
    colnams<-c("MP",ind+Current_Year)
    names(Tab1)<-colnams
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
    caption=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab1,caption=caption)%>%
      formatStyle(columns = 2:ncol(Tab1), valueColumns = 2:ncol(Tab1), color = styleInterval(c(50,90),c('red','orange','green')))
    
  }
  
  Tab_title[[2]] <- "Table 2. Projected biomass relative to the TRP"
  Tab_text[[2]] <-"The probability that projected biomass is above BMSY"
  
  Tabs[[2]]<-function(MSEobj,MSEobj_reb,options=list(res=1),rnd=1){
    
    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    burnin<-10
    ind<-1+(0:1000*options$res)
    ind<-ind[ind<=min(burnin,proyears)]
    
    TRP<-round(apply(MSEobj@B_BMSY[,,1:burnin,drop=FALSE]>1,2:3,mean)*100,rnd)[,ind]
    Tab2<-as.data.frame(cbind(MSEobj@MPs,TRP))
    colnams<-c("MP",ind+Current_Year)
    names(Tab2)<-colnams
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
    caption=paste0("Simulations start between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab2,caption=caption)%>%
      formatStyle(columns = 2:ncol(Tab2), valueColumns = 2:ncol(Tab2), color = styleInterval(c(25,50,100),c('red','orange','green','darkgreen')))
    
  }
  
  Tab_title[[3]] <- Tab_title[[4]] <- Tab_title[[5]] <- Tab_title[[6]] <- Tab_title[[7]] <- Tab_title[[8]] <- Tab_title[[9]] <- "" # make extras empty


  # --- Figures ---
  
  Fig_title[[1]]<-""
  
  Fig_title[[2]] <- "Figure 1. Biomass projection relative to the Target and Limit Reference Points"
  Fig_text[[2]] <- "Projections of biomass and yield relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, 
                    the white solid line is the median and the dark blue lines are two example simulations.
                    Grey horizontal lines denote the Target (BMSY) and Limit (50% BMSY) Reference Points." 
  Figs[[2]]<-function(MSEobj,MSEobj_reb,options=list()) BMSYproj(MSEobj,MSEobj_reb,options)
  Fig_dim[[2]]<-function(dims)list(height=ceiling(dims$nMPs/5)*250,width=1100)
  
  Fig_title[[3]] <- "Figure 2. Long-term HCR"
  Fig_text[[3]] <- "Projections of biomass relative to MSY and unfished (B0) levels given a starting depletion of half BMSY. The rebuilding analysis simulates the fishery currently in a depleted state even if the user-specified depletion in the operating model is higher.
  In these cases, the rebuilding analysis provides added assurance whether a particular management procedure would be likely to rebuild the stock if the user-specified depletion level is overly optimistic and in need of rebuilding.
  The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the limit and target reference points" 
  Figs[[3]]<-function(MSEobj,MSEobj_reb,options=list()) LT_HCR(MSEobj,MSEobj_reb,options)
  Fig_dim[[3]]<-function(dims)list(height=ceiling(dims$nMPs/5)*250,width=1100)
  
  Fig_title[[4]] <- "Figure 3. Short-term HCR"
  Fig_text[[4]] <- "As Figure 2 but over the first 10 years." 
  Figs[[4]] <- function(MSEobj,MSEobj_reb,options=list()) ST_HCR(MSEobj,MSEobj_reb,options)
  Fig_dim[[4]]<-function(dims)list(height=ceiling(dims$nMPs/5)*250,width=1100)
  
  Fig_title[[5]] <- "Figure 4. Evaluation of current uncertainties"
  Fig_text[[5]] <- "This figure identifies those questions across which there is the highest variability in long term yield (average yield over last 10 years of the projection). This figures identifies which elements of the questionnaire (Step A) are the most consequential uncertainties." 
  Figs[[5]] <- function(MSEobj,MSEobj_reb,options=list()) CCU_plot(MSEobj,MSEobj_reb,options)
  Fig_dim[[5]]<-function(dims)list(height=ceiling(dims$nMPs/3)*350,width=1300)
  
  Fig_title[[6]] <- "Figure 5. Value of information"
  Fig_text[[6]] <- "This figure identifies the key observation uncertainties (biases and errors) in determing the long-term yield performance of MPs (average yield over last 10 years of the projection)." 
  Figs[[6]] <- function(MSEobj,MSEobj_reb,options=list()) VOI_plot(MSEobj,MSEobj_reb,options)
  Fig_dim[[6]]<-function(dims)list(height=ceiling(dims$nMPs/6)*350,width=1300)
  
  Fig_title[[7]] <- "Figure 6. Yield projection"
  Fig_text[[7]] <- "Future yield as a fraction of current yield" 
  Figs[[7]] <- function(MSEobj,MSEobj_reb,options=list()) Yproj(MSEobj,MSEobj_reb,options)
  Fig_dim[[7]]<-function(dims)list(height=ceiling(dims$nMPs/5)*350,width=1300)
  
  Fig_title[[8]] <- "Figure 7. Fishing mortality rate projection"
  Fig_text[[8]] <- "Future fishing mortality rate relative to FMSY" 
  Figs[[8]] <- function(MSEobj,MSEobj_reb,options=list()) F_FMSYproj(MSEobj,MSEobj_reb,options)
  Fig_dim[[8]]<-function(dims)list(height=ceiling(dims$nMPs/5)*350,width=1300)
  
  Fig_title[[9]] <- "Figure 8. Yield - Biomass trade-offs"
  Fig_text[[9]] <- "Trade-off between yield and biomass risks" 
  Figs[[9]] <- function(MSEobj,MSEobj_reb,options=list()) Tplot(MSEobj,MSEobj_reb,options)
  Fig_dim[[9]]<-function(dims)list(height=550,width=1100)
  
  Planning<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, Fig_text=Fig_text, Fig_dim=Fig_dim, options=options)
  

# ============= Evaluation =======================

  Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- list()
  
  # These are the names of widgets and their values to display in this skin / mode
  #             years in projection,  year resolution of reporting  rounding of digits
  options<-list(burnin=10,            res=1,  YrsIU = 5)

  # --- Tables --- 
  Tab_title[[1]] <- "Table 1. Biomass relative to 50% BMSY"
  Tab_text[[1]] <-"The biomass projection for the interim years that an MP has been in use."

  Tabs[[1]]<-function(MSEobj,MSEobj_reb,options=list(burnin=10,res=1),res=5,rnd=1){
    
    nMPs<-MSEobj_reb@nMPs
    proyears<-MSEobj_reb@proyears
    ind<-1:min(options$burnin,proyears)
    
    LRP<-matrix(round(apply(MSEobj@B_BMSY[,,1:options$burnin,drop=FALSE]>0.5,2:3,mean)*100,rnd)[,ind],nrow=nMPs)
    Tab1<-as.data.frame(cbind(MSEobj@MPs,LRP))
    colnams<-c("MP",Current_Year-((options$burnin-1):0))
    names(Tab1)<-colnams
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
    caption=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab1,caption=caption)%>%
      formatStyle(columns = 2:ncol(Tab1), valueColumns = 2:ncol(Tab1), color = styleInterval(c(50,100),c('red','orange','green')))
  
  }
  
  Tab_title[[2]] <- "Table 2. Biomass relative to BMSY"
  Tab_text[[2]] <-"The biomass projection for the interim years that an MP has been in use."
  
  Tabs[[2]]<-function(MSEobj,MSEobj_reb, options=list(burnin=10,res=1),rnd=1){
    
    nMPs<-MSEobj_reb@nMPs
    proyears<-MSEobj_reb@proyears
    ind<-1:min(options$burnin,proyears)
    
    TRP<-matrix(round(apply(MSEobj@B_BMSY[,,ind,drop=FALSE]>1,2:3,mean)*100,rnd)[,ind],nrow=nMPs)
    Tab2<-as.data.frame(cbind(MSEobj@MPs,TRP))
    colnams<-c("MP",Current_Year-((options$burnin-1):0))
    names(Tab2)<-colnams
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
    caption=paste0("Simulations start between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab2,caption=caption)%>%
      formatStyle(columns = 2:ncol(Tab2), valueColumns = 2:ncol(Tab2), color = styleInterval(c(25,50,100),c('red','orange','green','darkgreen')))
    
  }

  Tab_title[[3]] <- "Table 3. Long term HCR"
  Tab_text[[3]] <-"Probability of biomass exceeding the target reference point in the years since MP adoption."
  
  Tabs[[3]]<-function(MSEobj,MSEobj_reb,options=list(burnin=10,res=1,YIU)){
    
    nMPs<-MSEobj_reb@nMPs
    proyears<-MSEobj_reb@proyears
    ind<-proyears-(9:0)
    TRP<-matrix(round(apply(MSEobj_reb@B_BMSY[,,ind,drop=F]>1,2:3,mean)*100,rnd),nrow=nMPs)
    Tab3<-as.data.frame(cbind(MSEobj_reb@MPs,TRP))
    colnams<-c("MP",Current_Year+proyears-options$burnin-(9:0))
    names(Tab3)<-colnams
    Bdeps<-MSEobj_reb@OM$D/MSEobj_reb@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
    caption=paste0("Simulations start between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab3,caption=caption)%>%
      formatStyle(columns = 2:ncol(Tab3), valueColumns = 2:ncol(Tab3), color = styleInterval(c(25,50,100),c('red','orange','green','darkgreen')))

  }
  
  Tab_title[[4]] <- "Table 4. Short term HCR"
  Tab_text[[4]] <-"Probability of biomass exceeding the target reference point in the years since MP adoption"
  
  Tabs[[4]]<-function(MSEobj, MSEobj_reb,options=list(),rnd=1){
    
    nMPs<-MSEobj_reb@nMPs
    proyears<-MSEobj_reb@proyears
    
    MGT2<-2* MSEobj_reb@OM$MGT
    ind<-1:20
    TRP<-matrix(round(apply(MSEobj_reb@B_BMSY[,,ind,drop=FALSE]>1,2:3,mean)*100,rnd)[,ind],nrow=nMPs)
    
    shaderng=range(ceiling(MGT2))
    #shaderng[2]<-min(20,shaderng[2])
    
    Tab4<-as.data.frame(cbind(MSEobj_reb@MPs,TRP))
    colnams<-c("MP",Current_Year+(1:20)-options$burnin)
    names(Tab4)<-colnams
    Bdeps<-MSEobj_reb@OM$D/MSEobj_reb@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
    caption=paste0("Simulations start between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
    datatable(Tab4,caption=caption)%>%
      formatStyle(columns = 2:ncol(Tab4), valueColumns = 2:ncol(Tab4), color = styleInterval(c(25,50,100),c('red','orange','green','darkgreen')))%>%
      formatStyle(colnams[1+shaderng[1]:shaderng[2]],backgroundColor='lightgrey')
    
  }
 
  Tab_title[[5]] <- Tab_title[[6]] <- Tab_title[[7]] <- Tab_title[[8]] <- Tab_title[[9]] <- "" # make extras empty
  
  # --- Figures ---
 
  Fig_title[[1]]<-""
  
  Fig_title[[2]] <- "Figure 1. Biomass projected since MP adoption"
  Fig_text[[2]] <- "Projections of biomass relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points. The bold black vertical line is the current year." 
  
  Figs[[2]]<-function(MSEobj,MSEobj_reb,options=list()) BMSYproj(MSEobj,MSEobj_reb,options,maxcol=1)
  Fig_dim[[2]] <- function(dims)list(height=420,width=600)
  
  Fig_title[[3]] <- "Figure 2. Long-term HCR"
  Fig_text[[3]] <- "Projections of biomass relative to MSY and unfished (B0) levels given a starting depletion of half BMSY. The rebuilding analysis simulates the fishery currently in a depleted state even if the user-specified depletion in the operating model is higher.
  In these cases, the rebuilding analysis provides added assurance whether a particular management procedure would be likely to rebuild the stock if the user-specified depletion level is overly optimistic and in need of rebuilding.
  The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the limit and target reference points. The bold black vertical line is the current year, the black vertical line denotes the last 10 years of the projection over which results are tabulated." 

  Figs[[3]]<-function(MSEobj,MSEobj_reb,options=list()) LT_HCR(MSEobj,MSEobj_reb,options,maxcol=1,vline=41)
  Fig_dim[[3]] <- function(dims)list(height=420,width=600)
  
  Fig_title[[4]] <- "Figure 3. Short-term HCR"
  Fig_text[[4]] <- "As Figure 2 but over a 20 year projection. The shaded grey region is the period between the minimum and maximum values of two mean generation times." 
  
  Figs[[4]]<-function(MSEobj,MSEobj_reb,options=list()) ST_HCR(MSEobj,MSEobj_reb,options,plotMGT=T,maxcol=1)
  Fig_dim[[4]] <- function(dims)list(height=420,width=600)
  
  Fig_title[[5]] <- "Figure 4. Evaluation of current uncertainties"
  Fig_text[[5]] <- "This figure identifies those questions across which there is the highest variability in long term yield (average yield over last 10 years of the projection). This figures identifies which elements of the questionnaire (Step A) are the most consequential uncertainties." 
  Figs[[5]] <- function(MSEobj,MSEobj_reb,options=list()) CCU_plot(MSEobj,MSEobj_reb,options,maxcol=1)
  Fig_dim[[5]]<-function(dims)list(height=420,width=600)
  
  Fig_title[[6]] <- "Figure 5. Value of information"
  Fig_text[[6]] <- "This figure identifies the key observation uncertainties (biases and errors) in determing the long-term yield performance of MPs (average yield over last 10 years of the projection)." 
  Figs[[6]] <- function(MSEobj,MSEobj_reb,options=list()) VOI_plot(MSEobj,MSEobj_reb,options,maxcol=1)
  Fig_dim[[6]]<-function(dims)list(height=420,width=600)
  
  
  Fig_title[[7]] <- Fig_title[[8]] <- Fig_title[[9]] <- "" # make extras empty
  
  Evaluation<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, Fig_text=Fig_text, Fig_dim=Fig_dim, options=options)
 

# ========== Build ============================= 
  
MSC<-list(Risk_Assessment=Risk_Assessment,Planning=Planning,Evaluation=Evaluation) 
Skins[[1]]<-MSC



# ===== FAO ========================================================================================


# ============= Risk Evaluation ================

# ============= Planning =======================

# ============= Assessment =====================



# ===== Pew ========================================================================================


# ============= Risk Evaluation ================

# ============= Planning =======================

# ============= Assessment =====================




# ====== Wrap up ====================================================================================

Skin<-Skins[[1]]

