
# ====================================================================================================================================
# ====  MSC Skin  ====================================================================================================================
# ====================================================================================================================================

# Reused Figure code
Current_Year<-2019

B0proj<-function(MSEobj,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),vline=NA,fease=F,MPcols=NA,MPnams=NA){

  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  if(is.na(MPcols))MPcols<-rep('black',nMPs)
  if(is.na(MPnams))MPnams<-MSEobj@MPs

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
    plot(range(yrs),Blims,col="white",axes=F)

    axis(1,labels=(i==nMPs))
    axis(2)

    plotquant(B_B0[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.4))
    mtext(MPnams[i],2,line=2.4,font=2,col=MPcols[i])
    if(i==1)mtext("Biomass relative to unfished",3,line=0.5,font=2)
  }

 # mtext("Stock status",2,line=0.7,outer=T)


}


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
    if("YIU"%in%names(options))abline(v=yrs[options$YIU],lwd=2) #polygon(yrs[c(1,options$burnin,options$burnin,1)],c(-10,-10,10,10),col='lightgrey',border=NA)
    
  }
  
  mtext("B/BMSY",2,line=0.7,outer=T)
  mtext("Year",1,line=0.7,outer=T)
  
}


Y_proj<-function(MSEobj,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),vline=NA,fease=F){

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
  #par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))

  RY<-MSEobj@C/MSEobj@OM$RefY
  Blims <- c(0,quantile(RY,0.95))

  for(i in 1:nMPs){
    plot(range(yrs),Blims,col="white",axes=F)
    axis(1,labels=(i==nMPs))
    axis(2)
    plotquant(RY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5))
    #mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
    if(i==1)mtext("Yield relative to MSY",3,line=0.5,font=2)
  }


  #mtext("Year",1,line=0.7,outer=T)

}


plotquant<-function(x,p=c(0.05,0.25,0.75,0.95),yrs,qcol,lcol,addline=T,ablines=NA){
  #plot(range(yrs),Ylims,col="white")

  ny<-length(yrs)
  x[x==Inf]<-NA
  qs<-apply(x,2,quantile,p=p[c(1,4)],na.rm=T,type=3)
  qsi<-apply(x,2,quantile,p=p[2:3],na.rm=T,type=3)
  polygon(c(yrs,yrs[ny:1]),c(qs[1,],qs[2,ny:1]),border=NA,col='#b3ecff')

  polygon(c(yrs,yrs[ny:1]),c(qsi[1,],qsi[2,ny:1]),border=NA,col=qcol)
  if(!is.na(ablines[1]))abline(h=ablines,col='red',lty=2)

  if(addline)for(i in 1:2)lines(yrs,x[i,],col=lcol,lty=i)
  lines(yrs,apply(x,2,quantile,p=0.5,na.rm=T),lwd=2,col="white")
}



CCU_plot<-function(MSEobj,MSEobj_reb,options=list(),maxrow=1,maxcol=3,fease=F){

  if(fease){
    MPcols=MPcols#FeaseLabs(MSEobj@MPs)$MPcols
  }else{
    MPcols<-rep('black',MSEobj@nMPs)
  }


  #          F2   F3           F4     F5      F6      F7        F8       F9      F10         F11        F12    F13    F14       F15      F16      F17     F18      F19
  opt1<-  c("M",  "D", "hs",  "Ftype","Esd",  "qhssim", "qinc",  "L50",  "Sel50sim", "Vmaxlen", "DR",  "Fdisc", "procsd", "Ahsim", "Vhsim", "Asim", "Vsim", "initD",
            "TACFrac", "TACSD", "TAEFrac","TAESD", "SizeLimFrac","SizeLimSD","Cbias","betas","RefY")

  MSEtemp<-MSEobj
#  if(length(MSEtemp@Misc)<4)MSEtemp@Misc[[4]]<-NULL
 # MSEtemp@OM<-cbind(MSEtemp@OM,betas=MSEtemp@Obs$betas,MSEtemp@Misc[[4]])
  #MSEtemp@OM<-MSEtemp@OM[,names(MSEtemp@OM)%in%opt1]
  VOIout<-VOI(MSEtemp,ncomp=15,nbins=6,plot=F)[[1]]


  qno<-   c("F2",       "F3",             "F4",       "F5",            "F6",         "F7",               "F8",                  "F9",                "F10",        "F11",      "F12",        "F13",              "F14",      "F15",            "F16",         "F17",         "F18",           "F19",
            "M2",       "M3",          "M4",         "M5",     "M6",               "M7", "D2",    "D3")
  qtext<- c("Longevity","Stock depletion","Steepness","Effort Pattern","Effort Var.","Hist. catchability","Future catchability","Length at Maturity","Selectivity","Dome Sel.","Discard rate","Post. Rel. Mort.","Rec. Var.","Hist. MPA size", "Hist. Mixing", "Future MPA", "Future Mixing", "Initial Dep.",
            "TAC offset", "TAC Var.", "TAE offset", "TAE Var", "Size Lim. offset", "Size Lim. Var.", "Cat. Rep. Bias.","Hyperstability")
  nMPs<-MSEobj@nMPs

  nrow=ceiling(nMPs/maxcol)
 # par(mfrow=c(max(maxrow,nrow),maxcol),mai=c(2.4,0.4,0.2,0.01),omi=c(0.3,0.3,0.05,0.01))

  for(i in 1:MSEobj@nMPs){

    MP<-MSEobj@MPs[i]
    dat<-VOIout[match(MP,VOIout[,1])+0:1,2:16]
    lab1<-qno[match(as.factor(unlist(dat[1,])),opt1)]
    dat2<-aggregate(as.numeric(as.character(unlist(dat[2,]))),by=list(lab1),max)
    dat2<-dat2[order(dat2$x,decreasing=T),]
    labs<-paste(qno,qtext,sep=" - ")

    barplot(dat2[,2],names.arg=labs[match(dat2[,1],qno)], las=2,col=rgb(0.4,0.8,0.95),border=NA,cex.axis=1.4,cex.names=1.3)
    mtext("(a) Cost of Current Uncertainties",3,line=1,adj=0.3,font=2,cex=1.2,col=MPcols[i])

  }

  mtext("Question / operating model characteristic",1,line=13,cex=1.3)
  #mtext("Variability in Long Term Yield (% LTY)",2,outer=T,line=0.5)

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
  #par(mfrow=c(nrow,maxcol),mai=c(1.4,0.4,0.2,0.01),omi=c(0.3,0.3,0.05,0.01))

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
    mtext("(b) Value of Information",3,line=1,adj=0.3,font=2,cex=1.2,col=MPcols[i])

  }

  if(sum(!MPplot)>0){

    plot(c(0,1),axes=F,xlab="",ylab="",main='',col='white')
    prescriptive<-MSEobj@MPs[!MPplot]
    legend('center',legend=prescriptive,title="'Zero data' MPs:",cex=1.3,bty='n')

  }

  mtext("Data quality parameter",1,line=13,cex=1.3)
  #mtext("Variability in Long Term Yield (% LTY)",2,outer=T,line=0.5)

}


D40<-function (MSEobj = NULL, Ref = 0.4, Yrs = -50)
{
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Probability long-term biomass is greater than 40% Unfished"

    PMobj@Caption <- paste0("Prob. Biomass > ", Ref, " Unfished (Years ",
                            2020+Yrs[1],"-",2020+Yrs[2],")")

  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@SSB[, , Yrs[1]:Yrs[2]]/MSEobj@OM$SSB0
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(D40)<-'PM'

Y50<-function (MSEobj = NULL, Ref = 0.5, Yrs = -50)
{
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Yield relative to MSY (Years ",
                       Yrs[1], "-", Yrs[2], ")")

    PMobj@Caption <- paste0("Prob. Yield > ", Ref,
                            " MSY (Years ", 2020+Yrs[1], "-", 2020+Yrs[2],
                            ")")


  RefYd <- array(MSEobj@OM$RefY, dim = dim(MSEobj@C[, , Yrs[1]:Yrs[2]]))
  PMobj@Stat <- MSEobj@C[, , Yrs[1]:Yrs[2]]/RefYd
  PMobj@Ref <- 0.5
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(Y50)<-'PM'










# ============= Risk Assessment ==================

  Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')

  # These are the names of widgets and their values to display in this skin / mode
  #             years in projection,  year resolution of reporting  rounding of digits
  options<-list(res=5)

  Intro_title[[1]] <- "Introduction"
  Intro_text[[1]] <- "Status quo fishing effort and catches are projected to evaluate biological risk. Zero catch and FMSY fishing are also projected to frame performance."

  # --- Tables ---
  Tab_title[[1]] <- "Table 1. Projected biomass relative 50% BMSY"
  Tab_text[[1]] <-"The probability that projected biomass exceeds 50% BMSY. "

  Tabs[[1]]<-function(MSEobj,MSEobj_reb,options=list(res=5,YIU=1),rnd=1){

    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    ind<-1+(0:1000*options$res)
    ind<-ind[ind<=proyears]

    LRP<-round(apply(MSEobj@B_BMSY>0.5,2:3,mean)*100,rnd)[,ind]
    Tab1<-as.data.frame(cbind(c("Current effort", "Current catches", "FMSY fishing", "Zero fishing"),LRP),stringsAsFactors = F)
    for(i in 2:ncol(Tab1))Tab1[,i]<-as.numeric(Tab1[,i])
    colnams<-c("MP",ind+Current_Year)
    names(Tab1)<-colnams
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
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
      formatStyle(columns = 2:ncol(Tab1), valueColumns = 2:ncol(Tab1), color = styleInterval(c(50,100),c('red','orange','green')))

  }

  Tab_title[[2]] <- "Table 2. Projected biomass relative to BMSY"
  Tab_text[[2]] <-"The probability that projected biomass is above BMSY. "

  Tabs[[2]]<-function(MSEobj,MSEobj_reb,options=list(res=5),rnd=1){

    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    ind<-1+(0:1000*options$res)
    ind<-ind[ind<=proyears]

    TRP<-round(apply(MSEobj@B_BMSY>1,2:3,mean)*100,rnd)[,ind]
    Tab1<-as.data.frame(cbind(c("Current effort", "Current catches", "FMSY fishing", "Zero fishing"),TRP),stringsAsFactors = F)
    for(i in 2:ncol(Tab1)) Tab1[,i]<-as.numeric(Tab1[,i])
    colnams<-c("MP",ind+Current_Year)
    names(Tab1)<-colnams
    Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
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
      formatStyle(columns = 2:ncol(Tab1), valueColumns = 2:ncol(Tab1), color = styleInterval(c(50,100),c('red','orange','green')))

  }

  #Tab_title[[3]] <- Tab_title[[4]] <- Tab_title[[5]] <- Tab_title[[6]] <- Tab_title[[7]] <- Tab_title[[8]] <- Tab_title[[9]] <- "" # make extras empty

  Fig_title[[2]] <- "Figure 1. Risk Assessment. B/BMSY and Yield (relative to today) projection plots"
  Fig_text[[2]] <-  "Figure 1. Risk assessment text. Projections of biomass and yield relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points."

  Figs[[2]]<-function(MSEobj,MSEobj_reb,options=list()){
    MSEobj@MPs<-c("Current effort", "Current catches", "FMSY fishing", "Zero fishing")
    BMSYproj(MSEobj,MSEobj_reb,options,maxcol=4)
  }
  Fig_dim[[2]]<-function(dims)list(height=400,width=1200)

  Fig_title[[1]] <- ""#<- Fig_title[[3]] <- Fig_title[[4]] <- Fig_title[[5]]<- Fig_title[[7]] <- Fig_title[[8]] <- Fig_title[[9]] <- "" # make extras empty

  Risk_Assessment<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title,
                        Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)


#MSC<-list(Risk_Assessment=Risk_Assessment,Planning=Planning,Evaluation=Evaluation)
#None<-list()



