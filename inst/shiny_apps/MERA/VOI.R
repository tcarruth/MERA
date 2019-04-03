getVOI<-function(MSEobj_top){

  opt1<-  c("M",        "Depletion",      "hs",      "Esd",          "LFS",        "Vmaxlen",  "DR",          "PRM",             "procsd",   "qinc",            "Frac_area_1","Prob_staying",
            "TACFrac", "TACSD","Cbias","betas")

  opt2<-  c(rep("",12),"TAEFrac", "TAESD","","","RefY")

  MSEtemp<-MSEobj_top
  MSEtemp@OM<-cbind(MSEtemp@OM,betas=MSEtemp@Obs$betas,Cbias=MSEtemp@Obs$Cbias)
  MSEtemp@OM<-MSEtemp@OM[,names(MSEtemp@OM)%in%opt1 | names(MSEtemp@OM)%in%opt2]
  VOI(MSEtemp,ncomp=16,nbins=6,plot=F)[[1]]

}


CCU_plot<-function(VOIout,MSEobj,MPcols,maxrow=1,maxcol=3){

  qno<-   c("F2",       "F3",             "F4",       "F6",         "F7",       "F8",          "F9",              "F10",      "F11",             "F12",        "F13",          "F14",
            "M2",       "M3",  "D2",    "D3")
  qtext<- c("Longevity","Stock depletion","Resilence","Exploit. Var.","Selectivity","Dome Sel.","Discard rate","Post. Rel. Mort.","Rec. Var.","Fish. efficiency","MPA size",   "Mixing",
            "Imp. over/under",  "Imp. Var.", "Cat. Rep. Bias.","Hyperstability")
  opt1<-  c("M",        "Depletion",      "hs",      "Esd",          "LFS",        "Vmaxlen",  "DR",          "PRM",             "procsd",   "qinc",            "Frac_area_1","Prob_staying",
            "TACFrac", "TACSD","Cbias","betas")
  opt2<-  c(rep("",12),"TAEFrac", "TAESD","","","RefY")

  nMPs<-MSEobj@nMPs

  nrow=ceiling(nMPs/maxcol)
  par(mfrow=c(max(maxrow,nrow),maxcol),mai=c(2.4,0.4,0.01,0.01),omi=c(0.3,0.3,0.05,0.01))

  MPcols[MPcols=="green"]<-'darkgreen'

  for(i in 1:MSEobj@nMPs){

    MP<-MSEobj@MPs[i]
    dat<-VOIout[match(MP,VOIout[,1])+0:1,2:17]
    lab1<-qno[match(as.factor(unlist(dat[1,])),opt1)]
    lab2<-qno[match(as.factor(unlist(dat[1,])),opt2)]
    lab1[is.na(lab1)]<-lab2[is.na(lab1)]
    dat2<-aggregate(as.numeric(as.character(unlist(dat[2,]))),by=list(lab1),max)
    dat2<-dat2[order(dat2$x,decreasing=T),]
    labs<-paste(qno,qtext,sep=" - ")
    barplot(dat2[,2],names.arg=labs[match(dat2[,1],qno)], las=2,col=fcol,border=NA,cex.axis=1.4,cex.names=1.3)
    legend('topright',MP,bty='n',text.font=2,cex=1.6,text.col=MPcols[i])

  }

  mtext("Question / operating model characteristic",1,outer=T,line=0.5)
  mtext("Variability in Long Term Yield (% LTY)",2,outer=T,line=0.5)

}

