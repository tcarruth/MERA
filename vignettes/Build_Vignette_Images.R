# ============================================================================================
# Figures for MERA paper =====================================================================
# ============================================================================================

library('corpcor')
library('DLMtool')
library('dplyr')
library('formattable')
library('kableExtra')
library('knitr')
library('shiny')
library('shinyjs')
library('shinyalert')
library('MSEtool')
library('tinytex')
library('DT')
library('rmarkdown')


setwd("C:/Users/tcar_/Dropbox/MERA paper/Figures")
setwd("C:/Users/tcarruth/Dropbox/MERA paper/Figures")
source("MSC4paper.R")
res<-600

library(DLMtool)
library(MSEtool)
library(DLMextra)

avail('OM')
MSEobj<-MSEobj_reb<-runMSE(testOM,MPs=c("curE","curE75","FMSYref","NFref"))
save(MSEobj,file="MSEobj")
save(MSEobj_reb,file="MSEobj_reb")

California_Halibut_CA_CDFW@D<-c(0.1,0.7)
California_Halibut_CA_CDFW@nsim<-98
OM<-California_Halibut_CA_CDFW
load(file="MSEobj2")
#MSEobj2<-runMSE(OM,MPs=c("matlenlim","DD","FMSYref","MCD","DCAC","MRnoreal","LstepCE1","slotlim"))
OM_reb<-OM
OM_reb@cpars$D<-(50/100)*MSEobj2@OM$SSBMSY_SSB0 
MSEobj_reb2<-runMSE(OM_reb,MPs=c("matlenlim","DD","FMSYref","MCD","DCAC","MRnoreal","LstepCE1","slotlim"))

#save(MSEobj2,file="MSEobj2")
save(MSEobj_reb2,file="MSEobj_reb2")

# Figure 1 - MERA schematic --------------------------------------------------------

# Figure 2 - App front end headshot ------------------------------------------------

# Figure 3 - Risk Assessment Mode --------------------------------------------------

render(input="RA4paper.Rmd", output_file="Figures.html")

# Figure 4 - CCU and VOI -----------------------------------------------------------

load("MSEobj2")
MSEobj2a<-Sub(MSEobj2,MPs="DD")
MSEobj2a@MPs<-"DD"

jpeg("Figure 4.jpg",res=600,units="in",width=10,height=6.5)
  par(mfrow=c(1,2),mai=c(3,0.5,0.5,0.05),omi=c(0,0.3,0,0))
  CCU_plot(MSEobj2a,MSEobj2)
  VOI_plot(MSEobj2a,MSEobj2)
  mtext("Difference in yield across parameter range (kilotonnes)",2,outer=T,cex=1.3)
dev.off()

# Figure 5 - performance & trade-off ----------------------------------------------

MPcols<-c("black","red","green","blue")

load("MSEobj2")
MSEobj<-Sub(MSEobj2,MP=c("DD","MRnoreal","DCAC","MCD"))
labs<-c("Assessment","Size limit","Spatial closure","Current effort")
jpeg("Figure 5.jpg",res=600,units="in",width=11,height=6.5)

  par(mai=c(0.04,0.2,0.01,0.05),omi=c(0.45,0.3,0.6,0))
  layout(matrix(c(1,2,3,4,1,2,3,4,5,6,7,8,5,6,7,8,rep(9,12)),nrow=4))
  B0proj(MSEobj,MPcols=MPcols,MPnams=labs)
  Y_proj(MSEobj)

  mtext("(a) Projections of stock status and Yield",3,line=2.8,outer=T,adj=-0.02,cex=1)
  mtext("Year",1,line=2.2,outer=T,cex=1,adj=0.25)

  PD<-D40(MSEobj)@Mean
  PY<-Y50(MSEobj)@Mean
  xlim<-range(PD)+c(-0.11,0.1)
  ylim<-range(PY)+c(-0.01,0.03)

  par(mai=c(0.04,0.46,0.01,0.05))
  plot(PD,PY,xlim=xlim,ylim=ylim,col=MPcols,pch=19,cex=1.1)
  text(PD,PY+0.02,labs,col=MPcols,font=2,cex=1.3)
  mtext("(b) Performance Trade-Off",3,line=2.8,outer=T,adj=0.72,cex=1)
  mtext("Prob. Biomass > 0.4 Unfished",1,line=2.2,outer=T,cex=1,adj=0.88)
  mtext("Prob. Yield > 0.5 MSY",2,line=2.3)

dev.off()

# Figure 6 - Anciliary indicators -----------------------------------------------------------

source("AI_results4paper.R")
California_Halibut_CA_CDFW@nsim<-200
setup()
MSEobjPPD<-runMSE(California_Halibut_CA_CDFW,MPs=c("MCD"),PPD=T)
indData<-indPPD<-getinds(MSEobjPPD@Misc$Data[[1]],styr=MSEobjPPD@nyears,res=5)

# inconsistent
indData[1,,1]<-indPPD[1,,1]*4.2
indData[3,,1]<-indPPD[3,,1]*1.04
indData[4,,1]<-indPPD[4,,1]+0.25
# not consistent
indData[,,2]<-indPPD[,,3]
indData[2,,2]<-indData[2,,2]+0.2
indData[5,,2]<-indData[5,,2]-0.02
#indDataF[5,,2]<-(-3)

#indDataF[3,,2]<-indPPD[3,,3]

# consistent
indData[,,3]<-indPPD[,,2]
indData[5,,3]<-indPPD[5,,3]-0.005
#indDataF[3,,3]<-indPPD[3,,2]

jpeg("Figure 6.jpg",res=600,units="in",width=10,height=7.8)

  layout(matrix(c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16,21,21,21,21,20,17,18,19),nrow=4),widths=c(1,1,1,1,0.1,2))
  par(mai=rep(0,4),omi=c(0.5,0.5,0,0.05))
  CC(indPPD,indData,dnam=c("Catch slope","Catch variance","Catch mean","Index slope","Index mean"))
  par(mai=c(0.3,0.3,0.05,0.01))
  plot_mdist(indPPD,array(indData[,,1],c(dim(indData)[1:2],1)),alpha=0.05,coly="red",legy="Observed (inconsistent)",CV=TRUE)
  mtext("(b)",3,line=0.5)
  plot_mdist(indPPD,array(indData[,,2],c(dim(indData)[1:2],1)),alpha=0.05,coly="orange",legy="Observed (not consistent)")
  mtext("Density",2,line=3)
  plot_mdist(indPPD,array(indData[,,3],c(dim(indData)[1:2],1)),alpha=0.05,coly="green",legy="Observed (consistent)")
  mtext("Multivariate distance",1,line=3)
  plot(1,1,col='white',axes=F)
  legend('topright',legend=c("Predicted by operating model","Observed (inconsistent)","Observed (not consistent)","Observed (consistent)"),
                    text.col=c("blue","red","orange","green"),cex=1.2,
                    title="Data",title.col="black",bty='n')
  mtext("(a)",3,adj=0.33,line=-1.2,outer=T)

dev.off()


# Figure 7 status determination -----------------------------------------------------------

set.seed(2)
nmeths<-8
nsamp<-200
prec<-seq(0.1,0.3,length.out=nmeths)
bias1<-runif(nmeths,0.25,2)
bias2<-runif(nmeths,0.25,2)
bias1[4]<-5
bias2[4]<-0.8
deps<-seq(0.05,0.7,length.out=nsamp)
vals<-pred<-array(NA,c(nmeths,nsamp))
precadj<-seq(4,1,length.out=nsamp)

for(i in 1:nmeths){
  mutrad<-seq(bias1[i],bias2[i],length.out=nsamp)
  vals[i,]<-mutrad*trlnorm(nsamp,1,prec[i]*precadj)
  pred[i,]<-deps*vals[i,]
}


realdep<-80
depest<-pred[,realdep]
bce<-rep(NA,nmeths)
method<-c("Stock assessment","Length-based","Catch-based","Index-based")

jpeg("Figure 7.jpg",res=600,units="in",width=9,height=8)


  par(mfrow=c(2,2),mai=rep(0.2,4),omi=c(0.5,0.5,0.01,0.01))
  j<-0
  for(i in c(1,2,5,7)){
    j<-j+1
    plot(deps,pred[i,],xlim=c(0,0.7),ylim=c(0,1))
    legend('topleft',legend=paste0("(",letters[j],") ",method[j]),bty='n')
    lines(c(-1000,1000),c(-1000,1000),col='#99999930',lwd=3)

    #abline(v=deps[realdep],col='black',lty=2)
    if(i==1)legend('topleft',legend=c(rep("",4),'Estimated from data','Fitted bias',
                                          'Bias-corrected estimate'),
                                           text.col=c(rep('white',4),'green','blue','red'),bt='n')
    out<-loess(y~x,dat=data.frame(y=pred[i,],x=deps),enp.target=4)
    fitted<-predict(out,data=data.frame(x=deps))
    lines(deps,fitted,col="blue")

    ys<-depest[i]*qlnorm(c(0.1,0.25,0.75,0.9),0,prec[i])
    dosegs(ys,x=deps,y=fitted)

    location<-which.min((fitted-depest[i])^2)
    lines(rep(deps[location],2),c(0,fitted[location]),col='red',lwd=2)
    lines(rep(depest[i],2),c(0,depest[i]),col='green',lwd=2)
    lines(c(0,deps[location]),rep(depest[i],2),col='green',lwd=2)
    bce[i]<-deps[location]

  }

  mtext("Simulated depletion",1,outer=T,line=2)
  mtext("Estimated depletion",2,outer=T,line=2)

dev.off()


dosegs<-function(ys,x,y,cols=c('#00ff0040','#00ff0080','#ff000040','#ff000080')){
  nys<-length(ys)
  ny<-length(y)
  yarr<-array(rep(y,each=nys),c(nys,ny))
  ysarr<-array(ys,c(nys,ny))
  inds<-apply((yarr-ysarr)^2,1,which.min)

  fits<-y[inds]
  xs<-x[inds]
  xall<-x[inds[1]:inds[4]]
  fitall<-y[inds[1]:inds[4]]

  #outys
  polygon(c(0,xall,0),c(fitall[1],fitall,fitall[length(fitall)]),col=cols[1],border=NA)
  xall<-x[inds[2]:inds[3]]
  fitall<-y[inds[2]:inds[3]]
  polygon(c(0,xall,0),c(fitall[1],fitall,fitall[length(fitall)]),col=cols[2],border=NA)

  #outxz
  xall<-x[inds[1]:inds[4]]
  fitall<-y[inds[1]:inds[4]]
  polygon(c(xall[1],xall,xall[length(xall)]),c(0,fitall,0),col=cols[3],border=NA)
  xall<-x[inds[2]:inds[3]]
  fitall<-y[inds[2]:inds[3]]
  polygon(c(xall[1],xall,xall[length(xall)]),c(0,fitall,0),col=cols[3],border=NA)

}






