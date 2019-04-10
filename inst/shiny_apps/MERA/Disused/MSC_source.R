# ==================================================================
# == Code for Marine Stewardship Council - DLMtool application =====
# ==================================================================


MSCplot<-function(MSEobj,LTL=FALSE,zoom=NA,plot=T) {

  par(mfrow=c(1,2),mai = c(0.8, 0.8, 0.4, 0.02))

  yend <- max(MSEobj@proyears - 4, 1):MSEobj@proyears
  MPcex<-0.8
  # - Status plots -------------------------------------------------

  Status=rep("None",MSEobj@nMPs)

  if(!LTL){

    B50<-100*apply(MSEobj@B_BMSY[, , yend]>0.5, 2, mean, na.rm = T)
    B100<-100*apply(MSEobj@B_BMSY[, , yend]>1, 2, mean, na.rm = T)

    if(plot){
      plot(B50, B100, col = "white", xlab = "", ylab = "", axes = F,ylim=c(0,100),xlim=c(60,100), main="Status")
      MSC_status_zones()
      xs <- pretty(seq(min(B50), max(B50), length.out = 8))
      ys <- pretty(seq(min(B100), max(B100), length.out = 8))
      axis(1, xs, xs)
      axis(2, ys, ys)
      text(B50, B100, MSEobj@MPs, font = 2, cex = MPcex)
      mtext("Probability Biomass > 50% BMSY", 1, line = 2.5)
      mtext("Probability Biomass > BMSY", 2, line = 2.5)
    }

    Status[B50>50]<-"SG60"
    Status[B50>50&B100>50]<-"SG80"
    Status[B50>95&B100>50]<-"SG100"

  }else{

    SSB20<-100*apply((MSEobj@SSB[, , yend]/MSEobj@OM$SSB0)>0.2, 2, mean, na.rm = T)
    SSB75<-100*apply((MSEobj@SSB[, , yend]/MSEobj@OM$SSB0)>0.75, 2, mean, na.rm = T)

    if(plot){

      plot(SSB20, SSB75, col = "white", xlab = "", ylab = "",  axes = F,xlim=c(60,100),ylim=c(0,100), main="Status")
      MSC_status_zones_LTL()
      xs <- pretty(seq(60, 100, length.out = 8))
      ys <- pretty(seq(0,100, length.out = 8))
      axis(1, xs, xs)
      axis(2, ys, ys)
      text(SSB20, SSB75, MSEobj@MPs, font = 2, cex = MPcex)
      mtext("Probability Spawning biomass > 20% SSB0", 1, line = 2.5)
      mtext("Probability Spawning biomass > 75% SSB0", 2, line = 2.5)

    }

    Status[SSB20>70]<-"SG60"
    Status[SSB20>80 & SSB75>50]<-"SG80"
    Status[SSB20>95 & SSB75>95]<-"SG100"

  }

  # - Rebuilding plots -------------------------------------------------

  Rebuilding=rep("SG60",MSEobj@nMPs)

  na<-dim(MSEobj@CAA)[3]
  Z<-array(MSEobj@OM$M,c(MSEobj@nsim,na))
  MGTsurv<-t(exp(-apply(Z,1,cumsum)))
  agearray<-array(rep(1:na,each=MSEobj@nsim),c(MSEobj@nsim,na))
  ageMsd<-0.15
  Mat_age <- 1/(1 + exp((MSEobj@OM$ageM - agearray)/(MSEobj@OM$ageM * ageMsd)))
  MGT<-apply(agearray*(Mat_age*MGTsurv),1,sum)/apply(Mat_age*MGTsurv,1,sum)
  HZN<-ceiling(MGT*2)
  HZN[HZN>20]<-20
  HZN[HZN<5]<-5

  if(sum(HZN>MSEobj@proyears)>0){
    warning("At least one simulation needs a greater number of projected years to reach the rebuilding time horizon")
    print(paste("Projected number of years:",MSEobj@proyears))
    print("Required time horizon for projection for each simulation:")
    print(HZN)
    cond<-HZN>MSE@proyears
    print(paste(sum(cond),"simulations had rebuilding time artificially set to the last projection year"))
    HZN[cond]<-MSE@proyears
  }

  HZN1<-ceiling(MGT)
  HZN1<-max(HZN1,2)
  HZN1<-min(HZN1,10)

  RB1<-100*apply(MSEobj@B_BMSY[, , HZN1]>1, 2, mean, na.rm = T)
  RB2<-100*apply(MSEobj@B_BMSY[, , HZN]>1, 2, mean, na.rm = T)

  if(plot){
    plot(RB1, RB2, col = "white", xlab = "", ylab = "", axes = F,xlim=c(0,100),ylim=c(20,100), main="Rebuilding")
    MSC_rebuilding_zones()

    xs <- pretty(seq(0,100, length.out = 8))
    ys <- pretty(seq(0,100, length.out = 8))
    axis(1, xs, xs)
    axis(2, ys, ys)
    text(RB1, RB2, MSEobj@MPs, font = 2, cex = MPcex)
    mtext("Probability Biommass > BMSY (2 MGT)", 1, line = 2.5)
    mtext("Probability Biomass > BMSY (1 MGT)", 2, line = 2.5)
  }

  Rebuilding[RB2>70]<-"SG80"
  Rebuilding[RB1>95]<-"SG100"

  as.data.frame(cbind(MSEobj@MPs,Status,Rebuilding))

}


MSC_layout<-function(nMPs=1){

 design<-1:(nMPs*2)
 ncol<-floor(nMPs^0.5)
 nrow<-ceiling(nMPs/ncol)

 design2<-rep(nMPs*2+1,(ncol*nrow)*2)
 design2[1:length(design)]<-design
 mat<-t(matrix(design2,nrow=ncol*2))
 par(mai=c(0.3,0.4,0.3,0.1),omi=c(0.5,0.5,0.1,0.05))
 layout(mat,widths=rep(c(4,1.9),ncol),heights=rep(4,nrow))
}


MSC_layout_app<-function(nMPs=1){

  design<-1:(nMPs*2)
  ncol<-min(nMPs,3)
  nrow<-ceiling(nMPs/ncol)

  design2<-rep(nMPs*2+1,(ncol*nrow)*2)
  design2[1:length(design)]<-design
  mat<-t(matrix(design2,nrow=ncol*2))
  par(mai=c(0.3,0.4,0.3,0.1),omi=c(0.5,0.5,0.1,0.05))
  layout(mat,widths=rep(c(4,1.9),ncol),heights=rep(4,nrow))
}



MSC_layout_exp<-function(){

  #design<-c(rep(1,32),rep(c(2,2,2,2,5,5,5,5),3),rep(c(3,3,3,4,6,6,6,7),3))
  design<-c(rep(1,24),rep(c(2,2,2,3,4,4,4,5),3))

  mat<-t(matrix(design,nrow=8))
  par(mai=c(0.4,0.4,0.4,0.1),omi=c(0.4,0.5,0.1,0.05))
  layout(mat)#,widths=rep(c(4,1.9),ncol),heights=rep(4,nrow))
}

MSC_layout_exp2<-function(){

  #design<-c(rep(1,32),rep(c(2,2,2,2,5,5,5,5),3),rep(c(3,3,3,4,6,6,6,7),3))
  design<-c(rep(1,24),rep(c(2,2,3,3,4,4,5,5),3))

  mat<-t(matrix(design,nrow=8))
  par(mai=c(0.4,0.4,0.4,0.1),omi=c(0.4,0.5,0.1,0.05))
  layout(mat)#,widths=rep(c(4,1.9),ncol),heights=rep(4,nrow))
}


# some testing
# nMPs<-2
# MSC_layout(nMPs=nMPs)
# for(i in 1:(nMPs*2))plot(1:10)

MSC_Uplot<-function(MSEobj,MPno=1,LTL=F,pcex=0.8,lev=80,labs=T,plot=T,highlight=NA,inc_thresh=F,burnin=10){

  yend <- max(MSEobj@proyears - 4, 1):MSEobj@proyears
  MPcex<-0.8

  Status=rep("None",MSEobj@nsim)
  xs<-ys<-c(-20,0,20,40,60,80,100)
  nsim<-MSEobj@nsim

  if(!LTL){

    B50<-100*apply(MSEobj@B_BMSY[,MPno ,1:burnin]>0.5,1,mean)
    B100<-100*apply(MSEobj@B_BMSY[,MPno,1:burnin]>1, 1, mean)
    B50[B50<60]<-60

    agg<-aggregate(rep(1,nsim),by=list(B50,B100),sum)
    names(agg)<-c("B50","B100","N")
    maxcex=2.5
    cexy<-0.15+sqrt(agg$N)/max(sqrt(agg$N))*maxcex

    if(plot){

      plot(agg$B50, agg$B100, col = "white", xlab = "", ylab = "", axes = F,ylim=c(0,100),xlim=c(60,100))
      MSC_status_zones(highlight=highlight)

      axis(1, xs, xs)
      axis(2, ys, ys)
      points(agg$B50, agg$B100, pch = 19, cex = cexy,lwd=2,col="red")
      #cond<-B50<60
      #points(rep(60,sum(cond)),B50[cond],pch=3,cex = pcex,lwd=2)
      if(labs)mtext("Probability Biomass > 50% BMSY", 1, line = 2.5)
      if(labs)mtext("Probability Biomass > BMSY", 2, line = 2.5)

    }

    Status[B50>70]<-"SG60"
    Status[B50>70&B100>50]<-"SG80"
    Status[B50>95&B100>50]<-"SG100"

  }else{

    SSB20<-100*apply((MSEobj@SSB[, MPno, 1:burnin]/MSEobj@OM$SSB0)>0.2, 1, mean, na.rm = T)
    SSB75<-100*apply((MSEobj@SSB[, MPno, 1:burnin]/MSEobj@OM$SSB0)>0.75, 1, mean, na.rm = T)

    if(plot){
      plot(SSB20, SSB75, col = "white", xlab = "", ylab = "",  axes = F,xlim=c(60,100),ylim=c(0,100), main="Status")
      axis(1, xs, xs)
      axis(2, ys, ys)

      MSC_status_zones_LTL(highlight=highlight)

      points(SSB20, SSB75, pch = 3, cex = pcex,lwd=2)
      if(labs)mtext("Probability Spawning biomass > 20% SSB0", 1, line = 2.5)
      if(labs)mtext("Probability Spawning biomass > 75% SSB0", 2, line = 2.5)
    }

    Status[SSB20>70]<-"SG60"
    Status[SSB20>80 & SSB75>50]<-"SG80"
    Status[SSB20>95 & SSB75>95]<-"SG100"

  }

  freq<-aggregate(rep(1,MSEobj@nsim),by=list(Status),sum)
  #scores<-c("None","SG60","SG80","SG100")
  scores<-c("SG100","SG80","SG60","None")
  freq2<-rep(0,4)
  freq2[match(freq[,1],scores)]<-freq[,2]
  cumfreq<-cumsum(freq2)
  cumfreq<-c(0,cumfreq/max(cumfreq)*100)
  i<-match('TRUE',cumfreq>lev)-1
  scores[i]
}

MSC_Ubar<-function(MSEobj, MPno=1,LTL=F, lev=80,inc_thresh=F,burnin=10){

  Status=rep("None",MSEobj@nsim)
  if(!LTL){

    B50<-100*apply(MSEobj@B_BMSY[,MPno ,1:burnin]>0.5,1,mean)
    B100<-100*apply(MSEobj@B_BMSY[,MPno,1:burnin]>1, 1, mean)
    Status[B50>70]<-"SG60"
    Status[B50>70&B100>50]<-"SG80"
    Status[B50>95&B100>50]<-"SG100"

  }else{

    SSB20<-100*apply((MSEobj@SSB[, MPno, 1:burnin]/MSEobj@OM$SSB0)>0.2, 1, mean, na.rm = T)
    SSB75<-100*apply((MSEobj@SSB[, MPno, 1:burnin]/MSEobj@OM$SSB0)>0.75, 1, mean, na.rm = T)
    Status[SSB20>70]<-"SG60"
    Status[SSB20>80 & SSB75>50]<-"SG80"
    Status[SSB20>95 & SSB75>95]<-"SG100"

  }

  plot(c(0,2),c(0,100),col='white',axes=F,xlab="",ylab="")
  yx<-(0:10)*10
  yl<-c(0,NA,20,NA,40,NA,60,NA,80,NA,100)
  axis(2,yx,yl)
  mtext("Cumulative % simulations",2,line=2.2,cex=0.8)

  freq<-aggregate(rep(1,MSEobj@nsim),by=list(Status),sum)
  #scores<-c("None","SG60","SG80","SG100")
  scores<-c("SG100","SG80","SG60","None")
  freq2<-rep(0,4)
  freq2[match(freq[,1],scores)]<-freq[,2]
  cumfreq<-cumsum(freq2)
  cumfreq<-c(0,cumfreq/max(cumfreq)*100)

  mid<-(cumfreq[1:4]+cumfreq[2:5])/2
  #cols<-c('green3','green2','chartreuse','white')
  cols=c("white",'#b3ecff',rgb(0.4,0.8,0.95),"dodgerblue3")[4:1]

  bcols<-c('black',rep('white',3))[4:1]


  xvals<-c(0.25,1,1,0.25)
  if(inc_thresh)abline(h=lev,col='red',lty=2)
  for(i in 1:4){

    polygon(xvals,c(rep(cumfreq[i],2),rep(cumfreq[i+1],2)),col=cols[i],border=bcols[i])

  }

  if(inc_thresh){
    i<-match('TRUE',cumfreq>lev)-1
    polygon(xvals,c(rep(cumfreq[i],2),rep(cumfreq[i+1],2)),col=cols[i],border="red")
  }

  tcols<-c("black",'#b3ecff',rgb(0.4,0.8,0.95),"dodgerblue3")[4:1]
  text(rep(1.5,4),mid,scores,col=tcols,font=2)

}



MSC_bar2<-function(MSEobj, MPno=1,LTL=F, lev=80){

  Status=rep("None",MSEobj@nsim)

  if(!LTL){

    B50<-100*apply(MSEobj@B_BMSY[,MPno ,]>0.5,1,mean)
    B100<-100*apply(MSEobj@B_BMSY[,MPno,]>1, 1, mean)
    Status[B50>70]<-"SG60"
    Status[B50>70&B100>50]<-"SG80"
    Status[B50>95&B100>50]<-"SG100"

  }else{

    SSB20<-100*apply((MSEobj@SSB[, MPno, ]/MSEobj@OM$SSB0)>0.2, 1, mean, na.rm = T)
    SSB75<-100*apply((MSEobj@SSB[, MPno, ]/MSEobj@OM$SSB0)>0.75, 1, mean, na.rm = T)
    Status[SSB20>70]<-"SG60"
    Status[SSB20>80 & SSB75>50]<-"SG80"
    Status[SSB20>95 & SSB75>95]<-"SG100"

  }

  # plot(c(0,2),c(0,100),col='white',axes=F,xlab="",ylab="")
  yx<-(0:10)*10
  yl<-c(0,NA,20,NA,40,NA,60,NA,80,NA,100)
  axis(2,yx,yl)
  scores<-c("None","SG60","SG80","SG100")
  freq<-aggregate(rep(1,MSEobj@nsim),by=list(Status),sum)
  freq<-freq[order(match(freq[,1],scores)),]
  #scores<-c("None","SG60","SG80","SG100")

  freq2<-rep(0,4)
  freq2[match(freq[,1],scores)]<-freq[,2]
  freq2<-freq2[4:1]
  cumfreq<-cumsum(freq2)
  cumfreq<-cumfreq/max(cumfreq)*100
  ind<-4-match(TRUE,cumfreq>lev)+1

  tcols<-c('white','chartreuse','green2','green3')
  bcols<-rep('white',4)
  bcols[ind]<-'red'
  colind<-match(freq[,1],scores)
  barplot(freq$x/sum(freq$x)*100,col=tcols[colind],border=bcols[colind],names.arg=freq[,1])

}



MSC_bar2<-function(MSEobj, MPno=1,LTL=F, lev=80){

  Status=rep("None",MSEobj@nsim)

  if(!LTL){

    B50<-100*apply(MSEobj@B_BMSY[,MPno ,]>0.5,1,mean)
    B100<-100*apply(MSEobj@B_BMSY[,MPno,]>1, 1, mean)
    Status[B50>70]<-"SG60"
    Status[B50>70&B100>50]<-"SG80"
    Status[B50>95&B100>50]<-"SG100"

  }else{

    SSB20<-100*apply((MSEobj@SSB[, MPno, ]/MSEobj@OM$SSB0)>0.2, 1, mean, na.rm = T)
    SSB75<-100*apply((MSEobj@SSB[, MPno, ]/MSEobj@OM$SSB0)>0.75, 1, mean, na.rm = T)
    Status[SSB20>70]<-"SG60"
    Status[SSB20>80 & SSB75>50]<-"SG80"
    Status[SSB20>95 & SSB75>95]<-"SG100"

  }

  # plot(c(0,2),c(0,100),col='white',axes=F,xlab="",ylab="")
  yx<-(0:10)*10
  yl<-c(0,NA,20,NA,40,NA,60,NA,80,NA,100)
  axis(2,yx,yl)
  scores<-c("None","SG60","SG80","SG100")
  freq<-aggregate(rep(1,MSEobj@nsim),by=list(Status),sum)
  for(i in 1:length(scores))if(!(scores[i]%in%freq$Group.1))freq<-rbind(freq,c(scores[i],0))
  freq<-freq[order(match(freq[,1],scores)),]
  freq$x<-as.numeric(freq$x)
  #scores<-c("None","SG60","SG80","SG100")

  #tcols<-c('white','chartreuse','green2','green3')
  tcols=c("white",'#b3ecff',rgb(0.4,0.8,0.95),"dodgerblue3")
  bcols<-c('black',rep('white',3))
  colind<-match(freq[,1],scores)
  barplot(freq$x/sum(freq$x)*100,col=tcols[colind],border=bcols[colind],names.arg=freq[,1])

}

MSC_uncertain<-function(MSEobj,maxMPs=8,lev=80,LTL=F,inc_thresh=F,burnin=10,MPcols="NA"){

  if(is.na(MPcols[1]))MPcols=rep('black',maxMPs)
  MPcols[MPcols=='green']<-'darkgreen'
  maxMPs<-min(maxMPs,MSEobj@nMPs)

  MSC_layout_app(maxMPs)

  for(i in 1:maxMPs){

    score<-MSC_Uplot(MSEobj,MPno=i,labs=F,lev=lev,LTL=LTL,inc_thresh=inc_thresh,burnin=burnin)
    if(i ==1 & inc_thresh)legend('topleft',legend=c("a simulation","selection threshold"),pch=c(19,NA),lwd=c(2,2),lty=c(NA,1),col=c('red','red'),cex=0.9,text.col=c("black","red"))
    if(i ==1 & !inc_thresh)legend('topleft',legend="a simulation",pch=19,lwd=2,lty=NA,col='red',cex=0.9,text.col="black")

    if(inc_thresh)mtext(paste0(MSEobj@MPs[i]," (",score,")"),3,col=MPcols[i],line=0.4,adj=0.8,font=2)
    if(!inc_thresh)mtext(MSEobj@MPs[i],3,col=MPcols[i],line=0.4,adj=0.8,font=2)
    MSC_Ubar(MSEobj,MPno=i,lev=lev,LTL=LTL,inc_thresh=inc_thresh,burnin=burnin)

  }

  if(LTL){
    mtext("Probability Spawning biomass > 20% SSB0", 1, line = 1.5,outer=T)
    mtext("Probability Spawning biomass > 75% SSB0", 2, line = 1.5,outer=T)
  }else{
    mtext("Probability Biomass > 50% BMSY", 1, line = 1.5,outer=T)
    mtext("Probability Biomass > BMSY", 2, line = 1.5,outer=T)
  }


}


MSC_proj<-function(MSEobj,MP=1,LTL=F,yrslist=NA){

  if(!LTL){

    temp1<-t(MSEobj@B_BMSY[,MP,])

  }else{

    temp1<-t(MSEobj@SSB[,MP,]/MSEobj@OM$SSB0)

  }

  temp1[temp1>5]<-NA
  matplot(temp1,col="dark grey",lty=1,type="l",xlab="")


  mtext("Projection year",1,cex=0.9,line=2)
  mtext("Biomass / BMSY", 2, line = 2.5)

  if(!is.na(yrslist[[1]])){

  #section_cols=c('red',"blue","purple")
  section_cols=rep('black',4)
  Status<-new('list')
  for(j in 1:length(yrslist)){

    Status[[j]]=rep("None",MSEobj@nsim)

    if(!LTL){

      B50<-100*apply(MSEobj@B_BMSY[,MP ,yrslist[[j]]]>0.5,1,mean)
      B100<-100*apply(MSEobj@B_BMSY[,MP,yrslist[[j]]]>1, 1, mean)
      Status[[j]][B50>70]<-"SG60"
      Status[[j]][B50>70&B100>50]<-"SG80"
      Status[[j]][B50>95&B100>50]<-"SG100"

    }else{

      SSB20<-100*apply((MSEobj@SSB[, MP,yrslist[[j]] ]/MSEobj@OM$SSB0)>0.2, 1, mean, na.rm = T)
      SSB75<-100*apply((MSEobj@SSB[, MP,yrslist[[j]] ]/MSEobj@OM$SSB0)>0.75, 1, mean, na.rm = T)
      Status[[j]][SSB20>70]<-"SG60"
      Status[[j]][SSB20>80 & SSB75>50]<-"SG80"
      Status[[j]][SSB20>95 & SSB75>95]<-"SG100"

    }

    scores<-c("SG100","SG80","SG60","None")
    cols<-c('green3','green2','chartreuse','light grey')
    lcols<-cols[match(Status[[j]],scores)]

    if(!LTL){
      temp=t(MSEobj@B_BMSY[,MP,yrslist[[j]]])
    }else{
      temp=t(MSEobj@SSB[,MP,yrslist[[j]]]/MSEobj@OM$SSB0)
    }
    matplot(yrslist[[j]],temp,col=lcols,add=T,type="l",lty=1)

    abline(v=range(yrslist[[j]]),col=section_cols[j])
    text(mean(yrslist[[j]]),max(temp1,na.rm=T)*0.97,paste("Years", min(yrslist[[j]]),"-", max(yrslist[[j]])),col=section_cols[j])
  }
  } # if yrslist !na

  if(!LTL){
    abline(h=c(0.5,1),lty=2)
  }else{
    abline(h=c(0.2,0.75),lty=2)
  }

}






MSC_explanatory<-function(MSEobj,MP=1,lev=80,LTL=F,Stock="unspecified", yrslist=list(6:20,31:45),hist=F){


  if(!hist)MSC_layout_exp()
  if(hist)MSC_layout_exp2()

  MSC_proj(MSEobj,MP=MP,LTL=LTL,yrslist=yrslist)

  for(j in 1:length(yrslist)){

    MSEobj2<-MSEobj
    MSEobj2@B_BMSY<-MSEobj@B_BMSY[,,yrslist[[j]]]
    MSEobj2@SSB<-MSEobj@SSB[,,yrslist[[j]]]

    score<-MSC_Uplot(MSEobj2,MPno=MP,labs=F,lev=lev,LTL=LTL,plot=F)
    score<-MSC_Uplot(MSEobj2,MPno=MP,labs=F,lev=lev,LTL=LTL,highlight=score)
    if(j==1){
    if(LTL){
      mtext("Probability Spawning biomass > 75% SSB0", 2, line = 2.5)
    }else{
      mtext("Probability Biomass > BMSY", 2, line = 2.5)
    }
    }
    mtext(paste0("Years ", min(yrslist[[j]])," - ", max(yrslist[[j]]),"  (Score: ",score,")"),3,line=0.5,adj=0.7,paste("Years", min(yrslist[[j]]),"-", max(yrslist[[j]])),cex=0.9)
    if(j ==1)legend('topleft',legend=c("a simulation","selection threshold"),pch=c(3,NA),lwd=c(2,2),lty=c(NA,1),col=c('black','red'),cex=0.9,text.col=c("black","red"))
    #mtext(paste0(MSEobj@MPs[i]," (",score,")"),3,line=0.4,adj=0.8,font=2)
    if(!hist)MSC_Ubar(MSEobj2,MPno=MP,lev=lev,LTL=LTL)
    if(hist)MSC_bar(MSEobj2,MPno=MP,lev=lev, LTL=LTL)

  }

  if(LTL){
    mtext("Probability Spawning biomass > 20% SSB0", 1, line = 1,outer=T)
  }else{
    mtext("Probability Biomass > 50% BMSY", 1, line = 1,outer=T)
  }

  mtext(paste("Biomass projections for",Stock),3,line=-1.6,outer=T)

}



MSC_status_zones<-function(highlight=NA){

  SG60<-'#b3ecff'
  SG80<-rgb(0.4,0.8,0.95)
  SG100<-"dodgerblue3"


  textcol='white'
  textcex<-0.9

  polygon(c(70,100,100,80,80,70),c(0,0,50,50,100,100),col=SG60,border=SG60)
  polygon(c(80,95,95,80),c(50,50,100,100),col=SG80,border=SG80)
  polygon(c(95,100,100,95),c(50,50,100,100),col=SG100,border=SG100)

  if(!is.na(highlight)){

    scores<-c("None","SG60","SG80","SG100")
    lev<-match(highlight,scores)
    if(lev==1)polygon(c(60,100,100,60),c(0,0,100,100),col=NA,border="red")
    if(lev==2)polygon(c(70,100,100,70),c(0,0,100,100),col=NA,border="red")
    if(lev==3)polygon(c(80,100,100,80),c(50,50,100,100),col=NA,border="red")
    if(lev==4)polygon(c(95,100,100,95),c(50,50,100,100),col=NA,border="red")

  }

  text(97.5,75,"SG100",col=textcol,font=2,srt=90,cex=textcex)
  text(87.5,75,"SG80",col=textcol,font=2,srt=90,cex=textcex)
  text(85,35,"SG60",col=textcol,font=2,cex=textcex)

}

MSC_status_zones_LTL<-function(highlight=NA){

  SG60<-'#b3ecff'
  SG80<-rgb(0.4,0.8,0.95)
  SG100<-"dodgerblue3"

  textcol='white'
  textcex<-0.9

  polygon(c(70,80,80,70),c(0,0,100,100),col=SG60,border=SG60)
  polygon(c(80,100,100,80),c(0,0,50,50),col=SG60,border=SG60)
  polygon(c(80,95,95,80),c(50,50,100,100),col=SG80,border=SG80)
  polygon(c(95,100,100,95),c(50,50,95,95),col=SG80,border=SG80)
  polygon(c(95,100,100,95),c(95,95,100,100),col=SG100,border=SG100)

  text(97.5,97.5,"SG100",col=textcol,font=2,cex=textcex*0.6)
  text(90,72.5,"SG80",col=textcol,font=2,cex=textcex)
  text(85,35,"SG60",col=textcol,font=2,cex=textcex)

}


MSC_rebuilding_zones<-function(){

  SG60<-'chartreuse'
  SG80<-'green2'
  SG100<-'green3'

  textcol='white'
  textcex<-0.9

  polygon(c(0,100,100,0),c(0,0,100,100),col=SG60,border=SG60)
  polygon(c(70,100,100,70),c(0,0,100,100),col=SG80,border=SG80)
  polygon(c(0,100,100,0),c(95,95,100,100),col=SG100,border=SG100)

  text(50,97.5,"SG100",col=textcol,font=2,cex=textcex*0.6)
  text(85,55,"SG80",col=textcol,font=2,cex=textcex)
  text(35,55,"SG60",col=textcol,font=2,cex=textcex)

}


P122<-function(OM,nsim=50,MPs=c("FMSYref","curE","DD","DD4010","AvC","DCAC","DBSRA","DBSRA4010","matlenlim")){

  myOM<-OM
  myOM@nsim<-nsim
  myOM@cpars$D<-seq(0.05,0.4,length.out=nsim)

  # assign deterministic values to lower bound
  #slots<-slotNames('OM')
  #for(i in 18:length(slots)){
  #  temp<-slot(myOM,slots[i])
  #  if(slots[i]!="cpars" & !is.na(temp[1]) & length(temp)==2 & temp[1]!=temp[2]){
  #    slot(myOM,slots[i])<-rep(temp[1],2)
  #  }
  #}

  myMSE<-runMSE(myOM,MPs=MPs)

  nrow<-ceiling(length(MPs)^0.5)
  ncol<-floor(length(MPs)/nrow)

  par(mfrow=c(nrow,ncol),mar=c(2,2,0.5,0.5),omi=c(0.4,0.4,0.05,0.05))

  for(i in 1:length(MPs)){

    cond<-myMSE@B_BMSY[,i,1]<1 & myMSE@B_BMSY[,i,1]>0.5

    plot(myMSE@B_BMSY[!cond,i,1],myMSE@F_FMSY[!cond,i,1],col="#99999998",pch=19)


    dat<-data.frame(x=myMSE@B_BMSY[cond,i,1],y=myMSE@F_FMSY[cond,i,1])
    temp<-lm(y~x,dat=dat)
    stemp<-summary(temp)
    Pval<-stemp$coefficients[2,4]
    Slope<-stemp$coefficient[2,1]
    pos<-Slope>0

    if(pos&Pval<0.025){
      cols<-"#00ff0090" # green
    }else{
      cols<-"#ff000090" # red
    }
    points(myMSE@B_BMSY[cond,i,1],myMSE@F_FMSY[cond,i,1],col=cols,pch=19)
    abline(v=c(0.5,1),lty=2,col='#99999980')

    preddat<-data.frame(x=seq(0.5,1,length.out=10))
    predy<-predict(temp,preddat)
    lines(preddat$x,predy,col=cols)
    legend('top',MPs[i],bty='n',text.col=cols,text.font=2)
    legend('right',c(paste0("Slope: ",round(Slope,2)),paste0("p: ",round(Pval,3))),text.col=cols,bty='n')

  }

  mtext("B/BMSY",1,line=1,outer=T)
  mtext("F/FMSY",2,line=0.8,outer=T)

}




MSC_U<-function(MSEobj, MPno=1,LTL=F, lev=80){

  Status=rep("None",MSEobj@nsim)

  if(!LTL){

    B50<-100*apply(MSEobj@B_BMSY[,MPno ,]>0.5,1,mean)
    B100<-100*apply(MSEobj@B_BMSY[,MPno,]>1, 1, mean)
    Status[B50>70]<-"SG60"
    Status[B50>70&B100>50]<-"SG80"
    Status[B50>95&B100>50]<-"SG100"

  }else{

    SSB20<-100*apply((MSEobj@SSB[, MPno, ]/MSEobj@OM$SSB0)>0.2, 1, mean, na.rm = T)
    SSB75<-100*apply((MSEobj@SSB[, MPno, ]/MSEobj@OM$SSB0)>0.75, 1, mean, na.rm = T)
    Status[SSB20>70]<-"SG60"
    Status[SSB20>80 & SSB75>50]<-"SG80"
    Status[SSB20>95 & SSB75>95]<-"SG100"

  }

  freq<-aggregate(rep(1,MSEobj@nsim),by=list(Status),sum)
  #scores<-c("None","SG60","SG80","SG100")
  scores<-c("SG100","SG80","SG60","None")
  freq2<-rep(0,4)
  freq2[match(freq[,1],scores)]<-freq[,2]
  cumfreq<-cumsum(freq2)
  cumfreq<-c(0,cumfreq/max(cumfreq)*100)

  mid<-(cumfreq[1:4]+cumfreq[2:5])/2
  i<-match('TRUE',cumfreq>lev)-1
  5-i

}


MSC_Usim<-function(MSEobj, MPno=1,LTL=F, lev=80,output="Numeric",yrs=NA){

  Status=rep("None",MSEobj@nsim)
  if(is.na(yrs))yrs<-1:MSEobj@proyears

  if(!LTL){

    B50<-100*apply(MSEobj@B_BMSY[,MPno ,yrs]>0.5,1,mean)
    B100<-100*apply(MSEobj@B_BMSY[,MPno,yrs]>1, 1, mean)
    Status[B50>70]<-"SG60"
    Status[B50>70&B100>50]<-"SG80"
    Status[B50>95&B100>50]<-"SG100"

  }else{

    SSB20<-100*apply((MSEobj@SSB[, MPno, yrs]/MSEobj@OM$SSB0)>0.2, 1, mean, na.rm = T)
    SSB75<-100*apply((MSEobj@SSB[, MPno, yrs]/MSEobj@OM$SSB0)>0.75, 1, mean, na.rm = T)
    Status[SSB20>70]<-"SG60"
    Status[SSB20>80 & SSB75>50]<-"SG80"
    Status[SSB20>95 & SSB75>95]<-"SG100"

  }


  if(output=="Numeric"){

    return(match(Status,c("None","SG60","SG80","SG100")))

  }else{

    return(Status)

  }


}



VOI_MSC<-function(MSEobj,LTL=F,lev=80, ncomp = 10, nbins = 5,maxrow = 8,yrs=46:50,MPcols=NA){

  # MSEobj<-MSEobj_reb
  if(is.na(MPcols[1]))MPcols=rep('black',MSEobj@nMPs)
  MPcols[MPcols=="green"]<-'dark green'

  Utnam <- "Variability in probability of certification"
  MPs <- MSEobj@MPs

  nMPs <- MSEobj@nMPs
  nsim <- MSEobj@nsim

  Ut <- array(NA, c(nsim, nMPs))
  for(mp in 1:nMPs){
    Ut[, mp]=apply(MSEobj@C[,mp,1:10],1,mean,na.rm=T)/MSEobj@OM$RefY
      #as.integer(MSC_Usim(MSEobj, MPno=mp,LTL=F, lev=80)>1, yrs=yrs)
  }

  # remove correlative variables and those that are constant
  onlycor <- c("RefY", "A", "MSY", "Linf", "t0", "OFLreal", "Spat_targ")
  vargood <- (apply(MSEobj@OM, 2, sd,na.rm=T)/(apply(MSEobj@OM, 2, mean,na.rm=T)^2)^0.5) >  0.05
  vargood[is.na(vargood)]<-FALSE
  vargood[grep("qvar", names(MSEobj@OM))] <- FALSE
  allunique<-function(x)length(unique(x))==length(x)
  continuous<-apply(MSEobj@OM,2,allunique)
  MSEobj@OM <- MSEobj@OM[, which((!names(MSEobj@OM) %in% onlycor) & vargood & continuous)]

  obsgood <- (apply(MSEobj@Obs, 2, sd,na.rm=T)/(apply(MSEobj@Obs, 2, mean,na.rm=T)^2)^0.5) >  0.05
  obsgood[is.na(obsgood)]<-FALSE
  MSEobj@Obs <- MSEobj@Obs[, obsgood]


  OMp <- apply(MSEobj@OM, 2, quantile, p = seq(0, 1, length.out = nbins + 1), na.rm = TRUE)
  Obsp <- apply(MSEobj@Obs, 2, quantile, p = seq(0, 1, length.out = nbins + 1), na.rm = TRUE)

  OMv <- array(NA, c(nMPs, ncol(MSEobj@OM), nbins))
  Obsv <- array(NA, c(nMPs, ncol(MSEobj@Obs), nbins))

  for (mm in 1:nMPs) {
    for (j in 1:nbins) {
      for (i in 1:ncol(MSEobj@OM)) {
        cond <- MSEobj@OM[, i] > OMp[j, i] & MSEobj@OM[, i] < OMp[j + 1, i]
        OMv[mm, i, j] <- mean(Ut[cond, mm], na.rm = T)
      }
      for (i in 1:ncol(MSEobj@Obs)) {
        cond <- MSEobj@Obs[, i] > Obsp[j, i] & MSEobj@Obs[, i] < Obsp[j + 1, i]
        Obsv[mm, i, j] <- mean(Ut[cond, mm], na.rm = T)
      }
    }
  }

  # -- Operating model variables
  OMs <- apply(OMv, 1:2, sd, na.rm = T)
  Obss <- apply(Obsv, 1:2, sd, na.rm = T)
  Obsnam<-names(MSEobj@Obs)

  # Cols ----------

  # Plot Obs ------------------

  nrow=ceiling(nMPs^0.5)
  ncol=floor(nMPs/nrow)
  xlims<-ncol*4

  # set up the plot
  layout(topcorner(nrow,ncol,nMPs),widths=rep(c(1,1),ncol),heights=rep(c(1.6,1),nrow))
  par(mai=c(1,0.4,0.15,0.1),omi=c(0.4,0.4,0.15,0.01))
  obslim<-c(0,max(Obss))

  for(mp in 1:nMPs){

    pp<-mean(Ut[, mp],na.rm=T)
    nsamp<-floor(nsim/nbins)
    samped<-matrix(rbinom(1000*nbins,nsamp,pp)/nsamp,c(1000,nbins))
    sds<-apply(samped,1,sd,na.rm=T)
    mus<-quantile(sds,p=c(0.75,0.95,0.99),na.rm=T)

    samped2<-matrix(rbinom(1000*nbins,floor(nsamp/4),pp)/floor(nsamp/4),c(1000,nbins))
    sds2<-apply(samped2,1,sd,na.rm=T)

    ncols <- 100
    greenlim<-mus[1]
    if(is.na(greenlim))greenlim=0.4
    redlim<-quantile(sds2,p=0.99,na.rm=T)
    if(is.na(redlim))redlim=0.8
    maxobs<-sd(c(rep(0,floor(nbins/2)),rep(1,ceiling(nbins/2))),na.rm=T)
    totcols<-ceiling(ncols*maxobs/(redlim-greenlim))
    if(is.na(totcols))totcols<-ceiling(ncols*0.2)

    coly <- rainbow(ncols, start = 0.0, end = 0.25)[ncols:1]
    colsse<-rep(coly[1],totcols)
    colsse[((greenlim/maxobs)*totcols)+(1:ncols)]<-coly
    colsse[ceiling(totcols*redlim/maxobs):totcols]<-coly[ncols]

    ord<-order(Obss[mp,],decreasing=T)[1:ncomp]
    colno <- ceiling(Obss[mp,ord]/maxobs * totcols)
    temp<- Obss[mp,ord]
    ylim<-c(0,temp[length(temp)]*3)
    barplot(temp,names.arg=Obsnam[ord],las=2,border=NA,ylim=ylim,col=colsse[colno])


    abline(h=mus, col=colsse[ceiling(mus/maxobs * totcols)])

    barplot(Obss[mp,ord],names.arg=Obsnam[ord],las=2,border=NA,ylim=obslim,col=colsse[colno],add=T)
    mtext(MSEobj@MPs[mp],3,adj=0.05,col=MPcols[1],line=0.25,font=2)

    linefunc(x1=0.75,x2=ncomp*0.5,y1=Obss[mp,ord[1]], y2=obslim[2],col=colsse[colno[1]],gap=0.15)

    par(bg = 'white')
    mids<-(Obsp[,ord[1]][1:nbins]+Obsp[,ord[1]][1+(1:nbins)])/2

    barplot(Obsv[mp,ord[1],],names.arg=round(mids,2),col=colsse[colno[1]],border='white')
    mtext(paste0("StDev = ",round(Obss[mp,ord[1]],2)),3,line=0.1,cex=0.75)
    mtext(Obsnam[ord[1]],1,line=2.5,cex=0.8)
    mtext("P(SG rating)",2,line=2.5,cex=0.8)
  }

  mtext("Observation Variable",1,line=0.9,outer=T,font=2)
  mtext("Variability in Long Term Yield (LTY)",2,line=0.9,outer=T,font=2)
  #mtext(MSEobj@MPs[1],3,line=0.3,col=MPcols[1],outer=T,font=2)

}


linefunc<-function(x1,x2,y1,y2,col,gap=0.1){
  if(y2<y1)y1<-0.9*y2
  np<-100
  x<-seq(x1,x2,length.out=np)
  a=0.1
  d=0.4
  for(i in 1:20){  # converges luckily since my algebra is crapola
    c=y1-(a*x1^d)
    a=(y2-c)/x2^d
  }
  y=a*x^d+c
  ind<-ceiling(np*gap):np
  x<-x[ind]
  y<-y[ind]
  lines(x,y,col=col,lwd=2)
  arrows(x[2],y[2],x[1],y[1],lwd=2,col=col,length=0.125)
}


topcorner<-function(nrow,ncol,nMPs){
  st<-rep((1:nMPs)*2-1,each=2)
  mat1<-t(matrix(st,ncol=nrow))
  mat<-matrix(NA,ncol=ncol*2,nrow=nrow*2)
  mat[(1:nrow)*2-1,]<-mat1[1:nrow,]
  mat[(1:nrow)*2,]<-mat1[1:nrow,]
  ind<-cbind(rep((1:nrow)*2-1,ncol),rep((1:ncol)*2,each=nrow))
  mat[ind]<- mat[ind]+1
  mat
}
