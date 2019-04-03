slp<-function(x,mat,ind){
  y<-log(mat[x,ind])
  if(sum(!is.na(y))<2){
    return(NA)
  }else{
    return(lm(y~x1,data.frame(x1=1:length(ind),y=log(mat[x,ind]+1E-10)))$coef[2])
  }
}

slp2<-function(x,mat,ind){
  x1<-1:length(ind)
  y=log(mat[x,ind])
  mux<-mean(x1)
  muy<-mean(y)
  SS<-sum((x1-mux)^2)
  (1/SS)*sum((x1-mux)*(y-muy))
}

AAV<-function(x,mat,ind){
  ni<-length(ind)
  mean(abs((mat[x,ind[2:ni]]-mat[x,ind[1:(ni-1)]])/mat[x,ind[1:(ni-1)]]))
}

mu<-function(x,mat,ind){
  log(mean(mat[x,ind],na.rm=T))
}

getinds<-function(PPD,styr,res, tsd= c("Cat","Cat","Cat","Ind","ML"),stat=c("slp","AAV","mu","slp", "slp")){
  nsim<-dim(PPD@Cat)[1]
  proyears<-dim(PPD@Cat)[2]-styr+1

  if(res>proyears)message(paste0("The temporal resolution for posterior predictive data calculation (",res,") is higher than the number of projected years (",proyears,"). Only one time step of indicators are calculated for ",proyears, " projected years."))
  np<-1#floor(proyears/res)

  ntsd<-length(tsd)
  inds<-array(NA,c(ntsd,1,nsim))

  for(i in 1:ntsd){
    for(pp in 1:np){
      ind<-(styr+((pp-1)*res)+1:res)-1
      inds[i,pp,]<-sapply(1:nsim,get(stat[i]),mat=slot(PPD,tsd[i]),ind=ind)
    }
  }
  inds
}


CC<-function(indPPD,indData,pp=1,dnam=c("CS","CV","CM","IS","IM","MLS","MLM"),res=6){

  if(pp>1)namst<-paste(rep(dnam,pp),rep((1:pp)*res,each=length(dnam)))
  if(pp==1)namst=dnam
  cols<-c("#0000ff50","orange")
  ntsd<-dim(indPPD)[1]
  ni<-pp*ntsd
  ind2PPD<-matrix(indPPD[,1:pp,],nrow=ni)
  ind2Data<-matrix(indData[,1:pp,1],nrow=ni)
  par(mfrow=c(ni-1,ni-1),mai=rep(0,4),omi=c(0.5,0.75,0,0.05))

  for(i in 2:ni){

    for(j in 1:(ni-1)){

      if(j==i|j>i){

        plot(1,1,col='white',axes=F)

      }else{

        #coly=cols[ceiling(posmean(cor(mcmc@rawdat[1:maxn,keep1[i]],mcmc@rawdat[1:maxn,keep2[j]]))*ncols)]
        xlim<-range(quantile(ind2PPD[j,],c(0.02,0.98),na.rm=T),ind2Data[j,])
        ylim<-range(quantile(ind2PPD[i,],c(0.02,0.98),na.rm=T),ind2Data[i,])
        plot(ind2PPD[j,],ind2PPD[i,],pch=19,xlim=xlim,ylim=ylim,cex=0.9,col=cols[1],axes=F)
        axis(1,c(-100,100))
        axis(2,c(-100,100))
        axis(3,c(-100,100))
        axis(4,c(-100,100))
        points(ind2Data[j,],ind2Data[i,],pch=4,cex=1.6,col=cols[2],lwd=2)

      }
      if(i==2&j==(ni-1)){
        legend('center',legend=c("Observed","Simulated"),text.col=c("orange","blue"),bty='n')

      }

      if(j==1)mtext(namst[i],2,line=2,cex=0.8,las=2)
      if(i==ni)mtext(namst[j],1,line=1,cex=0.8,las=1)
      #if(j==1)mtext(i,2,line=2,cex=0.5,las=2)
      #if(i==nplotted)mtext(j,1,line=1,cex=0.5,las=2)

    }

  }

}

mahalanobis_robust<-function (x, center, cov, inverted = FALSE) {

  x <- if (is.vector(x))
    matrix(x, ncol = length(x))
  else as.matrix(x)
  if (!identical(center, FALSE))
    x <- sweep(x, 2L, center)

  invcov <- corpcor::pseudoinverse(cov)
  setNames(rowSums(x %*% invcov * x), rownames(x))

}

getsegment<-function(densobj,thresh,lower=T){
  if(lower){
    cond<-densobj$x<thresh
  }else{
    cond<-densobj$x>thresh
  }

  xs<-c(0,densobj$y[cond],0)
  ys<-densobj$x[cond]
  ys<-c(ys[1],ys,ys[length(ys)])

  list(x=xs,y=ys)
}

plot_mdist<-function(indPPD,indData,alpha=0.05){

  indPPD[indPPD=="Inf"|indPPD=="-Inf"]<-NA
  keep_not_na<-apply(indPPD[,1,],2,FUN=function(x)sum(is.na(x))==0)
  nullcov<-cov(t(indPPD[,1,keep_not_na]))
  nullm<-apply(indPPD[,1,],1,mean,na.rm=T)
  nullsims<-t(indPPD[,1,])
  obs=indData[,1,1]

  dist<-mahalanobis_robust(x=obs, center=nullm, cov=nullcov)
  dists<-mahalanobis_robust(x=nullsims, center=nullm, cov=nullcov)
  dists<-dists[!is.na(dists)]

  par(mai=c(1.5,1.5,1.2,0.05))
  dens<-density(dists,from=0,to=quantile(dists,0.99))
  xlim=c(0,quantile(dists,0.98))
  plot(dens,xlab="",main="",col='blue',ylab="",xlim=xlim)
  thresh<-quantile(dists,1-alpha)
  abline(v=thresh,lty=2,lwd=2)
  text(thresh+0.5*(xlim[2]-thresh),max(dens$y)*0.9,paste0("V (alpha = ",alpha*100,"%)"))

  mtext("Mahanobis distance, D",1,line=2)
  mtext("Density",2,line=2)
  subdens<-getsegment(dens,thresh,lower=T)
  polygon(y=subdens$x,x=subdens$y,col="blue",border=NA)

  leg<-"Outlier detected (Obs > V)"
  lcol<-"Red"
  if(dist<thresh){
    leg<-"Outlier not detected (Obs < V)"
    lcol="green"
  }
  abline(v=dist,lwd=2,col="orange")

  legend('right',legend=c(paste0("Critical value = ",round(thresh,2)),
                          paste0("Test statistic = ",round(dist,2))),
         text.col=c('black','orange'))

  text(dist,max(dens$y)*0.05,"D (observed data)",font=2,col="orange")
  legend('top',legend=leg,text.col=lcol,text.font=2)
  mtext(paste("AI Analysis for",MSEobj_app@MPs[1]),3,line=0.1)

}
