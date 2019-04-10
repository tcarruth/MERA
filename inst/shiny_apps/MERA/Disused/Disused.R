#observeEvent(input$D1,{

# if(input$D1[1])
#temp<-input$D1
#if("ann_cat"%in%temp)  temp<-c(temp,"ann_cat_R")
#if("ind"%in%temp)  temp<-c(temp,"ind_R")
#updateCheckboxGroupInput(session=session,inputId="D1",selected=temp)
#updateTextInput(session=session,inputId="Debug1", value=input$D1)

#})


wormplot_msc<-function(MSEobj, Bref = 0.5, LB = 0.25, UB = 0.75, MPcols){

  #par(mai=c(0.6,0.6,0.01,0.01))
  #wormplot(MSEobj)

  ncol <- ceiling(MSEobj@nMPs^0.3)
  nrow <- ceiling(MSEobj@nMPs/ncol)
  par(mfcol = c(nrow, ncol), mar = c(0.1, 0.1, 0.1, 0.1), omi = c(0.6, 0.25, 0.3, 0))
  Bprob <- apply(MSEobj@B_BMSY > Bref, 2:3, sum)/MSEobj@nsim
  ind <- order(apply(Bprob, 1, sum), decreasing = T)
  BLB <- Bprob > LB
  BUB <- Bprob > UB

  col <- array("red", dim(Bprob))
  col[BLB & !BUB] = "yellow"
  col[BUB] = "green"

  for (i in 1:(nrow * ncol)) {
    if (i < (MSEobj@nMPs + 1)) {
      MP <- ind[i]
      plot(c(1, MSEobj@proyears + 2), c(-1, 1), col = "white", axes = F)
      # abline(h=0)

      for (ys in 1:MSEobj@proyears) {
        x <- c(ys - 1, ys, ys, ys - 1)
        y <- c(rep(Bprob[MP, ys], 2), rep(-Bprob[MP, ys], 2))
        pol <- data.frame(x, y)
        polygon(pol, col = col[MP, ys], border = NA)
      }

      legend("top", legend = MSEobj@MPs[MP], bty = "n",text.col=MPcols[MP])
      if ((i/nrow) == round(i/nrow, 0))
        axis(1, pretty(1:MSEobj@proyears), pretty(1:MSEobj@proyears))


    } else {
      plot.new()
    }

    if (i == (nrow * ncol)) {
      legend("topright", fill = c("green", "red"), legend = c(paste(">",round(UB * 100, 0), "% prob.", sep = ""), paste("<", round(LB * 100, 0), "% prob.", sep = "")), bty = "n")
    }

  }

  mtext(paste("Probability of biomass above ", round(Bref * 100, 0),"% BMSY for ", deparse(substitute(MSE)), sep = ""), 3, outer = T,line = 0.5)
  mtext("Projection year", 1, outer = T, line = 2.5)
  mtext(paste("Fraction of simulations above ", round(Bref * 100, 0),"% BMSY", sep = ""), 2, outer = T, line = 0.25)

}

HCRplot<-function(MSEobj,Pcrit=0.2){

  nMPs<-MSEobj@nMPs
  ncol=6
  nrow=ceiling(nMPs/ncol)
  par(mfrow=c(nrow,ncol),mai=c(0.4,0.4,0.01,0.01),omi=c(0.3,0.3,0.01,0.01))

  for(i in 1:MSEobj@nMPs){

    cond<-MSEobj_FB@B_BMSY[,i,1]<1 & MSEobj_FB@B_BMSY[,i,1]>0.1
    plot(MSEobj_FB@B_BMSY[,i,1],MSEobj_FB@F_FMSY[,i,2],col="white",pch=19,xlim=c(0,1.6),xlab="",ylab="",cex.axis=1.5)
    points(MSEobj_FB@B_BMSY[!cond,i,1],MSEobj_FB@F_FMSY[!cond,i,2],col="#99999998",pch=19)

    dat<-data.frame(x=MSEobj_FB@B_BMSY[cond,i,1],y=MSEobj_FB@F_FMSY[cond,i,2])
    temp<-lm(y~x,dat=dat)
    stemp<-summary(temp)
    pred<-predict(temp,newdata=data.frame(x=seq(0.1,1,length.out=20)))
    Pval<-stemp$coefficients[2,4]
    Slope<-stemp$coefficient[2,1]
    pos<-Slope>0

    col<-'red'
    if(pos & Pval<Pcrit)col="green"
    points(MSEobj_FB@B_BMSY[cond,i,1],MSEobj_FB@F_FMSY[cond,i,2],col=col,pch=19)
    lines(seq(0.1,1,length.out=20),pred,col=col)
    legend('bottomright',legend=c(paste("pval=",round(Pval,3)),paste("slope=",round(Slope,3))),bty='n',cex=1.3)
    legend('top',MSEobj@MPs[i],bty='n',text.font=2,cex=1.5)

  }

  mtext("F/FMSY",2,line=0.3,outer=T)
  mtext("B/BMSY",1,line=0.3,outer=T)

}

