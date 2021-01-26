
getminmax<-function(panel,parameter,PanelState){
  
  loc<-match(parameter,inputnames[[panel]])
  mins<-get(paste0(parameter,"_mins"))
  maxs<-get(paste0(parameter,"_maxes"))
  cond<-unlist(PanelState[[panel]][loc])
  range(mins[cond],maxs[cond])
  
}


rnorm_T<-function(n=1,mean=0,sd=1,trunc=90){
  
  incomplete=T
  ntrial<-n*10
  out<-rep(NA,n)
  fillind<-(1:n)[is.na(out)]
  nfill<-length(fillind)
  LB<-((100-trunc)/2)/100
  UB<-1-LB
  #i<-0
  
  while(incomplete){
    #i<-i+1
    #print(i)
    samps<-rnorm(ntrial,mean,sd)
    cond<-samps > qnorm(LB,mean,sd) & samps < qnorm(UB, mean, sd)
    canfill<-sum(cond)
    if(canfill >= nfill){
      out[fillind]<-samps[cond][1:nfill]
      incomplete=F
    }else{
      out[fillind[1:canfill]]<-samps[cond]
      fillind<-(1:n)[is.na(out)]
      nfill<-length(fillind)
    }
    
  }
  
  if(all(out > qnorm(LB,mean,sd) & out < qnorm(UB, mean, sd))){
    return(out)
  }else{
    message("An error in rnorm_T occurred")
    return(NULL)
  }
  
}

#hist(rnorm_T(1000,0,1,10))


samp_par<-function(n,type="Truncated normal",LB,UB,trunc=99){
  
  
  if(LB==UB){
    out<-rep(LB,n)
  }else{
    if(type=="Uniform"){
      out<-runif(n,LB,UB)
    }else{
      mu<-mean(c(LB,UB))
      UBtemp<-qnorm(1-(100-trunc)/200,mu,1)
      sd<-(UB-mu)/(UBtemp-mu)
      out<-rnorm_T(n,mu,sd,trunc)
    }
  }  
  out
  
}


trimOM<-function(OM,newsim=8,silent=T){
  
  if(newsim > OM@nsim)stop("You asked for more simulations than are available in the OM object")
  
  if(length(OM@cpars)==0){
    
    if(!silent) message("There is no cpars slot in this OM object, only the nsim slot has been modified")
  }else{
    
    for(i in 1:length(OM@cpars)){
      
      dims<-dim(OM@cpars[[i]])
      ndim<-length(dims)
      
      if(ndim==0){
        OM@cpars[[i]]<-OM@cpars[[i]][1:newsim]
      }else if(ndim==2){
        OM@cpars[[i]]<-matrix(OM@cpars[[i]][1:newsim,],nrow=newsim)
      }else if(ndim==3){
        OM@cpars[[i]]<-array(OM@cpars[[i]][1:newsim,,],c(newsim,dims[2:3]))
      }else if(ndim==4){
        OM@cpars[[i]]<-array(OM@cpars[[i]][1:newsim,,,],c(newsim,dims[2:4]))
      }else if(ndim==5){  
        OM@cpars[[i]]<-array(OM@cpars[[i]][1:newsim,,,,],c(newsim,dims[2:5]))
      }
      
    }
    
  }
  
  OM@nsim<-newsim
  
  OM
}


whatOMmess<-function(){
  if(LoadOM()==1 & input$OM_L){
    mess<-"Using loaded operating model"
  }else if(CondOM()==1 & input$OM_C){
    mess<-"Using conditioned operating model"
  }else{
    mess<-"Building operating model from questionnaire"  
  }
  mess
}


makeOM<-function(PanelState,nyears=NA,maxage=NA,proyears=NA,UseQonly=F){

  # ---- Misc OM building ------------------------------------------------------------------------------------
   nsim<-input$nsim
 
  if(input$OM_L & !UseQonly){
    OM<-OM_L
    SampList<<-NULL
    AM("Using loaded operating model")

  }else{
    
    type<-input$Distribution # sampling distribution
    trunc<-input$IQRange     # inter quartile range (trunc, a % e.g. 90)
    
    OM<-LowSlopes(MSEtool::testOM)
    if(!is.na(nsim)){
      OM@nsim<-nsim
    }else{
      OM<-trimOM(OM,input$nsim)
    }
    
    if(input$use_seed)OM@seed<-input$seed
    
    OM@R0<-1e9
    OM@Linf<-c(100,100)
    OM@L50<-NaN
    OM@K<-NaN
    OM@isRel<-"FALSE"
  
    OM@Name<-input$Name
    OM@Species<-input$Species
    OM@Region<-input$Region
    OM@Agency<-input$Agency
    nyears<-input$Lyear-input$Syear+1
    OM@nyears<-nyears
    
    OM@Source<-input$Author
    OM@interval<-input$interval
    if(is.na(proyears)){
      OM@proyears<-proyears<-50 #input$proyears
    }else{
      OM@proyears<-proyears #input$proyears
    }
    
    loc<-match("Err",inputnames[[3]])                                                        # D1 -----------
    cond<-as.vector(unlist(PanelState[[3]][loc]))
    Dquality<-as.vector(unlist(Err_list)[cond])
  
    if(Dquality=="Err_perf"){
      temp<-new('OM',Albacore,Generic_Fleet,Perfect_Info,Perfect_Imp)
    }else if(Dquality=="Err_good"){
      temp<-new('OM',Albacore,Generic_Fleet,Precise_Unbiased,Perfect_Imp)
    }else if(Dquality=="Err_mod"){
      temp<-new('OM',Albacore,Generic_Fleet,Generic_obs,Perfect_Imp)
    }else{
      temp<-new('OM',Albacore,Generic_Fleet,Imprecise_Biased,Perfect_Imp)
    }
    
    OM<-Replace(OM,temp,Sub="Obs")
    
    # ---- Fishery characteristics ---------------------------------------------------------------------------
    
    OM@M<-getminmax(1,"M",PanelState)                                                        # F2 ----------
    OM@L50<-getminmax(1,"LM",PanelState)                                                     # F9 ----------
  
    if(is.na(maxage)){
      OM@maxage<-maxage<-ceiling(-log(0.1)/min(OM@M))
    }else{
      OM@maxage=maxage
    }
    OM@maxage<-maxage<-min(OM@maxage,input$plusgroup)
    
    
    # --- Life history imputation
    OMtemp<-OM
    OMtemp@nsim<-1000
    OMtemp<-LH2OM(OMtemp, dist='norm',plot=F,filterMK=T) # get sample
    OM@K<-quantile(OMtemp@cpars$K,c(0.1,0.9)) # 80th percentile from LH2OM
    OM<-LH2OM(OM, dist='norm',plot=F,filterMK=T) # truncated sample
    
    OM@L50<-quantile(OM@cpars$L50,c(0.1,0.90))
    OM@L50_95<-c(10,10)
    OM@Linf<-c(100,100)
    OM@D<-getminmax(1,"D",PanelState)                                                        # F3 -----------
    OM@h<-getminmax(1,"h",PanelState)                                                        # F4 -----------
   
    # Ftrend and error 
    # eff_values<-readRDS("C:/temp/eff_values.rda"); input<-list(ny=68); nyears=68; nsim=48; Esd_min=0.1; Esd_max=0.5 # F5 -----------
    trends<-effort_mat()
    trends<-trends/apply(trends,1,mean)
    nt<-dim(trends)[1]
  
    Esd<-getminmax(1,"F",PanelState)                                                         # F6 ----------
    Esd_max<-Esd[2]
    Esd_min<-Esd[1]
    Esdrand<-samp_par(nsim,type=type,Esd_min,Esd_max,trunc=trunc) #runif(nsim,Esd_min,Esd_max)
    Emu<-(-0.5*Esdrand^2)
    Esdarray<-array(exp(rnorm(nsim*nyears,Emu,Esdrand)),c(nsim,nyears))
    
    qhs<-getminmax(1,"qh",PanelState)
    qhssim<-samp_par(nsim,type=type,qhs[1],qhs[2],trunc=trunc) #(nsim,qhs[1],qhs[2])
    qssim<-1+qhssim/100                                                   # F7 ----------
    trendsamp<-ceiling(runif(nsim)*nt)
    
    Find<-array(NA,c(nsim,nyears))
    for(i in 1:nsim)Find[i,]<-trends[trendsamp[i],]*Esdarray[i,]* qssim[i]^((1:nyears)-(nyears/2))
  
    # --- Future catchability ----------
  
    OM@qinc<-getminmax(1,"q",PanelState)                                                     # F8 ----------
    
    # --- Selectivity -----------------------
   
    Sel50<-getminmax(1,"sel",PanelState)                                                     # F10 ----------
    Sel50sim<-samp_par(nsim,type=type,Sel50[1],Sel50[2],trunc=trunc) #runif(nsim,Sel50[1],Sel50[2])
  
    L5<-OM@cpars$Linf*Sel50sim*0.8
    LFS<-OM@cpars$Linf*Sel50sim*1.2
    cond<-LFS>0.95*OM@cpars$Linf
    LFS[cond]<-0.95*OM@cpars$Linf[cond]
    Linf<-rep(100,nsim)
   
    OM@Vmaxlen<-getminmax(1,"dome",PanelState)                                               # F11 ----------
    
    # --- Discarding ------------------------
    
    OM@DR<-getminmax(1,"DR",PanelState) # F12 ----------
    #DR<-matrix(samp_par(nsim,type=type,OM@DR[1],OM@DR[2],trunc=trunc),ncol=nsim,nrow=nyears+proyears,byrow=T)
    
    OM@Fdisc<-getminmax(1,"PRM",PanelState)                                                  # F13 ----------
    
    # --- Recruitment deviations ------------
    
    OM@Perr<-getminmax(1,"sigR",PanelState)                                                  # F14 ----------
   
    # --- MPAs ------------------------------
   
    nareas<-3
    
    Ahrng<-getminmax(1,"Ah",PanelState) # size / frac habitat area 3                         # F15 ----------
    Vhrng<-getminmax(1,"Vh",PanelState) # prob staying in area 3                             # F16 ----------
    Arng<-getminmax(1,"A",PanelState)   # size / frac habitat area 1                         # F17 ----------
    Vrng<-getminmax(1,"V",PanelState)   # prob staying in area 3                             # F18 ----------
    
    Ahsim<-samp_par(nsim,type=type,Ahrng[1],Ahrng[2],trunc=trunc) #runif(nsim,Ahrng[1],Ahrng[2])
    Vhsim<-samp_par(nsim,type=type,Vhrng[1],Vhrng[2],trunc=trunc) #runif(nsim,Vhrng[1],Vhrng[2])
    Asim<-samp_par(nsim,type=type,Arng[1],Arng[2],trunc=trunc) #runif(nsim,Arng[1],Arng[2])
    Vsim<-samp_par(nsim,type=type,Vrng[1],Vrng[2],trunc=trunc) #runif(nsim,Vrng[1],Vrng[2])
    
    ilogit<-function(x)log(x/(1-x))
    logit<-function(x)exp(x)/(1+exp(x))
    
    mov1<-mov2<-array(NA,c(nsim,2,2))
    
    for(i in 1:nsim){
      mov1[i,,]<-getmov2(i,Vsim,Asim)      
      mov2[i,,]<-getmov2(i,Vhsim,Ahsim)
    }
     
    V2<-apply(cbind(mov1[,2,2], # staying in areas 2 and 3 minus staying in area 3
                    mov2[,2,2]), # staying in areas 2 and 3 minus staying in area 1
              1,mean) # a WRONG GUESS of the prob_staying in area 2 - need to do the linear equation modelling for this. 
    
    Sz2<-1-(Ahsim+Asim)
    Asize<-cbind(Asim,Sz2,Ahsim) # area 1 is Asim as future MPs close area 1
    probs<-cbind(Vsim,V2,Vhsim)
   
    # plot(Ahsim,Vhsim,type='l',xlim=c(0,0.9)); lines(Asim,Vsim,col="grey"); lines(Sz2,V2,col="red")
   
    mov<-array(NA,c(nsim, maxage+1, nareas, nareas, nyears+proyears))
    for(i in 1:nsim)mov[i,,,,]<-array(rep(makemov(fracs=Asize[i,], prob=probs[i,]),each=maxage+1),c(maxage+1,nareas,nareas,nyears+proyears))
    
    # OM@MPA<-matrix(c(1,1,1,0,                                            # year1, area1 open, area2 open, area3 shut
    #                 nyears-1,0,1,1),ncol=nareas+1,byrow=T)              # nyears-1, area1 shut, area2 open, area3 open 
    
    OM@cpars$MPA<-matrix(1,nrow=OM@nyears+OM@proyears,ncol=3)
    OM@cpars$MPA[1:(nyears-1),3]<-0
    OM@cpars$MPA[nyears:proyears,1]<-0
 
    # Initial depletion                                                                      # F19 ----------
    initDrng<-getminmax(1,"Dh",PanelState)
    #print(initDrng)
    initD<-samp_par(nsim,type=type,initDrng[1],initDrng[2],trunc=trunc) #runif(nsim,initDrng[1],initDrng[2])
    
    # ---- Management parameters -----------------------------------------------------------------------------------------------
    
    OM@TACFrac<-getminmax(2,"IB",PanelState)                                                 # M2 -----------
    OM@TACSD<-getminmax(2,"IV",PanelState)                                                   # M3 -----------
    
    OM@TAEFrac<-getminmax(2,"IBE",PanelState)                                                # M4 -----------
    OM@TAESD<-getminmax(2,"IVE",PanelState)                                                  # M5 -----------
    
    OM@SizeLimFrac<-getminmax(2,"IBSL",PanelState)                                           # M6 -----------
    OM@SizeLimSD<-getminmax(2,"IVSL",PanelState)                                             # M7 -----------
  
    
    # ---- Data parameters -----------------------------------------------------------------------------------------------------
    
    CB_rng<-getminmax(3,"CB",PanelState)                                                     # D2 -----------
    Cbias<-samp_par(nsim,type=type,CB_rng[1],CB_rng[2],trunc=trunc) #runif(nsim,CB_rng[1],CB_rng[2])
  
    OM@beta<-getminmax(3,"Beta",PanelState)                                                  # D3 -----------
   
    
    # ---- Custom parameters ---------------------------------------------------------------------------------------------------
   
    slots2cpars<-c("D","h","Vmaxlen","Fdisc","Perr","TACFrac","TACSD",
      "TAEFrac","TAESD","SizeLimFrac","SizeLimSD","beta") # all slots that need making into cpars vectors
    
    makevec<-function(i,OM,slots2cpars,nsim,type,trunc){
      LB<-slot(OM,slots2cpars[i])[1]
      UB<-slot(OM,slots2cpars[i])[2]
      OM@cpars[[slots2cpars[i]]]<-samp_par(nsim,type=type,LB,UB,trunc)
      OM
    }
    
    for(i in 1:length(slots2cpars))OM<-makevec(i,OM,slots2cpars,nsim,type,trunc)
    
    OM@cpars<-c(OM@cpars,list(Find=Find,L5=L5,LFS=LFS,Asize=Asize,mov=mov,initD=initD,Cbias=Cbias,
                              control=list(progress=T,ntrials=1000,fracD=0.2)))#,DR=DR))
    
    SampList<<-data.frame(Esdrand,qhssim,Sel50sim,Ahsim,Vhsim,Asim,Vsim,initD,Cbias)
    
    # ---- Bioeconomic parameters ----------------------------------------------------------------------------------------------
    #AM("TEST BE")
    
    # if(input$EC_Model!="None"){
      
    #  OM@cpars<-c(OM@cpars,list(CostCurr=rep(input$CostCurr,OM@nsim), 
     #                           RevCurr=rep(input$RevCurr,OM@nsim), 
    #                            Response=rep(input$Response/100,OM@nsim), 
     #                           CostInc=rep(input$CostInc,OM@nsim), 
      #                          RevInc=rep(input$RevInc,OM@nsim)))
      #AM("Using bioeconomic model parameters")
      
    #}
    
    # ---- Data overwriting ---------------------------------------------------------------------------------------------------
    #saveRDS(OM,"C:/temp/OMpost.rda") # 
    #saveRDS(dat,"C:/temp/datpost.rda") # 
    
    if(Data()==1){
      AM("Questionnaire growth and mortality overwritten by those specified in uploaded data")
      if(!is.na(dat@vbLinf[1])){
        ratio<-dat@vbLinf[1]/mean(OM@cpars$Linf)
        OM@Linf<-rep(dat@vbLinf,2)
        OM@cpars$Linf<-OM@cpars$Linf*ratio
        OM@cpars$LFS<-OM@cpars$LFS*ratio
        OM@cpars$L5<-OM@cpars$L5*ratio
        OM@cpars$L50<-OM@cpars$L50*ratio
      }
      
      if(!is.na(dat@wla))OM@a<-dat@wla
      if(!is.na(dat@wlb))OM@b<-dat@wlb
      if(!is.na(dat@vbt0[1])) OM@t0<-rep(dat@vbt0[1],2)
      if(!is.na(dat@vbK[1])){ OM@K<-rep(dat@vbK[1],2); OM@cpars$K<-OM@cpars$K*dat@vbK/mean(OM@cpars$K)}
      if(!is.null(dat@Mort) & !is.na(dat@Mort)) OM@cpars$M <- OM@cpars$M * dat@Mort / mean(OM@cpars$M) 
      
      if(!is.na(dat@LFC))OM@L5<-rep(dat@LFC,2)
      if(!is.na(dat@LFS))OM@LFS<-rep(dat@LFS,2)
      if(!is.na(dat@Vmaxlen))OM@Vmaxlen<-rep(dat@Vmaxlen,2)
      if(!is.na(dat@LenCV))OM@LenCV<-rep(dat@LenCV,2)
     
    }
    
    # AM("Using questionnaire-based operating model")
  
    if(Data()==1&input$OM_C){
      
      code<-input$Cond_ops
      AM(paste0("Conditioning operating model using method ",code))
      
      setup(cpus=ncpus)
      
      tryCatch({
        
        withProgress(message = "Conditioning Operating Model", value = 0, {
          incProgress(0.1)
          #saveRDS(OM,"C:/temp/OM.rda") 
          #saveRDS(dat,"C:/temp/dat.rda")
          dofit(OM,dat)
          CFit<-Status$Fit[[1]] #GetDep(OM,dat,code=code,cores=4)
          if(sum(CFit@conv)==0)AM(paste0(code,": ",sum(CFit@conv), " of ",length(CFit@conv)," simulations converged"))
          incProgress(0.8)
          
        })
        #saveRDS(CFit,"C:/temp/CFit.rda")
        OM<-Sub_cpars(CFit@OM,CFit@conv & CFit@OM@cpars$D<1.5) # subset operating model by converged runs and final depletion below 150% unfished
        SampList<<-SampList[CFit@conv& CFit@OM@cpars$D<1.5,]  # subset other question parameter samples
        updateNumericInput(session=session, "nsim",value=sum(CFit@conv)) # make sure OM nsim matches the text box
        
        #OM<-CFit@OM
        CondOM(1)
        SD(1)
        AM("------------- New conditioned OM made --------------")
        MadeOM(1) 
        redoSD()
        AM("Updating status determination outputs following OM rebuilding")
       
      },
      error = function(e){
        AM(paste0(e,sep="\n"))
        shinyalert("Computational error", "Operating model conditioning returned an error. Try using a different model for conditioning.", type = "info")
      }
     )
    
    #testing=F
    #if(testing){
    # MSEobj<-runMSE(OM,"DCAC")
    #  OM_reb<-OM
    #  OM_reb@proyears<-max(OM@proyears,20+2) # only have to compute to this year
    #  Dep_reb<-runif(OM@nsim,50,50)#input$Dep_reb[1],input$Dep_reb[2]) # is a %
    #  OM_reb@cpars$D<-(Dep_reb/100)*MSEobj@OM$SSBMSY_SSB0#apply(MSEobj@SSB_hist[,,MSEobj@nyears,],1, sum)/(MSEobj@OM$SSB0*2) # start from half BMSY
    #  MSEobj_reb<-runMSE(OM_reb,"DCAC")
    #  Bdeps<-MSEobj_reb@OM$D/MSEobj_reb@OM$SSBMSY_SSB0#MSEobj_reb@B_BMSY[,1,1]#
    #}
    
    } # end of OM conditioning 
    AM("------------- New OM made --------------")
    MadeOM(1) 
  } # #end of loaded OM or not
  OM # OM

}

dofit<-function(OM,dat){
  nsim<-input$nsim
  Status<-Sim<-SimSams<-BCfit<-new('list')# Sim, SimSams and BCfit are redundant
  Fit<<-new('list')
  Est<<-new('list')
  codes<<-input$Cond_ops
 
  Fit[[1]]<-GetDep(OM,dat,code=codes)
  Est[[1]]<-Fit[[1]]@OM@cpars$D[Fit[[1]]@conv]
  if(sum(Fit[[1]]@conv)!=0)AM(paste(sum(Fit[[1]]@conv),"of",length(Fit[[1]]@conv),"simulations converged, the rest will removed and not be used in other calculations"))
  Status <<- list(codes=codes, Est=Est, Sim=Sim, Fit=Fit, nsim=nsim, Years=dat@Year, SimSams=SimSams, BCfit=BCfit) 
  #saveRDS(Status,"C:/temp/Status.rda")# 
  AM("Operating model conditioned: Management Planning and Status Determination models available")
}
