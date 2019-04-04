getminmax<-function(panel,parameter,PanelState){
  loc<-match(parameter,inputnames[[panel]])
  mins<-get(paste0(parameter,"_mins"))
  maxs<-get(paste0(parameter,"_maxes"))
  cond<-unlist(PanelState[[panel]][loc])
  range(mins[cond],maxs[cond])
}

makeOM<-function(PanelState,nsim=48,nyears=NA,maxage=NA){

  OM<-LowSlopes(testOM)

  OM@R0<-100000
  OM@nsim<-nsim

  OM@Linf<-c(100,100)
  OM@L50<-NaN
  OM@K<-NaN
  OM@isRel<-"FALSE"

  OM@Name<-input$Name
  OM@Species<-input$Species
  OM@Region<-input$Region
  OM@Agency<-input$Agency
  if(is.na(nyears)){
    if(is.na(as.integer(input$nyears))){
      OM@nyears<-68
    }else{
      OM@nyears<-as.integer(input$nyears)
    }
  }else{
    OM@nyears<-nyears
  }
  nyears<-OM@nyears

  OM@Source<-input$Author
  OM@interval<-input$interval
  OM@proyears<-input$proyears

  #save(OM,file="OM.Rdata")  # debug

  loc<-match("Err",inputnames[[3]])
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

  # Fishery characteristics -------
  OM@M<-getminmax(1,"M",PanelState)                                                         # F2 ----------
  OM@L50<-getminmax(1,"LM",PanelState)                                                      # F9 ----------

  if(is.na(maxage)){
    OM@maxage=ceiling(-log(0.02)/min(OM@M))
  }else{
    OM@maxage=maxage
  }

  OM<-LH2OM(OM, dist='norm')                                                               
  OM@K<-quantile(OM@cpars$K,c(0.05,0.95))
  OM@L50<-quantile(OM@cpars$L50,c(0.05,0.95))
  OM@L50_95<-c(10,10)
  OM@Linf<-c(100,100)
  OM@D<-getminmax(1,"D",PanelState)                                                        # F3 -----------
  OM@h<-getminmax(1,"h",PanelState)                                                        # F4 -----------

  # Ftrend and error                                                                       # F5 -----------
  loc<-match("FP",inputnames[[1]])
  cond<-(1:length(unlist(PanelState[[1]][loc])))[unlist(PanelState[[1]][loc])]
  Ftype<<-sample(cond,nsim,replace=T)
  M1sim<-M1s[Ftype]
  M2sim<-M2s[Ftype]
  sd1sim<-sd1s[Ftype]
  sd2sim<-sd2s[Ftype]
  h2sim<-h2s[Ftype]
  locsim<-PanelState[[4]][[1]]
  stmagsim<-PanelState[[4]][[2]]
  Find<-array(NA,c(nsim,nyears))
  #Ftrendfunc<-function(                 M1=0.2,M2=1.2,sd1=0.1,sd2=0.3,h2=2,ny=68,loc=1,start_mag=1,bm=F,plot=F){
    
  for(i in 1:nsim)Find[i,]<-Ftrendfunc(M1=M1sim[i],M2=M2sim[i],sd1=sd1sim[i],sd2=sd2sim[i],h2=h2sim[i],ny=nyears,loc=locsim,start_mag=2-stmagsim,bm=F,plot=F)
  # for(i in 1:nsim)Find[i,]<-Ftrendfunc(M1=M1sim[i],M2=M2sim[i],sd1=sd1sim[i],sd2=sd2sim[i],h2=h2sim[i],ny=nyears,loc=0.5,start_mag=2-0.5,bm=F,plot=F)
  
 
  Esd<-getminmax(1,"F",PanelState)                                                         # F6 ----------
  Esd_max<-Esd[2]
  Esd_min<-Esd[1]
  Esdrand<-runif(nsim,Esd_min,Esd_max)
  Emu<-(-0.5*Esdrand^2)
  Esdarray<-array(exp(rnorm(nsim*nyears,Emu,Esdrand)),c(nsim,nyears))
  
  qhs<-getminmax(1,"qh",PanelState)
  qssim<-1+runif(nsim,qhs[1],qhs[2])/100                                                   # F7 ----------
  
  for(i in 1:nsim)Find[i,]<-Find[i,]*Esdarray[i,]* qssim[i]^((1:nyears)-(nyears/2))

  # --- Future catchability ----------

  OM@qinc<-getminmax(1,"q",PanelState)                                                     # F8 ----------
  
  # --- Selectivity -----------------------
  
  Sel50<-getminmax(1,"sel",PanelState)                                                     # F10 ----------
  Sel50sim<-runif(nsim,Sel50[1],Sel50[2])

  
  L5<-OM@cpars$Linf*Sel50sim*0.8
  LFS<-OM@cpars$Linf*Sel50sim*1.2
  cond<-LFS>0.95*OM@cpars$Linf
  LFS[cond]<-0.95*OM@cpars$Linf[cond]
  
  OM@cpars$L5<-L5
  OM@cpars$LFS<-LFS

  OM@Vmaxlen<-getminmax(1,"dome",PanelState)                                               # F11 ----------
  
  OM@DR<-getminmax(1,"DR",PanelState)                                                      # F12 ----------
  OM@Fdisc<-getminmax(1,"PRM",PanelState)                                                  # F13 ----------
  OM@Perr<-getminmax(1,"sigR",PanelState)                                                  # F14 ----------
 
  Arng<-getminmax(1,"A",PanelState)
  Size_area_1<-Frac_area_1<-runif(nsim,Arng[1],Arng[2])
  OM@Prob_staying<-1-getminmax(1,"V",PanelState)[2:1]

  # Management parameters
  OM@TACFrac<-OM@TAEFrac<-getminmax(2,"IB",PanelState)
  OM@TACSD<-OM@TAESD<-getminmax(2,"IV",PanelState)

  # Data parameters
  CB_rng<-getminmax(3,"CB",PanelState)
  Cbias<-runif(nsim,CB_rng[1],CB_rng[2])

  OM@beta<-getminmax(3,"Beta",PanelState)
  D<-runif(nsim,OM@D[1],OM@D[2])

  # Custom parameters
  OM@cpars<-c(OM@cpars,list(D=D,Find=Find, Size_area_1=Size_area_1,
                 Frac_area_1=Frac_area_1,Cbias=Cbias))

  saveRDS(OM,"OM_autosave.rda")
  OM

}
