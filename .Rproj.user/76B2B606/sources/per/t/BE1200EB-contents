
getminmax<-function(panel,parameter,PanelState){
  
  loc<-match(parameter,inputnames[[panel]])
  mins<-get(paste0(parameter,"_mins"))
  maxs<-get(paste0(parameter,"_maxes"))
  cond<-unlist(PanelState[[panel]][loc])
  range(mins[cond],maxs[cond])
  
}

makeOM<-function(PanelState,nsim=48,nyears=NA,maxage=NA){

  # ---- Misc OM building ------------------------------------------------------------------------------------
  
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
  OM@proyears<-proyears<-50#input$proyears

  #save(OM,file="OM.Rdata")  # debug

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
 
  OM@Vmaxlen<-getminmax(1,"dome",PanelState)                                               # F11 ----------
  
  
  # --- Discarding ------------------------
  
  OM@DR<-getminmax(1,"DR",PanelState)                                                      # F12 ----------
  OM@Fdisc<-getminmax(1,"PRM",PanelState)                                                  # F13 ----------
  
  # --- Recruitment deviations ------------
  
  OM@Perr<-getminmax(1,"sigR",PanelState)                                                  # F14 ----------
 
  # --- MPAs ------------------------------
  nareas<-3
  Ahrng<-getminmax(1,"Ah",PanelState) # size / frac habitat area 3                         # F15 ----------
  Vhrng<-getminmax(1,"Vh",PanelState) # prob staying in area 3                             # F16 ----------
  Arng<-getminmax(1,"A",PanelState)   # size / frac habitat area 1                         # F17 ----------
  Vrng<-getminmax(1,"V",PanelState)   # prob staying in area 3                             # F18 ----------
  
  Ahsim<-runif(nsim,Ahrng[1],Ahrng[2])
  Vhsim<-runif(nsim,Vhrng[1],Vhrng[2])
  Asim<-runif(nsim,Arng[1],Arng[2])
  Vsim<-runif(nsim,Vrng[1],Vrng[2])
  
  ilogit<-function(x)log(x/(1-x))
  logit<-function(x)exp(x)/(1+exp(x))
  
  mov1<-mov2<-array(NA,c(nsim,2,2))
  
  for(i in 1:nsim){
    mov1[i,,]<-getmov2(i,Vsim,Asim)      
    mov2[i,,]<-getmov2(i,Vhsim,Ahsim)
  }
  V2<-apply(cbind(mov1[,2,2]-Vhsim, # staying in areas 2 and 3 minus staying in area 3
              mov2[,2,2]-Vsim), # staying in areas 2 and 3 minus staying in area 1
        1,min) # an overestimate of the prob_staying in area 2
  
  Sz2<-1-(Ahsim+Asim)
  Asize<-cbind(Asim,Sz2,Ahsim) # area 1 is Asim as future MPs close area 1
  probs<-cbind(Vsim,V2,Vhsim)
 
  # plot(Ahsim,Vhsim,type='l',xlim=c(0,0.9)); lines(Asim,Vsim,col="grey"); lines(Sz2,V2,col="red")
  
  mov<-array(NA,c(nsim, maxage, nareas, nareas, nyears+proyears))
  for(i in 1:nsim)mov[i,,,,]<-array(rep(makemov(fracs=Asize[i,], prob=probs[i,]),each=maxage),c(maxage,nareas,nareas,nyears+proyears))
  
  OM@MPA<-matrix(c(1,1,1,0,                                            # year1, area1 open, area2 open, area3 shut
                   nyears-1,0,1,1),ncol=nareas+1,byrow=T)              # nyears-1, area1 shut, area2 open, area3 open              
  
 
  # Initial depletion                                                                      # F19 ----------
  initDrng<-getminmax(1,"Dh",PanelState)
  
  initD<-runif(nsim,initDrng[1],initDrng[2])
  
  
  # ---- Management parameters -----------------------------------------------------------------------------------------------
  
  OM@TACFrac<-getminmax(2,"IB",PanelState)                                                 # M2 -----------
  OM@TACSD<-getminmax(2,"IV",PanelState)                                                   # M3 -----------
  
  OM@TAEFrac<-getminmax(2,"IBE",PanelState)                                                # M4 -----------
  OM@TAESD<-getminmax(2,"IVE",PanelState)                                                  # M5 -----------
  
  OM@SizeLimFrac<-getminmax(2,"IBSL",PanelState)                                           # M6 -----------
  OM@SizeLimFrac<-getminmax(2,"IVSL",PanelState)                                           # M7 -----------

  
  # ---- Data parameters -----------------------------------------------------------------------------------------------------
  
  CB_rng<-getminmax(3,"CB",PanelState)                                                     # D2 -----------
  Cbias<-runif(nsim,CB_rng[1],CB_rng[2])

  OM@beta<-getminmax(3,"Beta",PanelState)                                                  # D3 -----------
 
  
  # ---- Custom parameters ---------------------------------------------------------------------------------------------------
 
  OM@cpars<-c(OM@cpars,list(Find=Find,L5=L5,LFS=LFS,Asize=Asize,mov=mov,initD=initD,Cbias=Cbias))

  saveRDS(OM,"OM_autosave.rda")
  
  OM

}
