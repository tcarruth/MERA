# Calculations

# Status Calcs
Calc_Status<-function(){
  Status<-new('list')
  
  nsim<-input$nsim_SD
  
  if(LoadOM()==1&input$OM_L){ 
    OM<-OM_L
    OMsimsam<-OM_L
  }else if(CondOM()==1&input$OM_C){ 
    OM<-OM_C
    OMsimsam<-OM_L
  }else{
    OM<-makeOM(PanelState,nsim=nsim)
    OMsimsam<-makeOM(PanelState,nsim=40)
  }
  
  #saveRDS(OM,"C:/Users/tcarruth/Dropbox/MERA prototyping/OM_Scoping/OM3.rds")
  
  if(input$SDset=="Custom"){
    codes<<-input$SDsel
  }else{
    if(input$SDset=="All"){
      nSD=Inf
    }else if(input$SDset=="Top 6"){
      nSD=6
    }else if(input$SDset=="Top 3"){
      nSD=3
    }
    codes<<-getCodes(dat,maxtest=nSD)
  }
  
  ncode<-length(codes)
  Est<-Sim<-list()
  Fit<<-list()
  
  #saveRDS(OM,"C:/temp/OM3")
  #saveRDS(dat,"C:/temp/dat3")
  #saveRDS(codes,"C:/temp/codes3")
  setup(cpus=4)
  
  tryCatch({
    
    withProgress(message = "Running Status Determination", value = 0, {
      #saveRDS(OM,"C:/temp/OM.rda")
      for(cc in 1:ncode){
        Fit[[cc]]<<-GetDep(OM,dat,code=codes[cc],cores=4)
        Est[[cc]]<-Fit[[cc]]@OM@cpars$D[Fit[[cc]]@conv]
        if(sum(Fit[[cc]]@conv)==0)AM(paste(cc,codes[cc],"Did not return depletion"))
        incProgress(1/ncode, detail = round(cc*100/ncode))
      }  
      
    })
    
    saveRDS(OMsimsam,"C:/temp/OMsimsam")
    saveRDS(Est,"C:/temp/Est")
    saveRDS(codes,"C:/temp/codes")
    
  },
  error = function(e){
    AM(paste0(e,sep="\n"))
    shinyalert("Computational error", "One or more of the Status Determination methods you selected returned an error. Try using a custom selection of Status Determination methods. Sim testing for effort-based methods is currently not available.", type = "info")
    return(0)
  }
  )
  
  
  
  tryCatch({
    
    if(input$SD_simtest){
      
      SimSams<-BCfit<-list()
      
      if(any(grepl("E",codes))){
        AM("You have requested to sim test at least one method for status determination that uses effort data...")
        AM("This is not possible given the current version of DLMtool. If you want to sim-test please choose methods that do not include 'E' for effort.")
      }  
      
      AM("Conducting sim-testing of methods for Status Determination")
      #setup(cpus=4)
      withProgress(message = "Running simulation test of SD methods", value = 0, {
        
        # Generate simulated data over a range of stock depletion
        for(cc in 1:ncode){
          
          AM(paste(" -------------- ", codes[cc],":", cc,"/",ncode, " -------------- "))
          SimSams[[cc]]<-SimTest(OMsimsam,code=codes[cc], ndeps=40, DepLB=0.05, DepUB=0.8)
          incProgress(1/ncode, detail = round(cc*100/ncode))
          
        } 
        
      })
      
      #saveRDS(SimSams,"C:/temp/SimSams.rds")
      
    }
    
  },
  error = function(e){
    AM(paste0(e,sep="\n"))
    shinyalert("Computational error", "Something went wrong with the Simulation test. Try using an alternative selection of Status Determination methods.", type = "info")
    return(0)
    
  }
  )
  
  
  
  tryCatch({    
    
    if(input$SD_simtest){
      
      BCfit<-list() 
      
      # Fit non-linear models to the sim-sam fit and calculate bias-corrected estimates of stock depletion
      withProgress(message = "Calculating bias correction", value = 0, {
        
        for(cc in 1:ncode){
          
          BCfit[[cc]]<-fitdep(out=SimSams[[cc]],dEst=Est[[cc]])
          incProgress(1/ncode, detail = round(cc*100/ncode))
        }
        
      })  
      
      
    }else{
      
      SimSams<-NULL
      BCfit<-NULL
      
    }
    
    # ==== Types of reporting ==========================================================
    
    # Status<-list(codes=codes,Est=Est, Sim=Sim, Fit=Fit,nsim=nsim,Years=dat@Year,SimSams=SimSams,BCfit=BCfit)
    # saveRDS(Status,"C:/temp/Status.rda")
    
    Status <<- list(codes=codes, Est=Est, Sim=Sim, Fit=Fit, nsim=nsim, Years=dat@Year, SimSams=SimSams, BCfit=BCfit)
    SD(1) 
    message("preredoSD")
    smartRedo()
    message("postredoSD")
    
    updateSelectInput(session, "SDdet",choices=codes,selected=codes[1])
    # updateSelectInput(session,'SDsel',choices=SD_codes,selected=SD_codes[1])
    # Tweak(0)
    # updateTabsetPanel(session,"Res_Tab",selected="1")
    
  },
  error = function(e){
    AM(paste0(e,sep="\n"))
    shinyalert("Computational error", "One or more of the power models used to characterize estimation bias failed to converge. Try selecting a different set of status determination methods.", type = "info")
    return(0)
  })
}



Calc_Plan<-function(){
  
  doprogress("Building OM from Questionnaire",1)
  
  if(LoadOM()==1&input$OM_L){ 
    OM<<-OM_L
  }else if(CondOM()==1&input$OM_C){ 
    OM<<-OM_C
  }else{
    OM<<-makeOM(PanelState,nsim=input$nsim_Plan)
  }
  
  Fpanel(1)
  MPs<<-getMPs()
  
  nsim<<-input$nsim_Plan
  parallel=F
  
  if(input$Parallel){
    
    if(nsim>47){
      
      parallel=T
      setup()
      
    }
    
  }
  MSClog<<-list(PanelState, Just, Des)
  
  Update_Options()
  #tags$audio(src = "RunMSE.mp3", type = "audio/mp3", autoplay = NA, controls = NA)
  
  tryCatch({
    withProgress(message = "Running Planning Analysis", value = 0, {
      silent=T
      MSEobj<<-runMSE(OM,MPs=MPs,silent=silent,control=list(progress=T),PPD=T,parallel=parallel)
    })
    
    # if (input$Skin !="Train") { # don't run rebuild for Train skin
    if(exists('SampList'))MSEobj@Misc[[4]]<<-SampList
    Dep_reb<-runif(OM@nsim,input$Dep_reb[1],input$Dep_reb[2]) # is a %
    OM_reb<-OM
    OM_reb@cpars$D<-(Dep_reb/100)*MSEobj@OM$SSBMSY_SSB0 
    
    withProgress(message = "Rebuilding Analysis", value = 0, {
      if (!'NFref' %in% MPs) MPs <- c("NFref", MPs) # added this so I can calculate Tmin - rebuild time with no fishing - AH
      MSEobj_reb<<-runMSE(OM_reb,MPs=MPs,silent=silent,control=list(progress=T),parallel=parallel)
    })
    
    MSEobj_reb@Misc[[4]]<<-SampList
    # } else {
    #   MSEobj_reb <<- MSEobj
    # }
    
    #saveRDS(MSEobj,file="C:/temp/MSEobj2.Rdata")
    #saveRDS(MSEobj_reb,file="C:/temp/MSEobj_reb2.Rdata")
    
    # ==== Types of reporting ==========================================================
    
    message("preredoPlan")
    Plan(1)
    smartRedo()
    message("postredoPlan")
    
    #Tweak(0)
    #updateTabsetPanel(session,"Res_Tab",selected="1")
    
  },
  error = function(e){
    AM(paste0(e,"\n"))
    shinyalert("Computational error", "This probably occurred because the fishery dynamics of your questionnaire are not possible.
                     For example, a short lived stock a low stock depletion with recently declining effort.
                    Try revising operating model parameters.", type = "info")
    return(0)
  }
  
  )
}



Calc_RA<-function(){
  Fpanel(1)
  MPs<-c('curE','CurC','FMSYref','NFref')
  nsim <- ifelse(quick, 8, input$nsim_RA)
  
  if(LoadOM()==1&input$OM_L){ 
    OM<<-OM_L
  }else if(CondOM()==1&input$OM_C){ 
    OM<<-OM_C
  }else{
    OM<<-makeOM(PanelState,nsim=nsim)
  }
  
  #saveRDS(OM,"C:/Users/tcar_/Dropbox/MERA paper/Figures/OM_Boc.rda")
  
  MSClog<<-list(PanelState, Just, Des)
  OM@interval<<-input$interval
  
  parallel=F
  if(input$Parallel){
    
    if(nsim>47){
      
      parallel=T
      setup()
      
    }
    
  }
  
  Update_Options()
  
  tryCatch({
    
    withProgress(message = "Running Risk Assessment", value = 0, {
      silent=T
      RAobj<<-runMSE(OM,MPs=MPs,silent=silent,control=list(progress=T),PPD=T,parallel=parallel)
    })
    
    RAobj@Misc[[4]]<<-SampList
    
    # ==== Types of reporting ==========================================================
    RA(1)
    message("preredoRA")
    smartRedo()
    message("postredoRA")
    
    #Tweak(0)
    #updateTabsetPanel(session,"Res_Tab",selected="1")
    
  },
  error = function(e){
    AM(paste0(e,sep="\n"))
    shinyalert("Computational error", "This probably occurred because your simulated conditions are not possible.
                     For example a short lived stock a low stock depletion with recently declining effort.
                    Try revising operating model parameters.", type = "info")
    return(0)
  }
  
  )
}

Calc_Perf<-function(){
  
  doprogress("Building OM from Questionnaire",1)
  YIU=length(dat_ind@Year)-length(dat@Year)
  
  if(LoadOM()==1&input$OM_L){ 
    OM_Eval<<-OM_L
  }else if(CondOM()==1&input$OM_C){ 
    OM_Eval<<-OM_C
  }else{
    OM_Eval<<-makeOM(PanelState,proyears=YIU*2,nsim=input$nsim_Eval) # project to 2 x years in use
  }  
  
  Fpanel(1)
  EvalMPs<-input$sel_MP
  
  nsim<<-input$nsim_Eval
  parallel=F
  
  if(input$Parallel){
    
    if(nsim>47){
      
      parallel=T
      setup()
      
    }
    
  }
  
  MSClog<<-list(PanelState, Just, Des)
  Update_Options()
  
  tryCatch({
    
    withProgress(message = "Running Performance Evaluation", value = 0, {
      #saveRDS(OM_Eval,file="C:/temp/OM_Eval.Rdata")
      #saveRDS(EvalMPs,file="C:/temp/EvalMPs.Rdata")
      
      EvalMPs<-input$sel_MP
      MSEobj_Eval<<-runMSE(OM_Eval,MPs=EvalMPs,silent=T,control=list(progress=T),PPD=T,parallel=parallel)
      
    })
    
    Eval(1)
    Ind(1)
    message("preredoEval")
    smartRedo()
    message("postredoEval")
    
    #saveRDS(MSEobj_Eval,file="C:/temp/MSEobj_Eval.Rdata")
    #saveRDS(dat,file="C:/temp/dat.Rdata")
    #saveRDS(dat_ind,file="C:/temp/dat_ind.Rdata")
    
  },
  error = function(e){
    AM(paste0(e,"\n"))
    shinyalert("Computational error", "This probably occurred because your simulated conditions are not possible.
                   For example a short lived stock a low stock depletion with recently declining effort.
                   Try revising operating model parameters.", type = "info")
    return(0)
  }
  ) # try catch
  
}
