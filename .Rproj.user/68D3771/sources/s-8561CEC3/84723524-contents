# Calculations

# Status Calcs
Calc_Status<-function(){
  Status<-Sim<-SimSams<-BCfit<-new('list')# Sim, SimSams and BCfit are redundant
  Fit<<-new('list')
  Est<<-new('list')
  
  nsim<-input$nsim
  
  if(LoadOM()==1&input$OM_L){ 
    OM<-OM_L
  }else{
    if(MadeOM()==0)OM<<-makeOM(PanelState)
  }
  
  #if(input$SDset=="Custom"){
    codes<<-input$SDsel
  #}else{
  #  codes<<-getCodes(dat,maxtest=1)
  #}

  setup(cpus=4)
  
  tryCatch({
    
    withProgress(message = "Running Status Determination", value = 0, {
      incProgress(1/2, detail = 50)
      Fit[[1]]<-GetDep(OM,dat,code=codes,cores=4)
      Est[[1]]<-Fit[[1]]@OM@cpars$D[Fit[[1]]@conv]
      if(sum(Fit[[1]]@conv)==0)AM(paste(codes,"Did not return depletion"))
      incProgress(2/2, detail = 50)  
    })
    
    Status <<- list(codes=codes, Est=Est, Sim=Sim, Fit=Fit, nsim=nsim, Years=dat@Year, SimSams=SimSams, BCfit=BCfit) 
    #saveRDS(list(codes=codes, Est=Est, Sim=Sim, Fit=Fit, nsim=nsim, Years=dat@Year, SimSams=SimSams, BCfit=BCfit) ,"C:/temp/Status.rda")
    SD(1) 
    message("preredoSD")
    smartRedo()
    message("postredoSD")
    
  },
  error = function(e){
    AM(paste0(e,sep="\n"))
    shinyalert("Computational error", paste("The Status Determination method",codes,"returned an error. Try selecting an alternative Status Determination method from the settings menu."), type = "info")
    return(0)
  }
  )

}



Calc_Plan<-function(){
  
  doprogress("Building OM from Questionnaire",1)
  
  if(LoadOM()==1&input$OM_L){ 
    OM<<-OM_L
  #}else if(CondOM()==1&input$OM_C){ 
    #OM<<-OM_C
  }else{
    if(MadeOM()==0) OM<<-makeOM(PanelState)
  }
  #saveRDS(OM,file="C:/temp/OM.rda")
  Fpanel(1)
  MPs<<-getMPs()
  
  nsim<<-input$nsim_Plan
  parallel=F
  
  if(input$Parallel){
    
    if(nsim>47){
      
      parallel=T
      setup(cpus=ncpus)
      
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
  
  if(LoadOM()==1&input$OM_L){ 
    OM<<-OM_L
  }else{
    if(MadeOM()==0)OM<<-makeOM(PanelState)
  }
 
  MSClog<<-list(PanelState, Just, Des)
  OM@interval<<-input$interval
  
  parallel=F
  if(input$Parallel){
    
    if(nsim>47){
      
      parallel=T
      setup(cpus=ncpus)
      
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
  }else{
    if(MadeOM()==0)OM_Eval<<-makeOM(PanelState,proyears=YIU*2) # project to 2 x years in use
  }  
  
  Fpanel(1)
  EvalMPs<-input$sel_MP
  
  nsim<<-input$nsim_Eval
  parallel=F
  
  if(input$Parallel){
    
    if(nsim>47){
      
      parallel=T
      setup(cpus=ncpus)
      
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
