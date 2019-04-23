
library(shiny)
library(DLMtool)
library(MSEtool)
library(kableExtra)
library(formattable)
library(knitr)
library(dplyr)
library(httpuv)
library(shinyalert)
library(DT)

options(shiny.maxRequestSize=1000*1024^2)

source("./global.R")


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {

  Version<<-"4.1.6"
  # MPs

  # -------------------------------------------------------------
  # Explanatory figures
  source("./Source/Questionnaire/Fishery_figs.R",local=TRUE)
  source("./Source/Questionnaire/Management_figs.R",local=TRUE)
  source("./Source/Questionnaire/Data_figs.R",local=TRUE)

  # Presentation of results
  source("./Source/Skins/MSC.R",local=TRUE)
  Skins<<-new('list')
  Skins[[1]]<-MSC
  Skin<-MSC
  
  #source("./Analysis_results.R",local=TRUE)
  source("./AI_results.R",local=TRUE)
  #source("./VOI.R",local=TRUE)
  #source("./Fease.R",local=TRUE)

  # OM construction / translation
  source("./Source/OM/makeOM.R",local=TRUE)
  source("./Source/OM/ML2D.R",local=TRUE)
  source('./Source/OM/Backwards.R',local=TRUE ) # Stochastic SRA until progress bar update comes to DLMtool
  
  # Reporting
  source("./Source/Reports/OM_report.R",local=TRUE)

  # MSE running / indicator calculation
  source("./Source/MSE/Redos.R",local=TRUE)
  
  # Advice
  source("./Advice.R",local=TRUE)
  
  # SRA related
  source('./Source/SSRA/SSRA.R',local=TRUE ) # Stochastic SRA wrapper
  source('./Source/SSRA/StochasticSRA_MSC.R',local=TRUE ) # Stochastic SRA until progress bar update comes to DLMtool
  
  # Data related
  source("./Source/Data/Data_trim.R",local=TRUE)
  source('./Source/Data/Fease_Functions.R',local=TRUE )
  
  # MP related
  source('./Source/MPs/Custom_MPs.R',local=TRUE)
  
  # MERA shiny app related
  source("./Source/App/Update_objects.R",local=TRUE) # functions that update stored objects, panelstate justification etc
  source("./Source/App/Debug.R",local=TRUE) # functions that update stored objects, panelstate justification etc
  
  
  # Miscellaneous
  source("./Source/Misc/Misc.R",local=TRUE)
 
  incProgress<-shiny::incProgress

  # --------------------------------------------------------------

  Fpanel<-reactiveVal(0)
  Mpanel<-reactiveVal(0)
  Dpanel<-reactiveVal(0)
  Started<-reactiveVal(0)
  Quest<-reactiveVal(0)
  Data<-reactiveVal(0)
  CondOM<-reactiveVal(0)
  MadeOM<-reactiveVal(0)
  RA<-reactiveVal(0) # Have run risk assessment (multi MP)
  Plan<-reactiveVal(0) # Have run Planning (multi MP)
  Eval<-reactiveVal(0)  # Have run Evaluation (single MP)
  DataInd<-reactiveVal(0) # Indicator data loaded
  Ind<-reactiveVal(0)  # Have run Indicator (single MP)
  AdCalc<-reactiveVal(0) # Has advice been calculated
  Tweak<-reactiveVal(0)  # Have things affecting performance metrics been tweaked?
 
  output$Fpanel <- reactive({ Fpanel()})
  output$Mpanel <- reactive({ Mpanel()})
  output$Dpanel <- reactive({ Dpanel()})

  output$Started  <- reactive({ Started()})
  output$Quest    <- reactive({ Quest()})
  output$Data     <- reactive({ Data()})
  output$CondOM   <- reactive({ CondOM()})
  output$MadeOM   <- reactive({ MadeOM()})

  output$RA       <- reactive({ RA()})
  output$Plan     <- reactive({ Plan()})
  output$Eval     <- reactive({ Eval()})
  output$DataInd  <- reactive({ DataInd()})
  output$Ind      <- reactive({ Ind()})

  output$AdCalc   <- reactive({ AdCalc()})
  output$Tweak    <- reactive({Tweak()})

  outputOptions(output,"Fpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Mpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Dpanel",suspendWhenHidden=FALSE)

  outputOptions(output,"Started",suspendWhenHidden=FALSE)
  outputOptions(output,"Data",suspendWhenHidden=FALSE)
  outputOptions(output,"Quest",suspendWhenHidden=FALSE)

  outputOptions(output,"CondOM",suspendWhenHidden=FALSE)
  outputOptions(output,"MadeOM",suspendWhenHidden=FALSE)

  outputOptions(output,"RA",suspendWhenHidden=FALSE)
  outputOptions(output,"Plan",suspendWhenHidden=FALSE)
  outputOptions(output,"Eval",suspendWhenHidden=FALSE)
  outputOptions(output,"DataInd",suspendWhenHidden=FALSE)
  outputOptions(output,"Ind",suspendWhenHidden=FALSE)

  outputOptions(output,"AdCalc",suspendWhenHidden=FALSE)
  outputOptions(output,"Tweak",suspendWhenHidden=FALSE)

  output$Fpanelout <- renderText({ paste("Fishery",Fpanel(),"/ 19")})
  output$Mpanelout <- renderText({ paste("Management",Mpanel(),"/ 7")})
  output$Dpanelout <- renderText({ paste("Data",Dpanel(),"/ 4")})

  # Some useful things
  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  CurrentYr<-as.integer(substr(as.character(Sys.time()),1,4))
  Copyright<-"Open Source, GPL-2"
  
  Just<-list(
    c(
"1. Describe the history and current status of the fishery, including fleets, sectors, vessel types and practices/gear by vessel type, landing ports, economics/markets, whether targeted/bycatch, other stocks caught in the fishery.

2. Describe the stock’s ecosystem functions, dependencies, and habitat types.

3. Provide all relevant reference materials, such as assessments, research, and other analysis.

      ",
      rep("No justification was provided",18)),

     c(
"1. Describe what, if any, current management measures are used to constrain catch/effort.

2. Describe historical management measures, if any.

3. Describe main strengths and weaknesses of current monitoring and enforcement capacity.

4. Describe and reference any legal/policy requirements for management, monitoring and enforcement.

       ",
       rep("No justification was provided",6)),

    c(
"1. Provide the time series (specify years, if possible) that exist for catch, effort, and CPUE/abundance indices.

2. Describe how these data collected (e.g., log books, dealer reporting, observers).

3. Describe what types of sampling programs and methodologies exist for data collection, including the time-series of available sampling data and quality.

4. Describe all sources of uncertainty in the status, biology, life history and data sources of the fishery.	Include links to documentation, reports.

      "
      , rep("No justification was provided",3)))


  # Default simulation ttributes --------------------------------------------------------------------------------
  nyears<-68 # 1950-2018
  nsim<-48

  makeState<-function(x)rep(T,length(get(x)))

  Fpanel_names<-c("M_list","D_list","h_list","FP_list","F_list","qh_list","q_list","LM_list","sel_list","dome_list","DR_list","PRM_list","sigR_list","Ah_list","Vh_list","A_list","V_list","Dh_list")
  Mpanel_names<-c("M1_list","IB_list","IV_list","IBE_list","IVE_list","IBSL_list","IVSL_list")
  Dpanel_names<-c("D1_list","CB_list","Beta_list","Err_list")
  Slider_names<-c("loc","stmag","co")

  MasterList<<-list(Fpanel_names,Mpanel_names,Dpanel_names,Slider_names)

  PanelState<-list(Fpanel=lapply(Fpanel_names, makeState),
                   Mpanel=lapply(Mpanel_names, makeState),
                   Dpanel=lapply(Dpanel_names, makeState),
                   Slider=lapply(Slider_names, makeState))

  PanelState[[1]][[18]]<-c(F,F,F,F,T) # Exception is the final fishery initial depletion
  PanelState[[3]][[4]]<-c(F,F,F,T) # Exception is the final selection of the data menu - quality is a radio button default to data-poor

  getinputnames<-function(x)strsplit(x,"_")[[1]][1]

  inputnames<<-list(Fpanel=lapply(Fpanel_names,getinputnames),
                    Mpanel=lapply(Mpanel_names,getinputnames),
                    Dpanel=lapply(Dpanel_names,getinputnames),
                    Slider=lapply(Slider_names,getinputnames))

  inputtabs<-as.vector(unlist(inputnames))

  # Record all changes to tabs
  observeEvent(sapply(inputtabs, function(x) input[[x]]),{

    UpPanelState()
    #for(i in 1:length(PanelState)){
     # for(j in 1:length(PanelState[[i]])) {
      #  value<-sapply(inputnames[[i]][j],function(x) input[[x]])
       # PanelState[[i]][[j]] <<- get(MasterList[[i]][j])%in%value
      #}
    #}

  })

  observeEvent(input$Mode,{
    
    updateSelectInput(session=session,inputId="sel_MP",choices=getAllMPs()) # update MP selection in Evaluation
    
    RA(0)
    Plan(0)
    Eval(0)
    
    Update_Options()
 
  })

  # == File I/O ==========================================================================

  # Questionnaire save
  output$Save<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".mera"),

    content=function(file){
      Des<<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)

      MSClog<-list(PanelState, Just, Des)
      doprogress("Saving Questionnaire")
      saveRDS(MSClog,file)

    }

  )

  # Questionnaire load
  observeEvent(input$Load,{

    filey<-input$Load
    tryCatch({

        MSClog<-readRDS(file=filey$datapath)
        cond<-length(MSClog)==3 & sum(names(MSClog[[1]])==c("Fpanel","Mpanel","Dpanel","Slider"))==4

        if(cond){
          PanelState<<-MSClog[[1]]
          Just<<-MSClog[[2]]

          # All panels except radio button on D4
          for(i in 1:3){
            for(j in 1:length(PanelState[[i]])) {
              if(!(i==3 & j==4)){ # not the radio button
                state<-as.vector(unlist(PanelState[[i]][j]))
                choices<-as.vector(unlist(get(MasterList[[i]][j])))
                selected<-as.list(choices[state])
                choices<-as.list(choices)
                updateCheckboxGroupInput(session, as.character(inputnames[[i]][j]), selected = selected)
              }
            }
          }
          for(j in 1:length(PanelState[[4]])){
            updateSliderInput(session,as.character(inputnames[[4]][j]),value=as.numeric(PanelState[[4]][j]))
          }

          # update the radio button D4
          i<-3
          j<-4
          state<-as.vector(unlist(PanelState[[i]][j]))
          choices<-as.vector(unlist(get(MasterList[[i]][j])))
          selected<-as.list(choices[state])
          choices<-as.list(choices)
          updateRadioButtons(session, as.character(inputnames[[i]][j]), selected = selected)

          updateTextInput(session, "Name",     value= MSClog[[3]]$Name)
          updateTextInput(session, "Species",  value= MSClog[[3]]$Species)
          updateTextInput(session, "Region",   value= MSClog[[3]]$Region)
          updateTextInput(session, "Agency",   value= MSClog[[3]]$Agency)
          updateTextInput(session, "nyears",   value= MSClog[[3]]$nyears)
          updateTextInput(session, "Author",   value= MSClog[[3]]$Author)
          updateTextInput(session, "Justification",value=Just[[1]][1])
          updateTabsetPanel(session,"tabs1",selected="1")

          #=== DEBUGGING WINDOW =====================================================
          #updateTextAreaInput(session,"Debug",value=choices)
          #updateTextAreaInput(session,"Debug2",value=selected)
          #updateTextAreaInput(session,"Debug3",value=inputId)
          # ==========================================================================

          Fpanel(1)
          Mpanel(1)
          Dpanel(1)
          Plan(0)
        }else{
          shinyalert("File read error", "This does not appear to be a MERA questionnaire file", type = "error")
        }

      },
      error = function(e){
        shinyalert("File read error", "This does not appear to be a MERA questionnaire file", type = "error")
        return(0)
      }
    )

  })

  # Data load
  observeEvent(input$Load_Data,{

    filey<-input$Load_Data

    if(grepl(".csv",filey$datapath)){ # if it is a .csv file

      tryCatch(
        {
         dat<<-new('Data',filey$datapath)

        },
        error = function(e){
          shinyalert("Not a properly formatted DLMtool Data .csv file", "Trying to load as an object of class 'Data'", type = "error")
          Data(0)
          loaded=F
        }
      )

    }else{

      tryCatch(
        {
          dat<<-load(filey$datapath)
        },
        error = function(e){
          shinyalert("Could not load object", "Failed to load this file as a formatted data object", type = "error")
          Data(0)
        }
      )

      if(class(dat)!="Data"){
        shinyalert("Data load error!", "Failed to load this file as either a formatted .csv datafile or a DLMtool object of class 'Data'", type = "error")
        stop()
      }
    }

    dat_test<-Data_trimer(dat)

    if(class(dat_test)!='Data'){
      DataInd(0)

    }else{
      dat_ind<<-dat
      dat<<-dat_test


      DataInd(1)
    }

    tryCatch(
      {
        noCAA<-is.na(sum(dat@CAA))|sum(dat@CAA)==0
        noCAL<-is.na(sum(dat@CAL))|sum(dat@CAL)==0
        incompC<-sum(is.na(dat@Cat))>0
        noML<-sum(!is.na(dat@ML))==0
        noInd<-sum(!is.na(dat@Ind))==0
        noML5<-sum(is.na(dat@ML[1,length(dat@ML[1,])-(0:4)]))==5

        Cond_op<-"None"
        if(!noML5) Cond_op<-c(Cond_op,"MERA SRA ML (DLMtool)")
        if(!((noCAA & noCAL)|incompC))  Cond_op<-c(Cond_op,"Stochastic SRA (Walters et al. 2006)")
        updateSelectInput(session,"Cond_ops",choices=Cond_op,selected="None")
        Data(1)
        MadeOM(0)
        Plan(0)
        Eval(0)
        Ind(0)
        AdCalc(0)
        MPs<-getMPs(All=TRUE)
        MPs<-MPs[!grepl("FMSYref",MPs)] # remove reference FMSY MPs
        if(length(MPs)<3)MPs<-c("curC","curC75","MCD") # just an error catch in case, for some reason getMPs returns less than three MPs
        updateSelectInput(session,"Advice_MP1",choices=MPs,selected=MPs[1])
        updateSelectInput(session,"Advice_MP2",choices=MPs,selected=MPs[2])
        updateSelectInput(session,"Advice_MP3",choices=MPs,selected=MPs[3])

        #Calc_Advice(Advice_MPs=MPs[1:3])

      },
      error = function(e){
        shinyalert("Advice calculation error", "Check data formatting", type = "error")
        AdCalc(0)
      }
    )

  })

  # OM save
  output$Save_OM<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".OM"),

    content=function(file){

      doprogress("Saving Operating Model")
      saveRDS(OM,file)

    }

  )

  # OM load
  observeEvent(input$Load_OM,{

      filey<-input$Load_OM

      tryCatch({
        OM<<-readRDS(file=filey$datapath)
      },
      error = function(e){
        shinyalert("File read error", "This does not appear to be a DLMtool OM object, saved by saveRDS()", type = "error")
        return(0)
      })

      if(class(OM)=='OM'){
        MPs<<-getMPs()
        MadeOM(1)
        CondOM(0)
        Quest(0)
      }else{
        shinyalert("Incorrect class of object", "This file should be an object of DLMtool class 'OM'", type = "error")
      }

  })

  observeEvent(input$MPset,{
    getMPs()
  })


  # Plan save
  output$Save_Plan<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".Plan"),

    content=function(file){

      doprogress("Saving Evaluation data")
      saveRDS(list(MSEobj=MSEobj,MSEobj_reb=MSEobj_reb),file)

    }

  )

  # Plan load
  observeEvent(input$Load_Plan,{

    filey<-input$Load_Plan

    tryCatch({
      listy<-readRDS(file=filey$datapath)
    },
    error = function(e){
      shinyalert("File read error", "This does not appear to be a MERA evaluation object", type = "error")
      return(0)
    }
    )

    cond<-class(listy[[1]])=="MSE" & class(listy[[2]])=="MSE" & listy[[1]]@nMPs>1

    if(cond){
      MSEobj<<-listy[[1]]
      MSEobj_reb<<-listy[[2]]
      Plan(1)
      Eval(0)
      MadeOM(0)
      CondOM(0)
      Ind(0)
      Quest(0)
      redoPlan()
      updateTabsetPanel(session,"Res_Tab",selected="1")
    }else{
      shinyalert("File read error", "This does not appear to be a MERA planning object", type = "error")
    }

  })

  # Eval save
  output$Save_Eval<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".Eval"),

    content=function(file){

      doprogress("Saving Evaluation data")
      saveRDS(list(MSEobj=MSEobj,MSEobj_reb=MSEobj_reb),file)

    }
  )

  # Eval load
  observeEvent(input$Load_Eval,{

    filey<-input$Load_Eval

    tryCatch({
      listy<-readRDS(file=filey$datapath)
    },
    error = function(e){
      shinyalert("File read error", "This does not appear to be a MERA Evaluation object", type = "error")
      return(0)
    }
    )

    cond<-class(listy[[1]])=="MSE" & class(listy[[2]])=="MSE" & listy[[1]]@nMPs==1

    if(cond){
      MSEobj<<-listy[[1]]
      MSEobj_reb<<-listy[[2]]
      Eval(1)
      Plan(0)
      MadeOM(0)
      CondOM(0)
      Quest(0)
      Ind(0)
      redoEval()
      updateTabsetPanel(session,"Res_Tab",selected="2")
    }else{
      shinyalert("File read error", "This does not appear to be a MERA Application object", type = "error")
    }

  })

  # Indicator Data load


  observeEvent(input$getMPhelp,{

    #browseURL(MPurl(input$help_MP))
    js$browseURL(MPurl(input$help_MP))
    #output$MPdoc<-renderUI({
    #  HTML(readLines('http://dlmtool.github.io/DLMtool/reference/DCAC.html'))
    #})
        #js$browseURL(MPurl(input$help_MP))

  })

  # End of file I/O ===================================================================================

  #observeEvent(input$nsim, {
   # nsim<<-as.numeric(input$nsim)
    #updateTextInput(session,"Debug1",value=nsim)
    #if(nsim>0){
    #  if(nsim<48) shinyjs::disable("Parallel")
    #  if(nsim>47) shinyjs::enable("Parallel")
    #}
  #})

  observeEvent(input$sel_MP,{
    selectedMP<<-input$sel_MP
  })

  observeEvent(input$Load_anything,{
   tryCatch(
      {
        filey<-input$Load_anything
        updateTextInput(session,"Debug1",value=filey$datapath)
        source(file=filey$datapath)
        updateSelectInput(session=session,inputId="sel_MP",choices=getAllMPs()) # update MP selection in Application

    },
      error = function(e){
        shinyalert("File read error", "Your source code did not load correctly. Try sourcing this file in an R session to debug errors", type = "error")
      }
    )

  })

  # ======= OM building =============================

  observeEvent(input$Build_OM_2,{

    nsim<<-input$nsim

    #tryCatch({

      if(input$Cond_ops == "MERA SRA ML (DLMtool)"){
        withProgress(message = "Building OM from Questionnaire & SRA ML", value = 0, {

          OM<-makeOM(PanelState,nsim=nsim)
          ML<-mean(dat@ML[1,length(dat@ML[1,])-(0:4)],na.rm=T)/dat@vbLinf*mean(OM@Linf)
          OM<<-ML2D_frame(OM,ML=ML,ploty=F,nsim=OM@nsim,Dlim=c(0.05,0.7))

        })

      }else if(input$Cond_ops == "Stochastic SRA (Walters et al. 2006)"){ # Build from SRA

        OM<-makeOM(PanelState,nsim=nsim,nyears=ncol(dat@Cat),maxage=dat@MaxAge)

        if(input$Debug){
          saveRDS(OM,"OM_autosave.rda")
          saveRDS(dat,"Data_autosave.rda")
        }

        updateTextAreaInput(session,"Debug1",value=paste(OM@nyears,ncol(dat@Cat)))
        withProgress(message = "Building OM from Questionnaire & S-SRA", value = 0, {
          SRAout<<-SSRA_wrap(OM,dat)
          OM<<-SRAout$OM
          SRAinfo<<-SRAout$SRAinfo
        })
        #GoBackwards_SRA(OM)
        #UpdateQuest()
        Just[[1]][1+c(2,4,5,6,7,10)]<<-"Estimated by Stochastic SRA"
        CondOM(1)
        Fpanel(1)
        Mpanel(1)
        Dpanel(1)
        Data(1)

      }else if(input$Cond_ops=="None"){ # Build OM from questionnaire only

        doprogress("Building OM from Questionnaire",1)
        OM<<-makeOM(PanelState,nsim=nsim)

      }
     #},
     #error = function(e){
    #  shinyalert("Could not build operating model", "Try again with another OM conditioning method or examine data object", type = "info")
    #  return(0)
    # }

    #)

    Quest(1)
    MadeOM(1)
    Plan(0)
    Eval(0)
    Ind(0)

    MPs<<-getMPs()
    selectedMP<<-MPs[2]

  })


#############################################################################################################################################################################
### MSE functions
#############################################################################################################################################################################

  
  observeEvent(input$Calculate_risk,{
    
    Fpanel(1)
    MPs<-c('curE','curC','FMSYref','NFref')
    nsim<-96
    OM<<-makeOM(PanelState,nsim=nsim)
    MSClog<<-list(PanelState, Just, Des)
    
    OM@interval<<-12
    
    parallel=F
    if(input$Parallel){
      
      if(nsim>47){
        
        parallel=T
        setup()
        
      }
      
    }
     
    Update_Options()
    
    #tryCatch({
      
      withProgress(message = "Running Risk Assessment", value = 0, {
        silent=T
        MSEobj<<-runMSE(OM,MPs=MPs,silent=silent,control=list(progress=T),PPD=F,parallel=parallel)
      })
      
      MSEobj@Misc[[4]]<<-SampList
      MSEobj_reb<<-MSEobj
      
      if(input$Debug)SaveDebug()
      
      # ==== Types of reporting ==========================================================
      
      if(input$Debug)message("preredoRA")
      redoRA()
      if(input$Debug)message("postredoRA")
      RA(1)
      Tweak(0)
      #updateTabsetPanel(session,"Res_Tab",selected="1")
      
    #},
    #error = function(e){
    #  shinyalert("Computational error", "This probably occurred because your simulated conditions are not possible.
    #               For example a short lived stock a low stock depletion with recently declining effort.
    #              Try revising operating model parameters.", type = "info")
    #  return(0)
    #}
    
    #)
    
  }) # press calculate
  
  
  
  observeEvent(input$Calculate_Plan,{

    Fpanel(1)
    MPs<<-getMPs()
    nsim<<-input$nsim
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

    #tryCatch({

        withProgress(message = "Running Planning Analysis", value = 0, {
          silent=T
          MSEobj<<-runMSE(OM,MPs=MPs,silent=silent,control=list(progress=T),PPD=T,parallel=parallel)
        })
        
        if(exists('SampList'))MSEobj@Misc[[4]]<<-SampList
        Dep_reb<-runif(OM@nsim,input$Dep_reb[1],input$Dep_reb[2]) # is a %
        OM_reb<-OM
        OM_reb@cpars$D<-(Dep_reb/100)*MSEobj@OM$SSBMSY_SSB0 
        
        
        withProgress(message = "Rebuilding Analysis", value = 0, {
          MSEobj_reb<<-runMSE(OM_reb,MPs=MPs,silent=silent,control=list(progress=T),parallel=parallel)
        })
        MSEobj_reb@Misc[[4]]<<-SampList

        if(input$Debug)SaveDebug()

        # ==== Types of reporting ==========================================================
          
        if(input$Debug)message("preredoPlan")
        redoPlan()
        if(input$Debug)message("postredoPlan")
        Plan(1)
        Tweak(0)
        #updateTabsetPanel(session,"Res_Tab",selected="1")

     #},
      #error = function(e){
       # shinyalert("Computational error", "This probably occurred because your simulated conditions are not possible.
        #           For example a short lived stock a low stock depletion with recently declining effort.
         #         Try revising operating model parameters.", type = "info")
        #return(0)
      #}

    #)

  }) # press calculate


  # ------------------------------------------------------------------------------------------------------------------------------------

  observeEvent(input$Calculate_Eval,{

    Fpanel(1)

    selectedMP<<-input$sel_MP

    nsim<<-input$nsim
    parallel=F
    if(input$Parallel){

      if(nsim>47){
        parallel=T
        setup()
      }
    }
    
    Update_Options()
    
    #YIU<-input$proyears_app # Years in use
    OM_eval<-OM
    #OM_eval@proyears<-YIU
    OM_eval@interval<-input$interval_app
    #OM_eval@cpars$mov<-OM_eval@cpars$mov[,,,,1:OM@nyears+YIU]
    
    #tryCatch({
        withProgress(message = "Running Evaluation", value = 0, {
          EvalMPs<-input$sel_MP
          MSEobj<<-runMSE(OM_eval,MPs=EvalMPs,silent=T,control=list(progress=T),PPD=T,parallel=parallel)

        })
        
        MSEobj@Misc[[4]]<<-SampList

        MGT2<-ceiling(MSEobj@OM$MGT*2)
        MGT2[MGT2<5]<-5
        MGT2[MGT2>20]<-20

        OM_reb<-OM_eval
        #OM_reb@proyears<-max(MGT2)+2 # only have to compute to this year
        OM_reb@cpars$D<-MSEobj@OM$SSBMSY_SSB0/2#apply(MSEobj@SSB_hist[,,MSEobj@nyears,],1, sum)/(MSEobj@OM$SSB0*2) # start from half BMSY
        
        Dep_reb<-runif(OM@nsim,input$Dep_reb_app[1],input$Dep_reb_app[2]) # is a %
        OM_reb<-OM_eval
        OM_reb@cpars$D<-(Dep_reb/100)*MSEobj@OM$SSBMSY_SSB0 
        
        withProgress(message = "Rebuilding Analysis", value = 0, {
          MSEobj_reb<<-runMSE(OM_reb,MPs=EvalMPs,silent=T,control=list(progress=T),parallel=parallel)
        })
        
        MSEobj_reb@Misc[[4]]<-SampList

        if(input$Debug)SaveDebug()
        
        if(input$Debug){
          save(MSEobj_reb,file="MSEobj_reb")
          save(MSEobj,file="MSEobj")
        }

        Eval(1)
        Tweak(0)
        redoEval()
        #updateTabsetPanel(session,"Res_Tab",selected="2")
      #},
      #error = function(e){
      #  shinyalert("Computational error", "This probably occurred because your simulated conditions are not possible.
       #            For example a short lived stock a low stock depletion with recently declining effort.
        #           Try revising operating model parameters.", type = "info")
        #return(0)
      #}
    #) # try catch

  }) # calculate MSE app

  observeEvent(input$Calculate_Ind,{
    tryCatch({

      redoInd()
      #updateTabsetPanel(session,"Res_Tab",selected="3")
      Ind(1)
    },
    error = function(e){
      shinyalert("Computational error", "Could not calculate Ancillary indicators, check file format.", type = "info")
      return(0)
    }
    ) # try catch

  })

 
  CheckJust<-function(){

    isjust<-function(x)sum(x=="No justification was provided")
    as.integer(sum(unlist(lapply(Just,isjust)))==0)

  }

  # Show REFRESH RESULTS if ...

  observeEvent(input$burnin,{ Tweak(1) })
  observeEvent(input$YIU,{ Tweak(1) })
  observeEvent(input$res,{ Tweak(1) })
  observeEvent(input$M1,{ Tweak(1) })
  observeEvent(input$D1,{ Tweak(1) })

  # Update tables if ...

  observeEvent(input$Redo,{
    if(input$Mode=='Planning')  redoPlan()
    if(input$Mode=='Evaluation')  redoEval()
    Tweak(0)
  })


  # Update panelstate if ...
  observeEvent(input$D1,{
    UpPanelState()
  })

  observeEvent(input$M1,{
    UpPanelState()
  })


  # ===== Reports ==================================================================================================

  # OM questionnaire report
  output$Build_OM <- downloadHandler(

    # For PDF output, change this to "report.pdf"
    # updateTextInput(session, "Name", value = input$Name),

    filename =  function(){  paste0(namconv(input$Name),"_Questionnaire_Report.html") },
    content = function(file) {
      withProgress(message = "Building questionnaire report", value = 0, {
      #doprogress("Building OM report",1)
      OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('OMRep.Rmd')

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'OMRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Questionnaire Report for ",input$Name),
                     set_type=paste0("(MERA version ",Version,")"),
                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     ntop=input$ntop,
                     inputnames=inputnames,
                     SessionID=SessionID,
                     copyright=paste(Copyright,CurrentYr)
      )
      knitr::knit_meta(class=NULL, clean = TRUE) 
    
      output<-render(input="OMRep.Rmd",output_format="html_document", params = params)
      file.copy(output, file)

      }) # end of progress meter
    }
  )

  # Data report
  output$Build_Data <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_data.html")}, #"report.html",

    content = function(file) {
      withProgress(message = "Building data report", value = 0, {
      nsim<<-input$nsim
      OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('DataRep.Rmd')

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'DataRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Data report for ",input$Name),
                     set_type=paste0("Demonstration Data description"," (MERA version ",Version,")"),
                     dat=dat,
                     author=input$Author,
                     ntop=input$ntop,
                     inputnames=inputnames,
                     SessionID=SessionID,
                     copyright=paste(Copyright,CurrentYr)
      )
      incProgress(0.2)
      knitr::knit_meta(class=NULL, clean = TRUE) 
      output<-render(input="DataRep.Rmd",output_format="html_document", params = params)
      incProgress(0.7)
      file.copy(output, file)
      incProgress(0.1)
      }) # end of progress
    }
  )

  # Conditioning report
  output$Build_Cond <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_Cond.html")}, #"report.html",

    content = function(file) {
      withProgress(message = "Building conditioning report", value = 0, {

      incProgress(0.1)
      #OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('CondRep.Rmd')

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'CondRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Operating Model Conditioning Report for ",input$Name),
                     set_type=paste0("Demonstration Conditioning analysis"," (MERA version ",Version,")"),
                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     SRAinfo=SRAinfo,
                     ntop=input$ntop,
                     inputnames=inputnames,
                     SessionID=SessionID,
                     copyright=paste(Copyright,CurrentYr)
      )
      incProgress(0.1)
      knitr::knit_meta(class=NULL, clean = TRUE) 
      output<-render(input="CondRep.Rmd",output_format="html_document", params = params)
      incProgress(0.8)
      file.copy(output, file)
      }) # end of progress
    }
  )

  # Full OM report
  output$Build_full_OM <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_full_OM.html")}, #"report.html",

    content = function(file) {
      withProgress(message = "Building operating model report", value = 0, {
      #OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('OM_full_Rep.Rmd')
      incProgress(0.1)
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'OM_full_Rep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Full Operating Model Specification Report for ",input$Name),
                     set_type=paste0("(MERA version ",Version,")"),
                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     ntop=input$ntop,
                     inputnames=inputnames,
                     SessionID=SessionID,
                     copyright=paste(Copyright,CurrentYr)
      )
      incProgress(0.1)
      knitr::knit_meta(class=NULL, clean = TRUE) 
      output<-render(input="OM_full_Rep.Rmd",output_format="html_document", params = params)
      incProgress(0.8)
      file.copy(output, file)
      })
    }
  )

  
  output$Build_RA <-downloadHandler(
    
    filename = function(){paste0(namconv(input$Name),"_MERA_Risk_Assessment_Report.html")}, #"report.html",
    
    content = function(file) {
      withProgress(message = "Building risk assessment report", value = 0, {
        src <- normalizePath('RA.Rmd')
        Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
        MSClog<-list(PanelState, Just, Des)
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'RA.Rmd', overwrite = TRUE)
        options=Skin$Risk_Assessment$options
        library(rmarkdown)
        options()
        params <- list(test = input$Name,
                       set_title=paste0("Risk Assessment Report for ",input$Name),
                       set_type=paste0("Risk Assessment of Status Quo Management "," (MERA version ",Version,")"),
                       Skin=Skin,
                       MSEobj=MSEobj,
                       MSEobj_reb=MSEobj_reb,
                       OM=OM,
                       options=options,
                       SessionID=SessionID,
                       copyright=paste(Copyright,CurrentYr)
        )
        knitr::knit_meta(class=NULL, clean = TRUE) 
        out<-render("RA.Rmd", params = params)
        file.rename(out, file)
      })
    }
    
  )
  
  
  output$Build_Plan <-downloadHandler(
    
    filename = function(){paste0(namconv(input$Name),"_MERA_Planning_Report.html")}, #"report.html",
    
    content = function(file) {
      withProgress(message = "Building planning report", value = 0, {
        src <- normalizePath('Plan.Rmd')
        Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
        MSClog<-list(PanelState, Just, Des)
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'Plan.Rmd', overwrite = TRUE)
        options=Skin$Risk_Assessment$options
        library(rmarkdown)
        options <- list(burnin = input$burnin, res=input$res)
        
        params <- list(test = input$Name,
                       set_title=paste0("Planning Report for ",input$Name),
                       set_type=paste0("Multiple MP testing "," (MERA version ",Version,")"),
                       Skin=Skin,
                       MSEobj=MSEobj,
                       MSEobj_reb=MSEobj_reb,
                       OM=OM,
                       options=options,
                       SessionID=SessionID,
                       copyright=paste(Copyright,CurrentYr)
        )
        knitr::knit_meta(class=NULL, clean = TRUE) 
        out<-render("Plan.Rmd", params = params)
        file.rename(out, file)
      })
    }
    
  )
  
  
  output$Build_Eval <-downloadHandler(
    
    filename = function(){paste0(namconv(input$Name),"_MERA_Evaluation_Report.html")}, #"report.html",
    
    content = function(file) {
      withProgress(message = "Building evaluation report", value = 0, {
        src <- normalizePath('Eval.Rmd')
        Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
        MSClog<-list(PanelState, Just, Des)
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'Eval.Rmd', overwrite = TRUE)
        options=Skin$Risk_Assessment$options
        library(rmarkdown)
        options <- list(YIU = input$YIU, res=1)
        
        params <- list(test = input$Name,
                       set_title=paste0("Evaluation Report for ",input$Name),
                       set_type=paste0("Multiple MP testing "," (MERA version ",Version,")"),
                       Skin=Skin,
                       MSEobj=MSEobj,
                       MSEobj_reb=MSEobj_reb,
                       OM=OM,
                       options=options,
                       SessionID=SessionID,
                       copyright=paste(Copyright,CurrentYr)
        )
        knitr::knit_meta(class=NULL, clean = TRUE) 
        out<-render("Eval.Rmd", params = params)
        file.rename(out, file)
      })
    }
    
  )
  
 
  # Anciliary indicators report
  output$Build_AI <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_AI.html")}, #"report.html",
    content = function(file) {
      withProgress(message = "Building indicators report", value = 0, {
      src <- normalizePath('IndRep.Rmd')

      test<-match(input$sel_MP,MPs)
      if(is.na(test))mm<-1
      if(!is.na(test))mm<-test

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'IndRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Ancillary Indicator Analysis Report for ",input$Name),
                     set_type=paste0("Ancillary indicators report"," (MERA version ",Version,")"),

                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     inputnames=inputnames,
                     MSEobj=MSEobj,
                     dat=dat,
                     dat_ind=dat_ind,
                     mm=mm,
                     ntop=input$ntop,
                     burnin=burnin,
                     SessionID=SessionID,
                     copyright=paste(Copyright,CurrentYr)
      )
      knitr::knit_meta(class=NULL, clean = TRUE)
      out<-render("IndRep.Rmd", params = params)
      file.rename(out, file)
      })
    }
  )


  # Fishery panel reactions ============================================================================================================

  observeEvent(input$Justification,{
    RecJust()
  })

  observeEvent(input$tabs1, {

    UpJust()
    Des<<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
    MSCsave_auto()
    #getMPs()
    #selectedMP<<-MPs[2]

  })

  observeEvent(input$Fback,{

    if(input$tabs1==1 && Fpanel() >1){
      Fpanel(Fpanel()-1)
    }else if(input$tabs1==2 && Mpanel() >1){
      Mpanel(Mpanel()-1)
    }else if(input$tabs1==3 && Dpanel() >1){
      Dpanel(Dpanel()-1)
    }

    UpJust()

    Des<<-list(Name=input$Name,Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
    MSCsave_auto()

  })

  observeEvent(input$Fcont,{

    if(input$tabs1==1 && Fpanel() < 19){
      Fpanel(Fpanel()+1)
    }else if(input$tabs1==2 && Mpanel() < 7){
      Mpanel(Mpanel()+1)
    }else if(input$tabs1==3 && Dpanel() < 4){
      Dpanel(Dpanel()+1)
    }

    # Write old values
    UpJust()

    Des<<-list(Name=input$Name,Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
    MSCsave_auto()

  })

  # ---- Fishery all switches -----------------

  observeEvent(input$All_M,
     if(input$All_M == 0 | input$All_M%%2 == 0){
       updateCheckboxGroupInput(session,"M",choices=M_list,selected=M_list)
     }else{
       updateCheckboxGroupInput(session,"M",choices=M_list)
     }
  )
  observeEvent(input$All_D,
     if(input$All_D == 0 | input$All_D%%2 == 0){
        updateCheckboxGroupInput(session,"D",choices=D_list,selected=D_list)
     }else{
        updateCheckboxGroupInput(session,"D",choices=D_list)
     }
  )
  observeEvent(input$All_h,
     if(input$All_h == 0 | input$All_h%%2 == 0){
        updateCheckboxGroupInput(session,"h",choices=h_list,selected=h_list)
     }else{
        updateCheckboxGroupInput(session,"h",choices=h_list)
     }
  )
  observeEvent(input$All_FP,
     if(input$All_FP == 0 | input$All_FP%%2 == 0){
       updateCheckboxGroupInput(session,"FP",choices=FP_list,selected=FP_list)
       updateSliderInput(session,"loc",value=1)
       updateSliderInput(session,"stmag",value=1)

     }else{
       updateCheckboxGroupInput(session,"FP",choices=FP_list)
       updateSliderInput(session,"loc",value=1)
       updateSliderInput(session,"stmag",value=1)

     }
  )
  observeEvent(input$All_F,
     if(input$All_F == 0 | input$All_F%%2 == 0){
        updateCheckboxGroupInput(session,"F",choices=F_list,selected=F_list)
     }else{
        updateCheckboxGroupInput(session,"F",choices=F_list)
     }
  )
  observeEvent(input$All_qh,
     if(input$All_qh == 0 | input$All_qh%%2 == 0){
       updateCheckboxGroupInput(session,"q_h",choices=q_list,selected=q_list)
     }else{
       updateCheckboxGroupInput(session,"q_h",choices=q_list)
     }
  )
  observeEvent(input$All_q,
     if(input$All_q == 0 | input$All_q%%2 == 0){
       updateCheckboxGroupInput(session,"q",choices=q_list,selected=q_list)
     }else{
       updateCheckboxGroupInput(session,"q",choices=q_list)
     }
  )
  observeEvent(input$All_LM,
     if(input$All_LM == 0 | input$All_LM%%2 == 0){
       updateCheckboxGroupInput(session,"LM",choices=LM_list,selected=LM_list)
     }else{
       updateCheckboxGroupInput(session,"LM",choices=LM_list)
     }
  )
  observeEvent(input$All_sel,
     if(input$All_sel == 0 | input$All_sel%%2 == 0){
        updateCheckboxGroupInput(session,"sel",choices=sel_list,selected=sel_list)
     }else{
        updateCheckboxGroupInput(session,"sel",choices=sel_list)
     }
  )
  observeEvent(input$All_dome,
     if(input$All_dome == 0 | input$All_dome%%2 == 0){
        updateCheckboxGroupInput(session,"dome",choices=dome_list,selected=dome_list)
     }else{
        updateCheckboxGroupInput(session,"dome",choices=dome_list)
    }
  )
  observeEvent(input$All_DR,
     if(input$All_DR == 0 | input$All_DR%%2 == 0){
        updateCheckboxGroupInput(session,"DR",choices=DR_list,selected=DR_list)
     }else{
        updateCheckboxGroupInput(session,"DR",choices=DR_list)
     }
  )
  observeEvent(input$All_PRM,
    if(input$All_PRM == 0 | input$All_PRM%%2 == 0){
        updateCheckboxGroupInput(session,"PRM",choices=PRM_list,selected=PRM_list)
    }else{
        updateCheckboxGroupInput(session,"PRM",choices=PRM_list)
    }
  )
  observeEvent(input$All_sigR,
     if(input$All_sigR == 0 | input$All_sigR%%2 == 0){
        updateCheckboxGroupInput(session,"sigR",choices=sigR_list,selected=sigR_list)
     }else{
        updateCheckboxGroupInput(session,"sigR",choices=sigR_list)
     }
  )

  observeEvent(input$All_Ah,
     if(input$All_Ah  == 0 | input$All_Ah%%2 == 0){
       updateCheckboxGroupInput(session,"Ah",choices=Ah_list,selected=Ah_list[[1]]) # I know this duplication is...
     }else{
       updateCheckboxGroupInput(session,"Ah",choices=Ah_list,selected=Ah_list[[1]]) # ... lazy but I'm keeping it here for future toggle adaptation.
     }
  )
  observeEvent(input$All_Vh,
     if(input$All_Vh  == 0 | input$All_Vh%%2 == 0){
       updateCheckboxGroupInput(session,"Vh",choices=Vh_list,selected=Vh_list[[length(Vh_list)]]) # I know this duplication is...
     }else{
       updateCheckboxGroupInput(session,"Vh",choices=Vh_list,selected=Vh_list[[length(Vh_list)]]) # ... lazy but I'm keeping it here for future toggle adaptation.
     }
  )
  
  observeEvent(input$All_A,
    if(input$All_A  == 0 | input$All_A%%2 == 0){
       updateCheckboxGroupInput(session,"A",choices=A_list,selected=input$Ah) # I know this duplication is...
    }else{
      updateCheckboxGroupInput(session,"A",choices=A_list,selected=input$Ah)  # ... lazy but I'm keeping it here for future toggle adaptation. 
    } 
  )
  observeEvent(input$All_V,
    if(input$All_V  == 0 | input$All_V%%2 == 0){
       updateCheckboxGroupInput(session,"V",choices=V_list,selected=input$Vh) # I know this duplication is...
    }else{
       updateCheckboxGroupInput(session,"V",choices=V_list,selected=input$Vh) # ... lazy but I'm keeping it here for future toggle adaptation. 
    }
  )
  observeEvent(input$All_Dh,
    if(input$All_Dh == 0 | input$All_Dh%%2 == 0){
      updateCheckboxGroupInput(session,"Dh",choices=Dh_list,selected=Dh_list[[5]])
    }else{
      updateCheckboxGroupInput(session,"Dh",choices=Dh_list,selected=Dh_list[[5]])
    }
  )

  # ---- Management all switches -------------

  observeEvent(input$All_M1,
               if(input$All_M1 == 0 | input$All_M1%%2 == 0){
                 updateCheckboxGroupInput(session,"M1",choices=M1_list,selected=M1_list)
               }else{
                 updateCheckboxGroupInput(session,"M1",choices=M1_list)
               }
  )
  observeEvent(input$All_IB,
               if(input$All_IB == 0 | input$All_IB%%2 == 0){
                 updateCheckboxGroupInput(session,"IB",choices=IB_list,selected=IB_list)
               }else{
                 updateCheckboxGroupInput(session,"IB",choices=IB_list)
               }
  )
  observeEvent(input$All_IV,
               if(input$All_IV == 0 | input$All_IV%%2 == 0){
                 updateCheckboxGroupInput(session,"IV",choices=IV_list,selected=IV_list)
               }else{
                 updateCheckboxGroupInput(session,"IV",choices=IV_list)
               }
  )

  observeEvent(input$All_IBE,
               if(input$All_IBE == 0 | input$All_IBE%%2 == 0){
                 #vals<-as.list(IB_list[input$IB])
                 updateCheckboxGroupInput(session,"IBE",choices=IBE_list,selected=input$IB)
               }else{
                 updateCheckboxGroupInput(session,"IBE",choices=IBE_list,selected=input$IB)
               }
  )
  observeEvent(input$All_IVE,
               if(input$All_IVE == 0 | input$All_IVE%%2 == 0){
                 updateCheckboxGroupInput(session,"IVE",choices=IVE_list,selected=input$IV)
               }else{
                 updateCheckboxGroupInput(session,"IVE",choices=IVE_list,selected=input$IV)
               }
  )
  observeEvent(input$All_IBSL,
               if(input$All_IBSL == 0 | input$All_IBSL%%2 == 0){
                 updateCheckboxGroupInput(session,"IBSL",choices=IBSL_list,selected=IBSL_list[length(IB_list)-match(input$IB,IB_list)+1])
               }else{
                 updateCheckboxGroupInput(session,"IBSL",choices=IBSL_list,selected=IBSL_list[length(IB_list)-match(input$IB,IB_list)+1])
               }
  )
  observeEvent(input$All_IVSL,
               if(input$All_IVSL == 0 | input$All_IVSL%%2 == 0){
                 updateCheckboxGroupInput(session,"IVSL",choices=IVSL_list,selected=input$IV)
               }else{
                 updateCheckboxGroupInput(session,"IVSL",choices=IVSL_list,selected=input$IV)
               }
  )



  # ---- Data all switches -------------

  observeEvent(input$All_D1,
               if(input$All_D1 == 0 | input$All_D1%%2 == 0){
                 updateCheckboxGroupInput(session,"D1",choices=D1_list,selected=D1_list)
               }else{
                 updateCheckboxGroupInput(session,"D1",choices=D1_list)
               }
  )
  observeEvent(input$All_CB,
               if(input$All_CB == 0 | input$All_CB%%2 == 0){
                 updateCheckboxGroupInput(session,"CB",choices=CB_list,selected=CB_list)
               }else{
                 updateCheckboxGroupInput(session,"CB",choices=CB_list)
               }
  )
  observeEvent(input$All_Beta,
               if(input$All_Beta == 0 | input$All_Beta%%2 == 0){
                 updateCheckboxGroupInput(session,"Beta",choices=Beta_list,selected=Beta_list)
               }else{
                 updateCheckboxGroupInput(session,"Beta",choices=Beta_list)
               }
  )
  observeEvent(input$All_Err,

              updateRadioButtons(session,"Err",choices=Err_list,selected="Err_bad")

  )
  
  
  # ---- Sliders ----------------------
  
  observeEvent(input$Dep_reb_def,
              
        updateSliderInput(session,"Dep_reb",value=c(50,50),min=10,max=100)
              
  )
  
  observeEvent(input$Dep_reb_def_app,
               
               updateSliderInput(session,"Dep_reb_app",value=c(50,50),min=10,max=100)
               
  )
  

  # ======================= Explanatory Plots ===================================
  # Scheme
  fcol = rgb(0.4,0.8,0.95)#"#0299f"
  fcol2 = "dark grey"
  icol <- "dodgerblue4"
  maxcol="cadetblue"
  mincol="dark grey"

  # Fishery
  output$plotM <- renderPlot(plotM())
  output$plotD <- renderPlot(plotD())
  output$ploth <- renderPlot(ploth())
  output$plotFP <- renderPlot(plotFP())
  output$plotF <- renderPlot(plotF())
  output$plotqh <- renderPlot(plotqh())
  output$plotq <- renderPlot(plotq())
  output$plotLM <- renderPlot(plotLM())
  output$plotmat <- renderPlot(plotmat())
  output$plotsel <- renderPlot(plotsel())
  output$plotdome <- renderPlot(plotdome())
  output$plotDR <- renderPlot(plotDR())
  output$plotPRM <- renderPlot(plotPRM())
  output$plotsigR <- renderPlot(plotsigR())
  output$plotAh <- renderPlot(plotAh())
  output$plotVh <- renderPlot(plotVh())
  output$plotA <- renderPlot(plotA())
  output$plotV <- renderPlot(plotV())
  output$plotDh <- renderPlot(plotDh()) #19

  # Management
  output$plotIB <- renderPlot(plotIB())
  output$plotIV <- renderPlot(plotIV())
  output$plotIB_E <- renderPlot(plotIB_E())
  output$plotIV_E <- renderPlot(plotIV_E())
  output$plotIB_SL <- renderPlot(plotIB_SL())
  output$plotIV_SL <- renderPlot(plotIV_SL()) # 7

  # Data
  output$plotBeta <- renderPlot(plotBeta())
  output$plotCB <- renderPlot(plotCB())

  observeEvent(input$debug,
              updateTextAreaInput(session,"Debug1",value=MadeOM())
  )

})
