quick <- FALSE # switch to use 3 sims for quick test runs in RA mode

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
library(mvtnorm)
library(cowplot)

options(shiny.maxRequestSize=1000*1024^2)

source("./global.R")

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {

  Version<<-"5.4.4"
  
  # -------------------------------------------------------------
  # Explanatory figures
  source("./Source/Questionnaire/Fishery_figs.R",local=TRUE)
  source("./Source/Questionnaire/Management_figs.R",local=TRUE)
  source("./Source/Questionnaire/Data_figs.R",local=TRUE)

  # Presentation of results
  source("./Source/Skins/Generic.R",local=TRUE)
  source("./Source/Skins/ABNJ.R",local=TRUE)
  source("./Source/Skins/MSC.R",local=TRUE)
  # source("./Source/Skins/Train.R",local=TRUE) # replaced with Generic now
 
  
  #source("./Analysis_results.R",local=TRUE)
  source("./Source/AI/AI_results.R",local=TRUE)
  #source("./VOI.R",local=TRUE)
  #source("./Fease.R",local=TRUE)

  # OM construction / translation
  source("./Source/OM/makeOM.R",local=TRUE)
  source("./Source/OM/ML2D.R",local=TRUE)
  source('./Source/OM/Backwards.R',local=TRUE) # Stochastic SRA until progress bar update comes to DLMtool
  source('./Source/OM/Scoping.R',local=TRUE)
  
  # Reporting
  source("./Source/Reports/OM_report.R",local=TRUE)

  # MSE running / indicator calculation
  source("./Source/MSE/Redos.R",local=TRUE)
  
  # Advice
  source("./Source/Advice/Advice.R",local=TRUE)
  
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
  Opanel<-reactiveVal(0)
  Started<-reactiveVal(0)
  Quest<-reactiveVal(0)
  Data<-reactiveVal(0)
  LoadOM<-reactiveVal(0)
  CondOM<-reactiveVal(0)
  MadeOM<-reactiveVal(0)
  RA<-reactiveVal(0) # Have run risk assessment (multi MP)
  SD<-reactiveVal(0) # Has a status determination been run yet?
  Plan<-reactiveVal(0) # Have run Planning (multi MP)
  Eval<-reactiveVal(0)  # Have run Evaluation (single MP)
  DataInd<-reactiveVal(0) # Indicator data loaded
  Ind<-reactiveVal(0)  # Have run Indicator (single MP)
  AdCalc<-reactiveVal(0) # Has advice been calculated
  Tweak<-reactiveVal(0)  # Have things affecting performance metrics been tweaked?
  SkinNo<-reactiveVal(0)   # Skin selection
 
  output$Fpanel <- reactive({ Fpanel()})
  output$Mpanel <- reactive({ Mpanel()})
  output$Dpanel <- reactive({ Dpanel()})
  output$Opanel <- reactive({ Opanel()})

  output$Started  <- reactive({ Started()})
  output$Quest    <- reactive({ Quest()})
  output$Data     <- reactive({ Data()})
  output$CondOM   <- reactive({ CondOM()})
  output$LoadOM   <- reactive({ LoadOM()})
  output$MadeOM   <- reactive({ MadeOM()})

  output$RA       <- reactive({ RA()})
  output$SD       <- reactive({ SD()})
  output$Plan     <- reactive({ Plan()})
  output$Eval     <- reactive({ Eval()})
  output$DataInd  <- reactive({ DataInd()})
  output$Ind      <- reactive({ Ind()})

  output$AdCalc   <- reactive({ AdCalc()})
  output$Tweak    <- reactive({Tweak()})
  
  output$SkinNo     <- reactive({SkinNo()})
  
  outputOptions(output,"Fpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Mpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Dpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Opanel",suspendWhenHidden=FALSE)

  outputOptions(output,"Started",suspendWhenHidden=FALSE)
  outputOptions(output,"Data",suspendWhenHidden=FALSE)
  outputOptions(output,"Quest",suspendWhenHidden=FALSE)

  outputOptions(output,"LoadOM",suspendWhenHidden=FALSE)
  outputOptions(output,"CondOM",suspendWhenHidden=FALSE)
  outputOptions(output,"MadeOM",suspendWhenHidden=FALSE)

  outputOptions(output,"RA",suspendWhenHidden=FALSE)
  outputOptions(output,"SD",suspendWhenHidden=FALSE)
  outputOptions(output,"Plan",suspendWhenHidden=FALSE)
  outputOptions(output,"Eval",suspendWhenHidden=FALSE)
  outputOptions(output,"DataInd",suspendWhenHidden=FALSE)
  outputOptions(output,"Ind",suspendWhenHidden=FALSE)

  outputOptions(output,"AdCalc",suspendWhenHidden=FALSE)
  outputOptions(output,"Tweak",suspendWhenHidden=FALSE)
  
  outputOptions(output,"SkinNo",suspendWhenHidden=FALSE)
  
  output$Fpanelout <- renderText({ paste("Fishery",Fpanel(),"/ 19")})
  output$Mpanelout <- renderText({ paste("Management",Mpanel(),"/ 7")})
  output$Dpanelout <- renderText({ paste("Data",Dpanel(),"/ 4")})
  output$Opanelout <- renderText({ paste("Extra",Opanel(),"/ 5")})

  # Update UI
  output$Version<-renderText(paste0("method evaluation and risk assessment    (MSC-DLMtool App v", Version, ")")) #"method evaluation and risk assessment    (MSC-DLMtool App v4.1.7)"
  output$Dependencies<-renderText(paste0("Powered by: DLMtool v", packageVersion('DLMtool'), "  /  MSEtool v",packageVersion('MSEtool'))) #"method evaluation and risk assessment    (MSC-DLMtool App v4.1.7)"

  
  # Skin changing tips: you need to:
  # (A) add an Icon to /www/<skin>.png   must be a .png to go with the reporting params
  # (B) add the name to the list at line Skin_nams =  (below)   
  # (C) go to the UI at tags$a(img(src = "DLMtool.png" and add a conditional panel
  
  
  tt <- try(!is.null(MERA:::PKGENVIR$skin), silent=TRUE)
  if (class(tt) == 'try-error') {
    skin <- 'Generic'
  } else {
    if (!is.null(MERA:::PKGENVIR$skin)) {
      skin <- MERA:::PKGENVIR$skin
    } else {
      skin <- "Generic"
    }
  }
       
  dat<-dat_int<-NULL
      
  Skin_nams<<-c("Generic","MSC","ABNJ") # unlist(strsplit(list.files(path="./Source/Skins"),".R"))
  updateSelectInput(session=session,inputId="Skin",choices=Skin_nams[length(Skin_nams):1],selected=skin)
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['skin']])) {
      updateSelectInput(session, "Skin", label = "", choices = NULL,
                        selected = query[['skin']])
    }
  })
  
  observeEvent(input$Skin,{
    temp<-input$Skin
    Skin<<-get(temp) 
    SkinNo(match(temp,Skin_nams))
    smartRedo() 
   
    AM(paste0("Skin selected: ",temp))
    
  })# update MP selection in Evaluation
  
  
  #shinyjs::hide("Skin")
  shinyjs::hide("Demo_mode")
  
  #onevent("mouseenter", "SkinArea", {
  #           shinyjs::show("Skin")
  #        }
  #)
  #onevent("mouseleave", "SkinArea", {
  #          shinyjs::hide("Skin")
  #        }
  #)
  
  onevent("mouseenter", "DemoArea", {
    shinyjs::show("Demo_mode")
          }
  )
  
  onevent("mouseleave", "DemoArea", {
    shinyjs::hide("Demo_mode")
          }
  )
 
  
  # Some useful things
  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  CurrentYr<-as.integer(substr(as.character(Sys.time()),1,4))
  Copyright<-"Open Source, GPL-2"
  
  FeaseMPs<<-NULL
  redoBlank() # make all the results plots with default sizes - seems to stabilize initial plotting and spacing
  
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


  # Default simulation attributes --------------------------------------------------------------------------------
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
    updateSelectInput(session=session,inputId="ManPlanMPsel",choices=getAllMPs(),selected="curE") 
    Update_Options()
    AM(paste0("Mode selected: ", input$Mode))
    smartRedo()
 
  })

  observeEvent(input$OM_L,{               # reset the MSE results if you change the OM type
    RA(0); SD(0); Plan(0); Eval()
  })
  
  observeEvent(input$OM_C,{               # reset the MSE results if you change the OM type
    RA(0); SD(0); Plan(0); Eval()
  })
  
  observeEvent(input$Demo_mode,{
    updateNumericInput(session=session,inputId='nsim_RA',value=24)
    updateNumericInput(session=session,inputId='nsim_SD',value=8)
    updateNumericInput(session=session,inputId='nsim_Plan',value=12)
    updateNumericInput(session=session,inputId='nsim_Eval',value=24)
    
  })
  
  # == File I/O ==========================================================================

  # Questionnaire save
  output$Save<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".mera"),

    content=function(file){
      Des<<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      if(Data()==1){
        PanelState$dat<-dat
        AM("Saving appended data")
      }
      if(DataInd()==1){
        PanelState$dat_ind<-dat_ind
      }
      
      #saveRDS(PanelState,"C:/temp/PanelState.rds")
      
      MSClog<-list(PanelState, Just, Des)
      
      #saveRDS(MSClog,"C:/temp/MSClog.rds")
      
      doprogress("Saving Questionnaire")
      saveRDS(MSClog,file)
      AM(paste0("Questionnaire saved:", file))

    }

  )

  # Questionnaire load
  observeEvent(input$Load,{

    filey<-input$Load
    tryCatch({

        MSClog<-readRDS(file=filey$datapath)
        cond<-length(MSClog)==3 & sum(names(MSClog[[1]])[1:4]==c("Fpanel","Mpanel","Dpanel","Slider"))==4
        AM(paste0("Questionnaire loaded:", filey$datapath))
        
        if(cond){
          PanelState<<-MSClog[[1]]
          Just<<-MSClog[[2]]

          # All panels except radio button on D4
          for(i in 1:2){
            for(j in 1:length(PanelState[[i]])) {
             
              state<-as.vector(unlist(PanelState[[i]][j]))
              choices<-as.vector(unlist(get(MasterList[[i]][j])))
              selected<-as.list(choices[state])
              choices<-as.list(choices)
              updateCheckboxGroupInput(session, as.character(inputnames[[i]][j]), selected = selected)
              
            }
          }
          
          i<-3
          
          for(j in 2:3){
            
            state<-as.vector(unlist(PanelState[[i]][j]))
            choices<-as.vector(unlist(get(MasterList[[i]][j])))
            selected<-as.list(choices[state])
            choices<-as.list(choices)
            updateCheckboxGroupInput(session, as.character(inputnames[[i]][j]), selected = selected)
            
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
          updateNumericInput(session, "nyears",   value= MSClog[[3]]$nyears)
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
          Opanel(1)
          Plan(0)
          
          
          if(!is.null(MSClog[[1]]$dat)){
            dat<<-MSClog[[1]]$dat
            Data(1)
            DataInd(0)
            FeaseMPs<<-Fease(dat)
            AM("Data loaded with questionnaire")
            if(!is.null(MSClog[[1]]$dat_ind)){
               dat_ind<<-MSClog[[1]]$dat_ind
               DataInd(1)
               AM("Additional data loaded since MP was adopted")
            }
            SD_codes<-getCodes(dat,maxtest=Inf)
            AM(paste0("Data object is compatible with the following status determination methods: ", SD_codes))
            updateSelectInput(session,'SDsel',choices=SD_codes,selected=SD_codes[1])
            updateSelectInput(session,'Cond_ops',choices=SD_codes,selected=SD_codes[1])
          }
           
          
        }else{
          AM(paste0("Questionnaire failed to load:", filey$datapath))
          shinyalert("File read error", "This does not appear to be a MERA questionnaire file", type = "error")
        }

      },
      error = function(e){
        AM(paste0("Questionnaire failed to load:", filey$datapath))
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
         Data(1) 
        
         AM(paste0(".csv data loaded:", filey$datapath))
        },
        error = function(e){
          shinyalert("Not a properly formatted DLMtool Data .csv file", "Trying to load as an object of class 'Data'", type = "error")
          Data(0)
          AM(paste0(".csv data failed to load:", filey$datapath))
          loaded=F
        }
      )

    }else{

      tryCatch(
        {
          dat<<-load(filey$datapath)
         
          AM(paste0("Data object loaded:", filey$datapath))
          Data(1)
        },
        error = function(e){
          shinyalert("Could not load object", "Failed to load this file as a formatted data object", type = "error")
          AM(paste0("Data object failed to load:", filey$datapath))
          Data(0)
        }
      )

      if(class(dat)!="Data"){
        shinyalert("Data load error!", "Failed to load this file as either a formatted .csv datafile or a DLMtool object of class 'Data'", type = "error")
        AM(paste0("Data object failed to load:", filey$datapath))
        Data(0)
      }
    }
    
    if(Data()==1){
      dat_test<-Data_trimer(dat)
  
      if(class(dat_test)!='Data'){
        DataInd(0)
  
      }else{
        dat_ind<<-dat
        dat<<-dat_test
        AM(paste0("Data object contains ", length(dat_ind@Year)-length(dat@Year)," years of indicator data after LHYear"))
  
        DataInd(1)
      }
      FeaseMPs<<-Fease(dat)
  
      #saveRDS(dat_ind,"C:/temp/dat_ind.rda")
      #saveRDS(dat,"C:/temp/dat.rda")
      
      SD_codes<-getCodes(dat,maxtest=Inf)
      AM(paste0("Data object is compatible with the following status determination methods: ", SD_codes))
      updateSelectInput(session,'SDsel',choices=SD_codes,selected=SD_codes[1])
      updateSelectInput(session,'Cond_ops',choices=SD_codes,selected=SD_codes[1])
    }
    
  })

 
  # OM save
  output$Save_OM<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".OM"),

    content=function(file){
      OM<-makeOM(PanelState,nsim=input$nsim_OMsave)
      AM(paste0("Operating model saved: ", file))
      doprogress("Saving Operating Model")
      saveRDS(OM,file)

    }

  )

  # OM load
  observeEvent(input$Load_OM,{

      filey<-input$Load_OM

      tryCatch({
        
        OM_L<<-readRDS(file=filey$datapath)
        
      },
      
      error = function(e){
        
        shinyalert("File read error", "This does not appear to be a DLMtool OM object, saved by saveRDS()", type = "error")
        AM(paste0("Operating model failed to load: ", filey$datapath))
        return(0)
      
      })

      if(class(OM_L)=='OM'){
        
        MPs<<-getMPs()
        MadeOM(1)
        LoadOM(1)
        updateCheckboxInput(session,"OM_L",value=TRUE)
        CondOM(0)
        Quest(0)
        AM(paste0("Operating model loaded: ", filey$datapath))
        
      }else{
        
        shinyalert("Incorrect class of object", "This file should be an object of DLMtool class 'OM'", type = "error")
        AM(paste0("Object not of class 'OM'", filey$datapath))
        
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
      listy<<-readRDS(file=filey$datapath)
      if (class(listy[[1]]) !='MSE') stop()
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
      MadeOM(0)
      CondOM(0)
      smartRedo()
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
      MadeOM(0)
      CondOM(0)
      Quest(0)
      Ind(0)
      smartRedo()
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
        AM(paste0("Source file loaded: ",filey$datapath))
        source(file=filey$datapath)
        updateSelectInput(session=session,inputId="sel_MP",choices=getAllMPs()) # update MP selection in Application
        updateSelectInput(session=session,inputId="ManPlanMPsel",choices=getAllMPs(),selected="curE") 

    },
      error = function(e){
        shinyalert("File read error", "Your source code did not load correctly. Try sourcing this file in an R session to debug errors", type = "error")
      }
    )

  })


#############################################################################################################################################################################
### Calculation functions
#############################################################################################################################################################################

  # OM conditioning ============================
  
  observeEvent(input$Cond,{
    
    code<-input$Cond_ops
    AM(paste0("Conditioning operating model using method ",code))
  
    OM<-makeOM(PanelState,nsim=input$nsim_OM,UseQonly=T)
    setup()
    
    tryCatch({
      
      withProgress(message = "Conditioning Operating Model", value = 0, {
        incProgress(0.1)
        CFit<<-GetDep(OM,dat,code=code,cores=4)
        
        if(sum(CFit@conv)==0)AM(paste0(code,": ",sum(CFit@conv), " of ",length(CFit@conv)," simulations converged"))
        
        incProgress(0.8)
     
      })
      
      OM_C<<-CFit@OM
      CondOM(1)
      updateCheckboxInput(session,"OM_C",value=TRUE)
 
   
    },
    error = function(e){
      shinyalert("Computational error", "Operating model conditionin returned an error. Try using a different model for conditioning.", type = "info")
      CondOM(0)
      return(0)
    }
    
    )
    
  }) # press calculate
  
  
  observeEvent(input$Calculate_risk,{
    
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
     shinyalert("Computational error", "This probably occurred because your simulated conditions are not possible.
                   For example a short lived stock a low stock depletion with recently declining effort.
                  Try revising operating model parameters.", type = "info")
      return(0)
    }
    
    )
    
  }) # press calculate
  
  
  observeEvent(input$Calculate_status,{
    
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
    Est<-Sim<-Fit<-list()
    
    #saveRDS(OM,"C:/temp/OM3")
    #saveRDS(dat,"C:/temp/dat3")
    #saveRDS(codes,"C:/temp/codes3")
    setup(cpus=4)
    
    tryCatch({
      
      withProgress(message = "Running Status Determination", value = 0, {
        
        for(cc in 1:ncode){
          Fit[[cc]]<-GetDep(OM,dat,code=codes[cc],cores=4)
          Est[[cc]]<-Fit[[cc]]@OM@cpars$D[Fit[[cc]]@conv]
          if(sum(Fit[[cc]]@conv)==0)AM(paste(cc,codes[cc],"Did not return depletion"))
          incProgress(1/ncode, detail = round(cc*100/ncode))
        }  
    
      })
      
      #saveRDS(OMsimsam,"C:/temp/OMsimsam")
      #saveRDS(Est,"C:/temp/Est")
      #saveRDS(codes,"C:/temp/codes")
      
    },
      error = function(e){
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
      shinyalert("Computational error", "One or more of the power models used to characterize estimation bias failed to converge. Try selecting a different set of status determination methods.", type = "info")
        return(0)
    })
    
    
  }) # press calculate
  
   
  observeEvent(input$Calculate_Plan,{
  
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
        print(e)
        shinyalert("Computational error", "This probably occurred because the fishery dynamics of your questionnaire are not possible.
                   For example, a short lived stock a low stock depletion with recently declining effort.
                  Try revising operating model parameters.", type = "info")
        return(0)
      }

    )

  }) # press calculate


  # ------------------------------------------------------------------------------------------------------------------------------------

  observeEvent(input$Calculate_Eval,{

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
        shinyalert("Computational error", "This probably occurred because your simulated conditions are not possible.
                   For example a short lived stock a low stock depletion with recently declining effort.
                   Try revising operating model parameters.", type = "info")
        return(0)
      }
    ) # try catch

  }) # calculate MSE app

  CheckJust<-function(){

    isjust<-function(x)sum(x=="No justification was provided")
    as.integer(sum(unlist(lapply(Just,isjust)))==0)

  }

  # Observe for selecting rows from tables
  P_Tab_1_track = observe({
    input$P_Tab_1_rows_selected
    # print(input$P_Tab_1_rows_selected)
    isolate({
      if(input$Skin == "Generic" & input$Mode =="Management Planning") {
        UpdateGenericSkinPlots()
      }  
    })
  }) 
  P_Tab_3_track = observe({
    input$P_Tab_3_rows_selected
    isolate({
      if(input$Skin == "Generic" & input$Mode =="Management Planning") {
        UpdateGenericSkinPlots()
      }  
    })
  }) 
  
  # Show REFRESH RESULTS if ...
  observeEvent(input$burnin,{ Tweak(1) })
  observeEvent(input$YIU,{ Tweak(1) })
  observeEvent(input$res,{ Tweak(1) })
  observeEvent(input$M1,{ Tweak(1) })
  observeEvent(input$D1,{ Tweak(1) })
  # observeEvent(input$minProb,{ Tweak(1) })

  # Update tables if ...

  observeEvent(input$Redo,{
    smartRedo()
    #Tweak(0)
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
      
      OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('Source/Markdown/OMRep.Rmd')
      src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
     
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'OMRep.Rmd', overwrite = TRUE)
      file.copy(src2, 'logo.png', overwrite = TRUE) #NEW

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
        
        # saveRDS(tempdir(),"C:/temp/tempdir.rda")
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        library(rmarkdown)
        output<-paste0(tempdir(),"/Data-Report.html")
        
        #Report(dat,title=paste0("Data Report for",input$Name),author=input$Author,quiet=T,overwrite=T,dir=getwd(),open=F)
        Report(dat,title=paste0("Data Report for",input$Name),author=input$Author,quiet=T,overwrite=T,open=F,dir=tempdir())
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
      src <- normalizePath('Source/Markdown/CondRep.Rmd')
      src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
     
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'CondRep.Rmd', overwrite = TRUE)
      file.copy(src2, 'logo.png', overwrite = TRUE) #NEW
      
      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Operating Model Conditioning Report for ",input$Name),
                     set_type=paste0("Demonstration Conditioning analysis"," (MERA version ",Version,")"),
                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM_C,
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
      src <- normalizePath('Source/Markdown/OM_full_Rep.Rmd')
      src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
     
      incProgress(0.1)
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'OM_full_Rep.Rmd', overwrite = TRUE)
      file.copy(src2, 'logo.png', overwrite = TRUE) #NEW
      
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
                     copyright=paste(Copyright,CurrentYr),
                     tabs=TRUE, 
                     Pars=OM,
                     plotPars=list(),
                     its=NULL,
                     nyears=OM@nyears,
                     proyears=OM@proyears
                       
      )
      incProgress(0.1)
      knitr::knit_meta(class=NULL, clean = TRUE) 
      output<-render(input="OM_full_Rep.Rmd",output_format="html_document", params = params)
      incProgress(0.8)
      file.copy(output, file)
      })
    }
  )

  # Conditioning report
  output$Cond_rep <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_Det_Cond.html")}, #"report.html",
    
    content = function(file) {
      withProgress(message = "Building Conditioning Report", value = 0, {
        
        incProgress(0.1)
        Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
        MSClog<-list(PanelState, Just, Des)
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        library(rmarkdown)
        
        incProgress(0.1)
        knitr::knit_meta(class=NULL, clean = TRUE) 
        OM<-CFit@OM
        output<-plot(CFit,open_file = FALSE)
        
        incProgress(0.8)
        file.copy(output, file)
      }) # end of progress
    }
  )
  
  
  
  
  output$Build_RA <-downloadHandler(
    
    filename = function(){paste0(namconv(input$Name),"_MERA_Risk_Assessment_Report.html")}, #"report.html",
    
    content = function(file) {
      withProgress(message = "Building Risk Assessment report", value = 0, {
        src <- normalizePath('Source/Markdown/RA.Rmd')
        src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
       
        Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
        MSClog<-list(PanelState, Just, Des)
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'RA.Rmd', overwrite = TRUE)
        file.copy(src2, 'logo.png', overwrite = TRUE) #NEW
        
        options=Skin$Risk_Assessment$options
        library(rmarkdown)
        options()
        params <- list(test = input$Name,
                       set_title=paste0("Risk Assessment Report for ",input$Name),
                       set_type=paste0("Risk Assessment of Status Quo Management "," (MERA version ",Version,")"),
                       Skin=Skin,
                       MSEobj=RAobj,
                       OM=OM,
                       MSEobj_reb=RAobj,
                       OM=OM,
                       options=options,
                       SessionID=SessionID,
                       copyright=paste(Copyright,CurrentYr)
        )
        knitr::knit_meta(class=NULL, clean = TRUE) 
        out<-render("RA.Rmd", output_format="html_document", params = params)
        file.rename(out, file)
      })
    }
    
  )
  
  output$Build_Status <-downloadHandler(
    
    filename = function(){paste0(namconv(input$Name),"_MERA_Status_Determination_Report.html")}, #"report.html",
    
    content = function(file) {
      withProgress(message = "Building Status Determination report", value = 0, {
        src <- normalizePath('Source/Markdown/SD.Rmd')
        src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
        
        Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
        MSClog<-list(PanelState, Just, Des)
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'SD.Rmd', overwrite = TRUE)
        file.copy(src2, 'logo.png', overwrite = TRUE) #NEW
        
        options=Skin$Risk_Assessment$options
        library(rmarkdown)
        options()
        params <- list(test = input$Name,
                       set_title=paste0("Status Determination Report for ",input$Name),
                       set_type=paste0("Status Determination "," (MERA version ",Version,")"),
                       Skin=Skin,
                       Status=Status,
                       options=options,
                       SessionID=SessionID,
                       Source=SessionID,
                       copyright=paste(Copyright,CurrentYr)
        )
        knitr::knit_meta(class=NULL, clean = TRUE) 
        out<-render("SD.Rmd", output_format="html_document", params = params)
        file.rename(out, file)
      })
    }
    
  )
  
  
  # Conditioning report
  output$SDdet_rep <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_Det_Cond.html")}, #"report.html",
    
    content = function(file) {
      withProgress(message = "Building detailed Status Determination report", value = 0, {
        
        incProgress(0.1)
        Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
        MSClog<-list(PanelState, Just, Des)
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
         
        library(rmarkdown)
       
        incProgress(0.1)
        knitr::knit_meta(class=NULL, clean = TRUE) 
        ccno<-match(input$SDdet,codes)
        OM<-Fit[[ccno]]@OM
        output<-plot(Fit[[ccno]],open_file = FALSE)
           
        incProgress(0.8)
        file.copy(output, file)
      }) # end of progress
    }
  )
  
  
  
  output$Build_Plan <-downloadHandler(
    
    filename = function(){paste0(namconv(input$Name),"_MERA_Planning_Report.html")}, #"report.html",
    
    content = function(file) {
      withProgress(message = "Building planning report", value = 0, {
        src <- normalizePath('Source/Markdown/Plan.Rmd')
        src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
      
        Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
        MSClog<-list(PanelState, Just, Des)
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'Plan.Rmd', overwrite = TRUE)
        file.copy(src2, 'logo.png', overwrite = TRUE) #NEW
        options=Skin$Risk_Assessment$options
        library(rmarkdown)
        options <- list(burnin = input$burnin, res=input$res,
                        tab1.row.select=input$P_Tab_1_rows_selected,
                        tab4.row.select=input$P_Tab_4_rows_selected,
                        train_nplot=length(input$P_Tab_1_rows_selected),
                        train_nplot_4=length(input$P_Tab_4_rows_selected))
        
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
        src <- normalizePath('Source/Markdown/Eval.Rmd')
        src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
       
        Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
        MSClog<-list(PanelState, Just, Des)
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'Eval.Rmd', overwrite = TRUE)
        file.copy(src2, 'logo.png', overwrite = TRUE) #NEW
        options=Skin$Risk_Assessment$options
        library(rmarkdown)
        
        options <- list()
        
        params <- list(test = input$Name,
                       set_title=paste0("Evaluation Report for ",input$Name),
                       set_type=paste0("Multiple MP testing "," (MERA version ",Version,")"),
                       Skin=Skin,
                       MSEobj=MSEobj_Eval,
                       dat=dat,
                       dat_ind=dat_ind,
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
  
 
  # Fishery panel reactions ============================================================================================================

  observeEvent(input$Justification,{
    RecJust()
  })

  observeEvent(input$tabs1, {

    UpJust()
    Des<<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
    #MSCsave_auto()
    #getMPs()
    #selectedMP<<-MPs[2]

  })

  observeEvent(input$Fback,{

    if(input$tabs1==1 && Fpanel() >0){
      Fpanel(Fpanel()-1)
    }else if(input$tabs1==2 && Mpanel() >0){
      Mpanel(Mpanel()-1)
    }else if(input$tabs1==3 && Dpanel() >0){
      Dpanel(Dpanel()-1)
    }else if(input$tabs1==4 && Opanel() >0){
      Opanel(Opanel()-1)
    }  
    UpJust()

    Des<<-list(Name=input$Name,Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
    #MSCsave_auto()

  })

  observeEvent(input$Fcont,{

    if(input$tabs1==1 && Fpanel() < 19){
      Fpanel(Fpanel()+1)
    }else if(input$tabs1==2 && Mpanel() < 7){
      Mpanel(Mpanel()+1)
    }else if(input$tabs1==3 && Dpanel() < 4){
      Dpanel(Dpanel()+1)
    }else if(input$tabs1==4 && Opanel() < 6){
      Opanel(Opanel()+1)
    }

    # Write old values
    UpJust()

    Des<<-list(Name=input$Name,Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
    #MSCsave_auto()

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

  #observeEvent(input$debug,
  #            updateTextAreaInput(session,"Debug1",value=MadeOM())
  #)
  
  # observeEvent(input$P_Tab_1_rows_selected, {
  #   row.select <<- input$P_Tab_1_rows_selected
  #   print("*1.**********")
  #   print(row.select)
  #   print("----------")
  #   
  # })

})
