quick <- FALSE # switch to use 3 sims for quick test runs in RA mode

library(shiny)
library(MSEtool)
library(DLMtool)
library(SAMtool)
library(kableExtra)
library(formattable)
library(knitr)
library(dplyr)
library(httpuv)
library(shinyalert)
library(DT)
library(mvtnorm)
library(cowplot)
library(shinyBS)
#library(parallel)

options(shiny.maxRequestSize=1000*1024^2)

source("./global.R")

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {

  Version<<-"6.1.12"
  
  # -------------------------------------------------------------
  # Explanatory figures
  source("./Source/Questionnaire/Fishery_figs.R",local=TRUE)
  source("./Source/Questionnaire/Management_figs.R",local=TRUE)
  source("./Source/Questionnaire/Data_figs.R",local=TRUE)
  source("./Source/Questionnaire/Effort_sketching.R",local=TRUE)

  # Presentation of results
  source("./Source/AI/AI_results.R",local=TRUE)
  source("./Source/Skins/Generic.R",local=TRUE)
  source("./Source/Skins/ABNJ.R",local=TRUE)
  source("./Source/Skins/MSC.R",local=TRUE)
  
  # OM construction / translation
  source("./Source/OM/makeOM.R",local=TRUE)
  source("./Source/OM/ML2D.R",local=TRUE)
  source('./Source/OM/Backwards.R',local=TRUE) # Stochastic SRA until progress bar update comes to DLMtool
  source('./Source/OM/Scoping.R',local=TRUE)
  
  # Reporting
  source("./Source/Reports/OM_report.R",local=TRUE)

  # MSE running / indicator calculation
  source("./Source/MSE/Calculations.R",local=TRUE)
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
  source("./Source/App/IO.R",local=TRUE)  # functions that deal with input/output of objects (file load/save)
  source("./Source/App/Tooltips.R",local=TRUE)  # tooltip info
  source("./Source/App/Data_gate.R",local=TRUE) # Data gate keeper for ensuring compatability with questionnaire
  

  # Miscellaneous
  source("./Source/Misc/Misc.R",local=TRUE)
  source("./Source/Misc/Extra_Figures.R",local=TRUE)
 
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
  RA<-reactiveVal(0)     # Have run risk assessment (multi MP)
  SD<-reactiveVal(0)     # Has a status determination been run yet?
  Plan<-reactiveVal(0)   # Have run Planning (multi MP)
  Eval<-reactiveVal(0)   # Have run Evaluation (single MP)
  DataInd<-reactiveVal(0)# Indicator data loaded
  Ind<-reactiveVal(0)    # Have run Indicator (single MP)
  AdCalc<-reactiveVal(0) # Has advice been calculated
  Tweak<-reactiveVal(0)  # Have things affecting performance metrics been tweaked?
  SkinNo<-reactiveVal(0) # Skin selection
  #Start<-reactiveVal(0)  # Start App?
 
  #LHYear<<-2018          # Starting value for the global variable that divides conditioning and indicator data
  
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
  
  output$SkinNo   <- reactive({SkinNo()})
  
  #output$Start    <- reactive({Start()})
  
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
  
  #outputOptions(output,"Start",suspendWhenHidden=FALSE)
  
   
  #output$allMPs(getAllMPs())
  updateSelectInput(session=session,inputId="sel_MP",choices=getAllMPs())
  output$Fpanelout <- renderText({ paste("Fishery",Fpanel(),"/ 19")})
  output$Mpanelout <- renderText({ paste("Management",Mpanel(),"/ 7")})
  output$Dpanelout <- renderText({ paste("Data",Dpanel(),"/ 4")})
  #output$Opanelout <- renderText({ paste("Extra",Opanel(),"/ 1")})

  # Update UI
  output$Version<-renderText(paste0("MSC-DLMtool App v", Version)) 
  output$Dependencies<-renderText(paste0("Powered by: DLMtool v", packageVersion('DLMtool'), "  /  MSEtool v",packageVersion('MSEtool'))) #"method evaluation and risk assessment    (MSC-DLMtool App v4.1.7)"
  output$Version_help<-renderText(paste0("MSC-DLMtool App v", Version)) 
  output$Dependencies_help<-renderText(paste0("Powered by: DLMtool v", packageVersion('DLMtool'), "  /  MSEtool v",packageVersion('MSEtool'))) #"method evaluation and risk assessment    (MSC-DLMtool App v4.1.7)"
  
  
  # Skin changing tips: you need to:
  # (A) add an Icon to /www/<skin>.png   must be a .png to go with the reporting params
  # (B) add the name to the list at line Skin_nams =  (below)   
  # (C) go to the UI at tags$a(img(src = "DLMtool.png" and add a conditional panel
  
  
  tt <- try(!is.null(MERA:::PKGENVIR$skin), silent=TRUE)
  if (class(tt) == 'try-error') {
    skin <- 'MSC'
  } else {
    if (!is.null(MERA:::PKGENVIR$skin)) {
      skin <- MERA:::PKGENVIR$skin
    } else {
      skin <- "MSC"
    }
  }
       
  dat<-dat_int<-NULL
      
  Skin_nams<<-c("MSC")#,"ABNJ","Generic") # unlist(strsplit(list.files(path="./Source/Skins"),".R"))
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
  
  
  # Some useful things
  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  
  
 
  Copyright<-"Open Source, GPL-2"
  
  # Log stuff
  Log_text <- reactiveValues(text=paste0("-------- Start of Session -------- \nSession ID: ",SessionID,"\nUser ID: ",USERID))
  output$Log <- renderText(Log_text$text)
  
  # Variables
  ncpus<-4 # min(12,detectCores()) # how much parallel processing to use
  #AM(paste(ncpus,"processors available for computing"))
  FeaseMPs<<-NULL
  redoBlank() # make all the results plots with default sizes - seems to stabilize initial plotting and spacing
  
  Just<-list(
    c(
      "1. Describe the history and current status of the fishery, including fleets, sectors, vessel types and practices/gear by vessel type, landing ports, economics/markets, whether targeted/bycatch, other stocks caught in the fishery.

      2. Describe the stocks ecosystem functions, dependencies, and habitat types.

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
  nyears<<-68#input$Lyear-input$Syear+1 # 1950-2018
  #nsim<-48

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
   
  })

  observeEvent(input$Mode,{
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

  
  # == File I/O ==========================================================================

  # Questionnaire save
  output$Save<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".mera"),
    
    content=function(file){
      #saveRDS(as.list(eff_Values),"C:/temp/eff_values.rda")
      MSClog<-package_Questionnaire()
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
        Update_Questionnaire(MSClog)
        #Start(1)
       
      },
      error = function(e){
        AM(paste0(e,"\n"))
        shinyalert("File read error", "This does not appear to be a MERA questionnaire file", type = "error")
        return(0)
      }
    )

  })

  # Data load
  observeEvent(input$Load_Data,{
    #filey=list(datapath=paste0(getwd(),"/Data_from_SS.rda"))
    filey<-input$Load_Data
    dattest=NULL
   
    if(grepl(".csv",filey$datapath)|grepl(".xlsx",filey$datapath)){ # if it is a .csv file

      tryCatch(
        {
         dattest<-XL2Data(filey$datapath)
         #saveRDS(dattest,'C:/temp2/dattest.rda') # 
        
         AM(paste0("data loaded:", filey$datapath))
        },
        error = function(e){
          AM(paste0(e,"\n"))
          shinyalert("Not a properly formatted MSEtool Data .csv or .xlsx file", paste("Trying to load as an object of class 'Data'",e,collapse="\n"), type = "error")
          Data(0)
          loaded=F
        }
      )

     }else{

      tryCatch(
        {
          dattest<-readRDS(filey$datapath)
         
          AM(paste0("Data object loaded:", filey$datapath))
         
        },
        error = function(e){
          AM(paste0(e,"\n"))
          shinyalert("Could not load object", paste("Failed to load this file as a formatted data object",e), type = "error")
          Data(0)
        }
      )

      if(class(dattest)!="Data"){
        shinyalert("Data load error!", "Failed to load this file as either a formatted .csv datafile or a MSEtool object of class 'Data'", type = "error")
        AM(paste0("Data object failed to load:", filey$datapath))
        Data(0)
      }
    }
   
    # now see whether trimmed data work with the questionnaire
    
    if(!is.null(dattest)){
      LHYear<<-input$Lyear
      
      if(!is.null(dattest@LHYear)){
        updateNumericInput(session,"Lyear",value=dattest@LHYear,min=min(dattest@Year)+5,max=max(dattest@Year))
        LHYear<-dattest@LHYear
        AM("Data slot LHYear used to set end year in Fishery Question 1 (years before and including this are used for conditioning after which data are used in Management Performance mode) ")
      }else{
        updateNumericInput(session,"Lyear",value=max(dattest@Year),min=min(dattest@Year)+5,max=max(dattest@Year))
        LHYear<-max(dattest@Year)
        AM("Data slot Year used to specify fishery end year in Fishery Question 1")
      }
      updateNumericInput(session,"Syear",value=min(dattest@Year),min=min(dattest@Year),max=min(dattest@Year))
      AM("Data slot Year used to specify fishery start year in Fishery Question 1")
      #saveRDS(LHYear,"C:/temp/LHYear.rda") #
      dat_trim<-Data_trimer(dattest,Syear=min(dattest@Year),LHYear)
      
      if(class(dat_trim)!='Data'){ # indicator data not availble
        DataInd(0)
        dat_temp<-dattest
        AM("No indicator data included (since MP adoption delineated by dat@LHYear)")
      }else{                       # indicator data available
        dat_ind<<-dattest
        DataInd(1)
        AM(paste0("Data object contains ", max(dat_ind@Year)-LHYear," years of indicator data after LHYear"))
        dat_temp<-dat_trim
      }
      
      daterrs<-Data_gate(dat_temp) # do the trimmed (historical data) have errors?
      
      if(length(daterrs)!=0){
        shinyalert(title="Data not imported due to formatting inconsistencies", text=paste(unlist(daterrs),collapse="\n"), type="error")
        Data(0)
        DataInd(1)
      }else{
        dat<<-dat_temp
        Data(1) 
      }
      
    }
  
    if(Data()==1){
         
      FeaseMPs<<-Fease(dat)
      SD_codes<-getCodes(dat,maxtest=Inf)
      AM(paste0("Data object is compatible with the following status determination methods: ", paste(SD_codes,collapse=",")))
      updateSelectInput(session,'SDsel',choices=SD_codes,selected=SD_codes[1])
      updateSelectInput(session,'Cond_ops',choices=SD_codes,selected=SD_codes[1])
      
    }
    
    # saveRDS(dat,"C:/temp/dat.rda") #
    #saveRDS(dat_ind,"C:/temp/dat_ind.rda") # 
  })

 
  # OM save
  output$Save_OM<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".OM"),

    content=function(file){
      
      if(checkQs()$error){shinyalert("Incomplete Questionnaire", text=paste("The following questions have not been answered:",paste(temp$probQs,collapse=", ")), type = "warning");stop()}
      if(MadeOM()==0)OM<-makeOM(PanelState,nsim=input$nsim)
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
        #Start(1)
        
      },
      
      error = function(e){
        AM(paste0(e,"\n"))
        shinyalert("File read error", "This does not appear to be a MSEtool OM object, saved by saveRDS()", type = "error")
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
        #Start(1)
        
      }else{
        
        shinyalert("Incorrect class of object", "This file should be an object of MSEtool class 'OM'", type = "error")
        AM(paste0("Object not of class 'OM'", filey$datapath))
        
      }

  })

 
  # Session save
  output$Save_session<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".merasession"),

    content=function(file){
      MSClog<-package_Questionnaire()
      doprogress("Saving MERA session")
      if(!exists("MSEobj"))MSEobj=NULL
      if(!exists("MSEobj_reb"))MSEobj_reb=NULL
      if(!exists("RAobj"))RAobj=NULL
      if(!exists("MSEobj_Eval"))MSEobj_Eval=NULL
      if(!exists("Status"))Status=NULL
      if(!exists("OM"))OM=NULL
      if(!exists("SampList"))SampList=NULL
      MPnams<-avail('MP')
      MPnams<-MPnams[!(MPnams%in%c(ls("package:MSEtool"),ls("package:DLMtool")))]
      MPList<-list()
      for(i in 1:length(MPnams))MPList[[MPnams[i]]]<-get(MPnams[i])
      saveRDS(list(MSEobj=MSEobj,MSEobj_reb=MSEobj_reb,RAobj=RAobj,MSEobj_Eval=MSEobj_Eval,MSClog=MSClog,Status=Status,OM=OM,SampList=SampList,MPList=MPList),file)

    }

  )

  # Session load
  observeEvent(input$Load_session,{

    filey<-input$Load_session

    tryCatch({
        listy<<-readRDS(file=filey$datapath)
      },
      error = function(e){
        AM(paste0(e,"\n"))
        shinyalert("File read error", "This does not appear to be a MERA evaluation object", type = "error")
        return(0)
      }
    )
    Update_Questionnaire(listy$MSClog)
    
    MSEobj<<-listy$MSEobj
    MSEobj_reb<<-listy$MSEobj_reb
    RAobj<<-listy$RAobj
    MSEobj_Eval<<-listy$MSEobj_Eval
    Status<<-listy$Status
    OM<<-listy$OM
    if(!is.null(listy$SampList))SampList<<-listy$SampList
    if(!is.null(listy$MPList))for(i in 1:length(listy$MPList))assign(names(listy$MPList)[i],listy$MPList[[i]],envir=.GlobalEnv);AM(paste("Custom MPs loaded:",paste(names(listy$MPList),collapse=", ")))
    Plan(0); Eval(0); SD(0); CondOM(0); RA(0); MadeOM(0) # reset status switches
    redoBlank()
    #"Management Planning","Management Performance","Risk Assessment","Status Determination"
    if(!is.null(MSEobj_Eval)){Eval(1);AM("Management Performance results loaded");redoEval();updateRadioButtons(session=session,"Mode",selected="Management Performance")}
    if(!is.null(Status)){SD(1);CondOM(1);AM("Status Determination results loaded");redoSD();updateRadioButtons(session=session,"Mode",selected="Status Determination")}
    if(!is.null(RAobj)){RA(1);AM("Risk Assessment results loaded");redoRA();updateRadioButtons(session=session,"Mode",selected="Risk Assessment")}
    if(!is.null(MSEobj)){Plan(1);AM("Management Planning results loaded");redoPlan(); updateRadioButtons(session=session,"Mode",selected="Management Planning")}
    if(!is.null(OM)){
      AM("Operating model loaded")
      updateNumericInput(session,'nsim', value=OM@nsim)
      nsim<<-OM@nsim
      updateNumericInput(session,'interval',value=OM@interval)
      MadeOM(1)
    }
   
    #Start(1)
    updateTabsetPanel(session,"Res_Tab",selected="1")
    
  })

  observeEvent(input$getMPhelp,{
    #js$browseURL(MPurl(input$help_MP))
  })

  # End of file I/O ===================================================================================

  observeEvent(input$sel_MP,{
    selectedMP<<-input$sel_MP
    AM(paste("MP",selectedMP,"selected for management performance evalulation"))
  })

  observeEvent(input$Load_anything,{
   tryCatch(
      {
        filey<-input$Load_anything
        AM(paste0("Source file loaded: ",filey$datapath))
        source(file=filey$datapath)
        updateSelectInput(session=session,inputId="sel_MP",choices=getAllMPs()) # update MP selection in Evaluation
        updateSelectInput(session=session,inputId="ManPlanMPsel",selected=c("DCAC","matlenlim","MRreal","curE75","IT10"),choices=getAllMPs()) 
        

    },
      error = function(e){
        AM(paste0(e,sep="\n"))
        shinyalert("File read error", "Your source code did not load correctly. Try sourcing this file in an R session to debug errors", type = "error")
      }
    )

  })
  
  observeEvent(input$DemoMPs,{
    updateSelectInput(session=session,inputId="ManPlanMPsel",selected = c("DCAC","matlenlim","MRreal","curE75","IT10"),choices=getAllMPs())
  })
  
  observeEvent(input$Top20MPs,{
    updateSelectInput(session=session,inputId="ManPlanMPsel",selected = c("DCAC","DBSRA", "DBSRA4010", "DD","DDe","DDe75",  "DD4010","MCD","MCD4010","IT10","IT5",  "MRreal","MRnoreal","matlenlim","matlenlim2","DCAC_40", "DBSRA_40","Fratio","HDAAC","ITe10"),choices=getAllMPs())
  })
  
  observeEvent(input$AllMPs,{
    updateSelectInput(session=session,inputId="ManPlanMPsel",selected = getAllMPs(),choices=getAllMPs())
  })
  
  observeEvent(input$ Status_Quo,{
    updateSelectInput(session=session,inputId="ManPlanMPsel",selected = c("curE","CurC","FMSYref"),choices=getAllMPs())
  })
 
  
  observeEvent(input$Ex_Ref_MPs,{
    tempMPs<-input$ManPlanMPsel
    refMPs<-c("FMSYref","FMSYref75","FMSYref50","NFref")
    if(any(tempMPs%in%refMPs)){
      updateSelectInput(session=session,inputId="ManPlanMPsel",selected=tempMPs[!(tempMPs%in%refMPs)])
      AM("Reference MPs excluded from Management Planning")
    }else{
      updateSelectInput(session=session,inputId="ManPlanMPsel",selected=unique(c(tempMPs,refMPs)))
      AM("Reference MPs included in Management Planning")
    }
    
  })
  
  observeEvent(input$Data_Rich,{
    tempMPs<-input$ManPlanMPsel
    refMPs<-c("DDSS_4010","DDSS_MSY","SPSS_4010","SPSS_MSY","SCA_MSY","SCA_4010")
    if(any(tempMPs%in%refMPs)){
      updateSelectInput(session=session,inputId="ManPlanMPsel",selected=tempMPs[!(tempMPs%in%refMPs)])
      AM("Data-rich MPs excluded from Management Planning")
    }else{
      updateSelectInput(session=session,inputId="ManPlanMPsel",selected=unique(c(tempMPs,refMPs)))
      AM("Data-rich MPs included in Management Planning")
    }
    
  })
  
  observeEvent(input$StatusQuo_MPs,{
    
    tempMPs<-input$ManPlanMPsel
    refMPs<-c("curE","curE75","CurC")
    
    if(any(refMPs%in%tempMPs)){
      updateSelectInput(session=session,inputId="ManPlanMPsel",selected=tempMPs[!(tempMPs%in%refMPs)])
      AM("Status-quo MPs excluded from Management Planning")
    }else{
      updateSelectInput(session=session,inputId="ManPlanMPsel",selected=unique(c(tempMPs,refMPs)))
      AM("Status-quo MPs included in Management Planning")
    }
    
  })
  
  observeEvent(input$Clear_MPs,{
    updateSelectInput(session=session,inputId="ManPlanMPsel",selected="")
    AM("MP selection cleared for Management Planning")
  })
  
  observeEvent(input$DemoSims,{
    updateNumericInput(session=session,inputId="nsim",value=24)
    AM("Demonstration number of operating model simulations selected (24)")
  })
  
  observeEvent(input$DefSims,{
    updateNumericInput(session=session,inputId="nsim",value=64)
    AM("Default number of operating model simulations selected (64)")
  })

  observeEvent(input$RemakeOM,{
    temp<-checkQs()
    if(temp$error){
      
      shinyalert("Incomplete Questionnaire", text=paste("The following questions have not been answered or answered incorrectly:",paste(temp$probQs,collapse=", ")), type = "warning")
      
    }else{
      doprogress("Building OM",1)
      OM<<-makeOM(PanelState)
      
    }
    
  })
  
  
#############################################################################################################################################################################
### Calculation functions
#############################################################################################################################################################################

  observeEvent(input$Calculate,{
    
    temp<-checkQs()
    if(temp$error){
      
      shinyalert("Incomplete Questionnaire", text=paste("The following questions have not been answered or answered incorrectly:",paste(temp$probQs,collapse=", ")), type = "warning")
     
    }else{
    
      if(input$Mode=="Management Planning"){
        Calc_Plan()
      }else if(input$Mode=="Management Performance"){
        Calc_Perf()       
      }else{
        Calc_Status()
      }
    }  
  
  }) # press calculate
  

  # ------------------------------------------------------------------------------------------------------------------------------------

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
      AM("Building MERA questionnaire report")
      withProgress(message = "Building questionnaire report", value = 0, {
      if(checkQs()$error){shinyalert("Incomplete Questionnaire", text=paste("The following questions have not been answered:",paste(temp$probQs,collapse=", ")), type = "warning");stop()}
      if(MadeOM()==0)OM<<-makeOM(PanelState)
      src <- normalizePath('Source/Markdown/OMRep.Rmd')
      src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
     
      MSClog<<-package_MSClog()

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
      AM("Building MERA Data report")
      withProgress(message = "Building data report", value = 0, {
         
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        library(rmarkdown)
        output<-paste0(tempdir(),"/Data-Report.html")
        
        #Report(dat,title=paste0("Data Report for",input$Name),author=input$Author,quiet=T,overwrite=T,dir=getwd(),open=F)
        
        tryCatch({
          Report(dat,title=paste0("Data Report for",input$Name),author=input$Author,quiet=T,overwrite=T,open=F,dir=tempdir())
        },
        error = function(e){
          AM(paste0(e,"\n"))
          shinyalert("Data report build error", paste(e), type = "info")
          #return(0)
        })
        
        incProgress(0.7)
        file.copy(output, file)
        incProgress(0.1)
        
      }) # end of progress
    }
    
  )
  
  
 
  # Full OM report
  output$Build_full_OM <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_full_OM.html")}, #"report.html",

    content = function(file) {
      AM("Building detailed MERA operating model report")
      tryCatch({
        withProgress(message = "Building operating model report", value = 0, {
          if(checkQs()$error){shinyalert("Incomplete Questionnaire", text=paste("The following questions have not been answered:",paste(temp$probQs,collapse=", ")), type = "warning");stop()}
          if(MadeOM()==0)OM<<-makeOM(PanelState)
          src <- normalizePath('Source/Markdown/OM_full_Rep.Rmd')
          src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
          incProgress(0.1)
        
          MSClog<<-package_MSClog()
    
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
          #saveRDS(params,"C:/temp/params.rda") # ! alert
          incProgress(0.1)
          knitr::knit_meta(class=NULL, clean = TRUE) 
          output<-render(input="OM_full_Rep.Rmd",output_format="html_document", params = params)
          incProgress(0.8)
          file.copy(output, file)
        }) # with progres
      },
      error = function(e){
        AM(paste0(e,"\n"))
        shinyalert("Full OM report build error", paste(e), type = "info")
      })  
  
    }  # content
  )


  output$Build_Status <-downloadHandler(
    
    filename = function(){paste0(namconv(input$Name),"_MERA_Status_Determination_Report.html")}, #"report.html",
   
    content = function(file) {
      tryCatch({
        withProgress(message = "Building Status Determination report", value = 0, {
          src <- normalizePath('Source/Markdown/SD.Rmd')
          src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
          
          MSClog<<-package_MSClog()
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
      },
      error = function(e){
        AM(paste0(e,"\n"))
        shinyalert("Status Determination Report build error", paste(e), type = "info")
      }) 
    }
    
  )
  
  
  # Conditioning report
  output$SDdet_rep <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_Det_Cond.html")}, #"report.html",
    
    content = function(file) {
      withProgress(message = "Building model fitting report", value = 0, {
        
        incProgress(0.1)
        MSClog<<-package_MSClog()
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
         
        library(rmarkdown)
       
        incProgress(0.1)
        knitr::knit_meta(class=NULL, clean = TRUE) 
        
        output<-plot(Status$Fit[[1]],sims=Status$Fit[[1]]@conv,open_file = FALSE)
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
      
        MSClog<<-package_MSClog()
        
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
    
    filename = function(){paste0(namconv(input$Name),"_MERA_Performance_Report.html")}, #"report.html",
    
    content = function(file) {
      withProgress(message = "Building evaluation report", value = 0, {
        src <- normalizePath('Source/Markdown/Eval.Rmd')
        src2 <-normalizePath(paste0('www/',input$Skin,'.png'))
       
        MSClog<<-package_MSClog()
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

  output$Download_Log <-downloadHandler(
    
    filename = function(){paste0(namconv(input$Name),"_Log.txt")}, #"report.html",
    
    content = function(file) {
      writeLines(paste(Log_text$text, collapse = ", "), file)
    }
    
  )
 
  # Fishery panel reactions ============================================================================================================

  observeEvent(input$Justification,{
    RecJust()
  })

  observeEvent(input$tabs1, {

    UpJust()
    Des<<-package_MSClog()$Des
    
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

    Des<<-package_MSClog()
    #MSCsave_auto()

  })

  observeEvent(input$Fcont,{

    if(input$tabs1==1 && Fpanel() < 19){
      Fpanel(Fpanel()+1)
    }else if(input$tabs1==2 && Mpanel() < 7){
      Mpanel(Mpanel()+1)
    }else if(input$tabs1==3 && Dpanel() < 4){
      Dpanel(Dpanel()+1)
    }else if(input$tabs1==4 && Opanel() < 1){
      Opanel(Opanel()+1)
    }

    # Write old values
    UpJust()

    Des<<-package_MSClog()$Des
    #MSCsave_auto()

  })
  
  observeEvent(input$Fcont_red,{
    
    if(input$tabs1==1 && Fpanel() < 19){
      Fpanel(Fpanel()+1)
    }else if(input$tabs1==2 && Mpanel() < 7){
      Mpanel(Mpanel()+1)
    }else if(input$tabs1==3 && Dpanel() < 4){
      Dpanel(Dpanel()+1)
    }else if(input$tabs1==4 && Opanel() < 1){
      Opanel(Opanel()+1)
    }
    
    # Write old values
    UpJust()
    
    Des<<-package_MSClog()$Des
    #MSCsave_auto()
    
  })

  # ---- Fishery all switches -----------------

  observeEvent(input$All_M,
     #if(input$All_M == 0 | input$All_M%%2 == 0){
       updateCheckboxGroupInput(session,"M",choices=M_list,selected=M_list)
     #}else{
      # updateCheckboxGroupInput(session,"M",choices=M_list)
     #}
  )
  observeEvent(input$All_D,
     #if(input$All_D == 0 | input$All_D%%2 == 0){
        updateCheckboxGroupInput(session,"D",choices=D_list,selected=D_list)
     #}else{
      #  updateCheckboxGroupInput(session,"D",choices=D_list)
     #}
  )
  observeEvent(input$All_h,
     #if(input$All_h == 0 | input$All_h%%2 == 0){
        updateCheckboxGroupInput(session,"h",choices=h_list,selected=h_list)
     #}else{
       # updateCheckboxGroupInput(session,"h",choices=h_list)
     #}
  )
  observeEvent(input$All_FP,
     #if(input$All_FP == 0 | input$All_FP%%2 == 0){
       {updateCheckboxGroupInput(session,"FP",choices=FP_list,selected=FP_list)
       updateSliderInput(session,"loc",value=1)
       updateSliderInput(session,"stmag",value=1)}

     #}else{
      # updateCheckboxGroupInput(session,"FP",choices=FP_list)
      # updateSliderInput(session,"loc",value=1)
      # updateSliderInput(session,"stmag",value=1)

     #}
  )
  observeEvent(input$All_F,
     #if(input$All_F == 0 | input$All_F%%2 == 0){
        updateCheckboxGroupInput(session,"F",choices=F_list,selected=F_list)
     #}else{
      #  updateCheckboxGroupInput(session,"F",choices=F_list)
     #}
  )
  observeEvent(input$All_qh,
     #if(input$All_qh == 0 | input$All_qh%%2 == 0){
       updateCheckboxGroupInput(session,"qh",choices=q_list,selected=q_list)
     #}else{
    #   updateCheckboxGroupInput(session,"qh",choices=q_list)
     #}
  )
  observeEvent(input$All_q,
     #if(input$All_q == 0 | input$All_q%%2 == 0){
       updateCheckboxGroupInput(session,"q",choices=q_list,selected=q_list)
     #}else{
    #   updateCheckboxGroupInput(session,"q",choices=q_list)
     #}
  )
  observeEvent(input$All_LM,
     #if(input$All_LM == 0 | input$All_LM%%2 == 0){
       updateCheckboxGroupInput(session,"LM",choices=LM_list,selected=LM_list)
     #}else{
    #   updateCheckboxGroupInput(session,"LM",choices=LM_list)
     #}
  )
  observeEvent(input$All_sel,
     #if(input$All_sel == 0 | input$All_sel%%2 == 0){
        updateCheckboxGroupInput(session,"sel",choices=sel_list,selected=sel_list)
     #}else{
    #    updateCheckboxGroupInput(session,"sel",choices=sel_list)
     #}
  )
  observeEvent(input$All_dome,
     #if(input$All_dome == 0 | input$All_dome%%2 == 0){
        updateCheckboxGroupInput(session,"dome",choices=dome_list,selected=dome_list)
     #}else{
    #    updateCheckboxGroupInput(session,"dome",choices=dome_list)
    #}
  )
  observeEvent(input$All_DR,
     #if(input$All_DR == 0 | input$All_DR%%2 == 0){
        updateCheckboxGroupInput(session,"DR",choices=DR_list,selected=DR_list)
     #}else{
    #    updateCheckboxGroupInput(session,"DR",choices=DR_list)
     #}
  )
  observeEvent(input$All_PRM,
    #if(input$All_PRM == 0 | input$All_PRM%%2 == 0){
        updateCheckboxGroupInput(session,"PRM",choices=PRM_list,selected=PRM_list)
    #}else{
    #    updateCheckboxGroupInput(session,"PRM",choices=PRM_list)
    #}
  )
  observeEvent(input$All_sigR,
     #if(input$All_sigR == 0 | input$All_sigR%%2 == 0){
        updateCheckboxGroupInput(session,"sigR",choices=sigR_list,selected=sigR_list)
     #}else{
    #    updateCheckboxGroupInput(session,"sigR",choices=sigR_list)
    # }
  )

  observeEvent(input$All_Ah,
     #if(input$All_Ah  == 0 | input$All_Ah%%2 == 0){
       updateCheckboxGroupInput(session,"Ah",choices=Ah_list,selected=Ah_list[[1]]) # I know this duplication is...
     #}else{
    #   updateCheckboxGroupInput(session,"Ah",choices=Ah_list,selected=Ah_list[[1]]) # ... lazy but I'm keeping it here for future toggle adaptation.
     #}
  )
  observeEvent(input$All_Vh,
     #if(input$All_Vh  == 0 | input$All_Vh%%2 == 0){
       updateCheckboxGroupInput(session,"Vh",choices=Vh_list,selected=Vh_list[[length(Vh_list)]]) # I know this duplication is...
     #}else{
      # updateCheckboxGroupInput(session,"Vh",choices=Vh_list,selected=Vh_list[[length(Vh_list)]]) # ... lazy but I'm keeping it here for future toggle adaptation.
     #}
  )
  
  observeEvent(input$All_A,
    #if(input$All_A  == 0 | input$All_A%%2 == 0){
       updateCheckboxGroupInput(session,"A",choices=A_list,selected=input$Ah) # I know this duplication is...
    #}else{
    #  updateCheckboxGroupInput(session,"A",choices=A_list,selected=input$Ah)  # ... lazy but I'm keeping it here for future toggle adaptation. 
    #} 
  )
  observeEvent(input$All_V,
    #if(input$All_V  == 0 | input$All_V%%2 == 0){
       updateCheckboxGroupInput(session,"V",choices=V_list,selected=input$Vh) # I know this duplication is...
    #}else{
    #   updateCheckboxGroupInput(session,"V",choices=V_list,selected=input$Vh) # ... lazy but I'm keeping it here for future toggle adaptation. 
    #}
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
               #if(input$All_M1 == 0 | input$All_M1%%2 == 0){
                 updateCheckboxGroupInput(session,"M1",choices=M1_list,selected=M1_list)
              # }else{
               #  updateCheckboxGroupInput(session,"M1",choices=M1_list)
               #}
  )
  observeEvent(input$All_IB,
               #if(input$All_IB == 0 | input$All_IB%%2 == 0){
                 updateCheckboxGroupInput(session,"IB",choices=IB_list,selected=IB_list)
               #}else{
              #   updateCheckboxGroupInput(session,"IB",choices=IB_list)
              # }
  )
  observeEvent(input$All_IV,
               #if(input$All_IV == 0 | input$All_IV%%2 == 0){
                 updateCheckboxGroupInput(session,"IV",choices=IV_list,selected=IV_list)
               #}else{
                # updateCheckboxGroupInput(session,"IV",choices=IV_list)
               #}
  )

  observeEvent(input$All_IBE,
               #if(input$All_IBE == 0 | input$All_IBE%%2 == 0){
                 #vals<-as.list(IB_list[input$IB])
                 updateCheckboxGroupInput(session,"IBE",choices=IBE_list,selected=input$IB)
               #}else{
              #   updateCheckboxGroupInput(session,"IBE",choices=IBE_list,selected=input$IB)
               #}
  )
  observeEvent(input$All_IVE,
               #if(input$All_IVE == 0 | input$All_IVE%%2 == 0){
                 updateCheckboxGroupInput(session,"IVE",choices=IVE_list,selected=input$IV)
               #}else{
                # updateCheckboxGroupInput(session,"IVE",choices=IVE_list,selected=input$IV)
               #}
  )
  observeEvent(input$All_IBSL,
               #if(input$All_IBSL == 0 | input$All_IBSL%%2 == 0){
                 updateCheckboxGroupInput(session,"IBSL",choices=IBSL_list,selected=IBSL_list[length(IB_list)-match(input$IB,IB_list)+1])
               #}else{
               #  updateCheckboxGroupInput(session,"IBSL",choices=IBSL_list,selected=IBSL_list[length(IB_list)-match(input$IB,IB_list)+1])
               #}
  )
  observeEvent(input$All_IVSL,
               #if(input$All_IVSL == 0 | input$All_IVSL%%2 == 0){
                 updateCheckboxGroupInput(session,"IVSL",choices=IVSL_list,selected=input$IV)
               #}else{
                # updateCheckboxGroupInput(session,"IVSL",choices=IVSL_list,selected=input$IV)
               #}
  )



  # ---- Data all switches -------------

  observeEvent(input$All_D1,
               #if(input$All_D1 == 0 | input$All_D1%%2 == 0){
                 updateCheckboxGroupInput(session,"D1",choices=D1_list,selected=D1_list)
               #}else{
              #   updateCheckboxGroupInput(session,"D1",choices=D1_list)
               #}
  )
  observeEvent(input$All_CB,
               #if(input$All_CB == 0 | input$All_CB%%2 == 0){
                 updateCheckboxGroupInput(session,"CB",choices=CB_list,selected=CB_list)
               #}else{
              #   updateCheckboxGroupInput(session,"CB",choices=CB_list)
               #}
  )
  observeEvent(input$All_Beta,
               #if(input$All_Beta == 0 | input$All_Beta%%2 == 0){
                 updateCheckboxGroupInput(session,"Beta",choices=Beta_list,selected=Beta_list)
               #}else{
              #   updateCheckboxGroupInput(session,"Beta",choices=Beta_list)
               #}
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

  
  # Tool tips
  for(i in 1:ntips)addTooltip(session,ids[[i]],title=titles[[i]])
  
  
  # Effort sketching
  
  eff_values <- reactiveValues(df=data.frame(x=c(1951,1980,2018), y=c(0,0.5,0.5), series=rep(1,3)),
                               series=1,
                               stack=data.frame(x=c(1951,1980,2018), y=c(0,0.5,0.5), series=rep(1,3)))
  
  reset_eff_values<-function(){
    eff_values$df=data.frame(x=c(input$Syear,floor(mean(c(input$Syear,input$Lyear))),input$Lyear), y=c(0,0.5,0.5), series=rep(1,3))
    eff_values$series=1
    eff_values$stack=data.frame(x=c(input$Syear,floor(mean(c(input$Syear,input$Lyear))),input$Lyear), y=c(0,0.5,0.5), series=rep(1,3))
  }
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("Yr. = ", round(e$x, 1), " Eff. = ", round(e$y, 3), "\n")
    }
    temp <- input$plot_hover
    if(!is.null(temp$x) & !is.null(temp$y)) temp$x <- round(temp$x,0)
    if(!is.null(temp)){
      paste0(xy_str(temp))
    }else{
      " "
    }
  })
  
  output$effort_plot <- renderPlot({
    # first series
    plotFP()
    #print(eff_values$df) # for debugging & use elsewhere in the app
  })
  
  observeEvent(input$plot_click$x, {
    
    newX <- round(input$plot_click$x,0)
    newY <- input$plot_click$y
    
    # check if x value already exists
    ind <- which(eff_values$df$x == newX & eff_values$df$series == eff_values$series)
    if (length(ind)>0) eff_values$df <- eff_values$df[-ind,]
    
    eff_values$stack <- data.frame(x=c(eff_values$stack$x,newX),
                                   y=c(eff_values$stack$y, newY),
                                   series=c(eff_values$stack$series, eff_values$series))
    
    tempDF <- data.frame(x=c(eff_values$df$x, newX),
                         y=c(eff_values$df$y, newY),
                         series=c(eff_values$df$series, eff_values$series))
    
    tempDF <- dplyr::arrange(tempDF, series, x)
    
    eff_values$df <- tempDF
    
  })
  
  
  observeEvent(input$new_series, {
    # check that last series is complete
    lastX <- eff_values$df$x[nrow(eff_values$df)]
    lstYr<-input$Lyear
    initYr<-input$Syear
    #tempyrs<-getyrs()
    #nyears <- length(tempyrs)
    #initYr <- tempyrs[1] # initial year
    #lstYr <- initYr + nyears-1
    yvals <- 0 # initial effort
    AM(paste("lastX",lastX))
    AM(paste("lstYr",lstYr))
    if (lastX != lstYr) {
      #showNotification("Series must include last historical year", type="error")
      #shinyalert("Incomplete effort series", paste0("Series must include last historical year (",CurrentYr,")"), type = "info")
      vec<-c(input$Lyear,eff_values$df$y[nrow(eff_values$df)],eff_values$df$series[nrow(eff_values$df)])
      eff_values$df<-rbind(eff_values$df,vec)
      eff_values$stack<-rbind(eff_values$stack,vec)
    }
    
    eff_values$series <-eff_values$series+1
    eff_values$df <- data.frame(x=c(eff_values$df$x, initYr),
                                y=c(eff_values$df$y, yvals),
                                series=c(eff_values$df$series, eff_values$series))
    
  })
  
  observeEvent(input$undo_last, {
    # remove the last point
    if (nrow(eff_values$df)>1) {
      nrows <- nrow(eff_values$stack)
      last_vals <- eff_values$stack[nrows,]
      eff_values$stack <- eff_values$stack[1:(nrows-1),]
      eff_values$df <- dplyr::anti_join(eff_values$df, last_vals, by=c('x', 'y', 'series'))
    }
  })
  
  observeEvent(input$reset_plot, {
    reset_eff_values()
  })
  
  observeEvent(input$Syear,{
    nyears<<-input$Lyear-input$Syear+1
    Syear<<-input$Syear
    #reset_eff_values()
  })
  
  observeEvent(input$Lyear,{
    nyears<<-input$Lyear-input$Syear+1
    Lyear<<-input$Lyear
    #reset_eff_values()
  })
  
  #observeEvent(input$Start,{
   # Start(1)
  #})
  
  

})
