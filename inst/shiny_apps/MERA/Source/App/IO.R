# MERA I/O

# Get fishery description info
getDes<-function(){
  list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, Syear= input$Syear, Lyear=input$Lyear, Author=input$Author)
}

package_MSClog<-function(){
  Des<-getDes()
  if(Data()==0)dat<-NULL
  if(DataInd()==0)dat_ind<-NULL
   
  list(PanelState=PanelState, Just=Just, Des=Des, dat=dat, dat_ind=dat_ind, eff_values=reactiveValuesToList(eff_values),settings=save_settings())
}

# for saving questionnaires
package_Questionnaire<-function(){
  
  MSClog<-package_MSClog()
}

# for loading questionnaires
Update_Questionnaire<-function(MSClog){
  
  cond<-all(names(MSClog[[1]])[1:4]==c("Fpanel","Mpanel","Dpanel","Slider"))
  AM(paste("Questionnaire loaded:",cond))
  
  if(cond){
    PanelState<<-MSClog[[1]]
    Just<<-MSClog[[2]]
    
    # All panels except radio button on D4
    for(i in 1:2){
      for(j in 1:length(PanelState[[i]])) {
        
        if(!(i==1 & j==4)){ # not the defunct effort trend type
          state<-as.vector(unlist(PanelState[[i]][j]))
          choices<-as.vector(unlist(get(MasterList[[i]][j])))
          selected<-as.list(choices[state])
          choices<-as.list(choices)
          updateCheckboxGroupInput(session, as.character(inputnames[[i]][j]), selected = selected)
        }
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
    
    #for(j in 1:length(PanelState[[4]])){
    #  updateSliderInput(session,as.character(inputnames[[4]][j]),value=as.numeric(PanelState[[4]][j]))
    #}
    
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
    #saveRDS(MSElog,"C:/temp/MSClog.rda")
    
    if("nyears" %in% names(MSClog[[3]])){
      updateNumericInput(session, "Lyear",   value= 2018)
      updateNumericInput(session, "Syear",   value= 2018-MSClog[[3]]$nyears+1)
      Lyear<<-2018
      Syear<<-2018-MSClog[[3]]$nyears+1
    }else{
      updateNumericInput(session, "Lyear",   value= MSClog[[3]]$Lyear)
      updateNumericInput(session, "Syear",   value= MSClog[[3]]$Syear)
      Lyear<<-MSClog[[3]]$Lyear
      Syear<<-MSClog[[3]]$Syear
    }
    
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
    RA(0) # Have run risk assessment (multi MP)
    SD(0) # Has a status determination been run yet?
    Plan(0) # Have run Planning (multi MP)
    Eval(0)
    MadeOM(0)
    
    if(!is.null(MSClog$dat)){
      #dattest<<-MSClog$dat
      #daterrs<-Data_gate(dattest)
      #if(length(daterrs)!=0){
       # shinyalert(title="Data not imported due to formatting inconsistencies", text=unlist(daterrs), type="error")
      #}else{
        AM("Appended data uploaded to MERA")
        dat<<-MSClog$dat
        Data(1)
        DataInd(0)
        FeaseMPs<<-Fease(dat)
        AM("Data loaded with questionnaire")
        updateNumericInput(session,"C_eq_val",value=dat@Cat[1,1])
        AM(paste0("Suggested equilibrium catch is catch in first year:",dat@Cat[1,1]))
        if(!is.null(MSClog$dat_ind)){
          dat_ind<<-MSClog$dat_ind
          DataInd(1)
          AM("Additional indicator data loaded since MP was adopted")
        }
        SD_codes<-getCodes(dat,maxtest=Inf)
        AM(paste0("Data object is compatible with the following status determination methods: ", paste(SD_codes,collapse=",")))
        updateSelectInput(session,'SDsel',choices=SD_codes,selected=SD_codes[1])
        updateSelectInput(session,'Cond_ops',choices=SD_codes,selected=SD_codes[1])
      #} # if not a data formatting error
    }
   
    
    if("eff_values"%in%names(MSClog)){
      eff_values$df=MSClog$eff_values$df
      eff_values$series=MSClog$eff_values$series
      eff_values$stack=MSClog$eff_values$stack
      AM("Sketched effort loaded")
      #AM(paste(eff_values$df$x,collapse=" - "))
    }else{
      
      eff_backwards(MSClog)
      
    }
    if(!is.null(MSClog$settings))load_settings(MSClog$settings)
    
  }else{
    AM("Questionnaire failed to load")
    shinyalert("File read error", "This does not appear to be a MERA questionnaire file", type = "error")
  }
}

save_settings<-function(){
  
  settings<-list()
  settings[['plusgroup']]<-input$plusgroup
  settings[['seed']]<-input$seed
  settings[['use_seed']]<-input$use_seed
  settings[['Distribution']]<-input$Distribution
  settings[['IQRange']]<-input$IQRange
  settings[['interval']]<-input$interval
  settings[['Parallel']]<-input$Parallel
  settings[['nsim']]<-input$nsim
  settings[['Dep_reb']]<-input$Dep_reb
  settings[['Cond_ops']]<-input$Cond_ops
  settings[['OM_C']]<-input$OM_C
  settings[['C_eq_val']]<-input$C_eq_val
  settings[['C_eq']]<-input$C_eq
  settings[['ESS']]<-input$ESS
  settings[['Wt_comp']]<-input$Wt_comp
  settings[['max_F']]<-input$max_F
  settings[['Mode']]<-input$Mode
  settings[['Skin']]<-input$Skin
  
  settings
}
  
load_settings<-function(settings){
  
  if(!is.null(settings$plusgroup))updateNumericInput(session,'plusgroup',value=settings$plusgroup)
  if(!is.null(settings$seed))updateNumericInput(session,'seed',value=settings$seed)
  if(!is.null(settings$use_seed))updateCheckboxInput(session,'use_seed',value=settings$use_seed)
  if(!is.null(settings$Distribution))updateRadioButtons(session,'Distribution',selected=settings$Distribution)
  if(!is.null(settings$IQRange))updateNumericInput(session,'IQRange',value=settings$IQRange)
  if(!is.null(settings$interval))updateNumericInput(session,'interval',value=settings$interval)
  if(!is.null(settings$Parallel))updateCheckboxInput(session,'Parallel',value=settings$Parallel)
  if(!is.null(settings$nsim))updateNumericInput(session,'nsim',value=settings$nsim)
  if(!is.null(settings$Dep_reb))updateSliderInput(session,'Dep_reb',value=settings$Dep_reb)
  if(!is.null(settings$Cond_ops))updateSelectInput(session,'Cond_ops',selected=settings$Cond_ops)
  if(!is.null(settings$OM_C))updateCheckboxInput(session,'OM_C',value=settings$OM_C)
  if(!is.null(settings$C_eq_val))updateNumericInput(session,'C_eq_val',value=settings$C_eq_val)
  if(!is.null(settings$C_eq))updateCheckboxInput(session,'C_eq',value=settings$C_eq)
  if(!is.null(settings$ESS))updateNumericInput(session,'ESS',value=settings$ESS)
  if(!is.null(settings$Wt_comp))updateNumericInput(session,'Wt_comp',value=settings$Wt_comp)
  if(!is.null(settings$max_F))updateNumericInput(session,'max_F',value=settings$max_F)
  if(!is.null(settings$Mode))updateRadioButtons(session,'Mode',selected=settings$Mode)
  if(!is.null(settings$Skin))updateSelectInput(session,'Skin',selected=settings$Skin)
  
}  
  

