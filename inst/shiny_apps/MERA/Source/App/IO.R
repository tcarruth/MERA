# MERA I/O

# for saving questionnaires
package_Questionnaire<-function(){
  Des<<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
  if(Data()==1){
    PanelState$dat<-dat
    AM("Saving appended data")
  }
  if(DataInd()==1){
    PanelState$dat_ind<-dat_ind
  }
  
  MSClog<-list(PanelState=PanelState, Just=Just, Des=Des,eff_values=eff_values,Version=Version)
}

# for loading questionnaires
Update_Questionnaire<-function(MSClog){
  cond<-sum(names(MSClog[[1]])[1:4]==c("Fpanel","Mpanel","Dpanel","Slider"))==4
  AM("Questionnaire loaded")
  
  if(cond){
    PanelState<<-MSClog[[1]]
    Just<<-MSClog[[2]]
    
    # All panels except radio button on D4
    for(i in 1:2){
      for(j in 1:length(PanelState[[i]])) {
        
        if(i!=1 & j!=4){ # not the defunct effort trend type
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
    RA(0) # Have run risk assessment (multi MP)
    SD(0) # Has a status determination been run yet?
    Plan(0) # Have run Planning (multi MP)
    Eval(0)
    
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
    
    if("eff_values"%in%names(MSClog)){
      eff_values$stack<-MSClog$eff_values$stack
      eff_values$df<-MSClog$eff_values$df
      eff_values$series<-MSClog$eff_values$series
    }else{
      
      eff_backwards(MSClog)
      
    }
    
    
  }else{
    AM("Questionnaire failed to load")
    shinyalert("File read error", "This does not appear to be a MERA questionnaire file", type = "error")
  }
}



