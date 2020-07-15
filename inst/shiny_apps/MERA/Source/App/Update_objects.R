
checkQs<-function(){
  
  Qs<-unlist(lapply(PanelState[1:3],function(x)lapply(x,sum)))
  Qs[names(Qs)=="Fpanel4"]<-6
  Qnams<-c(paste0("F",2:19),paste0("M",1:7),paste0("D",1:4))
  
  #input$
  #eff_values$df$x
 # eff_values<-readRDS("C:/Users/tcar_/Dropbox/MERA/MERA_TESTS/TokyoBaySeaBass_TC/Japanese_Seabas.mera")$eff_values 
  if(any(!(eff_values$df$x %in% input$Syear:input$Lyear))){
    Qs[names(Qs)=="Fpanel4"]<-0 # error in effort prescription
    AM("Error. Inconsistency in Questionnaire specification: The years specified in Fishery Question 1 do not match the effort drawn in Fishery Question 5")
  }  
  list(error=any(Qs==0),probQs=Qnams[Qs==0])
  
}



UpPanelState<-function(){
  for(i in 1:2){
    for(j in 1:length(PanelState[[i]])) {
      value<-sapply(inputnames[[i]][j],function(x) input[[x]])
      PanelState[[i]][[j]] <<- get(MasterList[[i]][j])%in%value
    }
  }
  i<-3
  for(j in 2:length(PanelState[[i]])) {
    value<-sapply(inputnames[[i]][j],function(x) input[[x]])
    PanelState[[i]][[j]] <<- get(MasterList[[i]][j])%in%value
  }
  
  for(j in 1:length(Slider_names)) {
    PanelState[[4]][[j]] <<-sapply(inputnames[[4]][j],function(x) input[[x]])
  }
  
}

UpJust<-function(){

  if(input$tabs1==1){
    updateTextAreaInput(session,"Justification",value=Just[[1]][Fpanel()])
  }else if(input$tabs1==2){
    updateTextAreaInput(session,"Justification",value=Just[[2]][Mpanel()])
  }else if(input$tabs1==3){
    updateTextAreaInput(session,"Justification",value=Just[[3]][Dpanel()])
  }

}

RecJust<-function(){

  if(input$tabs1==1 & Fpanel()>0 & Fpanel()<20){
    Just[[1]][Fpanel()]<<-input$Justification
  }else if(input$tabs1==2 & Mpanel()>0 & Mpanel()<8){
    Just[[2]][Mpanel()]<<-input$Justification
  }else if(input$tabs1==3 & Dpanel()>0 & Dpanel()<5){
    Just[[3]][Dpanel()]<<-input$Justification
  }
  

}

Update_Options<-function(){
  
  shinyjs::hide("burnin")
  shinyjs::hide("YIU")
  shinyjs::hide("res")
  
  if(input$Mode=='Planning'){
    nopt<-length(Skin$Planning$options)
    if(nopt>0){
      for(i in 1:nopt){
        optnam<-names(Skin$Planning$options[i])
        shinyjs::show(optnam)
        updateNumericInput(session, optnam,value=as.numeric(Skin$Planning$options[i]))  
      }
    }
  }
  
  if(input$Mode=='Evaluation'){
    nopt<-length(Skin$Evaluation$options)
    if(nopt>0){
      for(i in 1:nopt){
        optnam<-names(Skin$Evaluation$options[i])
        shinyjs::show(optnam)
        updateNumericInput(session, optnam,value=as.numeric(Skin$Evaluation$options[i]))
      }
    }
  }
  
  

}
