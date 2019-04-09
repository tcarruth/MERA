

UpPanelState<-function(){
  for(i in 1:3){
    for(j in 1:length(PanelState[[i]])) {
      value<-sapply(inputnames[[i]][j],function(x) input[[x]])
      PanelState[[i]][[j]] <<- get(MasterList[[i]][j])%in%value
    }
  }
  for(j in 1:length(Slider_names)) {
    PanelState[[4]][[j]] <<-sapply(inputnames[[4]][j],function(x) input[[x]])
  }
  saveRDS(PanelState,file="PanelState_autosave.rds")
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
