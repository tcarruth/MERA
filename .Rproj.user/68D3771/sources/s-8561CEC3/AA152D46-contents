
checkQs<-function(){
  
  Qs<-unlist(lapply(PanelState[1:3],function(x)lapply(x,sum)))
  
  Qnams<-c(paste0("F",2:19),paste0("M",1:7),paste0("D",1:4))
  
  list(error=any(Qs==0),probQs=Qnams[Qs==0])
  
}

checkJs<-function(){
  
  Js<-unlist(Just)
  probJs<-grepl("No justification",Js)|Js==""
  
  probJs[1]<-(Js[1]=="1. Describe the history and current status of the fishery, including fleets, sectors, vessel types and practices/gear by vessel type, landing ports, economics/markets, whether targeted/bycatch, other stocks caught in the fishery.\n\n2. Describe the stock’s ecosystem functions, dependencies, and habitat types.\n\n3. Provide all relevant reference materials, such as assessments, research, and other analysis.\n\n      ")
  probJs[20]<-(Js[20]=="1. Describe what, if any, current management measures are used to constrain catch/effort.\n\n2. Describe historical management measures, if any.\n\n3. Describe main strengths and weaknesses of current monitoring and enforcement capacity.\n\n4. Describe and reference any legal/policy requirements for management, monitoring and enforcement.\n\n       ") 
  probJs[27]<-(Js[27]=="1. Provide the time series (specify years, if possible) that exist for catch, effort, and CPUE/abundance indices.\n\n2. Describe how these data collected (e.g., log books, dealer reporting, observers).\n\n3. Describe what types of sampling programs and methodologies exist for data collection, including the time-series of available sampling data and quality.\n\n4. Describe all sources of uncertainty in the status, biology, life history and data sources of the fishery.\tInclude links to documentation, reports.\n\n      ")
  Qnams<-c(paste0("F",1:19),paste0("M",1:7),paste0("D",1:4))

  list(error=any(probJs),probJs=Qnams[probJs],nprob=sum(probJs))
  
}
  
plotJs<-function(){
  
  Jstats<-checkJs()
  dat<-matrix(c(Jstats$nprob,30-Jstats$nprob),ncol=1) #dat<-matrix(c(22,8),ncol=1)
  rownames(dat)<-c("Justified","Not justified")
  colfunc<-colorRampPalette(c("springgreen","yellow","red"))
  coly<-colfunc(1000)[ceiling(dat[1,1]/30*999)+0.01]
  par(mai=c(0.01,0.01,0.3,0.01))
  singular=as.integer(Jstats$nprob!=29)+1
  barplot(dat,horiz=T,col=c('red','green'),border=F,axes=F,main=paste0(30-Jstats$nprob, " question",c(" was","s were")[singular], " provided with justification",c("","s")[singular],collapse=""),col.main=coly)  
  
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
  #saveRDS(PanelState,file="C:/temp/PanelState_autosave.rds")
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
  #saveRDS(Just,file="C:/temp/Just.rds")

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
