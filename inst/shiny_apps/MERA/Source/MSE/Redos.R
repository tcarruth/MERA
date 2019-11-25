
# Redo Results functions
smartRedo<-function(){
  
  if(input$Mode=="Risk Assessment" & RA()==1){
    redoBlank()
    redoRA()
  }else if(input$Mode=="Status Determination" & SD()==1){
    redoBlank()
    redoSD()
  }else if(input$Mode=="Management Planning" & Plan()==1){
    redoBlank()
    redoPlan()
  }else if(input$Mode=="Management Performance" & Eval()==1){
    redoBlank()
    redoEval()
  }  #else{
  #  RA(0)
  #  SD(0)
  #  Plan(0)
  #  Eval(0)
  #}
  
}


redoRA<-function(){
  withProgress(message = "Calculating Risk Assessment results", value = 0, {
    nres<-length(Skin$Risk_Assessment$Tab_title)
    incrate<-1/nres
    message(paste0("Max number of RA plots = ",nres))
    
    options <- list(res=5)
    
    if(Skin$Risk_Assessment$Intro_title[[1]]==""){
      output[["P_Intro_title"]]<-renderText(NULL)
      output[["P_Intro_text"]]<-renderText(NULL)
    }else{
      output[["P_Intro_title"]]<-renderText(Skin$Risk_Assessment$Intro_title[[1]])
      output[["P_Intro_text"]]<-renderUI(Skin$Risk_Assessment$Intro_text[[1]])
    }

    for(res in 1:nres){
      
      local({
        
        res2<-res
          
        if(Skin$Risk_Assessment$Tab_title[[res2]]==""){
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(NULL) 
        }else{
          output[[paste0("P_Tab_",res2,"_title")]]<-renderUI(Skin$Risk_Assessment$Tab_title[[res2]])
          output[[paste0("P_Tab_",res2,"_text")]]<-renderUI(Skin$Risk_Assessment$Tab_text[[res2]])
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(Skin$Risk_Assessment$Tabs[[res2]](RAobj,options)) 
        }
        
        if(Skin$Risk_Assessment$Fig_title[[res2]]==""){
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2)]]<-renderPlot(NULL,height=10,width=10) 
        }else{ 
          output[[paste0("P_Fig_",res2,"_title")]]<-renderUI(Skin$Risk_Assessment$Fig_title[[res2]])
          output[[paste0("P_Fig_",res2,"_text")]]<-renderUI(Skin$Risk_Assessment$Fig_text[[res2]])
          height=Skin$Risk_Assessment$Fig_dim[[res2]](dims)$height
          width =Skin$Risk_Assessment$Fig_dim[[res2]](dims)$width
          output[[paste0("P_Fig_",res2)]]<-renderPlot(Skin$Risk_Assessment$Figs[[res2]](RAobj,RAobj,options),  height =ceiling(height) , width = ceiling(width)) 
        }
        
      })
      
      incProgress(incrate)
      
    }
    
    if(nres<10){
      
      for(res in (nres+1):10){
        local({
          res2<-res
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(NULL)
          
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2)]]<-renderPlot(NULL)
          
        })
      }
      
    }
    
     incProgress(incrate)
    
  })
}


redoSD<-function(){
  
  withProgress(message = "Calculating Status Determination results", value = 0, {
    
    nres<-length(Skin$SD$Tab_title)
    incrate<-1/nres
    dims<-list(nmeth=sum(unlist(lapply(Status$Est,length))>3),SimSam=(!is.null(Status$SimSams)))
    
    # option code
    # options <- list( burnin = input$burnin, res=input$res )
    # options <- list( res = 1 )
    
    if(Skin$SD$Intro_title[[1]]==""){
      output[["P_Intro_title"]]<-renderText(NULL)
      output[["P_Intro_text"]]<-renderText(NULL)
    }else{
      output[["P_Intro_title"]]<-renderText(Skin$SD$Intro_title[[1]])
      output[["P_Intro_text"]]<-renderText(Skin$SD$Intro_text[[1]])
    } 
    
    for(res in 1:nres){
      
      local({
        
        res2<-res
        
        if(Skin$SD$Tab_title[[res2]]==""){
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(NULL) 
        }else{
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(Skin$SD$Tab_title[[res2]])
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(Skin$SD$Tab_text[[res2]])
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(Skin$SD$Tabs[[res2]](Status)) 
        }
        
        if(Skin$SD$Fig_title[[res2]]==""){
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2)]]<-renderPlot(NULL) 
        }else{ 
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(Skin$SD$Fig_title[[res2]])
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(Skin$SD$Fig_text[[res2]])
          height=Skin$SD$Fig_dim[[res2]](dims)$height
          width=Skin$SD$Fig_dim[[res2]](dims)$width
          output[[paste0("P_Fig_",res2)]]<-renderPlot(Skin$SD$Figs[[res2]](Status), height =ceiling(height) , width = ceiling(width)) 
        }
        
      })
      
      incProgress(incrate)
      
    }
    
    # Blank additional non-specified Figs / Tabs
    
    if(nres<10){
      
      for(res in (nres+1):10){
        local({
          
          res2<-res
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(NULL)
          
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2)]]<-renderPlot(NULL)
          
        })
      }
      
    }
    
    incProgress(incrate)
    
  })
  

}


redoPlan<-function(fease=F){

  withProgress(message = "Calculating Management Planning results", value = 0, {

    nres<-length(Skin$Planning$Tab_title)
    dims<-list(nMPs=MSEobj@nMPs)
    incrate<-1/nres
    
    # option code
    options <- list(burnin = input$burnin, res=input$res, 
                    tab1.row.select=input$P_Tab_1_rows_selected,
                    tab4.row.select=input$P_Tab_4_rows_selected)
    #options <- list( res=1)
    
    if(Skin$Planning$Intro_title[[1]]==""){
      output[["P_Intro_title"]]<-renderText(NULL)
      output[["P_Intro_text"]]<-renderText(NULL)
    }else{
      output[["P_Intro_title"]]<-renderText(Skin$Planning$Intro_title[[1]])
      output[["P_Intro_text"]]<-renderText(Skin$Planning$Intro_text[[1]])
    } 
    
    for(res in 1:nres){
      
      local({
        
        res2<-res
        
        if(Skin$Planning$Tab_title[[res2]]==""){
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(NULL) 
        }else{
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(Skin$Planning$Tab_title[[res2]])
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(Skin$Planning$Tab_text[[res2]])
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(Skin$Planning$Tabs[[res2]](MSEobj,MSEobj_reb,options))
          
        }
        
        
        
        if(Skin$Planning$Fig_title[[res2]]==""){
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2)]]<- NULL # renderPlot(NULL,height=10,width=10) 
         }else{ 
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(Skin$Planning$Fig_title[[res2]])
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(Skin$Planning$Fig_text[[res2]])
          height=Skin$Planning$Fig_dim[[res2]](dims)$height
          width=Skin$Planning$Fig_dim[[res2]](dims)$width
          output[[paste0("P_Fig_",res2)]]<-renderPlot(Skin$Planning$Figs[[res2]](MSEobj,MSEobj_reb,options), height =ceiling(height) , width = ceiling(width)) 
        }
        
      })
      
      incProgress(incrate)
      
    }
    
    # Blank additional non-specified Figs / Tabs
    
    if(nres<10){
      
      for(res in (nres+1):10){
        local({
          res2<-res
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(NULL)
          
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2)]]<-renderPlot(NULL)
          
        })
      }
    
    }
  
    incProgress(incrate)

  })
}


redoEval<-function(fease=F){
  
  withProgress(message = "Calculating Management Performance results", value = 0, {
    
    nres<-length(Skin$Evaluation$Tab_title)
    dims<-list(nMPs=MSEobj_Eval@nMPs)
    incrate<-1/nres
    
    # option code
    #options <- list(burnin = input$burnin, res=input$res)
    options <- list(res=1)
    
    if(Skin$Evaluation$Intro_title[[1]]==""){
      output[["P_Intro_title"]]<-renderText(NULL)
      output[["P_Intro_text"]]<-renderText(NULL)
    }else{
      output[["P_Intro_title"]]<-renderText(Skin$Evaluation$Intro_title[[1]])
      output[["P_Intro_text"]]<-renderText(Skin$Evaluation$Intro_text[[1]])
    } 
    
    for(res in 1:nres){
      
      local({
        
        res2<-res
        
        if(Skin$Evaluation$Tab_title[[res2]]==""){
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(NULL) 
        }else{
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(Skin$Evaluation$Tab_title[[res2]])
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(Skin$Evaluation$Tab_text[[res2]])
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(Skin$Evaluation$Tabs[[res2]](MSEobj_Eval,dat,dat_ind,options)) 
        }
        
        if(Skin$Evaluation$Fig_title[[res2]]==""){
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2)]]<-renderPlot(NULL) 
        }else{ 
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(Skin$Evaluation$Fig_title[[res2]])
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(Skin$Evaluation$Fig_text[[res2]])
          height=Skin$Evaluation$Fig_dim[[res2]](dims)$height
          width=Skin$Evaluation$Fig_dim[[res2]](dims)$width
          output[[paste0("P_Fig_",res2)]]<-renderPlot(Skin$Evaluation$Figs[[res2]](MSEobj_Eval,dat,dat_ind,options), height =ceiling(height) , width = ceiling(width)) 
        }
        
      })
      
      incProgress(incrate)
      
    }
    
    
    if(nres<10){
      
      for(res in (nres+1):10){
        local({
          res2<-res
          output[[paste0("P_Tab_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(NULL)
          
          output[[paste0("P_Fig_",res2,"_title")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2,"_text")]]<-renderText(NULL)
          output[[paste0("P_Fig_",res2)]]<-renderPlot(NULL)
          
        })
      }
      
    }
 
    incProgress(incrate)
     
  })
}

blankplot<-function()plot(c(0,1),col='white',xlab="",ylab="",axes=F)

redoBlank<-function(){
 
  for(res in 1:10){
    
    local({
      
      res2<-res
      
      output[[paste0("P_Tab_",res2,"_title")]]<-renderText(NULL)
      output[[paste0("P_Tab_",res2,"_text")]]<-renderText(NULL)
      output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(NULL) 
      
      output[[paste0("P_Fig_",res2,"_title")]]<-renderText(NULL)
      output[[paste0("P_Fig_",res2,"_text")]]<-renderText(NULL)
      output[[paste0("P_Fig_",res2)]]<-renderPlot(blankplot,height =150 , width = 400 ) 
     
    })
    
    # incProgress(incrate)
    
  }
 
}

