
# Redo Results functions
smartRedo<-function(){
  
  if(input$Mode=="Risk Assessment" & RA()==1){
    redoRA()
  }else if(input$Mode=="Status Determination" & SD()==1){
    redoSD()
  }else if(input$Mode=="Management Planning" & Plan()==1){
    redoPlan()
  }else if(input$Mode=="Management Performance" & Eval()==1){
    redoEval()
  }  #else{
  #  RA(0)
  #  SD(0)
  #  Plan(0)
  #  Eval(0)
  #}
  
}

redoRA<-function(fease=F){
  withProgress(message = "Calculating Risk Assessment results", value = 0, {
    nres<-length(Skin$Risk_Assessment$Tab_title)
    dims<-list()
    incrate<-1/nres
    if(input$Debug)message(paste0("Max number of RA plots = ",nres))
    
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
          output[[paste0("P_Fig_",res2)]]<-renderPlot(NULL) 
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
    
    if(nres<9){
      
      for(res in (nres+1):9){
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
    
    # option code
    #options <- list(burnin = input$burnin, res=input$res)
    #options <- list( res=1)
    
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
          height=Skin$SD$Fig_dim[[res2]]()$height
          width=Skin$SD$Fig_dim[[res2]]()$width
          output[[paste0("P_Fig_",res2)]]<-renderPlot(Skin$SD$Figs[[res2]](Status), height =ceiling(height) , width = ceiling(width)) 
        }
        
      })
      
      incProgress(incrate)
      
    }
    
    # Blank additional non-specified Figs / Tabs
    
    if(nres<9){
      
      for(res in (nres+1):9){
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

  withProgress(message = "Calculating Evaluation results", value = 0, {

    nres<-length(Skin$Planning$Tab_title)
    dims<-list(nMPs=MSEobj@nMPs)
    incrate<-1/nres
    
    # option code
    options <- list(burnin = input$burnin, res=input$res)
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
          output[[paste0("P_Fig_",res2)]]<-renderPlot(NULL) 
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
    
    if(nres<9){
      
      for(res in (nres+1):9){
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
  
  withProgress(message = "Calculating Evaluation results", value = 0, {
    
    nres<-length(Skin$Evaluation$Tab_title)
    dims<-list(nMPs=MSEobj@nMPs)
    incrate<-1/nres
    
    # option code
    #options <- list(burnin = input$burnin, res=input$res)
    options <- list(YIU = input$YIU, res=1)
    
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
          output[[paste0("P_Tab_",res2)]]<-DT::renderDataTable(Skin$Evaluation$Tabs[[res2]](MSEobj,MSEobj_reb,options)) 
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
          output[[paste0("P_Fig_",res2)]]<-renderPlot(Skin$Evaluation$Figs[[res2]](MSEobj,MSEobj_reb,options), height =ceiling(height) , width = ceiling(width)) 
        }
        
      })
      
      incProgress(incrate)
      
    }
    
    
    if(nres<9){
      
      for(res in (nres+1):9){
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



redoInd<-function(){

  styr=max(dat@Year)-min(dat@Year)+1
  PPD<-MSEobj@Misc$Data[[1]]

  # Standardization
  PPD@Cat<-PPD@Cat/PPD@Cat[,styr]
  PPD@Ind<-PPD@Ind/PPD@Ind[,styr]
  PPD@ML<-PPD@ML/PPD@ML[,styr]

  tsd= c("Cat","Cat","Cat","Ind","Ind","ML")
  stat=c("slp","AAV","mu","slp","mu", "slp")
  res<-max(dat_ind@Year-max(dat@Year))
  datayears<-dim(dat_ind@Cat)[2]

  indPPD<-getinds(PPD,styr=styr,res=res,tsd=tsd,stat=stat)

  # Standardization
  dat_ind@Cat<-dat_ind@Cat/dat_ind@Cat[,styr]
  dat_ind@Ind<-dat_ind@Ind/dat_ind@Ind[,styr]
  dat_ind@ML<-dat_ind@ML/dat_ind@ML[,styr]

  indData<-getinds(dat_ind,styr=styr,res=res,tsd=tsd,stat=stat)

  output$CC<-renderPlot( CC(indPPD,indData,pp=1,res=res),height =700 ,width=700)
  output$mdist<-renderPlot(plot_mdist(indPPD,indData,alpha=0.05),height =550 ,width=550)

}
