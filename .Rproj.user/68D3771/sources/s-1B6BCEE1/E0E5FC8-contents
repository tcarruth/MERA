# Evaluation MSE report
output$Build_Eval <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = function(){paste0(namconv(input$Name),"_Eval.html")}, #"report.html",
  
  content = function(file) {
    withProgress(message = "Building evaluation report", value = 0, {
      src <- normalizePath('EvalRep.Rmd')
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'EvalRep.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      thresh<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
      
      Ptab1<<-Ptab(MSEobj,MSEobj_reb,burnin=burnin,rnd=0)
      #thresh<<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
      fease<-input$Fease
      temp<-Ptab_ord(Ptab1,burnin=burnin,ntop=input$ntop,thresh=thresh,fease=fease)
      Ptab2<<-temp[[1]]
      MPcols<<-temp[[2]]
      
      params <- list(test = input$Name,
                     set_title=paste0("Evaluation Report for ",input$Name),
                     set_type=paste0("Evaluation of MPs analysis "," (MERA version ",Version,")"),
                     
                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     MSEobj=MSEobj,
                     MSEobj_reb=MSEobj_reb,
                     MSEobj_top=MSEobj_top,
                     MSEobj_reb_top=MSEobj_reb_top,
                     Ptab2=Ptab2,
                     MPcols=MPcols,
                     ntop=input$ntop,
                     burnin=burnin,
                     thresh=thresh,
                     SessionID=SessionID,
                     copyright=paste("copyright (c) NRDC",CurrentYr)
      )
      
      out<-render("EvalRep.Rmd", params = params)
      file.rename(out, file)
    })
  }
  
)

output$Build_Eval_MSC <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = function(){paste0(namconv(input$Name),"_Eval.html")}, #"report.html",
  
  content = function(file) {
    withProgress(message = "Building evaluation report", value = 0, {
      src <- normalizePath('EvalRep_MSC.Rmd')
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'EvalRep_MSC.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      thresh<-c(input$P_STL,input$P_STT,input$P_LTL,input$P_LTT)
      
      Ptab1<<-Ptab_MSC(MSEobj,burnin=burnin,rnd=0)
      #thresh<<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
      temp<-Ptab_ord_MSC(Ptab1,burnin=burnin,ntop=input$ntop,thresh=thresh)
      Ptab2<<-temp[[1]]
      MPcols<<-temp[[2]]
      
      MSEobj_top<<-Sub(MSEobj,MPs=Ptab2$MP)
      MSEobj_reb_top<<-Sub(MSEobj_reb,MPs=Ptab2$MP)
      #save(MSEobj_top,file="MSEobj_top")
      #save(MSEobj_reb_top,file="MSEobj_reb_top")
      nMPs<<-length(MSEobj_top@MPs)
      GreenMPs<<-Ptab2$MP[MPcols=="green"]
      #updateTextAreaInput(session,"Debug1",value=Ptab2$MP)
      
      Ptab3<<-Ptab_TO(MSEobj,MSEobj_reb,burnin=burnin,rnd=0)
      #incProgress(incrate)
      Ptab4<<-Ptab_ord_TO(Ptab3,burnin=burnin)
      #incProgress(incrate)
      if(length(GreenMPs)>0){
        MSEobj_top_TO<<-Sub(MSEobj,MPs=GreenMPs) # for trade-off plots
        MSEobj_reb_top_TO<<-Sub(MSEobj_reb,MPs=GreenMPs) # for trade-off plots
        Ptab4<<-Ptab4[Ptab4$MP%in%GreenMPs,]
      }else{
        Ptab4<-data.frame(MP="-",Type="-",P_RB="-",LTY="-")
      }
      nMPs_TO<<-nrow(Ptab4)
      
      params <- list(test = input$Name,
                     set_title=paste0("Evaluation Report for ",input$Name),
                     set_type=paste0("Evaluation of MPs analysis "," (MERA version ",Version,")"),
                     
                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     MSEobj=MSEobj,
                     MSEobj_reb=MSEobj_reb,
                     MSEobj_top=MSEobj_top,
                     MSEobj_reb_top=MSEobj_reb_top,
                     MSEobj_top_TO=MSEobj_top_TO,
                     MSEobj_reb_top_TO=MSEobj_reb_top_TO,
                     nMPs_TO=nMPs_TO,
                     Ptab2=Ptab2,
                     Ptab4=Ptab4,
                     MPcols=MPcols,
                     nMPs=nMPs,
                     ntop=input$ntop,
                     burnin=burnin,
                     thresh=thresh,
                     SessionID=SessionID,
                     copyright=paste("copyright (c) NRDC",CurrentYr)
      )
      
      out<-render("EvalRep_MSC.Rmd", params = params)
      file.rename(out, file)
    })
  }
  
)


# Evaluation MSE report
output$Build_App <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = function(){paste0(namconv(input$Name),"_Eval.html")}, #"report.html",
  
  content = function(file) {
    withProgress(message = "Building evaluation report", value = 0, {
      
      src <- normalizePath('AppRep.Rmd')
      #doprogress("Building evaluation report",1)
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'AppRep.Rmd', overwrite = TRUE)
      
      burnin<<-input$burnin
      Ptab1_app<<-Ptab(MSEobj_app,MSEobj_reb_app,burnin=burnin,rnd=0)
      thresh<<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
      temp<-Ptab_ord(Ptab1_app,burnin=burnin,ntop=input$ntop, Eval=F,thresh=thresh)
      Ptab2_app<<-temp[[1]]
      MPcols_app<<-temp[[2]]
      
      
      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Evaluation Report for ",input$Name),
                     set_type=paste0("Evaluation of an MP"," (MERA version ",Version,")"),
                     
                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     MSEobj_app=MSEobj,
                     MSEobj_reb_app=MSEobj_reb,
                     MPcols=MPcols_app,
                     Ptab2_app=Ptab2_app,
                     thresh=thresh,
                     ntop=input$ntop,
                     burnin=burnin,
                     SessionID=SessionID,
                     copyright=paste("copyright (c) NRDC",CurrentYr)
      )
      
      out<-render("AppRep.Rmd", params = params)
      file.rename(out, file)
    })
  }
  
)

