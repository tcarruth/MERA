
# Redo MSE functions

redoEval<-function(fease=F){

  withProgress(message = "Calculating Evaluation results", value = 0, {

    incrate<-1/5
    incProgress(incrate)
    burnin<<-input$burnin

    if(input$Perf_type=="MSC continuity"){
      PI_cont(incrate)
    }else if(input$Perf_type=="MSC"){
      PI_MSC(incrate) # gets the MPcols you need for PI_TO to subset
      PI_TO(incrate)

    }

    if(input$Perf_type=="MSC continuity"){
      output$P1_LTY<-renderPlot(P1_LTY_plot(MSEobj_top,burnin,MPcols=MPcols),height=400,width=400)
      output$P2_LTY<-renderPlot(P2_LTY_plot(MSEobj_top,MPcols=MPcols),height=400,width=400)
      output$P3_LTY<-renderPlot(P3_LTY_plot(MSEobj_top,MSEobj_reb_top,MPcols=MPcols),height=400,width=400)
    }else{
      if(nMPs_TO>0){
        output$P1_LTY<-renderPlot(P1_LTY_plot(MSEobj_top_TO,burnin,MPcols=MPcols),height=400,width=400)
        output$P2_LTY<-renderPlot(P2_LTY_plot(MSEobj_top_TO,MPcols=MPcols),height=400,width=400)
        output$P3_LTY<-renderPlot(P3_LTY_plot(MSEobj_top_TO,MSEobj_reb_top_TO,MPcols=MPcols),height=400,width=400)
      }else{ # Temporary hack
        output$P1_LTY<-renderPlot(plot(1,col='white',axes=F,xlab="",ylab="",main=""),height=40,width=40)
        output$P2_LTY<-renderPlot(plot(1,col='white',axes=F,xlab="",ylab="",main=""),height=40,width=40)
        output$P3_LTY<-renderPlot(plot(1,col='white',axes=F,xlab="",ylab="",main=""),height=40,width=40)
      }
    }
    output$wormplot<-renderPlot(Pplot3(MSEobj_top,MPcols=MPcols), height =ceiling(nMPs/6)*320 , width = 1300)
    output$wormplot2<-renderPlot(Rplot(MSEobj_reb_top,MPcols=MPcols), height =ceiling(nMPs/6)*320 , width = 1300)
    output$PI111_uncertain<-renderPlot(MSC_uncertain(MSEobj_top,MPcols=MPcols,maxMPs=MSEobj_top@nMPs, LTL=F,inc_thresh = F,burnin=burnin),height =ceiling(nMPs/6)*400 , width = 1100)

    incProgress(incrate)

    #VOIout<<-getVOI(MSEobj_top)
    #output$CCU<-renderPlot(CCU_plot(VOIout,MSEobj_top,MPcols=MPcols),height=ceiling(nMPs/3)*290,width=1300)
    output$Eval_Converge<-renderPlot(Converge(MSEobj_top,PMs = list(Yield, P10),ncol=2,nrow=1),height =400 , width = 1300)

  })
}

redoApp<-function(fease=F){

  withProgress(message = "Calculating Application results", value = 0, {

    incrate<-1/5
    incProgress(incrate)
    burnin<<-input$burnin

    if(input$Perf_type=="MSC continuity"){

      Ptab1_app<<-Ptab(MSEobj_app,MSEobj_reb_app,burnin=burnin,rnd=0,Ap=T)
      thresh<<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
      temp<-Ptab_ord(Ptab1_app,burnin=burnin,ntop=1, Eval=F,fease=fease,thresh=thresh)
      incProgress(incrate)
      Ptab2_app<<-temp[[1]]
      MPcols_app<<-temp[[2]]
      output$App_Ptable <- function()Ptab_formatted(Ptab2_app,burnin=burnin,cols=MPcols_app,thresh=thresh)
      output$App_threshtable<-function()Thresh_tab(thresh)

    }else if(input$Perf_type=="MSC"){

      Ptab1_app<<-Ptab_MSC(MSEobj_app,burnin=burnin,rnd=0)
      incProgress(incrate)
      thresh<<-c(input$P_STL,input$P_STT,input$P_LTL,input$P_LTT)
      temp<-Ptab_ord_MSC(Ptab1_app,burnin=burnin,ntop=1,fease=fease,thresh=thresh)
      incProgress(incrate)
      Ptab2_app<<-temp[[1]]
      MPcols_app<<-temp[[2]]
      #save(Ptab2_app,file="C:/temp/Ptab2_app")
      output$App_Ptable <- function()Ptab_formatted_MSC(Ptab2_app,burnin=burnin,cols='black',thresh=thresh)
      incProgress(incrate)
      output$App_threshtable<-function()Thresh_tab_MSC(thresh)

    }

    incProgress(incrate)
    output$MSC_PMs<-renderPlot(MSC_PMs(MSEobj_app,MSEobj_reb_app,MPcols=MPcols_app),height=1000,width=900)
    output$App_wormplot<-renderPlot(Pplot3(MSEobj_app,MPcols=MPcols_app,maxcol=1,maxrow=2), height =450 , width =550)
    output$App_wormplot2<-renderPlot(Pplot4(MSEobj_app,MPcols=MPcols_app,maxcol=1,maxrow=2), height =450 , width =550)
    output$App_wormplot3<-renderPlot(Rplot(MSEobj_reb_app,MPcols=MPcols_app,maxcol=1,maxrow=2), height =450 , width =550)
    output$App_PI111_uncertain<-renderPlot(MSC_uncertain(MSEobj_app,MPcols=MPcols_app,maxMPs=MSEobj_app@nMPs, LTL=F,inc_thresh = F,burnin=burnin),height =450 , width =550)

    #VOIout_app<<-getVOI(MSEobj_app)
    incProgress(incrate)

    #output$App_CCU<-renderPlot(CCU_plot(VOIout_app,MSEobj_app,MPcols=MPcols_app,maxrow=1,maxcol=1),height =550 , width =550)
    #output$App_VOI<-renderPlot(VOI_MSC(MSEobj_app,MPcols=MPcols_app),height =550 , width =550)

    incProgress(incrate)

  })
}

redoInd<-function(){

  styr=max(dat@Year)-min(dat@Year)+1
  PPD<-MSEobj_app@Misc[[1]]

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
  output$mdist<-renderPlot(plot_mdist(indPPD,indData,alpha=input$Ind_Alpha),height =550 ,width=550)

}
