PI_MSC<-function(incrate){
  Ptab1<<-Ptab_MSC(MSEobj,burnin=burnin,rnd=0)
  incProgress(incrate)
  thresh<<-c(input$P_STL,input$P_STT,input$P_LTL,input$P_LTT)
  temp<-Ptab_ord_MSC(Ptab1,burnin=burnin,ntop=input$ntop,fease=fease,thresh=thresh)
  incProgress(incrate)
  Ptab2<<-temp[[1]]
  MPcols<<-temp[[2]]
  MSEobj_top<<-Sub(MSEobj,MPs=Ptab2$MP)
  MSEobj_reb_top<<-Sub(MSEobj_reb,MPs=Ptab2$MP)
  #save(MSEobj_top,file="MSEobj_top")
  #save(MSEobj_reb_top,file="MSEobj_reb_top")
  nMPs<<-length(MSEobj_top@MPs)
  GreenMPs<<-Ptab2$MP[MPcols=="green"]
  #updateTextAreaInput(session,"Debug1",value=Ptab2$MP)

  output$Ptable <- function()Ptab_formatted_MSC(Ptab2,burnin=burnin,cols=MPcols,thresh=thresh)
  incProgress(incrate)
  output$threshtable<-function()Thresh_tab_MSC(thresh)

}


Ptab_MSC<-function(MSEobj,burnin=5,rnd=0){

  # PI 1.1.1
  nMPs<-MSEobj@nMPs
  P_STL<-round(apply(MSEobj@B_BMSY[,,1:burnin,drop=FALSE]>0.5,2,mean)*100,rnd)
  P_STT<-round(apply(MSEobj@B_BMSY[,,1:burnin,drop=FALSE]>1,2,mean)*100,rnd)

  # PI 1.2.1
  P_LTL<-round(apply(MSEobj@B_BMSY[,,(burnin+1):50,drop=FALSE]>0.5,2,mean)*100,rnd)
  P_LTT<-round(apply(MSEobj@B_BMSY[,,(burnin+1):50,drop=FALSE]>1,2,mean)*100,rnd)

  MP<-MSEobj@MPs
  tab<-data.frame(MP,P_STL, P_STT, P_LTL, P_LTT)
  tab[order(tab$P_LTT,decreasing=TRUE),]

}

Thresh_tab_MSC<-function(thresh=c(70, 50, 80, 50)){

  Ptab2<-as.data.frame(matrix(thresh,nrow=1))
  names(Ptab2)<-c("P_STL","P_STT","P_LTL","P_LTT")

  Ptab2 %>%
    mutate(
      P_STL = cell_spec(P_STL, "html", color = "black"),
      P_STT = cell_spec(P_STT, "html", color = "black"),
      P_LTL = cell_spec(P_LTL, "html", color = "black"),
      P_LTT = cell_spec(P_LTT, "html", color = "black")
    )%>%
    knitr::kable("html", escape = F,align = "c",col.names=c("Limit","Target","Limit","Target"))%>%
    kable_styling("striped", full_width = F)%>%
    add_header_above(c("Stock status" = 2, "Harvest Strategy"=2))%>%
    add_header_above(c("Probability Thresholds" = 4))
}

#
Ptab_ord_MSC<-function(Ptab1,thresh=c(70, 50, 80, 50),burnin=10,ntop=NA,Eval=T,fease=F){

  # save(Ptab1,file="Ptab1")

  MPs<-as.character(Ptab1$MP)
  #MPs<-avail('MP')
  if(is.na(ntop))ntop<-nrow(Ptab1)
  if(ntop>nrow(Ptab1))ntop<-nrow(Ptab1)
  if(ntop<1)ntop<-1

  # Proper Data Feasibility based on complex fease analysis by MP
  tempdat<-tempdat0<-DLMtool::SimulatedData
  tempdat@Cat<-array(NA,dim(tempdat0@Cat))
  tempdat@Ind<-array(NA,dim(tempdat0@Ind))
  tempdat@CAL<-array(NA,dim(tempdat0@CAL))
  tempdat@CAA<-array(NA,dim(tempdat0@CAA))
  tempdat@vbK<-rep(NA,length(tempdat0@vbK))
  tempdat@Abun<-rep(NA,length(tempdat0@Abun))

  ndaty<-dim(tempdat@Cat)[2]
  cond<-unlist(PanelState[[3]][1]) # cond=rep(T,9) cond=c(T,T,F,T,T,T,T,T,T)
  FeasePos<-c("Catch","Catch","Index","Index","Index","Catch_at_length","Catch_at_age","Growth","Abundance")
  Datslot<-c("Cat","Cat","Ind",  "Ind","Ind","CAL","CAA","vbK","Abun")
  yrrange<-c(ndaty, 5,    ndaty,  5,    ndaty,        2,                2, NA, NA)


    for(i in 1:length(Datslot)){
      if(cond[i]){ # if user has specified that data are available
        if(!is.na(yrrange[i])){ # it not a vector of values
          ndim<-length(dim(slot(tempdat0,Datslot[i])))
          if(ndim==2){ # is a matrix
            slot(tempdat,Datslot[i])[,ndaty-(yrrange[i]:1)+1]<-slot(tempdat0,Datslot[i])[,ndaty-(yrrange[i]:1)+1]
          }else{ # is a 3D array
            slot(tempdat,Datslot[i])[,ndaty-(yrrange[i]:1)+1,]<-slot(tempdat0,Datslot[i])[,ndaty-(yrrange[i]:1)+1,]
          }
        }else{
          slot(tempdat,Datslot[i])<-slot(tempdat0,Datslot[i])
        }
      }
    }
    if(!cond[3])tempdat@Dep<-rep(NA,2)

    DFeasible<-Fease(tempdat)

    # TAC TAE Feasibility
    cond<-unlist(PanelState[[2]][1]) # cond=rep(T,4)
    runMPs <- applyMP(tempdat0, MPs, reps = 2, nsims=1, silent=TRUE)
    recs <- runMPs[[1]]
    type <- matrix(0, nrow=length(MPs),ncol=4) # TAC TAE SL MPA
    for (mm in seq_along(recs)) {
      type[mm,1] <- as.integer(length(recs[[mm]]$TAC) > 0)
      type[mm,2] <- as.integer(length(recs[[mm]]$Effort)>0)
      type[mm,3] <- as.integer(length(recs[[mm]]$LR5)>0)
      type[mm,4] <- as.integer(!is.na(recs[[mm]]$Spatial[1,1]))
    }

    DFeasible<-unique(c(DFeasible,MPs[(type[,4]==1|type[,3]==1) & apply(type,1,sum)==1])) # Size limits and area closures might not need data

    totneeded<-apply(type,1,sum)
    speced<-matrix(rep(as.integer(cond),each=length(MPs)),nrow=length(MPs))
    MFeasible<-MPs[apply(speced*type,1,sum)==totneeded]

  MP_Type<-rep("TAC",length(MPs))
  MP_Type[type[,2]==1]<-"TAE"
  MP_Type[type[,3]==1]<-"SzLim"
  MP_Type[type[,4]==1]<-"MPA"
  MP_Type[totneeded>1]<-"Mixed"

  Ptab2<-Ptab1 #[,1:ncol(Ptab1)]
  Ptab2<-cbind(Ptab2[,1],MP_Type,Ptab2[,2:ncol(Ptab2)])
  names(Ptab2)<-c("MP","Type","P_STL","P_STT","P_LTL","P_LTT")

  PIsmet<-Ptab2$P_STL >= thresh[1] & Ptab2$P_STT >= thresh[2] & Ptab2$P_LTL >= thresh[3] & Ptab2$P_LTT >= thresh[4]
  cols<<-rep('black',length(MPs))
  cols[MPs%in%MFeasible & MPs%in%DFeasible & PIsmet]<<-'green'
  cols[MPs%in%MFeasible & MPs%in%DFeasible & !PIsmet]<<-'red'

  feasible<<-rep("",length(MPs))
  condD<-!MPs%in%DFeasible
  condM<-!MPs%in%MFeasible
  condDM<-condD&condM
  feasible[condD]<<-"D"
  feasible[condM]<<-"M"
  feasible[condDM]<<-"D/M"

  Ptab2<-cbind(Ptab2,feasible)

  # Rankings
  rnkscore<-Ptab2$P_LTT
  rnkscore[cols=="green"]=rnkscore[cols=="green"]+2000
  rnkscore[cols=="red"]=rnkscore[cols=="red"]+1000
  ord<-order(rnkscore,decreasing = T)
  Ptab2<-Ptab2[ord[1:ntop],]
  cols<<-cols[ord[1:ntop]]

  list(Ptab2, cols)

}

Ptab_formatted_MSC<-function(Ptab2X,thresh=c(70, 50, 80, 50),burnin=5,cols){

  #Ptab2<-as.data.frame(as.matrix(Ptab2))
  dynheader<-c(1,1,2,2,1)
  names(dynheader)<-c(" ", " ", paste0("Stock Status (yrs 1-",burnin,")"), paste0("Harvest Strategy (yrs ",burnin+1,"-50)"), "Reason not")
  MPurlconv<-function(x)MPurl(as.character(x))
  linky<-sapply(Ptab2X$MP,MPurlconv)
  #linky<- paste0("<a href='",linko,"' target='_blank'>",linko,"</a>")

  Ptab2X %>%
    mutate(
      MP =  cell_spec(MP, "html", color = cols,link=linky),
      Type =  cell_spec(Type, "html"),
      P_STL = ifelse(P_STL >= thresh[1],
                       cell_spec(P_STL, "html", color = "green"),
                       cell_spec(P_STL, "html", color = "red")),
      P_STT = ifelse(P_STT >= thresh[2],
                       cell_spec(P_STT, "html", color = "green"),
                       cell_spec(P_STT, "html", color = "red")),
      P_LTL = ifelse(P_LTL >= thresh[3],
                      cell_spec(P_LTL, "html", color = "green"),
                      cell_spec(P_LTL, "html", color = "red")),
      P_LTT = ifelse(P_LTT >= thresh[4],
                       cell_spec(P_LTT, "html", color = "green"),
                       cell_spec(P_LTT, "html", color = "red")),
      feasible =  cell_spec(feasible, "html")
    )%>%
    knitr::kable("html", escape = FALSE,align = "c",col.names=c("MP","Type","Limit","Target","Limit","Target","feasible"))%>%
    kable_styling("striped", full_width = F)%>%
    add_header_above(dynheader)#%>%
    #add_header_above(c(" ", " ","Stock Status" = 2, "Harvest Strategy" = 2,"Reason not"=1))
    #column_spec(5, width = "3cm")

}
