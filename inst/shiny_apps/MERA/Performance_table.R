

PI_cont<-function(incrate){
  Ptab1<<-Ptab(MSEobj,MSEobj_reb,burnin=burnin,rnd=0)
  incProgress(incrate)
  thresh<<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
  fease<-input$Fease
  print(fease)
  temp<-Ptab_ord(Ptab1,burnin=burnin,ntop=input$ntop,fease=fease,thresh=thresh)
  incProgress(incrate)
  Ptab2<<-temp[[1]]
  MPcols<<-temp[[2]]
  MSEobj_top<<-Sub(MSEobj,MPs=Ptab2$MP)
  MSEobj_reb_top<<-Sub(MSEobj_reb,MPs=Ptab2$MP)
  #save(MSEobj_top,file="MSEobj_top")
  #save(MSEobj_reb_top,file="MSEobj_reb_top")
  nMPs<<-length(MSEobj_top@MPs)
  #updateTextAreaInput(session,"Debug1",value=Ptab2$MP)
  output$Ptable <- function()Ptab_formatted(Ptab2,burnin=burnin,cols=MPcols,thresh=thresh)
  #incProgress(incrate)
  output$threshtable<-function()Thresh_tab(thresh)

}

Ptab<-function(MSEobj,MSEobj_reb,burnin=5,rnd=0,Ap=FALSE){

  # PI 1.1.1
  nMPs<-MSEobj@nMPs
  PI.111.a<-round(apply(MSEobj@B_BMSY[,,1:burnin,drop=FALSE]>0.5,2,mean)*100,rnd)
  PI.111.b<-round(apply(MSEobj@B_BMSY[,,1:burnin,drop=FALSE]>1,2,mean)*100,rnd)

  # PI 1.1.2
  MGT2<-ceiling(MSEobj@OM$MGT*2)
  MGT2[MGT2<5]<-5
  MGT2[MGT2>20]<-20

  Bind<-cbind(as.matrix(expand.grid(1:MSEobj@nsim,1:MSEobj@nMPs)),rep(MGT2,MSEobj@nMPs))
  Bmat<-array(MSEobj_reb@B_BMSY[Bind],c(MSEobj_reb@nsim,MSEobj_reb@nMPs))
  PI.112<-round(apply(Bmat>1,2,mean)*100,rnd)

  # PI 1.2.1
  PI.121.a<-round(apply(MSEobj@B_BMSY[,,11:50,drop=FALSE]>0.5,2,mean)*100,rnd)
  PI.121.b<-round(apply(MSEobj@B_BMSY[,,11:50,drop=FALSE]>1,2,mean)*100,rnd)

  # LTY
  #refY<-sum(MSEobj@C[,1,11:50])
  LTY<-apply(MSEobj@C[,,11:50,drop=FALSE],2,sum)
  LTY<-round(LTY/max(LTY,na.rm=T)*100,rnd)
  MP<-MSEobj@MPs

  tab<-data.frame(MP,PI.111.a, PI.111.b, PI.112, PI.121.a, PI.121.b,LTY)
  tab<-tab[order(tab$LTY,decreasing=T),]
  tab

}

Thresh_tab<-function(thresh=c(70, 50, 70, 80, 50)){

  Ptab2<-as.data.frame(matrix(thresh,nrow=1))
  names(Ptab2)<-c("PI.111a","PI.111b","PI.112","PI.121a","PI.121b")

  Ptab2 %>%
    mutate(
      PI.111a =   cell_spec(PI.111a, "html", color = "black"),
      PI.111b =   cell_spec(PI.111b, "html", color = "black"),
      PI.112 = cell_spec(PI.112, "html", color = "black"),
      PI.121a = cell_spec(PI.121a, "html", color = "black"),
      PI.121b =  cell_spec(PI.121b, "html", color = "black")
    )%>%
    knitr::kable("html", escape = F,align = "c") %>%
    kable_styling("striped", full_width = F)%>%
    column_spec(5, width = "3cm")  %>%
    add_header_above(c("Thresholds" = 5))
}

#                                 11a 11b 12  21a 21a
Ptab_ord<-function(Ptab1,thresh=c(70, 50, 70, 80, 50),burnin=10,ntop=NA,Eval=T,fease=FALSE){

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

  if(fease){
    DFeasible<-RealFease(dat)
  }else{
    DFeasible<-Fease(tempdat)
  }
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
  names(Ptab2)<-c("MP","Type","PI.111a","PI.111b","PI.112","PI.121a","PI.121b","LTY")

  PIsmet<-Ptab2$PI.111a >= thresh[1] & Ptab2$PI.111b >= thresh[2] # & Ptab2$PI.112 >= thresh[3] & Ptab2$PI.121a >= thresh[4] & Ptab2$PI.121b >= thresh[5]
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
  rnkscore<-Ptab2$LTY
  rnkscore[cols=="green"]=rnkscore[cols=="green"]+2000
  rnkscore[cols=="red"]=rnkscore[cols=="red"]+1000
  ord<-order(rnkscore,decreasing = T)
  Ptab2<-Ptab2[ord[1:ntop],]
  cols<<-cols[ord[1:ntop]]

  list(Ptab2, cols)

}

Ptab_formatted<-function(Ptab2,thresh=c(70, 50, 70, 80, 50),burnin=5,cols){

  dynheader<-c(1,1,2,1,2,1,1)
  names(dynheader)<-c(" ", " ", paste0("Biomass (yrs 1-",burnin,")"), "Biomass (2 MGT)", paste0("Biomass (yrs ",burnin,"-50)"), paste0("Yield (yrs ",burnin+1,"-50)"),"Reason")
  MPurlconv<-function(x)MPurl(as.character(x))
  linky<-sapply(Ptab2$MP,MPurlconv)
  #linky<- paste0("<a href='",linko,"' target='_blank'>",linko,"</a>")

  Ptab2 %>%
    mutate(
      MP =  cell_spec(MP, "html", color = cols,link=linky),
      Type =  cell_spec(Type, "html"),
      PI.111a = ifelse(PI.111a >= thresh[1],
                       cell_spec(PI.111a, "html", color = "green"),
                       cell_spec(PI.111a, "html", color = "red")),
      PI.111b = ifelse(PI.111b >= thresh[2],
                       cell_spec(PI.111b, "html", color = "green"),
                       cell_spec(PI.111b, "html", color = "red")),
      PI.112 = ifelse(PI.112 >= thresh[3],
                      cell_spec(PI.112, "html", color = "green"),
                      cell_spec(PI.112, "html", color = "red")),
      PI.121a = ifelse(PI.121a >= thresh[4],
                       cell_spec(PI.121a, "html", color = "green"),
                       cell_spec(PI.121a, "html", color = "red")),
      PI.121b = ifelse(PI.121b >= thresh[5],
                       cell_spec(PI.121b, "html", color = "green"),
                       cell_spec(PI.121b, "html", color = "red")),
      LTY =  cell_spec(LTY, "html"),
      feasible =  cell_spec(feasible, "html")

    )%>%
    knitr::kable("html", escape = FALSE,align = "c") %>%
    kable_styling("striped", full_width = F)%>%
    column_spec(5, width = "3cm") %>%
    add_header_above(c(" ", " ","> 0.5 BMSY" = 1, "> BMSY" = 1,
                       "> BMSY"=1,"> 0.5 BMSY"=1,"> BMSY"=1,"vs FMSYref"=1,"not"=1))%>%

    add_header_above(dynheader)%>%

    add_header_above(c(" ", " ", "Stock Status" = 2, "Rebuilding" = 1,
                       "Harvest Strategy"=2,
                       "Relative Yield"=1," "=1))


}
