

PI_TO<-function(incrate){
  Ptab3<-Ptab_TO(MSEobj,MSEobj_reb,burnin=burnin,rnd=0)
  #incProgress(incrate)
  Ptab4<<-Ptab_ord_TO(Ptab3,burnin=burnin)
  #incProgress(incrate)
  if(length(GreenMPs)>0){
    MSEobj_top_TO<<-Sub(MSEobj,MPs=GreenMPs) # for trade-off plots
    MSEobj_reb_top_TO<<-Sub(MSEobj_reb,MPs=GreenMPs) # for trade-off plots
    Ptab4<<-Ptab4[Ptab4$MP%in%GreenMPs,]
  }else{
    Ptab4<<-data.frame(MP="-",Type="-",P_RB="-",LTY="-")
  }
  nMPs_TO<<-nrow(Ptab4)
  output$Ptable2 <- function()Ptab_formatted_TO(Ptab4,burnin=burnin)
  incProgress(incrate)
}

Ptab_TO<-function(MSEobj,MSEobj_reb,burnin=5,rnd=0,Ap=FALSE){

  # PI 1.1.1
  nMPs<-MSEobj@nMPs

  # PI 1.1.2
  MGT2<-ceiling(MSEobj@OM$MGT*2)
  MGT2[MGT2<5]<-5
  MGT2[MGT2>20]<-20

  Bind<-cbind(as.matrix(expand.grid(1:MSEobj@nsim,1:MSEobj@nMPs)),rep(MGT2,MSEobj@nMPs))
  Bmat<-array(MSEobj_reb@B_BMSY[Bind],c(MSEobj_reb@nsim,MSEobj_reb@nMPs))
  P_RB<-round(apply(Bmat>1,2,mean)*100,rnd)

  # LTY
  #refY<-sum(MSEobj@C[,1,11:50])
  LTY<-apply(MSEobj@C[,,11:50,drop=FALSE],2,sum)
  LTY<-round(LTY/max(LTY,na.rm=T)*100,rnd)         #/refY*100,rnd)
  MP<-MSEobj@MPs

  tab<-data.frame(MP,P_RB, LTY)

  #if(Ap)tab<-tab[2,]
  #if(!Ap)
  tab[order(tab$LTY,decreasing=T),]


}

#
Ptab_ord_TO<-function(Ptab1,burnin=10,Eval=T,fease=F){

  # save(Ptab1,file="Ptab1")

  MPs<-as.character(Ptab1$MP)


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
  names(Ptab2)<-c("MP","Type","P_RB","LTY")

  # Rankings

  Ptab2

}

Ptab_formatted_TO<-function(Ptab4,thresh=c(70, 50, 70, 80, 50),burnin=5){

 # save(Ptab4,file="C:/temp/Ptab4")
  dynheader<-c("MP", "Type", "Rebuilding", paste0("Relative Yield (yrs ",burnin,"-50)"))
  MPurlconv<-function(x)MPurl(as.character(x))
  linky<-sapply(Ptab4$MP,MPurlconv)
  #linky<- paste0("<a href='",linko,"' target='_blank'>",linko,"</a>")
  cols<-rep('green',nrow(Ptab4))
  Ptab4 %>%
    mutate(
      MP =  cell_spec(MP, "html", color = cols,link=linky),
      Type =  cell_spec(Type, "html"),
      P_RB =  cell_spec(P_RB, "html"),
      LTY= cell_spec(LTY,"html")
    )%>%
    knitr::kable("html", escape = FALSE,align = "c", col.names=dynheader) %>%
    kable_styling("striped", full_width = F)


}
