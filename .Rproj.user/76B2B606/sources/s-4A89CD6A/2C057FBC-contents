
# ====================================================================================================================================
# ====  Skins  =======================================================================================================================
# ====================================================================================================================================

Skins<-list()

# ==== MSC =============================================================================


# ============= Risk Evaluation ================


  

# ============= Planning =======================

  
  Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- list()
  
  # --- Tables --- 
  Tab_title[[1]] <- "Test table 1 title"
  Tab_text[[1]] <-"Test table 1 text"

  Tabs[[1]]<-function(MSEobj,planning_phase=50,rnd=1,res=5){
    
    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    ind<-1+(0:1000*res)
    ind<-ind[ind<=min(planning_phase,proyears)]
    LRP<-round(apply(MSEobj@B_BMSY[,,1:planning_phase,drop=FALSE]>0.5,2:3,mean)*100,rnd)[,ind]
    Tab1<-as.data.frame(cbind(MSEobj@MPs,LRP))
    colnams<-c("MP",ind+Current_Year)
    names(Tab1)<-colnams
    datatable(Tab1)
  
  }
  
  Tab_title[[2]] <- "Test table 2 title"
  Tab_text[[2]] <-"Test table 2 text"
  
  Tabs[[2]]<-function(MSEobj,planning_phase=50,rnd=1,res=5){
    
    nMPs<-MSEobj@nMPs
    proyears<-MSEobj@proyears
    ind<-1+(0:1000*res)
    ind<-ind[ind<=min(planning_phase,proyears)]
    TRP<-round(apply(MSEobj@B_BMSY[,,1:planning_phase,drop=FALSE]>1,2:3,mean)*100,rnd)[,ind]
    Tab2<-as.data.frame(cbind(MSEobj@MPs,TRP))
    colnams<-c("MP",ind+Current_Year)
    names(Tab2)<-colnams
    datatable(Tab2)
    
  }

  
  Tab_title[[3]] <- "Rebuilding analysis"
  Tab_text[[3]] <-"Projections of biomass relative to MSY and unfished (B0) levels. The rebuilding analysis simulates the fishery currently in a depleted state even if the user-specified depletion in the operating model is higher.
                    In these cases, the rebuilding analysis provides added assurance whether a particular management procedure would be likely to rebuild the stock if the user-specified depletion level is overly optimistic and the stock status is more depleted in actuality, and thus in need of rebuilding."
  
  Tabs[[3]]<-function(MSEobj_reb,planning_phase=50,rnd=1,res=5){
    
    nMPs<-MSEobj_reb@nMPs
    proyears<-MSEobj_reb@proyears
    ind<-1+(0:1000*res)
    ind<-ind[ind<=min(planning_phase,proyears)]
    TRP<-round(apply(MSEobj_reb@B_BMSY[,,1:planning_phase,drop=FALSE]>1,2:3,mean)*100,rnd)[,ind]
    Tab3<-as.data.frame(cbind(MSEobj_reb@MPs,TRP))
    colnams<-c("MP",ind+Current_Year)
    names(Tab3)<-colnams
    datatable(Tab3)
    
  }
  
  
  # --- Figures ---
  Fig_title[[1]] <- "B/BMSY and Yield (relative to today) projection plots"
  Fig_text[[1]] <- "Projections of biomass and yield relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points." 
  
  Figs[[1]]<-function(MSEobj,maxcol=6,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),maxrow=NA){
    
    MPcols<-rep('purple',MSEobj@nMPs)
    
    if(is.na(maxcol))maxcol=ceiling(length(MSEobj@MPs)/0.5) # defaults to portrait 1:2
    MPs<-MSEobj@MPs
    nMPs<-length(MPs)
    yrs<-Current_Year+(1:MSEobj@proyears)
    
    MPcols[MPcols=="green"]<-"darkgreen"
    
    plots<-split(1:nMPs, ceiling(seq_along(1:nMPs)/maxcol))
    
    nr<-length(plots)*2
    if(!is.na(maxrow))nr=max(nr,maxrow)
    nc<-maxcol
    
    mat<-array(0,c(nc,nr*1.5))
    ind<-floor(0.5+(1:nr)*1.5)
    mat[,ind]<-1:(nr*nc)
    mat<-t(mat)
    ht<-rep(0.2,nr*1.5)
    ht[ind]<-1
    layout(mat,heights=ht)
    par(mai=c(0.3,0.3,0.01,0.01),omi=c(0.5,0.5,0.05,0.05))
    
    B_BMSY<-MSEobj@B_BMSY
    Yd<-MSEobj@C/ array(rep(MSEobj@C[,,1],MSEobj@proyears),dim(MSEobj@C))#MSEobj@OM$RefY
    Yd[is.na(Yd)]<-0
    
    Blims <- c(0,quantile(B_BMSY,0.95))
    Ylims<- c(0,quantile(Yd,0.95))
    
    for(pp in 1:length(plots)){
      
      toplot<-unlist(plots[pp])
      nt<-length(toplot)
      
      for(i in toplot){
        plot(range(yrs),Blims,col="white")
        plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
        mtext(MSEobj@MPs[i],3,line=0.2,font=2,col=MPcols[i])
        if(i==toplot[1])mtext("B/BMSY",2,line=2.3)
      }
      if(nt<maxcol)for(i in 1:(maxcol-nt))plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel",axes=F)
      
      for(i in toplot){
        plot(range(yrs),Ylims,col="white")
        plotquant(Yd[,i,],p=quants,yrs,qcol,lcol,ablines=1)
        if(i==toplot[1])mtext("Yield relative to today",2,line=2.3)
      }
      if(nt<maxcol)for(i in 1:(maxcol-nt))plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel",axes=F)
      
    }
    
    mtext("Projection Year",1,line=0.7,outer=T)
    
  }
 
  
  Fig_title[[2]] <- "Rebuilding analysis"
  Fig_text[[2]] <- "Projections of biomass relative to MSY and unfished (B0) levels given a starting depletion of half BMSY. The rebuilding analysis simulates the fishery currently in a depleted state even if the user-specified depletion in the operating model is higher.
                    In these cases, the rebuilding analysis provides added assurance whether a particular management procedure would be likely to rebuild the stock if the user-specified depletion level is overly optimistic and the stock status is more depleted in actuality, and thus in need of rebuilding.
                    The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the limit and target reference points" 

  Figs[[2]]<-function(MSEobj_reb,maxcol=6,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),maxrow=NA){
    
    MPcols<-rep('purple',MSEobj_reb@nMPs)
    
    if(is.na(maxcol))maxcol=ceiling(length(MSEobj_reb@MPs)/0.5) # defaults to portrait 1:2
    MPs<-MSEobj_reb@MPs
    nMPs<-length(MPs)
    yrs<-Current_Year+(1:MSEobj_reb@proyears)
    
    MPcols[MPcols=="green"]<-"darkgreen"
    
    plots<-split(1:nMPs, ceiling(seq_along(1:nMPs)/maxcol))
    
    nr<-length(plots)*2
    if(!is.na(maxrow))nr=max(nr,maxrow)
    nc<-maxcol
    
    mat<-array(0,c(nc,nr*1.5))
    ind<-floor(0.5+(1:nr)*1.5)
    mat[,ind]<-1:(nr*nc)
    mat<-t(mat)
    ht<-rep(0.2,nr*1.5)
    ht[ind]<-1
    layout(mat,heights=ht)
    par(mai=c(0.3,0.3,0.01,0.01),omi=c(0.5,0.5,0.05,0.05))
    
    B_BMSY<-MSEobj_reb@B_BMSY
    B_B0<-MSEobj_reb@B_BMSY*MSEobj_reb@OM$SSBMSY_SSB0#<-MSEobj_reb@C/ array(rep(MSEobj_reb@C[,,1],MSEobj_reb@proyears),dim(MSEobj_reb@C))#MSEobj_reb@OM$RefY
    
    Blims <- c(0,quantile(B_BMSY,0.95))
    B2lims<- c(0,quantile(B_B0,0.95))
    
    
    for(pp in 1:length(plots)){
      
      toplot<-unlist(plots[pp])
      nt<-length(toplot)
      
      for(i in toplot){
        plot(range(yrs),Blims,col="white")
        plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1))
        mtext(MSEobj_reb@MPs[i],3,line=0.2,font=2,col=MPcols[i])
        if(i==toplot[1])mtext("B/BMSY",2,line=2.3)
      }
      if(nt<maxcol)for(i in 1:(maxcol-nt))plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel",axes=F)
      
      for(i in toplot){
        plot(range(yrs),B2lims,col="white")
        plotquant(B_B0[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.4,1))
        if(i==toplot[1])mtext("B/B0",2,line=2.3)
      }
      if(nt<maxcol)for(i in 1:(maxcol-nt))plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel",axes=F)
      
    }
    
    mtext("Projection Year",1,line=0.7,outer=T)
    
  }

  Planning<-list(Tabs=Tabs,Figs=Figs,Tab_title=Tab_title,Tab_text=Tab_text, Fig_title=Fig_title,Fig_text=Fig_text)
  
 
# ====== Assessment ===========================

  Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- list()

  
  
  
  
  
  
MSC<-list(Risk_Evaluation=Planning,Planning=Planning,Assessment=Planning) 
Skins$MSC<-MSC





# ===== FAO ========================================================================================


# ============= Risk Evaluation ================

# ============= Planning =======================

# ============= Assessment =====================

MSC<-list(Risk_Evaluation=Planning,Planning=Planning,Assessment=Planning) 
Skins$FAO<-MSC

# ===== Pew ========================================================================================


# ============= Risk Evaluation ================

# ============= Planning =======================

# ============= Assessment =====================

MSC<-list(Risk_Evaluation=Planning,Planning=Planning,Assessment=Planning) 
Skins$Pew<-MSC



# ====== Wrap up ====================================================================================

Skin<-Skins[[1]]



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
