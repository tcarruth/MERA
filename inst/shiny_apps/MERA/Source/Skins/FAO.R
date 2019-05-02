# ====================================================================================================================================
# ====  FAO Skin  ====================================================================================================================
# ====================================================================================================================================

# Performance Metrics
P50_1 <<- function (MSEobj = NULL, Ref = 0.5, Yrs = NULL) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. SB > ", Ref, " SBMSY (Years ", 
                            Yrs[1], " - ", Yrs[2], ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. SB > SBMSY (Years ", 
                            Yrs[1], " - ", Yrs[2], ")")
  }
  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@B_BMSY[, , Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(P50_1) <<- "PM"

P50_2 <<- function (MSEobj = NULL, Ref = 0.5, Yrs = NULL) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. SB > ", Ref, " SBMSY (Years ", 
                            Yrs[1], " - ", Yrs[2], ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. SB > SBMSY (Years ", 
                            Yrs[1], " - ", Yrs[2], ")")
  }
  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@B_BMSY[, , Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(P50_2) <<- "PM"

Yield1 <<- function (MSEobj = NULL, Ref = 1, Yrs = -5) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Yield relative to Reference Yield (Years ", 
                       Yrs[1], "-", Yrs[2], ")")
  PMobj@Caption <- "Average relative long-term yield"
  RefYd <- array(MSEobj@OM$RefY, dim = dim(MSEobj@C[, , Yrs[1]:Yrs[2]]))
  PMobj@Stat <- MSEobj@C[, , Yrs[1]:Yrs[2]]/RefYd
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(Yield1) <<- "PM"

# ============= Risk Assessment ==================

Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')

# These are the names of widgets and their values to display in this skin / mode
#             years in projection,  year resolution of reporting  rounding of digits
options<-list()

Intro_title[[1]] <- "Introduction"
Intro_text[[1]] <- "Status quo fishing effort and catches are projected to evaluate biological risk. Zero catch and FMSY fishing are also projected to frame performance."

# --- Figures ----

# Trade-Off Plot 
Fig_title[[1]] <- "Figure 1. Probability B > 0.5BMSY, F < FMSY, and average long-term Yield Trade-Off Plot"
Fig_text[[1]] <-  "The probability spawning biomass is above 0.5BMSY and F < FMSY in projection years from mean generation time (MGT) through last projection year against the average long-term yield." 

Figs[[1]]<-function(MSEobj, MSEobj_reb, options=list()){
  MGT <- round(mean(MSEobj@OM$MGT),0)
  PMlist <- c("P50_1", "Yield1", "PNOF", "Yield1")

  Labels <- list(curE="Current Effort", curC="Current Catch", FMSYref="FMSY Fishing", NFref="No Fishing")
  DLMtool::TradePlot(MSEobj, PMlist=PMlist, Labels=Labels, Show='plots',
            Lims=c(0.8, 0, 0.8,0),
            Yrs=list(P50_1=c(MGT, MSEobj@proyears), P50_2=-10))
  
} 
Fig_dim[[1]]<-function(dims)list(height=400,width=1200)


# Projection Plot 
Fig_title[[2]] <- "Figure 2. Risk Assessment. B/BMSY and Yield (relative to today) projection plots"
Fig_text[[2]] <-  "Projections of biomass and yield relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points." 

Figs[[2]]<-function(MSEobj,MSEobj_reb,options=list()){
  MSEobj@MPs<-c("Current effort", "Current catches", "FMSY fishing", "Zero fishing")
  BMSYproj(MSEobj,MSEobj_reb,options,maxcol=4)
} 
Fig_dim[[2]]<-function(dims)list(height=400,width=1200)

Fig_title[[3]] <- ""

# --- Tables ----

# Probability of rebuilding
Tab_title[[1]] <- ""
Tab_text[[1]] <-""

Tabs[[1]]<- function(MSE, MSEobj_reb,options=list(res=5),rnd=1) {
}


Tab_title[[2]] <- "Table 1. Probability of B > 0.5BMSY"
Tab_text[[2]] <- "The probability spawning biomass is above 0.5BMSY in years mean generation time through year 50 and the last 10 years of the projection period." 

Tabs[[2]]<- function(MSEobj, MSEobj_reb,options=list(res=5),rnd=1) {
  nMPs<-MSEobj@nMPs
  Labels <- list(curE="Current Effort", curC="Current Catch", FMSYref="FMSY Fishing", NFref="No Fishing")
  PMlist <- c('P50_1', 'P50_2')
  nPM <- length(PMlist)
  runPM <- vector("list", length(PMlist))

  MGT <- round(mean(MSEobj@OM$MGT),0)
  Yrs <- list(c(MGT, MSEobj@proyears), c(-10))
  for (X in 1:length(PMlist)) {
    runPM[[X]] <- eval(call(PMlist[X], MSEobj, Yrs=Yrs[[X]]))
  }
  
  df <- data.frame(MP=lapply(runPM, function(x) x@MPs) %>% unlist(),
                   prob=lapply(runPM, function(x) x@Mean) %>% unlist(),
                   PM=rep(1:nPM, each=nMPs))
  df$prob <- round(df$prob,2)
  
  temp <- df %>% dplyr::group_by(MP) %>% dplyr::summarize(min=min(prob))
  df <- dplyr::left_join(df, temp, by='MP') %>% dplyr::arrange(MP)
  df$MP <- as.character(df$MP)
  df$url <- sapply(df$MP, MPurl) %>% unlist()
  types <- MPtype(df$MP)
  df$Type <- NA
  ind <- match(df$MP, types[,1])
  df$Type <- types[ind,2]
  
  labels <- MSEobj@MPs
  if (class(Labels) == "list") {
    repnames <- names(Labels)
    invalid <- repnames[!repnames %in% labels]
    if (length(invalid >0)) {
      warning("Labels: ", paste(invalid, collapse=", "), " are not MPs in MSE")
      Labels[invalid] <- NULL
      repnames <- names(Labels)
    }
    labels[labels %in% repnames] <- Labels %>% unlist()
  }
  df$MP <- labels[match(df$MP,MSEobj@MPs)]
  

  Prob <- 0# .8
  TabDF <- tidyr::spread(df, PM, prob)
  TabDF <- TabDF %>% dplyr::arrange(desc(min))
  
  MPwithurl <- !is.na(TabDF$url) 
  # fail.ind <- TabDF$min <=Prob 
  # TabDF$MP[!fail.ind&MPwithurl] <- 
  #   paste0("<a href='", TabDF$url[!fail.ind&MPwithurl],"' style='color: #008000' ' target='_blank'>", TabDF$MP[!fail.ind&MPwithurl],"</a>")
  # TabDF$MP[fail.ind&MPwithurl] <- 
  #   paste0("<a href='", TabDF$url[fail.ind&MPwithurl],"' style='color: #FF0000' ' target='_blank'>", TabDF$MP[fail.ind&MPwithurl],"</a>")

  TabDF$MP[MPwithurl] <- paste0("<a href='", TabDF$url[MPwithurl]," ' target='_blank'>", TabDF$MP[MPwithurl],"</a>")

  
  TabDF$url <- NULL; TabDF$Type = NULL; TabDF$min <- NULL
  caption <- ""
  DT::datatable(TabDF, escape=FALSE, caption=caption,
                colnames=c('', "MP", runPM[[1]]@Caption, runPM[[2]]@Caption),
                class = 'display', 
                options = list(
                  dom = 't',
                  autoWidth = TRUE,
                  columnDefs = list(list(width = '200px', targets = "_all")))) 

}

# 
# Tab_title[[2]] <- "Table 2. Projected biomass relative to BMSY"
# Tab_text[[2]] <-"The probability that projected biomass is above BMSY. "
# 
# Tabs[[2]]<-function(MSEobj,MSEobj_reb,options=list(res=5),rnd=1){
#   
#   nMPs<-MSEobj@nMPs
#   proyears<-MSEobj@proyears
#   ind<-1+(0:1000*options$res)
#   ind<-ind[ind<=proyears]
#   
#   TRP<-round(apply(MSEobj@B_BMSY>1,2:3,mean)*100,rnd)[,ind]
#   Tab1<-as.data.frame(cbind(c("Current effort", "Current catches", "FMSY fishing", "Zero fishing"),TRP),stringsAsFactors = F)
#   for(i in 2:ncol(Tab1)) Tab1[,i]<-as.numeric(Tab1[,i])
#   colnams<-c("MP",ind+Current_Year)
#   names(Tab1)<-colnams
#   Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
#   caption=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
#   datatable(Tab1,caption=caption,options=list(dom='t'))%>%
#     formatStyle(columns = 2:ncol(Tab1), valueColumns = 2:ncol(Tab1), color = styleInterval(c(50,100),c('red','orange','green')))
#   
# }


Risk_Assessment<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
                      Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)



# ============= Planning =========================

Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')

# These are the names of widgets and their values to display in this skin / mode
#             years in projection,  year resolution of reporting  rounding of digits

Intro_title[[1]] <- "Introduction"
Intro_text[[1]] <- "Planning mode projects multiple MPs to evaluate their absolute and relative performance with respect to target and limit reference points."

# --- Figures ----

# --- Tables ----

FeaseLabs<-function(MPs,dat=NA){
  
  nMPs<-length(MPs) 
  
  # Proper Data Feasibility based on complex fease analysis by MP
  tempdat<-tempdat0<-DLMtool::SimulatedData
  tempdat@Cat<-array(NA,dim(tempdat0@Cat))
  tempdat@Ind<-array(NA,dim(tempdat0@Ind))
  tempdat@CAL<-array(NA,dim(tempdat0@CAL))
  tempdat@CAA<-array(NA,dim(tempdat0@CAA))
  tempdat@vbK<-rep(NA,length(tempdat0@vbK))
  tempdat@Abun<-rep(NA,length(tempdat0@Abun))
  
  ndaty<-dim(tempdat@Cat)[2]
  cond<-unlist(PanelState[[3]][1]) # cond=rep(T,9) cond=c(T,T,F,T,T,F,T,T,T)
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
  
  if(!is.na(dat)){
    DFeasible<-RealFease(dat)
  }else{
    DFeasible<-Fease(tempdat,msg=F)
  }
  
  # TAC TAE Feasibility
  cond<-unlist(PanelState[[2]][1]) # cond=rep(T,4) cond=c(F,T,T,T)
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
  
  cols<-rep('black',length(MPs))
  #cols[!MPs%in%MFeasible & !MPs%in%DFeasible]<-'purple'
  #cols[!MPs%in%MFeasible & MPs%in%DFeasible]<-'red'
  cols[!MPs%in%MFeasible | !MPs%in%DFeasible]<-'red'
  
  feasible<-rep("",length(MPs))
  condD<-!MPs%in%DFeasible
  condM<-!MPs%in%MFeasible
  condDM<-condD&condM
  feasible[condD]<-"D"
  feasible[condM]<-"M"
  feasible[condDM]<-"D/M"
  
  # Rankings
  #rnkscore<-Ptab2$LTY
  rnkscore<-rep(0,nMPs)
  # rnkscore[cols=="green"]=rnkscore[cols=="green"]+2000
  rnkscore[cols=="red"]=rnkscore[cols=="red"]+1000
  ord<-order(rnkscore,decreasing = T)
  
  list(feasible=feasible,MPcols=cols,MPs=MPs,MP_Type=MP_Type,ord=ord)
  
}


Tab_title[[1]] <- "Table 1. Minimum Performance Criteria"
Tab_text[[1]] <-"Management Procedures must have at least an 0.8 probability of meeting all Performance Limits to be considered Acceptable. MPs that fail to meet at least one of the performance limits are colored red."

Tabs[[1]]<- function(MSEobj, MSEobj_reb, options=list(),rnd=1) {
  nMPs<-MSEobj@nMPs
  Labels <- NULL # list(curE="Current Effort", curC="Current Catch", FMSYref="FMSY Fishing", NFref="No Fishing")
  PMlist <- c('P50_1', 'P50_2')
  nPM <- length(PMlist)
  runPM <- vector("list", length(PMlist))
  
  MGT <- round(mean(MSEobj@OM$MGT),0)
  Yrs <- list(c(MGT, MSEobj@proyears), c(-10))
  for (X in 1:length(PMlist)) {
    runPM[[X]] <- eval(call(PMlist[X], MSEobj, Yrs=Yrs[[X]]))
  }
  
  df <- data.frame(MP=lapply(runPM, function(x) x@MPs) %>% unlist(),
                   prob=lapply(runPM, function(x) x@Mean) %>% unlist(),
                   PM=rep(1:nPM, each=nMPs))
  df$prob <- round(df$prob,2)
  
  temp <- df %>% dplyr::group_by(MP) %>% dplyr::summarize(min=min(prob))
  df <- dplyr::left_join(df, temp, by='MP') %>% dplyr::arrange(MP)
  df$MP <- as.character(df$MP)
  df$url <- sapply(df$MP, MPurl) %>% unlist()
  types <- MPtype(df$MP)
  df$Type <- NA
  ind <- match(df$MP, types[,1])
  df$Type <- types[ind,2]
  
  labels <- MSEobj@MPs
  if (class(Labels) == "list") {
    repnames <- names(Labels)
    invalid <- repnames[!repnames %in% labels]
    if (length(invalid >0)) {
      warning("Labels: ", paste(invalid, collapse=", "), " are not MPs in MSE")
      Labels[invalid] <- NULL
      repnames <- names(Labels)
    }
    labels[labels %in% repnames] <- Labels %>% unlist()
  }
  df$MP <- labels[match(df$MP,MSEobj@MPs)]
  
  Prob <- 0.8 # options$minProb
  
  # Feasiblity
  df$Feasible <- TRUE
  cond <<-unlist(PanelState[[3]][1]) 
  isFease <<- FeaseLabs(MSEobj@MPs,dat=NA)
  notFease <- isFease$MPs[(nchar(isFease$feasible) > 0)]
  df$Feasible[df$MP %in% notFease] <- 'No'
  df$Feasible[df$Feasible != "No"] <- "Yes"
  
  TabDF <- tidyr::spread(df, PM, prob)
  TabDF <- TabDF %>% dplyr::arrange(desc(min))
  
  MPwithurl <- !is.na(TabDF$url) 
  fail.ind <- TabDF$min <Prob
  TabDF$MP[!fail.ind&MPwithurl] <-
    paste0("<a href='", TabDF$url[!fail.ind&MPwithurl],"' style='color: #008000' ' target='_blank'>", TabDF$MP[!fail.ind&MPwithurl],"</a>")
  TabDF$MP[fail.ind&MPwithurl] <-
    paste0("<a href='", TabDF$url[fail.ind&MPwithurl],"' style='color: #FF0000' ' target='_blank'>", TabDF$MP[fail.ind&MPwithurl],"</a>")

  # TabDF$MP[MPwithurl] <- paste0("<a href='", TabDF$url[MPwithurl]," ' target='_blank'>", TabDF$MP[MPwithurl],"</a>")
  
  TabDF$url <- NULL; TabDF$min <- NULL
  caption <- ""

  acceptMPs <- df %>% filter(min>=Prob) %>% select(MP)
  PassMPs <<- unique(acceptMPs$MP)
    
  DT::datatable(TabDF, escape=FALSE, caption=caption,
                colnames=c('', "MP", "Type", "Feasible", runPM[[1]]@Caption, runPM[[2]]@Caption),
                class = 'display', 
                options = list(
                  dom = 't',
                  autoWidth = TRUE
               )) 
  
}


Tab_title[[2]] <- "Table 2. Performance Objectives"
Tab_text[[2]] <- "Probability of meeting the performance objectives for acceptable MPs. MPs that are not feasible with current data are indicated in red text."

Tabs[[2]]<-function(MSEobj,MSEobj_reb,options=list(res=1),rnd=1){
  Labels <- NULL
  if (length(PassMPs)==0) {
    TabDF <- data.frame(MP="No Feasible MPs met all performance limits")
    DT::datatable(TabDF, escape=FALSE, caption='',
                  class = 'display', 
                  options = list(
                    dom = 't',
                    autoWidth = TRUE))
  } else {
    MSEobj2 <- Sub(MSEobj, MPs=PassMPs)
    nMPs <- MSEobj2@nMPs
    
    PMlist <- c("STY", "LTY", "P10", "AAVY")
    nPM <- length(PMlist)
    
    for (X in seq_along(PMlist))
      if (!PMlist[X] %in% avail("PM")) stop(PMlist[X], " is not a valid PM method")
    runPM <- vector("list", length(PMlist))
    for (X in 1:length(PMlist)) {
      runPM[[X]] <- eval(call(PMlist[X], MSEobj2))
    }
    
    Plimits <- lapply(runPM, function(x) x@Caption) %>% unlist()
    chk <- grepl("<", Plimits)
    if (sum(chk)) {
      ind <- which(chk)  
      for (x in ind)
        Plimits[x] <- gsub("<", "\\\\<", Plimits[x])
    }
    
    colsfun <- colorRampPalette(c("forestgreen", "orange", "red"))
    cols <- rev(colsfun(5))
    quants <- seq(0, 1, length.out=length(cols)-1)
    levels <- cut(quants, quants) %>% levels()
    
    # Create data frame of probs
    df <- data.frame(MP=lapply(runPM, function(x) x@MPs) %>% unlist(),
                     prob=lapply(runPM, function(x) x@Mean) %>% unlist(),
                     PM=rep(1:nPM, each=nMPs))
    df$prob <- round(df$prob,2)
    temp <- df %>% dplyr::group_by(MP) %>% dplyr::summarize(min=min(prob))
    df <- dplyr::left_join(df, temp, by='MP') %>% dplyr::arrange(MP)
    df$MP <- as.character(df$MP)
    df$url <- sapply(df$MP, MPurl) %>% unlist()
    types <- MPtype(df$MP)
    df$Type <- NA
    ind <- match(df$MP, types[,1])
    df$Type <- types[ind,2]
    
    labels <- MSEobj2@MPs
    if (class(Labels) == "list") {
      repnames <- names(Labels)
      invalid <- repnames[!repnames %in% labels]
      if (length(invalid >0)) {
        warning("Labels: ", paste(invalid, collapse=", "), " are not MPs in MSE")
        Labels[invalid] <- NULL
        repnames <- names(Labels)
      }
      labels[labels %in% repnames] <- Labels %>% unlist()
    }
    df$MP <- labels[match(df$MP,MSEobj2@MPs)]
    
    TabDF <- tidyr::spread(df, PM, prob)
    TabDF <- TabDF %>% dplyr::arrange(desc(min))
    
    TabDF$min <- NULL; TabDF$prob <- NULL
    MPwithurl <- !is.na(TabDF$url) 
    
    MPwithurl <- !is.na(TabDF$url) 
    notFease <- isFease$MPs[(nchar(isFease$feasible) > 0)]
    Notfease.ind <- TabDF$MP %in% notFease
    
    TabDF$MP[Notfease.ind&MPwithurl] <-
      paste0("<a href='", TabDF$url[Notfease.ind&MPwithurl],"' style='color: #FF0000' ' target='_blank'>", TabDF$MP[Notfease.ind&MPwithurl],"</a>")
    TabDF$MP[!Notfease.ind&MPwithurl] <-
      paste0("<a href='", TabDF$url[!Notfease.ind&MPwithurl],"' style='color: #000000' ' target='_blank'>", TabDF$MP[!Notfease.ind&MPwithurl],"</a>")
  
    TabDF$url <- NULL
    TabDF$Type <- as.factor(TabDF$Type)
    
    cnames <- colnames(TabDF)
    cind <- which(cnames == "Type")
    cnames <- cnames[(cind+1):length(cnames)]
    TabDF$Type <- NULL
    
    PMCaptions <- vector("character", length(runPM))
    for (i in 1:length(runPM)) {
      PMCaptions[i] <- runPM[[i]]@Caption
    }
    
    colnames <- c("", "MP", PMCaptions)
    DT::datatable(TabDF, escape=FALSE, caption='',
                  colnames=colnames,
                  class = 'display', 
                  options = list(
                    dom = 't',
                    autoWidth = TRUE
                  )) %>%
      DT::formatStyle(cnames, color = DT::styleInterval(cuts=quants, values=cols))
  }
}

Tab_title[[3]] <- Tab_title[[4]] <- Tab_title[[5]] <- Tab_title[[6]] <- Tab_title[[7]] <- Tab_title[[8]] <- Tab_title[[9]] <- "" # make extras empty


# --- Figures ----
Fig_title <- rep(list(""), 9)

Fig_title[[3]] <- "Figure 1.Performance Objectives Trade-Off Plot"
Fig_text[[3]] <- "Trade-off plots of acceptable MPs for each performance objective. Non-feasible MPs are colored red." 
Figs[[3]]<- function(MSEobj,MSEobj_reb,options=list()) {
  if (length(PassMPs)==0) {
    
  } else {
    MSEobj2 <- Sub(MSEobj, MPs=PassMPs)
    PMlist <- c("STY", "LTY", "P10", "AAVY")
    PMs <- combn( PMlist, 2)
    cols <- rep('#000000', MSEobj2@nMPs)
    
    notFease <- isFease$MPs[(nchar(isFease$feasible) > 0)]
    Notfease.ind <- MSEobj2@MPs %in% notFease
    cols[Notfease.ind] <- "#FF0000"
    TradePlot(MSEobj2, PMlist= as.vector(PMs), Lims=0, Show='plots', cols=cols)
    
  }
}
  
Fig_dim[[3]]<-function(dims)list(height=555,width=1100)


Fig_title[[4]] <- "Figure 2.Projection Plots"
Fig_text[[4]] <- "Projection plots of acceptable and feasible MPs" 
Figs[[4]]<- function(MSEobj,MSEobj_reb,options=list()) {
  if (length(PassMPs)==0) {
    
  } else {
    MSEobj2 <- Sub(MSEobj, MPs=PassMPs)
    
    Pplot2(MSEobj2, YVar=c("SSB_SSBMSY", "SSB_SSB0", "Yield"), RefYield="curr",
           incquant=TRUE, traj="both")

  }
}

Fig_dim[[4]]<-function(dims)list(height=555,width=1100)


Planning<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
               Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)


# ========== Evaluation ========================

Evaluation<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
                 Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)

# ========== Build ============================= 

FAO <- list(Risk_Assessment=Risk_Assessment,Planning=Planning,Evaluation=Evaluation) 
