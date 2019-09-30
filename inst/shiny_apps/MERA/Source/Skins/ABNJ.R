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

PB100a<<-function (MSEobj = NULL, Ref = 1, Yrs = -40) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Probability long-term biomass is greater than BMSY"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. B > ", Ref, " BMSY (Year ", 
                            Yrs[1],"-",Yrs[2],")")
  } else {
    PMobj@Caption <- paste0("Prob. B > BMSY (Years ", 
                            Yrs[1],"-",Yrs[2],")")
  }
  if (Yrs[1] == Yrs[2]) {
    PMobj@Caption <- paste0("Prob. B > BMSY (Year ", Yrs[1],")")
  }
  
  PMobj@Ref <- Ref
  PMobj@Stat <- array(MSEobj@B_BMSY[, , Yrs[1]:Yrs[2]], dim=c(MSEobj@nsim, MSEobj@nMPs, length(Yrs[1]:Yrs[2])))
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(PB100a)<<-'PM'

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

STY1 <<- function (MSEobj = NULL, Ref = 1, Yrs = 10) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Average short-term yield"
  PMobj@Caption <- "Average short-term yield"
  RefYd <- array(MSEobj@OM$RefY, dim = dim(MSEobj@C[, , Yrs[1]:Yrs[2]]))
  PMobj@Stat <- MSEobj@C[, , Yrs[1]:Yrs[2]]/RefYd
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(STY1) <<- "PM"

LTY1 <<- function (MSEobj = NULL, Ref = 1, Yrs = -40) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Average long-term yield"
  PMobj@Caption <- "Average long-term yield"
  RefYd <- array(MSEobj@OM$RefY, dim = dim(MSEobj@C[, , Yrs[1]:Yrs[2]]))
  PMobj@Stat <- MSEobj@C[, , Yrs[1]:Yrs[2]]/RefYd
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(LTY1) <<- "PM"

AAVY1 <<- function (MSEobj = NULL, Ref = 0.2, Yrs = NULL) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Average inter-annual variability in yield "
  PMobj@Caption <- "Prob. AAVY < 20%"
  
  y1<- Yrs[1]:(Yrs[2]-1) # year index
  y2<-(Yrs[1]+1):Yrs[2] 
  
  MSEobj@C[MSEobj@C<tiny] <- tiny
  if (MSEobj@nMPs > 1) {
    AAVY <- apply(((((MSEobj@C[, , y1] - MSEobj@C[, , y2])/MSEobj@C[, , y2])^2)^0.5), c(1, 2), mean)
  } else {
    AAVY <- array(apply(((((MSEobj@C[,1,y1]-MSEobj@C[,1,y2])/MSEobj@C[,1,y2])^2)^0.5),c(1),mean))
  }
  PMobj@Stat <- AAVY
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat<Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(AAVY1) <<- "PM"

AAVE1 <<- function (MSEobj = NULL, Ref = 0.2, Yrs = -40) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Average inter-annual variability in effort "
  PMobj@Caption <- "Prob. AAVE < 20%"
  y1<- Yrs[1]:(Yrs[2]-1) # year index
  y2<-(Yrs[1]+1):Yrs[2]
  
  MSEobj@Effort[MSEobj@Effort<tiny] <- tiny
  if (MSEobj@nMPs > 1) {
    AAVE <- apply(((((MSEobj@Effort[, , y1] - MSEobj@Effort[, , y2])/MSEobj@Effort[, , y2])^2)^0.5), c(1, 2), mean)
  } else {
    AAVE <- array(apply(((((MSEobj@Effort[,1,y1]-MSEobj@Effort[,1,y2])/MSEobj@Effort[,1,y2])^2)^0.5),c(1),mean))
  }
  PMobj@Stat <- AAVE
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat<Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(AAVE1) <<- "PM"

# ============= Risk Assessment ==================

Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')

# These are the names of widgets and their values to display in this skin / mode
#             years in projection,  year resolution of reporting  rounding of digits
options<-list()

Intro_title[[1]] <- HTML("Introduction")
Intro_text[[1]] <- HTML(paste("Status quo fishing effort and catches are projected to evaluate biological risk. Zero catch and FMSY fishing are also projected to frame performance.",
                               tags$h4(tags$b('Risk Assessment Results')),
                               tags$p("This report provides a quick assessment of biological risk to 
the fishery under status quo management by comparing the performance of four 
Reference Management Procedures over 50 years into the future:",
                               tags$ol(
                                 tags$li('Current Catch'),
                                 tags$li('Current Effort'),
                                 tags$li('Fishing at FMSY'),                                       
                                 tags$li('No Fishing')
                               ))))
                               

# --- Figures ----

Fig_title <- rep(list(""), 9)

# Trade-Off Plot 
Fig_title[[2]] <- HTML(paste0("Figure 1. Probability B > 0.5B", tags$sub('MSY'), " and average long-term Yield Trade-Off Plot"))
Fig_text[[2]] <-  "The probability spawning biomass is above 0.5BMSY from mean generation time (MGT) through last projection year and the last 10-years of the 
projection period against the average long-term yield. MPs in the grey shaded area have failed to meet the minimum performance limit." 

Figs[[2]]<-function(MSEobj, MSEobj_reb, options=list()){
  MGT <- round(mean(MSEobj@OM$MGT),0)
  PMlist <- c("P50_1", "Yield1", "P50_2", "Yield1")

  Labels <- list(curE="Current Effort", curC="Current Catch", FMSYref="FMSY Fishing", NFref="No Fishing")
  DLMtool::TradePlot(MSEobj, PMlist=PMlist, Labels=Labels, Show='plots',
            Lims=c(0.8, 0, 0.8,0),
            Yrs=list(P50_1=c(MGT, MSEobj@proyears), P50_2=-10))
  
} 
Fig_dim[[2]]<-function(dims)list(height=400,width=1200)


# Projection Plot 
Fig_title[[3]] <- HTML(paste0("Figure 2. Risk Assessment. B/B", tags$sub('MSY'), " projection plots"))
Fig_text[[3]] <-  "Projections of biomass relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points." 

Figs[[3]]<-function(MSEobj,MSEobj_reb,options=list()){
  MSEobj@MPs<-c("Current effort", "Current catches", "FMSY fishing", "Zero fishing")
  BMSYproj(MSEobj,MSEobj_reb,options,maxcol=4)
} 
Fig_dim[[3]]<-function(dims)list(height=400,width=1200)



# --- Tables ----
Tab_title <- rep(list(""), 9)
# Probability of rebuilding
# Tab_title[[1]] <- "Risk Assessment Results"
# Tab_text[[1]] <- "This report provides a quick assessment of biological risk to 
# the fishery under status quo management by comparing the performance of four 
# Reference Management Procedures over 50 years into the future: 
# (1) Current Catch
# (2) Current Effort
# (3) Fishing at FMSY
# (4) No Fishing
# "

Tabs[[1]]<- function(MSE, MSEobj_reb,options=list(res=5),rnd=1) {
}


Tab_title[[1]] <- "Minimum Sustainability Limits"
Tab_text[[1]] <-  HTML(paste0("The reference management procedures are tested against the following two minimum performance limits:",
                             tags$ol(
                               tags$li('80% probability of B > 0.5 BMSY for the time period starting at the mean generation time (MGT) through year 50 of the simulation'),
                               tags$li('80% probability of B > 0.5 BMSY for years 41-50 of the simulation.')
                             )))

Tabs[[2]]<- function(MSE, MSEobj_reb,options=list(res=5),rnd=1) {
}


Tab_title[[4]] <- HTML(paste0("Table 1. Probability of B > 0.5B", tags$sub('MSY')))
Tab_text[[4]] <- HTML(paste0("The probability spawning biomass is above 0.5B", tags$sub('MSY'), 
                            " in years mean generation time through year 50 and the last 10 years of the projection period.")) 

Tabs[[4]]<- function(MSEobj, MSEobj_reb,options=list(res=5),rnd=1) {
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
  DT::datatable(TabDF, escape=FALSE, caption=caption, rownames=FALSE,
                extensions = 'Buttons',
                colnames=c("MP", runPM[[1]]@Caption, runPM[[2]]@Caption),
                class = 'display', 
                options = list(
                  buttons = 
                    list('copy', list(
                      extend = 'collection',
                      buttons = c('csv', 'excel', 'pdf'),
                      text = 'Download'
                    )),
                  dom = 'Brti',
                  autoWidth = TRUE,
                  columnDefs = list(list(width = '200px', targets = "_all"),
                                    list(searchable  = 'false', targets = 0)))) 

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



# ============= Status Determination ==================

Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')

for(i in 1:9) Fig_dim[[i]]<-function()list(height=1,width=1)

# These are the names of widgets and their values to display in this skin / mode
#             years in projection,  year resolution of reporting  rounding of digits
options<-list()

Intro_title[[1]] <- "Introduction"
Intro_text[[1]] <- "Status determination provides estimates of spawning stock biomass relative to asymptotic unfished conditions for various combinations of data types."

# --- Tables --- 
Tab_title[[1]] <- "Table 1. Depletion estimates"
Tab_text[[1]] <-"The median and 80% quantiles. "

Tabs[[1]]<-function(Status,options=list()){
  
  ncode<-length(Status$codes)
  qs<-matrix(NA,nrow=ncode,ncol=3)
  for(i in 1:ncode){
    if(length(Status$Est[[i]])>2){
      qs[i,]<-round(quantile(Status$Est[[i]]*100,c(0.1,0.5,0.9)),2)
      
    }else{
      qs[i,]<-NA
    }
  }
  
  conv<-round(unlist(lapply(Status$Est,length))/Status$nsim*100,2)
  tab<-as.data.frame(cbind(Status$codes,qs,conv))
  names(tab)<-c("Method","10%","Median","90%","Conv %")
  datatable(tab,caption="Stock status estimates",
            extensions = 'Buttons',
            options=list(buttons = 
                           list('copy', list(
                             extend = 'collection',
                             buttons = c('csv', 'excel', 'pdf'),
                             text = 'Download'
                           )),
                         dom = 'Brti')
  )
  
}

# --- Figures --- 
Fig_title[[1]] <- "Figure 1. Depletion estimates"
Fig_text[[1]] <-"The median and 80% quantiles. "

Figs[[1]]<-function(Status,options=list()){
  
  keep<-unlist(lapply(Status$Est,length))>2
  Est<-Status$Est[keep]
  nEst<-sum(keep)
  
  cols<-c('black','grey',rainbow(nEst-1))
  dens<-lapply(Est,function(x)density(x*100,from=0))
  
  ymax<-max(unlist(lapply(dens,function(x)max(x$y))))
  xmax<-max(unlist(lapply(dens,function(x)max(x$x))))
  plot(dens[[1]],type='l',xlim=c(0,xmax),ylim=c(0,ymax),main="",xlab="Depletion estimate (% unfished SSB)",ylab="Rel. Freq.")
  if(nEst>1)for(i in 2:nEst)lines(dens[[i]],col=cols[i])
  legend('topright',legend=Status$codes[keep],text.col=cols,bty='n',cex=0.9)
  
}
Fig_dim[[1]]<-function()list(height=600,width=600)

SD<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
         Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)




# ============= Planning =========================

Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')

Fig_title <- rep(list(""), 9)
Tab_title <- rep(list(""), 9)

Intro_title[[1]] <- "Introduction"
Intro_text[[1]] <- HTML(paste(tags$p("Planning mode projects multiple MPs to evaluate their absolute and relative performance with respect to target and limit reference points."),
                              tags$h4(tags$b("Planning Results")),
tags$p('The Planning mode projects the management procedures tested in the management 
strategy evaluation against their absolute and relative performance with respect 
to Minimum Sustainability Limits and Management Objectives.'),
tags$p('The intended purpose is to provide fishery managers and stakeholders a 
quantitive analytical framework with which to identify management procedures 
that will enable the fishery to be managed sustainably and best meet desired objectives. 
Such well-performing management procedures can then be further analyzed in the Evaluation
mode to further scrutinize projected performance, conduct sensitivity analyses, 
prioritize future data collection protocols, and to determine what data shall be 
collected to conduct future auxiliary indicator analysis testing whether the 
management procedure is working as expected.'),
tags$h4(tags$b('Minimum Sustainability Limits')),
                              tags$p("The user-selected group of management procedures are tested against the following 
two minimum performance limits (MPs that fail to meet at least one of the sustainability limits are colored red; 
MPs are considered feasible based on the user-specified fishery data types available in question F1):"),
                              tags$ol(
                                tags$li('80% probability of B > 0.5 BMSY for the time period starting at the mean generation time (MGT) through year 50 of the simulation'),
                                tags$li('80% probability of B > 0.5 BMSY for years 41-50 of the simulation.')
                              )
                              ))

# --- Figures ----

Fig_title[[4]] <- "Figure 1. Performance Objectives Trade-Off Plot"
Fig_text[[4]] <- "Trade-off plots of acceptable MPs for each performance objective. Non-feasible MPs are colored red."
Figs[[4]]<- function(MSEobj,MSEobj_reb,options=list()) {
  if (length(PassMPs)==0) {

  } else {
    MSEobj2 <- Sub(MSEobj, MPs=PassMPs)
    PMlist <- c("PB100a", "STY1", 
                "PB100a", "LTY1", 
                "LTY1", "AAVY1",
                "LTY1", "AAVE1")
   
    cols <- rep('#000000', MSEobj2@nMPs)

    notFease <- isFease$MPs[(nchar(isFease$feasible) > 0)]
    Notfease.ind <- MSEobj2@MPs %in% notFease
    cols[Notfease.ind] <- "#FF0000"
    Tplotout <<- TradePlot(MSEobj2, PMlist= PMlist, Lims=0, Show='plots', cols=cols)

  }
}

Fig_dim[[4]]<-function(dims) list(height=400,width=1200)

Fig_title[[5]] <- "Figure 2. Projection Plots"
Fig_text[[5]] <- "Projection plots of 6 acceptable and feasible MPs with highest yield."
Figs[[5]]<- function(MSEobj,MSEobj_reb,options=list()) {
  if (length(PassMPs)==0) {
    
  } else {
    MSEobj2 <- Sub(MSEobj, MPs=PassMPs)
    MPs <- MSEobj2@MPs[order(Tplotout$Results$LTY1, decreasing = TRUE)][1:6]
    MSEobj2 <- Sub(MSEobj, MPs=MPs)
    MPcol <- rep("black", MSEobj2@nMPs)

    nonFease <- isFease$MPs[nchar(isFease$feasible)>0]
    if (length(nonFease)>0) {
      MPcol[MSEobj2@MPs %in% nonFease] <- "red"  
    }
    
    Pplot2(MSEobj2, YVar=c("SSB_SSBMSY", "F_FMSY", "Yield"), RefYield="curr",
           incquant=TRUE, traj="both", MPcol=MPcol)
  }
}
Fig_dim[[5]]<-function(dims) list(height=400,width=1200)


Fig_title[[6]] <- "Figure 3. Trade-Off plot - Rebuilding Analysis"
Fig_text[[6]] <- "Trade-off plots of acceptable MPs for rebuilding analysis. Non-feasible MPs are colored red."
Figs[[6]] <- function(MSEobj,MSEobj_reb,options=list()) {
  if (length(PassMPs)==0) {
    
  } else {
    if (!"NFref" %in% PassMPs) PassMPs <- c(PassMPs, "NFref") 
    MSEobj2 <- Sub(MSEobj_reb, MPs=PassMPs)
    PMlist <- c("PB100a", "LTY1")
    
    # Calc Tmin 
    NFind <- match("NFref", MSEobj_reb@MPs)
    Tmin <- min(which(apply(MSEobj_reb@B_BMSY[,NFind, ] > 1, 2, mean) > 0.5)) # first year with >50% prob B > BMSY
    
    # Mean Generation Time
    MGT <- round(mean(MSEobj_reb@OM$MGT),0)
  
    cols <- rep('#000000', MSEobj2@nMPs)
    notFease <- isFease$MPs[(nchar(isFease$feasible) > 0)]
    Notfease.ind <- MSEobj2@MPs %in% notFease
    cols[Notfease.ind] <- "#FF0000"
    
    Yrs <- list(PB100a=c(Tmin+MGT, Tmin+MGT))
    RebTplotout <<- TradePlot(MSEobj2, PMlist= PMlist, Lims=0, Show='plots', 
                              cols=cols, Yrs=Yrs, Title= paste0("Tmin = ", Tmin, " MGT = ", MGT))
  }
}

Fig_dim[[6]]<-function(dims) list(height=400,width=1200)


Fig_title[[7]] <- "Figure 4. Rebuilding Analysis. B/BMSY projection plots - 6 MPs with highest yield."
Fig_text[[7]] <- "Projections of biomass relative to MSY. The vertical gray lines indicate
MGT (dotted), Tmin (dashed), and Tmin+MGT (dot-dash)." 

Figs[[7]] <- function(MSEobj,MSEobj_reb,options=list()) {
  if (length(PassMPs)==0) {
    
  } else {
    # Calc Tmin 
    NFind <- match("NFref", MSEobj_reb@MPs)
    Tmin <- min(which(apply(MSEobj_reb@B_BMSY[,NFind, ] > 1, 2, mean) > 0.5)) # first year with >50% prob B > BMSY
    
    # Mean Generation Time
    MGT <- round(mean(MSEobj_reb@OM$MGT),0)
    
    MSEobj2 <- Sub(MSEobj_reb, MPs=PassMPs)
    MPs <- MSEobj2@MPs[order(Tplotout$Results$LTY1, decreasing = TRUE)][1:6]
    MSEobj2 <- Sub(MSEobj_reb, MPs=MPs)
    
    MPcol <- rep("black", MSEobj2@nMPs)
    nonFease <- isFease$MPs[nchar(isFease$feasible)>0]
    
    MPcol[MSEobj2@MPs %in% nonFease] <- "red"
    Pplot2(MSEobj2, YVar="SSB_SSBMSY",
           incquant=TRUE, traj="quant", MPcol=MPcol,
           oneIt = FALSE,
           quants=c(0.2, 0.8),
           xline=c(Tmin, MGT, MGT+Tmin))
    
    
  }
}
  
Fig_dim[[7]]<-function(dims) list(height=400,width=1200)


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


Tab_title[[1]] <- "Table 1. Minimum Sustainability Limits"
Tab_text[[1]] <-"Management Procedures must have at least an 0.8 probability of meeting all Performance Limits to be considered Acceptable. 
MPs that fail to meet at least one of the performance limits are colored red."

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
  
  acceptMPs <- df %>% filter(min>=Prob) %>% select(MP)
  PassMPs <<- unique(acceptMPs$MP)
  
  df$Pass <- "No"
  df$Pass[df$MP %in% PassMPs] <- "Yes"
  
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

  TabDF$MP <- factor(TabDF$MP)
  TabDF$Feasible <- factor(TabDF$Feasible)
  TabDF$Type <- factor(TabDF$Type)
  TabDF$Pass <- factor(TabDF$Pass)

  cnames <- colnames(TabDF)
  cind <- which(cnames == "Type")
  cnames <- cnames[(cind+1):length(cnames)]
  
  DT::datatable(TabDF, escape=FALSE, caption=caption, filter = 'top', rownames= FALSE,
                extensions = "Buttons",
                colnames=c("MP", "Type", "Feasible", 'Pass', runPM[[1]]@Caption, runPM[[2]]@Caption),
                class = 'display', 
                options = list(
                  pageLength = 20,
                  buttons = 
                    list('copy', list(
                      extend = 'collection',
                      buttons = c('csv', 'excel', 'pdf'),
                      text = 'Download'
                    )),
                  dom = 'Blfrtip',
                  autoWidth = TRUE,
                  columnDefs = list(list(width = '150px', targets = "_all"),
                                    list(searchable  = FALSE, targets = 0))
                  )) %>%
    DT::formatStyle(colnames(TabDF), color = DT::styleInterval(cuts=c(-1, Prob*0.999), values=c("black", "red", "green"))) 
  
}


Tab_title[[2]] <- "Management Objectives"
Tab_text[[2]] <- HTML(
  paste0(tags$p("Management procedures that pass the minimum sustainability limits are then analyzed for how well they meet different management objectives.  Managers and fishery 
stakeholders can determine which objectives are most valuable in order to identify a suitable management procedure for further evaluation and adoption."),
         tags$p('The management objectives presented include:'),
         tags$ol(
           tags$li('Biomass Target - probability of B > BMSY over projected years 11-50'),
           tags$li('Short-Term Yield - average yield over projected years 1-10 relative to average yield under optimized FMSY fishing'),
           tags$li('Long-Term Yield - average yield over last 10 projected years relative to average yield under optimized FMSY fishing'),
           tags$li('Average Annual Variability in Yield - probability average inter-annual variability in yield is less than 20%'),
           tags$li('Average Annual Variability in Effort - probability average inter-annual variability in effort is less than 20%')
         )))

Tabs[[2]]<- function(MSEobj, MSEobj_reb,options=list(res=5),rnd=1) {
  Labels <- NULL
  if (length(PassMPs)==0) {
    TabDF <- data.frame(MP="No Feasible MPs met all performance limits")
    DT::datatable(TabDF, escape=FALSE, caption='',
                  class = 'display',
                  options = list(
                    dom = 't',
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '150px', targets = "_all"),
                                      )))
  } else {
    MSEobj2 <- Sub(MSEobj, MPs=PassMPs)
    nMPs <- MSEobj2@nMPs
    
    PMlist <- c("PB100a", "STY1", "LTY1", "AAVY1", "AAVE1")
    Yrs <- list(-40, 10, -10, 50, 50)
    nPM <- length(PMlist)
    
    for (X in seq_along(PMlist))
      if (!PMlist[X] %in% avail("PM")) stop(PMlist[X], " is not a valid PM method")
    runPM <- vector("list", length(PMlist))
    for (X in 1:length(PMlist)) {
      runPM[[X]] <- eval(call(PMlist[X], MSEobj2, Yrs=Yrs[[X]]))
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

    DT::datatable(TabDF, escape=FALSE, filter = 'top', rownames= FALSE, 
                  extensions = "Buttons",
                  colnames=c("MP", PMCaptions),
                  class = 'display', 
                  options = list(stateSave = TRUE,
                    pageLength = 20,
                    buttons = 
                      list('copy', list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download'
                      )),
                    dom = 'Blfrtip',
                    autoWidth = TRUE,
                    columnDefs=list(list(searchable  = FALSE, targets = 0),
                                    list(width = '150px', targets = "_all"))))

  }
}


Tab_title[[3]] <- HTML(paste0(tags$h4(tags$b("Rebuilding Analysis"))))
Tab_text[[3]] <- HTML(paste0(tags$p("Due to the inherent uncertainties around the current depletion level of data-limited fisheries, 
the rebuilding analysis provides an additional test to determine whether MPs that perform 
well under the user-specified assumed level of current depletion are robust to the 
possibility that the stock is actually more depleted than assumed. 
The rebuilding analysis assumes the fishery is currently at an overfished level 
(starting depletion set in Step D above) to test the probability the the stock would 
rebuild to BMSY within a time period calculated as the number of years rebuilding 
would be projected to occur in the absence of fishing (Tmin) plus a mean generation time for the fish species.  
")))

Tabs[[3]]<- function(MSE, MSEobj_reb,options=list(res=5),rnd=1) {

}

Tab_title[[8]] <- "Rebuilding Analysis"
Tab_text[[8]] <- "Probability of B>BMSY in Year MGT + Tmin."

Tabs[[8]]<- function(MSEobj, MSEobj_reb,options=list(res=5),rnd=1) {
  if (length(PassMPs)<1) {
    
  } else{
    if (!"NFref" %in% PassMPs) PassMPs <- c("NFref", PassMPs)
    MSEobj_reb2 <- Sub(MSEobj_reb, PassMPs)
    nMPs<-MSEobj_reb2@nMPs
    Labels <- NULL
    PMlist <- c('PB100a')
    nPM <- length(PMlist)
    runPM <- vector("list", length(PMlist))
    
    MGT <- round(mean(MSEobj_reb2@OM$MGT),0)
    NFind <- match("NFref", MSEobj_reb2@MPs)
    Tmin <- min(which(apply(MSEobj_reb2@B_BMSY[,NFind, ] > 1, 2, mean) > 0.5)) # first year with >50% prob B > BMSY
    
    Yrs <- list(c(MGT+Tmin, MGT+Tmin))
    for (X in 1:length(PMlist)) {
      runPM[[X]] <- eval(call(PMlist[X], MSEobj_reb2, Yrs=Yrs[[X]]))
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
    
    labels <- MSEobj_reb2@MPs
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
    df$MP <- labels[match(df$MP,MSEobj_reb2@MPs)]
    
    # Prob <- 0.8 # options$minProb
    
    # Feasiblity
    df$Feasible <- TRUE
    cond <<-unlist(PanelState[[3]][1]) 
    isFease <<- FeaseLabs(MSEobj_reb2@MPs,dat=NA)
    notFease <- isFease$MPs[(nchar(isFease$feasible) > 0)]
    df$Feasible[df$MP %in% notFease] <- 'No'
    df$Feasible[df$Feasible != "No"] <- "Yes"
    
    # acceptMPs <- df %>% filter(min>=Prob) %>% select(MP)

    TabDF <- tidyr::spread(df, PM, prob)
    TabDF <- TabDF %>% dplyr::arrange(desc(min))
    
    MPwithurl <- !is.na(TabDF$url) 
    # fail.ind <- TabDF$min <Prob
    # TabDF$MP[!fail.ind&MPwithurl] <-
    #   paste0("<a href='", TabDF$url[!fail.ind&MPwithurl],"' style='color: #008000' ' target='_blank'>", TabDF$MP[!fail.ind&MPwithurl],"</a>")
    # TabDF$MP[fail.ind&MPwithurl] <-
    #   paste0("<a href='", TabDF$url[fail.ind&MPwithurl],"' style='color: #FF0000' ' target='_blank'>", TabDF$MP[fail.ind&MPwithurl],"</a>")
    # 
    TabDF$MP[MPwithurl] <- paste0("<a href='", TabDF$url[MPwithurl]," ' target='_blank'>", TabDF$MP[MPwithurl],"</a>")
    
    TabDF$url <- NULL; TabDF$min <- NULL
    caption <- ""
    
    TabDF$MP <- factor(TabDF$MP)
    TabDF$Feasible <- factor(TabDF$Feasible)
    TabDF$Type <- factor(TabDF$Type)
  
    cnames <- colnames(TabDF)
    cind <- which(cnames == "Type")
    cnames <- cnames[(cind+1):length(cnames)]
    
    DT::datatable(TabDF, escape=FALSE, caption=caption, filter = 'top', rownames= FALSE, 
                  extensions = "Buttons",
                  colnames=c("MP", "Type", "Feasible", runPM[[1]]@Caption),
                  class = 'display', 
                  options = list(
                    pageLength = 20,
                    buttons = 
                      list('copy', list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download'
                      )),
                    dom = 'Blfrtip',
                    autoWidth = TRUE,
                    columnDefs=list(list(searchable  = FALSE, targets = 0),
                                    list(width = '150px', targets = "_all")))) 
    
    
  }
  
  
}


Planning<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
               Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)


# ========== Evaluation ========================

Evaluation<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
                 Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)

# ========== Build ============================= 

ABNJ <- list(Risk_Assessment=Risk_Assessment,SD=SD,Planning=Planning,Evaluation=Evaluation) 
