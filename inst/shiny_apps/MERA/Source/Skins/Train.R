
# ---- MERA Training Skin ----



# ---- Risk Assessment ----

Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- 
  options <- Intro_title <- Intro_text <- new('list')


Intro_title[[1]] <- HTML("Introduction")
Intro_text[[1]] <- HTML(
  paste("Status quo fishing effort and catches are projected to evaluate biological risk. Zero catch and FMSY fishing are also projected to frame performance.",
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


Fig_title <- rep(list(""), 9)


# Trade-Off Plot 
Fig_title[[1]] <- HTML(paste0("Figure 1. Probability B > 0.5B", tags$sub('MSY'), " and average long-term Yield Trade-Off Plot"))
Fig_text[[1]] <-  "The probability spawning biomass is above 0.5BMSY from mean generation time (MGT) through last projection year and the last 10-years of the 
projection period against the average long-term yield. MPs in the grey shaded area have failed to meet the minimum performance limit." 

Figs[[1]]<-function(MSEobj, MSEobj_reb, options=list()){
  MGT <- round(mean(MSEobj@OM$MGT),0)
  PMlist <- c("P50_1", "Yield1", "P50_2", "Yield1")
  
  Labels <- list(curE="Current Effort", CurC="Current Catch", FMSYref="FMSY Fishing", NFref="No Fishing")
  DLMtool::TradePlot(MSEobj, PMlist=PMlist, Labels=Labels, Show='plots',
                     Lims=c(0.8, 0, 0.8,0),
                     Yrs=list(P50_1=c(MGT, MSEobj@proyears), P50_2=-10))
  
} 
Fig_dim[[1]]<-function(dims)list(height=400,width=1200)


# Projection Plot 
Fig_title[[2]] <- HTML(paste0("Figure 2. Risk Assessment. B/B", tags$sub('MSY'), " projection plots"))
Fig_text[[2]] <-  "Projections of biomass relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points." 

Figs[[2]]<-function(MSEobj,MSEobj_reb,options=list()){
  MSEobj@MPs<-c("Current effort", "Current catches", "FMSY fishing", "Zero fishing")
  BMSYproj(MSEobj,MSEobj_reb,options,maxcol=4)
} 
Fig_dim[[2]]<-function(dims)list(height=400,width=1200)



# --- Tables ----
Tab_title <- rep(list(""), 9)
Tab_title[[1]] <- HTML(paste0("Table 1. Probability of B > 0.1B", tags$sub('MSY')))
Tab_text[[1]] <- HTML(paste0("The probability spawning biomass is above 0.5B", tags$sub('MSY'), 
                             " in years mean generation time through year 50 and the last 10 years of the projection period.")) 

Tabs[[1]]<- function(MSEobj, MSEobj_reb,options=list(res=5),rnd=1) {
  nMPs<-MSEobj@nMPs
  Labels <- list(curE="Current Effort", CurC="Current Catch", FMSYref="FMSY Fishing", NFref="No Fishing")
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




Risk_Assessment<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
                      Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)



# ---- Status Determination ----


# ---- Management Planning ----


# ---- Management Performance ----



Train <- list(Risk_Assessment=Risk_Assessment,SD=SD,Planning=Planning,Evaluation=Evaluation) 




