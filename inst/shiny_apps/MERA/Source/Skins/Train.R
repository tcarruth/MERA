
# ---- MERA Training Skin ----


# Performance Metrics
PB_1 <<- function (MSEobj = NULL, Ref = 0.1, Yrs = c(10, 50)) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  PMobj@Caption <- paste0('Prob. B > ', Ref, 'BMSY')
  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@B_BMSY[, , Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(PB_1) <<- "PM"

PB_2 <<- function (MSEobj = NULL, Ref = 0.25, Yrs = c(10, 50)) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  PMobj@Caption <- paste0('Prob. B > ', Ref, 'BMSY')
  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@B_BMSY[, , Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(PB_2) <<- "PM"

PB_3 <<- function (MSEobj = NULL, Ref = 0.5, Yrs = c(10, 50)) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  PMobj@Caption <- paste0('Prob. B > ', Ref, 'BMSY')
  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@B_BMSY[, , Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(PB_3) <<- "PM"


BMSY_proj<-function(MSEobj,MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),vline=NA,fease=F){
  
  if(fease){
    MPcols=MPcols#FeaseLabs(MSEobj@MPs)$MPcols
  }else{
    MPcols<-rep('black',MSEobj@nMPs)
  }
  
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  
  MPs[MPs=='curE'] <- "Current Effort"
  MPs[MPs=='CurC'] <- "Current Catch"
  MPs[MPs=='FMSYref'] <- "FMSY Fishing"
  MPs[MPs=='NFref'] <- "No Fishing"
  
  yrs <- Current_Year+(1:MSEobj@proyears)
  
  nc<-maxcol
  nr<-ceiling(nMPs/nc)
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05),
      las=1)
  
  B_BMSY<-MSEobj@B_BMSY
  Blims <- c(0,quantile(B_BMSY,0.95))
  
  cex.axis <- 1.1
  cex.mtext <- 2
  for(i in 1:nMPs){
    plot(range(yrs),Blims,col="white",yaxs="i", ylab="", xlab="", axes=FALSE) 
    if (i == 1) {
      axis(side=1, cex.axis=cex.axis)
      axis(side=2, cex.axis=cex.axis)
    } else {
      axis(side=1, cex.axis=cex.axis)
      axis(side=2, labels=FALSE)
    }

    plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.1, 0.25, 0.5,1))
    mtext(MPs[i],3,line=0.2,font=2,col=MPcols[i])
    
    if(i==1){
      Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0#MSEobj_reb@B_BMSY[,1,1]#
      legend('topleft',legend=paste0("Starting between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" ),bty='n')
    }
    if(!is.na(vline))abline(v=yrs[vline],lwd=2)
    if("YIU"%in%names(options))abline(v=yrs[options$YIU],lwd=2) #polygon(yrs[c(1,options$burnin,options$burnin,1)],c(-10,-10,10,10),col='lightgrey',border=NA)
    
  }
  
  mtext("B/BMSY",2,line=0.7,outer=T, las=0, cex=cex.mtext)
  mtext("Year",1,line=0.7,outer=T, cex=cex.mtext)
  
}

# ---- Risk Assessment ----

Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- 
  options <- Intro_title <- Intro_text <- new('list')


Intro_title[[1]] <- HTML("Introduction")
Intro_text[[1]] <- HTML(
  paste0(
    tags$p("In the Risk Assessment mode, the fishery system is projected forward in time under two 
           Status Quo management scenarios:",
         tags$ol(
           tags$li('Current Catch - catches in the future are held constant at 
                   the current catch level'),
           tags$li('Current Effort - fishing effort in the future is held constant
                   at the current effort level')
         ), 
         tags$p("and two Reference scenarios:"),
         tags$ol(
           tags$li('FMSY Fishing - a fixed fishing mortality rate of FMSY is 
                   used for all future years'),                                       
           tags$li('No Fishing - no fishing occurs in any years in the future')
           )
         ),
  tags$p("The two Reference scenarios are used to provide context to the Status 
         Quo management scenarios. FMSY Fishing represents essentially perfect
         fisheries management (fixed F at FMSY) and is used to calculate biological
         risk under these conditions. The No Fishing Scenario shows the expected 
         behaviour of the stock if fishing did not continue in the future.")
  )
)




Fig_title <- rep(list(""), 9)
Tab_title <- rep(list(""), 9)


Tab_title[[1]] <- "Table 1. Risk Assessment Probability Table"

Tab_text[[1]] <- HTML(paste0("The probability that the spawning biomass is 
                         above 0.1, 0.25, and 0.5 B", tags$sub("MSY"), " for two
                         status quo management scenarios (Current Catch and
                         Current Effort) and two Reference methods (FMSY Fishing
                         and No Fishing)."))

Tabs[[1]]<- function(MSEobj, MSEobj_reb,options=list(res=5),rnd=1) {

  nMPs<-MSEobj@nMPs
  Labels <- list(curE="Current Effort", CurC="Current Catch", FMSYref="FMSY Fishing", NFref="No Fishing")
  PMlist <- c('PB_1', 'PB_2', 'PB_3')
  nPM <- length(PMlist)
  runPM <- vector("list", length(PMlist))
  
  Yrs <- list(c(10, MSEobj@proyears),
              c(10, MSEobj@proyears),
              c(10, MSEobj@proyears))
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
  

  TabDF <- tidyr::spread(df, PM, prob)
  TabDF <- TabDF %>% dplyr::arrange(MP)
  MPwithurl <- !is.na(TabDF$url) 

  TabDF$MP[MPwithurl] <- paste0("<a href='", TabDF$url[MPwithurl]," ' target='_blank'>", TabDF$MP[MPwithurl],"</a>")
  TabDF$url <- NULL; TabDF$Type = NULL; TabDF$min <- NULL

  DT::datatable(TabDF, escape=FALSE, caption='', rownames=FALSE,
                extensions = 'Buttons',
                colnames=c("MP", runPM[[1]]@Caption, runPM[[2]]@Caption, runPM[[3]]@Caption),
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
     


# Trade-Off Plot 
Fig_title[[1]] <- "Figure 1. Risk Asssement Trade-Off Plot"
Fig_text[[1]] <- HTML(paste0('Trade-off plots showing the probability that spawning biomass 
                             is above 0.1, 0.25, and 0.5 B', tags$sub('MSY'),
                             ' (x-axis) and the average relative long-term yield (y-axis).')) 

Figs[[1]]<-function(MSEobj, MSEobj_reb, options=list()){

  lab.size <- 6
  axis.title.size <- 16
  axis.text.size <- 12
  legend <- FALSE
  
  Labels <- list(curE="Current Effort", CurC="Current Catch", FMSYref="FMSY Fishing", NFref="No Fishing")
  p1 <- DLMtool::TradePlot(MSEobj, PMlist=list("PB_1", "Yield1"), Labels=Labels,
                           Show='plots', Lims=0, legend=legend,
                           lab.size=lab.size, 
                           axis.title.size = axis.title.size,
                           axis.text.size = axis.text.size)[[2]][[1]]
  p2 <- DLMtool::TradePlot(MSEobj, PMlist=list("PB_2", "Yield1"), Labels=Labels, 
                           Show='plots', Lims=0, legend=legend,
                           lab.size=lab.size, 
                           axis.title.size = axis.title.size,
                           axis.text.size = axis.text.size)[[2]][[1]]
  p3 <- DLMtool::TradePlot(MSEobj, PMlist=list("PB_3", "Yield1"), Labels=Labels, 
                           Show='plots', Lims=0, legend=legend,
                           lab.size=lab.size, 
                           axis.title.size = axis.title.size,
                           axis.text.size = axis.text.size)[[2]][[1]]
  
  p1 <- p1 + ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA),
                            panel.grid.major = ggplot2::element_line(colour = "lightgray"),
                            panel.ontop = FALSE)
  
  p2 <- p2 + ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA),
                            panel.grid.major = ggplot2::element_line(colour = "lightgray"),
                            panel.ontop = FALSE)

  p3 <- p3 + ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA),
                            panel.grid.major = ggplot2::element_line(colour = "lightgray"),
                            panel.ontop = FALSE)
  
  p2 <- p2 + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_blank())
  
  p3 <- p3 + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_blank())
  prow <- cowplot::plot_grid(
    p1, p2, p3,
    align = 'vh',
    hjust = -1,
    ncol=3
  )
  
  cowplot::plot_grid(
    p1 + ggplot2::theme(legend.position="none"),
    p2 + ggplot2::theme(legend.position="none"),
    p3 + ggplot2::theme(legend.position="none"),
    align = 'vh',
    hjust = -1,
    ncol=3,
    axis="b"
  )
  # 
  # legend <- cowplot::get_legend(
  #   # create some space to the left of the legend
  #   p1 + ggplot2::theme(legend.box.margin = ggplot2::margin(0, 0, 0, 12))
  # )
  # 
  # cowplot::plot_grid(prow, legend, rel_widths = c(3, .4))
  
  # DLMtool::TradePlot(MSEobj, PMlist=list("PB_1", "Yield1", 
  #                                        "PB_2", "Yield1",
  #                                        "PB_3", "Yield1"), 
  #                    Labels=Labels,
  #                    Show='plots', Lims=0)
  
  
} 
Fig_dim[[1]]<-function(dims)list(height=400,width=1200)


# Projection Plot 
Fig_title[[2]] <- "Figure 2. Risk Assessment Projection Plots"
Fig_text[[2]] <-  "Projections of biomass relative to MSY levels. The blue 
regions represent the 90% and 50% probability intervals, the white solid 
line is the median and the dark blue lines are two example simulations. 
Grey horizontal lines denote 0.1, 0.25, 0.5BMSY, and BMSY reference points." 

Figs[[2]]<-function(MSEobj,MSEobj_reb,options=list()){
  BMSY_proj(MSEobj,MSEobj_reb,options,maxcol=4)
} 
Fig_dim[[2]]<-function(dims)list(height=400,width=1200)









Risk_Assessment<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
                      Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)



# ---- Status Determination ----


# ---- Management Planning ----

# Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')
# Fig_title <- Tab_title <- rep(list(""), 10)
# 
# Intro_title[[1]] <- "Introduction"
# Intro_text[[1]] <- HTML(paste(tags$p("Planning mode projects multiple MPs to evaluate their absolute and relative performance with respect to target and limit reference points."),
#                               tags$h4(tags$b("Planning Results")),
#                               tags$p('The Planning mode projects the management procedures tested in the management 
# strategy evaluation against their absolute and relative performance with respect 
# to Minimum Sustainability Limits and Management Objectives.'),
#                               tags$p('The intended purpose is to provide fishery managers and stakeholders a 
# quantitive analytical framework with which to identify management procedures 
# that will enable the fishery to be managed sustainably and best meet desired objectives. 
# Such well-performing management procedures can then be further analyzed in the Evaluation
# mode to further scrutinize projected performance, conduct sensitivity analyses, 
# prioritize future data collection protocols, and to determine what data shall be 
# collected to conduct future auxiliary indicator analysis testing whether the 
# management procedure is working as expected.'),
#                               tags$h4(tags$b('Minimum Sustainability Limits')),
#                               tags$p("The user-selected group of management procedures are tested against the following 
# two minimum performance limits (MPs that fail to meet at least one of the sustainability limits are colored red; 
# MPs are considered feasible based on the user-specified fishery data types available in question F1):"),
#                               tags$ol(
#                                 tags$li('80% probability of B > 0.5 BMSY for the time period starting at the mean generation time (MGT) through year 50 of the simulation'),
#                                 tags$li('80% probability of B > 0.5 BMSY for years 41-50 of the simulation.')
#                               )
# ))

 



# ---- Management Performance ----



Train <- list(Risk_Assessment=Risk_Assessment,SD=SD,Planning=Planning,Evaluation=Evaluation) 




