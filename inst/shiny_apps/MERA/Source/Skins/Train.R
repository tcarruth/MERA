UpdateTrainPlots <- function() {
  
  for (pp in 1:2) {
    local({
      p <- pp
      options <- list(burnin = input$burnin, res=input$res, 
                      tab1.row.select=input$P_Tab_1_rows_selected,
                      train_nplot=length(input$P_Tab_1_rows_selected))
      height <- Skin$Planning$Fig_dim[[p]](dims)$height
      width <- Skin$Planning$Fig_dim[[p]](dims)$width
      
      if (p == 2) {
        width <- 300 * min(4,options$train_nplot)
      }
      
      output[[paste0("P_Fig_", p)]] <- renderPlot(Skin$Planning$Figs[[p]](MSEobj,MSEobj_reb,options),                                     height =ceiling(height) , width = ceiling(width))
    })
  }
}

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

PB_4 <<- function (MSEobj = NULL, Ref = 1, Yrs = c(10, 50)) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  PMobj@Caption <- 'Prob. B > BMSY'
  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@B_BMSY[, , Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(PB_4) <<- "PM"


STY1 <<- function (MSEobj = NULL, Ref = 1, Yrs = -40) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Short-Term Yield"
  PMobj@Caption <- "Short-Term Yield"
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
  PMobj@Name <- "Long-Term Yield"
  PMobj@Caption <- "Long-Term Yield"
  RefYd <- array(MSEobj@OM$RefY, dim = dim(MSEobj@C[, , Yrs[1]:Yrs[2]]))
  PMobj@Stat <- MSEobj@C[, , Yrs[1]:Yrs[2]]/RefYd
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(LTY1) <<- "PM"

Plan_proj <- function(MSEobj, qcol=rgb(0.4,0.8,0.95), 
                      lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),
                      cols=NULL) {
  
  yrs <- Current_Year+(1:MSEobj@proyears)
  nc <- MSEobj@nMPs
  nr <- 2 
  par(mfrow=c(nr,nc),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05),
      las=1)
  
  B_BMSY<-MSEobj@B_BMSY
  Blims <- c(0,quantile(B_BMSY,0.95))
  MPs<-MSEobj@MPs
  nMPs<-length(MPs)
  
  if (!is.null(cols)) {
    MPcols <- cols
  }
  cex.axis <- 1.1
  cex.mtext <- 2
  for(i in 1:nMPs){
    plot(range(yrs),Blims,col="white",yaxs="i", ylab="", xlab="", axes=FALSE) 
    axis(side=1, labels=FALSE)
    if (i == 1) {
      axis(side=2, cex.axis=cex.axis)
      mtext('B/BMSY',side=2, line=3, las=0, cex=cex.mtext)
    } else {
      axis(side=2, labels=FALSE)
    }
    
    plotquant(B_BMSY[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.25, 0.5,1))
    mtext(MPs[i],3,line=0.2,font=2,col=MPcols[i])
    
    if(i==1){
      Bdeps<-MSEobj@OM$D/MSEobj@OM$SSBMSY_SSB0#MSEobj_reb@B_BMSY[,1,1]#
      legend('topleft',legend=paste0("Starting between ",
                                     round(min(Bdeps)*100,0), "% and ", 
                                     round(max(Bdeps)*100,0), "% BMSY" ),bty='n',
             xpd=NA)
    }

  }
  
  # Yield
  refY <- array(MSEobj@OM$RefY, dim=dim(MSEobj@C))
  Yield<- MSEobj@C/refY
  Ylims <- c(0,quantile(Yield,0.95))
 
  for(i in 1:nMPs){
    plot(range(yrs),Ylims,col="white",yaxs="i", ylab="", xlab="", axes=FALSE) 
    if (i == 1) {
      axis(side=1, cex.axis=cex.axis)
      axis(side=2, cex.axis=cex.axis)
      mtext('Yield',side=2, line=3, las=0, cex=cex.mtext)
    } else {
      axis(side=1, cex.axis=cex.axis)
      axis(side=2, labels=FALSE)
    }
    
    plotquant(Yield[,i,],p=quants,yrs,qcol,lcol,ablines=c(0.1, 0.25, 0.5,1))
    # mtext(MPs[i],3,line=0.2,font=2,col=MPcols[i])
  }
  
  mtext("Year",1,line=0.7,outer=T, cex=cex.mtext)
  
}
BMSY_proj<-function(MSEobj,MSEobj_reb,options=list(),maxcol=5,qcol=rgb(0.4,0.8,0.95), 
                    lcol= "dodgerblue4",quants=c(0.05,0.25,0.75,0.95),vline=NA,fease=F){
  
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
  legend.title.size <- 12
  legend <- FALSE
  
  PMlist <- c('PB_1', 'PB_2', 'PB_3', 'LTY1')
  nPM <- length(PMlist)
  nMPs <- MSEobj@nMPs
  
  runPM <- vector("list", nPM)
  for (X in 1:nPM) {
    runPM[[X]] <- eval(call(PMlist[X], MSEobj))
  }
  
  caps <- lapply(runPM, function(x) x@Caption) %>% unlist()
  df <- data.frame(MP=lapply(runPM, function(x) x@MPs) %>% unlist(),
                   prob=lapply(runPM, function(x) x@Mean) %>% unlist(),
                   PM=rep(1:nPM, each=nMPs),
                   caption=rep(caps, each=nMPs))
  
  df$MP <- as.character(df$MP)
  Labels <- list(curE="Current Effort", CurC="Current Catch", FMSYref="FMSY Fishing", NFref="No Fishing")
  labels <- df$MP
  repnames <- names(Labels)
  labels[labels %in% repnames] <- Labels %>% unlist()
  df$MP <- labels
  
  d1 <- df %>% filter(PM ==1)
  d2 <- df %>% filter(PM ==2)
  d3 <- df %>% filter(PM ==3)
  d4 <- df %>% filter(PM ==4)
  
  library(ggplot2)
  cols <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
  
  pdat <- left_join(d1, d4, by='MP')
  p1 <- ggplot(pdat, aes(x=prob.x, y=prob.y, label=MP, color=MP)) + geom_point() + 
    ggrepel::geom_text_repel(size=lab.size) +
    expand_limits(x=c(0,1), y=c(0,1)) +
    labs(x=pdat$caption.x, y=pdat$caption.y) + 
    theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_text(size=axis.title.size),
                   axis.text = ggplot2::element_text(size=axis.text.size),
                   legend.text=ggplot2::element_text(size=legend.title.size),
                   legend.title = ggplot2::element_text(size=legend.title.size)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA),
                     panel.grid.major = ggplot2::element_line(colour = "lightgray"),
                     panel.ontop = FALSE) +
    ggplot2::scale_colour_manual(values=cols) +
    ggplot2::guides(color=FALSE)
  
  pdat <- left_join(d2, d4, by='MP')
  p2 <- ggplot(pdat, aes(x=prob.x, y=prob.y, label=MP, color=MP)) + geom_point() + 
    ggrepel::geom_text_repel(size=lab.size) +
    expand_limits(x=c(0,1), y=c(0,1)) +
    labs(x=pdat$caption.x, y='') + 
    theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_text(size=axis.title.size),
                   axis.text = ggplot2::element_text(size=axis.text.size),
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   panel.background = ggplot2::element_rect(fill = NA),
                   panel.grid.major = ggplot2::element_line(colour = "lightgray"),
                   panel.ontop = FALSE) +
    ggplot2::scale_colour_manual(values=cols) +
    ggplot2::guides(color=FALSE)
  
  pdat <- left_join(d3, d4, by='MP')
  p3 <- ggplot(pdat, aes(x=prob.x, y=prob.y, label=MP, color=MP)) + geom_point() + 
    ggrepel::geom_text_repel(size=lab.size) +
    expand_limits(x=c(0,1), y=c(0,1)) +
    labs(x=pdat$caption.x, y='') + 
    theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_text(size=axis.title.size),
                   axis.text = ggplot2::element_text(size=axis.text.size),
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   panel.background = ggplot2::element_rect(fill = NA),
                   panel.grid.major = ggplot2::element_line(colour = "lightgray"),
                   panel.ontop = FALSE) +
    ggplot2::scale_colour_manual(values=cols) +
    ggplot2::guides(color=FALSE)

  cowplot::plot_grid(
    p1, p2, p3,
    align = 'vh',
    hjust = -1,
    ncol=3,
    axis="b"
  )
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

Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- 
  options <- Intro_title <- Intro_text <- rep(list(""), 10)


Intro_title[[1]] <- "Introduction"
Intro_text[[1]] <- HTML(
  paste0(
    tags$p("The Management Planning mode is used to calculate the expected
           performance of candidate management procedures against a set of performance metrics."),
    tags$p("Four performance metrics are calculated in this analysis:", 
    tags$ol(
      tags$li('Prob. B > 0.5BMSY - the probability that the biomass is above half 
              of BMSY, a commonly used limit reference point'),
      tags$li('Prob. B > BMSY - the probability that the biomass is above BMSY, a commonly
              used target reference point'),
      tags$li('Short-Term Yield - the average yield in the first 10 years of the projection period'),
      tags$li('Long-Term Yield - the average yield in the last 10 years of the projection period')
    ), 
    tags$p("Note that the yield metrics are calculated relative to the maximum yield obtainable with
           perfect management"))))
  

Tab_title[[1]] <- "Table 1. Management Planning Performance Table"
Tab_text[[1]] <- HTML(paste0(
tags$p("The Performance Table shows the quantitative 
       performance of the candidate management 
       procedures with respect to the performance metrics described above."),
tags$p('The Table can be filtered by:',
       tags$ul(
         tags$li('MP - name of the Management Procedure'),
         tags$li('Rec. Type - management recommendation type, where 
                 TAC = total allowable catch, TAE = total allowable effort, 
                 SL = size limit, and Spatial = spatial closure'),
         tags$li('Feasible - feasibility of the MP given the available Data 
                 (note that a valid Data file must be loaded in the Extra panel for this feature)'),
         tags$li('And the four performance metrics. Use the filters sliders to set
                 minimum values to eliminate MPs with undesirable properties')
       )),
tags$p('The last column of the table (MP Doc.) provides a link to documentation 
       for each management procedure.')
))
 
                  

Tabs[[1]]<- function(MSEobj, MSEobj_reb, options=list(),rnd=1) {
  
  nMPs <- MSEobj@nMPs
  
  PMlist <- c('PB_3', 'PB_4', 'STY1', 'LTY1')
  nPM <- length(PMlist)
  runPM <- vector("list", nPM)
  for (X in 1:nPM) {
    runPM[[X]] <- eval(call(PMlist[X], MSEobj))
  }
  
  df <- data.frame(MP=lapply(runPM, function(x) x@MPs) %>% unlist(),
                   prob=lapply(runPM, function(x) x@Mean) %>% unlist(),
                   PM=rep(1:nPM, each=nMPs))
  df$prob <- round(df$prob,2)
  
  # Management Rec Type
  temp <- df %>% dplyr::group_by(MP) %>% dplyr::summarize(min=min(prob))
  df <- dplyr::left_join(df, temp, by='MP') %>% dplyr::arrange(MP)
  df$MP <- as.character(df$MP)
  df$url <- sapply(df$MP, MPurl) %>% unlist()
  types <- MPtype(df$MP)
  df$Type <- NA
  ind <- match(df$MP, types[,1])
  df$Type <- types[ind,3]
  
  # Feasible Data
  if (is.null(FeaseMPs)) {
    df$Fease <- "Unknown"
  } else {
    df$Fease <- df$MP %in% FeaseMPs
    df$Fease[df$Fease==TRUE] <- "Yes"
    df$Fease[df$Fease==FALSE] <- "No"
  }
  
  TabDF <- tidyr::spread(df, PM, prob)
  TabDF <- TabDF %>% dplyr::arrange(dplyr::desc(min))
  
  TabDF$Documentation <- paste0("<a href='", TabDF$url,
                                "' style='color: #000000' ' target='_blank'>", 'Link',"</a>")
  TabDF$Documentation[!MPwithurl] <-''
  TabDF$url <- NULL; TabDF$min <- NULL

  
  TabDF$colorcond <- "Avail"
  TabDF$colorcond[TabDF$Fease == "No"] <- 'NotAvail'
  TabDF$colorcond[TabDF$Fease == "Unknown"] <- 'Unknown'
  
  TabDF$MP <- factor(TabDF$MP)
  TabDF$Fease <- factor(TabDF$Fease)
  TabDF$Type <- factor(TabDF$Type)

  brks <- seq(0, 1, 0.25)
  palette <- colorRampPalette(colors=c("#FF0000", "#008000"))
  clrs <- palette(length(brks)+1)
  
  TabDF <<- TabDF
  DT::datatable(TabDF, escape=FALSE, caption='', filter = 'top', rownames= FALSE,
                extensions = "Buttons",
                colnames=c("MP", "Rec. Type", "Feasible",
                           runPM[[1]]@Caption, runPM[[2]]@Caption, 
                           runPM[[3]]@Caption, runPM[[4]]@Caption,
                           "MP Doc.", "ignore"),
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
                  columnDefs = list(list(className = 'dt-center', targets = 1:7),
                                    list(searchable  = FALSE, targets = 7),
                                    list(visible=FALSE, targets=8))
                )) %>%
    DT::formatStyle(colnames(TabDF), color = DT::styleInterval(brks, clrs)) %>% 
    DT::formatStyle('MP', 'colorcond' , color=DT::styleEqual(c("Avail", "NotAvail", "Unknown"), 
                                                             c('#008000', '#ff2200', '#000000')))  
  
}



Fig_title[[1]] <- "Figure 1.  Management Planning Trade-Off Plots"
Fig_text[[1]] <- HTML(paste0(
  tags$p("The quantitative results from Table 1 are plotted as a series of trade-off plots."),
  tags$p("The labels are colored according to feasibility of the MP given the uploaded data:",
         tags$ul(
           tags$li("Green - the uploaded data is sufficient to apply the MP"),
           tags$li("Red - the uploaded data is insufficient to apply the MP"),
           tags$li("Black - feasibility of the MP could not be calculated, probably
                   because no Data file has been uploaded.")
         )),
  tags$p('Select rows in Table 1 to re-draw the trade-off plots with a limited set of MPs. 
         Click the "Refresh Results" button to show results for all MPs.')
  ))

Figs[[1]] <- function(MSEobj, MSEobj_reb, options=list()) {
  if (is.null(options$tab1.row.select)) {
    select.MPs <<- MSEobj@MPs
  } else {
    select.MPs <<- TabDF$MP[options$tab1.row.select]
  }
  
  library(ggplot2)
  
  MSEobj2 <- Sub(MSEobj, MP=select.MPs)
  TabDF2 <- TabDF %>% filter(MP %in% select.MPs)
  cols <- rep("black", MSEobj2@nMPs)
  cols[TabDF2$Fease == "Yes"] <- '#008000'
  cols[TabDF2$Fease == "No"] <- '#ff2200'
  
  lab.size <- 4
  axis.title.size <- 16
  axis.text.size <- 12
  legend.title.size <- 12
  legend <- FALSE
  
  PMlist <- c('PB_3', 'PB_4', 'STY1', 'LTY1')
  nPM <- length(PMlist)
  nMPs <- MSEobj2@nMPs
  
  runPM <- vector("list", nPM)
  for (X in 1:nPM) {
    runPM[[X]] <- eval(call(PMlist[X], MSEobj2))
  }
  
  caps <- lapply(runPM, function(x) x@Caption) %>% unlist()
  df <- data.frame(MP=lapply(runPM, function(x) x@MPs) %>% unlist(),
                   prob=lapply(runPM, function(x) x@Mean) %>% unlist(),
                   PM=rep(1:nPM, each=nMPs),
                   caption=rep(caps, each=nMPs))
  
  d1 <- df %>% filter(PM ==1)
  d2 <- df %>% filter(PM ==2)
  d3 <- df %>% filter(PM ==3)
  d4 <- df %>% filter(PM ==4)
  

  pdat <- left_join(d1, d4, by='MP')
  p1 <- ggplot(pdat, aes(x=prob.x, y=prob.y, label=MP, color=MP)) + geom_point() + 
    ggrepel::geom_text_repel(size=lab.size) +
    expand_limits(x=c(0,1), y=c(0,1)) +
    labs(x=pdat$caption.x, y=pdat$caption.y) + 
    theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_text(size=axis.title.size),
                   axis.text = ggplot2::element_text(size=axis.text.size),
                   legend.text=ggplot2::element_text(size=legend.title.size),
                   legend.title = ggplot2::element_text(size=legend.title.size)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA),
                   panel.grid.major = ggplot2::element_line(colour = "lightgray"),
                   panel.ontop = FALSE) +
    ggplot2::scale_colour_manual(values=cols) +
    ggplot2::guides(color=FALSE)
  
  pdat <- left_join(d2, d4, by='MP')
  p2 <- ggplot(pdat, aes(x=prob.x, y=prob.y, label=MP, color=MP)) + geom_point() + 
    ggrepel::geom_text_repel(size=lab.size) +
    expand_limits(x=c(0,1), y=c(0,1)) +
    labs(x=pdat$caption.x, y=pdat$caption.y) + 
    theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_text(size=axis.title.size),
                   axis.text = ggplot2::element_text(size=axis.text.size),
                   panel.background = ggplot2::element_rect(fill = NA),
                   panel.grid.major = ggplot2::element_line(colour = "lightgray"),
                   panel.ontop = FALSE) +
    ggplot2::scale_colour_manual(values=cols) +
    ggplot2::guides(color=FALSE)
  
  pdat <- left_join(d1, d2, by='MP')
  p3 <- ggplot(pdat, aes(x=prob.x, y=prob.y, label=MP, color=MP)) + geom_point() + 
    ggrepel::geom_text_repel(size=lab.size) +
    expand_limits(x=c(0,1), y=c(0,1)) +
    labs(x=pdat$caption.x, y=pdat$caption.y) + 
    theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_text(size=axis.title.size),
                   axis.text = ggplot2::element_text(size=axis.text.size),
                   panel.background = ggplot2::element_rect(fill = NA),
                   panel.grid.major = ggplot2::element_line(colour = "lightgray"),
                   panel.ontop = FALSE) +
    ggplot2::scale_colour_manual(values=cols) +
    ggplot2::guides(color=FALSE)
  
  pdat <- left_join(d4, d3, by='MP')
  p4 <- ggplot(pdat, aes(x=prob.x, y=prob.y, label=MP, color=MP)) + geom_point() + 
    ggrepel::geom_text_repel(size=lab.size) +
    expand_limits(x=c(0,1), y=c(0,1)) +
    labs(x=pdat$caption.x, y=pdat$caption.y) + 
    theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_text(size=axis.title.size),
                   axis.text = ggplot2::element_text(size=axis.text.size),
                   panel.background = ggplot2::element_rect(fill = NA),
                   panel.grid.major = ggplot2::element_line(colour = "lightgray"),
                   panel.ontop = FALSE) +
    ggplot2::scale_colour_manual(values=cols) +
    ggplot2::guides(color=FALSE)
  
  cowplot::plot_grid(
    p1, p2, p3, p4,
    align = 'vh',
    hjust = -1,
    ncol=2,
    axis="b"
  )
  
  
  
  
}
Fig_dim[[1]]<-function(dims) list(height=800,width=1000)


# Projection Plot 
Fig_title[[2]] <- "Figure 2. Management Planning Projection Plots"
Fig_text[[2]] <-  HTML(paste0(
tags$p("Select up to four rows in Table 1 to plot projection plots
of biomass relative to BMSY and relative yield.",
       tags$p("Note: if only the first four 
       selected rows will be plotted if more than four rows are 
              selected in Table 1."),
       tags$p("The blue regions represent the 90% and 50% probability intervals, the white solid 
               line is the median and the dark blue lines are two example simulations. 
               For reference the grey horizontal lines denote values of 0.25, 0.5, and 1.")
Figs[[2]]<-function(MSEobj,MSEobj_reb,options=list()){
  if (is.null(options$tab1.row.select)) {
    par(mfrow=c(1,1))
    plot(c(0,1), c(0,1), type='n', axes=FALSE, xlab="", ylab="")
    text(0.5, 0.5, "Select up to 4 rows in the Table to display projection plots")
  } else {
    select.MPs <<- TabDF$MP[options$tab1.row.select]
    select.MPs <- select.MPs[1:4] %>% as.character()
    select.MPs <- select.MPs[!is.na(select.MPs)]
    nplot <<- length(select.MPs)
    TabDF2 <- TabDF %>% filter(MP %in% select.MPs)
    
    MSEobj2 <- Sub(MSEobj, MPs=select.MPs)
    
    cols <- rep("black", MSEobj2@nMPs)
    cols[TabDF2$Fease == "Yes"] <- '#008000'
    cols[TabDF2$Fease == "No"] <- '#ff2200'
    
    Plan_proj(MSEobj2,cols=cols)
  }
  
 
} 
Fig_dim[[2]]<-function(dims)list(height=600,width=600)


Planning<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
               Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)

# ---- Management Performance ----



Train <- list(Risk_Assessment=Risk_Assessment,SD=SD,Planning=Planning,Evaluation=Evaluation) 




