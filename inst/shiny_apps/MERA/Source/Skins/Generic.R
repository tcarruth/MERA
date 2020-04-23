UpdateGenericSkinPlots <- function() {
  
  for (pp in c(1,2,3,4)) {
    local({
      p <- pp
      options <- list(burnin = input$burnin, res=input$res, 
                      tab1.row.select=input$P_Tab_1_rows_selected,
                      tab3.row.select=input$P_Tab_3_rows_selected,
                      train_nplot=length(input$P_Tab_1_rows_selected),
                      train_nplot3=length(input$P_Tab_3_rows_selected))
      height <- Skin$Planning$Fig_dim[[p]](dims)$height
      width <- Skin$Planning$Fig_dim[[p]](dims)$width
      
      if (p == 2) {
        if (is.null(options$tab1.row.select)) {
          width <- 600
        } else {
          width <- 300 * min(4,options$train_nplot)  
        }
        
      }
      if (p == 4) {
        if (is.null(options$tab3.row.select)) {
          width <- 600
        } else {
          width <- 300 * min(4,options$train_nplot3)  
        }
        
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


STY1 <<- function (MSEobj = NULL, Ref = 1, Yrs = 10) {
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





# ============= Status Determination ==================

Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')
Fig_title <- Tab_title <- rep(list(""), 10)
#for(i in 1:10)Fig_dim[[i]]<-function(dims)list(height=1,width=1)

# These are the names of widgets and their values to display in this skin / mode
#             years in projection,  year resolution of reporting  rounding of digits
options<-list()

Intro_title[[1]] <- "Introduction"
Intro_text[[1]] <- "Status determination provides estimates of spawning stock biomass relative to asymptotic unfished conditions for various combinations of data types."

# --- Tables --- 
Tab_title[[1]] <- "Table 1. Depletion estimates (SSB relative to unfished)"
Tab_text[[1]] <-"Quantiles of the depletion estimates of various methods. Method refers to a stochastic 
  stock reduction analysis fitted to various combinations of data types (C Catch, I Index, M mean length, CAA Catch at age composition, CAL Catch at length composition).
  'Conv' is the fraction of runs that converged."

Tabs[[1]]<-function(Status,options=list()){
  
  ncode<-length(Status$codes)
  qs<-matrix(NA,nrow=ncode,ncol=5)
  for(i in 1:ncode){
    if(length(Status$Est[[i]])>2){
      qs[i,]<-round(quantile(Status$Est[[i]]*100,c(0.025,0.05,0.5,0.95,0.975)),2)
      
    }else{
      qs[i,]<-NA
    }
  }
  
  conv<-round(unlist(lapply(Status$Est,length))/Status$nsim*100,2)
  tab<-as.data.frame(cbind(Status$codes,qs,conv))
  names(tab)<-c("Method","2.5%","5%","Median","95%","97.5%","Conv %")
  datatable(tab,caption="Stock status estimates (SSB relative to 'unfished')",
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
Fig_title[[1]] <- "Figure 1. Depletion estimates (SSB relative to unfished)"
Fig_text[[1]] <-"The median, interquartile range and 95% interval of stock depletion estimated by various methods."

Figs[[1]]<-function(Status,options=list()){
  
  keep<-unlist(lapply(Status$Est,length))>3
  Est<-Status$Est[keep]
  nEst<-sum(keep)
  
  cols<-rep(rgb(0.4,0.8,0.95),nEst)#c('darkgrey','lightgrey',rainbow(nEst-1))
  
  SDdat<-data.frame(y=unlist(Est),x=rep(Status$codes[keep],unlist(lapply(Est,length))))
  
  boxplot(y~x,SDdat,col=cols,xlab="Status Determination Method",yaxs='i',ylab="Estimated Status (%, SSB relative to unfished)")
  #legend('topright',legend=Status$codes[keep],text.col=cols,bty='n',cex=0.9)
  abline(h=seq(0.1,1,length.out=10),col="grey")
  boxplot(y~x,SDdat,col=cols,xlab="Status Determination Method",yaxs='n',
          ylab="Estimated Status (SSB relative to unfished)",add=T)
  
  
}
Fig_dim[[1]]<-function(dims)list(height=500,width=400)

Fig_title[[2]] <- "Figure 2. Spawning stock depletion relative to equilibrium SSB in initial year "
Fig_text[[2]] <-"The first panel shows median estimated depletion trend for all status determination methods. 
Subsequent panels show the 90th (light grey), 50th (dark grey) and median estimates (white line) for each Status determination method"

Figs[[2]]<-function(Status,options=list()){
  
  ntot<-length(Status$Fit)
  keep<-unlist(lapply(Status$Est,length))>3 # keep estimates from one of the methods of estimation
  nmods<-sum(keep)
  keep_ind<-(1:ntot)[keep]
  nplots<-nmods+1 # add the overall mean plot
  cols<-c('darkgrey','lightgrey',rainbow(nmods-1))
  
  nc<-5
  nr<-ceiling(nplots/nc)
  
  procdeps_inst<-function(x){  # Instantaneous version
    t(sapply(1:length(x@Misc),function(X,listy)listy[[X]]$E[1:(length(listy[[X]]$E)-1)],listy=x@Misc)
      / sapply(1:length(x@Misc),function(X,listy)listy[[X]]$E0,listy=x@Misc))
  } 
  
  procdeps<-function(x){
    t(sapply(1:length(x@Misc),function(X,listy)listy[[X]]$E[1:(length(listy[[X]]$E)-1)],listy=x@Misc))/
      sapply(1:length(x@Misc),function(X,listy)listy[[X]]$E0[1],listy=x@Misc)
  } 
  
  deps<-lapply(Status$Fit,procdeps) # ntot matrices of depletion (nsim x nyears)
  
  getquants<-function(x)  apply(x,2,quantile,p=c(0.05,0.25,0.5,0.75,0.95))
  Dqs<-lapply(deps,getquants)
  meds<-matrix(unlist(lapply(Dqs,function(x)x[3,])),ncol=ntot)[,keep,drop=F]
  ny<-nrow(meds)
  par(mfrow=c(1,1),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  
  #plot(c(0,ny),c(0,1),col="white",yaxs='i',ylab="",xlab="")
  #abline(h=seq(0.1,1,length.out=10),col="light grey")
  #matplot(meds,type='l',col=cols,add=T,lty=1) 
  
  #legend('topright',legend=Status$codes[keep],text.col=cols,bty='n',cex=0.9)
  #mtext("Median trend, all methods",3,line=0.2,font=2)
  
  qplot<-function(mat,xlab=1:ny,main=""){ #qcol=rgb(0.4,0.8,0.95), lcol= "dodgerblue4"
    
    plot(c(0,ny),c(0,1),col="white",yaxs='i',ylab="",xlab="")
    abline(h=seq(0.1,1,length.out=10),col="light grey")
    
    polygon(c(xlab,xlab[length(xlab):1]),c(mat[1,],mat[5,ncol(mat):1]),col=rgb(0.4,0.8,0.95),border=rgb(0.4,0.8,0.95))
    polygon(c(xlab,xlab[length(xlab):1]),c(mat[2,],mat[4,ncol(mat):1]),col="dodgerblue4",border="dodgerblue4")
    lines(xlab,mat[3,],col='white',lwd=1)
    mtext(main,3,line=0.2,font=2)
    
  }
  
  for(i in keep_ind){
    
    qplot(Dqs[[i]],xlab=1:ny,main=Status$codes[[i]])
    
  }
  
  mtext("Historical Year",1,line=0.5,outer=T)
  mtext("Stock Depletion (SSB relative to unfished)",2,line=0.5,outer=T)
  
}
Fig_dim[[2]]<-function(dims)list(height=500*ceiling(dims$nmeth/5),width=700)



SD<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
         Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)



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
  
  MPwithurl <- nchar(df$url)>0
  TabDF$Documentation <- paste0("<a href='", TabDF$url,
                                "' style='color: #000000' ' target='_blank'>", 
                                TabDF$MP,
                                "</a>")
  TabDF$Documentation[which(!MPwithurl)] <- TabDF$MP[which(!MPwithurl)] # ""
  TabDF$url <- NULL; TabDF$min <- NULL
  
  TabDF$colorcond <- "Avail"
  TabDF$colorcond[TabDF$Fease == "No"] <- 'NotAvail'
  TabDF$colorcond[TabDF$Fease == "Unknown"] <- 'Unknown'
  
  TabDF$MP <- TabDF$Documentation # factor(TabDF$MP)
  TabDF$Documentation <- NULL
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
                           "ignore"),
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
                  columnDefs = list(list(className = 'dt-center', targets = 1:6),
                                    list(visible=FALSE, targets=7))
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
  tags$p('Select rows in Table 1 to re-draw the trade-off plots with a limited set of MPs.') 
))

Figs[[1]] <- function(MSEobj, MSEobj_reb, options=list()) {
  if (is.null(options$tab1.row.select)) {
    select.MPs <- MSEobj@MPs
  } else {
    tab.select <<- TabDF$MP[options$tab1.row.select]
    select.MPs <- tab.select
    # check if url
    ind <- which(grepl("href",tab.select))
    if (length(ind)>0) {
      mp.names <- lapply(strsplit(tab.select[ind], "_blank'>"), "[[", 2) %>% unlist()
      select.MPs[ind] <- gsub('</a>', '', mp.names)
    }
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
of biomass relative to BMSY and relative yield."),
  tags$p("Note: if only the first four selected rows will be plotted if more than
four rows are selected in Table 1."),
  tags$p("The blue regions represent the 90% and 50% probability intervals, the white solid 
               line is the median and the dark blue lines are two example simulations. 
               For reference the grey horizontal lines denote values of 0.25, 0.5, and 1.")
))
Figs[[2]] <-function(MSEobj,MSEobj_reb,options=list()){
  if (is.null(options$tab1.row.select)) {
    par(mfrow=c(1,1))
    plot(c(0,1), c(0,1), type='n', axes=FALSE, xlab="", ylab="")
    text(0.5, 0.5, "Select up to 4 rows in the Table to display projection plots")
  } else {
    
    tab.select <<- TabDF$MP[options$tab1.row.select]
    select.MPs <- tab.select
    # check if url
    ind <- which(grepl("href",tab.select))
    if (length(ind)>0) {
      mp.names <- lapply(strsplit(tab.select[ind], "_blank'>"), "[[", 2) %>% unlist()
      select.MPs[ind] <- gsub('</a>', '', mp.names)
    }
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



# ---- Rebuilding ----

Tab_title[[3]] <- HTML(paste0(tags$h4(tags$b("Table 2. Rebuilding Analysis"))))
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
  
  
  Labels <- NULL
  PMlist <- c('PB_4')
  nPM <- length(PMlist)
  runPM <- vector("list", length(PMlist))
  
  MGT <- round(mean(MSEobj_reb@OM$MGT),0)
  NFind <- match("NFref", MSEobj_reb@MPs)
  Tmin <- min(which(apply(MSEobj_reb@B_BMSY[,NFind, ] > 1, 2, mean) > 0.5)) # first year with >50% prob B > BMSY
  
  mps <- MSEobj_reb@MPs
  mps <- mps[!grepl('NFref',mps)]
  MSEobj_reb <- Sub(MSEobj_reb, MPs=mps)
  nMPs<-MSEobj_reb@nMPs
  Yrs <- list(c(MGT+Tmin, MGT+Tmin))
  for (X in 1:length(PMlist)) {
    runPM[[X]] <- eval(call(PMlist[X], MSEobj_reb, Yrs=Yrs[[X]]))
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
  
  MPwithurl <- nchar(df$url)>0
  TabDF$Documentation <- paste0("<a href='", TabDF$url,
                                "' style='color: #000000' ' target='_blank'>", 
                                TabDF$MP,
                                "</a>")
  TabDF$Documentation[which(!MPwithurl)] <- TabDF$MP[which(!MPwithurl)] # ""
  TabDF$url <- NULL; TabDF$min <- NULL
  
  TabDF$colorcond <- "Avail"
  TabDF$colorcond[TabDF$Fease == "No"] <- 'NotAvail'
  TabDF$colorcond[TabDF$Fease == "Unknown"] <- 'Unknown'
  
  TabDF$MP <- TabDF$Documentation # factor(TabDF$MP)
  TabDF$Documentation <- NULL
  TabDF$Fease <- factor(TabDF$Fease)
  TabDF$Type <- factor(TabDF$Type)
  
  brks <- seq(0, 1, 0.25)
  palette <- colorRampPalette(colors=c("#FF0000", "#008000"))
  clrs <- palette(length(brks)+1)
  
  TabDF2 <<- TabDF
  
  DT::datatable(TabDF2, escape=FALSE, caption='', filter = 'top', rownames= FALSE,
                extensions = "Buttons",
                colnames=c("MP", "Rec. Type", "Feasible",
                           runPM[[1]]@Caption, 'ignore'), 
                
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
                  columnDefs = list(list(className = 'dt-center', targets = 1:3),
                                    list(visible=FALSE, targets=4))
                )) %>%
    DT::formatStyle(colnames(TabDF), color = DT::styleInterval(brks, clrs)) %>% 
    DT::formatStyle('MP', 'colorcond' , color=DT::styleEqual(c("Avail", "NotAvail", "Unknown"), 
                                                             c('#008000', '#ff2200', '#000000'))) 
  
}


Fig_title[[3]] <- "Figure 3.  Rebuilding Trade-Off Plot"
Fig_text[[3]] <- HTML(paste0(
  tags$p("The quantitative results for the rebuiling analysis shown in Table 2 are
         plotted as a trade-off plot with expected long-term yield."),
  tags$p("The labels are colored according to feasibility of the MP given the uploaded data:",
         tags$ul(
           tags$li("Green - the uploaded data is sufficient to apply the MP"),
           tags$li("Red - the uploaded data is insufficient to apply the MP"),
           tags$li("Black - feasibility of the MP could not be calculated, probably
                   because no Data file has been uploaded.")
         )),
  tags$p('Select rows in Table 2 to re-draw the trade-off plots with a limited set of MPs.') 
))

Figs[[3]] <- function(MSEobj, MSEobj_reb, options=list()) {
  mps <- MSEobj_reb@MPs
  mps <- mps[!grepl('NFref',mps)]
  MSEobj_reb <- Sub(MSEobj_reb, MPs=mps)
  
  if (is.null(options$tab3.row.select)) {
    select.MPs <- MSEobj_reb@MPs
  } else {
    tab.select <- TabDF2$MP[options$tab3.row.select]
    select.MPs <- tab.select
    # check if url
    ind <- which(grepl("href",tab.select))
    if (length(ind)>0) {
      mp.names <- lapply(strsplit(tab.select[ind], "_blank'>"), "[[", 2) %>% unlist()
      select.MPs[ind] <- gsub('</a>', '', mp.names)
    }
  }
  library(ggplot2)
  MSEobj2 <- Sub(MSEobj_reb, MP=select.MPs)
  TabDF2 <- TabDF2 %>% filter(MP %in% select.MPs)
  cols <- rep("black", MSEobj2@nMPs)
  cols[TabDF2$Fease == "Yes"] <- '#008000'
  cols[TabDF2$Fease == "No"] <- '#ff2200'
  
  lab.size <- 4
  axis.title.size <- 16
  axis.text.size <- 12
  legend.title.size <- 12
  legend <- FALSE
  
  PMlist <- c('PB_4', 'LTY1')
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
  
  pdat <- left_join(d1, d2, by='MP')
  ggplot(pdat, aes(x=prob.x, y=prob.y, label=MP, color=MP)) + geom_point() + 
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
}
Fig_dim[[3]]<-function(dims) list(height=600,width=600)


# Projection Plot 
Fig_title[[4]] <- "Figure 4. Rebuilding Projection Plots"
Fig_text[[4]] <-  HTML(paste0(
  tags$p("Select up to four rows in Table 2 to plot projection plots
of biomass relative to BMSY and relative yield."),
  tags$p("Note: if only the first four selected rows will be plotted if more than
four rows are selected in Table 2."),
  tags$p("The blue regions represent the 90% and 50% probability intervals, the white solid 
               line is the median and the dark blue lines are two example simulations. 
               For reference the grey horizontal lines denote values of 0.25, 0.5, and 1.")
))
Figs[[4]] <-function(MSEobj,MSEobj_reb,options=list()){
  if (is.null(options$tab3.row.select)) {
    par(mfrow=c(1,1))
    plot(c(0,1), c(0,1), type='n', axes=FALSE, xlab="", ylab="")
    text(0.5, 0.5, "Select up to 4 rows in Table 2 to display projection plots")
  } else {
    
    tab.select <<- TabDF2$MP[options$tab3.row.select]
    select.MPs <- tab.select
    # check if url
    ind <- which(grepl("href",tab.select))
    if (length(ind)>0) {
      mp.names <- lapply(strsplit(tab.select[ind], "_blank'>"), "[[", 2) %>% unlist()
      select.MPs[ind] <- gsub('</a>', '', mp.names)
    }
    select.MPs <- select.MPs[1:4] %>% as.character()
    select.MPs <- select.MPs[!is.na(select.MPs)]
    nplot <<- length(select.MPs)
    TabDF2 <- TabDF2 %>% filter(MP %in% select.MPs)
    
    MSEobj2 <- Sub(MSEobj_reb, MPs=select.MPs)
    
    cols <- rep("black", MSEobj2@nMPs)
    cols[TabDF2$Fease == "Yes"] <- '#008000'
    cols[TabDF2$Fease == "No"] <- '#ff2200'
    
    Plan_proj(MSEobj2,cols=cols)
  }
  
  
} 
Fig_dim[[4]]<-function(dims)list(height=600,width=600)


Planning<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
               Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)



# ============= Evaluation =======================

Tabs <- Figs <- Tab_title <- Tab_text <- Fig_title <- Fig_text <- Fig_dim <- options <- Intro_title <- Intro_text <- new('list')
Fig_title <- Tab_title <- rep(list(""), 10)
#for(i in 1:10)Fig_dim[[i]]<-function(dims)list(height=1,width=1)
# These are the names of widgets and their values to display in this skin / mode
#             years in projection,  year resolution of reporting  rounding of digits
options<-list()

Intro_title[[1]] <- "Introduction"
Intro_text[[1]] <- "A single MP is projected to infer future stock status and determine whether the data observed are consistent with those that were projected"


# --- Tables --- 
Tab_title[[1]] <- "Table 1. Biomass relative to 50% BMSY"
Tab_text[[1]] <-"The biomass projection for the interim years that an MP has been in use."

Tabs[[1]]<-function(MSEobj_Eval,dat,dat_ind,options=list(res=1),res=5,rnd=1){
  
  YIU<-length(dat_ind@Year)-length(dat@Year)
  nMPs<-MSEobj_Eval@nMPs
  proyears<-MSEobj_Eval@proyears
  ind<-1:min(5,proyears)
  
  LRP<-matrix(round(apply(MSEobj_Eval@B_BMSY[,,1:YIU,drop=FALSE]>0.5,2:3,mean)*100,rnd)[,ind],nrow=nMPs)
  Tab1<-as.data.frame(cbind(MSEobj_Eval@MPs,LRP))
  
  colnams<-c("MP",Current_Year-((YIU-1):0))
  names(Tab1)<-colnams
  Tab1$MP<-as.character(Tab1$MP)
  
  URLs <- MPurl(as.character(Tab1$MP))
  MPwithurl <- !is.na(URLs) 
  Tab1$MP[MPwithurl] <- paste0("<a href='", URLs[MPwithurl]," ' target='_blank'>", Tab1$MP[MPwithurl],"</a>")
  
  
  Bdeps<-MSEobj_Eval@OM$D/MSEobj_Eval@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
  caption=paste0("Simulations start between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
  datatable(Tab1,caption=caption,extensions = 'Buttons',class = 'display',rownames=FALSE,escape=FALSE,
            options=list(buttons = 
                           list('copy', list(
                             extend = 'collection',
                             buttons = c('csv', 'excel', 'pdf'),
                             text = 'Download'
                           )),
                         dom = 'Brti', 
                         ordering=F
            )
  )%>%
    formatStyle(columns = 2:ncol(Tab1), valueColumns = 2:ncol(Tab1), color = styleInterval(c(50,100),c('red','orange','green')))
  
}

Tab_title[[2]] <- "Table 2. Biomass relative to BMSY"
Tab_text[[2]] <-"The biomass projection for the interim years that an MP has been in use."

Tabs[[2]]<-function(MSEobj_Eval, dat,dat_ind,options=list(burnin=10,res=1),rnd=1){
  
  YIU<-length(dat_ind@Year)-length(dat@Year)
  nMPs<-MSEobj_Eval@nMPs
  proyears<-MSEobj_Eval@proyears
  ind<-1:min(YIU,proyears)
  
  TRP<-matrix(round(apply(MSEobj_Eval@B_BMSY[,,ind,drop=FALSE]>1,2:3,mean)*100,rnd)[,ind],nrow=nMPs)
  Tab2<-as.data.frame(cbind(MSEobj_Eval@MPs,TRP))
  colnams<-c("MP",Current_Year-((YIU-1):0))
  names(Tab2)<-colnams
  Tab2$MP<-as.character(Tab2$MP)
  
  URLs <- sapply(Tab2$MP, MPurl) %>% unlist()
  MPwithurl <- !is.na(URLs) 
  Tab2$MP[MPwithurl] <- paste0("<a href='", URLs[MPwithurl]," ' target='_blank'>", Tab2$MP[MPwithurl],"</a>")
  
  Bdeps<-MSEobj_Eval@OM$D/MSEobj_Eval@OM$SSBMSY_SSB0 #MSEobj_reb@B_BMSY[,1,1]#
  caption=paste0("Simulations start between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% BMSY" )
  datatable(Tab2,caption=caption, extensions = 'Buttons',class = 'display',rownames=FALSE,escape=FALSE,
            options=list(buttons = 
                           list('copy', list(
                             extend = 'collection',
                             buttons = c('csv', 'excel', 'pdf'),
                             text = 'Download'
                           )),
                         dom = 'Brti', 
                         ordering=F
            )
  )%>%
    formatStyle(columns = 2:ncol(Tab2), valueColumns = 2:ncol(Tab2), color = styleInterval(c(25,50,100),c('red','orange','green','darkgreen')))
  
}

Tab_title[[3]] <- "Table 3. Spawning biomass relative to 20% of SSB unfished"
Tab_text[[3]] <-"Probability of biomass exceeding 20% unfished levels in the years since MP adoption."

Tabs[[3]]<-function(MSEobj_Eval,dat,dat_ind,options=list(burnin=10,res=1),rnd=1){
  
  YIU<-length(dat_ind@Year)-length(dat@Year)
  B_B0<-MSEobj_Eval@SSB/MSEobj_Eval@OM$SSB0
  nMPs<-MSEobj_Eval@nMPs
  proyears<-MSEobj_Eval@proyears
  ind<-1:min(YIU,proyears)
  RP<-matrix(round(apply(B_B0[,,ind,drop=F]>0.2,2:3,mean)*100,rnd),nrow=nMPs)
  Tab3<-as.data.frame(cbind(MSEobj_Eval@MPs,RP))
  colnams<-c("MP",Current_Year-((YIU-1):0))
  names(Tab3)<-colnams
  Tab3$MP<-as.character(Tab3$MP)
  
  URLs <- sapply(Tab3$MP, MPurl) %>% unlist()
  MPwithurl <- !is.na(URLs) 
  Tab3$MP[MPwithurl] <- paste0("<a href='", URLs[MPwithurl]," ' target='_blank'>", Tab3$MP[MPwithurl],"</a>")
  
  Bdeps<-MSEobj_Eval@OM$D#MSEobj_reb@B_BMSY[,1,1]#
  caption=paste0("Simulations start between ",round(min(Bdeps)*100,0), "% and ", round(max(Bdeps)*100,0), "% of unfished SSB" )
  datatable(Tab3,caption=caption,extensions = 'Buttons',class = 'display',rownames=FALSE,escape=FALSE,
            options=list(buttons = 
                           list('copy', list(
                             extend = 'collection',
                             buttons = c('csv', 'excel', 'pdf'),
                             text = 'Download'
                           )),
                         dom = 'Brti', 
                         ordering=F
            )
  )%>%
    formatStyle(columns = 2:ncol(Tab3), valueColumns = 2:ncol(Tab3), color = styleInterval(c(25,50,100),c('red','orange','green','darkgreen')))
  
}


# --- Figures ---

Fig_title[[2]] <- "Figure 1. Biomass projected since MP adoption"
Fig_text[[2]] <- "Projections of biomass relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points. The bold black vertical line is the current year." 

Figs[[2]]<-function(MSEobj_Eval,dat,dat_ind,options=list()) BMSYproj(MSEobj_Eval,MSEobj_Eval,options=list( YIU=length(dat_ind@Year)-length(dat@Year)),maxcol=1)
Fig_dim[[2]] <- function(dims)list(height=420,width=600)

Fig_title[[3]] <- "Figure 2. Biomass projected since MP adoption relative to unfished SSB"
Fig_text[[3]] <- "Projections of biomass relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations. Grey horizontal lines denote the target and limit reference points. The bold black vertical line is the current year." 

Figs[[3]]<-function(MSEobj_Eval,dat,dat_ind,options=list()) B0proj(MSEobj_Eval,MSEobj_Eval,options=list( YIU=length(dat_ind@Year)-length(dat@Year)),maxcol=1)
Fig_dim[[3]] <- function(dims)list(height=420,width=600)

Fig_title[[4]] <- "Figure 3. Posterior predicted data versus those observed"
Fig_text[[4]] <- "The 'cloud' of posterior predicted data are represented by the grey shaded areas that"

Figs[[4]]<-function(MSEobj_Eval,dat,dat_ind,options=list()){
  
  YIU=length(dat_ind@Year)-length(dat@Year)
  styr=max(dat@Year)-min(dat@Year)
  PPD<-MSEobj_Eval@Misc$Data[[1]]
  
  # Standardization
  predCat<-(PPD@Cat/PPD@Cat[,styr])[,styr+(1:YIU),drop=F]
  predInd<-(PPD@Ind/PPD@Ind[,styr])[,styr+(1:YIU),drop=F]
  predML<-(PPD@ML/PPD@ML[,styr])[,styr+(1:YIU),drop=F]
  
  # Standardization
  obsCat<-(dat_ind@Cat/dat_ind@Cat[,styr])[styr+(1:YIU)]
  obsInd<-(dat_ind@Ind/dat_ind@Ind[,styr])[styr+(1:YIU)]
  obsML<-(dat_ind@ML/dat_ind@ML[,styr])[styr+(1:YIU)]
  yrlab<-dat_ind@Year[styr+(1:YIU)]
  
  ppdplot<-function(pred,obs,yrlab,p=c(0.025,0.05,0.25,0.75,0.95,0.975),pcols=c("grey90","grey78","grey66"),lab="",pcex=1.3){
    
    qmat<-apply(pred,2,quantile,p)
    nobs<-length(obs)
    ylim<-range(pred,obs)
    plot(range(yrlab),ylim,col="white")
    yind<-c(1:nobs,nobs:1)
    rind<-nobs:1
    polygon(yrlab[yind],c(qmat[1,],qmat[6,rind]),col=pcols[1],border=pcols[1])
    polygon(yrlab[yind],c(qmat[2,],qmat[5,rind]),col=pcols[2],border=pcols[2])
    polygon(yrlab[yind],c(qmat[3,],qmat[4,rind]),col=pcols[3],border=pcols[3])
    
    #obs<-qmat[cbind(1:nobs,1:nobs)]-0.02
    ocol<-rep("black",nobs)
    ocol[obs<qmat[2,]|obs>qmat[5,]]<-"orange"
    ocol[obs<qmat[1,]|obs>qmat[6,]]<-"red"
    
    points(yrlab,obs,col=ocol,pch=19,cex=pcex)
    
    #points(yrlab,obs,pch=1,cex=pcex)
    
    mtext(lab,3,line=0.6,font=2)
    
  }
  
  par(mfrow=c(1,3),mai=c(0.3,0.3,0.2,0.01),omi=c(0.5,0.5,0.05,0.05))
  ppdplot(pred=predCat,obs=obsCat,yrlab,lab="Catch")
  ppdplot(pred=predML,obs=obsML,yrlab,lab="Mean Length in Catch")
  ppdplot(pred=predInd,obs=obsInd,yrlab,lab="Index of Abundance")
  mtext("Year",1,line=1.5,outer=T)
  mtext(paste("Data relative to",yrlab[1]-1),2,line=1.5,outer=T)
  
  legend('topleft',legend=c("95% PI","90% PI","50% PI"),fill=c("grey90","grey78","grey66"),title="Pred. Data")
  legend('topright',legend=c("Consistent","Borderline","Inconsistent"),pch=19,col=c("black","orange","red"),title="Obs. Data",text.col=c("black","orange","red"))
  
} 
Fig_dim[[4]] <- function(dims)list(height=400,width=800)


Evaluation<-list(Tabs=Tabs, Figs=Figs, Tab_title=Tab_title, Tab_text=Tab_text, Fig_title=Fig_title, 
                 Fig_text=Fig_text, Fig_dim=Fig_dim, Intro_title=Intro_title, Intro_text=Intro_text, options=options)



Generic <- list(Risk_Assessment=Risk_Assessment,SD=SD,Planning=Planning,Evaluation=Evaluation) 




