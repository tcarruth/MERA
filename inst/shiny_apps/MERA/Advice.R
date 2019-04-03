

Calc_Advice<-function(Advice_MPs=NA,updateres=FALSE){

  if(is.na(Advice_MPs[1])){ # if not user specified

    #if(Calc()==1&App()==1){  # if evaluation and applications are available
      #if(input$Advice_allMPs=="Custom MPs"){
       # Advice_MPs<-c(input$Advice_MP1,input$Advice_MP2,input$Advice_MP3)
      #}else if(input$Advice_allMPs=="Application MP"){
       # Advice_MPs<-as.character(Ptab2_app$MP[1])
      #}else if(input$Advice_allMPs=="Evaluation MPs"){
      #  Advice_MPs<-as.character(Ptab2$MP)
      #}else{
      #  Advice_MPs<-getMPs(All=TRUE)
     # }
    #}else if(Calc()==1){
      #if(input$Advice_nA=="Custom MPs"){
       # Advice_MPs<-c(input$Advice_MP1,input$Advice_MP2,input$Advice_MP3)
      #}else if(input$Advice_nA=="Evaluation MPs"){
       # Advice_MPs<-as.character(Ptab2$MP)
      #}else{
      #  Advice_MPs<-getMPs(All=TRUE)
     # }
    #}else if(App()==1){
      #if(input$Advice_nE=="Custom MPs"){
       # Advice_MPs<-c(input$Advice_MP1,input$Advice_MP2,input$Advice_MP3)
      #}else if(input$Advice_nE=="Application MP"){
       # Advice_MPs<-as.character(Ptab2_app$MP[1])
      #}else{
      #  Advice_MPs<-getMPs(All=TRUE)
     # }
    #}else{
      #if(input$Advice_nEA=="Custom MPs"){
       # Advice_MPs<-c(input$Advice_MP1,input$Advice_MP2,input$Advice_MP3)
      #}else{

     # }
    #}
    if(input$Advice_nE=="Custom MPs"){
      Advice_MPs<-c(input$Advice_MP1,input$Advice_MP2,input$Advice_MP3)
    }else{
      Advice_MPs<-getMPs(All=TRUE)
    }
    Advice_MPs
  }

  withProgress(message = "Calculating management advice",value=0, {
    out<-runMP_MSC(Data=dat,MPs=Advice_MPs)
    output$Advice <- DT::renderDataTable(out[[1]],options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
    output$Advice_TAC<-renderPlot(plotTAC(out[[2]]),height =900 ,width=900)
  })

  if(updateres)updateTabsetPanel(session,"Res_Tab",selected="4")

  AdCalc(1)

}


runMP_MSC <- function(Data, MPs = NA, reps = 100, perc=0.5, chkMPs=TRUE, silent=TRUE,shiny=FALSE) {
  if (class(MPs) != 'character' && !all(is.na(MPs))) stop('MPs must be character string', call.=FALSE)
  if (class(Data) != 'Data') stop("Data must be class 'Data'", call.=FALSE)
  if (all(is.na(MPs))) {
    MPs <- avail("MP")
    if (!silent) message("running all available MPs")
  }
  if (chkMPs) {
    cans <- Can(Data, MPs=MPs)
    MPs <- MPs[MPs %in% cans]
  }
  if (length(MPs) <1) stop("No MPs possible")

  MPrecs <- applyMP_MSC(Data, MPs, reps, nsims=1, silent=silent,shiny=shiny)

  names <- c("TAC", "Effort", "LR5", "LFR", "HS", "Rmaxlen",
             "L5", "LFS", 'Vmaxlen', 'Spatial')
  mat <- matrix(0, nrow=length(MPs), ncol=length(names)+Data@nareas-1)
  for (x in seq_along(names)) {
    temp <- lapply(MPrecs[[1]], '[[', names[x])
    if (names[x]!="Spatial") {
      mat[,x] <- unlist(lapply(temp, quantile, probs=perc, na.rm=TRUE))
    } else {
      mat[,x:ncol(mat)] <- t(matrix(unlist(temp), nrow=Data@nareas, ncol=length(MPs)))
    }
  }

  rownames(mat) <- MPs
  names[length(names)] <- "Area 1"
  names <- c(names, paste('Area', 2:Data@nareas))
  colnames(mat) <- names

  if (nrow(mat) > 1) {
    allNA <- colSums(apply(mat, 2, is.na)) == length(MPs)
    matout <- data.frame(round(mat[,!allNA], 2), stringsAsFactors = FALSE)
    names(matout) <- names[!allNA]
  }

  if (nrow(mat) == 1) {
    mat <- data.frame(mat)
    matout <- mat[!is.na(mat)]
    matout <- matrix(matout, nrow=nrow(mat))
    colnames(matout) <- names[!is.na(mat)]
    rownames(matout) <- MPs
  }

  #matout[is.na(matout)]<-"-"

  list(matout=matout,Data=MPrecs[[2]])

}

#' Apply Management Procedures to an object of class Data
#'
#' @param Data An object of class Data
#' @param MPs Name(s) of the MPs to run
#' @param reps Number of samples
#' @param nsims Optional. Number of simulations.
#' @param silent Logical. Should messages be suppressed?
#'
#' @return A list with the first element a list of management recommendations,
#' and the second the updated Data object
#' @export
#'
applyMP_MSC <- function(Data, MPs = NA, reps = 100, nsims=NA, silent=FALSE,shiny=F) {
  if (class(Data) != "Data") stop("First argument must be object of class 'Data'", call.=FALSE)
  Data <- updateMSE(Data)
  if (is.na(nsims)) nsims <- length(Data@Mort)
  nMPs <- length(MPs)

  incrate<-1/nMPs


  if (.hasSlot(Data, "nareas")) {
    nareas <- Data@nareas
  } else {
    nareas <- 2
  }
  returnList <- list() # a list nMPs long containing MPs recommendations
  recList <- list() # a list containing nsim recommendations from a single MP
  TACout <- array(NA, dim=c(nMPs, reps, nsims))
  # if (!sfIsRunning() | (nMPs < 8 & nsims < 8)) {
  for (mp in 1:nMPs) {
    if(shiny)incProgress(incrate)
    temp <- lapply(1:nsims, MPs[mp], Data = Data, reps = reps)
    slots <- slotNames(temp[[1]])
    for (X in slots) { # sequence along recommendation slots
      if (X == "Misc") { # convert to a list nsim by nareas
        rec <- lapply(temp, slot, name=X)
      } else {
        rec <- do.call("cbind", lapply(temp, slot, name=X)) # unlist(lapply(temp, slot, name=X))
      }
      if (X == "Spatial") { # convert to a matrix nsim by nareas
        rec <- matrix(rec, nareas, nsims, byrow=FALSE)
      }
      recList[[X]] <- rec
      for (x in 1:nsims) Data@Misc[[x]] <- recList$Misc[[x]]
      recList$Misc <- NULL
    }
    if (length(recList$TAC)>0)  TACout[mp,,] <- recList$TAC
    returnList[[mp]] <- recList
    if (!silent && any(apply(is.na(recList$TAC), 2, sum) > rep(0.5 * reps, nsims)))
      message("Method ", MPs[mp], " produced greater than 50% NA values")
  }

  Data@TAC <- TACout
  Data@MPs <- MPs

  list(returnList, Data)
}



plotTAC <- function(x, upq=0.9, lwq=0.1, outline = FALSE, ...) {
  Data <- updateMSE(x)
  if (class(Data) != "Data")  stop("Object must be of class 'Data'")
  tacs <- t(Data@TAC[, , 1])

  if (all(is.na(tacs))) {
    message("Nothing found in TAC slot")
    return(invisible(NULL))
  }
  units <- TRUE
  if (length(nchar(x@Units)) < 1) units <- FALSE
  MPs <- Data@MPs
  ind <- grep("ref", MPs)
  if (length(ind) > 0) {
    tacs <- tacs[, -ind, drop=FALSE]
    MPs <- MPs[-ind]
  }

  # exclude NAs
  nMPs <- dim(Data@TAC)[1]

  if (nMPs>1){
    allNAs <- colSums(apply(tacs, 2, is.na)) == nrow(tacs)
    tacs <- tacs[,!allNAs, drop=FALSE]
    MPs <- MPs[!allNAs]
    nMPs<-length(MPs)
  }

  if (nMPs>1) {
    cols <- rainbow(30)
    ord <- order(apply(tacs, 2, median, na.rm = TRUE))
    MPs <- MPs[ord]
    tacs <- tacs[, ord]
    ymax <- max(apply(tacs, 2, quantile, upq, na.rm = TRUE))
    ymin <- min(apply(tacs, 2, quantile, lwq, na.rm = TRUE))
    ylim <- c(ymin, ymax)
    Median <- round(apply(tacs, 2, median, na.rm = TRUE), 2)
    SD <- round(apply(tacs, 2, sd, na.rm = TRUE), 2)
  } else {
    ylim <- c(min(tacs), max(tacs))
    Median <- median(tacs)
    SD <- sd(tacs)
    tacs <- as.numeric(tacs)
    cols <- "darkgray"
  }

  par(mfrow = c(1, 1), oma = c(2, 4, 1, 0), mar = c(3, 3, 0, 0))
  if (nMPs>1) {
    boxplot(tacs, names = MPs, las = 1, col = cols, outline = outline,
            frame = FALSE, ylim = ylim, horizontal = TRUE, ...)
    if (units) mtext(paste("TAC (", Data@Units, ")", sep = ""), side = 1, outer = T,
                     line = 0.5, cex = 1.25)
    if (!units) mtext("TAC (no units supplied)", side = 1, outer = T,
                      line = 0.5, cex = 1.25)
    mtext(side = 2, "Management Procedures", outer = TRUE, line = 3, cex = 1.25)
  } else {
    boxplot(tacs, names = MPs, las = 1, col = cols, outline = outline,
            frame = FALSE, ylim = ylim, horizontal = FALSE, ...)
    if (units) mtext(paste("TAC (", Data@Units, ")", sep = ""), side = 2, outer = T,
                     line = 0.5, cex = 1.25)
    if (!units) mtext("TAC (no units supplied)", side = 2, outer = T,
                      line = 0.5, cex = 1.25)
    mtext(side = 3, MPs, outer = TRUE, line=-1, cex = 1.25, xpd=NA)
  }

  if (units) data.frame(MP = MPs, Median = Median, SD = SD, Units = Data@Units)
  if (!units) data.frame(MP = MPs, Median = Median, SD = SD)

}

