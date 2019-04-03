
Fease2 <- function(Data=NULL, TAC=TRUE, TAE=TRUE, SL=TRUE, Spatial=TRUE, names.only=TRUE, msg=TRUE, include.ref=FALSE) {
  if (msg) {
    message("Feasible management: ")
    if (TAC) message("TAC - total allowable catch")
    if (TAE) message("TAE - total allowable effort")
    if (SL) message("SL - size selectivity")
    if (Spatial) message("Spatial - spatial closures")
  }
  if (!(TAC | TAE | SL | Spatial)) stop("No feasible management options!", call.=FALSE)
  MPs <- avail('MP')
  if (class(Data) == "Data") {
    if (msg) message("Data object provided. Returning feasible and available MPs")
    canMPs <- Can(Data)
  } else {
    if (msg) message("No Data object provided. Returning feasible MPs")
    canMPs <- MPs
  }
  mptypes <- MPtype2(MPs)
  mprecs <- mptypes[,3]
  isfease <- rep(TRUE, length(MPs))
  isfease[17]
  cbind(MPs, mprecs)

  if (!TAC) isfease[grepl("TAC", mprecs)] <- FALSE
  if (!TAE) isfease[grepl("TAE", mprecs)] <- FALSE
  if (!SL) isfease[grepl("SL", mprecs)] <- FALSE
  if (!Spatial) isfease[grepl("Spatial", mprecs)] <- FALSE


  df <- data.frame(MP=mptypes[,1], Can=mptypes[,1]%in%canMPs, Fease=isfease, stringsAsFactors = FALSE)
  df <- df[order(df$MP),]
  if (!include.ref)df <- df[mptypes[,2] != "Reference",]

  if (names.only) {
    return(df$MP[df$Can & df$Fease])
  } else {
    return(df)
  }
}

MPtype2 <- function(MPs=NA) {
  if (any(is.na(MPs))) MPs <- avail("MP")

  Data <- DLMtool::SimulatedData

  runMPs <- applyMP(Data, MPs, reps = 2, nsims=1, silent=TRUE)
  recs <- runMPs[[1]]

  type <- rep("NA", length(MPs))
  rec <- rep("", length(MPs))
  rectypes <- c("TAE", "Spatial", "SL")
  for (mm in seq_along(recs)) {
    Effort <- Spatial <- Selectivity <- FALSE
    output <- length(recs[[mm]]$TAC) > 0
    names <- names(recs[[mm]])
    names <- names[!names %in% c("TAC", "Spatial")]
    input <- sum(unlist(lapply(Map(function(x) recs[[mm]][[x]], names), length))) > 0
    if (all(!is.na(recs[[mm]]$Spatial))) input <- TRUE
    if (output) {
      type[mm] <- "Output"
      thisrec <- "TAC"
    }
    if (input) {
      # what recommentations have been made?
      if (any(is.finite(recs[[mm]]$Effort))) Effort <- TRUE
      if (any(is.finite(recs[[mm]]$Spatial))) Spatial <- TRUE
      if (any(is.finite(recs[[mm]]$LR5)) | any(is.finite(recs[[mm]]$LFR)) | any(is.finite(recs[[mm]]$HS)) |
          any(is.finite(recs[[mm]]$Rmaxlen)) | any(is.finite(recs[[mm]]$L5)) | any(is.finite(recs[[mm]]$LFS)) |
          any(is.finite(recs[[mm]]$Vmaxlen))) Selectivity <- TRUE

      dorecs <- rectypes[c(Effort, Spatial, Selectivity)]
      thisrec <- dorecs
      type[mm] <- "Input"

    }
    if (input & output) {
      type[mm] <- "Mixed"
      thisrec <- c("TAC", thisrec)
    }
    if (length(thisrec)>1)  {
      rec[mm] <- paste(thisrec, collapse=", ")
    } else {
      rec[mm] <- thisrec
    }
  }
  type[grep("ref", MPs)] <- "Reference"

  df <- data.frame(MP=MPs, Type=type, Recs=rec, stringsAsFactors = FALSE)
  df[order(df$Type),]

}


