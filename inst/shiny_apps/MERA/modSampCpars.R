SampleCpars_mod <- function(cpars, nsim=48, msg=TRUE) {
  
  # Vector of valid names for custompars list or data.frame. Names not in this list will be printed out in warning and ignored #	
  ParsNames <- validcpars(FALSE)
  
  sampCpars <- list()
  ncparsim<-cparscheck(cpars)
  Names <- names(cpars)
  # report invalid names 
  invalid <- which(!Names %in% ParsNames)
  if (length(invalid) > 0) {
    outNames <- paste(Names[invalid], "")
    for (i in seq(5, by=5, length.out=floor(length(outNames)/5))) outNames <- gsub(outNames[i], paste0(outNames[i], "\n"), outNames)
    if(msg) message("ignoring invalid names found in custom parameters (OM@cpars) \n", outNames)	
  }
  # report found names
  valid <- which(Names %in% ParsNames)
  cpars <- cpars[valid]
  if (length(valid) == 0) stop("No valid names found in custompars (OM@cpars)", call.=FALSE)
  Names <- names(cpars)
  outNames <- paste(Names, "")
  for (i in seq(5, by=5, length.out=floor(length(outNames)/5)))
    outNames <- gsub(outNames[i], paste0(outNames[i], "\n"), outNames)
  if(msg) message("valid custom parameters (OM@cpars) found: \n", outNames)
  
  # Sample custom pars 
  if (ncparsim < nsim) ind <- sample(1:ncparsim, nsim, replace=TRUE)
  if (ncparsim == nsim) {
    ind <- 1:nsim
  } else {
    ind <- sample(1:ncparsim, nsim, replace=FALSE)
  }
  # if (!ncparsim < nsim) ind <- sample(1:ncparsim, nsim, replace=FALSE)
  
  for (i in 1:length(cpars)) {
    samps <- cpars[[i]]
    name <- names(cpars)[i]
    if (any(c("EffUpper", "EffLower", "EffYears", "maxage", "M_at_Length", "CAL_binsmid", "CAL_bins") %in% name)) {
      sampCpars[[name]] <- samps
    } else {
      if (class(samps) == "numeric" | class(samps) == "integer") sampCpars[[name]] <- samps[ind]
      
      if (class(samps) == "matrix") sampCpars[[name]] <- samps[ind,, drop=FALSE] 
      
      if (class(samps) == "array") {
        if (length(dim(samps)) == 3)  sampCpars[[name]] <- samps[ind, , ,drop=FALSE]
        if (length(dim(samps)) == 4)  sampCpars[[name]] <- samps[ind, , , ,drop=FALSE]
      }
      if (class(samps) == "data.frame")   sampCpars[[name]] <- samps 
    }
  }
  
  
  ### modified version - DLMtool > V5.1.3 use this version 
  
  sampCpars
}