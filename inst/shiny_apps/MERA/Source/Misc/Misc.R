AM<-function(newtext){
  #updateTextAreaInput(session, "Log", value=paste(newtext,input$Log, sep = "\n"))
  Log_text$text<-paste(newtext, Log_text$text, sep = "\n")
  
}

doprogress<-function(message,duration=1,n=20){
  withProgress(message = message, value = 0, {
    inc<-duration/n
    for (i in 1:n) {
      incProgress(1/n, detail = round(i*(100/n)))
      Sys.sleep(inc)
    }
  })
}

MSCsave_auto<-function(){

  MSClog<-list(PanelState, Just, Des)
  saveRDS(MSClog,file=paste0(USERID,"_autosave.mera"))

}

namconv<-function(nam){
  nam<-gsub(" ","_",nam)
  nam<-gsub("[.]","",nam)
  nam<-gsub(",","",nam)
  substr(nam,1,15)[[1]]
}

getAllMPs<-function(){
  MPs<-avail('MP')
  cond<-grepl("MLL",MPs)|grepl('ML',MPs)
  MPs<-MPs[!cond]
  MPs<-MPs[!MPs%in%c("YPR","YPR_CC","YPR_ML","LBSPR")]
  #MPs<-MPs[!MPs%in%c("DDSS_4010","DDSS_MSY","SPSS_4010","SPSS_MSY","SCA_MSY","SCA_4010")]
  MPs[order(MPs)]
  MPs
}

getMPs<-function(All=FALSE){

  MPs<-input$ManPlanMPsel
  

  MPs[order(MPs)]

}


LowSlopes<-function(OMin, except = NULL) {

  nms <- slotNames(OMin)
  # exceptions
  if (is.null(except)) except <- "EVERYTHING"
  exclude <- unique(grep(paste(except, collapse = "|"), nms, value = FALSE))

  vars <- c("inc","sd")
  ind <- unique(grep(paste(vars, collapse = "|"), nms, value = FALSE))
  ind <- ind[(!(nms[ind] %in% exclude))]
  for (X in seq_along(ind)) {
    slot(OMin, nms[ind[X]]) <- c(0, 1e-10)
  }

  return(OMin)

}

Data_parse<-function(file){

  parsed<-strsplit(file,"/")[[1]]
  out<-list()
  out$dir<-paste(parsed[1:(length(parsed)-1)],collapse="/")
  out$name<-parsed[length(parsed)]
  out

}
