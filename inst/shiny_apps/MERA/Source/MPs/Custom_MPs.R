
#DDSS_4010 <<- make_MP(DD_SS, HCR40_10)
#DDSS_MSY <<- make_MP(DD_SS, HCR_MSY)
SPSS_4010 <<- make_MP(SP_SS, HCR40_10)
SPSS_MSY <<- make_MP(SP_SS, HCR_MSY)
#SCA_4010 <- make_MP(SCA, HCR40_10)
#SCA_MSY <- make_MP(SCA, HCR_MSY)

curC<-function (x, Data, reps = 100, plot = FALSE){
  #curyr<-length(Data@Cat[x,]) # latest year of data
  TAC<-mean(Data@Cat[x,Data@LHYear-(0:4)],na.rm=T)
  Rec <- new("Rec") # make a new blank recommendation object
  Rec@TAC <- TAC    # Fill the TAC slot
  Rec
}
class(curC)<-'MP'
curC<<-curC

curC75<-function (x, Data, reps = 100, plot = FALSE){
  #curyr<-length(Data@Cat[x,]) # latest year of data
  TAC<-mean(Data@Cat[x,Data@LHYear-(0:4)],na.rm=T)*0.75
  Rec <- new("Rec") # make a new blank recommendation object
  Rec@TAC <- TAC    # Fill the TAC slot
  Rec
}
class(curC75)<-'MP'
curC75<<-curC75
