# Tooltips

ntips<<-7
ids<-list()
titles<-list()
placements<-as.list(rep('bottom',ntips))
triggers<-as.list(rep('hover',ntips))

ids[[1]]<-"interval"
titles[[1]]<-"The number of years between management implementations (e.g. between TACs are set)"

ids[[2]]<-"Save"
titles[[2]]<-"Save just the MERA questionnaire (small)"

ids[[3]]<-"Save_session"
titles[[3]]<-"Save a previous session including calculated results (large)"

ids[[4]]<-"Save_OM"
titles[[4]]<-"Export just the operating model created by this MERA session"

ids[[5]]<-"plusgroup"
titles[[5]]="For computational efficiency, MERA assumes a plus group age that is the smaller of
                               either the age specified here or the age at 10 per cent cumulative unfished survival"

ids[[6]]<-"Dep_reb"
titles[[6]]= "The Management Planning mode runs a second projection starting at a user-specified level of depletion to evaluate
the abilty of management procedures to rebuild from depleted levels."

ids[[7]]<-"Start"
titles[[7]]= "Start a new MERA session"
