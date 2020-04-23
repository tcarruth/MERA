# Tooltips

ntips<<-4
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

