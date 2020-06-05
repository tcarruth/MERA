# Tooltips

ntips<<-10
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

ids[[8]]<-"Distribution"
titles[[8]]<-"In Step 1: Characterize fishery system, the user answers questions relating to parameters of the operating models.
               These are presented as a range of values. Here the user can select the distribution used to sample those parameter
               values in the range specified"

ids[[9]]<-"IQRange"
titles[[9]]<-"If the user selected a non-uniform distribution for sampling operating model parameters, they can also specify the quantiles 
              that correspond to upper and lower bounds in the questionnaire. Selecting a value of 90% will sample values between the 5th and 95th 
              percentiles of the distribution"

ids[[10]]<-"Parallel"
titles[[10]]<-"The user can opt to use parallel processing for operating models with more than 48 simulations which can dramatically shorten 
               the time taken for calculations"


