# Tooltips

ntips<<-17
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

ids[[11]]<-"nsim"
titles[[11]]<-"The number of simulations (individual unique fishery instances) used for MSE analyses. In each simulation a unique sample of operating model parameters is 
#sampled based on the ranges specified in the MERA questionnaires. If operating models are conditioned on data, an individual model fit is done on each simulation.
#Non-converged simulations are dropped."

ids[[12]]<-"OM_C"
titles[[12]]<-"Should MSE analyses use the conditioned operating model or just use that determined by only the Questionnaire?"

ids[[13]]<-"C_eq_val"
titles[[13]]<-"The equilibrium catches that were taken on average, prior to the first year that catch data are provided. This provides a way to 
               condition a stock that is depleted prior to the first year that data are available."

ids[[14]]<-"C_eq"
titles[[14]]<-"Use the equilibrium catches in operating model conditioning."

ids[[15]]<-"ESS"
titles[[15]]<-"The maximum effective sample size (ESS) of annual age and length composition data. If three years of length composition data were provided 
               that had 90, 170 and 20 samples in each, an ESS value of 100 would affect only the second value and lead to a vector: [90, 100, 20]."

ids[[16]]<-"Wt_comp"
titles[[16]]<-"A weighting factor for the negative log-likelihood component for age and length composition data. A weighting of 1 is no adjustment. If three years of length composition data were provided 
               that had 90, 170 and 20 samples in each, a multiplier of 0.1 would lead to totals of 9, 17 and 2, respectively."

ids[[17]]<-"max_F"
titles[[17]]<-"The maximum apical (most selected age class), fishing mortality rate allowed in operating model conditioning. For example, a value of 3 corresponds to a 
               maximum harvest rate of 95 per cent for the most selected age class."




