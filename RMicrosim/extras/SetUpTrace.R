
# set up
number.cycles<-60
# 1 month cycles, 5 years

# set up:
# list of dataframes
# each item in the list is a dataframe
# each dataframe corresponds to an individual under a treatment option
# e.g. one dataframe for first individual with no FLS,
# another with the first individual with FLS,
# and so on

m.TR <- rbind(
    data.frame(cycle= rep(1:(number.cycles+1),
                          times=nrow(microsim_pop)),
               month=seq(0,60,1),
               year= c(0, rep(1,12), rep(2,12),rep(3,12), rep(4,12), rep(5,12)),
               id= rep(1:nrow(microsim_pop), each=(number.cycles+1)),# individual id
               intervention="no FLS",
               # states
               "s_hf"=NA,  # hip fracture
               "s_sf"=NA,  # spine fracture
               "s_of"=NA,  # other fracture
               "s_ff"=NA,  # fracture free
               "s_d"=NA,   # death
               # currently in a fracture state?
               c_af=NA,
               # history tracker
               h_hf=NA,    # history of hip fracture (how many experienced)
               h_sf=NA,    # history of spine fracture (how many experienced)
               h_of=NA,    # history of other fracture (how many experienced)
               h_af=NA,    # history of any fracture (how many experienced),
               recent.fx=NA, #most recent fx
               identified=NA,
               tr.onset=NA,
               treat=NA,
               medication=NA,
               monitored=NA,
               adhering=NA,
               time_med=NA,
               lag.passed=NA, # whether time on treatment is over lag
               apply.rr=NA, # was relative risk applied
               hospitalised=NA, # currently hospitalised
               procedure=NA, # procedure in hospital
               temp.rehab=NA,
               location=NA,
               lab.test=NA,
               dxa=NA

    ),
    data.frame(cycle= rep(1:(number.cycles+1), times=nrow(microsim_pop)),
               month=seq(0,60,1),
               year= c(0, rep(1,12), rep(2,12),rep(3,12), rep(4,12), rep(5,12)),
               id= rep(1:nrow(microsim_pop), each=(number.cycles+1)), # individual id
               intervention="FLS",
               "s_hf"=NA,  # hip fracture
               "s_sf"=NA,  # spine fracture
               "s_of"=NA,  # other fracture
               "s_ff"=NA,  # fracture free
               "s_d"=NA, # death
               # currently in a fracture state?
               c_af=NA,
               # history tracker
               h_hf=NA,    # history of hip fracture (how many experienced)
               h_sf=NA,    # history of spine fracture (how many experienced)
               h_of=NA,    # history of other fracture (how many experienced)
               h_af=NA,    # history of any fracture  (how many experienced)
               recent.fx=NA, #most recent fx
               identified=NA,
               tr.onset=NA,
               treat=NA,
               medication=NA,
               monitored=NA,
               adhering=NA,
               time_med=NA,
               lag.passed=NA, # whether time on treatment is over lag
               apply.rr=NA, # was relative risk applied
               hospitalised=NA, # currently hospitalised
               procedure=NA, # procedure in hospital
               temp.rehab=NA,
               location=NA,
               lab.test=NA,
               dxa=NA
    ))
  # so we have each individual twice
  # once for no FLS
  # another without FLS

# add individual's characteristics
m.TR<- m.TR %>%
    left_join(microsim_pop,
              by="id")
# drop if missing index fx (study pop=0 )
m.TR<- m.TR %>%
    filter(!is.na(index_fx))
# add working age
m.TR$working.age<-m.TR$age+m.TR$year