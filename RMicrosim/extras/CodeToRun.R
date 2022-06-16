

# functions -----
microsim.files<-list.files(here("RMicrosim","R"))
for (i in 1:length(microsim.files)) {
  source(here("RMicrosim","R", microsim.files[i]))
} 

# run app -----
#shiny::runApp(here("RShiny"))
# working folder for inputs/ outputs ------
inputs.folder<-here("inputs",country)
# with required RDATA file
# generated from app
# or created locally (to do!)


results.folder<-here("results",country)
# this is where we will save the model results after running

# load input data -----
load(paste0(inputs.folder, "/run_model.inputs.RData"))

print(paste0("Country: ", user_inputs$country))
print(paste0("Sims: ", n_microsimulation))
print(study_pop_n)

# add treatment rate by risk tier -----

user_inputs$no.fls.trt.spine.male.low.risk <-
  if_else(user_inputs$no.fls.trt.spine.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.spine.male>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$no.fls.trt.spine.male- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$no.fls.trt.spine.male.intermediate.risk <-
  if_else(user_inputs$no.fls.trt.spine.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.spine.male>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$no.fls.trt.spine.male-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$no.fls.trt.spine.male.high.risk<-
  if_else(user_inputs$no.fls.trt.spine.male<=user_inputs$prop.fx.low.risk,
        user_inputs$no.fls.trt.spine.male/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$no.fls.trt.spine.male>=(1-user_inputs$prop.fx.high.risk),1,
                1))

user_inputs$fls.trt.spine.male.low.risk <-
  if_else(user_inputs$fls.trt.spine.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.spine.male>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$fls.trt.spine.male- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$fls.trt.spine.male.intermediate.risk <-
  if_else(user_inputs$fls.trt.spine.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.spine.male>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$fls.trt.spine.male-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$fls.trt.spine.male.high.risk<-
  if_else(user_inputs$fls.trt.spine.male<=user_inputs$prop.fx.low.risk,
        user_inputs$fls.trt.spine.male/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$fls.trt.spine.male>=(1-user_inputs$prop.fx.high.risk),1,
                1))


user_inputs$no.fls.trt.hip.male.low.risk <-
  if_else(user_inputs$no.fls.trt.hip.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.hip.male>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$no.fls.trt.hip.male- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$no.fls.trt.hip.male.intermediate.risk <-
  if_else(user_inputs$no.fls.trt.hip.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.hip.male>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$no.fls.trt.hip.male-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$no.fls.trt.hip.male.high.risk<-
  if_else(user_inputs$no.fls.trt.hip.male<=user_inputs$prop.fx.low.risk,
        user_inputs$no.fls.trt.hip.male/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$no.fls.trt.hip.male>=(1-user_inputs$prop.fx.high.risk),1,
                1))

user_inputs$fls.trt.hip.male.low.risk <-
  if_else(user_inputs$fls.trt.hip.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.hip.male>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$fls.trt.hip.male- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$fls.trt.hip.male.intermediate.risk <-
  if_else(user_inputs$fls.trt.hip.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.hip.male>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$fls.trt.hip.male-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$fls.trt.hip.male.high.risk<-
  if_else(user_inputs$fls.trt.hip.male<=user_inputs$prop.fx.low.risk,
        user_inputs$fls.trt.hip.male/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$fls.trt.hip.male>=(1-user_inputs$prop.fx.high.risk),1,
                1))



user_inputs$no.fls.trt.other.male.low.risk <-
  if_else(user_inputs$no.fls.trt.other.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.other.male>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$no.fls.trt.other.male- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$no.fls.trt.other.male.intermediate.risk <-
  if_else(user_inputs$no.fls.trt.other.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.other.male>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$no.fls.trt.other.male-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$no.fls.trt.other.male.high.risk<-
  if_else(user_inputs$no.fls.trt.other.male<=user_inputs$prop.fx.low.risk,
        user_inputs$no.fls.trt.other.male/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$no.fls.trt.other.male>=(1-user_inputs$prop.fx.high.risk),1,
                1))

user_inputs$fls.trt.other.male.low.risk <-
  if_else(user_inputs$fls.trt.other.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.other.male>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$fls.trt.other.male- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$fls.trt.other.male.intermediate.risk <-
  if_else(user_inputs$fls.trt.other.male<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.other.male>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$fls.trt.other.male-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$fls.trt.other.male.high.risk<-
  if_else(user_inputs$fls.trt.other.male<=user_inputs$prop.fx.low.risk,
        user_inputs$fls.trt.other.male/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$fls.trt.other.male>=(1-user_inputs$prop.fx.high.risk),1,
                1))






user_inputs$no.fls.trt.spine.female.low.risk <-
  if_else(user_inputs$no.fls.trt.spine.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.spine.female>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$no.fls.trt.spine.female- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$no.fls.trt.spine.female.intermediate.risk <-
  if_else(user_inputs$no.fls.trt.spine.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.spine.female>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$no.fls.trt.spine.female-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$no.fls.trt.spine.female.high.risk<-
  if_else(user_inputs$no.fls.trt.spine.female<=user_inputs$prop.fx.low.risk,
        user_inputs$no.fls.trt.spine.female/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$no.fls.trt.spine.female>=(1-user_inputs$prop.fx.high.risk),1,
                1))

user_inputs$fls.trt.spine.female.low.risk <-
  if_else(user_inputs$fls.trt.spine.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.spine.female>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$fls.trt.spine.female- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$fls.trt.spine.female.intermediate.risk <-
  if_else(user_inputs$fls.trt.spine.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.spine.female>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$fls.trt.spine.female-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$fls.trt.spine.female.high.risk<-
  if_else(user_inputs$fls.trt.spine.female<=user_inputs$prop.fx.low.risk,
        user_inputs$fls.trt.spine.female/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$fls.trt.spine.female>=(1-user_inputs$prop.fx.high.risk),1,
                1))


user_inputs$no.fls.trt.hip.female.low.risk <-
  if_else(user_inputs$no.fls.trt.hip.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.hip.female>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$no.fls.trt.hip.female- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$no.fls.trt.hip.female.intermediate.risk <-
  if_else(user_inputs$no.fls.trt.hip.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.hip.female>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$no.fls.trt.hip.female-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$no.fls.trt.hip.female.high.risk<-
  if_else(user_inputs$no.fls.trt.hip.female<=user_inputs$prop.fx.low.risk,
        user_inputs$no.fls.trt.hip.female/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$no.fls.trt.hip.female>=(1-user_inputs$prop.fx.high.risk),1,
                1))

user_inputs$fls.trt.hip.female.low.risk <-
  if_else(user_inputs$fls.trt.hip.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.hip.female>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$fls.trt.hip.female- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$fls.trt.hip.female.intermediate.risk <-
  if_else(user_inputs$fls.trt.hip.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.hip.female>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$fls.trt.hip.female-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$fls.trt.hip.female.high.risk<-
  if_else(user_inputs$fls.trt.hip.female<=user_inputs$prop.fx.low.risk,
        user_inputs$fls.trt.hip.female/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$fls.trt.hip.female>=(1-user_inputs$prop.fx.high.risk),1,
                1))



user_inputs$no.fls.trt.other.female.low.risk <-
  if_else(user_inputs$no.fls.trt.other.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.other.female>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$no.fls.trt.other.female- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$no.fls.trt.other.female.intermediate.risk <-
  if_else(user_inputs$no.fls.trt.other.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$no.fls.trt.other.female>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$no.fls.trt.other.female-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$no.fls.trt.other.female.high.risk<-
  if_else(user_inputs$no.fls.trt.other.female<=user_inputs$prop.fx.low.risk,
        user_inputs$no.fls.trt.other.female/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$no.fls.trt.other.female>=(1-user_inputs$prop.fx.high.risk),1,
                1))

user_inputs$fls.trt.other.female.low.risk <-
  if_else(user_inputs$fls.trt.other.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.other.female>=(1-user_inputs$prop.fx.high.risk),
             (user_inputs$fls.trt.other.female- user_inputs$prop.fx.low.risk-
                   user_inputs$prop.fx.intermediate.risk)/user_inputs$prop.fx.high.risk,0))

user_inputs$fls.trt.other.female.intermediate.risk <-
  if_else(user_inputs$fls.trt.other.female<=user_inputs$prop.fx.low.risk, 0,
  if_else(user_inputs$fls.trt.other.female>=(1-user_inputs$prop.fx.high.risk),1,
        (user_inputs$fls.trt.other.female-user_inputs$prop.fx.low.risk)/user_inputs$prop.fx.intermediate.risk))

user_inputs$fls.trt.other.female.high.risk<-
  if_else(user_inputs$fls.trt.other.female<=user_inputs$prop.fx.low.risk,
        user_inputs$fls.trt.other.female/user_inputs$prop.fx.low.risk,
  if_else(user_inputs$fls.trt.other.female>=(1-user_inputs$prop.fx.high.risk),1,
                1))
# start timer -----
start<-Sys.time()

# set seed ----
RNGkind("L'Ecuyer-CMRG")
set.seed(123)

# set up trace -----
print("Setting up empty matrices")
  number.cycles<-60
  # 1 month cycles, 5 years

  # set up:
  # list of dataframes
  # each item in the list is a dataframe
  # each dataframe corresponds to an individual under a treatment option
  # e.g. one dataframe for first individual with no FLS,
  # another with the first individual with FLS,
  # and so on

#add risk profiles
  risk.profiles<-
    bind_rows(
  data.frame(id=microsim_pop %>%
                filter(index_fx=="spine") %>%
                filter(sex=="male") %>%
               select(id) %>% pull(),
            risk.type=c(rep(1, #"Low risk"
          floor(as.numeric(working.country_data %>%
  filter(name=="prop.fx.low.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="spine") %>%
                                    filter(sex=="male")))),
           rep(2, #"Intermediate risk"
          as.numeric(working.country_data %>%
  filter(name=="prop.fx.intermediate.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="spine") %>%
                                    filter(sex=="male"))),
  rep(3, #"High risk"
          ceiling(as.numeric(working.country_data %>%
  filter(name=="prop.fx.high.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="spine") %>%
                                    filter(sex=="male")))))),

   data.frame(id=microsim_pop %>%
                filter(index_fx=="hip") %>%
                filter(sex=="male") %>%
               select(id) %>% pull(),
            risk.type=c(rep(1, #"Low risk"
          floor(as.numeric(working.country_data %>%
  filter(name=="prop.fx.low.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="hip") %>%
                                    filter(sex=="male")))),
           rep(2, #"Intermediate risk"
          as.numeric(working.country_data %>%
  filter(name=="prop.fx.intermediate.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="hip") %>%
                                    filter(sex=="male"))),
  rep(3, #"High risk"
          ceiling(as.numeric(working.country_data %>%
  filter(name=="prop.fx.high.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="hip") %>%
                                    filter(sex=="male")))))),

    data.frame(id=microsim_pop %>%
                filter(index_fx=="other") %>%
                filter(sex=="male") %>%
               select(id) %>% pull(),
            risk.type=c(rep(1, #"Low risk"
          floor(as.numeric(working.country_data %>%
  filter(name=="prop.fx.low.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="other") %>%
                                    filter(sex=="male")))),
           rep(2, #"Intermediate risk"
          as.numeric(working.country_data %>%
  filter(name=="prop.fx.intermediate.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="other") %>%
                                    filter(sex=="male"))),
  rep(3, #"High risk"
          ceiling(as.numeric(working.country_data %>%
  filter(name=="prop.fx.high.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="other") %>%
                                    filter(sex=="male")))))),


    data.frame(id=microsim_pop %>%
                filter(index_fx=="spine") %>%
                filter(sex=="female") %>%
               select(id) %>% pull(),
            risk.type=c(rep(1, #"Low risk"
          floor(as.numeric(working.country_data %>%
  filter(name=="prop.fx.low.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="spine") %>%
                                    filter(sex=="female")))),
           rep(2, #"Intermediate risk"
          as.numeric(working.country_data %>%
  filter(name=="prop.fx.intermediate.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="spine") %>%
                                    filter(sex=="female"))),
  rep(3, #"High risk"
          ceiling(as.numeric(working.country_data %>%
  filter(name=="prop.fx.high.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="spine") %>%
                                    filter(sex=="female")))))),

   data.frame(id=microsim_pop %>%
                filter(index_fx=="hip") %>%
                filter(sex=="female") %>%
               select(id) %>% pull(),
            risk.type=c(rep(1, #"Low risk"
          floor(as.numeric(working.country_data %>%
  filter(name=="prop.fx.low.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="hip") %>%
                                    filter(sex=="female")))),
           rep(2, #"Intermediate risk"
          as.numeric(working.country_data %>%
  filter(name=="prop.fx.intermediate.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="hip") %>%
                                    filter(sex=="female"))),
  rep(3, #"High risk"
          ceiling(as.numeric(working.country_data %>%
  filter(name=="prop.fx.high.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="hip") %>%
                                    filter(sex=="female")))))),

    data.frame(id=microsim_pop %>%
                filter(index_fx=="other") %>%
                filter(sex=="female") %>%
               select(id) %>% pull(),
            risk.type=c(rep(1, #"Low risk"
          floor(as.numeric(working.country_data %>%
  filter(name=="prop.fx.low.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="other") %>%
                                    filter(sex=="female")))),
           rep(2, #"Intermediate risk"
          as.numeric(working.country_data %>%
  filter(name=="prop.fx.intermediate.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="other") %>%
                                    filter(sex=="female"))),
  rep(3, #"High risk"
          ceiling(as.numeric(working.country_data %>%
  filter(name=="prop.fx.high.risk") %>%
  select(Value) %>% pull())* nrow(microsim_pop %>%
                                    filter(index_fx=="other") %>%
                                    filter(sex=="female"))))))
    )


microsim_pop<-microsim_pop %>%
  left_join(risk.profiles)



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


# run model -----
print("Running the model")

source("RMicrosim/extras/runmodel.r")

# summary output  -----
#load(paste0(results.folder, "/results.m.TR.trace.complete.RData"))

output<-get.output(m.TR=m.TR, with.markov.trace=TRUE)
save(list=c("n_microsimulation", "study_pop_n", "output"),
     file=paste0(results.folder, "/results.output.RData"))


Sys.time()-start
