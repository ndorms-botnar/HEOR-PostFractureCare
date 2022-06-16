  ### SET UP MODEL ####
  print("Setting up the model")

  # starting state ------
  m.TR[which(m.TR$month==0),] <- m.TR[which(m.TR$month==0),]  %>%
    mutate(s_hf=if_else(index_fx=="hip" , 1, 0)) %>%
    mutate(s_sf=if_else(index_fx=="spine", 1, 0)) %>%
    mutate(s_of=if_else(index_fx=="other", 1, 0)) %>%
    mutate(s_ff=0,
           s_d=0)
# start in a fracture state
  m.TR[which(m.TR$month==0),] <- m.TR[which(m.TR$month==0),]  %>%
    mutate(c_af=1)
# no history yet
  m.TR[which(m.TR$month==0),] <- m.TR[which(m.TR$month==0),]  %>%
    mutate(h_hf= 0) %>%
    mutate(h_sf=0)  %>%
    mutate(h_of=0)  %>%
    mutate(h_af=0) # i.e. no history of fracture (will have from next period....)

# recent.fx
  m.TR[which(m.TR$month==0),] <-  m.TR[which(m.TR$month==0),]  %>%
    mutate(recent.fx= ifelse(index_fx=="spine" , 1,
                      ifelse(index_fx=="hip" , 2,
                      ifelse(index_fx=="other" , 3, NA))))
  # identified- no
  m.TR[which(m.TR$month==0),] <-  m.TR[which(m.TR$month==0),]  %>%
    mutate(identified=1,
           tr.onset=1,
           treat=1,
           medication=0,
           monitored=1,
           adhering=1,
           lag.passed=0,
           time_med=0)



# history
  m.TR$t_since_sf<- NA
  m.TR$t_since_hf<- NA
  m.TR$t_since_of<- NA

  m.TR$sf.tp<- NA
  m.TR$hf.tp<- NA
  m.TR$of.tp<- NA
  m.TR$of.tp<- NA
  m.TR$tp.death<- NA

  # as list  ---------
  m.TR<-m.TR %>%
    mutate(intervention=if_else(intervention=="no FLS",
                               1,2)) %>%
    mutate(index_fx=if_else(index_fx=="spine",
                           1,
                           if_else(index_fx=="hip",
                                  2,
                                  3))) %>%
    mutate(sex=if_else(sex=="male",
                      1,2))

m.TR <-  split(m.TR,
                 m.TR[,c('id','intervention')])

  # rr ---------
  rr<-get.rr(working.country_data=working.country_data)
  
  # lags ------
  lag<-get.lags(general.inputs=general.inputs)

  

  # run through cycles  -----
if(run.in.parallel==TRUE){
  print("Setting up parallel")
  if(detectCores()==32){
    cores<-detectCores() # ox pc
  } else(
    cores<-detectCores()/2 # will use half the available cores if not on ox pc
  )
 
  cl<-makeCluster(getOption("cl.cores", cores))
  clusterSetRNGStream(cl, 123)
  clusterExport(cl,c('background.tps','user_inputs',
                     'cmp_m.fun.time.since.fx',
                     'rMultinom',
'cmp_m.fun.tps','cmp_m.fun.two.fx',
'cmp_m.fun.identified','cmp_m.fun.tr.onset','cmp_m.fun.treat',
'cmp_m.fun.assign.treatment','cmp_m.fun.switch.treatment',
    'cmp_m.fun.hospitalised','cmp_m.fun.procedure',
                     'cmp_m.fun.temp.rehab','cmp_m.fun.location',
                     'm.fun.lab.test','m.fun.dxa',
"cmp_m.fun.adherrence.t1",
"cmp_m.fun.adherrence.t4", "cmp_m.fun.adherrence.t12","cmp_m.fun.adherrence.t24_36_48" ,
"lag", "rr"),
envir=environment())
}

for (i in 1:61) {
print(paste0("- Getting transitions for month ", i-1, " of 60"))
 
# get rand for time period
print(paste0("-- Getting random draws for time ", i))
working.rmultinorm<-get.rmultinorm(input=user_inputs, n_microsimulation) 
  # run 
if(run.in.parallel==TRUE){
m.TR<-parLapply(cl, m.TR,cmp_m.TR.go, t={{i}}, rmultinorm= working.rmultinorm,input=user_inputs,
                use.risk.profiles=use.risk.profiles)
  } else{
m.TR<-lapply(m.TR,cmp_m.TR.go, t={{i}}, rmultinorm= working.rmultinorm,input=user_inputs,
                use.risk.profiles=use.risk.profiles)
  }
rm(working.rmultinorm)
}
m.TR<-  lapply(m.TR,data.frame)

if(run.in.parallel==TRUE){
  stopCluster(cl)
  }


if (!file.exists(results.folder)){
  dir.create(results.folder, recursive = TRUE)}
save(list=c("m.TR"),
     file=paste0(results.folder, "/results.m.TR.trace.only.RData"))
# saveRDS(m.TR, file = paste0(results.folder, "/results.m.TR.trace.only.rds"))
gc()

  # tidy output -----
print("From list to dataframe")
  m.TR<-bind_rows(m.TR, .id = NULL)

  m.TR<-m.TR %>%
    mutate(intervention=ifelse(intervention==1, "no FLS", "FLS")) %>%
    mutate(identified=ifelse(identified==1,"No","Yes")) %>%
    mutate(apply.rr=ifelse(is.na(apply.rr),"No","Yes")) %>%
    #   mutate(tr.onset=ifelse(tr.onset==1,"No","Yes")) %>%
    mutate(treat=ifelse(treat==1,"No","Yes")) %>%
    mutate(adhering=ifelse(adhering==1,"No","Yes")) %>%
    mutate(adhering=ifelse(adhering=="No" & s_d==1, "No (died)",    
                             adhering)) %>%
    mutate(index_fx=ifelse(index_fx==1, "spine",
                           ifelse(index_fx==2, "hip", "other"))) %>%
    mutate(recent.fx=ifelse(recent.fx==1, "spine",
                            ifelse(recent.fx==2, "hip",
                                   ifelse(recent.fx==3, "other",NA)))) %>%
    mutate(sex=ifelse(sex==1, "male", "female")) %>%
    mutate(sex=factor(sex,
                      levels=c("male", "female"))) %>%
    mutate(medication=
             ifelse(medication== 0,"no drug",
             ifelse(medication==1,"alendronate",
             ifelse(medication==2,"risedronate",
             ifelse(medication==3,"strontium",
             ifelse(medication==4,"ibandronate",
             ifelse(medication==5,"raloxifene",
             ifelse(medication==6,"denosumab",
             ifelse(medication==7,"zoledronate",
             ifelse(medication==8,"teriparatide",
             ifelse(medication==9,"abaloparatide",
             ifelse(medication== 10,"romosozumab",
                     NA)))))))))))) %>%

    mutate(medication=ifelse(medication=="no drug" & s_d==1, "no drug (died)",    
                             medication)) %>%     
    mutate(hospitalised=
             ifelse(hospitalised==1, "no",
             ifelse(hospitalised==2,"yes", NA))) %>%
    mutate(procedure=
             ifelse(procedure== 1,"none",
             ifelse(procedure==2,"hip surgery",
             ifelse(procedure==3,"other surgery",
             ifelse(procedure==4,"kyphoplasty",
             ifelse(procedure==5,"vertebroplasty", 
                    NA )))))) %>%
    mutate(temp.rehab=
             ifelse(temp.rehab==1, "no",
             ifelse(temp.rehab==2,"yes", NA))) %>%
    mutate(location=
             ifelse(location==1, "home, no support",
             ifelse(location==2, "family home",
             ifelse(location==3, "home, support",
             ifelse(location==4, "long term care", 
             ifelse(location==5, "died",
                    NA))))) ) %>% 
    mutate(risk.type=ifelse(risk.type==1, "Low risk",
                     ifelse(risk.type==2, "Intermediate risk",
                     ifelse(risk.type==3, "High risk", NA)))) 
  
  

  # get costs -----
  m.TR<-m.TR %>%
    mutate(procedure.cost=ifelse(procedure=="none",0,
                          ifelse(procedure=="hip surgery", user_inputs$cost.hip.surg,
                          ifelse(procedure=="other surgery", user_inputs$cost.other.surg,
                          ifelse(procedure=="kyphoplasty", user_inputs$cost.spine.kyphoplasty,
                          ifelse(procedure=="vertebroplasty", user_inputs$cost.spine.vertebroplasty,
                                                             0))))))

  # hospital length of stay ----
  # if hospitalised, depending on index fracture and procedure
  m.TR<-m.TR %>%
    mutate(hosp.los=
             ifelse(hospitalised=="no", 0,
                    ifelse(hospitalised=="yes" &
                             procedure=="none"  &
                             s_hf==1, user_inputs$hospital.los.hip.no.surg,
                           ifelse(hospitalised=="yes" &
                                    procedure=="hip surgery", user_inputs$hospital.los.hip.surg,
                                  ifelse(hospitalised=="yes" &
                                           procedure=="none"  &
                                           s_sf==1, user_inputs$hospital.los.spine.no.surg,
                                         ifelse(hospitalised=="yes" &
                                                  procedure=="kyphoplasty", user_inputs$hospital.los.spine.kyphoplasty,
                                                ifelse(hospitalised=="yes" &
                                                         procedure=="vertebroplasty", user_inputs$hospital.los.spine.vertebroplasty,
                                                       ifelse(hospitalised=="yes" &
                                                                procedure=="none"  &
                                                                s_of==1, user_inputs$hospital.los.other.no.surg,
                                                              ifelse(hospitalised=="yes" &
                                                                       procedure=="other surgery", user_inputs$hospital.los.other.surg,
                                                                     NA)))))))))


  # hospital cost ----
  # for those hospitalised, given length of stay
  m.TR<-m.TR %>%
    mutate(hosp.cost=
             ifelse(hospitalised=="no", 0,
                    ifelse(hospitalised=="yes",
                           (hosp.los*user_inputs$cost.hosp.per.day),
                           NA)))

  m.TR<-m.TR %>%
    mutate(hosp.w.proc.cost=
             ifelse(hospitalised=="no", 0,
                    ifelse(hospitalised=="yes",
                           (hosp.los*user_inputs$cost.hosp.per.day)
                           +procedure.cost,
                           NA)))


  # community cost ----
  #browser()

  # spine only
  # dependent on procedure
  
  m.TR<-m.TR %>%
  mutate(comm.visits.count=
             ifelse(hospitalised=="no" &
                      s_sf==1,
                    user_inputs$visits.comm.consults.spine, 0))
  
  m.TR<-m.TR %>%
    mutate(comm.cost=
             ifelse(hospitalised=="no" &
                      s_sf==1,
                    (user_inputs$visits.comm.consults.spine* user_inputs$cost.spine.community.care)+
                      procedure.cost, 0))


  # clinic cost ----
  # for only those not hospitalised,
  # so for only hip and other
  # with cost of one visit
  m.TR<-m.TR %>%
  mutate(clinic.visits.count=
         ifelse(hospitalised=="no" &  s_hf==1,
                    1 ,
          ifelse(hospitalised=="no" & s_of==1,
                    1 ,
                    0)))
  
  m.TR<-m.TR %>%
    mutate(clinic.cost=
             ifelse(hospitalised=="no" &
                      s_hf==1,
                    user_inputs$cost.hosp.clinic.visit ,
                    ifelse(hospitalised=="no" &
                             s_of==1,
                           user_inputs$cost.hosp.clinic.visit ,
                           0)))

  # temp rehab los and cost
  m.TR<-m.TR %>%
    mutate(temp.rehab.los=
             ifelse(temp.rehab=="no", 0,
                    ifelse(temp.rehab=="yes"&
                             s_hf==1,   user_inputs$temp.rehab.los.hip,
                           ifelse(temp.rehab=="yes"&
                                    s_sf==1,   user_inputs$temp.rehab.los.spine,
                                  ifelse(temp.rehab=="yes"&
                                           s_of==1,   user_inputs$temp.rehab.los.other,
                                         NA))))) %>%
    mutate(temp.rehab.cost=temp.rehab.los*user_inputs$temp.rehab.daily.cost)

  # location cost -----
   m.TR<-m.TR %>%
    mutate(location_home_support.cost=
             ifelse(location=="home, support",
                    user_inputs$care.home.monthly.cost,
                           0  ))
   m.TR<-m.TR %>%
    mutate(location_long_term_care.cost=
                    ifelse(location=="long term care",
                           user_inputs$long.term.care.monthly.cost,
                           0  ))
   
  m.TR<-m.TR %>%
    mutate(location.cost=
             ifelse(location=="home, support",
                    user_inputs$care.home.monthly.cost,
                    ifelse(location=="long term care",
                           user_inputs$long.term.care.monthly.cost,
                           0  )))

  # discharge clinic cost -----
  m.TR<-m.TR %>%
    mutate(discharge.clinic.visits=
             ifelse(
               hospitalised=="no" &  s_hf==1,
               user_inputs$clinic.visits.not.admitted.hip,
               ifelse(
                 hospitalised=="no" &  s_sf==1,
                 user_inputs$clinic.visits.not.admitted.spine,
                 ifelse(
                   hospitalised=="no" &  s_of==1,
                   user_inputs$clinic.visits.not.admitted.other,

                   ifelse(
                     hospitalised=="yes" &  s_hf==1,
                     user_inputs$clinic.visits.admitted.hip,
                     ifelse(
                       hospitalised=="yes" &  s_sf==1,
                       user_inputs$clinic.visits.admitted.spine,
                       ifelse(
                         hospitalised=="yes" &  s_of==1,
                         user_inputs$clinic.visits.admitted.other,
                         0)))))))
  
  
  m.TR<-m.TR %>%
    mutate(discharge.clinic.cost=
             ifelse(
               hospitalised=="no" &  s_hf==1,
               user_inputs$clinic.visits.not.admitted.hip*user_inputs$clinic.visit.cost,
               ifelse(
                 hospitalised=="no" &  s_sf==1,
                 user_inputs$clinic.visits.not.admitted.spine*user_inputs$clinic.visit.cost,
                 ifelse(
                   hospitalised=="no" &  s_of==1,
                   user_inputs$clinic.visits.not.admitted.other*user_inputs$clinic.visit.cost,

                   ifelse(
                     hospitalised=="yes" &  s_hf==1,
                     user_inputs$clinic.visits.admitted.hip*user_inputs$clinic.visit.cost,
                     ifelse(
                       hospitalised=="yes" &  s_sf==1,
                       user_inputs$clinic.visits.admitted.spine*user_inputs$clinic.visit.cost,
                       ifelse(
                         hospitalised=="yes" &  s_of==1,
                         user_inputs$clinic.visits.admitted.other*user_inputs$clinic.visit.cost,
                         0)))))))



  # medication cost -----
  # only occurred if adhering
  # from yearly to monthly cost
  #based on most recent fx(?)

  m.TR<-m.TR %>%
    mutate(medication.cost= 
             # hip
             ifelse(
               medication=="alendronate" &  adhering=="Yes" & recent.fx=="hip",
               user_inputs$Alendronate.yearly.cost.hip/12,
               ifelse(
                 medication=="risedronate" &  adhering=="Yes" & recent.fx=="hip",
                 user_inputs$Risedronate.yearly.cost.hip/12,
                 ifelse(
                   medication=="ibandronate" &  adhering=="Yes" & recent.fx=="hip",
                   user_inputs$Ibandronate.yearly.cost.hip/12,
                   ifelse(
                   medication=="raloxifene" &  adhering=="Yes" & recent.fx=="hip",
                   user_inputs$Raloxifene.yearly.cost.hip/12,
                   ifelse(
                     medication=="strontium" &  adhering=="Yes" & recent.fx=="hip",
                     user_inputs$Strontium.yearly.cost.hip/12,
                     ifelse(
                       medication=="denosumab" &  adhering=="Yes" & recent.fx=="hip",
                       user_inputs$Denosumab.yearly.cost.hip/12,
                       ifelse(
                         medication=="zoledronate" &  adhering=="Yes" & recent.fx=="hip",
                         user_inputs$Zoledronate.yearly.cost.hip/12,
                         ifelse(
                           medication=="teriparatide" &  adhering=="Yes" & recent.fx=="hip",
                           user_inputs$Teriparatide.yearly.cost.hip/12,
                           ifelse(
                             medication=="abaloparatide" &  adhering=="Yes" & recent.fx=="hip",
                             user_inputs$Abaloparatide.yearly.cost.hip/12,
                             ifelse(
                               medication=="romosozumab" &  adhering=="Yes" & recent.fx=="hip",
                               user_inputs$Romo.yearly.cost.hip/12,
                               # spine
                               ifelse(
                                 medication=="alendronate" &  adhering=="Yes" & recent.fx=="spine",
                                 user_inputs$Alendronate.yearly.cost.spine/12,
                                 ifelse(
                                   medication=="risedronate" &  adhering=="Yes" & recent.fx=="spine",
                                   user_inputs$Risedronate.yearly.cost.spine/12,
                                   ifelse(
                                     medication=="ibandronate" &  adhering=="Yes" & recent.fx=="spine",
                                     user_inputs$Ibandronate.yearly.cost.spine/12,
                                     ifelse(
                                     medication=="raloxifene" &  adhering=="Yes" & recent.fx=="spine",
                                     user_inputs$Raloxifene.yearly.cost.spine/12,
                                     ifelse(
                                       medication=="strontium" &  adhering=="Yes" & recent.fx=="spine",
                                       user_inputs$Strontium.yearly.cost.spine/12,
                                       ifelse(
                                         medication=="denosumab" &  adhering=="Yes" & recent.fx=="spine",
                                         user_inputs$Denosumab.yearly.cost.spine/12,
                                         ifelse(
                                           medication=="zoledronate" &  adhering=="Yes" & recent.fx=="spine",
                                           user_inputs$Zoledronate.yearly.cost.spine/12,
                                           ifelse(
                                             medication=="teriparatide" &  adhering=="Yes" & recent.fx=="spine",
                                             user_inputs$Teriparatide.yearly.cost.spine/12,
                                             ifelse(
                                               medication=="abaloparatide" &  adhering=="Yes" & recent.fx=="spine",
                                               user_inputs$Abaloparatide.yearly.cost.spine/12,
                                               ifelse(
                                                 medication=="romosozumab" &  adhering=="Yes" & recent.fx=="spine",
                                                 user_inputs$Romo.yearly.cost.spine/12,
                                                 # other
                                                 ifelse(
                                                   medication=="alendronate" &  adhering=="Yes" & recent.fx=="other",
                                                   user_inputs$Alendronate.yearly.cost.other/12,
                                                   ifelse(
                                                     medication=="risedronate" &  adhering=="Yes" & recent.fx=="other",
                                                     user_inputs$Risedronate.yearly.cost.other/12,
                                                     ifelse(
                                                       medication=="ibandronate" &  adhering=="Yes" & recent.fx=="other",
                                                       user_inputs$Ibandronate.yearly.cost.other/12,
                                                        ifelse(
                                                       medication=="raloxifene" &  adhering=="Yes" & recent.fx=="other",
                                                       user_inputs$Raloxifene.yearly.cost.other/12,
                                                       ifelse(
                                                         medication=="strontium" &  adhering=="Yes" & recent.fx=="other",
                                                         user_inputs$Strontium.yearly.cost.other/12,
                                                         ifelse(
                                                           medication=="denosumab" &  adhering=="Yes" & recent.fx=="other",
                                                           user_inputs$Denosumab.yearly.cost.other/12,
                                                           ifelse(
                                                             medication=="zoledronate" &  adhering=="Yes" & recent.fx=="other",
                                                             user_inputs$Zoledronate.yearly.cost.other/12,
                                                             ifelse(
                                                               medication=="teriparatide" &  adhering=="Yes" & recent.fx=="other",
                                                               user_inputs$Teriparatide.yearly.cost.other/12,
                                                               ifelse(
                                                                 medication=="abaloparatide" &  adhering=="Yes" & recent.fx=="other",
                                                                 user_inputs$Abaloparatide.yearly.cost.other/12,
                                                                 ifelse(
                                                                   medication=="romosozumab" &  adhering=="Yes" & recent.fx=="other",
                                                                   user_inputs$Romo.yearly.cost.other/12,
                                                                   0 )))))))))))))))))))))))))))))))
  
  # Zolendronate cost to zero after three years
 # sum(m.TR$medication== "zoledronate" & m.TR$time_med>36)/nrow(m.TR)
  m.TR<-m.TR %>%
    mutate(medication.cost= 
             ifelse((medication== "zoledronate" & 
                      time_med>36), 0,
           medication.cost)) 


  # fx prevention time spent -----
 m.TR<-m.TR %>%
    mutate(identified.lag=lag(identified))
  
  
  no.fls.administrator.mins.hip<-(user_inputs$no.fls.administrator.identification.mins.hip +
            user_inputs$no.fls.administrator.assessment.mins.hip +
            user_inputs$no.fls.administrator.recommendation.mins.hip +
            user_inputs$no.fls.administrator.monitoring.mins.hip )
  fls.administrator.mins.hip<-(user_inputs$fls.administrator.identification.mins.hip +
            user_inputs$fls.administrator.assessment.mins.hip +
            user_inputs$fls.administrator.recommendation.mins.hip +
            user_inputs$fls.administrator.monitoring.mins.hip )
  no.fls.administrator.mins.spine<-(user_inputs$no.fls.administrator.identification.mins.spine +
            user_inputs$no.fls.administrator.assessment.mins.spine +
            user_inputs$no.fls.administrator.recommendation.mins.spine +
            user_inputs$no.fls.administrator.monitoring.mins.spine )
  fls.administrator.mins.spine<-(user_inputs$fls.administrator.identification.mins.spine +
            user_inputs$fls.administrator.assessment.mins.spine +
            user_inputs$fls.administrator.recommendation.mins.spine +
            user_inputs$fls.administrator.monitoring.mins.spine )
  no.fls.administrator.mins.other<-(user_inputs$no.fls.administrator.identification.mins.other +
            user_inputs$no.fls.administrator.assessment.mins.other +
            user_inputs$no.fls.administrator.recommendation.mins.other +
            user_inputs$no.fls.administrator.monitoring.mins.other )
  fls.administrator.mins.other<-(user_inputs$fls.administrator.identification.mins.other +
            user_inputs$fls.administrator.assessment.mins.other +
            user_inputs$fls.administrator.recommendation.mins.other +
            user_inputs$fls.administrator.monitoring.mins.other )
  
  m.TR<-m.TR %>%
    mutate(administrator.mins=
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
              recent.fx=="hip" & 
              intervention=="no FLS",
              no.fls.administrator.mins.hip,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="spine" & intervention=="no FLS",
             no.fls.administrator.mins.spine,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="other" & intervention=="no FLS",
             no.fls.administrator.mins.other,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="hip" & intervention=="FLS",
             fls.administrator.mins.hip,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="spine" & intervention=="FLS",
             fls.administrator.mins.spine,
      ifelse(identified=="Yes" &
            (identified.lag=="No"| c_af==1) &
            recent.fx=="other" & intervention=="FLS",
            fls.administrator.mins.other,
             0)))))))

  
  
  
    no.fls.doctor.mins.hip<-(user_inputs$no.fls.doctor.identification.mins.hip +
            user_inputs$no.fls.doctor.assessment.mins.hip +
            user_inputs$no.fls.doctor.recommendation.mins.hip +
            user_inputs$no.fls.doctor.monitoring.mins.hip )
  fls.doctor.mins.hip<-(user_inputs$fls.doctor.identification.mins.hip +
            user_inputs$fls.doctor.assessment.mins.hip +
            user_inputs$fls.doctor.recommendation.mins.hip +
            user_inputs$fls.doctor.monitoring.mins.hip )
  no.fls.doctor.mins.spine<-(user_inputs$no.fls.doctor.identification.mins.spine +
            user_inputs$no.fls.doctor.assessment.mins.spine +
            user_inputs$no.fls.doctor.recommendation.mins.spine +
            user_inputs$no.fls.doctor.monitoring.mins.spine )
  fls.doctor.mins.spine<-(user_inputs$fls.doctor.identification.mins.spine +
            user_inputs$fls.doctor.assessment.mins.spine +
            user_inputs$fls.doctor.recommendation.mins.spine +
            user_inputs$fls.doctor.monitoring.mins.spine )
  no.fls.doctor.mins.other<-(user_inputs$no.fls.doctor.identification.mins.other +
            user_inputs$no.fls.doctor.assessment.mins.other +
            user_inputs$no.fls.doctor.recommendation.mins.other +
            user_inputs$no.fls.doctor.monitoring.mins.other )
  fls.doctor.mins.other<-(user_inputs$fls.doctor.identification.mins.other +
            user_inputs$fls.doctor.assessment.mins.other +
            user_inputs$fls.doctor.recommendation.mins.other +
            user_inputs$fls.doctor.monitoring.mins.other )

m.TR<-m.TR %>%
    mutate(doctor.mins=
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
              recent.fx=="hip" & 
              intervention=="no FLS",
              no.fls.doctor.mins.hip,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="spine" & intervention=="no FLS",
             no.fls.doctor.mins.spine,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="other" & intervention=="no FLS",
             no.fls.doctor.mins.other,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="hip" & intervention=="FLS",
             fls.doctor.mins.hip,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="spine" & intervention=="FLS",
             fls.doctor.mins.spine,
      ifelse(identified=="Yes" &
            (identified.lag=="No"| c_af==1) &
            recent.fx=="other" & intervention=="FLS",
            fls.doctor.mins.other,
             0)))))))

        
  



  
    no.fls.nurse.mins.hip<-(user_inputs$no.fls.nurse.identification.mins.hip +
            user_inputs$no.fls.nurse.assessment.mins.hip +
            user_inputs$no.fls.nurse.recommendation.mins.hip +
            user_inputs$no.fls.nurse.monitoring.mins.hip )
  fls.nurse.mins.hip<-(user_inputs$fls.nurse.identification.mins.hip +
            user_inputs$fls.nurse.assessment.mins.hip +
            user_inputs$fls.nurse.recommendation.mins.hip +
            user_inputs$fls.nurse.monitoring.mins.hip )
  no.fls.nurse.mins.spine<-(user_inputs$no.fls.nurse.identification.mins.spine +
            user_inputs$no.fls.nurse.assessment.mins.spine +
            user_inputs$no.fls.nurse.recommendation.mins.spine +
            user_inputs$no.fls.nurse.monitoring.mins.spine )
  fls.nurse.mins.spine<-(user_inputs$fls.nurse.identification.mins.spine +
            user_inputs$fls.nurse.assessment.mins.spine +
            user_inputs$fls.nurse.recommendation.mins.spine +
            user_inputs$fls.nurse.monitoring.mins.spine )
  no.fls.nurse.mins.other<-(user_inputs$no.fls.nurse.identification.mins.other +
            user_inputs$no.fls.nurse.assessment.mins.other +
            user_inputs$no.fls.nurse.recommendation.mins.other +
            user_inputs$no.fls.nurse.monitoring.mins.other )
  fls.nurse.mins.other<-(user_inputs$fls.nurse.identification.mins.other +
            user_inputs$fls.nurse.assessment.mins.other +
            user_inputs$fls.nurse.recommendation.mins.other +
            user_inputs$fls.nurse.monitoring.mins.other )

m.TR<-m.TR %>%
    mutate(nurse.mins=
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
              recent.fx=="hip" & 
              intervention=="no FLS",
              no.fls.nurse.mins.hip,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="spine" & intervention=="no FLS",
             no.fls.nurse.mins.spine,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="other" & intervention=="no FLS",
             no.fls.nurse.mins.other,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="hip" & intervention=="FLS",
             fls.nurse.mins.hip,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="spine" & intervention=="FLS",
             fls.nurse.mins.spine,
      ifelse(identified=="Yes" &
            (identified.lag=="No"| c_af==1) &
            recent.fx=="other" & intervention=="FLS",
            fls.nurse.mins.other,
             0)))))))




  
    no.fls.radiographer.mins.hip<-(user_inputs$no.fls.radiographer.identification.mins.hip +
            user_inputs$no.fls.radiographer.assessment.mins.hip +
            user_inputs$no.fls.radiographer.recommendation.mins.hip +
            user_inputs$no.fls.radiographer.monitoring.mins.hip )
  fls.radiographer.mins.hip<-(user_inputs$fls.radiographer.identification.mins.hip +
            user_inputs$fls.radiographer.assessment.mins.hip +
            user_inputs$fls.radiographer.recommendation.mins.hip +
            user_inputs$fls.radiographer.monitoring.mins.hip )
  no.fls.radiographer.mins.spine<-(user_inputs$no.fls.radiographer.identification.mins.spine +
            user_inputs$no.fls.radiographer.assessment.mins.spine +
            user_inputs$no.fls.radiographer.recommendation.mins.spine +
            user_inputs$no.fls.radiographer.monitoring.mins.spine )
  fls.radiographer.mins.spine<-(user_inputs$fls.radiographer.identification.mins.spine +
            user_inputs$fls.radiographer.assessment.mins.spine +
            user_inputs$fls.radiographer.recommendation.mins.spine +
            user_inputs$fls.radiographer.monitoring.mins.spine )
  no.fls.radiographer.mins.other<-(user_inputs$no.fls.radiographer.identification.mins.other +
            user_inputs$no.fls.radiographer.assessment.mins.other +
            user_inputs$no.fls.radiographer.recommendation.mins.other +
            user_inputs$no.fls.radiographer.monitoring.mins.other )
  fls.radiographer.mins.other<-(user_inputs$fls.radiographer.identification.mins.other +
            user_inputs$fls.radiographer.assessment.mins.other +
            user_inputs$fls.radiographer.recommendation.mins.other +
            user_inputs$fls.radiographer.monitoring.mins.other )

m.TR<-m.TR %>%
    mutate(radiographer.mins=
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
              recent.fx=="hip" & 
              intervention=="no FLS",
              no.fls.radiographer.mins.hip,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="spine" & intervention=="no FLS",
             no.fls.radiographer.mins.spine,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="other" & intervention=="no FLS",
             no.fls.radiographer.mins.other,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="hip" & intervention=="FLS",
             fls.radiographer.mins.hip,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="spine" & intervention=="FLS",
             fls.radiographer.mins.spine,
      ifelse(identified=="Yes" &
            (identified.lag=="No"| c_af==1) &
            recent.fx=="other" & intervention=="FLS",
            fls.radiographer.mins.other,
             0)))))))




  
    no.fls.allied_health.mins.hip<-(user_inputs$no.fls.allied_health.identification.mins.hip +
            user_inputs$no.fls.allied_health.assessment.mins.hip +
            user_inputs$no.fls.allied_health.recommendation.mins.hip +
            user_inputs$no.fls.allied_health.monitoring.mins.hip )
  fls.allied_health.mins.hip<-(user_inputs$fls.allied_health.identification.mins.hip +
            user_inputs$fls.allied_health.assessment.mins.hip +
            user_inputs$fls.allied_health.recommendation.mins.hip +
            user_inputs$fls.allied_health.monitoring.mins.hip )
  no.fls.allied_health.mins.spine<-(user_inputs$no.fls.allied_health.identification.mins.spine +
            user_inputs$no.fls.allied_health.assessment.mins.spine +
            user_inputs$no.fls.allied_health.recommendation.mins.spine +
            user_inputs$no.fls.allied_health.monitoring.mins.spine )
  fls.allied_health.mins.spine<-(user_inputs$fls.allied_health.identification.mins.spine +
            user_inputs$fls.allied_health.assessment.mins.spine +
            user_inputs$fls.allied_health.recommendation.mins.spine +
            user_inputs$fls.allied_health.monitoring.mins.spine )
  no.fls.allied_health.mins.other<-(user_inputs$no.fls.allied_health.identification.mins.other +
            user_inputs$no.fls.allied_health.assessment.mins.other +
            user_inputs$no.fls.allied_health.recommendation.mins.other +
            user_inputs$no.fls.allied_health.monitoring.mins.other )
  fls.allied_health.mins.other<-(user_inputs$fls.allied_health.identification.mins.other +
            user_inputs$fls.allied_health.assessment.mins.other +
            user_inputs$fls.allied_health.recommendation.mins.other +
            user_inputs$fls.allied_health.monitoring.mins.other )

m.TR<-m.TR %>%
    mutate(allied_health.mins=
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
              recent.fx=="hip" & 
              intervention=="no FLS",
              no.fls.allied_health.mins.hip,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="spine" & intervention=="no FLS",
             no.fls.allied_health.mins.spine,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="other" & intervention=="no FLS",
             no.fls.allied_health.mins.other,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="hip" & intervention=="FLS",
             fls.allied_health.mins.hip,
       ifelse(identified=="Yes" &
             (identified.lag=="No"| c_af==1) &
             recent.fx=="spine" & intervention=="FLS",
             fls.allied_health.mins.spine,
      ifelse(identified=="Yes" &
            (identified.lag=="No"| c_af==1) &
            recent.fx=="other" & intervention=="FLS",
            fls.allied_health.mins.other,
             0)))))))





no.fls.fls_coordinator.mins.hip<-0
fls.fls_coordinator.mins.hip<-(user_inputs$fls.fls_coordinator.identification.mins.hip +
                               user_inputs$fls.fls_coordinator.assessment.mins.hip +
                               user_inputs$fls.fls_coordinator.recommendation.mins.hip +
                               user_inputs$fls.fls_coordinator.monitoring.mins.hip )
no.fls.fls_coordinator.mins.spine<-0
fls.fls_coordinator.mins.spine<-(user_inputs$fls.fls_coordinator.identification.mins.spine +
                                 user_inputs$fls.fls_coordinator.assessment.mins.spine +
                                 user_inputs$fls.fls_coordinator.recommendation.mins.spine +
                                 user_inputs$fls.fls_coordinator.monitoring.mins.spine )
no.fls.fls_coordinator.mins.other<-0
fls.fls_coordinator.mins.other<-(user_inputs$fls.fls_coordinator.identification.mins.other +
                                 user_inputs$fls.fls_coordinator.assessment.mins.other +
                                 user_inputs$fls.fls_coordinator.recommendation.mins.other +
                                 user_inputs$fls.fls_coordinator.monitoring.mins.other )

m.TR<-m.TR %>%
  mutate(fls_coordinator.mins=
           ifelse(identified=="Yes" &
                    (identified.lag=="No"| c_af==1) &
                    recent.fx=="hip" & 
                    intervention=="no FLS",
                  no.fls.fls_coordinator.mins.hip,
                  ifelse(identified=="Yes" &
                           (identified.lag=="No"| c_af==1) &
                           recent.fx=="spine" & intervention=="no FLS",
                         no.fls.fls_coordinator.mins.spine,
                         ifelse(identified=="Yes" &
                                  (identified.lag=="No"| c_af==1) &
                                  recent.fx=="other" & intervention=="no FLS",
                                no.fls.fls_coordinator.mins.other,
                                ifelse(identified=="Yes" &
                                         (identified.lag=="No"| c_af==1) &
                                         recent.fx=="hip" & intervention=="FLS",
                                       fls.fls_coordinator.mins.hip,
                                       ifelse(identified=="Yes" &
                                                (identified.lag=="No"| c_af==1) &
                                                recent.fx=="spine" & intervention=="FLS",
                                              fls.fls_coordinator.mins.spine,
                                              ifelse(identified=="Yes" &
                                                       (identified.lag=="No"| c_af==1) &
                                                       recent.fx=="other" & intervention=="FLS",
                                                     fls.fls_coordinator.mins.other,
                                                     0)))))))

no.fls.other.mins.hip<-(user_inputs$no.fls.other.identification.mins.hip +
                                  user_inputs$no.fls.other.assessment.mins.hip +
                                  user_inputs$no.fls.other.recommendation.mins.hip +
                                  user_inputs$no.fls.other.monitoring.mins.hip )
fls.other.mins.hip<-(user_inputs$fls.other.identification.mins.hip +
                               user_inputs$fls.other.assessment.mins.hip +
                               user_inputs$fls.other.recommendation.mins.hip +
                               user_inputs$fls.other.monitoring.mins.hip )
no.fls.other.mins.spine<-(user_inputs$no.fls.other.identification.mins.spine +
                                    user_inputs$no.fls.other.assessment.mins.spine +
                                    user_inputs$no.fls.other.recommendation.mins.spine +
                                    user_inputs$no.fls.other.monitoring.mins.spine )
fls.other.mins.spine<-(user_inputs$fls.other.identification.mins.spine +
                                 user_inputs$fls.other.assessment.mins.spine +
                                 user_inputs$fls.other.recommendation.mins.spine +
                                 user_inputs$fls.other.monitoring.mins.spine )
no.fls.other.mins.other<-(user_inputs$no.fls.other.identification.mins.other +
                                    user_inputs$no.fls.other.assessment.mins.other +
                                    user_inputs$no.fls.other.recommendation.mins.other +
                                    user_inputs$no.fls.other.monitoring.mins.other )
fls.other.mins.other<-(user_inputs$fls.other.identification.mins.other +
                                 user_inputs$fls.other.assessment.mins.other +
                                 user_inputs$fls.other.recommendation.mins.other +
                                 user_inputs$fls.other.monitoring.mins.other )

m.TR<-m.TR %>%
  mutate(other.mins=
           ifelse(identified=="Yes" &
                    (identified.lag=="No"| c_af==1) &
                    recent.fx=="hip" &
                    intervention=="no FLS",
                  no.fls.other.mins.hip,
                  ifelse(identified=="Yes" &
                           (identified.lag=="No"| c_af==1) &
                           recent.fx=="spine" & intervention=="no FLS",
                         no.fls.other.mins.spine,
                         ifelse(identified=="Yes" &
                                  (identified.lag=="No"| c_af==1) &
                                  recent.fx=="other" & intervention=="no FLS",
                                no.fls.other.mins.other,
                                ifelse(identified=="Yes" &
                                         (identified.lag=="No"| c_af==1) &
                                         recent.fx=="hip" & intervention=="FLS",
                                       fls.other.mins.hip,
                                       ifelse(identified=="Yes" &
                                                (identified.lag=="No"| c_af==1) &
                                                recent.fx=="spine" & intervention=="FLS",
                                              fls.other.mins.spine,
                                              ifelse(identified=="Yes" &
                                                       (identified.lag=="No"| c_af==1) &
                                                       recent.fx=="other" & intervention=="FLS",
                                                     fls.other.mins.other,
                                                     0)))))))

  
  # fx prevention costs ------
  # only applied when an individual is identified

  # staff cost
  # no.fls

  no.fls.hip.staff.cost<-
    (
        ((user_inputs$no.fls.nurse.identification.mins.hip +
            user_inputs$no.fls.nurse.assessment.mins.hip +
            user_inputs$no.fls.nurse.recommendation.mins.hip +
            user_inputs$no.fls.nurse.monitoring.mins.hip )*
           user_inputs$hourly.cost.nurse/60)+
        ((user_inputs$no.fls.doctor.identification.mins.hip +
            user_inputs$no.fls.doctor.assessment.mins.hip +
            user_inputs$no.fls.doctor.recommendation.mins.hip +
            user_inputs$no.fls.doctor.monitoring.mins.hip )*
           user_inputs$hourly.cost.doctor/60)+
        ((user_inputs$no.fls.radiographer.identification.mins.hip +
            user_inputs$no.fls.radiographer.assessment.mins.hip +
            user_inputs$no.fls.radiographer.recommendation.mins.hip +
            user_inputs$no.fls.radiographer.monitoring.mins.hip )*
           user_inputs$hourly.cost.radiographer/60)+
        ((user_inputs$no.fls.allied_health.identification.mins.hip +
            user_inputs$no.fls.allied_health.assessment.mins.hip +
            user_inputs$no.fls.allied_health.recommendation.mins.hip +
            user_inputs$no.fls.allied_health.monitoring.mins.hip )*
           user_inputs$hourly.cost.allied_health/60) +
        ((user_inputs$no.fls.administrator.identification.mins.hip +
            user_inputs$no.fls.administrator.assessment.mins.hip +
            user_inputs$no.fls.administrator.recommendation.mins.hip +
            user_inputs$no.fls.administrator.monitoring.mins.hip)*
           user_inputs$hourly.cost.administrator/60) +
        ((user_inputs$no.fls.other.identification.mins.hip +
      user_inputs$no.fls.other.assessment.mins.hip +
      user_inputs$no.fls.other.recommendation.mins.hip +
      user_inputs$no.fls.other.monitoring.mins.hip )*
       user_inputs$hourly.cost.other)
    )

  # if all zero
  no.fls.hip.staff.cost<-ifelse(length(no.fls.hip.staff.cost)==0,
                                0, no.fls.hip.staff.cost)

  no.fls.spine.staff.cost<-
    (
       ((user_inputs$no.fls.nurse.identification.mins.spine +
           user_inputs$no.fls.nurse.assessment.mins.spine +
           user_inputs$no.fls.nurse.recommendation.mins.spine +
           user_inputs$no.fls.nurse.monitoring.mins.spine )*
          user_inputs$hourly.cost.nurse/60)+
       ((user_inputs$no.fls.doctor.identification.mins.spine +
           user_inputs$no.fls.doctor.assessment.mins.spine +
           user_inputs$no.fls.doctor.recommendation.mins.spine +
           user_inputs$no.fls.doctor.monitoring.mins.spine )*
          user_inputs$hourly.cost.doctor/60)+
       ((user_inputs$no.fls.radiographer.identification.mins.spine +
           user_inputs$no.fls.radiographer.assessment.mins.spine +
           user_inputs$no.fls.radiographer.recommendation.mins.spine +
           user_inputs$no.fls.radiographer.monitoring.mins.spine )*
          user_inputs$hourly.cost.radiographer/60)+
       ((user_inputs$no.fls.allied_health.identification.mins.spine +
           user_inputs$no.fls.allied_health.assessment.mins.spine +
           user_inputs$no.fls.allied_health.recommendation.mins.spine +
           user_inputs$no.fls.allied_health.monitoring.mins.spine )*
          user_inputs$hourly.cost.allied_health/60)+
       ((user_inputs$no.fls.administrator.identification.mins.spine +
           user_inputs$no.fls.administrator.assessment.mins.spine +
           user_inputs$no.fls.administrator.recommendation.mins.spine +
           user_inputs$no.fls.administrator.monitoring.mins.spine)*
          user_inputs$hourly.cost.administrator/60)+
       ((user_inputs$no.fls.other.identification.mins.spine +
     user_inputs$no.fls.other.assessment.mins.spine +
     user_inputs$no.fls.other.recommendation.mins.spine +
     user_inputs$no.fls.other.monitoring.mins.spine )*
      user_inputs$hourly.cost.other)
    )
  # if all zero
  no.fls.spine.staff.cost<-ifelse(length(no.fls.spine.staff.cost)==0,
                                  0, no.fls.spine.staff.cost)
  no.fls.other.staff.cost<-
    (
       ((user_inputs$no.fls.nurse.identification.mins.other +
           user_inputs$no.fls.nurse.assessment.mins.other +
           user_inputs$no.fls.nurse.recommendation.mins.other +
           user_inputs$no.fls.nurse.monitoring.mins.other )*
          user_inputs$hourly.cost.nurse/60)+
       ((user_inputs$no.fls.doctor.identification.mins.other +
           user_inputs$no.fls.doctor.assessment.mins.other +
           user_inputs$no.fls.doctor.recommendation.mins.other +
           user_inputs$no.fls.doctor.monitoring.mins.other )*
          user_inputs$hourly.cost.doctor/60)+
       ((user_inputs$no.fls.radiographer.identification.mins.other +
           user_inputs$no.fls.radiographer.assessment.mins.other +
           user_inputs$no.fls.radiographer.recommendation.mins.other +
           user_inputs$no.fls.radiographer.monitoring.mins.other )*
          user_inputs$hourly.cost.radiographer/60)+
       ((user_inputs$no.fls.allied_health.identification.mins.other +
           user_inputs$no.fls.allied_health.assessment.mins.other +
           user_inputs$no.fls.allied_health.recommendation.mins.other +
           user_inputs$no.fls.allied_health.monitoring.mins.other )*
          user_inputs$hourly.cost.allied_health/60)+
       ((user_inputs$no.fls.administrator.identification.mins.other +
           user_inputs$no.fls.administrator.assessment.mins.other +
           user_inputs$no.fls.administrator.recommendation.mins.other +
           user_inputs$no.fls.administrator.monitoring.mins.other)*
          user_inputs$hourly.cost.administrator/60)+
       ((user_inputs$no.fls.other.identification.mins.other +
     user_inputs$no.fls.other.assessment.mins.other +
     user_inputs$no.fls.other.recommendation.mins.other +
     user_inputs$no.fls.other.monitoring.mins.other )*
      user_inputs$hourly.cost.other)
    )

  # if all zero
  no.fls.other.staff.cost<-ifelse(length(no.fls.other.staff.cost)==0,
                                  0, no.fls.other.staff.cost)




  # fls
  fls.hip.staff.cost<-
    (
       ((user_inputs$fls.fls_coordinator.identification.mins.hip +
           user_inputs$fls.fls_coordinator.assessment.mins.hip +
           user_inputs$fls.fls_coordinator.recommendation.mins.hip +
           user_inputs$fls.fls_coordinator.monitoring.mins.hip )*
          user_inputs$hourly.cost.fls_coordinator/60)+
       ((user_inputs$fls.nurse.identification.mins.hip +
           user_inputs$fls.nurse.assessment.mins.hip +
           user_inputs$fls.nurse.recommendation.mins.hip +
           user_inputs$fls.nurse.monitoring.mins.hip )*
          user_inputs$hourly.cost.nurse/60)+
       ((user_inputs$fls.doctor.identification.mins.hip +
           user_inputs$fls.doctor.assessment.mins.hip +
           user_inputs$fls.doctor.recommendation.mins.hip +
           user_inputs$fls.doctor.monitoring.mins.hip )*
          user_inputs$hourly.cost.doctor/60)+
       ((user_inputs$fls.radiographer.identification.mins.hip +
           user_inputs$fls.radiographer.assessment.mins.hip +
           user_inputs$fls.radiographer.recommendation.mins.hip +
           user_inputs$fls.radiographer.monitoring.mins.hip )*
          user_inputs$hourly.cost.radiographer/60)+
       ((user_inputs$fls.allied_health.identification.mins.hip +
           user_inputs$fls.allied_health.assessment.mins.hip +
           user_inputs$fls.allied_health.recommendation.mins.hip +
           user_inputs$fls.allied_health.monitoring.mins.hip )*
          user_inputs$hourly.cost.allied_health/60)+
       ((user_inputs$fls.administrator.identification.mins.hip +
           user_inputs$fls.administrator.assessment.mins.hip +
           user_inputs$fls.administrator.recommendation.mins.hip +
           user_inputs$fls.administrator.monitoring.mins.hip)*
          user_inputs$hourly.cost.administrator/60)+
       ((user_inputs$fls.other.identification.mins.hip +
     user_inputs$fls.other.assessment.mins.hip +
     user_inputs$fls.other.recommendation.mins.hip +
     user_inputs$fls.other.monitoring.mins.hip )*
      user_inputs$hourly.cost.other)
    )
  # if all zero
  fls.hip.staff.cost<-ifelse(length(fls.hip.staff.cost)==0,
                             0, fls.hip.staff.cost)

  fls.spine.staff.cost<-
    (       ((user_inputs$fls.fls_coordinator.identification.mins.hip +
           user_inputs$fls.fls_coordinator.assessment.mins.hip +
           user_inputs$fls.fls_coordinator.recommendation.mins.hip +
           user_inputs$fls.fls_coordinator.monitoring.mins.hip )*
          user_inputs$hourly.cost.fls_coordinator/60)+
       ((user_inputs$fls.nurse.identification.mins.spine +
          user_inputs$fls.nurse.assessment.mins.spine +
           user_inputs$fls.nurse.recommendation.mins.spine +
           user_inputs$fls.nurse.monitoring.mins.spine )*
           user_inputs$hourly.cost.nurse/60)+
       ((user_inputs$fls.doctor.identification.mins.spine +
           user_inputs$fls.doctor.assessment.mins.spine +
           user_inputs$fls.doctor.recommendation.mins.spine +
           user_inputs$fls.doctor.monitoring.mins.spine )*
          user_inputs$hourly.cost.doctor/60)+
       ((user_inputs$fls.radiographer.identification.mins.spine +
           user_inputs$fls.radiographer.assessment.mins.spine +
           user_inputs$fls.radiographer.recommendation.mins.spine +
           user_inputs$fls.radiographer.monitoring.mins.spine )*
          user_inputs$hourly.cost.radiographer/60)+
       ((user_inputs$fls.allied_health.identification.mins.spine +
           user_inputs$fls.allied_health.assessment.mins.spine +
           user_inputs$fls.allied_health.recommendation.mins.spine +
           user_inputs$fls.allied_health.monitoring.mins.spine )*
          user_inputs$hourly.cost.allied_health/60)+
       ((user_inputs$fls.administrator.identification.mins.spine +
           user_inputs$fls.administrator.assessment.mins.spine +
           user_inputs$fls.administrator.recommendation.mins.spine +
           user_inputs$fls.administrator.monitoring.mins.spine)*
          user_inputs$hourly.cost.administrator/60)+
       ((user_inputs$fls.other.identification.mins.spine +
     user_inputs$fls.other.assessment.mins.spine +
     user_inputs$fls.other.recommendation.mins.spine +
     user_inputs$fls.other.monitoring.mins.spine )*
      user_inputs$hourly.cost.other)
    )
  # if all zero
  fls.spine.staff.cost<-ifelse(length(fls.spine.staff.cost)==0,
                               0, fls.spine.staff.cost)

  fls.other.staff.cost<-
    (((user_inputs$fls.fls_coordinator.identification.mins.hip +
           user_inputs$fls.fls_coordinator.assessment.mins.hip +
           user_inputs$fls.fls_coordinator.recommendation.mins.hip +
           user_inputs$fls.fls_coordinator.monitoring.mins.hip )*
          user_inputs$hourly.cost.fls_coordinator/60)+
       ((user_inputs$fls.nurse.identification.mins.other +
           user_inputs$fls.nurse.assessment.mins.other +
           user_inputs$fls.nurse.recommendation.mins.other +
           user_inputs$fls.nurse.monitoring.mins.other )*
          user_inputs$hourly.cost.nurse/60)+
       ((user_inputs$fls.doctor.identification.mins.other +
           user_inputs$fls.doctor.assessment.mins.other +
           user_inputs$fls.doctor.recommendation.mins.other +
           user_inputs$fls.doctor.monitoring.mins.other )*
          user_inputs$hourly.cost.doctor/60)+
       ((user_inputs$fls.radiographer.identification.mins.other +
           user_inputs$fls.radiographer.assessment.mins.other +
           user_inputs$fls.radiographer.recommendation.mins.other +
           user_inputs$fls.radiographer.monitoring.mins.other )*
          user_inputs$hourly.cost.radiographer/60)+
       ((user_inputs$fls.allied_health.identification.mins.other +
           user_inputs$fls.allied_health.assessment.mins.other +
           user_inputs$fls.allied_health.recommendation.mins.other +
           user_inputs$fls.allied_health.monitoring.mins.other )*
          user_inputs$hourly.cost.allied_health/60)+
       ((user_inputs$fls.administrator.identification.mins.other +
           user_inputs$fls.administrator.assessment.mins.other +
           user_inputs$fls.administrator.recommendation.mins.other +
           user_inputs$fls.administrator.monitoring.mins.other)*
          user_inputs$hourly.cost.administrator/60)+
       ((user_inputs$fls.other.identification.mins.other +
     user_inputs$fls.other.assessment.mins.other +
     user_inputs$fls.other.recommendation.mins.other +
     user_inputs$fls.other.monitoring.mins.other )*
      user_inputs$hourly.cost.other)
    )
  # if all zero
  fls.other.staff.cost<-ifelse(length(fls.other.staff.cost)==0,
                               0, fls.other.staff.cost)



  # cost when identified- cycle where identified
  # (was not identified in previous period OR
  # new fx in current)
 
  #browser()
  m.TR<-m.TR %>%
    mutate(fx_prev.staff.cost=
             ifelse(identified=="Yes" &
                      (identified.lag=="No"| c_af==1) &
                      recent.fx=="hip" & intervention=="no FLS",
                    no.fls.hip.staff.cost,
                    ifelse(identified=="Yes" &
                             (identified.lag=="No"| c_af==1) &
                             recent.fx=="spine" & intervention=="no FLS",
                           no.fls.spine.staff.cost,
                           ifelse(identified=="Yes" &
                                    (identified.lag=="No"| c_af==1) &
                                    recent.fx=="other" & intervention=="no FLS",
                                  no.fls.other.staff.cost,

                                  ifelse(identified=="Yes" &
                                           (identified.lag=="No"| c_af==1) &
                                           recent.fx=="hip" & intervention=="FLS",
                                         fls.hip.staff.cost,
                                         ifelse(identified=="Yes" &
                                                  (identified.lag=="No"| c_af==1) &
                                                  recent.fx=="spine" & intervention=="FLS",
                                                fls.spine.staff.cost,
                                                ifelse(identified=="Yes" &
                                                         (identified.lag=="No"| c_af==1) &
                                                         recent.fx=="other" & intervention=="FLS",
                                                       fls.other.staff.cost,
                                                       0)))))))

  m.TR<-m.TR %>%
    select(-identified.lag)


  # lab.test and dxa cost ------
  m.TR<-m.TR %>%
    mutate(lab.test.cost=
             ifelse(lab.test==2 & s_hf==1,
                    user_inputs$cost.lab.test.hip,
                    ifelse(lab.test==2 & s_sf==1,
                           user_inputs$cost.lab.test.spine,
                           ifelse(lab.test==2 & s_of==1,
                                  user_inputs$cost.lab.test.other, 0  )))) %>%
    mutate(dxa.cost=
             ifelse(dxa==2,
                    user_inputs$cost.dxa, 0 ))





  # total cost ------
  m.TR$total.cost.excl.location<-
    m.TR$procedure.cost+
    m.TR$hosp.cost+m.TR$comm.cost+
    m.TR$clinic.cost+m.TR$temp.rehab.cost+
    m.TR$discharge.clinic.cost+
    m.TR$medication.cost+m.TR$fx_prev.staff.cost+
    m.TR$lab.test.cost+m.TR$dxa.cost

  m.TR$total.cost<-
    m.TR$procedure.cost+
    m.TR$hosp.cost+m.TR$comm.cost+
    m.TR$clinic.cost+m.TR$temp.rehab.cost+
    m.TR$location.cost+m.TR$discharge.clinic.cost+
    m.TR$medication.cost+m.TR$fx_prev.staff.cost+
    m.TR$lab.test.cost+m.TR$dxa.cost

  m.TR$discounted.total.cost<-m.TR$total.cost/((1.003)^m.TR$year)



  # tidy -----
  m.TR<-m.TR %>%
  mutate(intervention=ifelse(intervention=="no FLS", "Current practice",
                             "FLS")) %>%
  ungroup()   %>%
  mutate(index_fx=str_to_sentence(index_fx),
         sex=str_to_sentence(sex)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
  
  # add qol -----
  qol.max<-1
  qol.min<--0.594
  
  # Worsening in 1 qol associated with fx
  hip.fx.popc<-0.65
  spine.fx.popc<-0.4634831461
  other.fx.popc<-0.3234501348
  
  # Improvement post fracture
  # depends on time since fx
  hip.fx.porc.2_4<- 0.2272727273
  spine.fx.porc.2_4<- 0.2171717172
  other.fx.porc.2_4<- 0.25
  
  hip.fx.porc.5_12<-0.04464285714
  spine.fx.porc.5_12<-0.05434782609
  other.fx.porc.5_12<-0.08333333333
  
  hip.fx.porc.13_18<-0.06481481481
  spine.fx.porc.13_18<-0
  other.fx.porc.13_18<-0.125
  # after 18, individuals remain at the same level
  
  # starting values
  qol.hip.pre<- 0.77
  qol.hip.t0<- -0.11
  
  qol.spine.pre<- 0.83
  qol.spine.t0<- 0.17
  
  qol.other.pre<- 0.89
  qol.other.t0<- 0.41
  
  
  ### for first cycle
  m.TR$qol.max<-NA # the maximum they could improve to if they have a fracture now
  m.TR$qol.max<-ifelse(m.TR$cycle==1 & m.TR$s_hf==1, qol.hip.pre,m.TR$qol.max)
  m.TR$qol.max<-ifelse(m.TR$cycle==1 & m.TR$s_sf==1, qol.spine.pre,m.TR$qol.max)
  m.TR$qol.max<-ifelse(m.TR$cycle==1 & m.TR$s_of==1, qol.other.pre,m.TR$qol.max)
  # max at start is pre
  m.TR$qol.current<-NA
  m.TR$qol.current<-ifelse(m.TR$cycle==1 & m.TR$s_hf==1, qol.hip.t0, m.TR$qol.current)
  m.TR$qol.current<-ifelse(m.TR$cycle==1 & m.TR$s_sf==1, qol.spine.t0, m.TR$qol.current)
  m.TR$qol.current<-ifelse(m.TR$cycle==1 & m.TR$s_of==1, qol.other.t0, m.TR$qol.current)
  
  m.TR$qol.pot.imp<-NA
  m.TR$qol.pot.imp<-ifelse(m.TR$cycle==1,
                           m.TR$qol.max-m.TR$qol.current,
                           m.TR$qol.pot.imp)
  
  # for subsequent cycles
  # i<-2
  for(i in 2:61){
  print(paste0("-- Getting qol for time ", i))  
    # first, get max
    # if no fracture, max stays as before
    m.TR$qol.max<- if_else(m.TR$cycle==i,
                          lag(m.TR$qol.max,1), # will replace below if fx
                          m.TR$qol.max)
    # if fracture, max is now their qol from the period prior to the fracture
    m.TR$qol.max<-if_else(m.TR$cycle==i & m.TR$c_af==1,
                         lag(m.TR$qol.current,1), # previous qol
                         m.TR$qol.max)
    
    
    
    # potential improvement
    m.TR$qol.pot.imp<-if_else(m.TR$cycle==i,
                             lag(m.TR$qol.pot.imp,1),
                             m.TR$qol.pot.imp)
    
    m.TR$qol.pot.imp<-if_else(!is.na(m.TR$t_since_sf) &
                               m.TR$t_since_sf == 4  &
                               m.TR$recent.fx=="spine",
                             m.TR$qol.max-lag(m.TR$qol.current,1),
                             m.TR$qol.pot.imp)
    m.TR$qol.pot.imp<-if_else(!is.na(m.TR$t_since_sf) &
                               m.TR$t_since_sf == 12  &
                               m.TR$recent.fx=="spine",
                             m.TR$qol.max-lag(m.TR$qol.current,1),
                             m.TR$qol.pot.imp)
    m.TR$qol.pot.imp<-if_else(!is.na(m.TR$t_since_sf) &
                               m.TR$t_since_sf == 18  &
                               m.TR$recent.fx=="spine",
                             m.TR$qol.max-lag(m.TR$qol.current,1),
                             m.TR$qol.pot.imp)
    
    
    m.TR$qol.pot.imp<-if_else(!is.na(m.TR$t_since_hf) &
                               m.TR$t_since_hf == 4  &
                               m.TR$recent.fx=="hip",
                             m.TR$qol.max-lag(m.TR$qol.current,1),
                             m.TR$qol.pot.imp)
    m.TR$qol.pot.imp<-if_else(!is.na(m.TR$t_since_hf) &
                               m.TR$t_since_hf == 12  &
                               m.TR$recent.fx=="hip",
                             m.TR$qol.max-lag(m.TR$qol.current,1),
                             m.TR$qol.pot.imp)
    m.TR$qol.pot.imp<-if_else(!is.na(m.TR$t_since_hf) &
                               m.TR$t_since_hf == 18  &
                               m.TR$recent.fx=="hip",
                             m.TR$qol.max-lag(m.TR$qol.current,1),
                             m.TR$qol.pot.imp)
    
    m.TR$qol.pot.imp<-if_else(!is.na(m.TR$t_since_of) &
                               m.TR$t_since_of == 4  &
                               m.TR$recent.fx=="other",
                             m.TR$qol.max-lag(m.TR$qol.current,1),
                             m.TR$qol.pot.imp)
    m.TR$qol.pot.imp<-if_else(!is.na(m.TR$t_since_of) &
                               m.TR$t_since_of == 12  &
                               m.TR$recent.fx=="other",
                             m.TR$qol.max-lag(m.TR$qol.current,1),
                             m.TR$qol.pot.imp)
    m.TR$qol.pot.imp<-if_else(!is.na(m.TR$t_since_of) &
                               m.TR$t_since_of == 18  &
                               m.TR$recent.fx=="other",
                             m.TR$qol.max-lag(m.TR$qol.current,1),
                             m.TR$qol.pot.imp)
    
    
    
    # next, get current qol
    # if fracture in current cycle
    # individual worsen for PoPC of fx type
    m.TR$qol.current<-if_else(m.TR$cycle==i &
                               m.TR$s_hf==1,
                             lag(m.TR$qol.current,1)-(hip.fx.popc*(lag(m.TR$qol.current,1)-qol.min)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(m.TR$cycle==i &
                               m.TR$s_sf==1,
                             lag(m.TR$qol.current,1)-(spine.fx.popc*(lag(m.TR$qol.current,1)-qol.min)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(m.TR$cycle==i &
                               m.TR$s_of==1,
                             lag(m.TR$qol.current,1)-(other.fx.popc*(lag(m.TR$qol.current,1)-qol.min)),
                             m.TR$qol.current)
    
    # if no fracture in current cycle
    # an individual improves based on time since last fracture
    # type of last fracture
    # the popc associated with their last fracture
    
    # recent.fx=="spine"
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_sf) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_sf %in% c(1:3)  &
                               m.TR$recent.fx=="spine",
                             lag(m.TR$qol.current,1)+
                               (spine.fx.porc.2_4*
                                  (m.TR$qol.pot.imp)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_sf) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_sf %in% c(4:11)  &
                               m.TR$recent.fx=="spine",
                             lag(m.TR$qol.current,1)+
                               (spine.fx.porc.5_12*
                                  (m.TR$qol.pot.imp)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_sf) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_sf %in% c(12:17)  &
                               m.TR$recent.fx=="spine",
                             lag(m.TR$qol.current,1)+
                               (spine.fx.porc.13_18*
                                  (m.TR$qol.pot.imp)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_sf) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_sf %in% c(18:61)  &
                               m.TR$recent.fx=="spine",
                             lag(m.TR$qol.current,1),
                             m.TR$qol.current)
    
    
    # recent.fx=="hip"
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_hf) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_hf %in% c(1:3)  &
                               m.TR$recent.fx=="hip",
                             lag(m.TR$qol.current,1)+
                               (hip.fx.porc.2_4*
                                  (m.TR$qol.pot.imp)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_hf) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_hf %in% c(4:11)  &
                               m.TR$recent.fx=="hip",
                             lag(m.TR$qol.current,1)+
                               (hip.fx.porc.5_12*
                                  (m.TR$qol.pot.imp)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_hf) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_hf %in% c(12:17)  &
                               m.TR$recent.fx=="hip",
                             lag(m.TR$qol.current,1)+
                               (hip.fx.porc.13_18*
                                  (m.TR$qol.pot.imp)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_hf) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_hf %in% c(18:61)  &
                               m.TR$recent.fx=="hip",
                             lag(m.TR$qol.current,1),
                             m.TR$qol.current)
    
    # recent.fx=="other"
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_of) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_of %in% c(1:3)  &
                               m.TR$recent.fx=="other",
                             lag(m.TR$qol.current,1)+
                               (other.fx.porc.2_4*
                                  (m.TR$qol.pot.imp)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_of) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_of %in% c(4:11)  &
                               m.TR$recent.fx=="other",
                             lag(m.TR$qol.current,1)+
                               (other.fx.porc.5_12*
                                  (m.TR$qol.pot.imp)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_of) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_of %in% c(12:17)  &
                               m.TR$recent.fx=="other",
                             lag(m.TR$qol.current,1)+
                               (other.fx.porc.13_18*
                                  (m.TR$qol.pot.imp)),
                             m.TR$qol.current)
    m.TR$qol.current<-if_else(!is.na(m.TR$t_since_of) &
                               m.TR$c_af!=1 &
                               m.TR$t_since_of %in% c(18:61)  &
                               m.TR$recent.fx=="other",
                             lag(m.TR$qol.current,1),
                             m.TR$qol.current)
    
  }
  
  
  # m.TR<-m.TR %>% select(-qol)
  m.TR<-m.TR %>%
    rename("qol"="qol.current")
  
  # if died, set to zero
  m.TR<-m.TR %>%
    mutate(qol=ifelse(m.TR$s_d==1, 0, qol))
  
  
  m.TR$discounted.qol<-m.TR$qol/((1.003)^m.TR$year)
  
  # m.TR %>%
  #   group_by(month, index_fx,intervention) %>%
  #   summarise(qol=mean(qol)) %>%
  #   ggplot()+
  #    facet_grid(index_fx~ intervention)+
  #   geom_line(aes(month,qol, colour=index_fx), linetype="dashed")+
  #   geom_point(aes(month,qol, colour=index_fx))+
  #   theme_bw()
  #
  # m.TR %>%
  #   group_by(month, index_fx, intervention) %>%
  #   summarise(qol=mean(qol)) %>%
  #   ggplot()+
  #   facet_wrap(vars(index_fx))+
  #   geom_line(aes(month,qol, colour=intervention), linetype="dashed")+
  #   geom_point(aes(month,qol, colour=intervention))+
  #   theme_bw()
  
  
  # m.TR %>%
  #   group_by(month, index_fx, intervention,s_d) %>%
  #   summarise(qol=mean(qol)) %>%
  #   filter(s_d==0) %>%
  #   ggplot()+
  #   facet_wrap(vars(index_fx))+
  #   geom_line(aes(month,qol, colour=intervention), linetype="dashed")+
  #   geom_point(aes(month,qol, colour=intervention))+
  #   theme_bw()
  
  #
  #
  # a<-m.TR %>%
  #   group_by(index_fx, id, intervention) %>%
  #   summarise(fx=sum(c_af),
  #             s_d=sum(s_d),
  #             qol=sum(qol)/length(index_fx))
  #
  # b<-a %>%
  #   filter(s_d==0) %>%
  #   group_by(index_fx, fx,intervention) %>%
  #   summarise(qol=sum(qol)/length(index_fx))
  
  
  # save ----
  saveRDS("m.TR",
       file=paste0(results.folder, "/results.m.TR.trace.complete.rds"))
gc()
  