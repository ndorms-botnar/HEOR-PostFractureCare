
get.output<-function(m.TR, with.markov.trace=TRUE){

working.microsim_pop<-microsim_pop %>%
    ungroup()   %>%
  mutate(index_fx=str_to_sentence(index_fx),
         sex=str_to_sentence(sex)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" ))) %>% 
    mutate(risk.type=ifelse(risk.type==1, "Low risk",
                     ifelse(risk.type==2, "Intermediate risk",
                     ifelse(risk.type==3, "High risk", NA)))) %>%
  mutate(risk.type=factor(risk.type,
                          levels=c("Low risk","Intermediate risk","High risk")))


# list
output<-list()
###
output[["country.name"]]<-user_inputs$country.name
output[["region.name"]]<-user_inputs$region.name
output[["specified.fx"]]<-  if(user_inputs$checkbox_index_fx_option_1==TRUE){
    "Specified all index fractures"
  } else if(user_inputs$checkbox_index_fx_option_2==TRUE){
 "Specified only hip index fractures"
  } else{
    "Index fractures estimated based on size of general population"
  }
output[["n_microsimulation"]]<- n_microsimulation
## 1) HEALTH OUTCOMES TABLES ------
# identified  ------
# index_fx, sex
output[["summary.m.TR.identified.over_time.index_fx.sex"]]<-
m.TR %>%
  ungroup() %>%
  mutate(identified=factor(identified,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention,index_fx, sex, identified, .drop=FALSE) %>%
  tally(name="n.identified")%>%
 # add total n
   left_join(
     working.microsim_pop %>%
       group_by(index_fx, sex, .drop=FALSE) %>%
       tally(name="n_microsim")) %>%
 ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx),
         sex=str_to_sentence(sex)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# index_fx
output[["summary.m.TR.identified.over_time.index_fx"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(identified=factor(identified,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention,index_fx, identified, .drop=FALSE) %>%
  tally(name="n.identified")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx,.drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup() %>%
  ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other")))
# sex
output[["summary.m.TR.identified.over_time.sex"]]<- m.TR %>%
  ungroup() %>%
  mutate(identified=factor(identified,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention, sex, identified, .drop=FALSE) %>%
  tally(name="n.identified")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup() %>%
  ungroup()   %>%mutate(sex=str_to_sentence(sex)) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# overall
output[["summary.m.TR.identified.over_time.overall"]]<- m.TR %>%
  ungroup() %>%
  mutate(identified=factor(identified,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention, identified, .drop=FALSE) %>%
  tally(name="n.identified")%>%
  mutate(n_microsim=working.microsim_pop %>%   tally(name="n_microsim") %>% pull())

# treat  ------
# index_fx, sex
output[["summary.m.TR.treat.over_time.index_fx.sex"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(treat=factor(treat,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention,index_fx, sex, treat, .drop=FALSE) %>%
  tally(name="n.treat")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx, sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
 ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx),
         sex=str_to_sentence(sex)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# index_fx
output[["summary.m.TR.treat.over_time.index_fx"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(treat=factor(treat,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention,index_fx, treat, .drop=FALSE) %>%
  tally(name="n.treat")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx,.drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other")))
# sex
output[["summary.m.TR.treat.over_time.sex"]]<- m.TR %>%
  ungroup() %>%
  mutate(treat=factor(treat,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention, sex, treat, .drop=FALSE) %>%
  tally(name="n.treat")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%ungroup()   %>%mutate(sex=str_to_sentence(sex)) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# overall
output[["summary.m.TR.treat.over_time.overall"]]<- m.TR %>%
  ungroup() %>%
  mutate(treat=factor(treat,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention, treat, .drop=FALSE) %>%
  tally(name="n.treat")%>%
  mutate(n_microsim=working.microsim_pop %>%   tally(name="n_microsim") %>% pull())


# treat.risk.profile  ------
# index_fx, sex
output[["summary.m.TR.treat.over_time.index_fx.sex.risk.profile"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(treat=factor(treat,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention,risk.type,index_fx, sex, treat, .drop=FALSE) %>%
  tally(name="n.treat")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(risk.type,index_fx, sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
 ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx),
         sex=str_to_sentence(sex)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# index_fx
output[["summary.m.TR.treat.over_time.index_fx.risk.type"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(treat=factor(treat,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention,risk.type,index_fx, treat, .drop=FALSE) %>%
  tally(name="n.treat")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(risk.type,index_fx,.drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other")))
# sex
output[["summary.m.TR.treat.over_time.sex.risk.type"]]<- m.TR %>%
  ungroup() %>%
  mutate(treat=factor(treat,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention,risk.type, sex, treat, .drop=FALSE) %>%
  tally(name="n.treat")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(risk.type, sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%ungroup()   %>%mutate(sex=str_to_sentence(sex)) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# risk.type
output[["summary.m.TR.treat.over_time.risk.type"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(treat=factor(treat,
                           levels=c("No", "Yes"))) %>%
  group_by(month,intervention,risk.type, treat, .drop=FALSE) %>%
  tally(name="n.treat")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(risk.type,.drop=FALSE) %>%
      tally(name="n_microsim"))



# adhering  ------
# index_fx, sex
output[["summary.m.TR.adhering.over_time.index_fx.sex"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(adhering=factor(adhering,
                      levels=c("No (died)", "No", "Yes"))) %>%
  group_by(month,intervention,index_fx, sex, adhering, .drop=FALSE) %>%
  tally(name="n.adhering")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx, sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
 ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx),
         sex=str_to_sentence(sex)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# index_fx
output[["summary.m.TR.adhering.over_time.index_fx"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(adhering=factor(adhering,
                      levels=c("No (died)", "No", "Yes"))) %>%
  group_by(month,intervention,index_fx, adhering, .drop=FALSE) %>%
  tally(name="n.adhering")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx,.drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other")))
# sex
output[["summary.m.TR.adhering.over_time.sex"]]<- m.TR %>%
  ungroup() %>%
  mutate(adhering=factor(adhering,
                      levels=c("No (died)", "No", "Yes"))) %>%
  group_by(month,intervention, sex, adhering, .drop=FALSE) %>%
  tally(name="n.adhering")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(sex=str_to_sentence(sex)) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# overall
output[["summary.m.TR.adhering.over_time.overall"]]<- m.TR %>%
  ungroup() %>%
  mutate(adhering=factor(adhering,
                      levels=c("No (died)", "No", "Yes"))) %>%
  group_by(month,intervention, adhering, .drop=FALSE) %>%
  tally(name="n.adhering")%>%
  mutate(n_microsim=working.microsim_pop %>%   tally(name="n_microsim") %>% pull())



# apply.rr  ------
# index_fx, sex
output[["summary.m.TR.apply.rr.over_time.index_fx.sex"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(apply.rr=factor(apply.rr,
                      levels=c("No", "Yes"))) %>%
  group_by(month,intervention,index_fx, sex, apply.rr, .drop=FALSE) %>%
  tally(name="n.apply.rr")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx, sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
 ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx),
         sex=str_to_sentence(sex)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# index_fx
output[["summary.m.TR.apply.rr.over_time.index_fx"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(apply.rr=factor(apply.rr,
                      levels=c("No", "Yes"))) %>%
  group_by(month,intervention,index_fx, apply.rr, .drop=FALSE) %>%
  tally(name="n.apply.rr")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx,.drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other")))
# sex
output[["summary.m.TR.apply.rr.over_time.sex"]]<- m.TR %>%
  ungroup() %>%
  mutate(apply.rr=factor(apply.rr,
                      levels=c("No", "Yes"))) %>%
  group_by(month,intervention, sex, apply.rr, .drop=FALSE) %>%
  tally(name="n.apply.rr")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(sex=str_to_sentence(sex))
# overall
output[["summary.m.TR.apply.rr.over_time.overall"]]<- m.TR %>%
  ungroup() %>%
  mutate(apply.rr=factor(apply.rr,
                      levels=c("No", "Yes"))) %>%
  group_by(month,intervention, apply.rr, .drop=FALSE) %>%
  tally(name="n.apply.rr")%>%
  mutate(n_microsim=working.microsim_pop %>%   tally(name="n_microsim") %>% pull())



# medication -----
output[["summary.m.TR.medication.over_time.index_fx.sex"]]<-m.TR %>%
  ungroup() %>%
  mutate(medication=str_to_sentence(medication)) %>%
  mutate(medication=factor(medication,
                           levels=c("No drug (died)", "No drug",
                                    "Alendronate",
                                    "Risedronate","Strontium",
                                    "Ibandronate","Raloxifene",
                                    "Denosumab",
                                    "Zoledronate","Teriparatide",
                                    "Abaloparatide","Romosozumab")))  %>%
  group_by(month,intervention,index_fx, sex, medication, .drop=FALSE) %>%
  tally(name="n.medication")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx, sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
 ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx),
         sex=str_to_sentence(sex)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))

# index_fx
output[["summary.m.TR.medication.over_time.index_fx"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(medication=str_to_sentence(medication)) %>%
  mutate(medication=factor(medication,
                           levels=c("No drug (died)", "No drug",
                                    "Alendronate",
                                    "Risedronate","Strontium",
                                    "Ibandronate","Raloxifene",
                                    "Denosumab",
                                    "Zoledronate","Teriparatide",
                                    "Abaloparatide","Romosozumab")))  %>%
  group_by(month,intervention,index_fx, medication, .drop=FALSE) %>%
  tally(name="n.medication")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx,.drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other")))
# sex
output[["summary.m.TR.medication.over_time.sex"]]<- m.TR %>%
  ungroup() %>%
  mutate(medication=str_to_sentence(medication)) %>%
  mutate(medication=factor(medication,
                           levels=c("No drug (died)", "No drug",
                                     "Alendronate",
                                    "Risedronate","Strontium",
                                    "Ibandronate","Raloxifene",
                                    "Denosumab",
                                    "Zoledronate","Teriparatide",
                                    "Abaloparatide","Romosozumab")))  %>%
  group_by(month,intervention, sex, medication, .drop=FALSE) %>%
  tally(name="n.medication")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(sex=str_to_sentence(sex)) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# overall
output[["summary.m.TR.medication.over_time.overall"]]<- m.TR %>%
  ungroup() %>%
  mutate(medication=str_to_sentence(medication)) %>%
  mutate(medication=factor(medication,
                           levels=c("No drug (died)", "No drug",
                                    "Alendronate",
                                    "Risedronate","Strontium",
                                    "Ibandronate","Raloxifene",
                                    "Denosumab",
                                    "Zoledronate","Teriparatide",
                                    "Abaloparatide","Romosozumab")))  %>%
  group_by(month,intervention, medication, .drop=FALSE) %>%
  tally(name="n.medication")%>%
  mutate(n_microsim=working.microsim_pop %>%   tally(name="n_microsim") %>% pull())





## HEALTH OUTCOMES PLOTS ------
# identified plot -----
output[["identified.plot.overall"]]<-
  output$summary.m.TR.identified.over_time.overall %>%
    ggplot()+
    facet_grid(.~ intervention)+
    geom_col(aes(month, n.identified/n_microsim,
                 fill=as.character(identified)), width=1)
output[["identified.plot.overall"]]<-gg.general.format(output[["identified.plot.overall"]])
output[["identified.plot.overall"]]<-gg.add.colours.y_n(output[["identified.plot.overall"]])

output[["identified.plot.index_fx"]]<-
  output$summary.m.TR.identified.over_time.index_fx %>%
    ggplot()+
    facet_grid(index_fx
               ~ intervention)+
    geom_col(aes(month, n.identified/n_microsim,
                 fill=as.character(identified)), width=1)
output[["identified.plot.index_fx"]]<-gg.general.format(output[["identified.plot.index_fx"]])
output[["identified.plot.index_fx"]]<-gg.add.colours.y_n(output[["identified.plot.index_fx"]])

output[["identified.plot.sex"]]<-
 output$summary.m.TR.identified.over_time.sex %>%
    ggplot()+
    facet_grid(sex~ intervention)+
    geom_col(aes(month, n.identified/n_microsim,
                 fill=as.character(identified)), width=1)
output[["identified.plot.sex"]]<-gg.general.format(output[["identified.plot.sex"]])
output[["identified.plot.sex"]]<-gg.add.colours.y_n(output[["identified.plot.sex"]])


output[["identified.plot.index_fx.sex"]]<-
  output$summary.m.TR.identified.over_time.index_fx.sex %>%
  ggplot()+
  facet_grid(index_fx+ sex~ intervention)+
  geom_col(aes(month, n.identified/n_microsim,
               fill=as.character(identified)), width=1)
output[["identified.plot.index_fx.sex"]]<-gg.general.format(output[["identified.plot.index_fx.sex"]])
output[["identified.plot.index_fx.sex"]]<-gg.add.colours.y_n(output[["identified.plot.index_fx.sex"]])


# treat plot -----
output[["treat.plot.overall"]]<-
  output$summary.m.TR.treat.over_time.overall %>%
    ggplot()+
    facet_grid(.~ intervention)+
    geom_col(aes(month, n.treat/n_microsim,
                 fill=as.character(treat)), width=1)
output[["treat.plot.overall"]]<-gg.general.format(output[["treat.plot.overall"]])
output[["treat.plot.overall"]]<-gg.add.colours.y_n(output[["treat.plot.overall"]])

output[["treat.plot.index_fx"]]<-
  output$summary.m.TR.treat.over_time.index_fx %>%
    ggplot()+
    facet_grid(index_fx~ intervention)+
    geom_col(aes(month, n.treat/n_microsim,
                 fill=as.character(treat)), width=1)
output[["treat.plot.index_fx"]]<-gg.general.format(output[["treat.plot.index_fx"]])
output[["treat.plot.index_fx"]]<-gg.add.colours.y_n(output[["treat.plot.index_fx"]])

output[["treat.plot.sex"]]<-
 output$summary.m.TR.treat.over_time.sex %>%
    ggplot()+
    facet_grid(sex~ intervention)+
    geom_col(aes(month, n.treat/n_microsim,
                 fill=as.character(treat)), width=1)
output[["treat.plot.sex"]]<-gg.general.format(output[["treat.plot.sex"]])
output[["treat.plot.sex"]]<-gg.add.colours.y_n(output[["treat.plot.sex"]])


output[["treat.plot.index_fx.sex"]]<-
  output$summary.m.TR.treat.over_time.index_fx.sex %>%
  ggplot()+
  facet_grid(index_fx+ sex~ intervention)+
  geom_col(aes(month, n.treat/n_microsim,
               fill=as.character(treat)), width=1)
output[["treat.plot.index_fx.sex"]]<-gg.general.format(output[["treat.plot.index_fx.sex"]])
output[["treat.plot.index_fx.sex"]]<-gg.add.colours.y_n(output[["treat.plot.index_fx.sex"]])



# treat plot risk.profile -----

output[["treat.plot.risk.profile"]]<-
  output$summary.m.TR.treat.over_time.risk.type %>%
    ggplot()+
    facet_grid(risk.type~ intervention)+
    geom_col(aes(month, n.treat/n_microsim,
                 fill=as.character(treat)), width=1)
output[["treat.plot.risk.profile"]]<-gg.general.format(output[["treat.plot.risk.profile"]])
output[["treat.plot.risk.profile"]]<-gg.add.colours.y_n(output[["treat.plot.risk.profile"]])

output[["treat.plot.sex"]]<-
 output$summary.m.TR.treat.over_time.sex.risk.type %>%
    ggplot()+
    facet_grid(risk.type+sex~ intervention)+
    geom_col(aes(month, n.treat/n_microsim,
                 fill=as.character(treat)), width=1)
output[["treat.plot.sex"]]<-gg.general.format(output[["treat.plot.sex"]])
output[["treat.plot.sex"]]<-gg.add.colours.y_n(output[["treat.plot.sex"]])






# adhering plot -----
output[["adhering.plot.overall"]]<-
  output$summary.m.TR.adhering.over_time.overall %>%
    ggplot()+
    facet_grid(.~ intervention)+
    geom_col(aes(month, n.adhering/n_microsim,
                 fill=adhering), width=1)
output[["adhering.plot.overall"]]<-gg.general.format(output[["adhering.plot.overall"]])
output[["adhering.plot.overall"]]<-gg.add.colours.adh(output[["adhering.plot.overall"]])

output[["adhering.plot.index_fx"]]<-
  output$summary.m.TR.adhering.over_time.index_fx %>%
    ggplot()+
    facet_grid(index_fx~ intervention)+
    geom_col(aes(month, n.adhering/n_microsim,
                 fill=adhering), width=1)
output[["adhering.plot.index_fx"]]<-gg.general.format(output[["adhering.plot.index_fx"]])
output[["adhering.plot.index_fx"]]<-gg.add.colours.adh(output[["adhering.plot.index_fx"]])

output[["adhering.plot.sex"]]<-
 output$summary.m.TR.adhering.over_time.sex %>%
    ggplot()+
    facet_grid(sex~ intervention)+
    geom_col(aes(month, n.adhering/n_microsim,
                 fill=adhering), width=1)
output[["adhering.plot.sex"]]<-gg.general.format(output[["adhering.plot.sex"]])
output[["adhering.plot.sex"]]<-gg.add.colours.adh(output[["adhering.plot.sex"]])


output[["adhering.plot.index_fx.sex"]]<-
  output$summary.m.TR.adhering.over_time.index_fx.sex %>%
  ggplot()+
  facet_grid(index_fx+ sex~ intervention)+
  geom_col(aes(month, n.adhering/n_microsim,
               fill=adhering), width=1)
output[["adhering.plot.index_fx.sex"]]<-gg.general.format(output[["adhering.plot.index_fx.sex"]])
output[["adhering.plot.index_fx.sex"]]<-gg.add.colours.adh(output[["adhering.plot.index_fx.sex"]])




# apply.rr plot -----
output[["apply.rr.plot.overall"]]<-
  output$summary.m.TR.apply.rr.over_time.overall %>%
    ggplot()+
    facet_grid(.~ intervention)+
    geom_col(aes(month, n.apply.rr/n_microsim,
                 fill=as.character(apply.rr)), width=1)
output[["apply.rr.plot.overall"]]<-gg.general.format(output[["apply.rr.plot.overall"]])
output[["apply.rr.plot.overall"]]<-gg.add.colours.y_n(output[["apply.rr.plot.overall"]])

output[["apply.rr.plot.index_fx"]]<-
  output$summary.m.TR.apply.rr.over_time.index_fx %>%
    ggplot()+
    facet_grid(index_fx~ intervention)+
    geom_col(aes(month, n.apply.rr/n_microsim,
                 fill=as.character(apply.rr)), width=1)
output[["apply.rr.plot.index_fx"]]<-gg.general.format(output[["apply.rr.plot.index_fx"]])
output[["apply.rr.plot.index_fx"]]<-gg.add.colours.y_n(output[["apply.rr.plot.index_fx"]])

output[["apply.rr.plot.sex"]]<-
 output$summary.m.TR.apply.rr.over_time.sex %>%
    ggplot()+
    facet_grid(sex~ intervention)+
    geom_col(aes(month, n.apply.rr/n_microsim,
                 fill=as.character(apply.rr)), width=1)
output[["apply.rr.plot.sex"]]<-gg.general.format(output[["apply.rr.plot.sex"]])
output[["apply.rr.plot.sex"]]<-gg.add.colours.y_n(output[["apply.rr.plot.sex"]])


output[["apply.rr.plot.index_fx.sex"]]<-
  output$summary.m.TR.apply.rr.over_time.index_fx.sex %>%
  ggplot()+
  facet_grid(index_fx+ sex~ intervention)+
  geom_col(aes(month, n.apply.rr/n_microsim,
               fill=as.character(apply.rr)), width=1)
output[["apply.rr.plot.index_fx.sex"]]<-gg.general.format(output[["apply.rr.plot.index_fx.sex"]])
output[["apply.rr.plot.index_fx.sex"]]<-gg.add.colours.y_n(output[["apply.rr.plot.index_fx.sex"]])





# medication plot -----
# drop unused medications
unused<-output$summary.m.TR.medication.over_time.overall %>%
  group_by(medication) %>%
  summarise(n=sum(n.medication)) %>%
  filter(n==0) %>%
  mutate(medication=as.character(medication)) %>%
  select(medication)

output[["medication.plot.overall"]]<-
output$summary.m.TR.medication.over_time.overall   %>%
   anti_join(unused) %>%
      ggplot()+
      facet_grid(.~ intervention)+
      geom_col(aes(month, n.medication/n_microsim,
                   fill=medication), width=1)
output[["medication.plot.overall"]]<-gg.general.format(output[["medication.plot.overall"]])
output[["medication.plot.overall"]]<-gg.add.colours.meds(output[["medication.plot.overall"]])


output[["medication.plot.index_fx"]]<-
output$summary.m.TR.medication.over_time.index_fx   %>%
   anti_join(unused)  %>%
      ggplot()+
      facet_grid(index_fx~ intervention)+
      geom_col(aes(month, n.medication/n_microsim,
                   fill=medication), width=1)
output[["medication.plot.index_fx"]]<-gg.general.format(output[["medication.plot.index_fx"]])
output[["medication.plot.index_fx"]]<-gg.add.colours.meds(output[["medication.plot.index_fx"]])

output[["medication.plot.sex"]]<-
output$summary.m.TR.medication.over_time.sex   %>%
   anti_join(unused)  %>%
      ggplot()+
      facet_grid(sex~ intervention)+
      geom_col(aes(month, n.medication/n_microsim,
                   fill=medication), width=1)
output[["medication.plot.sex"]]<-gg.general.format(output[["medication.plot.sex"]])
output[["medication.plot.sex"]]<-gg.add.colours.meds(output[["medication.plot.sex"]])

output[["medication.plot.index_fx.sex"]]<-
output$summary.m.TR.medication.over_time.index_fx.sex   %>%
   anti_join(unused)  %>%
      ggplot()+
      facet_grid(index_fx+ sex~ intervention)+
      geom_col(aes(month, n.medication/n_microsim,
                   fill=medication), width=1)
output[["medication.plot.index_fx.sex"]]<-gg.general.format(output[["medication.plot.index_fx.sex"]])
output[["medication.plot.index_fx.sex"]]<-gg.add.colours.meds(output[["medication.plot.index_fx.sex"]])


## SECOND FX SUMMARY ONE YEAR ----
user_inputs$sec.frac.time<-12

user_inputs$choose.sec.frac.summary<-"Overall"
output[["summary.second.fx.t12"]]<- get.summary.second.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)



user_inputs$choose.sec.frac.summary<-"By sentinel fracture"
output[["summary.second.fx.t12.index_fx"]]<- get.summary.second.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)
user_inputs$choose.sec.frac.summary<-"By sex"
output[["summary.second.fx.t12.sex"]]<- get.summary.second.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)

user_inputs$choose.sec.frac.summary<-"By sentinel fracture and sex"
output[["summary.second.fx.t12.index_fx.sex"]]<- get.summary.second.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)


## SECOND FX SUMMARY FIVE YEARS ----
user_inputs$sec.frac.time<-60

user_inputs$choose.sec.frac.summary<-"Overall"
output[["summary.second.fx.t60"]]<- get.summary.second.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)



user_inputs$choose.sec.frac.summary<-"By sentinel fracture"
output[["summary.second.fx.t60.index_fx"]]<- get.summary.second.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)
user_inputs$choose.sec.frac.summary<-"By sex"
output[["summary.second.fx.t60.sex"]]<- get.summary.second.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)

user_inputs$choose.sec.frac.summary<-"By sentinel fracture and sex"
output[["summary.second.fx.t60.index_fx.sex"]]<- get.summary.second.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)

## SECOND FX PLOT OVER TIME -----
get.sub.fracs.to.plot<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){



sec.frac.time<-60

sub.frac.over.time<-NULL

for (i in 1:60) {

sub.frac<-m.TR %>%
  filter(month=={{i}}) %>% #last month of time horizon
  mutate(sub_hip.frac=ifelse(index_fx=="Hip", h_hf-1, h_hf), # subsequent hip fx, minus index (if their index was hip)
         sub_spine.frac=ifelse(index_fx=="Spine", h_sf-1, h_sf),
         sub_other.frac=ifelse(index_fx=="Other", h_of-1, h_of),
         sub_frac=h_af-1) %>%  # total fx, minus first fx
  select(id, index_fx, sex, intervention, sub_hip.frac, sub_spine.frac,sub_other.frac, sub_frac) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

sub.frac<-sub.frac %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(
    sum.sub_hip.frac=sum(sub_hip.frac),
    sum.sub_spine.frac=sum(sub_spine.frac),
    sum.sub_other.frac=sum(sub_other.frac),
    sum.sub.frac=sum(sub_frac))

sub.frac.over.time<-
  rbind(sub.frac.over.time,
        sub.frac %>% mutate(time=i))


}

sub.frac<-sub.frac.over.time

sub.frac$microsim_pop<-n_microsimulation


# add study pop
sub.frac$study.pop.n<-NA
sub.frac<-sub.frac %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
sub.frac<-sub.frac %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
sub.frac<-sub.frac %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

sub.frac<-sub.frac %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
sub.frac<-sub.frac %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
sub.frac<-sub.frac %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted number for study population
sub.frac$study.pop.sub.hip.frac<- round(sub.frac$sum.sub_hip.frac*
  (sub.frac$study.pop.n/sub.frac$microsim_pop))
sub.frac$study.pop.sub.spine.frac<- round(sub.frac$sum.sub_spine.frac*
  (sub.frac$study.pop.n/sub.frac$microsim_pop))
sub.frac$study.pop.sub.other.frac<- round(sub.frac$sum.sub_other.frac*
  (sub.frac$study.pop.n/sub.frac$microsim_pop))
sub.frac$study.pop.sub.frac<- round(sub.frac$sum.sub.frac*
  (sub.frac$study.pop.n/sub.frac$microsim_pop))

sub.frac<-sub.frac %>%
  select(intervention, index_fx, sex, study.pop.n, time,
         study.pop.sub.hip.frac,study.pop.sub.spine.frac,study.pop.sub.other.frac,
         study.pop.sub.frac)


#output -----
sub.frac
}

sub.frac<-get.sub.fracs.to.plot(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)

sub.frac.long<-sub.frac %>%
  select(intervention,index_fx,sex,time,
         study.pop.sub.hip.frac,
         study.pop.sub.spine.frac,
         study.pop.sub.other.frac) %>%
 pivot_longer(
   cols = starts_with("study.pop.sub."),
   names_to = "type",
   values_to = "n")
sub.frac.long<-sub.frac.long %>%
  mutate(type=ifelse(type=="study.pop.sub.hip.frac", "Hip",
              ifelse(type=="study.pop.sub.spine.frac", "Spine",
              ifelse(type=="study.pop.sub.other.frac", "Other",
                      NA)))) %>%
  mutate(type=factor(type,
                        levels=c("Hip", "Spine", "Other")))


# 
# y1_y2 -----
table<- inner_join(
   sub.frac.long %>% 
  filter(time== 12) %>% 
  mutate(n=as.numeric(n)) %>% 
  group_by(intervention) %>% 
  summarise(y1=sum(n)) , 
sub.frac.long %>% 
  filter(time== 24) %>% 
  mutate(n=as.numeric(n)) %>% 
  group_by(intervention) %>% 
  summarise(y2=sum(n))) %>% 
   mutate(y1_y2=y2-y1) %>% 
  select( intervention , y1_y2)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,2]- table[2,2])
 
 output[["summary.second.fx.t12_t24"]]<-kable(table,
       col.names = c( "Intervention",
                     # "N target population",
                     "Subsequent fractures",
                     "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 

 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 12) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 24) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y1_y2=y2-y1) %>% 
   select(index_fx, intervention ,  y1_y2) %>% 
   arrange(index_fx,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,3]- table[2,3])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,3]- table[4,3])
 }
 
 if(nrow(table)>5){
   table$difference[6]<-as.numeric(table[5,3]- table[6,3])
 }
 
 output[["summary.second.fx.t12_t24.index_fx"]]<-kable(table,
                                              col.names = c( "Index fracture",
                                                "Intervention",
                                                             # "N target population",
                                                             "Subsequent fractures",
                                                             "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 12) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, sex) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 24) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, sex) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y1_y2=y2-y1) %>% 
   select(sex, intervention ,  y1_y2) %>% 
   arrange(sex,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,3]- table[2,3])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,3]- table[4,3])
 }

 output[["summary.second.fx.t12_t24.sex"]]<-kable(table,
                                              col.names = c( "Sex",
                                                             "Intervention",
                                                             # "N target population",
                                                             "Subsequent fractures",
                                                             "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 12) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx, sex) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 24) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx, sex) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y1_y2=y2-y1) %>% 
   select(index_fx, sex, intervention ,  y1_y2) %>% 
   arrange(index_fx, sex,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,4]- table[2,4])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,4]- table[4,4])
 }
 
 if(nrow(table)>5){
   table$difference[6]<-as.numeric(table[5,4]- table[6,4])
 }
 
 if(nrow(table)>7){
   table$difference[8]<-as.numeric(table[7,4]- table[8,4])
 }
 
 if(nrow(table)>9){
   table$difference[10]<-as.numeric(table[9,4]- table[10,4])
 }
 
 if(nrow(table)>11){
   table$difference[12]<-as.numeric(table[11,4]- table[12,4])
 }
 
 output[["summary.second.fx.t12_t24.index_fx.sex"]]<-kable(table,
                                              col.names = c( "Index fracture",
                                                             "Sex",
                                                             "Intervention",
                                                             # "N target population",
                                                             "Subsequent fractures",
                                                             "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 
 
 
# y2_y3 -----
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 24) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 36) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y2_y3=y2-y1) %>% 
   select( intervention , y2_y3)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,2]- table[2,2])
 
 output[["summary.second.fx.t24_t36"]]<-kable(table,
                                              col.names = c( "Intervention",
                                                             # "N target population",
                                                             "Subsequent fractures",
                                                             "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 24) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 36) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y2_y3=y2-y1) %>% 
   select(index_fx, intervention ,  y2_y3) %>% 
   arrange(index_fx,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,3]- table[2,3])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,3]- table[4,3])
 }
 
 if(nrow(table)>5){
   table$difference[6]<-as.numeric(table[5,3]- table[6,3])
 }
 
 output[["summary.second.fx.t24_t36.index_fx"]]<-kable(table,
                                                       col.names = c( "Index fracture",
                                                                      "Intervention",
                                                                      # "N target population",
                                                                      "Subsequent fractures",
                                                                      "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 24) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, sex) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 36) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, sex) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y2_y3=y2-y1) %>% 
   select(sex, intervention ,  y2_y3) %>% 
   arrange(sex,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,3]- table[2,3])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,3]- table[4,3])
 }
 
 output[["summary.second.fx.t24_t36.sex"]]<-kable(table,
                                                  col.names = c( "Sex",
                                                                 "Intervention",
                                                                 # "N target population",
                                                                 "Subsequent fractures",
                                                                 "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 24) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx, sex) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 36) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx, sex) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y2_y3=y2-y1) %>% 
   select(index_fx, sex, intervention ,  y2_y3) %>% 
   arrange(index_fx, sex,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,4]- table[2,4])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,4]- table[4,4])
 }
 
 if(nrow(table)>5){
   table$difference[6]<-as.numeric(table[5,4]- table[6,4])
 }
 
 if(nrow(table)>7){
   table$difference[8]<-as.numeric(table[7,4]- table[8,4])
 }
 
 if(nrow(table)>9){
   table$difference[10]<-as.numeric(table[9,4]- table[10,4])
 }
 
 if(nrow(table)>11){
   table$difference[12]<-as.numeric(table[11,4]- table[12,4])
 }
 
 output[["summary.second.fx.t24_t36.index_fx.sex"]]<-kable(table,
                                                           col.names = c( "Index fracture",
                                                                          "Sex",
                                                                          "Intervention",
                                                                          # "N target population",
                                                                          "Subsequent fractures",
                                                                          "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 
 
 
 
# y3_y4 -----
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 36) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 48) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y3_y4=y2-y1) %>% 
   select( intervention , y3_y4)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,2]- table[2,2])
 
 output[["summary.second.fx.t36_t48"]]<-kable(table,
                                              col.names = c( "Intervention",
                                                             # "N target population",
                                                             "Subsequent fractures",
                                                             "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 36) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 48) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y3_y4=y2-y1) %>% 
   select(index_fx, intervention ,  y3_y4) %>% 
   arrange(index_fx,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,3]- table[2,3])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,3]- table[4,3])
 }
 
 if(nrow(table)>5){
   table$difference[6]<-as.numeric(table[5,3]- table[6,3])
 }
 
 output[["summary.second.fx.t36_t48.index_fx"]]<-kable(table,
                                                       col.names = c( "Index fracture",
                                                                      "Intervention",
                                                                      # "N target population",
                                                                      "Subsequent fractures",
                                                                      "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 36) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, sex) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 48) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, sex) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y3_y4=y2-y1) %>% 
   select(sex, intervention ,  y3_y4) %>% 
   arrange(sex,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,3]- table[2,3])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,3]- table[4,3])
 }
 
 output[["summary.second.fx.t36_t48.sex"]]<-kable(table,
                                                  col.names = c( "Sex",
                                                                 "Intervention",
                                                                 # "N target population",
                                                                 "Subsequent fractures",
                                                                 "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 36) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx, sex) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 48) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx, sex) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y3_y4=y2-y1) %>% 
   select(index_fx, sex, intervention ,  y3_y4) %>% 
   arrange(index_fx, sex,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,4]- table[2,4])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,4]- table[4,4])
 }
 
 if(nrow(table)>5){
   table$difference[6]<-as.numeric(table[5,4]- table[6,4])
 }
 
 if(nrow(table)>7){
   table$difference[8]<-as.numeric(table[7,4]- table[8,4])
 }
 
 if(nrow(table)>9){
   table$difference[10]<-as.numeric(table[9,4]- table[10,4])
 }
 
 if(nrow(table)>11){
   table$difference[12]<-as.numeric(table[11,4]- table[12,4])
 }
 
 output[["summary.second.fx.t36_t48.index_fx.sex"]]<-kable(table,
                                                           col.names = c( "Index fracture",
                                                                          "Sex",
                                                                          "Intervention",
                                                                          # "N target population",
                                                                          "Subsequent fractures",
                                                                          "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 
 
 
 
 
# y4_y5 -----
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 48) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 60) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y4_y5=y2-y1) %>% 
   select( intervention , y4_y5)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,2]- table[2,2])
 
 output[["summary.second.fx.t48_t60"]]<-kable(table,
                                              col.names = c( "Intervention",
                                                             # "N target population",
                                                             "Subsequent fractures",
                                                             "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 48) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 60) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y4_y5=y2-y1) %>% 
   select(index_fx, intervention ,  y4_y5) %>% 
   arrange(index_fx,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,3]- table[2,3])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,3]- table[4,3])
 }
 
 if(nrow(table)>5){
   table$difference[6]<-as.numeric(table[5,3]- table[6,3])
 }
 
 output[["summary.second.fx.t48_t60.index_fx"]]<-kable(table,
                                                       col.names = c( "Index fracture",
                                                                      "Intervention",
                                                                      # "N target population",
                                                                      "Subsequent fractures",
                                                                      "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 48) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, sex) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 60) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, sex) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y4_y5=y2-y1) %>% 
   select(sex, intervention ,  y4_y5) %>% 
   arrange(sex,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,3]- table[2,3])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,3]- table[4,3])
 }
 
 output[["summary.second.fx.t48_t60.sex"]]<-kable(table,
                                                  col.names = c( "Sex",
                                                                 "Intervention",
                                                                 # "N target population",
                                                                 "Subsequent fractures",
                                                                 "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 table<- inner_join(
   sub.frac.long %>% 
     filter(time== 48) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx, sex) %>% 
     summarise(y1=sum(n)) , 
   sub.frac.long %>% 
     filter(time== 60) %>% 
     mutate(n=as.numeric(n)) %>% 
     group_by(intervention, index_fx, sex) %>% 
     summarise(y2=sum(n))) %>% 
   mutate(y4_y5=y2-y1) %>% 
   select(index_fx, sex, intervention ,  y4_y5) %>% 
   arrange(index_fx, sex,intervention)
 
 table$difference<-""
 table$difference[2]<-as.numeric(table[1,4]- table[2,4])
 
 if(nrow(table)>3){
   table$difference[4]<-as.numeric(table[3,4]- table[4,4])
 }
 
 if(nrow(table)>5){
   table$difference[6]<-as.numeric(table[5,4]- table[6,4])
 }
 
 if(nrow(table)>7){
   table$difference[8]<-as.numeric(table[7,4]- table[8,4])
 }
 
 if(nrow(table)>9){
   table$difference[10]<-as.numeric(table[9,4]- table[10,4])
 }
 
 if(nrow(table)>11){
   table$difference[12]<-as.numeric(table[11,4]- table[12,4])
 }
 
 output[["summary.second.fx.t48_t60.index_fx.sex"]]<-kable(table,
                                                           col.names = c( "Index fracture",
                                                                          "Sex",
                                                                          "Intervention",
                                                                          # "N target population",
                                                                          "Subsequent fractures",
                                                                          "Subsequent fractures avoided")) %>%
   kable_styling(bootstrap_options = c("striped", "bordered"))
 
 
 
 
 
 
 
# total fx plot -----
output[["tot.sub.frac.plot.index_fx.sex"]]<-sub.frac.long %>%
  ggplot()+
  facet_grid(sex+index_fx~ intervention)+
  geom_col(aes(time, n, fill=type),width = 1)+
  ylab("Subseqent fractures")
output[["tot.sub.frac.plot.index_fx.sex"]]<-gg.general.format.not.perc(output[["tot.sub.frac.plot.index_fx.sex"]])
output[["tot.sub.frac.plot.index_fx.sex"]]<-gg.add.colours.sub.frac(output[["tot.sub.frac.plot.index_fx.sex"]])


output[["tot.sub.frac.plot.index_fx"]]<-sub.frac.long  %>%
  group_by(intervention, index_fx,time, type) %>%
  summarise(n=sum(n)) %>%
  ggplot()+
  facet_grid(index_fx~ intervention)+
  geom_col(aes(time, n, fill=type),width = 1)+
  ylab("Subseqent fractures")
output[["tot.sub.frac.plot.index_fx"]]<-gg.general.format.not.perc(output[["tot.sub.frac.plot.index_fx"]])
output[["tot.sub.frac.plot.index_fx"]]<-gg.add.colours.sub.frac(output[["tot.sub.frac.plot.index_fx"]])

output[["tot.sub.frac.plot.sex"]]<-sub.frac.long  %>%
  group_by(intervention, sex,time, type) %>%
  summarise(n=sum(n)) %>%
  ggplot()+
  facet_grid(sex~ intervention)+
  geom_col(aes(time, n, fill=type),width = 1)+
  ylab("Subseqent fractures")
output[["tot.sub.frac.plot.sex"]]<-gg.general.format.not.perc(output[["tot.sub.frac.plot.sex"]])
output[["tot.sub.frac.plot.sex"]]<-gg.add.colours.sub.frac(output[["tot.sub.frac.plot.sex"]])

output[["tot.sub.frac.plot"]]<-sub.frac.long  %>%
  group_by(intervention, time, type) %>%
  summarise(n=sum(n)) %>%
  ggplot()+
  facet_grid(.~ intervention)+
  geom_col(aes(time, n, fill=type),width = 1)+
  ylab("Subseqent fractures")
output[["tot.sub.frac.plot"]]<-gg.general.format.not.perc(output[["tot.sub.frac.plot"]])
output[["tot.sub.frac.plot"]]<-gg.add.colours.sub.frac(output[["tot.sub.frac.plot"]])


# plot diff by index fx and sex
sub.frac.wide.diff<-
sub.frac.long  %>%
  ungroup()   %>%mutate(intervention=ifelse(intervention=="Current practice",
                             "current_practice", "fls")) %>%
  pivot_wider(names_from = intervention,
              values_from = n)

sub.frac.wide.diff$diff<-
 as.numeric(sub.frac.wide.diff$current_practice-
  sub.frac.wide.diff$fls)


output[["sub.frac.plot.index_fx.sex"]]<-sub.frac.wide.diff %>%
  mutate(index_fx=ifelse(as.character(index_fx)=="Hip", "Index fx: Hip",
                  ifelse(as.character(index_fx)=="Spine", "Index fx: Spine",
                  ifelse(as.character(index_fx)=="Other", "Index fx: Other",
                         NA)))) %>%
  mutate(index_fx=factor(index_fx,
                         levels=c("Index fx: Hip","Index fx: Spine",
                                 "Index fx: Other" ))) %>%
  ggplot()+
  facet_grid(sex~ index_fx)+
  geom_col(aes(time, diff, fill=type),width = 1)+
  ylab("Subseqent fractures avoided")
output[["sub.frac.plot.index_fx.sex"]]<-gg.general.format.not.perc(output[["sub.frac.plot.index_fx.sex"]])
output[["sub.frac.plot.index_fx.sex"]]<-gg.add.colours.sub.frac(output[["sub.frac.plot.index_fx.sex"]])

output[["sub.frac.plot.index_fx"]]<-sub.frac.wide.diff %>%
  mutate(index_fx=ifelse(as.character(index_fx)=="Hip", "Index fx: Hip",
                  ifelse(as.character(index_fx)=="Spine", "Index fx: Spine",
                  ifelse(as.character(index_fx)=="Other", "Index fx: Other",
                         NA)))) %>%
  mutate(index_fx=factor(index_fx,
                         levels=c("Index fx: Hip","Index fx: Spine",
                                 "Index fx: Other" ))) %>%
  group_by(index_fx,time, type) %>%
  summarise(diff=sum(diff))%>%
  ggplot()+
  facet_grid(index_fx~ .)+
  geom_col(aes(time, diff, fill=type),width = 1)+
  ylab("Subseqent fractures avoided")
output[["sub.frac.plot.index_fx"]]<-gg.general.format.not.perc(output[["sub.frac.plot.index_fx"]])
output[["sub.frac.plot.index_fx"]]<-gg.add.colours.sub.frac(output[["sub.frac.plot.index_fx"]])

output[["sub.frac.plot.sex"]]<-sub.frac.wide.diff %>%
  group_by(sex,time, type) %>%
  summarise(diff=sum(diff))%>%
  ggplot()+
  facet_grid(sex~ .)+
  geom_col(aes(time, diff, fill=type),width = 1)+
  ylab("Subseqent fractures avoided")
output[["sub.frac.plot.sex"]]<-gg.general.format.not.perc(output[["sub.frac.plot.sex"]])
output[["sub.frac.plot.sex"]]<-gg.add.colours.sub.frac(output[["sub.frac.plot.sex"]])

output[["sub.frac.plot"]]<-sub.frac.wide.diff %>%
  group_by(time, type) %>%
  summarise(diff=sum(diff))%>%
  ggplot()+
  geom_col(aes(time, diff, fill=type),width = 1)+
  ylab("Subseqent fractures avoided")
output[["sub.frac.plot"]]<-gg.general.format.not.perc(output[["sub.frac.plot"]])
output[["sub.frac.plot"]]<-gg.add.colours.sub.frac(output[["sub.frac.plot"]])


## SECOND FX SUMMARY COUNT ONE YEAR ----
user_inputs$sec.frac.time<-12

user_inputs$choose.sec.frac.summary<-"Overall"
output[["summary.subs.fx.count.t12"]]<- get.num.subs.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)

user_inputs$choose.sec.frac.summary<-"By sentinel fracture"
output[["summary.subs.fx.count.t12.index_fx"]]<- get.num.subs.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)
user_inputs$choose.sec.frac.summary<-"By sex"
output[["summary.subs.fx.count.t12.sex"]]<- get.num.subs.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)

user_inputs$choose.sec.frac.summary<-"By sentinel fracture and sex"
output[["summary.subs.fx.count.t12.index_fx.sex"]]<- get.num.subs.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)




## SECOND FX SUMMARY COUNT FIVE YEARS ----
user_inputs$sec.frac.time<-60

user_inputs$choose.sec.frac.summary<-"Overall"
output[["summary.subs.fx.count.t60"]]<- get.num.subs.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)

user_inputs$choose.sec.frac.summary<-"By sentinel fracture"
output[["summary.subs.fx.count.t60.index_fx"]]<- get.num.subs.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)
user_inputs$choose.sec.frac.summary<-"By sex"
output[["summary.subs.fx.count.t60.sex"]]<- get.num.subs.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)

user_inputs$choose.sec.frac.summary<-"By sentinel fracture and sex"
output[["summary.subs.fx.count.t60.index_fx.sex"]]<- get.num.subs.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input=user_inputs)



## mc error ----

if(with.markov.trace!=FALSE){

# by index fx and sex
if(n_microsimulation<2000){
  run.for.seq<-seq(100,n_microsimulation,10)
}else if(n_microsimulation<60000) {
  run.for.seq<-seq(1000,n_microsimulation,n_microsimulation/25)
} else{
    run.for.seq<-seq(2000,n_microsimulation,4000)
}

mc.error.subs.fx.t12<-vector("list", length(run.for.seq))
mc.error.subs.fx.t60<-vector("list", length(run.for.seq))
mc.error.tot.cost.t12<-vector("list", length(run.for.seq))
mc.error.tot.cost.t60<-vector("list", length(run.for.seq))
for(i in run.for.seq){
 print(paste0(i, " of ", n_microsimulation))


  user_inputs$sec.frac.time<-12
  working.mc.error.subs.fx<-get.summary.second.fx.mc.error(m.TR=m.TR %>%
    filter(id %in%
             c(1:i,
               (n_microsimulation+1):(n_microsimulation+i),
               ((n_microsimulation+1)*2):((n_microsimulation*2)+i),
               ((n_microsimulation+1)*3):((n_microsimulation*3)+i),
               ((n_microsimulation+1)*4):((n_microsimulation*4)+i),
               ((n_microsimulation+1)*5):((n_microsimulation*5)+i),
               ((n_microsimulation+1)*6):((n_microsimulation*6)+i))),
                                    microsim_pop,
                                    study_pop_n,
                                    input=user_inputs,
                                    working.n_microsimulation=i)
    working.mc.error.subs.fx$seq<-i
  mc.error.subs.fx.t12[[i]] <-  working.mc.error.subs.fx

  user_inputs$sec.frac.time<-60
  working.mc.error.subs.fx<-get.summary.second.fx.mc.error(m.TR=m.TR %>%
    filter(id %in%
             c(1:i,
               (n_microsimulation+1):(n_microsimulation+i),
               ((n_microsimulation+1)*2):((n_microsimulation*2)+i),
               ((n_microsimulation+1)*3):((n_microsimulation*3)+i),
               ((n_microsimulation+1)*4):((n_microsimulation*4)+i),
               ((n_microsimulation+1)*5):((n_microsimulation*5)+i),
               ((n_microsimulation+1)*6):((n_microsimulation*6)+i))),
                                    microsim_pop,
                                    study_pop_n,
                                    input=user_inputs,
                                    working.n_microsimulation=i)
    working.mc.error.subs.fx$seq<-i
  mc.error.subs.fx.t60[[i]] <-  working.mc.error.subs.fx
  
  
  
    user_inputs$costs.time<-12
  working.mc.error.tot.cost<-get.summary.total.cost.mc.error(m.TR=m.TR %>%
    filter(id %in%
             c(1:i,
               (n_microsimulation+1):(n_microsimulation+i),
               ((n_microsimulation+1)*2):((n_microsimulation*2)+i),
               ((n_microsimulation+1)*3):((n_microsimulation*3)+i),
               ((n_microsimulation+1)*4):((n_microsimulation*4)+i),
               ((n_microsimulation+1)*5):((n_microsimulation*5)+i),
               ((n_microsimulation+1)*6):((n_microsimulation*6)+i))),
                                    microsim_pop,
                                    study_pop_n,
                                    input=user_inputs,
                                    working.n_microsimulation=i)
    working.mc.error.tot.cost$seq<-i
  mc.error.tot.cost.t12[[i]] <-  working.mc.error.tot.cost

  user_inputs$costs.time<-60
  working.mc.error.tot.cost<-get.summary.total.cost.mc.error(m.TR=m.TR %>%
    filter(id %in%
             c(1:i,
               (n_microsimulation+1):(n_microsimulation+i),
               ((n_microsimulation+1)*2):((n_microsimulation*2)+i),
               ((n_microsimulation+1)*3):((n_microsimulation*3)+i),
               ((n_microsimulation+1)*4):((n_microsimulation*4)+i),
               ((n_microsimulation+1)*5):((n_microsimulation*5)+i),
               ((n_microsimulation+1)*6):((n_microsimulation*6)+i))),
                                    microsim_pop,
                                    study_pop_n,
                                    input=user_inputs,
                                    working.n_microsimulation=i)
    working.mc.error.tot.cost$seq<-i
  mc.error.tot.cost.t60[[i]] <-  working.mc.error.tot.cost
  

# # working.m.TR<-as_tibble(m.TR)   %>%
# #     filter(id %in%
# #              c(1:i,
# #                (n_microsimulation+1):(n_microsimulation+i),
# #                ((n_microsimulation+1)*2):((n_microsimulation*2)+i),
# #                ((n_microsimulation+1)*3):((n_microsimulation*3)+i),
# #                ((n_microsimulation+1)*4):((n_microsimulation*4)+i),
# #                ((n_microsimulation+1)*5):((n_microsimulation*5)+i),
# #                ((n_microsimulation+1)*6):((n_microsimulation*6)+i)))
#
# working.m.TR<-rbind(m.TR %>%
#   select(cycle, month, id,index_fx,sex,intervention,
#          s_ff, s_hf, s_sf, s_of,
#          s_ff, s_d, c_af, h_hf,
#          h_sf, h_of, h_af,
#          total.cost) %>% filter(id %in% c(1:i)),
#                  m.TR  %>%
#   select(cycle, month, id,index_fx,sex,intervention,
#          s_ff, s_hf, s_sf, s_of,
#          s_ff, s_d, c_af, h_hf,
#          h_sf, h_of, h_af,
#          total.cost) %>%
#     filter(id %in% (n_microsimulation+1):(n_microsimulation+i)))
#
# working.m.TR<- rbind(working.m.TR,
#   m.TR  %>%
#   select(cycle, month, id,index_fx,sex,intervention,
#          s_ff, s_hf, s_sf, s_of,
#          s_ff, s_d, c_af, h_hf,
#          h_sf, h_of, h_af,
#          total.cost)%>%
#     filter(id %in% ((n_microsimulation+1)*2):((n_microsimulation*2)+i)))
#
# working.m.TR<- rbind(working.m.TR,
#   m.TR  %>%
#   select(cycle, month, id,index_fx,sex,intervention,
#          s_ff, s_hf, s_sf, s_of,
#          s_ff, s_d, c_af, h_hf,
#          h_sf, h_of, h_af,
#          total.cost)%>%
#     filter(id %in% ((n_microsimulation+1)*3):((n_microsimulation*3)+i)))
#
# working.m.TR<- rbind(working.m.TR,
#   m.TR  %>%
#   select(cycle, month, id,index_fx,sex,intervention,
#          s_ff, s_hf, s_sf, s_of,
#          s_ff, s_d, c_af, h_hf,
#          h_sf, h_of, h_af,
#          total.cost)%>%
#     filter(id %in% ((n_microsimulation+1)*4):((n_microsimulation*4)+i)))
#
# working.m.TR<- rbind(working.m.TR,
#   m.TR  %>%
#   select(cycle, month, id,index_fx,sex,intervention,
#          s_ff, s_hf, s_sf, s_of,
#          s_ff, s_d, c_af, h_hf,
#          h_sf, h_of, h_af,
#          total.cost)%>%
#     filter(id %in% ((n_microsimulation+1)*5):((n_microsimulation*5)+i)))
#
# working.m.TR<- rbind(working.m.TR,
#   m.TR  %>%
#   select(cycle, month, id,index_fx,sex,intervention,
#          s_ff, s_hf, s_sf, s_of,
#          s_ff, s_d, c_af, h_hf,
#          h_sf, h_of, h_af,
#          total.cost)%>%
#     filter(id %in% ((n_microsimulation+1)*6):((n_microsimulation*6)+i)))
#
#
#
# user_inputs$sec.frac.time<-12
#   working.mc.error.subs.fx<-get.summary.second.fx.mc.error(m.TR=working.m.TR,
#                                     microsim_pop,
#                                     study_pop_n,
#                                     input=user_inputs,
#                                     working.n_microsimulation=i)
#   working.mc.error.subs.fx$seq<-i
#
#   mc.error.subs.fx.t12[[i]] <-  working.mc.error.subs.fx
#
#   user_inputs$sec.frac.time<-60
#   working.mc.error.subs.fx<-get.summary.second.fx.mc.error(m.TR=working.m.TR,
#                                     microsim_pop,
#                                     study_pop_n,
#                                     input=user_inputs,
#                                     working.n_microsimulation=i)
#     working.mc.error.subs.fx$seq<-i
#   mc.error.subs.fx.t60[[i]] <-  working.mc.error.subs.fx
# #
# user_inputs$costs.time<-12
#  working.mc.error.tot.cost<-get.summary.total.cost.mc.error(m.TR=working.m.TR,
#                                     microsim_pop,
#                                     study_pop_n,
#                                     input=user_inputs,
#                                     working.n_microsimulation=i)
#    working.mc.error.tot.cost$seq<-i
# 
#   mc.error.tot.cost.t12[[i]] <-  working.mc.error.tot.cost
# 
#   user_inputs$costs.time<-60
#  working.mc.error.tot.cost<-get.summary.total.cost.mc.error(m.TR=m.TR,
#                                     microsim_pop,
#                                     study_pop_n,
#                                     input=user_inputs,
#                                     working.n_microsimulation=i)
#     working.mc.error.tot.cost$seq<-i
# 
#       mc.error.tot.cost.t60[[i]] <-  working.mc.error.tot.cost
#
# rm(working.m.TR)
# gc()
}

mc.error.subs.fx.t12<-setDF(rbindlist(mc.error.subs.fx.t12))
mc.error.subs.fx.t60<-setDF(rbindlist(mc.error.subs.fx.t60))
mc.error.tot.cost.t12<-setDF(rbindlist(mc.error.tot.cost.t12))
mc.error.tot.cost.t60<-setDF(rbindlist(mc.error.tot.cost.t60))


options(scipen = 999)
#subs.fx t12
mc.error.subs.fx.t12$difference<-as.numeric(mc.error.subs.fx.t12$difference)
max<-
  bind_rows(
  data.frame(
  sex="Male",
  index_fx ="Spine",
  max=
    ifelse(
      nrow(mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Spine" &
            seq==max(run.for.seq )))>0,
      mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Spine" &
            seq==max(run.for.seq )) %>%
  ungroup()   %>%select(difference) %>%
  pull(),
  NA)) ,
  data.frame(
    sex="Female",
    index_fx ="Spine",
    max= ifelse(
      nrow(mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Spine" &
            seq==max(run.for.seq )))>0,
      
    mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Spine" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),
    NA)) ,
  data.frame(
    sex="Male",
    index_fx ="Hip",
    max=ifelse(
      nrow(mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Hip" &
            seq==max(run.for.seq )))>0,
      
      mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
               sex=="Male" &
               index_fx =="Hip" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),NA)),
  data.frame(
    sex="female",
    index_fx ="hip",
    max=ifelse(
      nrow(mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Hip" &
            seq==max(run.for.seq )))>0,
      mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Hip" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(), NA)),
  data.frame(
    sex="male",
    index_fx ="other",
    max=ifelse(
      nrow(mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Other" &
            seq==max(run.for.seq )))>0,
    mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
               sex=="Male" &
               index_fx =="Other" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(), NA)),
  data.frame(
    sex="Female",
    index_fx ="Other",
    max=ifelse(
      nrow(mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Other" &
            seq==max(run.for.seq )))>0,
      
      mc.error.subs.fx.t12 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Other" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),NA)  )) %>%
    mutate(max=as.numeric(max)) %>%
  mutate(index_fx=factor(str_to_sentence(index_fx),
                         levels=c("Spine", "Hip",
                                  "Other"))) %>%
  mutate(sex=factor(str_to_sentence(sex),
                    levels=c("Male", "Female")))


mc.fx.plot<-
  mc.error.subs.fx.t12 %>%
  ungroup()   %>%mutate(index_fx=factor(str_to_sentence(index_fx),
                           levels=c("Spine", "Hip",
                                    "Other"))) %>%
    mutate(sex=factor(str_to_sentence(sex),
                      levels=c("Male", "Female")))

if(n_microsimulation>1900){
mc.fx.plot<-mc.fx.plot %>%
  filter(seq>1000)
}

mc.fx.plot<-
  mc.fx.plot %>%
  ggplot()+
  facet_grid(index_fx~sex)+
  geom_point(aes(seq, as.numeric(difference))) +
  theme_bw()+
  xlab("Number of simulations")+
  ylab("Subsequent fractures avoided under FLS") +
  geom_hline(data = max,
             aes(yintercept = max), colour="red")+
    theme(panel.spacing = unit(1, "lines"),
          legend.title = element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold"),
          strip.text = element_text(size=14, face="bold"),
          strip.background = element_rect( fill="#f7f7f7"),
          legend.text=element_text(size=14))+
  scale_x_continuous(labels = comma)+
  theme(panel.spacing.x=unit(1.5, "lines"))


output[["mc.fx.plot.subs.fx.t12"]]<- mc.fx.plot

#tot.cost t12
mc.error.tot.cost.t12$difference<-as.numeric(mc.error.tot.cost.t12$difference)

max<-
  bind_rows(
  data.frame(
  sex="Male",
  index_fx ="Spine",
  max=
    ifelse(
      nrow(mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Spine" &
            seq==max(run.for.seq )))>0,
      mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Spine" &
            seq==max(run.for.seq )) %>%
  ungroup()   %>%select(difference) %>%
  pull(),
  NA)) ,
  data.frame(
    sex="Female",
    index_fx ="Spine",
    max= ifelse(
      nrow(mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Spine" &
            seq==max(run.for.seq )))>0,
      
    mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Spine" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),
    NA)) ,
  data.frame(
    sex="Male",
    index_fx ="Hip",
    max=ifelse(
      nrow(mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Hip" &
            seq==max(run.for.seq )))>0,
      
      mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
               sex=="Male" &
               index_fx =="Hip" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),NA)),
  data.frame(
    sex="female",
    index_fx ="hip",
    max=ifelse(
      nrow(mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Hip" &
            seq==max(run.for.seq )))>0,
      mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Hip" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(), NA)),
  data.frame(
    sex="male",
    index_fx ="other",
    max=ifelse(
      nrow(mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Other" &
            seq==max(run.for.seq )))>0,
    mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
               sex=="Male" &
               index_fx =="Other" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(), NA)),
  data.frame(
    sex="Female",
    index_fx ="Other",
    max=ifelse(
      nrow(mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Other" &
            seq==max(run.for.seq )))>0,
      
      mc.error.tot.cost.t12 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Other" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),NA)  )) %>%
    mutate(max=as.numeric(max)) %>%
  mutate(index_fx=factor(str_to_sentence(index_fx),
                         levels=c("Spine", "Hip",
                                  "Other"))) %>%
  mutate(sex=factor(str_to_sentence(sex),
                    levels=c("Male", "Female")))



mc.fx.plot<-
  mc.error.tot.cost.t12 %>%
  ungroup()   %>%mutate(index_fx=factor(str_to_sentence(index_fx),
                           levels=c("Spine", "Hip",
                                    "Other"))) %>%
    mutate(sex=factor(str_to_sentence(sex),
                      levels=c("Male", "Female")))

if(n_microsimulation>1900){
mc.fx.plot<-mc.fx.plot %>%
  filter(seq>1000)
}

mc.fx.plot<-
  mc.fx.plot %>%
  ggplot()+
  facet_grid(index_fx~sex)+
  geom_point(aes(seq, as.numeric(difference))) +
  theme_bw()+
  xlab("Number of simulations")+
  ylab("Total costs avoided under FLS") +
  geom_hline(data = max,
             aes(yintercept = max), colour="red")+
    theme(panel.spacing = unit(1, "lines"),
          legend.title = element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold"),
          strip.text = element_text(size=14, face="bold"),
          strip.background = element_rect( fill="#f7f7f7"),
          legend.text=element_text(size=14))+
  scale_x_continuous(labels = comma)+
  theme(panel.spacing.x=unit(1.5, "lines"))


output[["mc.fx.plot.tot.cost.t12"]]<- mc.fx.plot





#subs.fx t60
mc.error.subs.fx.t60$difference<-as.numeric(mc.error.subs.fx.t60$difference)
max<-
  bind_rows(
  data.frame(
  sex="Male",
  index_fx ="Spine",
  max=
    ifelse(
      nrow(mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Spine" &
            seq==max(run.for.seq )))>0,
      mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Spine" &
            seq==max(run.for.seq )) %>%
  ungroup()   %>%select(difference) %>%
  pull(),
  NA)) ,
  data.frame(
    sex="Female",
    index_fx ="Spine",
    max= ifelse(
      nrow(mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Spine" &
            seq==max(run.for.seq )))>0,
      
    mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Spine" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),
    NA)) ,
  data.frame(
    sex="Male",
    index_fx ="Hip",
    max=ifelse(
      nrow(mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Hip" &
            seq==max(run.for.seq )))>0,
      
      mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
               sex=="Male" &
               index_fx =="Hip" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),NA)),
  data.frame(
    sex="female",
    index_fx ="hip",
    max=ifelse(
      nrow(mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Hip" &
            seq==max(run.for.seq )))>0,
      mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Hip" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(), NA)),
  data.frame(
    sex="male",
    index_fx ="other",
    max=ifelse(
      nrow(mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Other" &
            seq==max(run.for.seq )))>0,
    mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
               sex=="Male" &
               index_fx =="Other" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(), NA)),
  data.frame(
    sex="Female",
    index_fx ="Other",
    max=ifelse(
      nrow(mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Other" &
            seq==max(run.for.seq )))>0,
      
      mc.error.subs.fx.t60 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Other" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),NA)  )) %>%
    mutate(max=as.numeric(max)) %>%
  mutate(index_fx=factor(str_to_sentence(index_fx),
                         levels=c("Spine", "Hip",
                                  "Other"))) %>%
  mutate(sex=factor(str_to_sentence(sex),
                    levels=c("Male", "Female")))


mc.fx.plot<-
  mc.error.subs.fx.t60 %>%
  ungroup()   %>%mutate(index_fx=factor(str_to_sentence(index_fx),
                           levels=c("Spine", "Hip",
                                    "Other"))) %>%
    mutate(sex=factor(str_to_sentence(sex),
                      levels=c("Male", "Female")))

if(n_microsimulation>1900){
mc.fx.plot<-mc.fx.plot %>%
  filter(seq>1000)
}

mc.fx.plot<-
  mc.fx.plot %>%
  ggplot()+
  facet_grid(index_fx~sex)+
  geom_point(aes(seq, as.numeric(difference))) +
  theme_bw()+
  xlab("Number of simulations")+
  ylab("Subsequent fractures avoided under FLS") +
  geom_hline(data = max,
             aes(yintercept = max), colour="red")+
    theme(panel.spacing = unit(1, "lines"),
          legend.title = element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold"),
          strip.text = element_text(size=14, face="bold"),
          strip.background = element_rect( fill="#f7f7f7"),
          legend.text=element_text(size=14))+
  scale_x_continuous(labels = comma)+
  theme(panel.spacing.x=unit(1.5, "lines"))


output[["mc.fx.plot.subs.fx.t60"]]<- mc.fx.plot

# #tot.cost t60
mc.error.tot.cost.t60$difference<-as.numeric(mc.error.tot.cost.t60$difference)

max<-
  bind_rows(
  data.frame(
  sex="Male",
  index_fx ="Spine",
  max=
    ifelse(
      nrow(mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Spine" &
            seq==max(run.for.seq )))>0,
      mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Spine" &
            seq==max(run.for.seq )) %>%
  ungroup()   %>%select(difference) %>%
  pull(),
  NA)) ,
  data.frame(
    sex="Female",
    index_fx ="Spine",
    max= ifelse(
      nrow(mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Spine" &
            seq==max(run.for.seq )))>0,
      
    mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Spine" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),
    NA)) ,
  data.frame(
    sex="Male",
    index_fx ="Hip",
    max=ifelse(
      nrow(mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Hip" &
            seq==max(run.for.seq )))>0,
      
      mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
               sex=="Male" &
               index_fx =="Hip" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),NA)),
  data.frame(
    sex="female",
    index_fx ="hip",
    max=ifelse(
      nrow(mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Hip" &
            seq==max(run.for.seq )))>0,
      mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Hip" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(), NA)),
  data.frame(
    sex="male",
    index_fx ="other",
    max=ifelse(
      nrow(mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
             sex=="Male" &
            index_fx =="Other" &
            seq==max(run.for.seq )))>0,
    mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
               sex=="Male" &
               index_fx =="Other" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(), NA)),
  data.frame(
    sex="Female",
    index_fx ="Other",
    max=ifelse(
      nrow(mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
             sex=="Female" &
            index_fx =="Other" &
            seq==max(run.for.seq )))>0,
      
      mc.error.tot.cost.t60 %>%
      filter(intervention=="FLS" &
               sex=="Female" &
               index_fx =="Other" &
            seq==max(run.for.seq )) %>%
      ungroup()   %>%select(difference) %>%
      pull(),NA)  )) %>%
    mutate(max=as.numeric(max)) %>%
  mutate(index_fx=factor(str_to_sentence(index_fx),
                         levels=c("Spine", "Hip",
                                  "Other"))) %>%
  mutate(sex=factor(str_to_sentence(sex),
                    levels=c("Male", "Female")))




mc.fx.plot<-
  mc.error.tot.cost.t60 %>%
  ungroup()   %>%mutate(index_fx=factor(str_to_sentence(index_fx),
                           levels=c("Spine", "Hip",
                                    "Other"))) %>%
    mutate(sex=factor(str_to_sentence(sex),
                      levels=c("Male", "Female")))

if(n_microsimulation>1900){
mc.fx.plot<-mc.fx.plot %>%
  filter(seq>1000)
}

mc.fx.plot<-
  mc.fx.plot %>%
  ggplot()+
  facet_grid(index_fx~sex)+
  geom_point(aes(seq, as.numeric(difference))) +
  theme_bw()+
  xlab("Number of simulations")+
  ylab("Total costs avoided under FLS") +
  geom_hline(data = max,
             aes(yintercept = max), colour="red")+
    theme(panel.spacing = unit(1, "lines"),
          legend.title = element_blank(),
          axis.text=element_text(size=18),
          axis.title=element_text(size=18,face="bold"),
          strip.text = element_text(size=18, face="bold"),
          strip.background = element_rect( fill="#f7f7f7"),
          legend.text=element_text(size=18))+
  scale_y_continuous(labels = comma)


output[["mc.fx.plot.tot.cost.t60"]]<- mc.fx.plot

}

## cumulative incidence -----
user_inputs$choose.sec.frac.summary<-"Overall"
output[["c_inc"]]<-get.c.inc.plot(m.TR, user_inputs)
output[["c_inc"]]<-gg.general.format.c.inc(output[["c_inc"]]) 

user_inputs$choose.sec.frac.summary<-"By sentinel fracture"
output[["c_inc.index_fx"]]<-get.c.inc.plot(m.TR, user_inputs)
output[["c_inc.index_fx"]]<-gg.general.format.c.inc(output[["c_inc.index_fx"]]) 

user_inputs$choose.sec.frac.summary<-"By sex"
output[["c_inc.sex"]]<-get.c.inc.plot(m.TR, user_inputs)
output[["c_inc.sex"]]<-gg.general.format.c.inc(output[["c_inc.sex"]]) 

user_inputs$choose.sec.frac.summary<-"By sentinel fracture and sex"
output[["c_inc.index_fx.sex"]]<-get.c.inc.plot(m.TR, user_inputs)
output[["c_inc.index_fx.sex"]]<-gg.general.format.c.inc(output[["c_inc.index_fx.sex"]]) 

## RESOURCE USE TABLES ONE YEAR ----
# procedures -----
print("procedures")
user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.procedures.t12"]]<- get.summary.procedures(m.TR,
                       microsim_pop,
                       study_pop_n,
                       input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.procedures.t12.index_fx"]]<- get.summary.procedures(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.procedures.t12.sex"]]<- get.summary.procedures(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.procedures.t12.index_fx.sex"]]<- get.summary.procedures(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

# hosp.los -----
print("hosp.los")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.hosp.los.t12"]]<- get.summary.hosp.los(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.hosp.los.t12.index_fx"]]<- get.summary.hosp.los(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.hosp.los.t12.sex"]]<- get.summary.hosp.los(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.hosp.los.t12.index_fx.sex"]]<- get.summary.hosp.los(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)


# temp.rehab.los -----
print("temp.rehab.los")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.temp.rehab.los.t12"]]<- get.summary.temp.rehab.los(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.temp.rehab.los.t12.index_fx"]]<- get.summary.temp.rehab.los(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.temp.rehab.los.t12.sex"]]<- get.summary.temp.rehab.los(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.temp.rehab.los.t12.index_fx.sex"]]<- get.summary.temp.rehab.los(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)



# comm.visits -----
print("comm.visits")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.comm.visits.t12"]]<- get.summary.comm.visits(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.comm.visits.t12.index_fx"]]<- get.summary.comm.visits(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.comm.visits.t12.sex"]]<- get.summary.comm.visits(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.comm.visits.t12.index_fx.sex"]]<- get.summary.comm.visits(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)



# lab.test -----
print("lab.test")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.lab.test.t12"]]<- get.summary.lab.test(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.lab.test.t12.index_fx"]]<- get.summary.lab.test(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.lab.test.t12.sex"]]<- get.summary.lab.test(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.lab.test.t12.index_fx.sex"]]<- get.summary.lab.test(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# dxa -----
print("dxa")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.dxa.t12"]]<- get.summary.dxa(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.dxa.t12.index_fx"]]<- get.summary.dxa(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.dxa.t12.sex"]]<- get.summary.dxa(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.dxa.t12.index_fx.sex"]]<- get.summary.dxa(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)





# doctor.mins -----
print("doctor.mins")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.doctor.mins.t12"]]<- get.summary.doctor.mins(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.doctor.mins.t12.index_fx"]]<- get.summary.doctor.mins(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.doctor.mins.t12.sex"]]<- get.summary.doctor.mins(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.doctor.mins.t12.index_fx.sex"]]<- get.summary.doctor.mins(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)



# administrator.mins -----
print("administrator.mins")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.administrator.mins.t12"]]<- get.summary.administrator.mins(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.administrator.mins.t12.index_fx"]]<- get.summary.administrator.mins(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.administrator.mins.t12.sex"]]<- get.summary.administrator.mins(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.administrator.mins.t12.index_fx.sex"]]<- get.summary.administrator.mins(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# nurse.mins -----
print("nurse.mins")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.nurse.mins.t12"]]<- get.summary.nurse.mins(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.nurse.mins.t12.index_fx"]]<- get.summary.nurse.mins(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.nurse.mins.t12.sex"]]<- get.summary.nurse.mins(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.nurse.mins.t12.index_fx.sex"]]<- get.summary.nurse.mins(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# radiographer.mins -----
print("radiographer.mins")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.radiographer.mins.t12"]]<- get.summary.radiographer.mins(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.radiographer.mins.t12.index_fx"]]<- get.summary.radiographer.mins(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.radiographer.mins.t12.sex"]]<- get.summary.radiographer.mins(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.radiographer.mins.t12.index_fx.sex"]]<- get.summary.radiographer.mins(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# allied_health.mins -----
print("allied_health.mins")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.allied_health.mins.t12"]]<- get.summary.allied_health.mins(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.allied_health.mins.t12.index_fx"]]<- get.summary.allied_health.mins(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.allied_health.mins.t12.sex"]]<- get.summary.allied_health.mins(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.allied_health.mins.t12.index_fx.sex"]]<- get.summary.allied_health.mins(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# fls_coordinator.mins -----
print("fls_coordinator.mins")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.fls_coordinator.mins.t12"]]<- get.summary.fls_coordinator.mins(m.TR,
                                                                            microsim_pop,
                                                                            study_pop_n,
                                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.fls_coordinator.mins.t12.index_fx"]]<- get.summary.fls_coordinator.mins(m.TR,
                                                                                     microsim_pop,
                                                                                     study_pop_n,
                                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.fls_coordinator.mins.t12.sex"]]<- get.summary.fls_coordinator.mins(m.TR,
                                                                                microsim_pop,
                                                                                study_pop_n,
                                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.fls_coordinator.mins.t12.index_fx.sex"]]<- get.summary.fls_coordinator.mins(m.TR,
                                                                                         microsim_pop,
                                                                                         study_pop_n,
                                                                                         input=user_inputs)





# other.mins -----
print("other.mins")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.other.mins.t12"]]<- get.summary.other.mins(m.TR,
                                                              microsim_pop,
                                                              study_pop_n,
                                                              input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.other.mins.t12.index_fx"]]<- get.summary.other.mins(m.TR,
                                                                       microsim_pop,
                                                                       study_pop_n,
                                                                       input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.other.mins.t12.sex"]]<- get.summary.other.mins(m.TR,
                                                                  microsim_pop,
                                                                  study_pop_n,
                                                                  input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.other.mins.t12.index_fx.sex"]]<- get.summary.other.mins(m.TR,
                                                                           microsim_pop,
                                                                           study_pop_n,
                                                                           input=user_inputs)




# discharge.clinic.visits -----
print("discharge.clinic.visits")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.discharge.clinic.visits.t12"]]<- get.summary.discharge.clinic.visits(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.discharge.clinic.visits.t12.index_fx"]]<- get.summary.discharge.clinic.visits(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.discharge.clinic.visits.t12.sex"]]<- get.summary.discharge.clinic.visits(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.discharge.clinic.visits.t12.index_fx.sex"]]<- get.summary.discharge.clinic.visits(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# clinic.visits -----
print("clinic.visits")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.clinic.visits.t12"]]<- get.summary.clinic.visits(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.clinic.visits.t12.index_fx"]]<- get.summary.clinic.visits(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.clinic.visits.t12.sex"]]<- get.summary.clinic.visits(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.clinic.visits.t12.index_fx.sex"]]<- get.summary.clinic.visits(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# location table ----
print("location")

user_inputs$hcru.time<-12

user_inputs$choose.hcru.summary<-"Overall"

output[["summary.location.t12"]]<- get.summary.h_no_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>%   
  left_join(
    get.summary.f_home(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n")) %>% 
  left_join(
    get.summary.h_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n")) %>% 
  left_join(
    get.summary.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n"))

output[["summary.location.t12"]]<-
  kable(output[["summary.location.t12"]],
              col.names = c("Intervention",
                    "N target population",
                     "Home, no support (years)",
                     "Difference in home, no support (years)",
                    "Family home (years)",
                     "Difference in family home (years)",
                     "Home, no support (years)",
                     "Difference in home, support (years)",
                     "Long term care (years)",
                     "Difference in long term care (years)")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)


user_inputs$choose.hcru.summary<-"By sentinel fracture"
  output[["summary.location.t12.index_fx"]]<- get.summary.h_no_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>%   
    left_join(
    get.summary.f_home(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "index_fx")) %>% 
  left_join(
    get.summary.h_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "index_fx")) %>% 
  left_join(
    get.summary.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "index_fx"))

output[["summary.location.t12.index_fx"]]<-
  kable(output[["summary.location.t12.index_fx"]],
              col.names = c("Index fracture",
                            "Intervention",
                    "N target population",
                     "Home, no support (years)",
                     "Difference in home, no support (years)",
                    "Family home (years)",
                     "Difference in family home (years)",
                     "Home, no support (years)",
                     "Difference in home, support (years)",
                     "Long term care (years)",
                     "Difference in long term care (years)")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.location.t12.sex"]]<- get.summary.h_no_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>% 
    left_join(
    get.summary.f_home(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "sex")) %>% 
  left_join(
    get.summary.h_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "sex")) %>% 
  left_join(
    get.summary.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "sex"))

output[["summary.location.t12.sex"]]<-
  kable(output[["summary.location.t12.sex"]],
              col.names = c("Sex",
                "Intervention",
                    "N target population",
                     "Home, no support (years)",
                     "Difference in home, no support (years)",
                    "Family home (years)",
                     "Difference in family home (years)",
                     "Home, no support (years)",
                     "Difference in home, support (years)",
                     "Long term care (years)",
                     "Difference in long term care (years)")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.location.t12.index_fx.sex"]]<- get.summary.h_no_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>%  
  left_join(
    get.summary.f_home(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n","index_fx", "sex")) %>% 
  left_join(
    get.summary.h_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n","index_fx", "sex")) %>% 

  left_join(
    get.summary.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n","index_fx", "sex"))

output[["summary.location.t12.index_fx.sex"]]<-
  kable(output[["summary.location.t12.index_fx.sex"]],
              col.names = c("Intervention","Index fracture", "Sex",
                    "N target population",
                     "Home, no support (years)",
                     "Difference in home, no support (years)",
                    "Family home (years)",
                     "Difference in family home (years)",
                     "Home, no support (years)",
                     "Difference in home, support (years)",
                     "Long term care (years)",
                     "Difference in long term care (years)")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)





# location -----
output[["summary.m.TR.location.over_time.index_fx.sex"]]<-m.TR %>%
  ungroup() %>%
  mutate(location=str_to_sentence(location)) %>%
  mutate(location=factor(location,
                           levels=c("Died",
                                    "Home, no support",
                                    "Home, support",
                                    "Family home",
                                    "Long term care")))  %>%
  group_by(month,intervention,index_fx, sex, location, .drop=FALSE) %>%
  tally(name="n.location")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx, sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
 ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx),
         sex=str_to_sentence(sex)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))

# index_fx
output[["summary.m.TR.location.over_time.index_fx"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(location=str_to_sentence(location)) %>%
  mutate(location=factor(location,
                           levels=c("Died",
                                    "Home, no support",
                                    "Family home",
                                    "Home, support",
                                    "Long term care")))  %>%
  group_by(month,intervention,index_fx, location, .drop=FALSE) %>%
  tally(name="n.location")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx,.drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other")))
# sex
output[["summary.m.TR.location.over_time.sex"]]<- m.TR %>%
  ungroup() %>%
  mutate(location=str_to_sentence(location)) %>%
  mutate(location=factor(location,
                           levels=c("Died",
                                    "Home, no support",
                                    "Family home",
                                    "Home, support",
                                    "Long term care")))  %>%
  group_by(month,intervention, sex, location, .drop=FALSE) %>%
  tally(name="n.location")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(sex=str_to_sentence(sex)) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# overall
output[["summary.m.TR.location.over_time.overall"]]<- m.TR %>%
  ungroup() %>%
  mutate(location=str_to_sentence(location)) %>%
  mutate(location=factor(location,
                           levels=c("Died",
                                    "Home, no support",
                                    "Family home",
                                    "Home, support",
                                    "Long term care")))  %>%
  group_by(month,intervention, location, .drop=FALSE) %>%
  tally(name="n.location")%>%
  mutate(n_microsim=working.microsim_pop %>%   tally(name="n_microsim") %>% pull())






# location plot -----
# drop unused locations
unused<-output$summary.m.TR.location.over_time.overall %>%
  group_by(location) %>%
  summarise(n=sum(n.location)) %>%
  filter(n==0) %>%
  mutate(location=as.character(location)) %>%
  select(location)

output[["location.plot.overall"]]<-
output$summary.m.TR.location.over_time.overall   %>%
   anti_join(unused) %>%
      ggplot()+
      facet_grid(.~ intervention)+
      geom_col(aes(month, n.location/n_microsim,
                   fill=location), width=1)
output[["location.plot.overall"]]<-gg.general.format(output[["location.plot.overall"]])
output[["location.plot.overall"]]<-gg.add.colours.location(output[["location.plot.overall"]])


output[["location.plot.index_fx"]]<-
output$summary.m.TR.location.over_time.index_fx   %>%
   anti_join(unused)  %>%
      ggplot()+
      facet_grid(index_fx~ intervention)+
      geom_col(aes(month, n.location/n_microsim,
                   fill=location), width=1)
output[["location.plot.index_fx"]]<-gg.general.format(output[["location.plot.index_fx"]])
output[["location.plot.index_fx"]]<-gg.add.colours.location(output[["location.plot.index_fx"]])

output[["location.plot.sex"]]<-
output$summary.m.TR.location.over_time.sex   %>%
   anti_join(unused)  %>%
      ggplot()+
      facet_grid(sex~ intervention)+
      geom_col(aes(month, n.location/n_microsim,
                   fill=location), width=1)
output[["location.plot.sex"]]<-gg.general.format(output[["location.plot.sex"]])
output[["location.plot.sex"]]<-gg.add.colours.location(output[["location.plot.sex"]])

output[["location.plot.index_fx.sex"]]<-
output$summary.m.TR.location.over_time.index_fx.sex   %>%
   anti_join(unused)  %>%
      ggplot()+
      facet_grid(index_fx+ sex~ intervention)+
      geom_col(aes(month, n.location/n_microsim,
                   fill=location), width=1)
output[["location.plot.index_fx.sex"]]<-gg.general.format(output[["location.plot.index_fx.sex"]])
output[["location.plot.index_fx.sex"]]<-gg.add.colours.location(output[["location.plot.index_fx.sex"]])



## COST TABLES ONE YEAR ----
# total.cost.excl.location -----
print("total.cost.excl.location")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.total.cost.excl.location.t12"]]<- get.summary.total.cost.excl.location(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.total.cost.excl.location.t12.index_fx"]]<- get.summary.total.cost.excl.location(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.total.cost.excl.location.t12.sex"]]<- get.summary.total.cost.excl.location(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.total.cost.excl.location.t12.index_fx.sex"]]<- get.summary.total.cost.excl.location(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)


# total.cost -----
print("total.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.total.cost.t12"]]<- get.summary.total.cost(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.total.cost.t12.index_fx"]]<- get.summary.total.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.total.cost.t12.sex"]]<- get.summary.total.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.total.cost.t12.index_fx.sex"]]<- get.summary.total.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)



# hosp.cost -----
print("hosp.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.hosp.cost.t12"]]<- get.summary.hosp.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.hosp.cost.t12.index_fx"]]<- get.summary.hosp.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.hosp.cost.t12.sex"]]<- get.summary.hosp.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.hosp.cost.t12.index_fx.sex"]]<- get.summary.hosp.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)


# procedure.cost -----
print("procedure.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.procedure.cost.t12"]]<- get.summary.procedure.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.procedure.cost.t12.index_fx"]]<- get.summary.procedure.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.procedure.cost.t12.sex"]]<- get.summary.procedure.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.procedure.cost.t12.index_fx.sex"]]<- get.summary.procedure.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)



# location_home_support.cost -----
print("location_home_support.cost")
user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.location_home_support.cost.t12"]]<- get.summary.location_home_support.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.location_home_support.cost.t12.index_fx"]]<- get.summary.location_home_support.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.location_home_support.cost.t12.sex"]]<- get.summary.location_home_support.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.location_home_support.cost.t12.index_fx.sex"]]<- get.summary.location_home_support.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)


# location_long_term_care.cost -----
print("location_long_term_care.cost")
user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.location_long_term_care.cost.t12"]]<- get.summary.location_long_term_care.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.location_long_term_care.cost.t12.index_fx"]]<- get.summary.location_long_term_care.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.location_long_term_care.cost.t12.sex"]]<- get.summary.location_long_term_care.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.location_long_term_care.cost.t12.index_fx.sex"]]<- get.summary.location_long_term_care.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)



# comm.cost -----
print("comm.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.comm.cost.t12"]]<- get.summary.comm.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.comm.cost.t12.index_fx"]]<- get.summary.comm.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.comm.cost.t12.sex"]]<- get.summary.comm.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.comm.cost.t12.index_fx.sex"]]<- get.summary.comm.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)




# clinic.cost -----
print("clinic.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.clinic.cost.t12"]]<- get.summary.clinic.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.clinic.cost.t12.index_fx"]]<- get.summary.clinic.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.clinic.cost.t12.sex"]]<- get.summary.clinic.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.clinic.cost.t12.index_fx.sex"]]<- get.summary.clinic.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)




# temp.rehab.cost -----
print("temp.rehab.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.temp.rehab.cost.t12"]]<- get.summary.temp.rehab.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.temp.rehab.cost.t12.index_fx"]]<- get.summary.temp.rehab.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.temp.rehab.cost.t12.sex"]]<- get.summary.temp.rehab.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.temp.rehab.cost.t12.index_fx.sex"]]<- get.summary.temp.rehab.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)




# discharge.clinic.cost -----
print("discharge.clinic.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.discharge.clinic.cost.t12"]]<- get.summary.discharge.clinic.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.discharge.clinic.cost.t12.index_fx"]]<- get.summary.discharge.clinic.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.discharge.clinic.cost.t12.sex"]]<- get.summary.discharge.clinic.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.discharge.clinic.cost.t12.index_fx.sex"]]<- get.summary.discharge.clinic.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)





# medication.cost -----
print("medication.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.medication.cost.t12"]]<- get.summary.medication.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.medication.cost.t12.index_fx"]]<- get.summary.medication.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.medication.cost.t12.sex"]]<- get.summary.medication.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.medication.cost.t12.index_fx.sex"]]<- get.summary.medication.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)






# fx_prev.staff.cost -----
print("medication.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.fx_prev.staff.cost.t12"]]<- get.summary.fx_prev.staff.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.fx_prev.staff.cost.t12.index_fx"]]<- get.summary.fx_prev.staff.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.fx_prev.staff.cost.t12.sex"]]<- get.summary.fx_prev.staff.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.fx_prev.staff.cost.t12.index_fx.sex"]]<- get.summary.fx_prev.staff.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)





# lab.test.cost -----
print("lab.test.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.lab.test.cost.t12"]]<- get.summary.lab.test.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.lab.test.cost.t12.index_fx"]]<- get.summary.lab.test.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.lab.test.cost.t12.sex"]]<- get.summary.lab.test.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.lab.test.cost.t12.index_fx.sex"]]<- get.summary.lab.test.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)





# dxa.cost -----
print("dxa.cost")

user_inputs$costs.time<-12

user_inputs$choose.costs.summary<-"Overall"
output[["summary.dxa.cost.t12"]]<- get.summary.dxa.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.dxa.cost.t12.index_fx"]]<- get.summary.dxa.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.dxa.cost.t12.sex"]]<- get.summary.dxa.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.dxa.cost.t12.index_fx.sex"]]<- get.summary.dxa.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)





## RESOURCE USE TABLES FIVE YEARS ----
# procedures -----
print("procedures")
user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.procedures.t60"]]<- get.summary.procedures(m.TR,
                       microsim_pop,
                       study_pop_n,
                       input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.procedures.t60.index_fx"]]<- get.summary.procedures(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.procedures.t60.sex"]]<- get.summary.procedures(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.procedures.t60.index_fx.sex"]]<- get.summary.procedures(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

# hosp.los -----
print("hosp.los")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.hosp.los.t60"]]<- get.summary.hosp.los(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.hosp.los.t60.index_fx"]]<- get.summary.hosp.los(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.hosp.los.t60.sex"]]<- get.summary.hosp.los(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.hosp.los.t60.index_fx.sex"]]<- get.summary.hosp.los(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)


# temp.rehab.los -----
print("temp.rehab.los")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.temp.rehab.los.t60"]]<- get.summary.temp.rehab.los(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.temp.rehab.los.t60.index_fx"]]<- get.summary.temp.rehab.los(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.temp.rehab.los.t60.sex"]]<- get.summary.temp.rehab.los(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.temp.rehab.los.t60.index_fx.sex"]]<- get.summary.temp.rehab.los(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)



# comm.visits -----
print("comm.visits")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.comm.visits.t60"]]<- get.summary.comm.visits(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.comm.visits.t60.index_fx"]]<- get.summary.comm.visits(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.comm.visits.t60.sex"]]<- get.summary.comm.visits(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.comm.visits.t60.index_fx.sex"]]<- get.summary.comm.visits(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# lab.test -----
print("lab.test")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.lab.test.t60"]]<- get.summary.lab.test(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.lab.test.t60.index_fx"]]<- get.summary.lab.test(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.lab.test.t60.sex"]]<- get.summary.lab.test(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.lab.test.t60.index_fx.sex"]]<- get.summary.lab.test(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)





# dxa -----
print("dxa")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.dxa.t60"]]<- get.summary.dxa(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.dxa.t60.index_fx"]]<- get.summary.dxa(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.dxa.t60.sex"]]<- get.summary.dxa(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.dxa.t60.index_fx.sex"]]<- get.summary.dxa(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)






# doctor.mins -----
print("doctor.mins")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.doctor.mins.t60"]]<- get.summary.doctor.mins(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.doctor.mins.t60.index_fx"]]<- get.summary.doctor.mins(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.doctor.mins.t60.sex"]]<- get.summary.doctor.mins(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.doctor.mins.t60.index_fx.sex"]]<- get.summary.doctor.mins(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)



# administrator.mins -----
print("administrator.mins")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.administrator.mins.t60"]]<- get.summary.administrator.mins(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.administrator.mins.t60.index_fx"]]<- get.summary.administrator.mins(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.administrator.mins.t60.sex"]]<- get.summary.administrator.mins(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.administrator.mins.t60.index_fx.sex"]]<- get.summary.administrator.mins(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# nurse.mins -----
print("nurse.mins")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.nurse.mins.t60"]]<- get.summary.nurse.mins(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.nurse.mins.t60.index_fx"]]<- get.summary.nurse.mins(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.nurse.mins.t60.sex"]]<- get.summary.nurse.mins(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.nurse.mins.t60.index_fx.sex"]]<- get.summary.nurse.mins(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# radiographer.mins -----
print("radiographer.mins")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.radiographer.mins.t60"]]<- get.summary.radiographer.mins(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.radiographer.mins.t60.index_fx"]]<- get.summary.radiographer.mins(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.radiographer.mins.t60.sex"]]<- get.summary.radiographer.mins(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.radiographer.mins.t60.index_fx.sex"]]<- get.summary.radiographer.mins(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# allied_health.mins -----
print("allied_health.mins")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.allied_health.mins.t60"]]<- get.summary.allied_health.mins(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.allied_health.mins.t60.index_fx"]]<- get.summary.allied_health.mins(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.allied_health.mins.t60.sex"]]<- get.summary.allied_health.mins(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.allied_health.mins.t60.index_fx.sex"]]<- get.summary.allied_health.mins(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)




# fls_coordinator.mins -----
print("fls_coordinator.mins")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.fls_coordinator.mins.t60"]]<- get.summary.fls_coordinator.mins(m.TR,
                                                                            microsim_pop,
                                                                            study_pop_n,
                                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.fls_coordinator.mins.t60.index_fx"]]<- get.summary.fls_coordinator.mins(m.TR,
                                                                                     microsim_pop,
                                                                                     study_pop_n,
                                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.fls_coordinator.mins.t60.sex"]]<- get.summary.fls_coordinator.mins(m.TR,
                                                                                microsim_pop,
                                                                                study_pop_n,
                                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.fls_coordinator.mins.t60.index_fx.sex"]]<- get.summary.fls_coordinator.mins(m.TR,
                                                                                         microsim_pop,
                                                                                         study_pop_n,
                                                                                         input=user_inputs)





# other.mins -----
print("other.mins")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.other.mins.t60"]]<- get.summary.other.mins(m.TR,
                                                              microsim_pop,
                                                              study_pop_n,
                                                              input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.other.mins.t60.index_fx"]]<- get.summary.other.mins(m.TR,
                                                                       microsim_pop,
                                                                       study_pop_n,
                                                                       input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.other.mins.t60.sex"]]<- get.summary.other.mins(m.TR,
                                                                  microsim_pop,
                                                                  study_pop_n,
                                                                  input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.other.mins.t60.index_fx.sex"]]<- get.summary.other.mins(m.TR,
                                                                           microsim_pop,
                                                                           study_pop_n,
                                                                           input=user_inputs)




# discharge.clinic.visits -----
print("discharge.clinic.visits")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.discharge.clinic.visits.t60"]]<- get.summary.discharge.clinic.visits(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.discharge.clinic.visits.t60.index_fx"]]<- get.summary.discharge.clinic.visits(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.discharge.clinic.visits.t60.sex"]]<- get.summary.discharge.clinic.visits(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.discharge.clinic.visits.t60.index_fx.sex"]]<- get.summary.discharge.clinic.visits(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)





# clinic.visits -----
print("clinic.visits")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.clinic.visits.t60"]]<- get.summary.clinic.visits(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.clinic.visits.t60.index_fx"]]<- get.summary.clinic.visits(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.clinic.visits.t60.sex"]]<- get.summary.clinic.visits(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.clinic.visits.t60.index_fx.sex"]]<- get.summary.clinic.visits(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)





# location table ----
print("location")

user_inputs$hcru.time<-60

user_inputs$choose.hcru.summary<-"Overall"

output[["summary.location.t60"]]<- 
  get.summary.h_no_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>% 
   left_join(
    get.summary.f_home(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n")) %>%  
  left_join(
    get.summary.h_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n")) %>% 
  left_join(
    get.summary.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n"))

output[["summary.location.t60"]]<-
  kable(output[["summary.location.t60"]],
              col.names = c("Intervention",
                    "N target population",
                     "Home, no support (years)",
                     "Difference in home, no support (years)",
                     "Family home (years)",
                     "Difference in family home (years)",
                     "Home, support (years)",
                     "Difference in home, support (years)",
                     "Long term care (years)",
                     "Difference in long term care (years)")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)


user_inputs$choose.hcru.summary<-"By sentinel fracture"
  output[["summary.location.t60.index_fx"]]<- get.summary.h_no_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>% 
  left_join(
    get.summary.h_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "index_fx")) %>% 
  left_join(
    get.summary.f_home(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "index_fx")) %>% 
  left_join(
    get.summary.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "index_fx"))

output[["summary.location.t60.index_fx"]]<-
  kable(output[["summary.location.t60.index_fx"]],
              col.names = c("Index fracture",
                            "Intervention",
                    "N target population",
                     "Home, no support (years)",
                     "Difference in home, no support (years)",
                     "Home, no support (years)",
                     "Difference in home, support (years)",
                     "Family home (years)",
                     "Difference in family home (years)",
                     "Long term care (years)",
                     "Difference in long term care (years)")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.location.t60.sex"]]<- get.summary.h_no_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>% 
    left_join(
    get.summary.f_home(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "sex")) %>% 
  left_join(
    get.summary.h_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "sex")) %>% 

  left_join(
    get.summary.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n", "sex"))

output[["summary.location.t60.sex"]]<-
  kable(output[["summary.location.t60.sex"]],
              col.names = c("Sex",
                "Intervention",
                    "N target population",
                     "Home, no support (years)",
                     "Difference in home, no support (years)",
                     "Family home (years)",
                     "Difference in family home (years)",
                     "Home, support (years)",
                     "Difference in home, support (years)",
                     "Long term care (years)",
                     "Difference in long term care (years)")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.location.t60.index_fx.sex"]]<- get.summary.h_no_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>%  
  left_join(
    get.summary.f_home(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n","index_fx", "sex")) %>% 
  left_join(
    get.summary.h_supp(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n","index_fx", "sex")) %>% 
  left_join(
    get.summary.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n","index_fx", "sex"))

output[["summary.location.t60.index_fx.sex"]]<-
  kable(output[["summary.location.t60.index_fx.sex"]],
              col.names = c("Intervention","Index fracture", "Sex",
                    "N target population",
                     "Home, no support (years)",
                     "Difference in home, no support (years)",
                     "Family home (years)",
                     "Difference in family home (years)",
                     "Home, support (years)",
                     "Difference in home, support (years)",
                     "Long term care (years)",
                     "Difference in long term care (years)")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)




# location table- ever long term care ----
print("location - ever long term care")

user_inputs$choose.hcru.summary<-"Overall"
output[["summary.ever.ltc"]]<-   get.summary.ever.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.ever.ltc.index_fx"]]<-   get.summary.ever.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sex"
output[["summary.ever.ltc.sex"]]<-   get.summary.ever.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs)

user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.ever.ltc.index_fx.sex"]]<-   get.summary.ever.ltc(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs)


# LOCATION PLOT  -----
# location -----
output[["summary.m.TR.location.over_time.index_fx.sex"]]<-m.TR %>%
  ungroup() %>%
  mutate(location=str_to_sentence(location)) %>%
  mutate(location=factor(location,
                           levels=c("Died",
                                    "Home, no support",
                                    "Family home",
                                    "Home, support",
                                    "Long term care")))  %>%
  group_by(month,intervention,index_fx, sex, location, .drop=FALSE) %>%
  tally(name="n.location")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx, sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
 ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx),
         sex=str_to_sentence(sex)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))

# index_fx
output[["summary.m.TR.location.over_time.index_fx"]]<-
  m.TR %>%
  ungroup() %>%
  mutate(location=str_to_sentence(location)) %>%
  mutate(location=factor(location,
                           levels=c("Died",
                                    "Home, no support",
                                    "Family home",
                                    "Home, support",
                                    "Long term care")))  %>%
  group_by(month,intervention,index_fx, location, .drop=FALSE) %>%
  tally(name="n.location")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(index_fx,.drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(index_fx=str_to_sentence(index_fx)) %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other")))
# sex
output[["summary.m.TR.location.over_time.sex"]]<- m.TR %>%
  ungroup() %>%
  mutate(location=str_to_sentence(location)) %>%
  mutate(location=factor(location,
                           levels=c("Died",
                                    "Home, no support",
                                    "Family home",
                                    "Home, support",
                                    "Long term care")))  %>%
  group_by(month,intervention, sex, location, .drop=FALSE) %>%
  tally(name="n.location")%>%
  # add total n
  left_join(
    working.microsim_pop %>%
      group_by(sex, .drop=FALSE) %>%
      tally(name="n_microsim")) %>%
  ungroup()   %>%mutate(sex=str_to_sentence(sex)) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))
# overall
output[["summary.m.TR.location.over_time.overall"]]<- m.TR %>%
  ungroup() %>%
  mutate(location=str_to_sentence(location)) %>%
  mutate(location=factor(location,
                           levels=c("Died",
                                    "Home, no support",
                                    "Family home",
                                    "Home, support",
                                    "Long term care")))  %>%
  group_by(month,intervention, location, .drop=FALSE) %>%
  tally(name="n.location")%>%
  mutate(n_microsim=working.microsim_pop %>%   tally(name="n_microsim") %>% pull())






# location plot -----
# drop unused locations
unused<-output$summary.m.TR.location.over_time.overall %>%
  group_by(location) %>%
  summarise(n=sum(n.location)) %>%
  filter(n==0) %>%
  mutate(location=as.character(location)) %>%
  select(location)

output[["location.plot.overall"]]<-
output$summary.m.TR.location.over_time.overall   %>%
   anti_join(unused) %>%
      ggplot()+
      facet_grid(.~ intervention)+
      geom_col(aes(month, n.location/n_microsim,
                   fill=location), width=1)
output[["location.plot.overall"]]<-gg.general.format(output[["location.plot.overall"]])
output[["location.plot.overall"]]<-gg.add.colours.location(output[["location.plot.overall"]])


output[["location.plot.index_fx"]]<-
output$summary.m.TR.location.over_time.index_fx   %>%
   anti_join(unused)  %>%
      ggplot()+
      facet_grid(index_fx~ intervention)+
      geom_col(aes(month, n.location/n_microsim,
                   fill=location), width=1)
output[["location.plot.index_fx"]]<-gg.general.format(output[["location.plot.index_fx"]])
output[["location.plot.index_fx"]]<-gg.add.colours.location(output[["location.plot.index_fx"]])

output[["location.plot.sex"]]<-
output$summary.m.TR.location.over_time.sex   %>%
   anti_join(unused)  %>%
      ggplot()+
      facet_grid(sex~ intervention)+
      geom_col(aes(month, n.location/n_microsim,
                   fill=location), width=1)
output[["location.plot.sex"]]<-gg.general.format(output[["location.plot.sex"]])
output[["location.plot.sex"]]<-gg.add.colours.location(output[["location.plot.sex"]])

output[["location.plot.index_fx.sex"]]<-
output$summary.m.TR.location.over_time.index_fx.sex   %>%
   anti_join(unused)  %>%
      ggplot()+
      facet_grid(index_fx+ sex~ intervention)+
      geom_col(aes(month, n.location/n_microsim,
                   fill=location), width=1)
output[["location.plot.index_fx.sex"]]<-gg.general.format(output[["location.plot.index_fx.sex"]])
output[["location.plot.index_fx.sex"]]<-gg.add.colours.location(output[["location.plot.index_fx.sex"]])



# hosp los cat table ----
print("hosp los cat")



user_inputs$choose.hcru.summary<-"Overall"

output[["summary.hosp.los.cat"]]<- 
  get.fac.days.in.hospital_0(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>% 
   left_join(
    get.fac.days.in.hospital_1_7(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n")) %>%  
  left_join(
    get.fac.days.in.hospital_8_14(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n")) %>% 
  left_join(
    get.fac.days.in.hospital_15_30(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n")) %>% 
  left_join(
    get.fac.days.in.hospital_31up(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("intervention", "study.pop.n"))

output[["summary.hosp.los.cat"]]<-
  kable(output[["summary.hosp.los.cat"]],
              col.names = c("Intervention",
                    "N target population",
                     "Hospital days: 0",
                     "Difference",
                     "Hospital days: 1 to 7",
                     "Difference",
                     "Hospital days: 8 to 14",
                     "Difference",
                     "Hospital days: 15 to 30",
                     "Difference",
                     "Hospital days: 31 or more",
                     "Difference")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)


user_inputs$choose.hcru.summary<-"By sentinel fracture"
output[["summary.hosp.los.cat.index_fx"]]<- 
  get.fac.days.in.hospital_0(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>% 
   left_join(
    get.fac.days.in.hospital_1_7(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("index_fx" ,"intervention", "study.pop.n")) %>%  
  left_join(
    get.fac.days.in.hospital_8_14(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("index_fx" ,"intervention", "study.pop.n")) %>% 
  left_join(
    get.fac.days.in.hospital_15_30(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("index_fx" ,"intervention", "study.pop.n")) %>% 
  left_join(
    get.fac.days.in.hospital_31up(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("index_fx" ,"intervention", "study.pop.n"))

output[["summary.hosp.los.cat.index_fx"]]<-
  kable(output[["summary.hosp.los.cat.index_fx"]],
              col.names = c("Index fracture",
                            "Intervention",
                    "N target population",
                     "Hospital days: 0",
                     "Difference",
                     "Hospital days: 1 to 7",
                     "Difference",
                     "Hospital days: 8 to 14",
                     "Difference",
                     "Hospital days: 15 to 30",
                     "Difference",
                     "Hospital days: 31 or more",
                     "Difference")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)


user_inputs$choose.hcru.summary<-"By sex"
output[["summary.hosp.los.cat.sex"]]<- 
  get.fac.days.in.hospital_0(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>% 
   left_join(
    get.fac.days.in.hospital_1_7(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("sex" ,"intervention", "study.pop.n")) %>%  
  left_join(
    get.fac.days.in.hospital_8_14(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("sex" ,"intervention", "study.pop.n")) %>% 
  left_join(
    get.fac.days.in.hospital_15_30(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("sex" ,"intervention", "study.pop.n")) %>% 
  left_join(
    get.fac.days.in.hospital_31up(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("sex" ,"intervention", "study.pop.n"))

output[["summary.hosp.los.cat.sex"]]<-
  kable(output[["summary.hosp.los.cat.sex"]],
              col.names = c("Sex",
                            "Intervention",
                    "N target population",
                     "Hospital days: 0",
                     "Difference",
                     "Hospital days: 1 to 7",
                     "Difference",
                     "Hospital days: 8 to 14",
                     "Difference",
                     "Hospital days: 15 to 30",
                     "Difference",
                     "Hospital days: 31 or more",
                     "Difference")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)


user_inputs$choose.hcru.summary<-"By sentinel fracture and sex"
output[["summary.hosp.los.cat.index_fx.sex"]]<- 
  get.fac.days.in.hospital_0(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs) %>% 
   left_join(
    get.fac.days.in.hospital_1_7(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("index_fx", "sex" ,"intervention", "study.pop.n")) %>%  
  left_join(
    get.fac.days.in.hospital_8_14(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("index_fx","sex" ,"intervention", "study.pop.n")) %>% 
  left_join(
    get.fac.days.in.hospital_15_30(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("index_fx","sex" ,"intervention", "study.pop.n")) %>% 
  left_join(
    get.fac.days.in.hospital_31up(m.TR,
                      microsim_pop,
                      study_pop_n,
                     input=user_inputs),
    by=c("index_fx","sex" ,"intervention", "study.pop.n"))

output[["summary.hosp.los.cat.index_fx.sex"]]<-
  kable(output[["summary.hosp.los.cat.index_fx.sex"]],
              col.names = c("Index fracture","Sex",
                            "Intervention",
                    "N target population",
                     "Hospital days: 0",
                     "Difference",
                     "Hospital days: 1 to 7",
                     "Difference",
                     "Hospital days: 8 to 14",
                     "Difference",
                     "Hospital days: 15 to 30",
                     "Difference",
                     "Hospital days: 31 or more",
                     "Difference")
        ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")
)





## COST TABLES FIVE YEARS ----
# total.cost.excl.location -----
print("total.cost.excl.location")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.total.cost.excl.location.t60"]]<- get.summary.total.cost.excl.location(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.total.cost.excl.location.t60.index_fx"]]<- get.summary.total.cost.excl.location(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.total.cost.excl.location.t60.sex"]]<- get.summary.total.cost.excl.location(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.total.cost.excl.location.t60.index_fx.sex"]]<- get.summary.total.cost.excl.location(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)


# total.cost -----
print("total.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.total.cost.t60"]]<- get.summary.total.cost(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.total.cost.t60.index_fx"]]<- get.summary.total.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.total.cost.t60.sex"]]<- get.summary.total.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.total.cost.t60.index_fx.sex"]]<- get.summary.total.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

print("discounted.total.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.discounted.total.cost.t60"]]<- get.summary.discounted.total.cost(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.discounted.total.cost.t60.index_fx"]]<- get.summary.discounted.total.cost(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.discounted.total.cost.t60.sex"]]<- get.summary.discounted.total.cost(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.discounted.total.cost.t60.index_fx.sex"]]<- get.summary.discounted.total.cost(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)









user_inputs$min.costs.time<-12
user_inputs$costs.time<-24
user_inputs$choose.costs.summary<-"Overall"
output[["summary.total.cost.t12_t24"]]<- get.summary.total.cost1(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)
user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.total.cost.t12_t24.index_fx"]]<- get.summary.total.cost1(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)
user_inputs$choose.costs.summary<-"By sex"
output[["summary.total.cost.t12_t24.sex"]]<- get.summary.total.cost1(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)
user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.total.cost.t12_t24.index_fx.sex"]]<- get.summary.total.cost1(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)

user_inputs$min.costs.time<-24
user_inputs$costs.time<-36
user_inputs$choose.costs.summary<-"Overall"
output[["summary.total.cost.t24_t36"]]<- get.summary.total.cost1(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)
user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.total.cost.t24_t36.index_fx"]]<- get.summary.total.cost1(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)
user_inputs$choose.costs.summary<-"By sex"
output[["summary.total.cost.t24_t36.sex"]]<- get.summary.total.cost1(m.TR,
                                                                    microsim_pop,
                                                                    study_pop_n,
                                                                    input=user_inputs)
user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.total.cost.t24_t36.index_fx.sex"]]<- get.summary.total.cost1(m.TR,
                                                                             microsim_pop,
                                                                             study_pop_n,
                                                                             input=user_inputs)

user_inputs$min.costs.time<-36
user_inputs$costs.time<-48
user_inputs$choose.costs.summary<-"Overall"
output[["summary.total.cost.t36_t48"]]<- get.summary.total.cost1(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)
user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.total.cost.t36_t48.index_fx"]]<- get.summary.total.cost1(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)
user_inputs$choose.costs.summary<-"By sex"
output[["summary.total.cost.t36_t48.sex"]]<- get.summary.total.cost1(m.TR,
                                                                    microsim_pop,
                                                                    study_pop_n,
                                                                    input=user_inputs)
user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.total.cost.t36_t48.index_fx.sex"]]<- get.summary.total.cost1(m.TR,
                                                                             microsim_pop,
                                                                             study_pop_n,
                                                                             input=user_inputs)

user_inputs$min.costs.time<-48
user_inputs$costs.time<-60
user_inputs$choose.costs.summary<-"Overall"
output[["summary.total.cost.t48_t60"]]<- get.summary.total.cost1(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)
user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.total.cost.t48_t60.index_fx"]]<- get.summary.total.cost1(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)
user_inputs$choose.costs.summary<-"By sex"
output[["summary.total.cost.t48_t60.sex"]]<- get.summary.total.cost1(m.TR,
                                                                    microsim_pop,
                                                                    study_pop_n,
                                                                    input=user_inputs)
user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.total.cost.t48_t60.index_fx.sex"]]<- get.summary.total.cost1(m.TR,
                                                                             microsim_pop,
                                                                             study_pop_n,
                                                                             input=user_inputs)







# hosp.cost -----
print("hosp.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.hosp.cost.t60"]]<- get.summary.hosp.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.hosp.cost.t60.index_fx"]]<- get.summary.hosp.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.hosp.cost.t60.sex"]]<- get.summary.hosp.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.hosp.cost.t60.index_fx.sex"]]<- get.summary.hosp.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)


# procedure.cost -----
print("procedure.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.procedure.cost.t60"]]<- get.summary.procedure.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.procedure.cost.t60.index_fx"]]<- get.summary.procedure.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.procedure.cost.t60.sex"]]<- get.summary.procedure.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.procedure.cost.t60.index_fx.sex"]]<- get.summary.procedure.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)



# location_home_support.cost -----
print("location_home_support.cost")
user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.location_home_support.cost.t60"]]<- get.summary.location_home_support.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.location_home_support.cost.t60.index_fx"]]<- get.summary.location_home_support.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.location_home_support.cost.t60.sex"]]<- get.summary.location_home_support.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.location_home_support.cost.t60.index_fx.sex"]]<- get.summary.location_home_support.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)


# location_long_term_care.cost -----
print("location_long_term_care.cost")
user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.location_long_term_care.cost.t60"]]<- get.summary.location_long_term_care.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.location_long_term_care.cost.t60.index_fx"]]<- get.summary.location_long_term_care.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.location_long_term_care.cost.t60.sex"]]<- get.summary.location_long_term_care.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.location_long_term_care.cost.t60.index_fx.sex"]]<- get.summary.location_long_term_care.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)



# comm.cost -----
print("comm.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.comm.cost.t60"]]<- get.summary.comm.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.comm.cost.t60.index_fx"]]<- get.summary.comm.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.comm.cost.t60.sex"]]<- get.summary.comm.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.comm.cost.t60.index_fx.sex"]]<- get.summary.comm.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)




# clinic.cost -----
print("clinic.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.clinic.cost.t60"]]<- get.summary.clinic.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.clinic.cost.t60.index_fx"]]<- get.summary.clinic.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.clinic.cost.t60.sex"]]<- get.summary.clinic.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.clinic.cost.t60.index_fx.sex"]]<- get.summary.clinic.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)




# temp.rehab.cost -----
print("temp.rehab.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.temp.rehab.cost.t60"]]<- get.summary.temp.rehab.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.temp.rehab.cost.t60.index_fx"]]<- get.summary.temp.rehab.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.temp.rehab.cost.t60.sex"]]<- get.summary.temp.rehab.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.temp.rehab.cost.t60.index_fx.sex"]]<- get.summary.temp.rehab.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)




# discharge.clinic.cost -----
print("discharge.clinic.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.discharge.clinic.cost.t60"]]<- get.summary.discharge.clinic.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.discharge.clinic.cost.t60.index_fx"]]<- get.summary.discharge.clinic.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.discharge.clinic.cost.t60.sex"]]<- get.summary.discharge.clinic.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.discharge.clinic.cost.t60.index_fx.sex"]]<- get.summary.discharge.clinic.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)





# medication.cost -----
print("medication.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.medication.cost.t60"]]<- get.summary.medication.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.medication.cost.t60.index_fx"]]<- get.summary.medication.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.medication.cost.t60.sex"]]<- get.summary.medication.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.medication.cost.t60.index_fx.sex"]]<- get.summary.medication.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)






# fx_prev.staff.cost -----
print("medication.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.fx_prev.staff.cost.t60"]]<- get.summary.fx_prev.staff.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.fx_prev.staff.cost.t60.index_fx"]]<- get.summary.fx_prev.staff.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.fx_prev.staff.cost.t60.sex"]]<- get.summary.fx_prev.staff.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.fx_prev.staff.cost.t60.index_fx.sex"]]<- get.summary.fx_prev.staff.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)





# lab.test.cost -----
print("lab.test.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.lab.test.cost.t60"]]<- get.summary.lab.test.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.lab.test.cost.t60.index_fx"]]<- get.summary.lab.test.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.lab.test.cost.t60.sex"]]<- get.summary.lab.test.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.lab.test.cost.t60.index_fx.sex"]]<- get.summary.lab.test.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)





# dxa.cost -----
print("dxa.cost")

user_inputs$costs.time<-60

user_inputs$choose.costs.summary<-"Overall"
output[["summary.dxa.cost.t60"]]<- get.summary.dxa.cost(m.TR,
                                                                                        microsim_pop,
                                                                                        study_pop_n,
                                                                                        input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture"
output[["summary.dxa.cost.t60.index_fx"]]<- get.summary.dxa.cost(m.TR,
                                                                                                 microsim_pop,
                                                                                                 study_pop_n,
                                                                                                 input=user_inputs)

user_inputs$choose.costs.summary<-"By sex"
output[["summary.dxa.cost.t60.sex"]]<- get.summary.dxa.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)

user_inputs$choose.costs.summary<-"By sentinel fracture and sex"
output[["summary.dxa.cost.t60.index_fx.sex"]]<- get.summary.dxa.cost(m.TR,
                                                                                            microsim_pop,
                                                                                            study_pop_n,
                                                                                            input=user_inputs)





# QOL  -----

print("qol")

user_inputs$qol.time<-12

user_inputs$choose.qol.summary<-"Overall"
output[["summary.qol.t12"]]<- get.summary.qol(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)



user_inputs$choose.qol.summary<-"By sentinel fracture"
output[["summary.qol.t12.index_fx"]]<- get.summary.qol(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.qol.summary<-"By sex"
output[["summary.qol.t12.sex"]]<- get.summary.qol(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.qol.summary<-"By sentinel fracture and sex"
output[["summary.qol.t12.index_fx.sex"]]<- get.summary.qol(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)


user_inputs$qol.time<-60

user_inputs$choose.qol.summary<-"Overall"
output[["summary.qol.t60"]]<- get.summary.qol(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)

user_inputs$choose.qol.summary<-"By sentinel fracture"
output[["summary.qol.t60.index_fx"]]<- get.summary.qol(m.TR,
                                                                     microsim_pop,
                                                                     study_pop_n,
                                                                     input=user_inputs)

user_inputs$choose.qol.summary<-"By sex"
output[["summary.qol.t60.sex"]]<- get.summary.qol(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$choose.qol.summary<-"By sentinel fracture and sex"
output[["summary.qol.t60.index_fx.sex"]]<- get.summary.qol(m.TR,
                                                                         microsim_pop,
                                                                         study_pop_n,
                                                                         input=user_inputs)



user_inputs$min.qol.time<-12
user_inputs$qol.time<-24
user_inputs$choose.qol.summary<-"Overall"
output[["summary.qol.t12_t24"]]<- get.summary.qol1(m.TR,
                                              microsim_pop,
                                              study_pop_n,
                                              input=user_inputs)
user_inputs$choose.qol.summary<-"By sentinel fracture"
output[["summary.qol.t12_t24.index_fx"]]<- get.summary.qol1(m.TR,
                                                       microsim_pop,
                                                       study_pop_n,
                                                       input=user_inputs)
user_inputs$choose.qol.summary<-"By sex"
output[["summary.qol.t12_t24.sex"]]<- get.summary.qol1(m.TR,
                                                  microsim_pop,
                                                  study_pop_n,
                                                  input=user_inputs)
user_inputs$choose.qol.summary<-"By sentinel fracture and sex"
output[["summary.qol.t12_t24.index_fx.sex"]]<- get.summary.qol1(m.TR,
                                                           microsim_pop,
                                                           study_pop_n,
                                                           input=user_inputs)

user_inputs$min.qol.time<-24
user_inputs$qol.time<-36
user_inputs$choose.qol.summary<-"Overall"
output[["summary.qol.t24_t36"]]<- get.summary.qol1(m.TR,
                                                   microsim_pop,
                                                   study_pop_n,
                                                   input=user_inputs)
user_inputs$choose.qol.summary<-"By sentinel fracture"
output[["summary.qol.t24_t36.index_fx"]]<- get.summary.qol1(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)
user_inputs$choose.qol.summary<-"By sex"
output[["summary.qol.t24_t36.sex"]]<- get.summary.qol1(m.TR,
                                                       microsim_pop,
                                                       study_pop_n,
                                                       input=user_inputs)
user_inputs$choose.qol.summary<-"By sentinel fracture and sex"
output[["summary.qol.t24_t36.index_fx.sex"]]<- get.summary.qol1(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$min.qol.time<-36
user_inputs$qol.time<-48
user_inputs$choose.qol.summary<-"Overall"
output[["summary.qol.t36_t48"]]<- get.summary.qol1(m.TR,
                                                   microsim_pop,
                                                   study_pop_n,
                                                   input=user_inputs)
user_inputs$choose.qol.summary<-"By sentinel fracture"
output[["summary.qol.t36_t48.index_fx"]]<- get.summary.qol1(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)
user_inputs$choose.qol.summary<-"By sex"
output[["summary.qol.t36_t48.sex"]]<- get.summary.qol1(m.TR,
                                                       microsim_pop,
                                                       study_pop_n,
                                                       input=user_inputs)
user_inputs$choose.qol.summary<-"By sentinel fracture and sex"
output[["summary.qol.t36_t48.index_fx.sex"]]<- get.summary.qol1(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)

user_inputs$min.qol.time<-48
user_inputs$qol.time<-60
user_inputs$choose.qol.summary<-"Overall"
output[["summary.qol.t48_t60"]]<- get.summary.qol1(m.TR,
                                                   microsim_pop,
                                                   study_pop_n,
                                                   input=user_inputs)
user_inputs$choose.qol.summary<-"By sentinel fracture"
output[["summary.qol.t48_t60.index_fx"]]<- get.summary.qol1(m.TR,
                                                            microsim_pop,
                                                            study_pop_n,
                                                            input=user_inputs)
user_inputs$choose.qol.summary<-"By sex"
output[["summary.qol.t48_t60.sex"]]<- get.summary.qol1(m.TR,
                                                       microsim_pop,
                                                       study_pop_n,
                                                       input=user_inputs)
user_inputs$choose.qol.summary<-"By sentinel fracture and sex"
output[["summary.qol.t48_t60.index_fx.sex"]]<- get.summary.qol1(m.TR,
                                                                microsim_pop,
                                                                study_pop_n,
                                                                input=user_inputs)




print("discounted.qol")

user_inputs$discounted.qol.time<-12

user_inputs$choose.discounted.qol.summary<-"Overall"
output[["summary.discounted.qol.t12"]]<- get.summary.discounted.qol(m.TR,
                                              microsim_pop,
                                              study_pop_n,
                                              input=user_inputs)



user_inputs$choose.discounted.qol.summary<-"By sentinel fracture"
output[["summary.discounted.qol.t12.index_fx"]]<- get.summary.discounted.qol(m.TR,
                                                       microsim_pop,
                                                       study_pop_n,
                                                       input=user_inputs)

user_inputs$choose.discounted.qol.summary<-"By sex"
output[["summary.discounted.qol.t12.sex"]]<- get.summary.discounted.qol(m.TR,
                                                  microsim_pop,
                                                  study_pop_n,
                                                  input=user_inputs)

user_inputs$choose.discounted.qol.summary<-"By sentinel fracture and sex"
output[["summary.discounted.qol.t12.index_fx.sex"]]<- get.summary.discounted.qol(m.TR,
                                                           microsim_pop,
                                                           study_pop_n,
                                                           input=user_inputs)


user_inputs$discounted.qol.time<-60

user_inputs$choose.discounted.qol.summary<-"Overall"
output[["summary.discounted.qol.t60"]]<- get.summary.discounted.qol(m.TR,
                                              microsim_pop,
                                              study_pop_n,
                                              input=user_inputs)

user_inputs$choose.discounted.qol.summary<-"By sentinel fracture"
output[["summary.discounted.qol.t60.index_fx"]]<- get.summary.discounted.qol(m.TR,
                                                       microsim_pop,
                                                       study_pop_n,
                                                       input=user_inputs)

user_inputs$choose.discounted.qol.summary<-"By sex"
output[["summary.discounted.qol.t60.sex"]]<- get.summary.discounted.qol(m.TR,
                                                  microsim_pop,
                                                  study_pop_n,
                                                  input=user_inputs)

user_inputs$choose.discounted.qol.summary<-"By sentinel fracture and sex"
output[["summary.discounted.qol.t60.index_fx.sex"]]<- get.summary.discounted.qol(m.TR,
                                                           microsim_pop,
                                                           study_pop_n,
                                                           input=user_inputs)









# output[["plot.qol.index_fx"]]<- m.TR %>%
#   group_by(month, index_fx,intervention) %>%
#   summarise(qol=mean(qol)) %>%
#   ggplot()+
#   facet_grid(index_fx~ intervention)+
#   geom_line(aes(month,qol, colour=index_fx), linetype="dashed")+
#   geom_point(aes(month,qol, colour=index_fx))+
#   theme_bw()
# output[["plot.qol.index_fx"]]<- gg.general.format.not.perc(output[["plot.qol.index_fx"]])
# output[["plot.qol.index_fx"]]<- gg.add.colours.sub.frac(output[["plot.qol.index_fx"]])
# 





# output -----
#not needed
output[["summary.m.TR.medication.over_time.index_fx.sex"]]<-NULL
output[["summary.m.TR.treat.over_time.index_fx.sex.risk.profile"]]<-NULL
output[["summary.m.TR.identified.over_time.index_fx.sex"]]<-NULL
output[["summary.m.TR.treat.over_time.index_fx.sex"]]<-NULL
output[["summary.m.TR.treat.over_time.index_fx.risk.type"]]<-NULL
output[["summary.m.TR.treat.over_time.sex.risk.type"]]<-NULL
output[["summary.m.TR.treat.over_time.risk.type"]]<-NULL
output[["summary.m.TR.adhering.over_time.index_fx.sex"]]<-NULL
output[["summary.m.TR.apply.rr.over_time.index_fx.sex"]]<-NULL
output[["summary.m.TR.location.over_time.index_fx.sex"]]<-NULL
output[["summary.m.TR.location.over_time.index_fx"]]<-NULL

output

}
