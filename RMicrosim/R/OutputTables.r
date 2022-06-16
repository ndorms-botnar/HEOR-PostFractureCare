## summary tables
# total number of subsequent fractures -----
get.summary.second.fx<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
sec.frac.time<-input$sec.frac.time


sub.frac<-m.TR.df %>%
  filter(month=={{sec.frac.time}}) %>% #last month of time horizon
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
  select(intervention, index_fx, sex, study.pop.n,
         study.pop.sub.hip.frac,study.pop.sub.spine.frac,study.pop.sub.other.frac,
         study.pop.sub.frac)


if(input$choose.sec.frac.summary=="Overall"){
table<- sub.frac %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             n.hip.fx=sum(study.pop.sub.hip.frac),
             n.spine.fx=sum(study.pop.sub.spine.frac),
             n.other.fx=sum(study.pop.sub.other.frac),
             n.fx=sum(study.pop.sub.frac))

table$difference<-""
table$difference[2]<-as.numeric(table[1,6]- table[2,6])

table<-table %>%
  mutate( n.hip.fx=nice.num.count(n.hip.fx),
         n.spine.fx=nice.num.count(n.spine.fx),
         n.other.fx=nice.num.count(n.other.fx),
         n.fx=nice.num.count(n.fx),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Hip fractures",
                    "Spine fractures",
                    "Other fractures",
                    "All sites",
                    "Subsequent fractures avoided")) %>%
  add_header_above(c("",
                    "",
                    "Subsequent fractures"=4,
                    "")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.sec.frac.summary=="By sentinel fracture"){

table<- sub.frac %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             n.hip.fx=sum(study.pop.sub.hip.frac),
             n.spine.fx=sum(study.pop.sub.spine.frac),
             n.other.fx=sum(study.pop.sub.other.frac),
             n.fx=sum(study.pop.sub.frac))

table$difference<-""
table$difference[2]<-as.numeric(table[1,7]- table[2,7])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,7]- table[4,7])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,7]- table[6,7])
}

table<-table %>%
  mutate( n.hip.fx=nice.num.count(n.hip.fx),
         n.spine.fx=nice.num.count(n.spine.fx),
         n.other.fx=nice.num.count(n.other.fx),
         n.fx=nice.num.count(n.fx),
         difference=nice.num.count(difference))
table$index_fx<-str_to_sentence(table$index_fx)
table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Hip fractures",
                    "Spine fractures",
                    "Other fractures",
                    "All sites",
                    "Subsequent fractures avoided")) %>%
  add_header_above(c("","",
                    "",
                    "Subsequent fractures"=4,
                    "")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))


}else if(input$choose.sec.frac.summary=="By sex"){
table<- sub.frac %>%
  group_by(sex, intervention) %>%
  summarise(study.pop.n=sum(study.pop.n),
             n.hip.fx=sum(study.pop.sub.hip.frac),
             n.spine.fx=sum(study.pop.sub.spine.frac),
             n.other.fx=sum(study.pop.sub.other.frac),
             n.fx=sum(study.pop.sub.frac))

table$difference<-""
table$difference[2]<-as.numeric(table[1,7]- table[2,7])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,7]- table[4,7])
}

table<-table %>%
  mutate( n.hip.fx=nice.num.count(n.hip.fx),
         n.spine.fx=nice.num.count(n.spine.fx),
         n.other.fx=nice.num.count(n.other.fx),
         n.fx=nice.num.count(n.fx),
         difference=nice.num.count(difference))

table$sex<-str_to_sentence(table$sex)
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Hip fractures",
                    "Spine fractures",
                    "Other fractures",
                    "All sites",
                    "Subsequent fractures avoided")) %>%
  add_header_above(c("","",
                    "",
                    "Subsequent fractures"=4,
                    "")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- sub.frac %>%
  group_by(index_fx, sex, intervention) %>%
  summarise(study.pop.n=sum(study.pop.n),
             n.hip.fx=sum(study.pop.sub.hip.frac),
             n.spine.fx=sum(study.pop.sub.spine.frac),
             n.other.fx=sum(study.pop.sub.other.frac),
             n.fx=sum(study.pop.sub.frac))

table$difference<-""
table$difference[2]<-as.numeric(table[1,8]- table[2,8])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,8]- table[4,8])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,8]- table[8,8])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,8]- table[10,8])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,8]- table[12,8])
}

table<-table %>%
  mutate( n.hip.fx=nice.num.count(n.hip.fx),
         n.spine.fx=nice.num.count(n.spine.fx),
         n.other.fx=nice.num.count(n.other.fx),
         n.fx=nice.num.count(n.fx),
         difference=nice.num.count(difference))

table$index_fx<-str_to_sentence(table$index_fx)
table$sex<-str_to_sentence(table$sex)
table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Hip fractures",
                    "Spine fractures",
                    "Other fractures",
                    "All sites",
                    "Subsequent fractures avoided")) %>%
  add_header_above(c("","","",
                    "",
                    "Subsequent fractures"=4,
                    "")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table

}


## for mc error
# at time 60
#by index fx and sex
get.summary.second.fx.mc.error<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input,
                                working.n_microsimulation){
  sec.frac.time<-input$sec.frac.time


  sub.frac<-m.TR.df %>%
    filter(month=={{sec.frac.time}}) %>% #last month of time horizon
    mutate(sub_frac=h_af-1) %>%  # minus first fx
    select(id, index_fx, sex, intervention, sub_frac) %>%
    mutate(intervention=ifelse(intervention=="FLS", "FLS",
                               "Current practice")) %>%
    mutate(intervention=factor(intervention,
                               levels=c("Current practice", "FLS")))

  sub.frac<-sub.frac %>%
    group_by(intervention, index_fx, sex) %>%
    summarise(sum.sub.frac=sum(sub_frac))
  sub.frac$microsim_pop<-working.n_microsimulation


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
  sub.frac$study.pop.sub.frac<- round(sub.frac$sum.sub.frac*
                                        (sub.frac$study.pop.n/sub.frac$microsim_pop))

  sub.frac<-sub.frac %>%
    select(intervention, index_fx, sex, study.pop.n, study.pop.sub.frac)


 #"By sentinel fracture and sex"

    table<- sub.frac %>%
      group_by(index_fx, sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                n.fx=sum(study.pop.sub.frac))

    table$difference<-""
    table$difference[2]<-as.numeric(table[1,5]- table[2,5])

    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,5]- table[4,5])
    }

    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[5,5]- table[6,5])
    }

    if(nrow(table)>7){
      table$difference[8]<-as.numeric(table[7,5]- table[8,5])
    }

    if(nrow(table)>9){
      table$difference[10]<-as.numeric(table[9,5]- table[10,5])
    }

    if(nrow(table)>11){
      table$difference[12]<-as.numeric(table[11,5]- table[12,5])
    }

  #output -----
  table

}


# count of subsequent fractures -----
get.num.subs.fx<-function(m.TR.df,
                          microsim_pop,
                          study_pop_n,
                          input){
 # browser()
sec.frac.time<-input$sec.frac.time 
  

if(input$choose.sec.frac.summary=="Overall"){
subs.fx<-m.TR.df %>% 
  filter(month==sec.frac.time) %>% 
  group_by(id,intervention) %>% 
  mutate(subs.fx=h_af-1) %>%  # minus index fx
  select(id,intervention,  subs.fx)

subs.fx<-subs.fx %>%
  group_by(intervention) %>% 
  summarise(fx.0=sum(subs.fx==0)/length(id),
            fx.1=sum(subs.fx==1)/length(id),
            fx.2=sum(subs.fx==2)/length(id),
            fx.3plus=sum(subs.fx>=3)/length(id))

subs.fx<-subs.fx %>%
 pivot_longer(-c(intervention),
              names_to = "fx", values_to = "prop")
sum(subs.fx$prop)

subs.fx$study.pop.n<-sum(study_pop_n$n)

# get weighted number for study population
subs.fx$fx.scaled<-round(subs.fx$prop*subs.fx$study.pop.n)

table.data<- subs.fx
table.data$fx.scaled<-round(table.data$prop*table.data$study.pop.n)
table.data$fx.scaled_prop<-paste0(nice.num.count(table.data$fx.scaled), " (",
                                  nice.num(table.data$prop*100) ,
                                  "%)")


#wide  
table.data<-table.data %>% 
  select(intervention , study.pop.n, fx, fx.scaled_prop) %>%
  pivot_wider(names_from = fx, values_from = fx.scaled_prop)

  
kable(table.data %>% 
    mutate(study.pop.n=nice.num.count(as.numeric(study.pop.n))),
      col.names = c("Intervention",
                    "N target population",
                    "0",
                    "1",
                    "2",
                    "3+")) %>%
  add_header_above(c("","",
                    "Subsequent fractures"=4
                    ))%>%
  kable_styling(bootstrap_options = c("striped", "bordered"))


}else if(input$choose.sec.frac.summary=="By sentinel fracture"){
subs.fx<-m.TR.df %>% 
  filter(month==sec.frac.time) %>% 
  group_by(id,intervention, index_fx) %>% 
  mutate(subs.fx=h_af-1) %>%  # minus index fx
  select(id,intervention, index_fx, subs.fx)

subs.fx<-subs.fx %>%
  group_by(intervention, index_fx) %>% 
  summarise(fx.0=sum(subs.fx==0)/length(id),
            fx.1=sum(subs.fx==1)/length(id),
            fx.2=sum(subs.fx==2)/length(id),
            fx.3plus=sum(subs.fx>=3)/length(id))

subs.fx<-subs.fx %>%
 pivot_longer(-c(intervention, index_fx),
              names_to = "fx", values_to = "prop")
sum(subs.fx$prop)

subs.fx<-subs.fx %>% 
  left_join(
 rbind(
data.frame(index_fx="Hip",
           study.pop.n=study_pop_n %>% 
             filter(names %in% c("hip_fx_n.male", "hip_fx_n.female")) %>% 
              summarise(sum(n)) %>% pull()),
data.frame(index_fx="Spine",
           study.pop.n=study_pop_n %>% 
             filter(names %in% c("spine_fx_n.male", "spine_fx_n.female")) %>% 
              summarise(sum(n)) %>% pull()),
data.frame(index_fx="Other",
           study.pop.n=study_pop_n %>% 
             filter(names %in% c("other_fx_n.male", "other_fx_n.female")) %>% 
              summarise(sum(n)) %>% pull())))

# get weighted number for study population
subs.fx$fx.scaled<-round(subs.fx$prop*subs.fx$study.pop.n)
    
table.data<- subs.fx
table.data$fx.scaled<-round(table.data$prop*table.data$study.pop.n)
table.data$fx.scaled_prop<-paste0(nice.num.count(table.data$fx.scaled), " (",
                                  nice.num(table.data$prop*100) ,
                                  "%)")

#wide  
table.data<-table.data %>% 
  select(intervention , index_fx, study.pop.n, fx, fx.scaled_prop) %>%
  pivot_wider(names_from = fx, values_from = fx.scaled_prop)
  
table.data<-  table.data %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>% 
  arrange(index_fx)


 kable(table.data %>% 
    arrange(index_fx) %>% 
    mutate(study.pop.n=nice.num.count(as.numeric(study.pop.n))),
      col.names = c("Intervention",
                    "Index fracture",
                    "N target population",
                    "0",
                    "1",
                    "2",
                    "3+")) %>%
  add_header_above(c("","",
                    "",
                    "Subsequent fractures"=4
                    ))%>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.sec.frac.summary=="By sex"){
subs.fx<-m.TR.df %>% 
  filter(month==sec.frac.time) %>% 
  group_by(id,intervention, sex) %>% 
  mutate(subs.fx=h_af-1) %>%  # minus index fx
  select(id,intervention, sex, subs.fx)

subs.fx<-subs.fx %>%
  group_by(intervention, sex) %>% 
  summarise(fx.0=sum(subs.fx==0)/length(id),
            fx.1=sum(subs.fx==1)/length(id),
            fx.2=sum(subs.fx==2)/length(id),
            fx.3plus=sum(subs.fx>=3)/length(id))

subs.fx<-subs.fx %>%
 pivot_longer(-c(intervention, sex),
              names_to = "fx", values_to = "prop")
sum(subs.fx$prop)

subs.fx<-subs.fx %>% 
  left_join(
 rbind(
data.frame(sex="Male",
           study.pop.n=study_pop_n %>% 
             filter(names %in% c("hip_fx_n.male", "spine_fx_n.male",
                                 "other_fx_n.male")) %>% 
              summarise(sum(n)) %>% pull()),
data.frame(sex="Female",
           study.pop.n=study_pop_n %>% 
             filter(names %in% c("hip_fx_n.female", "spine_fx_n.female",
                                 "other_fx_n.female")) %>% 
              summarise(sum(n)) %>% pull())))

# get weighted number for study population
subs.fx$fx.scaled<-round(subs.fx$prop*subs.fx$study.pop.n)
    
table.data<- subs.fx
table.data$fx.scaled<-round(table.data$prop*table.data$study.pop.n)
table.data$fx.scaled_prop<-paste0(nice.num.count(table.data$fx.scaled), " (",
                                  nice.num(table.data$prop*100) ,
                                  "%)")

#wide  
table.data<-table.data %>% 
  select(intervention , sex, study.pop.n, fx, fx.scaled_prop) %>%
  pivot_wider(names_from = fx, values_from = fx.scaled_prop)
  
table.data<-table.data %>% 
  mutate(sex=factor(sex, levels = c("Male", "Female" ))) %>% 
  arrange(sex)

 kable(table.data %>% 
    arrange(sex) %>% 
    mutate(study.pop.n=nice.num.count(as.numeric(study.pop.n))),
      col.names = c("Intervention",
                    "Sex",
                    "N target population",
                    "0",
                    "1",
                    "2",
                    "3+")) %>%
  add_header_above(c("","",
                    "",
                    "Subsequent fractures"=4
                    ))%>%
  kable_styling(bootstrap_options = c("striped", "bordered"))
  }else{ #"By sentinel fracture and sex"

subs.fx<-m.TR.df %>% 
  filter(month==sec.frac.time) %>% 
  group_by(id,intervention, index_fx, sex) %>% 
  mutate(subs.fx=h_af-1) %>%  # minus index fx
  select(id,intervention, index_fx, sex, subs.fx)

subs.fx<-subs.fx %>%
  group_by(intervention, index_fx, sex) %>% 
  summarise(fx.0=sum(subs.fx==0)/length(id),
            fx.1=sum(subs.fx==1)/length(id),
            fx.2=sum(subs.fx==2)/length(id),
            fx.3plus=sum(subs.fx>=3)/length(id))

subs.fx<-subs.fx %>%
 pivot_longer(-c(intervention, index_fx, sex),
              names_to = "fx", values_to = "prop")
sum(subs.fx$prop)

subs.fx$study.pop.n<-NA
subs.fx<-subs.fx %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
subs.fx<-subs.fx %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
subs.fx<-subs.fx %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

subs.fx<-subs.fx %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
subs.fx<-subs.fx %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
subs.fx<-subs.fx %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted number for study population
subs.fx$fx.scaled<-round(subs.fx$prop*subs.fx$study.pop.n)
    
table.data<- subs.fx
table.data$fx.scaled<-round(table.data$prop*table.data$study.pop.n)
table.data$fx.scaled_prop<-paste0(nice.num.count(table.data$fx.scaled), " (",
                                  nice.num(table.data$prop*100) ,
                                  "%)")

#wide  
table.data<-table.data %>% 
  select(intervention , index_fx, sex,study.pop.n, fx, fx.scaled_prop) %>%
  pivot_wider(names_from = fx, values_from = fx.scaled_prop)
  
 kable(table.data %>% 
    arrange(index_fx, sex) %>% 
    mutate(study.pop.n=nice.num.count(as.numeric(study.pop.n))),
      col.names = c("Intervention",
                    "Index fracture",
                    "Sex",
                    "N target population",
                    "0",
                    "1",
                    "2",
                    "3+")) %>%
  add_header_above(c("","","",
                    "",
                    "Subsequent fractures"=4
                    ))%>%
  kable_styling(bootstrap_options = c("striped", "bordered"))
}
}

# deaths -----
get.summary.deaths<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){

deaths.time<-input$deaths.time

deaths<-lapply(m.TR,
                 function(df) {
                  deaths <- data.frame(
                   deaths=ifelse(df$s_d[which(df$month=={{deaths.time}})]
                                       ==1, 1,0),
                     id=as.numeric(df$id[1]),
                     intervention=df$intervention[1])
                   deaths})
deaths<-plyr::ldply(deaths,
                      data.frame, .id=NULL) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                            "Current practice")) %>%
  mutate(intervention=factor(intervention,
         levels=c("Current practice", "FLS")))


# add characterstics
deaths<-deaths %>%
  left_join(microsim_pop,
            by=c("id"))

deaths<-deaths %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(sum.deaths=sum(deaths))
deaths$microsim_pop<-n_microsimulation


# add study pop
deaths$study.pop.n<-NA
deaths<-deaths %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
deaths<-deaths %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
deaths<-deaths %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

deaths<-deaths %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
deaths<-deaths %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
deaths<-deaths %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted number for study population
deaths$study.pop.deaths<- round(deaths$sum.deaths*
  (deaths$study.pop.n/deaths$microsim_pop))

deaths<-deaths %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.deaths)


if(input$choose.deaths.summary=="Overall"){
table<- deaths %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             n.fx=sum(study.pop.deaths))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(n.fx=nice.num.count(n.fx),
         difference=nice.num.count(difference))



table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Deaths",
                    "Deaths avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.deaths.summary=="By sentinel fracture"){

table<- deaths %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             n.fx=sum(study.pop.deaths))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(n.fx=nice.num.count(n.fx),
         difference=nice.num.count(difference))



table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Deaths",
                    "Deaths avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.deaths.summary=="By sex"){
table<- deaths %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             n.fx=sum(study.pop.deaths))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(n.fx=nice.num.count(n.fx),
         difference=nice.num.count(difference))



table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Deaths",
                    "Deaths avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- deaths %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             n.fx=sum(study.pop.deaths))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(n.fx=nice.num.count(n.fx),
         difference=nice.num.count(difference))



table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Deaths",
                    "Deaths avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}


#output -----
table
}

# HCRU -----
get.summary.procedures<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

procedures<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(procedures=sum(procedure!="none"))

procedures<- procedures %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


procedures<-procedures %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(procedures=sum(procedures))
procedures$microsim_pop<-n_microsimulation

# add study pop
procedures$study.pop.n<-NA
procedures<-procedures %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
procedures<-procedures %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
procedures<-procedures %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

procedures<-procedures %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
procedures<-procedures %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
procedures<-procedures %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
procedures$study.pop.procedures<- round(procedures$procedures*
  (procedures$study.pop.n/procedures$microsim_pop))

procedures<-procedures %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.procedures)
if(input$choose.hcru.summary=="Overall"){
table<- procedures %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.procedures=sum(study.pop.procedures))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(study.pop.procedures=nice.num.count(study.pop.procedures),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Surgeries",
                    "Surgeries avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- procedures %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.procedures=sum(study.pop.procedures))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(study.pop.procedures=nice.num.count(study.pop.procedures),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)
table$index_fx<-str_to_sentence(table$index_fx)

table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Surgeries",
                    "Surgeries avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- procedures %>%
  group_by(sex, intervention) %>%
  summarise(study.pop.n=sum(study.pop.n),
            study.pop.procedures=sum(study.pop.procedures))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(study.pop.procedures=nice.num.count(study.pop.procedures),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Surgeries",
                    "Surgeries avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- procedures %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.procedures=sum(study.pop.procedures))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(study.pop.procedures=nice.num.count(study.pop.procedures),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Surgeries",
                    "Surgeries avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}


get.summary.hosp.los<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){

hcru.time<-input$hcru.time

hosp.los<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(hosp.los=sum(hosp.los))

hosp.los<- hosp.los %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


hosp.los<-hosp.los %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(hosp.los=sum(hosp.los))
hosp.los$microsim_pop<-n_microsimulation

# add study pop
hosp.los$study.pop.n<-NA
hosp.los<-hosp.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
hosp.los<-hosp.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
hosp.los<-hosp.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

hosp.los<-hosp.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
hosp.los<-hosp.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
hosp.los<-hosp.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
hosp.los$study.pop.hosp.los<- round(hosp.los$hosp.los*
  (hosp.los$study.pop.n/hosp.los$microsim_pop))

hosp.los<-hosp.los %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.hosp.los)

if(input$choose.hcru.summary=="Overall"){
table<- hosp.los %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosp.los=sum(study.pop.hosp.los))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(hosp.los=nice.num.count(hosp.los),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Hospital bed days",
                    "Hospital bed days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- hosp.los %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosp.los=sum(study.pop.hosp.los))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(hosp.los=nice.num.count(hosp.los),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Hospital bed days",
                    "Hospital bed days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- hosp.los %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosp.los=sum(study.pop.hosp.los))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(hosp.los=nice.num.count(hosp.los),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Hospital bed days",
                    "Hospital bed days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- hosp.los %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosp.los=sum(study.pop.hosp.los))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(hosp.los=nice.num.count(hosp.los),
         difference=nice.num.count(difference))

table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Hospital bed days",
                    "Hospital bed days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.temp.rehab.los<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){

hcru.time<-input$hcru.time

temp.rehab.los<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(temp.rehab.los=sum(temp.rehab.los))

temp.rehab.los<- temp.rehab.los %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


temp.rehab.los<-temp.rehab.los %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(temp.rehab.los=sum(temp.rehab.los))
temp.rehab.los$microsim_pop<-n_microsimulation

# add study pop
temp.rehab.los$study.pop.n<-NA
temp.rehab.los<-temp.rehab.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
temp.rehab.los<-temp.rehab.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
temp.rehab.los<-temp.rehab.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

temp.rehab.los<-temp.rehab.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
temp.rehab.los<-temp.rehab.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
temp.rehab.los<-temp.rehab.los %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
temp.rehab.los$study.pop.temp.rehab.los<- round(temp.rehab.los$temp.rehab.los*
  (temp.rehab.los$study.pop.n/temp.rehab.los$microsim_pop))

temp.rehab.los<-temp.rehab.los %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.temp.rehab.los)

if(input$choose.hcru.summary=="Overall"){
table<- temp.rehab.los %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             temp.rehab.los=sum(study.pop.temp.rehab.los))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(temp.rehab.los=nice.num.count(temp.rehab.los),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Temporary rehabilitation days",
                    "Temporary rehabilitation days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- temp.rehab.los %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             temp.rehab.los=sum(study.pop.temp.rehab.los))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(temp.rehab.los=nice.num.count(temp.rehab.los),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Temporary rehabilitation days",
                    "Temporary rehabilitation days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- temp.rehab.los %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             temp.rehab.los=sum(study.pop.temp.rehab.los))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(temp.rehab.los=nice.num.count(temp.rehab.los),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Temporary rehabilitation days",
                    "Temporary rehabilitation days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- temp.rehab.los %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             temp.rehab.los=sum(study.pop.temp.rehab.los))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(temp.rehab.los=nice.num.count(temp.rehab.los),
         difference=nice.num.count(difference))

table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Temporary rehabilitation days",
                    "Temporary rehabilitation days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.comm.visits<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

comm.visits<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(comm.visits=sum(comm.visits.count))

comm.visits<- comm.visits %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


comm.visits<-comm.visits %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(comm.visits=sum(comm.visits))
comm.visits$microsim_pop<-n_microsimulation

# add study pop
comm.visits$study.pop.n<-NA
comm.visits<-comm.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
comm.visits<-comm.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
comm.visits<-comm.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

comm.visits<-comm.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
comm.visits<-comm.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
comm.visits<-comm.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted count for study population
comm.visits$study.pop.comm.visits<- round(comm.visits$comm.visits*
  (comm.visits$study.pop.n/comm.visits$microsim_pop))

comm.visits<-comm.visits %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.comm.visits)
if(input$choose.hcru.summary=="Overall"){
table<- comm.visits %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.comm.visits=sum(study.pop.comm.visits))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(study.pop.comm.visits=nice.num.count(study.pop.comm.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Community care consultations (spine fractures only)",
                    "Consultations avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- comm.visits %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.comm.visits=sum(study.pop.comm.visits))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(study.pop.comm.visits=nice.num.count(study.pop.comm.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)
table$index_fx<-str_to_sentence(table$index_fx)

table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Community care consultations (spine fractures only)",
                    "Consultations avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- comm.visits %>%
  group_by(sex, intervention) %>%
  summarise(study.pop.n=sum(study.pop.n),
            study.pop.comm.visits=sum(study.pop.comm.visits))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(study.pop.comm.visits=nice.num.count(study.pop.comm.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Community care consultations (spine fractures only)",
                    "Consultations avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- comm.visits %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.comm.visits=sum(study.pop.comm.visits))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(study.pop.comm.visits=nice.num.count(study.pop.comm.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Community care consultations (spine fractures only)",
                    "Consultations avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.lab.test<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

lab.test<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(lab.test=sum(lab.test==2))

lab.test<- lab.test %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


lab.test<-lab.test %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(lab.test=sum(lab.test))
lab.test$microsim_pop<-n_microsimulation

# add study pop
lab.test$study.pop.n<-NA
lab.test<-lab.test %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
lab.test<-lab.test %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
lab.test<-lab.test %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

lab.test<-lab.test %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
lab.test<-lab.test %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
lab.test<-lab.test %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted count for study population
lab.test$study.pop.lab.test<- round(lab.test$lab.test*
  (lab.test$study.pop.n/lab.test$microsim_pop))

lab.test<-lab.test %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.lab.test)
if(input$choose.hcru.summary=="Overall"){
table<- lab.test %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.lab.test=sum(study.pop.lab.test))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(study.pop.lab.test=nice.num.count(study.pop.lab.test),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Lab tests",
                    "Lab tests avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- lab.test %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.lab.test=sum(study.pop.lab.test))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(study.pop.lab.test=nice.num.count(study.pop.lab.test),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)
table$index_fx<-str_to_sentence(table$index_fx)

table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Lab tests",
                    "Lab tests avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- lab.test %>%
  group_by(sex, intervention) %>%
  summarise(study.pop.n=sum(study.pop.n),
            study.pop.lab.test=sum(study.pop.lab.test))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(study.pop.lab.test=nice.num.count(study.pop.lab.test),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Lab tests",
                    "Lab tests avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- lab.test %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.lab.test=sum(study.pop.lab.test))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(study.pop.lab.test=nice.num.count(study.pop.lab.test),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Lab tests",
                    "Lab tests avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.dxa<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

dxa<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(dxa=sum(dxa==2))

dxa<- dxa %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


dxa<-dxa %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(dxa=sum(dxa))
dxa$microsim_pop<-n_microsimulation

# add study pop
dxa$study.pop.n<-NA
dxa<-dxa %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
dxa<-dxa %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
dxa<-dxa %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

dxa<-dxa %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
dxa<-dxa %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
dxa<-dxa %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted count for study population
dxa$study.pop.dxa<- round(dxa$dxa*
  (dxa$study.pop.n/dxa$microsim_pop))

dxa<-dxa %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.dxa)
if(input$choose.hcru.summary=="Overall"){
table<- dxa %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.dxa=sum(study.pop.dxa))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(study.pop.dxa=nice.num.count(study.pop.dxa),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "DXA",
                    "DXA avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- dxa %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.dxa=sum(study.pop.dxa))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(study.pop.dxa=nice.num.count(study.pop.dxa),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)
table$index_fx<-str_to_sentence(table$index_fx)

table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "DXA",
                    "DXA avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- dxa %>%
  group_by(sex, intervention) %>%
  summarise(study.pop.n=sum(study.pop.n),
            study.pop.dxa=sum(study.pop.dxa))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(study.pop.dxa=nice.num.count(study.pop.dxa),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "DXA",
                    "DXA avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- dxa %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.dxa=sum(study.pop.dxa))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(study.pop.dxa=nice.num.count(study.pop.dxa),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "DXA",
                    "DXA avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.clinic.visits<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

clinic.visits<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(clinic.visits=sum(clinic.visits.count))

clinic.visits<- clinic.visits %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


clinic.visits<-clinic.visits %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(clinic.visits=sum(clinic.visits))
clinic.visits$microsim_pop<-n_microsimulation

# add study pop
clinic.visits$study.pop.n<-NA
clinic.visits<-clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
clinic.visits<-clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
clinic.visits<-clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

clinic.visits<-clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
clinic.visits<-clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
clinic.visits<-clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted count for study population
clinic.visits$study.pop.clinic.visits<- round(clinic.visits$clinic.visits*
  (clinic.visits$study.pop.n/clinic.visits$microsim_pop))

clinic.visits<-clinic.visits %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.clinic.visits)
if(input$choose.hcru.summary=="Overall"){
table<- clinic.visits %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.clinic.visits=sum(study.pop.clinic.visits))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(study.pop.clinic.visits=nice.num.count(study.pop.clinic.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Clinic visits",
                    "Clinic visits avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- clinic.visits %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.clinic.visits=sum(study.pop.clinic.visits))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(study.pop.clinic.visits=nice.num.count(study.pop.clinic.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)
table$index_fx<-str_to_sentence(table$index_fx)

table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Clinic visits",
                    "Clinic visits avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- clinic.visits %>%
  group_by(sex, intervention) %>%
  summarise(study.pop.n=sum(study.pop.n),
            study.pop.clinic.visits=sum(study.pop.clinic.visits))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(study.pop.clinic.visits=nice.num.count(study.pop.clinic.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Clinic visits",
                    "Clinic visits avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- clinic.visits %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.clinic.visits=sum(study.pop.clinic.visits))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(study.pop.clinic.visits=nice.num.count(study.pop.clinic.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Clinic visits",
                    "Clinic visits avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.discharge.clinic.visits<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

discharge.clinic.visits<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(discharge.clinic.visits=sum(discharge.clinic.visits))

discharge.clinic.visits<- discharge.clinic.visits %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


discharge.clinic.visits<-discharge.clinic.visits %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(discharge.clinic.visits=sum(discharge.clinic.visits))
discharge.clinic.visits$microsim_pop<-n_microsimulation

# add study pop
discharge.clinic.visits$study.pop.n<-NA
discharge.clinic.visits<-discharge.clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
discharge.clinic.visits<-discharge.clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
discharge.clinic.visits<-discharge.clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

discharge.clinic.visits<-discharge.clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
discharge.clinic.visits<-discharge.clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
discharge.clinic.visits<-discharge.clinic.visits %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted count for study population
discharge.clinic.visits$study.pop.discharge.clinic.visits<- round(discharge.clinic.visits$discharge.clinic.visits*
  (discharge.clinic.visits$study.pop.n/discharge.clinic.visits$microsim_pop))

discharge.clinic.visits<-discharge.clinic.visits %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.discharge.clinic.visits)
if(input$choose.hcru.summary=="Overall"){
table<- discharge.clinic.visits %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.discharge.clinic.visits=sum(study.pop.discharge.clinic.visits))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(study.pop.discharge.clinic.visits=nice.num.count(study.pop.discharge.clinic.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Discharge clinic visits",
                    "Discharge clinic visits avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- discharge.clinic.visits %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.discharge.clinic.visits=sum(study.pop.discharge.clinic.visits))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(study.pop.discharge.clinic.visits=nice.num.count(study.pop.discharge.clinic.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)
table$index_fx<-str_to_sentence(table$index_fx)

table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Discharge clinic visits",
                    "Discharge clinic visits avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- discharge.clinic.visits %>%
  group_by(sex, intervention) %>%
  summarise(study.pop.n=sum(study.pop.n),
            study.pop.discharge.clinic.visits=sum(study.pop.discharge.clinic.visits))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(study.pop.discharge.clinic.visits=nice.num.count(study.pop.discharge.clinic.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Discharge clinic visits",
                    "Discharge clinic visits avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- discharge.clinic.visits %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             study.pop.discharge.clinic.visits=sum(study.pop.discharge.clinic.visits))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(study.pop.discharge.clinic.visits=nice.num.count(study.pop.discharge.clinic.visits),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Discharge clinic visits",
                    "Discharge clinic visits avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.doctor.mins<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){

hcru.time<-input$hcru.time

doctor.mins<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(doctor.mins=sum(doctor.mins))

doctor.mins<- doctor.mins %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


doctor.mins<-doctor.mins %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(doctor.mins=sum(doctor.mins))
doctor.mins$microsim_pop<-n_microsimulation

# add study pop
doctor.mins$study.pop.n<-NA
doctor.mins<-doctor.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
doctor.mins<-doctor.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
doctor.mins<-doctor.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

doctor.mins<-doctor.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
doctor.mins<-doctor.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
doctor.mins<-doctor.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
doctor.mins$study.pop.doctor.mins<- round(doctor.mins$doctor.mins*
  (doctor.mins$study.pop.n/doctor.mins$microsim_pop))

doctor.mins<-doctor.mins %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.doctor.mins)

if(input$choose.hcru.summary=="Overall"){
table<- doctor.mins %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             doctor.mins=sum(study.pop.doctor.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(doctor.mins=nice.num.count(doctor.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Doctor minutes",
                    "Doctor minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- doctor.mins %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             doctor.mins=sum(study.pop.doctor.mins))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(doctor.mins=nice.num.count(doctor.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Doctor minutes",
                    "Doctor minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- doctor.mins %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             doctor.mins=sum(study.pop.doctor.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(doctor.mins=nice.num.count(doctor.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Doctor minutes",
                    "Doctor minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- doctor.mins %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             doctor.mins=sum(study.pop.doctor.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(doctor.mins=nice.num.count(doctor.mins),
         difference=nice.num.count(difference))

table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Doctor minutes",
                    "Doctor minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.administrator.mins<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){

hcru.time<-input$hcru.time

administrator.mins<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(administrator.mins=sum(administrator.mins))

administrator.mins<- administrator.mins %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


administrator.mins<-administrator.mins %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(administrator.mins=sum(administrator.mins))
administrator.mins$microsim_pop<-n_microsimulation

# add study pop
administrator.mins$study.pop.n<-NA
administrator.mins<-administrator.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
administrator.mins<-administrator.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
administrator.mins<-administrator.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

administrator.mins<-administrator.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
administrator.mins<-administrator.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
administrator.mins<-administrator.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
administrator.mins$study.pop.administrator.mins<- round(administrator.mins$administrator.mins*
  (administrator.mins$study.pop.n/administrator.mins$microsim_pop))

administrator.mins<-administrator.mins %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.administrator.mins)

if(input$choose.hcru.summary=="Overall"){
table<- administrator.mins %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             administrator.mins=sum(study.pop.administrator.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(administrator.mins=nice.num.count(administrator.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "administrator minutes",
                    "administrator minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- administrator.mins %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             administrator.mins=sum(study.pop.administrator.mins))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(administrator.mins=nice.num.count(administrator.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "administrator minutes",
                    "administrator minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- administrator.mins %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             administrator.mins=sum(study.pop.administrator.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(administrator.mins=nice.num.count(administrator.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "administrator minutes",
                    "administrator minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- administrator.mins %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             administrator.mins=sum(study.pop.administrator.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(administrator.mins=nice.num.count(administrator.mins),
         difference=nice.num.count(difference))

table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "administrator minutes",
                    "administrator minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.nurse.mins<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){

hcru.time<-input$hcru.time

nurse.mins<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(nurse.mins=sum(nurse.mins))

nurse.mins<- nurse.mins %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


nurse.mins<-nurse.mins %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(nurse.mins=sum(nurse.mins))
nurse.mins$microsim_pop<-n_microsimulation

# add study pop
nurse.mins$study.pop.n<-NA
nurse.mins<-nurse.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
nurse.mins<-nurse.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
nurse.mins<-nurse.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

nurse.mins<-nurse.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
nurse.mins<-nurse.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
nurse.mins<-nurse.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
nurse.mins$study.pop.nurse.mins<- round(nurse.mins$nurse.mins*
  (nurse.mins$study.pop.n/nurse.mins$microsim_pop))

nurse.mins<-nurse.mins %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.nurse.mins)

if(input$choose.hcru.summary=="Overall"){
table<- nurse.mins %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             nurse.mins=sum(study.pop.nurse.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(nurse.mins=nice.num.count(nurse.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "nurse minutes",
                    "nurse minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- nurse.mins %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             nurse.mins=sum(study.pop.nurse.mins))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(nurse.mins=nice.num.count(nurse.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "nurse minutes",
                    "nurse minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- nurse.mins %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             nurse.mins=sum(study.pop.nurse.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(nurse.mins=nice.num.count(nurse.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "nurse minutes",
                    "nurse minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- nurse.mins %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             nurse.mins=sum(study.pop.nurse.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(nurse.mins=nice.num.count(nurse.mins),
         difference=nice.num.count(difference))

table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "nurse minutes",
                    "nurse minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}


get.summary.radiographer.mins<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){

hcru.time<-input$hcru.time

radiographer.mins<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(radiographer.mins=sum(radiographer.mins))

radiographer.mins<- radiographer.mins %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


radiographer.mins<-radiographer.mins %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(radiographer.mins=sum(radiographer.mins))
radiographer.mins$microsim_pop<-n_microsimulation

# add study pop
radiographer.mins$study.pop.n<-NA
radiographer.mins<-radiographer.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
radiographer.mins<-radiographer.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
radiographer.mins<-radiographer.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

radiographer.mins<-radiographer.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
radiographer.mins<-radiographer.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
radiographer.mins<-radiographer.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
radiographer.mins$study.pop.radiographer.mins<- round(radiographer.mins$radiographer.mins*
  (radiographer.mins$study.pop.n/radiographer.mins$microsim_pop))

radiographer.mins<-radiographer.mins %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.radiographer.mins)

if(input$choose.hcru.summary=="Overall"){
table<- radiographer.mins %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             radiographer.mins=sum(study.pop.radiographer.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(radiographer.mins=nice.num.count(radiographer.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "radiographer minutes",
                    "radiographer minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- radiographer.mins %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             radiographer.mins=sum(study.pop.radiographer.mins))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(radiographer.mins=nice.num.count(radiographer.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "radiographer minutes",
                    "radiographer minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- radiographer.mins %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             radiographer.mins=sum(study.pop.radiographer.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(radiographer.mins=nice.num.count(radiographer.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "radiographer minutes",
                    "radiographer minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- radiographer.mins %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             radiographer.mins=sum(study.pop.radiographer.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(radiographer.mins=nice.num.count(radiographer.mins),
         difference=nice.num.count(difference))

table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "radiographer minutes",
                    "radiographer minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}


get.summary.allied_health.mins<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){

hcru.time<-input$hcru.time

allied_health.mins<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(allied_health.mins=sum(allied_health.mins))

allied_health.mins<- allied_health.mins %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


allied_health.mins<-allied_health.mins %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(allied_health.mins=sum(allied_health.mins))
allied_health.mins$microsim_pop<-n_microsimulation

# add study pop
allied_health.mins$study.pop.n<-NA
allied_health.mins<-allied_health.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
allied_health.mins<-allied_health.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
allied_health.mins<-allied_health.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

allied_health.mins<-allied_health.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
allied_health.mins<-allied_health.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
allied_health.mins<-allied_health.mins %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
allied_health.mins$study.pop.allied_health.mins<- round(allied_health.mins$allied_health.mins*
  (allied_health.mins$study.pop.n/allied_health.mins$microsim_pop))

allied_health.mins<-allied_health.mins %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.allied_health.mins)

if(input$choose.hcru.summary=="Overall"){
table<- allied_health.mins %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             allied_health.mins=sum(study.pop.allied_health.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(allied_health.mins=nice.num.count(allied_health.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "allied_health minutes",
                    "allied_health minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- allied_health.mins %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             allied_health.mins=sum(study.pop.allied_health.mins))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(allied_health.mins=nice.num.count(allied_health.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "allied_health minutes",
                    "allied_health minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- allied_health.mins %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             allied_health.mins=sum(study.pop.allied_health.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(allied_health.mins=nice.num.count(allied_health.mins),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "allied_health minutes",
                    "allied_health minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- allied_health.mins %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             allied_health.mins=sum(study.pop.allied_health.mins))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(allied_health.mins=nice.num.count(allied_health.mins),
         difference=nice.num.count(difference))

table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "allied_health minutes",
                    "allied_health minutes avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.other.mins<-function(m.TR.df,
                                  microsim_pop,
                                  study_pop_n,
                                  input){
  
  hcru.time<-input$hcru.time
  
  other.mins<-m.TR.df %>%
    filter(month<={{hcru.time}}) %>%
    group_by(id, index_fx, sex, intervention) %>%
    summarise(other.mins=sum(other.mins))
  
  other.mins<- other.mins %>%
    mutate(intervention=ifelse(intervention=="FLS", "FLS",
                               "Current practice")) %>%
    mutate(intervention=factor(intervention,
                               levels=c("Current practice", "FLS")))
  
  
  other.mins<-other.mins %>%
    group_by(intervention, index_fx, sex) %>%
    summarise(other.mins=sum(other.mins))
  other.mins$microsim_pop<-n_microsimulation
  
  # add study pop
  other.mins$study.pop.n<-NA
  other.mins<-other.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.male") %>%
                                           select(n)), study.pop.n))
  other.mins<-other.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.male") %>%
                                           select(n)), study.pop.n))
  other.mins<-other.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.male") %>%
                                           select(n)), study.pop.n))
  
  other.mins<-other.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.female") %>%
                                           select(n)), study.pop.n))
  other.mins<-other.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.female") %>%
                                           select(n)), study.pop.n))
  other.mins<-other.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.female") %>%
                                           select(n)), study.pop.n))
  
  # get weighted cost for study population
  other.mins$study.pop.other.mins<- round(other.mins$other.mins*
                                              (other.mins$study.pop.n/other.mins$microsim_pop))
  
  other.mins<-other.mins %>%
    select(intervention, index_fx, sex, study.pop.n, study.pop.other.mins)
  
  if(input$choose.hcru.summary=="Overall"){
    table<- other.mins %>%
      group_by(intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                other.mins=sum(study.pop.other.mins))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,3]- table[2,3])
    
    table<-table %>%
      mutate(other.mins=nice.num.count(other.mins),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Intervention",
                                "N target population",
                                "other minutes",
                                "other minutes avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.hcru.summary=="By sentinel fracture"){
    
    table<- other.mins %>%
      group_by(index_fx,intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                other.mins=sum(study.pop.other.mins))
    
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,4]- table[2,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,4]- table[4,4])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[5,4]- table[6,4])
    }
    
    table<-table %>%
      mutate(other.mins=nice.num.count(other.mins),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Intervention",
                                "N target population",
                                "other minutes",
                                "other minutes avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.hcru.summary=="By sex"){
    table<- other.mins %>%
      group_by(sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                other.mins=sum(study.pop.other.mins))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,4]- table[2,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,4]- table[4,4])
    }
    
    table<-table %>%
      mutate(other.mins=nice.num.count(other.mins),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Sex",
                                "Intervention",
                                "N target population",
                                "other minutes",
                                "other minutes avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else{ #"By sentinel fracture and sex"
    
    table<- other.mins %>%
      group_by(index_fx, sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                other.mins=sum(study.pop.other.mins))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,5]- table[2,5])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,5]- table[4,5])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[5,5]- table[6,5])
    }
    
    if(nrow(table)>7){
      table$difference[8]<-as.numeric(table[7,5]- table[8,5])
    }
    
    if(nrow(table)>9){
      table$difference[10]<-as.numeric(table[9,5]- table[10,5])
    }
    
    if(nrow(table)>11){
      table$difference[12]<-as.numeric(table[11,5]- table[12,5])
    }
    
    table<-table %>%
      mutate(other.mins=nice.num.count(other.mins),
             difference=nice.num.count(difference))
    
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Sex",
                                "Intervention",
                                "N target population",
                                "other minutes",
                                "other minutes avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }
  #output -----
  table
}

get.summary.fls_coordinator.mins<-function(m.TR.df,
                                         microsim_pop,
                                         study_pop_n,
                                         input){
  
  hcru.time<-input$hcru.time
  
  fls_coordinator.mins<-m.TR.df %>%
    filter(month<={{hcru.time}}) %>%
    group_by(id, index_fx, sex, intervention) %>%
    summarise(fls_coordinator.mins=sum(fls_coordinator.mins))
  
  fls_coordinator.mins<- fls_coordinator.mins %>%
    mutate(intervention=ifelse(intervention=="FLS", "FLS",
                               "Current practice")) %>%
    mutate(intervention=factor(intervention,
                               levels=c("Current practice", "FLS")))
  
  
  fls_coordinator.mins<-fls_coordinator.mins %>%
    group_by(intervention, index_fx, sex) %>%
    summarise(fls_coordinator.mins=sum(fls_coordinator.mins))
  fls_coordinator.mins$microsim_pop<-n_microsimulation
  
  # add study pop
  fls_coordinator.mins$study.pop.n<-NA
  fls_coordinator.mins<-fls_coordinator.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.male") %>%
                                           select(n)), study.pop.n))
  fls_coordinator.mins<-fls_coordinator.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.male") %>%
                                           select(n)), study.pop.n))
  fls_coordinator.mins<-fls_coordinator.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.male") %>%
                                           select(n)), study.pop.n))
  
  fls_coordinator.mins<-fls_coordinator.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.female") %>%
                                           select(n)), study.pop.n))
  fls_coordinator.mins<-fls_coordinator.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.female") %>%
                                           select(n)), study.pop.n))
  fls_coordinator.mins<-fls_coordinator.mins %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.female") %>%
                                           select(n)), study.pop.n))
  
  # get weighted cost for study population
  fls_coordinator.mins$study.pop.fls_coordinator.mins<- round(fls_coordinator.mins$fls_coordinator.mins*
                                                            (fls_coordinator.mins$study.pop.n/fls_coordinator.mins$microsim_pop))
  
  fls_coordinator.mins<-fls_coordinator.mins %>%
    select(intervention, index_fx, sex, study.pop.n, study.pop.fls_coordinator.mins)
  
  if(input$choose.hcru.summary=="Overall"){
    table<- fls_coordinator.mins %>%
      group_by(intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                fls_coordinator.mins=sum(study.pop.fls_coordinator.mins))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,3]- table[2,3])
    
    table<-table %>%
      mutate(fls_coordinator.mins=nice.num.count(fls_coordinator.mins),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Intervention",
                                "N target population",
                                "fls_coordinator minutes",
                                "fls_coordinator minutes avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.hcru.summary=="By sentinel fracture"){
    
    table<- fls_coordinator.mins %>%
      group_by(index_fx,intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                fls_coordinator.mins=sum(study.pop.fls_coordinator.mins))
    
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,4]- table[2,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,4]- table[4,4])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[5,4]- table[6,4])
    }
    
    table<-table %>%
      mutate(fls_coordinator.mins=nice.num.count(fls_coordinator.mins),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Intervention",
                                "N target population",
                                "fls_coordinator minutes",
                                "fls_coordinator minutes avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.hcru.summary=="By sex"){
    table<- fls_coordinator.mins %>%
      group_by(sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                fls_coordinator.mins=sum(study.pop.fls_coordinator.mins))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,4]- table[2,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,4]- table[4,4])
    }
    
    table<-table %>%
      mutate(fls_coordinator.mins=nice.num.count(fls_coordinator.mins),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Sex",
                                "Intervention",
                                "N target population",
                                "fls_coordinator minutes",
                                "fls_coordinator minutes avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else{ #"By sentinel fracture and sex"
    
    table<- fls_coordinator.mins %>%
      group_by(index_fx, sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                fls_coordinator.mins=sum(study.pop.fls_coordinator.mins))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,5]- table[2,5])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,5]- table[4,5])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[5,5]- table[6,5])
    }
    
    if(nrow(table)>7){
      table$difference[8]<-as.numeric(table[7,5]- table[8,5])
    }
    
    if(nrow(table)>9){
      table$difference[10]<-as.numeric(table[9,5]- table[10,5])
    }
    
    if(nrow(table)>11){
      table$difference[12]<-as.numeric(table[11,5]- table[12,5])
    }
    
    table<-table %>%
      mutate(fls_coordinator.mins=nice.num.count(fls_coordinator.mins),
             difference=nice.num.count(difference))
    
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Sex",
                                "Intervention",
                                "N target population",
                                "fls_coordinator minutes",
                                "fls_coordinator minutes avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }
  #output -----
  table
}


# costs -----
get.summary.total.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){

costs.time<-input$costs.time

costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(total.cost=sum(total.cost)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(total.cost=sum(total.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.total.cost<- round(costs$total.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.total.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             total.cost=sum(study.pop.total.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(total.cost=nice.num.count(total.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Total costs",
                    "Total costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             total.cost=sum(study.pop.total.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(total.cost=nice.num.count(total.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Total costs",
                    "Total costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             total.cost=sum(study.pop.total.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(total.cost=nice.num.count(total.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Total costs",
                    "Total costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             total.cost=sum(study.pop.total.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(total.cost=nice.num.count(total.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Total costs",
                    "Total costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.total.cost1<-function(m.TR.df,
                                 microsim_pop,
                                 study_pop_n,
                                 input){
  
  min.costs.time<-input$min.costs.time
  costs.time<-input$costs.time
  
  costs<-m.TR.df %>%
    filter(month>{{min.costs.time}}) %>%
    filter(month<={{costs.time}}) %>%
    group_by(id, index_fx, sex, intervention) %>%
    summarise(total.cost=sum(total.cost)) %>%
    mutate(intervention=ifelse(intervention=="FLS", "FLS",
                               "Current practice")) %>%
    mutate(intervention=factor(intervention,
                               levels=c("Current practice", "FLS")))
  
  costs<-costs %>%
    group_by(intervention, index_fx, sex) %>%
    summarise(total.cost=sum(total.cost))
  costs$microsim_pop<-n_microsimulation
  
  # add study pop
  costs$study.pop.n<-NA
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.male") %>%
                                           select(n)), study.pop.n))
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.male") %>%
                                           select(n)), study.pop.n))
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.male") %>%
                                           select(n)), study.pop.n))
  
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.female") %>%
                                           select(n)), study.pop.n))
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.female") %>%
                                           select(n)), study.pop.n))
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.female") %>%
                                           select(n)), study.pop.n))
  
  # get weighted cost for study population
  costs$study.pop.total.cost<- round(costs$total.cost*
                                       (costs$study.pop.n/costs$microsim_pop))
  
  costs<-costs %>%
    select(intervention, index_fx, sex, study.pop.n, study.pop.total.cost)
  
  if(input$choose.costs.summary=="Overall"){
    table<- costs %>%
      group_by(intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                total.cost=sum(study.pop.total.cost))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,3]- table[2,3])
    
    table<-table %>%
      mutate(total.cost=nice.num.count(total.cost),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Intervention",
                                "N target population",
                                "Total costs",
                                "Total costs avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.costs.summary=="By sentinel fracture"){
    
    table<- costs %>%
      group_by(index_fx,intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                total.cost=sum(study.pop.total.cost))
    
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,4]- table[2,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,4]- table[4,4])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[5,4]- table[6,4])
    }
    
    table<-table %>%
      mutate(total.cost=nice.num.count(total.cost),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Intervention",
                                "N target population",
                                "Total costs",
                                "Total costs avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.costs.summary=="By sex"){
    table<- costs %>%
      group_by(sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                total.cost=sum(study.pop.total.cost))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,4]- table[2,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,4]- table[4,4])
    }
    
    table<-table %>%
      mutate(total.cost=nice.num.count(total.cost),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Sex",
                                "Intervention",
                                "N target population",
                                "Total costs",
                                "Total costs avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else{ #"By sentinel fracture and sex"
    
    table<- costs %>%
      group_by(index_fx, sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                total.cost=sum(study.pop.total.cost))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,5]- table[2,5])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,5]- table[4,5])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[5,5]- table[6,5])
    }
    
    if(nrow(table)>7){
      table$difference[8]<-as.numeric(table[7,5]- table[8,5])
    }
    
    if(nrow(table)>9){
      table$difference[10]<-as.numeric(table[9,5]- table[10,5])
    }
    
    if(nrow(table)>11){
      table$difference[12]<-as.numeric(table[11,5]- table[12,5])
    }
    
    table<-table %>%
      mutate(total.cost=nice.num.count(total.cost),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Sex",
                                "Intervention",
                                "N target population",
                                "Total costs",
                                "Total costs avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }
  #output -----
  table
}

get.summary.discounted.total.cost<-function(m.TR.df,
                                 microsim_pop,
                                 study_pop_n,
                                 input){
  
  costs.time<-input$costs.time
  
  costs<-m.TR.df %>%
    filter(month<={{costs.time}}) %>%
    group_by(id, index_fx, sex, intervention) %>%
    summarise(discounted.total.cost=sum(discounted.total.cost)) %>%
    mutate(intervention=ifelse(intervention=="FLS", "FLS",
                               "Current practice")) %>%
    mutate(intervention=factor(intervention,
                               levels=c("Current practice", "FLS")))
  
  costs<-costs %>%
    group_by(intervention, index_fx, sex) %>%
    summarise(discounted.total.cost=sum(discounted.total.cost))
  costs$microsim_pop<-n_microsimulation
  
  # add study pop
  costs$study.pop.n<-NA
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.male") %>%
                                           select(n)), study.pop.n))
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.male") %>%
                                           select(n)), study.pop.n))
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.male") %>%
                                           select(n)), study.pop.n))
  
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.female") %>%
                                           select(n)), study.pop.n))
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.female") %>%
                                           select(n)), study.pop.n))
  costs<-costs %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.female") %>%
                                           select(n)), study.pop.n))
  
  # get weighted cost for study population
  costs$study.pop.discounted.total.cost<- round(costs$discounted.total.cost*
                                       (costs$study.pop.n/costs$microsim_pop))
  
  costs<-costs %>%
    select(intervention, index_fx, sex, study.pop.n, study.pop.discounted.total.cost)
  
  if(input$choose.costs.summary=="Overall"){
    table<- costs %>%
      group_by(intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                discounted.total.cost=sum(study.pop.discounted.total.cost))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,3]- table[2,3])
    
    table<-table %>%
      mutate(discounted.total.cost=nice.num.count(discounted.total.cost),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Intervention",
                                "N target population",
                                "Discounted total costs",
                                "Discounted total costs avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.costs.summary=="By sentinel fracture"){
    
    table<- costs %>%
      group_by(index_fx,intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                discounted.total.cost=sum(study.pop.discounted.total.cost))
    
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,4]- table[2,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,4]- table[4,4])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[5,4]- table[6,4])
    }
    
    table<-table %>%
      mutate(discounted.total.cost=nice.num.count(discounted.total.cost),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Intervention",
                                "N target population",
                                "Discounted total costs",
                                "Discounted total costs avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.costs.summary=="By sex"){
    table<- costs %>%
      group_by(sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                discounted.total.cost=sum(study.pop.discounted.total.cost))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,4]- table[2,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,4]- table[4,4])
    }
    
    table<-table %>%
      mutate(discounted.total.cost=nice.num.count(discounted.total.cost),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Sex",
                                "Intervention",
                                "N target population",
                                "Discounted total costs",
                                "Discounted total costs avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else{ #"By sentinel fracture and sex"
    
    table<- costs %>%
      group_by(index_fx, sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                discounted.total.cost=sum(study.pop.discounted.total.cost))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[1,5]- table[2,5])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[3,5]- table[4,5])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[5,5]- table[6,5])
    }
    
    if(nrow(table)>7){
      table$difference[8]<-as.numeric(table[7,5]- table[8,5])
    }
    
    if(nrow(table)>9){
      table$difference[10]<-as.numeric(table[9,5]- table[10,5])
    }
    
    if(nrow(table)>11){
      table$difference[12]<-as.numeric(table[11,5]- table[12,5])
    }
    
    table<-table %>%
      mutate(discounted.total.cost=nice.num.count(discounted.total.cost),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Sex",
                                "Intervention",
                                "N target population",
                                "Discounted total costs",
                                "Discounted total costs avoided")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }
  #output -----
  table
}


get.summary.total.cost.mc.error<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input,
                                working.n_microsimulation){

costs.time<-input$costs.time

costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(total.cost=sum(total.cost)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(total.cost=sum(total.cost))
costs$microsim_pop<-working.n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.total.cost<- round(costs$total.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.total.cost)

 #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             total.cost=sum(study.pop.total.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}





  #output -----
table
}

get.summary.total.cost.excl.location<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time


costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(total.cost.excl.location=sum(total.cost.excl.location)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                            "Current practice")) %>%
  mutate(intervention=factor(intervention,
         levels=c("Current practice", "FLS")))


costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(total.cost.excl.location=sum(total.cost.excl.location))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.total.cost.excl.location<- round(costs$total.cost.excl.location*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.total.cost.excl.location)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             total.cost.excl.location=sum(study.pop.total.cost.excl.location))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(total.cost.excl.location=nice.num.count(total.cost.excl.location),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Total costs (exc. formal care)",
                    "Total costs (exc. formal care) avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             total.cost.excl.location=sum(study.pop.total.cost.excl.location))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(total.cost.excl.location=nice.num.count(total.cost.excl.location),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Total costs (exc. formal care)",
                    "Total costs (exc. formal care) avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             total.cost.excl.location=sum(study.pop.total.cost.excl.location))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(total.cost.excl.location=nice.num.count(total.cost.excl.location),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Total costs (exc. formal care)",
                    "Total costs (exc. formal care) avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             total.cost.excl.location=sum(study.pop.total.cost.excl.location))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(total.cost.excl.location=nice.num.count(total.cost.excl.location),
         difference=nice.num.count(difference))

table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Total costs (exc. formal care)",
                    "Total costs (exc. formal care) avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}




get.summary.hosp.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time

costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(hosp.cost=sum(hosp.cost)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(hosp.cost=sum(hosp.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.hosp.cost<- round(costs$hosp.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.hosp.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosp.cost=sum(study.pop.hosp.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(hosp.cost=nice.num.count(hosp.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Hospital (exc. surgeries) costs",
                    "Hospital (exc. surgeries) costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosp.cost=sum(study.pop.hosp.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(hosp.cost=nice.num.count(hosp.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Hospital (exc. surgeries) costs",
                    "Hospital (exc. surgeries) costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosp.cost=sum(study.pop.hosp.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(hosp.cost=nice.num.count(hosp.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Hospital (exc. surgeries) costs",
                    "Hospital (exc. surgeries) costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosp.cost=sum(study.pop.hosp.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(hosp.cost=nice.num.count(hosp.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Hospital (exc. surgeries) costs",
                    "Hospital (exc. surgeries) costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.procedure.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time

costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(procedure.cost=sum(procedure.cost)) %>%
mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(procedure.cost=sum(procedure.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.procedure.cost<- round(costs$procedure.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.procedure.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             procedure.cost=sum(study.pop.procedure.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(procedure.cost=nice.num.count(procedure.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Procedure costs",
                    "Procedure costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             procedure.cost=sum(study.pop.procedure.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(procedure.cost=nice.num.count(procedure.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Procedure costs",
                    "Procedure costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             procedure.cost=sum(study.pop.procedure.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(procedure.cost=nice.num.count(procedure.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Procedure costs",
                    "Procedure costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             procedure.cost=sum(study.pop.procedure.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(procedure.cost=nice.num.count(procedure.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Procedure costs",
                    "Procedure costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.location_home_support.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time

costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(location_home_support.cost=sum(location_home_support.cost)) %>%
mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(location_home_support.cost=sum(location_home_support.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.location_home_support.cost<- round(costs$location_home_support.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.location_home_support.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             location_home_support.cost=sum(study.pop.location_home_support.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(location_home_support.cost=nice.num.count(location_home_support.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Location costs",
                    "Location costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             location_home_support.cost=sum(study.pop.location_home_support.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(location_home_support.cost=nice.num.count(location_home_support.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Location costs",
                    "Location costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             location_home_support.cost=sum(study.pop.location_home_support.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(location_home_support.cost=nice.num.count(location_home_support.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Location costs",
                    "Location costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             location_home_support.cost=sum(study.pop.location_home_support.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(location_home_support.cost=nice.num.count(location_home_support.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Location costs",
                    "Location costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.location_long_term_care.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time

costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(location_long_term_care.cost=sum(location_long_term_care.cost)) %>%
mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(location_long_term_care.cost=sum(location_long_term_care.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.location_long_term_care.cost<- round(costs$location_long_term_care.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.location_long_term_care.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             location_long_term_care.cost=sum(study.pop.location_long_term_care.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(location_long_term_care.cost=nice.num.count(location_long_term_care.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Location costs",
                    "Location costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             location_long_term_care.cost=sum(study.pop.location_long_term_care.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(location_long_term_care.cost=nice.num.count(location_long_term_care.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Location costs",
                    "Location costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             location_long_term_care.cost=sum(study.pop.location_long_term_care.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(location_long_term_care.cost=nice.num.count(location_long_term_care.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Location costs",
                    "Location costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             location_long_term_care.cost=sum(study.pop.location_long_term_care.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(location_long_term_care.cost=nice.num.count(location_long_term_care.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Location costs",
                    "Location costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.comm.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time


costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(comm.cost=sum(comm.cost)) %>%
mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(comm.cost=sum(comm.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.comm.cost<- round(costs$comm.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.comm.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             comm.cost=sum(study.pop.comm.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(comm.cost=nice.num.count(comm.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Community care costs",
                    "Community care costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             comm.cost=sum(study.pop.comm.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(comm.cost=nice.num.count(comm.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Community care costs",
                    "Community care costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             comm.cost=sum(study.pop.comm.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(comm.cost=nice.num.count(comm.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Community care costs",
                    "Community care costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             comm.cost=sum(study.pop.comm.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(comm.cost=nice.num.count(comm.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Community care costs",
                    "Community care costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.clinic.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time

costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(clinic.cost=sum(clinic.cost)) %>%
mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(clinic.cost=sum(clinic.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.clinic.cost<- round(costs$clinic.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.clinic.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             clinic.cost=sum(study.pop.clinic.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(clinic.cost=nice.num.count(clinic.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Clinic costs",
                    "Clinic costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             clinic.cost=sum(study.pop.clinic.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(clinic.cost=nice.num.count(clinic.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Clinic costs",
                    "Clinic costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             clinic.cost=sum(study.pop.clinic.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(clinic.cost=nice.num.count(clinic.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Clinic costs",
                    "Clinic costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             clinic.cost=sum(study.pop.clinic.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(clinic.cost=nice.num.count(clinic.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Clinic costs",
                    "Clinic costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.temp.rehab.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time

costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(temp.rehab.cost=sum(temp.rehab.cost)) %>%
mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(temp.rehab.cost=sum(temp.rehab.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.temp.rehab.cost<- round(costs$temp.rehab.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.temp.rehab.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             temp.rehab.cost=sum(study.pop.temp.rehab.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(temp.rehab.cost=nice.num.count(temp.rehab.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Temporary rehab costs",
                    "Temporary rehab costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             temp.rehab.cost=sum(study.pop.temp.rehab.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(temp.rehab.cost=nice.num.count(temp.rehab.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Temporary rehab costs",
                    "Temporary rehab costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             temp.rehab.cost=sum(study.pop.temp.rehab.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(temp.rehab.cost=nice.num.count(temp.rehab.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Temporary rehab costs",
                    "Temporary rehab costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             temp.rehab.cost=sum(study.pop.temp.rehab.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(temp.rehab.cost=nice.num.count(temp.rehab.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Temporary rehab costs",
                    "Temporary rehab costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.discharge.clinic.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time

costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(discharge.clinic.cost=sum(discharge.clinic.cost)) %>%
mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(discharge.clinic.cost=sum(discharge.clinic.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.discharge.clinic.cost<- round(costs$discharge.clinic.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.discharge.clinic.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             discharge.clinic.cost=sum(study.pop.discharge.clinic.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(discharge.clinic.cost=nice.num.count(discharge.clinic.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Discharge clinic costs",
                    "Discharge clinic costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             discharge.clinic.cost=sum(study.pop.discharge.clinic.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(discharge.clinic.cost=nice.num.count(discharge.clinic.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Discharge clinic costs",
                    "Discharge clinic costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             discharge.clinic.cost=sum(study.pop.discharge.clinic.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(discharge.clinic.cost=nice.num.count(discharge.clinic.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Discharge clinic costs",
                    "Discharge clinic costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             discharge.clinic.cost=sum(study.pop.discharge.clinic.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(discharge.clinic.cost=nice.num.count(discharge.clinic.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Discharge clinic costs",
                    "Discharge clinic costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.medication.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time


costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(medication.cost=sum(medication.cost)) %>%
mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(medication.cost=sum(medication.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.medication.cost<- round(costs$medication.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.medication.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             medication.cost=sum(study.pop.medication.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(medication.cost=nice.num.count(medication.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Medication costs",
                    "Medication costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             medication.cost=sum(study.pop.medication.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(medication.cost=nice.num.count(medication.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Medication costs",
                    "Medication costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             medication.cost=sum(study.pop.medication.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(medication.cost=nice.num.count(medication.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Medication costs",
                    "Medication costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             medication.cost=sum(study.pop.medication.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(medication.cost=nice.num.count(medication.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Medication costs",
                    "Medication costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.fx_prev.staff.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time


costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(fx_prev.staff.cost=sum(fx_prev.staff.cost)) %>%
mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(fx_prev.staff.cost=sum(fx_prev.staff.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.fx_prev.staff.cost<- round(costs$fx_prev.staff.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.fx_prev.staff.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             fx_prev.staff.cost=sum(study.pop.fx_prev.staff.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(fx_prev.staff.cost=nice.num.count(fx_prev.staff.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Fracture prevention staff costs",
                    "Fracture prevention staff costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             fx_prev.staff.cost=sum(study.pop.fx_prev.staff.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(fx_prev.staff.cost=nice.num.count(fx_prev.staff.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Fracture prevention staff costs",
                    "Fracture prevention staff costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             fx_prev.staff.cost=sum(study.pop.fx_prev.staff.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(fx_prev.staff.cost=nice.num.count(fx_prev.staff.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Fracture prevention staff costs",
                    "Fracture prevention staff costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             fx_prev.staff.cost=sum(study.pop.fx_prev.staff.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(fx_prev.staff.cost=nice.num.count(fx_prev.staff.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Fracture prevention staff costs",
                    "Fracture prevention staff costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.lab.test.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time

costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(lab.test.cost=sum(lab.test.cost)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(lab.test.cost=sum(lab.test.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.lab.test.cost<- round(costs$lab.test.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.lab.test.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             lab.test.cost=sum(study.pop.lab.test.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(lab.test.cost=nice.num.count(lab.test.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Lab test costs",
                    "Lab test costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             lab.test.cost=sum(study.pop.lab.test.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(lab.test.cost=nice.num.count(lab.test.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Lab test costs",
                    "Lab test costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             lab.test.cost=sum(study.pop.lab.test.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(lab.test.cost=nice.num.count(lab.test.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Lab test costs",
                    "Lab test costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             lab.test.cost=sum(study.pop.lab.test.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(lab.test.cost=nice.num.count(lab.test.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Lab test costs",
                    "Lab test costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.dxa.cost<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time


costs<-m.TR.df %>%
  filter(month<={{costs.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(dxa.cost=sum(dxa.cost)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(dxa.cost=sum(dxa.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.dxa.cost<- round(costs$dxa.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.dxa.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             dxa.cost=sum(study.pop.dxa.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(dxa.cost=nice.num.count(dxa.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "DXA costs",
                    "DXA costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             dxa.cost=sum(study.pop.dxa.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(dxa.cost=nice.num.count(dxa.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "DXA costs",
                    "DXA costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.costs.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             dxa.cost=sum(study.pop.dxa.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(dxa.cost=nice.num.count(dxa.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "DXA costs",
                    "DXA costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             dxa.cost=sum(study.pop.dxa.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(dxa.cost=nice.num.count(dxa.cost),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "DXA costs",
                    "DXA costs avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

# location -----
get.summary.h_no_supp<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

costs<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(h_no_supp=sum(location=="home, no support")) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(h_no_supp=sum(h_no_supp))
costs$microsim_pop<-n_microsimulation

#to years
costs<-costs %>% 
  mutate(h_no_supp=h_no_supp/12)

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))




# get weighted cost for study population
costs$study.pop.h_no_supp<- round(costs$h_no_supp*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.h_no_supp)

if(input$choose.hcru.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             h_no_supp=sum(study.pop.h_no_supp))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(h_no_supp=nice.num.count(h_no_supp),
         difference=nice.num.count(difference))


table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             h_no_supp=sum(study.pop.h_no_supp))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(h_no_supp=nice.num.count(h_no_supp),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Index fracture",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             h_no_supp=sum(study.pop.h_no_supp))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(h_no_supp=nice.num.count(h_no_supp),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             h_no_supp=sum(study.pop.h_no_supp))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(h_no_supp=nice.num.count(h_no_supp),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Index fracture",
#                     "Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.h_supp<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

costs<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(h_supp=sum(location=="home, support")) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(h_supp=sum(h_supp))
costs$microsim_pop<-n_microsimulation

#to years
costs<-costs %>% 
  mutate(h_supp=h_supp/12)

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))




# get weighted cost for study population
costs$study.pop.h_supp<- round(costs$h_supp*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.h_supp)

if(input$choose.hcru.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             h_supp=sum(study.pop.h_supp))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(h_supp=nice.num.count(h_supp),
         difference=nice.num.count(difference))


table$study.pop.n<-nice.num.count(table$study.pop.n)

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             h_supp=sum(study.pop.h_supp))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(h_supp=nice.num.count(h_supp),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else if(input$choose.hcru.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             h_supp=sum(study.pop.h_supp))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(h_supp=nice.num.count(h_supp),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             h_supp=sum(study.pop.h_supp))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(h_supp=nice.num.count(h_supp),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}
  #output -----
table
}


get.summary.f_home<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

costs<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(f_home=sum(location=="family home")) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(f_home=sum(f_home))
costs$microsim_pop<-n_microsimulation

#to years
costs<-costs %>% 
  mutate(f_home=f_home/12)

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))




# get weighted cost for study population
costs$study.pop.f_home<- round(costs$f_home*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.f_home)

if(input$choose.hcru.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             f_home=sum(study.pop.f_home))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(f_home=nice.num.count(f_home),
         difference=nice.num.count(difference))


table$study.pop.n<-nice.num.count(table$study.pop.n)

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             f_home=sum(study.pop.f_home))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(f_home=nice.num.count(f_home),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else if(input$choose.hcru.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             f_home=sum(study.pop.f_home))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(f_home=nice.num.count(f_home),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             f_home=sum(study.pop.f_home))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(f_home=nice.num.count(f_home),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}
  #output -----
table
}

get.summary.ltc<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

costs<-m.TR.df %>%
  filter(month<={{hcru.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(ltc=sum(location=="long term care")) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

costs<-costs %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(ltc=sum(ltc))
costs$microsim_pop<-n_microsimulation

#to years
costs<-costs %>% 
  mutate(ltc=ltc/12)

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
costs<-costs %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))




# get weighted cost for study population
costs$study.pop.ltc<- round(costs$ltc*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.ltc)

if(input$choose.hcru.summary=="Overall"){
table<- costs %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             ltc=sum(study.pop.ltc))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(ltc=nice.num.count(ltc),
         difference=nice.num.count(difference))


table$study.pop.n<-nice.num.count(table$study.pop.n)

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- costs %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             ltc=sum(study.pop.ltc))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(ltc=nice.num.count(ltc),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else if(input$choose.hcru.summary=="By sex"){
table<- costs %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             ltc=sum(study.pop.ltc))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(ltc=nice.num.count(ltc),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else{ #"By sentinel fracture and sex"

table<- costs %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             ltc=sum(study.pop.ltc))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(ltc=nice.num.count(ltc),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}
  #output -----
table
}

# ever in ltc -----
get.summary.ever.ltc<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){

ever.ltc<-m.TR.df %>%
    group_by(id, index_fx, sex, intervention) %>% 
    summarise(ever.ltc=ifelse(any(location=="long term care"),1,0)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

ever.ltc<-ever.ltc %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(
    ever.ltc=sum(ever.ltc))
ever.ltc$microsim_pop<-n_microsimulation


# add study pop
ever.ltc$study.pop.n<-NA
ever.ltc<-ever.ltc %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
ever.ltc<-ever.ltc %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
ever.ltc<-ever.ltc %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

ever.ltc<-ever.ltc %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
ever.ltc<-ever.ltc %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
ever.ltc<-ever.ltc %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted number for study population
ever.ltc$study.pop.ever.ltc<- round(ever.ltc$ever.ltc*
  (ever.ltc$study.pop.n/ever.ltc$microsim_pop))


if(input$choose.hcru.summary=="Overall"){
table<- ever.ltc %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             ever.ltc=sum(study.pop.ever.ltc))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate( ever.ltc=nice.num.count(ever.ltc),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Long term care",
                    "Long term care avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){
table<- ever.ltc %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             ever.ltc=sum(study.pop.ever.ltc))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate( ever.ltc=nice.num.count(ever.ltc),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Long term care",
                    "Long term care avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))


}else if(input$choose.hcru.summary=="By sex"){
 table<- ever.ltc %>%
  group_by(sex,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             ever.ltc=sum(study.pop.ever.ltc))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}


table<-table %>%
  mutate(ever.ltc=nice.num.count(ever.ltc),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Long term care",
                    "Long term care avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))


}else{ #"By sentinel fracture and sex"

table<- ever.ltc %>%
  group_by(sex, index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             ever.ltc=sum(study.pop.ever.ltc))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate( ever.ltc=nice.num.count(ever.ltc),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Sex",
                    "Index fracture",
                    "Intervention",
                    "N target population",
                    "Long term care",
                    "Long term care avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))  
  


}
  #output -----
table

}


# hosp los categories -----
get.fac.days.in.hospital_0<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hosplos0<-m.TR.df %>%
  group_by(id, index_fx, sex, intervention) %>%
  filter(month>=1) %>% 
  summarise(hosplos0=ifelse(sum(hosp.los)==0,1,0)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

hosplos0<-hosplos0 %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(hosplos0=sum(hosplos0))
hosplos0$microsim_pop<-n_microsimulation

# add study pop
hosplos0$study.pop.n<-NA
hosplos0<-hosplos0 %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
hosplos0<-hosplos0 %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
hosplos0<-hosplos0 %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

hosplos0<-hosplos0 %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
hosplos0<-hosplos0 %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
hosplos0<-hosplos0 %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))




# get weighted cost for study population
hosplos0$study.pop.hosplos0<- round(hosplos0$hosplos0*
  (hosplos0$study.pop.n/hosplos0$microsim_pop))

hosplos0<-hosplos0 %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.hosplos0)

if(input$choose.hcru.summary=="Overall"){
table<- hosplos0 %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos0=sum(study.pop.hosplos0))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(hosplos0=nice.num.count(hosplos0),
         difference=nice.num.count(difference))


table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- hosplos0 %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos0=sum(study.pop.hosplos0))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(hosplos0=nice.num.count(hosplos0),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else if(input$choose.hcru.summary=="By sex"){
table<- hosplos0 %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos0=sum(study.pop.hosplos0))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(hosplos0=nice.num.count(hosplos0),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- hosplos0 %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos0=sum(study.pop.hosplos0))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(hosplos0=nice.num.count(hosplos0),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Index fracture",
#                     "Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.fac.days.in.hospital_1_7<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hosplos1_7<-m.TR.df %>%
  group_by(id, index_fx, sex, intervention) %>%
  filter(month>=1) %>% 
  summarise(hosplos1_7=ifelse(sum(hosp.los)>=1 & sum(hosp.los)<= 7,1,0)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

hosplos1_7<-hosplos1_7 %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(hosplos1_7=sum(hosplos1_7))
hosplos1_7$microsim_pop<-n_microsimulation

# add study pop
hosplos1_7$study.pop.n<-NA
hosplos1_7<-hosplos1_7 %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
hosplos1_7<-hosplos1_7 %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
hosplos1_7<-hosplos1_7 %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

hosplos1_7<-hosplos1_7 %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
hosplos1_7<-hosplos1_7 %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
hosplos1_7<-hosplos1_7 %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))




# get weighted cost for study population
hosplos1_7$study.pop.hosplos1_7<- round(hosplos1_7$hosplos1_7*
  (hosplos1_7$study.pop.n/hosplos1_7$microsim_pop))

hosplos1_7<-hosplos1_7 %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.hosplos1_7)

if(input$choose.hcru.summary=="Overall"){
table<- hosplos1_7 %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos1_7=sum(study.pop.hosplos1_7))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(hosplos1_7=nice.num.count(hosplos1_7),
         difference=nice.num.count(difference))


table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- hosplos1_7 %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos1_7=sum(study.pop.hosplos1_7))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(hosplos1_7=nice.num.count(hosplos1_7),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else if(input$choose.hcru.summary=="By sex"){
table<- hosplos1_7 %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos1_7=sum(study.pop.hosplos1_7))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(hosplos1_7=nice.num.count(hosplos1_7),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- hosplos1_7 %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos1_7=sum(study.pop.hosplos1_7))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(hosplos1_7=nice.num.count(hosplos1_7),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Index fracture",
#                     "Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.fac.days.in.hospital_8_14<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hosplos8_14<-m.TR.df %>%
  group_by(id, index_fx, sex, intervention) %>%
  filter(month>=1) %>% 
  summarise(hosplos8_14=ifelse(sum(hosp.los)>=8 & sum(hosp.los)<= 14,1,0)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

hosplos8_14<-hosplos8_14 %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(hosplos8_14=sum(hosplos8_14))
hosplos8_14$microsim_pop<-n_microsimulation

# add study pop
hosplos8_14$study.pop.n<-NA
hosplos8_14<-hosplos8_14 %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
hosplos8_14<-hosplos8_14 %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
hosplos8_14<-hosplos8_14 %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

hosplos8_14<-hosplos8_14 %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
hosplos8_14<-hosplos8_14 %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
hosplos8_14<-hosplos8_14 %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))




# get weighted cost for study population
hosplos8_14$study.pop.hosplos8_14<- round(hosplos8_14$hosplos8_14*
  (hosplos8_14$study.pop.n/hosplos8_14$microsim_pop))

hosplos8_14<-hosplos8_14 %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.hosplos8_14)

if(input$choose.hcru.summary=="Overall"){
table<- hosplos8_14 %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos8_14=sum(study.pop.hosplos8_14))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(hosplos8_14=nice.num.count(hosplos8_14),
         difference=nice.num.count(difference))


table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- hosplos8_14 %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos8_14=sum(study.pop.hosplos8_14))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(hosplos8_14=nice.num.count(hosplos8_14),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else if(input$choose.hcru.summary=="By sex"){
table<- hosplos8_14 %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos8_14=sum(study.pop.hosplos8_14))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(hosplos8_14=nice.num.count(hosplos8_14),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- hosplos8_14 %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos8_14=sum(study.pop.hosplos8_14))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(hosplos8_14=nice.num.count(hosplos8_14),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Index fracture",
#                     "Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.fac.days.in.hospital_15_30<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hosplos15_30<-m.TR.df %>%
  group_by(id, index_fx, sex, intervention) %>%
  filter(month>=1) %>% 
  summarise(hosplos15_30=ifelse(sum(hosp.los)>=15 & sum(hosp.los)<= 30,1,0)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

hosplos15_30<-hosplos15_30 %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(hosplos15_30=sum(hosplos15_30))
hosplos15_30$microsim_pop<-n_microsimulation

# add study pop
hosplos15_30$study.pop.n<-NA
hosplos15_30<-hosplos15_30 %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
hosplos15_30<-hosplos15_30 %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
hosplos15_30<-hosplos15_30 %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

hosplos15_30<-hosplos15_30 %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
hosplos15_30<-hosplos15_30 %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
hosplos15_30<-hosplos15_30 %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))




# get weighted cost for study population
hosplos15_30$study.pop.hosplos15_30<- round(hosplos15_30$hosplos15_30*
  (hosplos15_30$study.pop.n/hosplos15_30$microsim_pop))

hosplos15_30<-hosplos15_30 %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.hosplos15_30)

if(input$choose.hcru.summary=="Overall"){
table<- hosplos15_30 %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos15_30=sum(study.pop.hosplos15_30))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(hosplos15_30=nice.num.count(hosplos15_30),
         difference=nice.num.count(difference))


table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- hosplos15_30 %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos15_30=sum(study.pop.hosplos15_30))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(hosplos15_30=nice.num.count(hosplos15_30),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else if(input$choose.hcru.summary=="By sex"){
table<- hosplos15_30 %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos15_30=sum(study.pop.hosplos15_30))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(hosplos15_30=nice.num.count(hosplos15_30),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- hosplos15_30 %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos15_30=sum(study.pop.hosplos15_30))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(hosplos15_30=nice.num.count(hosplos15_30),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Index fracture",
#                     "Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.fac.days.in.hospital_31up<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){
hosplos31up<-m.TR.df %>%
  group_by(id, index_fx, sex, intervention) %>%
  filter(month>=1) %>% 
  summarise(hosplos31up=ifelse(sum(hosp.los)>=31,1,0)) %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))

hosplos31up<-hosplos31up %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(hosplos31up=sum(hosplos31up))
hosplos31up$microsim_pop<-n_microsimulation

# add study pop
hosplos31up$study.pop.n<-NA
hosplos31up<-hosplos31up %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
hosplos31up<-hosplos31up %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
hosplos31up<-hosplos31up %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

hosplos31up<-hosplos31up %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
hosplos31up<-hosplos31up %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
hosplos31up<-hosplos31up %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))




# get weighted cost for study population
hosplos31up$study.pop.hosplos31up<- round(hosplos31up$hosplos31up*
  (hosplos31up$study.pop.n/hosplos31up$microsim_pop))

hosplos31up<-hosplos31up %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.hosplos31up)

if(input$choose.hcru.summary=="Overall"){
table<- hosplos31up %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos31up=sum(study.pop.hosplos31up))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])

table<-table %>%
  mutate(hosplos31up=nice.num.count(hosplos31up),
         difference=nice.num.count(difference))


table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- hosplos31up %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos31up=sum(study.pop.hosplos31up))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>%
  mutate(hosplos31up=nice.num.count(hosplos31up),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


}else if(input$choose.hcru.summary=="By sex"){
table<- hosplos31up %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos31up=sum(study.pop.hosplos31up))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>%
  mutate(hosplos31up=nice.num.count(hosplos31up),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- hosplos31up %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             hosplos31up=sum(study.pop.hosplos31up))

table$difference<-""
table$difference[2]<-as.numeric(table[1,5]- table[2,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,5]- table[4,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,5]- table[6,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[7,5]- table[8,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[9,5]- table[10,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[11,5]- table[12,5])
}

table<-table %>%
  mutate(hosplos31up=nice.num.count(hosplos31up),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


# table<- kable(table,
#       col.names = c("Index fracture",
#                     "Sex",
#                     "Intervention",
#                     "N target population",
#                     "Home, no support (years)",
#                     "Difference in home, no support (years)")) %>%
#   kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}
# qol ----
get.summary.qol<-function(m.TR.df,
                                microsim_pop,
                                study_pop_n,
                                input){

qol.time<-input$qol.time

qol<-m.TR.df %>%
  filter(month<={{qol.time}}) %>%
  group_by(id, index_fx, sex, intervention) %>%
  summarise(qol=sum(qol)/12)

qol<- qol %>%
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                             "Current practice")) %>%
  mutate(intervention=factor(intervention,
                             levels=c("Current practice", "FLS")))


qol<-qol %>%
  group_by(intervention, index_fx, sex) %>%
  summarise(qol=sum(qol))
qol$microsim_pop<-n_microsimulation

# add study pop
qol$study.pop.n<-NA
qol<-qol %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.male") %>%
    select(n)), study.pop.n))
qol<-qol %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.male") %>%
    select(n)), study.pop.n))
qol<-qol %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.male") %>%
    select(n)), study.pop.n))

qol<-qol %>%
  mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="spine_fx_n.female") %>%
    select(n)), study.pop.n))
qol<-qol %>%
  mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="hip_fx_n.female") %>%
    select(n)), study.pop.n))
qol<-qol %>%
  mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
         as.numeric(study_pop_n %>%
  filter(names=="other_fx_n.female") %>%
    select(n)), study.pop.n))

# get weighted for study population
qol$study.pop.qol<- round(qol$qol*
  (qol$study.pop.n/qol$microsim_pop))

qol<-qol %>%
  select(intervention, index_fx, sex, study.pop.n, study.pop.qol)

if(input$choose.qol.summary=="Overall"){
table<- qol %>%
  group_by(intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             qol=sum(study.pop.qol))

table$difference<-""
table$difference[2]<-as.numeric(table[2,3]- table[1,3])

table<-table %>%
  mutate(qol=nice.num.count(qol),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "QALYs",
                    "QALYs gained")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.qol.summary=="By sentinel fracture"){

table<- qol %>%
  group_by(index_fx,intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             qol=sum(study.pop.qol))


table$difference<-""
table$difference[2]<-as.numeric(table[2,4]- table[1,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[4,4]- table[3,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[6,4]- table[5,4])
}

table<-table %>%
  mutate(qol=nice.num.count(qol),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "QALYs",
                    "QALYs gained")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.qol.summary=="By sex"){
table<- qol %>%
  group_by(sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             qol=sum(study.pop.qol))

table$difference<-""
table$difference[2]<-as.numeric(table[2,4]- table[1,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[4,4]- table[3,4])
}

table<-table %>%
  mutate(qol=nice.num.count(qol),
         difference=nice.num.count(difference))
table$study.pop.n<-nice.num.count(table$study.pop.n)


table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "QALYs",
                    "QALYs gained")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- qol %>%
  group_by(index_fx, sex, intervention) %>%
   summarise(study.pop.n=sum(study.pop.n),
             qol=sum(study.pop.qol))

table$difference<-""
table$difference[2]<-as.numeric(table[2,5]- table[1,5])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[4,5]- table[3,5])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[6,5]- table[5,5])
}

if(nrow(table)>7){
table$difference[8]<-as.numeric(table[8,5]- table[7,5])
}

if(nrow(table)>9){
table$difference[10]<-as.numeric(table[10,5]- table[9,5])
}

if(nrow(table)>11){
table$difference[12]<-as.numeric(table[12,5]- table[11,5])
}

table<-table %>%
  mutate(qol=nice.num.count(qol),
         difference=nice.num.count(difference))

table$study.pop.n<-nice.num.count(table$study.pop.n)

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "QALYs",
                    "QALYs gained")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}
  #output -----
table
}

get.summary.qol1<-function(m.TR.df,
                          microsim_pop,
                          study_pop_n,
                          input){
  
  min.qol.time<-input$min.qol.time
  qol.time<-input$qol.time
  
  qol<-m.TR.df %>%
    filter(month>{{min.qol.time}}) %>%
    filter(month<={{qol.time}}) %>%
    group_by(id, index_fx, sex, intervention) %>%
    summarise(qol=sum(qol)/12)
  
  qol<- qol %>%
    mutate(intervention=ifelse(intervention=="FLS", "FLS",
                               "Current practice")) %>%
    mutate(intervention=factor(intervention,
                               levels=c("Current practice", "FLS")))
  
  
  qol<-qol %>%
    group_by(intervention, index_fx, sex) %>%
    summarise(qol=sum(qol))
  qol$microsim_pop<-n_microsimulation
  
  # add study pop
  qol$study.pop.n<-NA
  qol<-qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.male") %>%
                                           select(n)), study.pop.n))
  qol<-qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.male") %>%
                                           select(n)), study.pop.n))
  qol<-qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.male") %>%
                                           select(n)), study.pop.n))
  
  qol<-qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.female") %>%
                                           select(n)), study.pop.n))
  qol<-qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.female") %>%
                                           select(n)), study.pop.n))
  qol<-qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.female") %>%
                                           select(n)), study.pop.n))
  
  # get weighted for study population
  qol$study.pop.qol<- round(qol$qol*
                              (qol$study.pop.n/qol$microsim_pop))
  
  qol<-qol %>%
    select(intervention, index_fx, sex, study.pop.n, study.pop.qol)
  
  if(input$choose.qol.summary=="Overall"){
    table<- qol %>%
      group_by(intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                qol=sum(study.pop.qol))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[2,3]- table[1,3])
    
    table<-table %>%
      mutate(qol=nice.num.count(qol),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Intervention",
                                "N target population",
                                "QALYs",
                                "QALYs gained")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.qol.summary=="By sentinel fracture"){
    
    table<- qol %>%
      group_by(index_fx,intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                qol=sum(study.pop.qol))
    
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[2,4]- table[1,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[4,4]- table[3,4])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[6,4]- table[5,4])
    }
    
    table<-table %>%
      mutate(qol=nice.num.count(qol),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Intervention",
                                "N target population",
                                "QALYs",
                                "QALYs gained")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.qol.summary=="By sex"){
    table<- qol %>%
      group_by(sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                qol=sum(study.pop.qol))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[2,4]- table[1,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[4,4]- table[3,4])
    }
    
    table<-table %>%
      mutate(qol=nice.num.count(qol),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Sex",
                                "Intervention",
                                "N target population",
                                "QALYs",
                                "QALYs gained")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else{ #"By sentinel fracture and sex"
    
    table<- qol %>%
      group_by(index_fx, sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                qol=sum(study.pop.qol))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[2,5]- table[1,5])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[4,5]- table[3,5])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[6,5]- table[5,5])
    }
    
    if(nrow(table)>7){
      table$difference[8]<-as.numeric(table[8,5]- table[7,5])
    }
    
    if(nrow(table)>9){
      table$difference[10]<-as.numeric(table[10,5]- table[9,5])
    }
    
    if(nrow(table)>11){
      table$difference[12]<-as.numeric(table[12,5]- table[11,5])
    }
    
    table<-table %>%
      mutate(qol=nice.num.count(qol),
             difference=nice.num.count(difference))
    
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Sex",
                                "Intervention",
                                "N target population",
                                "QALYs",
                                "QALYs gained")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }
  #output -----
  table
}

# discounted.qol ----
get.summary.discounted.qol<-function(m.TR.df,
                          microsim_pop,
                          study_pop_n,
                          input){
  
  discounted.qol.time<-input$discounted.qol.time
  
  discounted.qol<-m.TR.df %>%
    filter(month<={{discounted.qol.time}}) %>%
    group_by(id, index_fx, sex, intervention) %>%
    summarise(discounted.qol=sum(discounted.qol)/12)
  
  discounted.qol<- discounted.qol %>%
    mutate(intervention=ifelse(intervention=="FLS", "FLS",
                               "Current practice")) %>%
    mutate(intervention=factor(intervention,
                               levels=c("Current practice", "FLS")))
  
  
  discounted.qol<-discounted.qol %>%
    group_by(intervention, index_fx, sex) %>%
    summarise(discounted.qol=sum(discounted.qol))
  discounted.qol$microsim_pop<-n_microsimulation
  
  # add study pop
  discounted.qol$study.pop.n<-NA
  discounted.qol<-discounted.qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.male") %>%
                                           select(n)), study.pop.n))
  discounted.qol<-discounted.qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.male") %>%
                                           select(n)), study.pop.n))
  discounted.qol<-discounted.qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Male",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.male") %>%
                                           select(n)), study.pop.n))
  
  discounted.qol<-discounted.qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Spine" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="spine_fx_n.female") %>%
                                           select(n)), study.pop.n))
  discounted.qol<-discounted.qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Hip" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="hip_fx_n.female") %>%
                                           select(n)), study.pop.n))
  discounted.qol<-discounted.qol %>%
    mutate(study.pop.n=ifelse(index_fx=="Other" & sex=="Female",
                              as.numeric(study_pop_n %>%
                                           filter(names=="other_fx_n.female") %>%
                                           select(n)), study.pop.n))
  
  # get weighted for study population
  discounted.qol$study.pop.discounted.qol<- round(discounted.qol$discounted.qol*
                              (discounted.qol$study.pop.n/discounted.qol$microsim_pop))
  
  discounted.qol<-discounted.qol %>%
    select(intervention, index_fx, sex, study.pop.n, study.pop.discounted.qol)
  
  if(input$choose.discounted.qol.summary=="Overall"){
    table<- discounted.qol %>%
      group_by(intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                discounted.qol=sum(study.pop.discounted.qol))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[2,3]- table[1,3])
    
    table<-table %>%
      mutate(discounted.qol=nice.num.count(discounted.qol),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Intervention",
                                "N target population",
                                "Discounted QALYs",
                                "Discounted QALYs gained")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.discounted.qol.summary=="By sentinel fracture"){
    
    table<- discounted.qol %>%
      group_by(index_fx,intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                discounted.qol=sum(study.pop.discounted.qol))
    
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[2,4]- table[1,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[4,4]- table[3,4])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[6,4]- table[5,4])
    }
    
    table<-table %>%
      mutate(discounted.qol=nice.num.count(discounted.qol),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Intervention",
                                "N target population",
                                "Discounted QALYs",
                                "Discounted QALYs gained")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else if(input$choose.discounted.qol.summary=="By sex"){
    table<- discounted.qol %>%
      group_by(sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                discounted.qol=sum(study.pop.discounted.qol))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[2,4]- table[1,4])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[4,4]- table[3,4])
    }
    
    table<-table %>%
      mutate(discounted.qol=nice.num.count(discounted.qol),
             difference=nice.num.count(difference))
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    
    table<- kable(table,
                  col.names = c("Sex",
                                "Intervention",
                                "N target population",
                                "Discounted QALYs",
                                "Discounted QALYs gained")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }else{ #"By sentinel fracture and sex"
    
    table<- discounted.qol %>%
      group_by(index_fx, sex, intervention) %>%
      summarise(study.pop.n=sum(study.pop.n),
                discounted.qol=sum(study.pop.discounted.qol))
    
    table$difference<-""
    table$difference[2]<-as.numeric(table[2,5]- table[1,5])
    
    if(nrow(table)>3){
      table$difference[4]<-as.numeric(table[4,5]- table[3,5])
    }
    
    if(nrow(table)>5){
      table$difference[6]<-as.numeric(table[6,5]- table[5,5])
    }
    
    if(nrow(table)>7){
      table$difference[8]<-as.numeric(table[8,5]- table[7,5])
    }
    
    if(nrow(table)>9){
      table$difference[10]<-as.numeric(table[10,5]- table[9,5])
    }
    
    if(nrow(table)>11){
      table$difference[12]<-as.numeric(table[12,5]- table[11,5])
    }
    
    table<-table %>%
      mutate(discounted.qol=nice.num.count(discounted.qol),
             difference=nice.num.count(difference))
    
    table$study.pop.n<-nice.num.count(table$study.pop.n)
    
    table<- kable(table,
                  col.names = c("Index fracture",
                                "Sex",
                                "Intervention",
                                "N target population",
                                "Discounted QALYs",
                                "Discounted QALYs gained")) %>%
      kable_styling(bootstrap_options = c("striped", "bordered"))
    
  }
  #output -----
  table
}