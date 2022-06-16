## summary tables
# number of subsequent fractures -----
get.summary.second.fx<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
sec.frac.time<-input$sec.frac.time
  
sub.frac<-lapply(m.TR,
                 function(df) {
                  sub.frac <- data.frame( 
                   sub.frac=df$h_af[which(df$month=={{sec.frac.time}})]-1, #minus index fx
                     id=as.numeric(df$id[1]),
                     intervention=df$intervention[1])
                   sub.frac})
sub.frac<-plyr::ldply(sub.frac,  
                      data.frame, .id=NULL) %>% 
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                            "Current practice")) %>% 
  mutate(intervention=factor(intervention,   
         levels=c("Current practice", "FLS")))

#histogram(sub.frac$sub.frac)
# add characterstics
sub.frac<-sub.frac %>% 
  left_join(microsim_pop,
            by=c("id"))

sub.frac<-sub.frac %>%  
  group_by(intervention, index_fx, sex) %>% 
  summarise(sum.sub.frac=sum(sub.frac))
sub.frac$microsim_pop<-n_microsimulation


# add study pop
sub.frac$study.pop.n<-NA
sub.frac<-sub.frac %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
sub.frac<-sub.frac %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
sub.frac<-sub.frac %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

sub.frac<-sub.frac %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
sub.frac<-sub.frac %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
sub.frac<-sub.frac %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.female") %>% 
    select(n)), study.pop.n))

# get weighted number for study population
sub.frac$study.pop.sub.frac<- round(sub.frac$sum.sub.frac*
  (sub.frac$study.pop.n/sub.frac$microsim_pop))

sub.frac<-sub.frac %>% 
  select(intervention, index_fx, sex, study.pop.n, study.pop.sub.frac)


if(input$choose.sec.frac.summary=="Overall"){
table<- sub.frac %>% 
  group_by(intervention) %>% 
   summarise(study.pop.n=sum(study.pop.n),
             n.fx=sum(study.pop.sub.frac))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])
  
table<-table %>% 
  mutate(n.fx=nice.num.count(n.fx),
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Subsequent fractures",
                    "Subsequent fractures avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.sec.frac.summary=="By sentinel fracture"){

table<- sub.frac %>% 
  group_by(index_fx,intervention) %>% 
   summarise(study.pop.n=sum(study.pop.n),
             n.fx=sum(study.pop.sub.frac))


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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Subsequent fractures",
                    "Subsequent fractures avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))
  
}else if(input$choose.sec.frac.summary=="By sex"){
table<- sub.frac %>% 
  group_by(sex, intervention) %>% 
   summarise(study.pop.n=sum(study.pop.n),
             n.fx=sum(study.pop.sub.frac))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>% 
  mutate(n.fx=nice.num.count(n.fx),
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Subsequent fractures",
                    "Subsequent fractures avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

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

table<-table %>% 
  mutate(n.fx=nice.num.count(n.fx),
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

table<- kable(table,
      col.names = c("Index fracture",
                    "Sex",
                    "Intervention",
                    "N target population",
                    "Subsequent fractures",
                    "Subsequent fractures avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

} 
  #output -----
table

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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
deaths<-deaths %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
deaths<-deaths %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

deaths<-deaths %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
deaths<-deaths %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
deaths<-deaths %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
get.summary.procedures<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
hcru.time<-input$hcru.time

procedures<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{hcru.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(procedures=sum(procedure!="none"))
               })

procedures<-plyr::ldply(procedures,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
procedures<-procedures %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
procedures<-procedures %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

procedures<-procedures %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
procedures<-procedures %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
procedures<-procedures %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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


get.summary.hosp.los<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
 # browser()
hcru.time<-input$hcru.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{hcru.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(hosp.los=sum(hosp.los))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                            "Current practice")) %>% 
  mutate(intervention=factor(intervention,   
         levels=c("Current practice", "FLS")))


costs<-costs %>%  
  group_by(intervention, index_fx, sex) %>% 
  summarise(hosp.los=sum(hosp.los))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.female") %>% 
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.hosp.los<- round(costs$hosp.los*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>% 
  select(intervention, index_fx, sex, study.pop.n, study.pop.hosp.los)

if(input$choose.hcru.summary=="Overall"){
table<- costs %>% 
  group_by(intervention) %>% 
   summarise(study.pop.n=sum(study.pop.n),
             hosp.los=sum(study.pop.hosp.los))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])
  
table<-table %>% 
  mutate(hosp.los=nice.num.count(hosp.los),
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

table<- kable(table,
      col.names = c("Intervention",
                    "N target population",
                    "Hospital bed days",
                    "Hospital bed days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else if(input$choose.hcru.summary=="By sentinel fracture"){

table<- costs %>% 
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

table<- kable(table,
      col.names = c("Index fracture",
                    "Intervention",
                    "N target population",
                    "Hospital bed days",
                    "Hospital bed days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))
  
}else if(input$choose.hcru.summary=="By sex"){
table<- costs %>% 
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

table<- kable(table,
      col.names = c("Sex",
                    "Intervention",
                    "N target population",
                    "Hospital bed days",
                    "Hospital bed days avoided")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

}else{ #"By sentinel fracture and sex"

table<- costs %>% 
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

# costs -----
get.summary.total.cost.excl.location<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(total.cost.excl.location=sum(total.cost.excl.location))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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


get.summary.total.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
 # browser()
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(total.cost=sum(total.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))

table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.hosp.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(hosp.cost=sum(hosp.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.procedure.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(procedure.cost=sum(procedure.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.location.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(location.cost=sum(location.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
  mutate(intervention=ifelse(intervention=="FLS", "FLS",
                            "Current practice")) %>% 
  mutate(intervention=factor(intervention,   
         levels=c("Current practice", "FLS")))


costs<-costs %>%  
  group_by(intervention, index_fx, sex) %>% 
  summarise(location.cost=sum(location.cost))
costs$microsim_pop<-n_microsimulation

# add study pop
costs$study.pop.n<-NA
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.female") %>% 
    select(n)), study.pop.n))

# get weighted cost for study population
costs$study.pop.location.cost<- round(costs$location.cost*
  (costs$study.pop.n/costs$microsim_pop))

costs<-costs %>% 
  select(intervention, index_fx, sex, study.pop.n, study.pop.location.cost)

if(input$choose.costs.summary=="Overall"){
table<- costs %>% 
  group_by(intervention) %>% 
   summarise(study.pop.n=sum(study.pop.n),
             location.cost=sum(study.pop.location.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,3]- table[2,3])
  
table<-table %>% 
  mutate(location.cost=nice.num.count(location.cost),
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
             location.cost=sum(study.pop.location.cost))


table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])
  
if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

if(nrow(table)>5){
table$difference[6]<-as.numeric(table[5,4]- table[6,4])
}

table<-table %>% 
  mutate(location.cost=nice.num.count(location.cost),
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
             location.cost=sum(study.pop.location.cost))

table$difference<-""
table$difference[2]<-as.numeric(table[1,4]- table[2,4])

if(nrow(table)>3){
table$difference[4]<-as.numeric(table[3,4]- table[4,4])
}

table<-table %>% 
  mutate(location.cost=nice.num.count(location.cost),
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
             location.cost=sum(study.pop.location.cost))

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
  mutate(location.cost=nice.num.count(location.cost),
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.comm.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(comm.cost=sum(comm.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.clinic.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(clinic.cost=sum(clinic.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.temp.rehab.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(temp.rehab.cost=sum(temp.rehab.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.discharge.clinic.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(discharge.clinic.cost=sum(discharge.clinic.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.medication.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(medication.cost=sum(medication.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.fx_prev.staff.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
 # browser()
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(fx_prev.staff.cost=sum(fx_prev.staff.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.lab.test.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(lab.test.cost=sum(lab.test.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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

get.summary.dxa.cost<-function(m.TR,
                                microsim_pop,
                                study_pop_n,
                                input){
costs.time<-input$costs.time
costs<-lapply(m.TR,
      function(df) {
        working.df<-df %>%  
          filter(month<={{costs.time}})
        
       working.df %>% 
          group_by(id, index_fx, sex, intervention) %>%
         summarise(dxa.cost=sum(dxa.cost))
               })

costs<-plyr::ldply(costs,  
                      data.frame, .id=NULL) %>% 
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
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="male",
         as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
    select(n)), study.pop.n))

costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="spine" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="hip" & sex=="female",
         as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
    select(n)), study.pop.n))
costs<-costs %>%  
  mutate(study.pop.n=ifelse(index_fx=="other" & sex=="female",
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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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
         difference=nice.num.count(as.numeric(difference)))
table<-apply(table, 2, 
              function(x) ifelse(is.na(x) | x == "NA", "", as.character(x)))

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