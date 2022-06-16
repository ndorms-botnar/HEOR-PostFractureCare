

gg.general.format<-function(plot){
  plot+ 
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(labels = percent, expand = c(0, 0))+
  theme(panel.spacing = unit(1, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) +
  ylab("Proportion of cohort")+
  xlab("Month")

}

gg.add.colours.meds<-function(plot){
  plot+
  scale_fill_manual(values=c("#f0f0f0", "#d53e4f", "#f46d43",
              "#fdae61","#fee08b","#e6f598",
               "#abdda4","#66c2a5", "#3288bd","#550307"))
}

gg.add.colours.y_n<-function(plot){
  plot+
  scale_fill_manual(values=c("#f0f0f0", "#b2182b"))
}

# ##
# get.c.inc.sec.fx.plot<-function(m.TR, input){
# #browser()
#   m.TR<- plyr::ldply(m.TR,  
#                       data.frame, .id=NULL)
# prop<-m.TR %>% 
#   filter(month>0) %>% 
#   group_by(month, intervention, index_fx, sex) %>% 
#   summarise(prop=sum(h_af-1)/length(id)) 
# 
# 
# prop %>% 
#   ggplot(aes(group=intervention, linetype=intervention))+
#   facet_grid(index_fx~sex)+
#   geom_step(aes(month, prop)) +
#   scale_y_continuous(labels = percent) + 
#    theme(legend.title = element_blank(), 
#         legend.position = "top") 
# }
# ##
get.c.inc.deaths.plot<-function(m.TR, input){
#browser()
  m.TR<- plyr::ldply(m.TR,  
                      data.frame, .id=NULL)
prop<-m.TR %>% 
  filter(month>0) %>% 
  group_by(month, intervention, index_fx, sex) %>% 
  summarise(prop=sum(s_d)/length(id))


prop %>% 
  ggplot(aes(group=intervention, linetype=intervention))+
  facet_grid(index_fx~sex)+
  geom_step(aes(month, prop)) +
  scale_y_continuous(labels = percent)+ 
   theme(legend.title = element_blank(), 
        legend.position = "top") 
}

# 
# get.c.inc.sec.fx.plot1<-function(m.TR, input){
# 
# c.inc<-lapply(m.TR,
#                  function(df) {
# c.inc<-data.frame(
#   id=df$id[1],
#   intervention=df$intervention[1],
#   index_fx=df$index_fx[1],
#   sex=df$sex[1],
#   sec.frac.time=NA,
#   death.time=NA)
# 
# # add time of events if they occurred
# if(any(df$h_af==2)){
# c.inc$sec.frac.time<-Position(function(x) x == 2, df$h_af)-1} # happened in previous
# if(any(df$s_d==1)){
# c.inc$death.time<-Position(function(x) x == 1, df$s_d)}
# 
# # add status
# c.inc$sec.frac.status<-ifelse(!is.na(c.inc$sec.frac.time),1,0)
# c.inc$death.status<-ifelse(!is.na(c.inc$death.time),1,0)
# 
# # add censor date if no event - end of model
# c.inc$sec.frac.time<-ifelse(!is.na(c.inc$sec.frac.time),c.inc$sec.frac.time,61)
# c.inc$death.time<-ifelse(!is.na(c.inc$death.time),c.inc$death.time,61)
# 
# c.inc
# })
# c.inc<-plyr::ldply(c.inc,
#                       data.frame, .id=NULL)
# # competing risk
# c.inc <- c.inc %>%
#                    mutate(fx2_or_death.etime=ifelse(sec.frac.status==0,
#                                        death.time, sec.frac.time))
# c.inc <- c.inc %>%
#                    mutate(fx2_or_death.event=ifelse(sec.frac.status==0,
#                                        2*death.status, 1))
# 
# c.inc <- c.inc %>%
#                   mutate(fx2_or_death.event=
#                         factor(ifelse(fx2_or_death.event==0,
#                                              "censor",
#                                ifelse(fx2_or_death.event==1,
#                                       "fx2",
#                                ifelse(fx2_or_death.event==2,
#                                       "death",  NA)))))
# # plot sec fx
# if(input$choose.sec.frac.summary=="Overall"){
# 
# tidy(npsurv(Surv(fx2_or_death.etime,
#                 fx2_or_death.event)~intervention,
#            data=c.inc)) %>%
#        filter(state %in% c("fx2")) %>%
#   mutate(strata=ifelse(strata=="intervention=FLS", "FLS",
#                             "Current practice")) %>%
#   ggplot(aes(x=time, linetype=strata))+
#   geom_step(aes(y=estimate)) +
#   scale_y_continuous(labels = percent)+
#   xlab("Months") +
#   ylab("Cumulative incidence (%)") +
#   theme_bw(base_size = 18)  +
#   theme(legend.title = element_blank(),
#         legend.position = "top")
# }else if(input$choose.sec.frac.summary=="By sentinel fracture"){
# 
#  a<-tidy(npsurv(Surv(fx2_or_death.etime,
#                 fx2_or_death.event)~intervention+index_fx,
#            data=c.inc)) %>%
#        filter(state %in% c("fx2"))
# 
# b<-data.frame(str_split(a$strata, ", ", n = 2, simplify=TRUE))
# names(b)<-c("intervention", "index_fx")
# a<-cbind(a,b)
# 
# a %>%
#   mutate(intervention=ifelse(intervention=="intervention=FLS", "FLS",
#                             "Current practice")) %>%
#   mutate(index_fx=
#            ifelse(str_detect(index_fx, "hip"), "hip",
#            ifelse(str_detect(index_fx, "spine"), "spine",
#                           "other"))) %>%
#   ggplot(aes(x=time, linetype=intervention))+
#   facet_grid(index_fx~.)+
#   geom_step(aes(y=estimate)) +
#   scale_y_continuous(labels = percent)+
#   xlab("Months") +
#   ylab("Cumulative incidence (%)") +
#   theme_bw(base_size = 18)  +
#   theme(legend.title = element_blank(),
#         legend.position = "top")
# 
# }else if(input$choose.sec.frac.summary=="By sex"){
# 
# a<-tidy(npsurv(Surv(fx2_or_death.etime,
#                 fx2_or_death.event)~intervention+sex,
#            data=c.inc)) %>%
#        filter(state %in% c("death"))
# 
# b<-data.frame(str_split(a$strata, ", ", n = 2, simplify=TRUE))
# names(b)<-c("intervention", "sex")
# a<-cbind(a,b)
# 
# a %>%
#   mutate(intervention=ifelse(intervention=="intervention=FLS", "FLS",
#                             "Current practice")) %>%
#   mutate(sex=ifelse(sex=="sex=female", "female",
#                             "male")) %>%
#   ggplot(aes(x=time, linetype=intervention))+
#   facet_grid(sex~.)+
#   geom_step(aes(y=estimate)) +
#   scale_y_continuous(labels = percent)+
#   xlab("Months") +
#   ylab("Cumulative incidence (%)") +
#   theme_bw(base_size = 18)  +
#   theme(legend.title = element_blank(),
#         legend.position = "top")
# 
# }else{ #"By sentinel fracture and sex"
# 
# a<-tidy(npsurv(Surv(fx2_or_death.etime,
#                 fx2_or_death.event)~intervention+sex+index_fx,
#            data=c.inc)) %>%
#        filter(state %in% c("fx2"))
# 
# b<-data.frame(str_split(a$strata, ", ", n = 3, simplify=TRUE))
# names(b)<-c("intervention", "sex", "index_fx")
# a<-cbind(a,b)
# 
# a %>%
#   mutate(intervention=ifelse(intervention=="intervention=FLS", "FLS",
#                             "Current practice")) %>%
#     mutate(sex=ifelse(sex=="sex=female", "female",
#                             "male")) %>%
#   mutate(index_fx=
#            ifelse(str_detect(index_fx, "hip"), "hip",
#            ifelse(str_detect(index_fx, "spine"), "spine",
#                           "other"))) %>%
#   ggplot(aes(x=time, linetype=intervention))+
#   facet_grid(index_fx~sex)+
#   geom_step(aes(y=estimate)) +
#   scale_y_continuous(labels = percent)+
#   xlab("Months") +
#   ylab("Cumulative incidence (%)") +
#   theme_bw(base_size = 18)  +
#   theme(legend.title = element_blank(),
#         legend.position = "top")
# 
#   }
# 
# }
# 
##
get.c.inc.plot<-function(m.TR, input){

c.inc<-lapply(m.TR,
                 function(df) {
c.inc<-data.frame(
  id=df$id[1],
  intervention=df$intervention[1],
  index_fx=df$index_fx[1],
  sex=df$sex[1],
  sec.frac.time=NA,
  death.time=NA)

# add time of events if they occurred
if(any(df$h_af==2)){
c.inc$sec.frac.time<-Position(function(x) x == 2, df$h_af)-1} # happened in previous
if(any(df$s_d==1)){
c.inc$death.time<-Position(function(x) x == 1, df$s_d)}

# add status
c.inc$sec.frac.status<-ifelse(!is.na(c.inc$sec.frac.time),1,0)
c.inc$death.status<-ifelse(!is.na(c.inc$death.time),1,0)

# add censor date if no event - end of model
c.inc$sec.frac.time<-ifelse(!is.na(c.inc$sec.frac.time),c.inc$sec.frac.time,61)
c.inc$death.time<-ifelse(!is.na(c.inc$death.time),c.inc$death.time,61)

c.inc
})
#browser()
c.inc<-plyr::ldply(c.inc,
                      data.frame, .id=NULL)
# competing risk
c.inc <- c.inc %>%
                   mutate(fx2_or_death.etime=ifelse(sec.frac.status==0,
                                       death.time, sec.frac.time))
c.inc <- c.inc %>%
                   mutate(fx2_or_death.event=ifelse(sec.frac.status==0,
                                       2*death.status, 1))

c.inc <- c.inc %>%
                  mutate(fx2_or_death.event=
                        factor(ifelse(fx2_or_death.event==0,
                                             "censor",
                               ifelse(fx2_or_death.event==1,
                                      "fx2",
                               ifelse(fx2_or_death.event==2,
                                      "death",  NA)))))
# plot sec fx
if(input$choose.sec.frac.summary.c.inc=="Overall"){

tidy(npsurv(Surv(fx2_or_death.etime,
                fx2_or_death.event)~intervention,
           data=c.inc)) %>%
       filter(state %in% c("fx2")) %>%
  mutate(strata=ifelse(strata=="intervention=FLS", "FLS",
                            "Current practice")) %>%
  ggplot(aes(x=time, linetype=strata))+
  geom_step(aes(y=estimate)) +
  scale_y_continuous(labels = percent,
                     limits=c(0,NA))+
  xlab("Months") +
  ylab("Cumulative incidence (%)") +
  theme_bw(base_size = 18)  +
  theme(legend.title = element_blank(),
        legend.position = "top")
  
}else if(input$choose.sec.frac.summary.c.inc=="By sentinel fracture"){

  # only include as tems if both more than one index fx was generated
if(length(unique(c.inc$index_fx))>1){
  c.index_fx<-"index_fx"
} else{
  c.index_fx<-NULL
}
  #browser()
a<-tidy(npsurv(as.formula(paste("Surv(fx2_or_death.etime,
                fx2_or_death.event)~", 
                 paste(c("intervention", c.index_fx), collapse="+"))),
           data=c.inc)) %>%
       filter(state %in% c("fx2"))

b<-data.frame(str_split(a$strata, ", ", n = 2, simplify=TRUE),stringsAsFactors = FALSE)
names(b)<-c("st.intervention", "st.index_fx")
a<-cbind(a,b)

# add if missing (only one)
a$st.index_fx<-ifelse(str_detect(a$st.index_fx, "hip|spine|other", negate=TRUE),
                   unique(as.character(c.inc$index_fx)), a$st.index_fx)

a %>%
  mutate(st.intervention=ifelse(st.intervention=="intervention=FLS",
                             "FLS",
                            "Current practice")) %>%
  mutate(st.index_fx=
           ifelse(str_detect(st.index_fx, "hip"), "hip",
           ifelse(str_detect(st.index_fx, "spine"), "spine",
                          "other"))) %>%
  ggplot(aes(x=time, linetype=st.intervention))+
  facet_grid(st.index_fx~.)+
  geom_step(aes(y=estimate)) +
  scale_y_continuous(labels = percent,
                     limits=c(0,NA))+
  xlab("Months") +
  ylab("Cumulative incidence (%)") +
  theme_bw(base_size = 18)  +
  theme(legend.title = element_blank(),
        legend.position = "top")

}else if(input$choose.sec.frac.summary.c.inc=="By sex"){

  # only include as tems if both sex was generated
if(length(unique(c.inc$sex))==2){
  c.sex<-"sex"
} else{
  c.sex<-NULL
} 
  
a<-tidy(npsurv(as.formula(paste("Surv(fx2_or_death.etime,
                fx2_or_death.event)~", 
                 paste(c("intervention", c.sex), collapse="+"))),
           data=c.inc)) %>%
       filter(state %in% c("fx2"))

b<-data.frame(str_split(a$strata, ", ", n = 2, simplify=TRUE),stringsAsFactors = FALSE)
names(b)<-c("st.intervention", "st.sex")
a<-cbind(a,b)

# add if missing (only one)
a$st.sex<-ifelse(str_detect(a$st.sex, "male|female", negate=TRUE),
                   unique(as.character(c.inc$sex)), a$st.sex)

a %>%
  mutate(st.intervention=ifelse(st.intervention=="intervention=FLS", "FLS",
                            "Current practice")) %>%
  mutate(st.sex=ifelse(str_detect(st.sex, "female"), "female",
                            "male")) %>%
  ggplot(aes(x=time, linetype=st.intervention))+
  facet_grid(st.sex~.)+
  geom_step(aes(y=estimate)) +
  scale_y_continuous(labels = percent,
                     limits=c(0,NA))+
  xlab("Months") +
  ylab("Cumulative incidence (%)") +
  theme_bw(base_size = 18)  +
  theme(legend.title = element_blank(),
        legend.position = "top")

}else{ #"By sentinel fracture and sex"
# for index fx and sex
# only include as tems if both sexes/ more than one index fx was generated
if(length(unique(c.inc$sex))==2){
  c.sex<-"sex"
} else{
  c.sex<-NULL
}
if(length(unique(c.inc$index_fx))>1){
  c.index_fx<-"index_fx"
} else{
  c.index_fx<-NULL
}

a<-tidy(npsurv(as.formula(paste("Surv(fx2_or_death.etime,
                fx2_or_death.event)~", 
                 paste(c("intervention", c.sex, c.index_fx), collapse="+"))),
           data=c.inc)) %>%
       filter(state %in% c("fx2"))

b<-data.frame(str_split(a$strata, ", ", n = 3, simplify=TRUE),stringsAsFactors = FALSE)
if(!is.null(c.sex)){
names(b)<-c("st.intervention", "st.sex", "st.index_fx")
} else {
  names(b)<-c("st.intervention", "st.index_fx","st.sex")

}

a<-cbind(a,b)

# add if missing (only one)
a$st.index_fx<-ifelse(str_detect(a$st.index_fx, "hip|spine|other", negate=TRUE),
                   unique(as.character(c.inc$index_fx)), a$st.index_fx)
# add if missing (only one)
a$st.sex<-ifelse(str_detect(a$st.sex, "male|female", negate=TRUE),
                   unique(as.character(c.inc$sex)), a$st.sex)

a %>%
  mutate(st.intervention=ifelse(st.intervention=="intervention=FLS", "FLS",
                            "Current practice")) %>%
  mutate(st.sex=ifelse(str_detect(st.sex, "female"), "female",
                            "male")) %>%
  mutate(st.index_fx=
           ifelse(str_detect(st.index_fx, "hip"), "hip",
           ifelse(str_detect(st.index_fx, "spine"), "spine",
                          "other"))) %>%
  ggplot(aes(x=time, linetype=st.intervention))+
  facet_grid(st.index_fx~st.sex)+
  geom_step(aes(y=estimate)) +
  scale_y_continuous(labels = percent,
                     limits=c(0,NA))+
  xlab("Months") +
  ylab("Cumulative incidence (%)") +
  theme_bw(base_size = 18)  +
  theme(legend.title = element_blank(),
        legend.position = "top")

  }

}



### 
get.hist.subs.fx<-function(m.TR, input,
                                microsim_pop){
  
sec.frac.time<-input$sec.frac.time #60#
  
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

sub.frac<-sub.frac %>% 
  left_join(microsim_pop,
            by=c("id"))

if(input$choose.sec.frac.summary=="Overall"){

n<-sub.frac %>%
  group_by(intervention) %>% 
  tally() 
n<-as.numeric(n[1,2])

a<-sub.frac %>%
  group_by(intervention) %>% 
  summarise(fx.0=sum(sub.frac==0)/n,
            fx.1=sum(sub.frac==1)/n,
            fx.2=sum(sub.frac==2)/n,
            fx.3plus=sum(sub.frac>=3)/n)

a<-a %>%
 pivot_longer(-intervention, 
              names_to = "fx", values_to = "count")

a<-a %>% 
  mutate(fx=ifelse(fx=="fx.0", "0",
            ifelse(fx=="fx.1", "1",
            ifelse(fx=="fx.2", "2",
            ifelse(fx=="fx.3plus", "3+", NA))))) %>% 
  mutate(fx=factor(fx,
                  levels=c("0", "1", "2", "3+")))

a %>% 
  ggplot()+
  facet_grid(.~intervention)+
  geom_col(aes(fx, count), width=1, colour="grey")+
  scale_y_continuous(labels = percent)+ 
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) +
  ylab("Proportion of cohort")+
  xlab("Subsequent fractures")

}else if(input$choose.sec.frac.summary=="By sentinel fracture"){
n<-sub.frac %>%
  group_by(intervention, index_fx) %>% 
  tally() 
n<-as.numeric(n[1,3])

a<-sub.frac %>%
  group_by(intervention,index_fx) %>% 
  summarise(fx.0=sum(sub.frac==0)/n,
            fx.1=sum(sub.frac==1)/n,
            fx.2=sum(sub.frac==2)/n,
            fx.3plus=sum(sub.frac>=3)/n)

a<-a %>%
 pivot_longer(-c(intervention, index_fx),
              names_to = "fx", values_to = "count")

a<-a %>% 
  mutate(fx=ifelse(fx=="fx.0", "0",
            ifelse(fx=="fx.1", "1",
            ifelse(fx=="fx.2", "2",
            ifelse(fx=="fx.3plus", "3+", NA))))) %>% 
  mutate(fx=factor(fx,
                  levels=c("0", "1", "2", "3+")))

a %>% 
  ggplot()+
  facet_grid(index_fx~intervention)+
  geom_col(aes(fx, count), width=1, colour="grey")+
  scale_y_continuous(labels = percent)+ 
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) +
  ylab("Proportion of cohort")+
  xlab("Subsequent fractures")

}else if(input$choose.sec.frac.summary=="By sex"){
n<-sub.frac %>%
  group_by(intervention, sex) %>% 
  tally() 
n<-as.numeric(n[1,3])

a<-sub.frac %>%
  group_by(intervention,sex) %>% 
  summarise(fx.0=sum(sub.frac==0)/n,
            fx.1=sum(sub.frac==1)/n,
            fx.2=sum(sub.frac==2)/n,
            fx.3plus=sum(sub.frac>=3)/n)

a<-a %>%
 pivot_longer(-c(intervention, sex),
              names_to = "fx", values_to = "count")

a<-a %>% 
  mutate(fx=ifelse(fx=="fx.0", "0",
            ifelse(fx=="fx.1", "1",
            ifelse(fx=="fx.2", "2",
            ifelse(fx=="fx.3plus", "3+", NA))))) %>% 
  mutate(fx=factor(fx,
                  levels=c("0", "1", "2", "3+")))

a %>% 
  ggplot()+
  facet_grid(sex~intervention)+
  geom_col(aes(fx, count), width=1, colour="grey")+
  scale_y_continuous(labels = percent)+ 
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) +
  ylab("Proportion of cohort")+
  xlab("Subsequent fractures")
}else{ #"By sentinel fracture and sex"
n<-sub.frac %>%
  group_by(intervention, index_fx, sex) %>% 
  tally() 
n<-as.numeric(n[1,4])

a<-sub.frac %>%
  group_by(intervention,index_fx, sex) %>% 
  summarise(fx.0=sum(sub.frac==0)/n,
            fx.1=sum(sub.frac==1)/n,
            fx.2=sum(sub.frac==2)/n,
            fx.3plus=sum(sub.frac>=3)/n)

a<-a %>%
 pivot_longer(-c(intervention, index_fx, sex),
              names_to = "fx", values_to = "count")

a<-a %>% 
  mutate(fx=ifelse(fx=="fx.0", "0",
            ifelse(fx=="fx.1", "1",
            ifelse(fx=="fx.2", "2",
            ifelse(fx=="fx.3plus", "3+", NA))))) %>% 
  mutate(fx=factor(fx,
                  levels=c("0", "1", "2", "3+")))

a %>% 
  ggplot()+
  facet_grid(index_fx+sex~intervention)+
  geom_col(aes(fx, count), width=1, colour="grey")+
  scale_y_continuous(labels = percent)+ 
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) +
  ylab("Proportion of cohort")+
  xlab("Subsequent fractures")
  
}
}

get.hist.subs.fx1<-function(m.TR, input,
                                microsim_pop){
 # browser()
sec.frac.time<-input$sec.frac.time #60#
  
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

sub.frac<-sub.frac %>% 
  left_join(microsim_pop,
            by=c("id"))

if(input$choose.sec.frac.summary=="Overall"){

n<-sub.frac %>%
  group_by(intervention) %>% 
  tally() 
n<-as.numeric(n[1,2])

a<-sub.frac %>%
  group_by(intervention) %>% 
  summarise(fx.0=sum(sub.frac==0)/n,
            fx.1=sum(sub.frac==1)/n,
            fx.2=sum(sub.frac==2)/n,
            fx.3plus=sum(sub.frac>=3)/n)

a<-a %>%
 pivot_longer(-intervention, 
              names_to = "fx", values_to = "count")

a<-a %>% 
  mutate(fx=ifelse(fx=="fx.0", "0",
            ifelse(fx=="fx.1", "1",
            ifelse(fx=="fx.2", "2",
            ifelse(fx=="fx.3plus", "3+", NA))))) %>% 
  mutate(fx=factor(fx,
                  levels=c("0", "1", "2", "3+")))

a<-  a %>% filter(intervention=="FLS") %>% 
left_join(
a %>% filter(intervention=="Current practice"),
by=c("fx"))
a$count<-a$count.y-a$count.x

a %>% 
  ggplot()+
  geom_col(aes(fx, count), width=1, colour="grey")+
  scale_y_continuous(labels = percent)+ 
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) +
  ylab("Percentage point difference")+
  xlab("Subsequent fractures")

}else if(input$choose.sec.frac.summary=="By sentinel fracture"){
n<-sub.frac %>%
  group_by(intervention, index_fx) %>% 
  tally() 
n<-as.numeric(n[1,3])

a<-sub.frac %>%
  group_by(intervention,index_fx) %>% 
  summarise(fx.0=sum(sub.frac==0)/n,
            fx.1=sum(sub.frac==1)/n,
            fx.2=sum(sub.frac==2)/n,
            fx.3plus=sum(sub.frac>=3)/n)

a<-a %>%
 pivot_longer(-c(intervention, index_fx),
              names_to = "fx", values_to = "count")

a<-a %>% 
  mutate(fx=ifelse(fx=="fx.0", "0",
            ifelse(fx=="fx.1", "1",
            ifelse(fx=="fx.2", "2",
            ifelse(fx=="fx.3plus", "3+", NA))))) %>% 
  mutate(fx=factor(fx,
                  levels=c("0", "1", "2", "3+")))


a<-  a %>% filter(intervention=="FLS") %>% 
left_join(
a %>% filter(intervention=="Current practice"),
by=c("fx","index_fx"))
a$count<-a$count.y-a$count.x

a %>% 
  ggplot()+
  facet_grid(index_fx~intervention)+
  geom_col(aes(fx, count), width=1, colour="grey")+
  scale_y_continuous(labels = percent)+ 
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) +
  ylab("Percentage point difference")+
  xlab("Subsequent fractures")

}else if(input$choose.sec.frac.summary=="By sex"){
n<-sub.frac %>%
  group_by(intervention, sex) %>% 
  tally() 
n<-as.numeric(n[1,3])

a<-sub.frac %>%
  group_by(intervention,sex) %>% 
  summarise(fx.0=sum(sub.frac==0)/n,
            fx.1=sum(sub.frac==1)/n,
            fx.2=sum(sub.frac==2)/n,
            fx.3plus=sum(sub.frac>=3)/n)

a<-a %>%
 pivot_longer(-c(intervention, sex),
              names_to = "fx", values_to = "count")

a<-a %>% 
  mutate(fx=ifelse(fx=="fx.0", "0",
            ifelse(fx=="fx.1", "1",
            ifelse(fx=="fx.2", "2",
            ifelse(fx=="fx.3plus", "3+", NA))))) %>% 
  mutate(fx=factor(fx,
                  levels=c("0", "1", "2", "3+")))


a<-  a %>% filter(intervention=="FLS") %>% 
left_join(
a %>% filter(intervention=="Current practice"),
by=c("fx","sex"))
a$count<-a$count.y-a$count.x

a %>% 
  ggplot()+
  facet_grid(sex~intervention)+
  geom_col(aes(fx, count), width=1, colour="grey")+
  scale_y_continuous(labels = percent)+ 
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) +
  ylab("Percentage point difference")+
  xlab("Subsequent fractures")

  }else{ #"By sentinel fracture and sex"
n<-sub.frac %>%
  group_by(intervention, index_fx, sex) %>% 
  tally() 
n<-as.numeric(n[1,4])

a<-sub.frac %>%
  group_by(intervention,index_fx, sex) %>% 
  summarise(fx.0=sum(sub.frac==0)/n,
            fx.1=sum(sub.frac==1)/n,
            fx.2=sum(sub.frac==2)/n,
            fx.3plus=sum(sub.frac>=3)/n)

a<-a %>%
 pivot_longer(-c(intervention, index_fx, sex),
              names_to = "fx", values_to = "count")

a<-a %>% 
  mutate(fx=ifelse(fx=="fx.0", "0",
            ifelse(fx=="fx.1", "1",
            ifelse(fx=="fx.2", "2",
            ifelse(fx=="fx.3plus", "3+", NA))))) %>% 
  mutate(fx=factor(fx,
                  levels=c("0", "1", "2", "3+")))

a<-  a %>% filter(intervention=="FLS") %>% 
left_join(
a %>% filter(intervention=="Current practice"),
by=c("fx","index_fx","sex"))
a$count<-a$count.y-a$count.x

a %>% 
  ggplot()+
  facet_grid(index_fx+sex~intervention)+
  geom_col(aes(fx, count), width=1, colour="grey")+
  scale_y_continuous(labels = percent)+ 
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) +
  ylab("Percentage point difference")+
  xlab("Subsequent fractures")
  
}
}