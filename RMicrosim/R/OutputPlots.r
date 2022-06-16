

gg.general.format<-function(plot){
  plot+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(labels = label_percent(accuracy = 1L), expand = c(0, 0))+
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

gg.general.format.not.perc<-function(plot){
  plot+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(panel.spacing = unit(1, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) +
  xlab("Month")

}


gg.add.colours.meds<-function(plot){
  plot+
  scale_fill_manual(values=c("grey80","#f0f0f0",
     "#d53e4f", "#f46d43",
              "#fdae61","#fee08b","#e6f598",
               "#abdda4","#66c2a5", "#3288bd","#550307",
              "#6a3d9a"))
}

gg.add.colours.location<-function(plot){
  plot+
  scale_fill_manual(values=c("grey",
    "#fee08b", "#fdae61", "#f46d43",
    "#d53e4f"))
}

gg.add.colours.sub.frac<-function(plot){
  plot+
  scale_fill_manual(values=c(
    "#e41a1c", "#377eb8", "#4daf4a"))
}

gg.add.colours.y_n<-function(plot){
  plot+
  scale_fill_manual(values=c("#f0f0f0", "#b2182b"))
}

gg.add.colours.adh<-function(plot){
  plot+
  scale_fill_manual(values=c("grey80","#f0f0f0", "#b2182b"))
}

#
get.c.inc.plot<-function(m.TR, input){
  
fx.times<-m.TR %>% 
  group_by(id, intervention) %>% 
  filter(month>=1) %>% # exclude index
  filter(c_af==1) %>%  # subs fx
  mutate(seq=1:length(id)) %>% # keep first
  filter(seq==1) %>% 
  select(id, intervention, month) %>% 
  rename(fx.time=month)

death.times<-m.TR %>% 
  group_by(id, intervention) %>% 
  filter(s_d==1) %>%  # subs fx
  mutate(seq=1:length(id)) %>% # keep first
  filter(seq==1) %>% 
  select(id, intervention, month)%>% 
  rename(death.time=month)


##
c.inc<-m.TR  %>% 
  select(id, intervention, index_fx, sex) %>% 
  distinct() %>% 
  left_join(fx.times) %>% 
  left_join(death.times)

# add status
c.inc$sec.frac.status<-ifelse(!is.na(c.inc$fx.time),1,0)
c.inc$death.status<-ifelse(!is.na(c.inc$death.time),1,0)

# add censor date if no event - end of model
c.inc$fx.time<-ifelse(!is.na(c.inc$fx.time),c.inc$fx.time,61)
c.inc$death.time<-ifelse(!is.na(c.inc$death.time),c.inc$death.time,61) 

# competing risk
c.inc <- c.inc %>%
         mutate(fx2_or_death.etime=ifelse(sec.frac.status==0,
                                       death.time, fx.time))
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
c.inc<-c.inc %>% 
  mutate(int=ifelse(intervention=="FLS","FLS", "Current_practice" )) %>% 
  mutate(int=  factor(int,
                levels=c("Current_practice", "FLS")))

# plot sec fx
if(input$choose.sec.frac.summary=="Overall"){
fit <- cuminc(ftime = c.inc$fx2_or_death.etime, 
              fstatus = c.inc$fx2_or_death.event, 
            group = c.inc$int,
            cencode = "censor")
plot.data<-ggcompetingrisks(fit)$data
  
plot.data<-plot.data %>% 
  mutate(group=ifelse(group=="FLS", "FLS", "Current Practice")) %>% 
  mutate(group=factor(group,
                      levels=c( "Current Practice","FLS")))

plot.data<-plot.data %>% 
  mutate(event=ifelse(event=="death", "Death", "Subsequent fracture")) %>% 
  mutate(event=factor(event,
                      levels=c("Subsequent fracture", "Death")))

plot.data %>%
  ggplot(aes(x=time, linetype=group, colour=group))+
  facet_grid(. ~ event)+
  geom_step(aes(y=est)) +
  scale_y_continuous(labels = label_percent(accuracy = 1L),
                     limits=c(0,NA))+
  scale_colour_manual(values = c("black", "red"))+
  xlab("Months") +
  ylab("Cumulative incidence (%)") +
  theme_bw(base_size = 18)  +
  theme(legend.title = element_blank(),
        legend.position = "top")
  
}else if(input$choose.sec.frac.summary=="By sentinel fracture"){

c.inc$int_index_fx<-paste0(c.inc$int,"_", c.inc$index_fx)
fit <- cuminc(ftime = c.inc$fx2_or_death.etime, 
              fstatus = c.inc$fx2_or_death.event, 
            group =c.inc$int_index_fx ,
            cencode = "censor")
plot.data<-ggcompetingrisks(fit)$data
  
plot.data<-plot.data %>%
  mutate(index_fx=
           ifelse(str_detect(group, "Hip"), "Hip",
           ifelse(str_detect(group, "Spine"), "Spine",
                          "Other")))
plot.data<-plot.data %>%
  mutate(intervention=
           ifelse(str_detect(group, "FLS"), "FLS",
                          "Current Practice"))  

plot.data<-plot.data %>% 
  mutate(event=ifelse(event=="death", "Death", "Subsequent fracture")) %>% 
  mutate(event=factor(event,
                      levels=c("Subsequent fracture", "Death")))

plot.data<-plot.data  %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) 

plot.data %>%
    ggplot(aes(x=time, linetype=intervention, colour=intervention))+
  facet_grid(index_fx ~ event)+
  geom_step(aes(y=est)) +
  scale_colour_manual(values = c("black", "red"))+
 scale_y_continuous(labels = label_percent(accuracy = 1L),
                     limits=c(0,NA))+
  xlab("Months") +
  ylab("Cumulative incidence (%)") +
  theme_bw(base_size = 18)  +
  theme(legend.title = element_blank(),
        legend.position = "top")
  

}else if(input$choose.sec.frac.summary=="By sex"){


c.inc$int_sex<-paste0(c.inc$int,"_", c.inc$sex)
fit <- cuminc(ftime = c.inc$fx2_or_death.etime, 
              fstatus = c.inc$fx2_or_death.event, 
            group =c.inc$int_sex ,
            cencode = "censor")
plot.data<-ggcompetingrisks(fit)$data
  
plot.data<-plot.data %>%
  mutate(sex=
           ifelse(str_detect(group, "Female"), "Female",
                          "Male"))
plot.data<-plot.data %>%
  mutate(intervention=
           ifelse(str_detect(group, "FLS"), "FLS",
                          "Current Practice"))  

plot.data<-plot.data %>% 
  mutate(event=ifelse(event=="death", "Death", "Subsequent fracture")) %>% 
  mutate(event=factor(event,
                      levels=c("Subsequent fracture", "Death")))

plot.data<-plot.data  %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))

plot.data %>%
    ggplot(aes(x=time, linetype=intervention, colour=intervention))+
  facet_grid(sex ~ event)+
  geom_step(aes(y=est)) +
  scale_colour_manual(values = c("black", "red"))+
  scale_y_continuous(labels = label_percent(accuracy = 1L),
                     limits=c(0,NA))+
  xlab("Months") +
  ylab("Cumulative incidence (%)") +
  theme_bw(base_size = 18)  +
  theme(legend.title = element_blank(),
        legend.position = "top")
   
  
  
}else{ #"By sentinel fracture and sex"

c.inc$int_index_fx_sex<-paste0(c.inc$int,"_", c.inc$sex, "_", c.inc$index_fx)
fit <- cuminc(ftime = c.inc$fx2_or_death.etime, 
              fstatus = c.inc$fx2_or_death.event, 
            group =c.inc$int_index_fx_sex ,
            cencode = "censor")
plot.data<-ggcompetingrisks(fit)$data
  
plot.data<-plot.data %>%
  mutate(sex=
           ifelse(str_detect(group, "Female"), "Female",
                          "Male")) %>%
  mutate(index_fx=
           ifelse(str_detect(group, "Hip"), "Hip",
           ifelse(str_detect(group, "Spine"), "Spine",
                          "Other")))
plot.data<-plot.data %>%
  mutate(intervention=
           ifelse(str_detect(group, "FLS"), "FLS",
                          "Current Practice"))  

plot.data<-plot.data %>% 
  mutate(event=ifelse(event=="death", "Death", "Subsequent fracture")) %>% 
  mutate(event=factor(event,
                      levels=c("Subsequent fracture", "Death")))

plot.data<-plot.data  %>%
  mutate(index_fx=factor(index_fx, levels = c("Hip", "Spine" , "Other"))) %>%
  mutate(sex=factor(sex, levels = c("Male", "Female" )))

plot.data %>%
    ggplot(aes(x=time, linetype=intervention, colour=intervention))+
  facet_grid(index_fx+sex ~ event)+
  geom_step(aes(y=est)) +
  scale_colour_manual(values = c("black", "red"))+
  scale_y_continuous(labels = label_percent(accuracy = 1L),
                     limits=c(0,NA))+
  xlab("Months") +
  ylab("Cumulative incidence (%)") +
  theme_bw(base_size = 18)  +
  theme(legend.title = element_blank(),
        legend.position = "top")  
  
  
  }

}

gg.general.format.c.inc<-function(plot){
  plot+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(panel.spacing = unit(1, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=18)) + 
  theme(legend.position = "top")

}
