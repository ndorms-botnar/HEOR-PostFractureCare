qol.max<-1
qol.min<--0.594

# Worsening in 1 qol associated with fx
hip.fx.popc<-0.65
spine.fx.popc<-0.4634831461
other.fx.popc<-0.3234501348

# Improvement post fracture
# depends on time since fx
hip.fx.porc.2_4<- 0.2272727273
hip.fx.porc.5_12<-0.04464285714
hip.fx.porc.13_18<-0.3888888889
spine.fx.porc.2_4<- 0.2171717172
spine.fx.porc.5_12<-0.05434782609
spine.fx.porc.13_18<-0
other.fx.porc.2_4<- 0.25  
other.fx.porc.5_12<-0.08333333333
other.fx.porc.13_18<-0.75
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


# for subsequent cycles
i<-2
for(i in 2:61){
# first, get max
# if no fracture, max stays as before 
m.TR$qol.max<- ifelse(m.TR$cycle==i,
                      lag(m.TR$qol.max,1), # will replace below if fx
                      m.TR$qol.max)
# if fracture, max is now their qol from the period prior to the fracture
m.TR$qol.max<-ifelse(m.TR$cycle==i & m.TR$c_af==1, 
                     lag(m.TR$qol.current,1), # previous qol
                     m.TR$qol.max)

# next, get current qol
# if fracture in current cycle
# individual worsen for PoPC of fx type
m.TR$qol.current<-ifelse(m.TR$cycle==i & 
                         m.TR$s_hf==1, 
                         lag(m.TR$qol.current,1)-(hip.fx.popc*(lag(m.TR$qol.current,1)-qol.min)),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(m.TR$cycle==i & 
                         m.TR$s_sf==1, 
                         lag(m.TR$qol.current,1)-(spine.fx.popc*(lag(m.TR$qol.current,1)-qol.min)),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(m.TR$cycle==i & 
                         m.TR$s_of==1, 
                         lag(m.TR$qol.current,1)-(other.fx.popc*(lag(m.TR$qol.current,1)-qol.min)),
                         m.TR$qol.current)

# if no fracture in current cycle
# an individual improves based on time since last fracture
# type of last fracture
# the popc associated with their last fracture

# recent.fx=="spine"
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_sf %in% c(1:3)  & 
                         m.TR$recent.fx=="spine", 
                         lag(m.TR$qol.current,1)+
                           (spine.fx.porc.2_4*(m.TR$qol.max-lag(m.TR$qol.current,1))),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_sf %in% c(4:11)  & 
                         m.TR$recent.fx=="spine", 
                         lag(m.TR$qol.current,1)+
                           (spine.fx.porc.5_12*(m.TR$qol.max-lag(m.TR$qol.current,1))),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_sf %in% c(12:17)  & 
                         m.TR$recent.fx=="spine", 
                         lag(m.TR$qol.current,1)+
                           (spine.fx.porc.13_18*(m.TR$qol.max-lag(m.TR$qol.current,1))),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_sf %in% c(18:61)  & 
                         m.TR$recent.fx=="spine", 
                         lag(m.TR$qol.current,1), 
                         m.TR$qol.current)

# recent.fx=="hip"
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_hf%in% c(1:3)  & 
                         m.TR$recent.fx=="hip", 
                         lag(m.TR$qol.current,1)+
                           (hip.fx.porc.2_4*(m.TR$qol.max-lag(m.TR$qol.current,1))),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_hf%in% c(4:11)  & 
                         m.TR$recent.fx=="hip", 
                         lag(m.TR$qol.current,1)+
                           (hip.fx.porc.5_12*(m.TR$qol.max-lag(m.TR$qol.current,1))),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_hf%in% c(12:17)  & 
                         m.TR$recent.fx=="hip", 
                         lag(m.TR$qol.current,1)+
                           (hip.fx.porc.13_18*(m.TR$qol.max-lag(m.TR$qol.current,1))),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_hf%in% c(18:61)  & 
                         m.TR$recent.fx=="hip", 
                         lag(m.TR$qol.current,1), 
                         m.TR$qol.current)

# recent.fx=="other"
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_of%in% c(1:3)  & 
                         m.TR$recent.fx=="other", 
                         lag(m.TR$qol.current,1)+
                           (other.fx.porc.2_4*(m.TR$qol.max-lag(m.TR$qol.current,1))),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_of%in% c(4:11)  & 
                         m.TR$recent.fx=="other", 
                         lag(m.TR$qol.current,1)+
                           (other.fx.porc.5_12*(m.TR$qol.max-lag(m.TR$qol.current,1))),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_of%in% c(12:17)  & 
                         m.TR$recent.fx=="other", 
                         lag(m.TR$qol.current,1)+
                           (other.fx.porc.13_18*(m.TR$qol.max-lag(m.TR$qol.current,1))),
                         m.TR$qol.current)
m.TR$qol.current<-ifelse(!is.na(m.TR$t_since_sf) &
                         m.TR$t_since_of%in% c(18:61)  & 
                         m.TR$recent.fx=="other", 
                         lag(m.TR$qol.current,1), 
                         m.TR$qol.current)

}


          
                         

