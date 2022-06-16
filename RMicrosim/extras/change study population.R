load(paste0(results.folder, "/results.m.TR.only.RData"))


# change study pop size
study_pop_n<-study_pop_n %>% 
  mutate(n=ifelse(names=="spine_fx_n.male",38937,
           ifelse(names=="hip_fx_n.male",51917,
           ifelse(names=="other_fx_n.male",168731,
           ifelse(names=="spine_fx_n.female",111064,
           ifelse(names=="hip_fx_n.female",148083,
           ifelse(names=="other_fx_n.female",481268,
                  NA)))))))


sum(study_pop_n$n)
##               names      n
## 1   spine_fx_n.male  38937
## 2     hip_fx_n.male  51917
## 3   other_fx_n.male 168731
## 4 spine_fx_n.female 111064
## 5   hip_fx_n.female 148083
## 6 other_fx_n.female 481268