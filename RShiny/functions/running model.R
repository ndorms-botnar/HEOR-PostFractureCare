# FUNCTIONS FOR RUNNING MODEL
# df- transition matrix for a given individual
####################

####################



## sample ----
## get random numbers ----
#id <- showNotification(paste("Random", i), duration = NULL, closeButton = FALSE) 
get.sample<-function(probs){
   # sample(1:length(probs),
   #       replace = TRUE,
   #       n_microsimulation*6,
   #        probs)
 
     sample.int(length(probs),
         replace = TRUE,
         n_microsimulation*6,
          probs)
  
}

get.rmultinorm<-function(input){
  rmultinorm<-NULL
  # browser()
rmultinorm$rmultinorm.no.fls.trt.choice.spine.male<-
 get.sample(c(input$no.fls.trt.alendronate.spine.male,
            input$no.fls.trt.risedronate.spine.male,
            input$no.fls.trt.strontium.spine.male,
            input$no.fls.trt.ibandronate.spine.male,
            input$no.fls.trt.denosumab.spine.male,
            input$no.fls.trt.zoledronate.spine.male,
            input$no.fls.trt.teriparatide.spine.male,
            input$no.fls.trt.abaloparatide.spine.male,
            input$no.fls.trt.romo.spine.male))
rmultinorm$rmultinorm.no.fls.trt.choice.hip.male<-
 get.sample(c(input$no.fls.trt.alendronate.hip.male,
            input$no.fls.trt.risedronate.hip.male,
            input$no.fls.trt.strontium.hip.male,
            input$no.fls.trt.ibandronate.hip.male,
            input$no.fls.trt.denosumab.hip.male,
            input$no.fls.trt.zoledronate.hip.male,
            input$no.fls.trt.teriparatide.hip.male,
            input$no.fls.trt.abaloparatide.hip.male,
            input$no.fls.trt.romo.hip.male))
rmultinorm$rmultinorm.no.fls.trt.choice.other.male<-
 get.sample(c(input$no.fls.trt.alendronate.other.male,
            input$no.fls.trt.risedronate.other.male,
            input$no.fls.trt.strontium.other.male,
            input$no.fls.trt.ibandronate.other.male,
            input$no.fls.trt.denosumab.other.male,
            input$no.fls.trt.zoledronate.other.male,
            input$no.fls.trt.teriparatide.other.male,
            input$no.fls.trt.abaloparatide.other.male,
            input$no.fls.trt.romo.other.male))

rmultinorm$rmultinorm.no.fls.trt.choice.spine.female<-
 get.sample(c(input$no.fls.trt.alendronate.spine.female,
            input$no.fls.trt.risedronate.spine.female,
            input$no.fls.trt.strontium.spine.female,
            input$no.fls.trt.ibandronate.spine.female,
            input$no.fls.trt.denosumab.spine.female,
            input$no.fls.trt.zoledronate.spine.female,
            input$no.fls.trt.teriparatide.spine.female,
            input$no.fls.trt.abaloparatide.spine.female,
            input$no.fls.trt.romo.spine.female))
rmultinorm$rmultinorm.no.fls.trt.choice.hip.female<-
 get.sample(c(input$no.fls.trt.alendronate.hip.female,
            input$no.fls.trt.risedronate.hip.female,
            input$no.fls.trt.strontium.hip.female,
            input$no.fls.trt.ibandronate.hip.female,
            input$no.fls.trt.denosumab.hip.female,
            input$no.fls.trt.zoledronate.hip.female,
            input$no.fls.trt.teriparatide.hip.female,
            input$no.fls.trt.abaloparatide.hip.female,
            input$no.fls.trt.romo.hip.female))
rmultinorm$rmultinorm.no.fls.trt.choice.other.female<-
 get.sample(c(input$no.fls.trt.alendronate.other.female,
            input$no.fls.trt.risedronate.other.female,
            input$no.fls.trt.strontium.other.female,
            input$no.fls.trt.ibandronate.other.female,
            input$no.fls.trt.denosumab.other.female,
            input$no.fls.trt.zoledronate.other.female,
            input$no.fls.trt.teriparatide.other.female,
            input$no.fls.trt.abaloparatide.other.female,
            input$no.fls.trt.romo.other.female))

rmultinorm$rmultinorm.fls.trt.choice.spine.male<-
 get.sample(c(input$fls.trt.alendronate.spine.male,
            input$fls.trt.risedronate.spine.male,
            input$fls.trt.strontium.spine.male,
            input$fls.trt.ibandronate.spine.male,
            input$fls.trt.denosumab.spine.male,
            input$fls.trt.zoledronate.spine.male,
            input$fls.trt.teriparatide.spine.male,
            input$fls.trt.abaloparatide.spine.male,
            input$fls.trt.romo.spine.male))
rmultinorm$rmultinorm.fls.trt.choice.hip.male<-
 get.sample(c(input$fls.trt.alendronate.hip.male,
            input$fls.trt.risedronate.hip.male,
            input$fls.trt.strontium.hip.male,
            input$fls.trt.ibandronate.hip.male,
            input$fls.trt.denosumab.hip.male,
            input$fls.trt.zoledronate.hip.male,
            input$fls.trt.teriparatide.hip.male,
            input$fls.trt.abaloparatide.hip.male,
            input$fls.trt.romo.hip.male))
rmultinorm$rmultinorm.fls.trt.choice.other.male<-
 get.sample(c(input$fls.trt.alendronate.other.male,
            input$fls.trt.risedronate.other.male,
            input$fls.trt.strontium.other.male,
            input$fls.trt.ibandronate.other.male,
            input$fls.trt.denosumab.other.male,
            input$fls.trt.zoledronate.other.male,
            input$fls.trt.teriparatide.other.male,
            input$fls.trt.abaloparatide.other.male,
            input$fls.trt.romo.other.male))

rmultinorm$rmultinorm.fls.trt.choice.spine.female<-
 get.sample(c(input$fls.trt.alendronate.spine.female,
            input$fls.trt.risedronate.spine.female,
            input$fls.trt.strontium.spine.female,
            input$fls.trt.ibandronate.spine.female,
            input$fls.trt.denosumab.spine.female,
            input$fls.trt.zoledronate.spine.female,
            input$fls.trt.teriparatide.spine.female,
            input$fls.trt.abaloparatide.spine.female,
            input$fls.trt.romo.spine.female))
rmultinorm$rmultinorm.fls.trt.choice.hip.female<-
 get.sample(c(input$fls.trt.alendronate.hip.female,
            input$fls.trt.risedronate.hip.female,
            input$fls.trt.strontium.hip.female,
            input$fls.trt.ibandronate.hip.female,
            input$fls.trt.denosumab.hip.female,
            input$fls.trt.zoledronate.hip.female,
            input$fls.trt.teriparatide.hip.female,
            input$fls.trt.abaloparatide.hip.female,
            input$fls.trt.romo.hip.female))
rmultinorm$rmultinorm.fls.trt.choice.other.female<-
 get.sample(c(input$fls.trt.alendronate.other.female,
            input$fls.trt.risedronate.other.female,
            input$fls.trt.strontium.other.female,
            input$fls.trt.ibandronate.other.female,
            input$fls.trt.denosumab.other.female,
            input$fls.trt.zoledronate.other.female,
            input$fls.trt.teriparatide.other.female,
            input$fls.trt.abaloparatide.other.female,
            input$fls.trt.romo.other.female))





rmultinorm$rmultinorm.no.fls.prob_identification.hip<-
 get.sample(c(1-input$no.fls.prob_identification.hip,
            input$no.fls.prob_identification.hip))
rmultinorm$rmultinorm.no.fls.prob_identification.spine<-
 get.sample(c(1-input$no.fls.prob_identification.spine,
            input$no.fls.prob_identification.spine))
rmultinorm$rmultinorm.no.fls.prob_identification.other<-
 get.sample(c(1-input$no.fls.prob_identification.other,
            input$no.fls.prob_identification.other))

rmultinorm$rmultinorm.fls.prob_identification.hip<-
 get.sample(c(1-input$fls.prob_identification.hip,
            input$fls.prob_identification.hip))
rmultinorm$rmultinorm.fls.prob_identification.spine<-
 get.sample(c(1-input$fls.prob_identification.spine,
            input$fls.prob_identification.spine))
rmultinorm$rmultinorm.fls.prob_identification.other<-
 get.sample(c(1-input$fls.prob_identification.other,
            input$fls.prob_identification.other))


#
rmultinorm$rmultinorm.no.fls.trt.spine.male<-
 get.sample(c(1-input$no.fls.trt.spine.male,
            input$no.fls.trt.spine.male))
rmultinorm$rmultinorm.no.fls.trt.hip.male<-
 get.sample(c(1-input$no.fls.trt.hip.male,
            input$no.fls.trt.hip.male))
rmultinorm$rmultinorm.no.fls.trt.other.male<-
 get.sample(c(1-input$no.fls.trt.other.male,
            input$no.fls.trt.other.male))
rmultinorm$rmultinorm.no.fls.trt.spine.female<-
 get.sample(c(1-input$no.fls.trt.spine.female,
            input$no.fls.trt.spine.female))
rmultinorm$rmultinorm.no.fls.trt.hip.female<-
 get.sample(c(1-input$no.fls.trt.hip.female,
            input$no.fls.trt.hip.female))
rmultinorm$rmultinorm.no.fls.trt.other.female<-
 get.sample(c(1-input$no.fls.trt.other.female,
            input$no.fls.trt.other.female))

rmultinorm$rmultinorm.fls.trt.spine.male<-
 get.sample(c(1-input$fls.trt.spine.male,
            input$fls.trt.spine.male))
rmultinorm$rmultinorm.fls.trt.hip.male<-
 get.sample(c(1-input$fls.trt.hip.male,
            input$fls.trt.hip.male))
rmultinorm$rmultinorm.fls.trt.other.male<-
 get.sample(c(1-input$fls.trt.other.male,
            input$fls.trt.other.male))
rmultinorm$rmultinorm.fls.trt.spine.female<-
 get.sample(c(1-input$fls.trt.spine.female,
            input$fls.trt.spine.female))
rmultinorm$rmultinorm.fls.trt.hip.female<-
 get.sample(c(1-input$fls.trt.hip.female,
            input$fls.trt.hip.female))
rmultinorm$rmultinorm.fls.trt.other.female<-
 get.sample(c(1-input$fls.trt.other.female,
            input$fls.trt.other.female))
##


rmultinorm$rmultinorm.romo.to.no.fls.male<-
 get.sample(c(
            input$romo.to.nothing.no.fls.male,
            input$romo.to.alendronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.risedronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.strontium.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.ibandronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.denosumab.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.zoledronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male)))
rmultinorm$rmultinorm.romo.to.no.fls.female<-
 get.sample(c(
            input$romo.to.nothing.no.fls.female,
            input$romo.to.alendronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.risedronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.strontium.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.ibandronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.denosumab.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.zoledronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female)))

rmultinorm$rmultinorm.romo.to.fls.male<-
 get.sample(c(
            input$romo.to.nothing.fls.male,
            input$romo.to.alendronate.fls.male*(1-input$romo.to.nothing.fls.male),
            input$romo.to.risedronate.fls.male*(1-input$romo.to.nothing.fls.male),
            input$romo.to.strontium.fls.male*(1-input$romo.to.nothing.fls.male),
            input$romo.to.ibandronate.fls.male*(1-input$romo.to.nothing.fls.male),
            input$romo.to.denosumab.fls.male*(1-input$romo.to.nothing.fls.male),
            input$romo.to.zoledronate.fls.male*(1-input$romo.to.nothing.fls.male)))
rmultinorm$rmultinorm.romo.to.fls.female<-
 get.sample(c(
            input$romo.to.nothing.fls.female,
            input$romo.to.alendronate.fls.female*(1-input$romo.to.nothing.fls.female),
            input$romo.to.risedronate.fls.female*(1-input$romo.to.nothing.fls.female),
            input$romo.to.strontium.fls.female*(1-input$romo.to.nothing.fls.female),
            input$romo.to.ibandronate.fls.female*(1-input$romo.to.nothing.fls.female),
            input$romo.to.denosumab.fls.female*(1-input$romo.to.nothing.fls.female),
            input$romo.to.zoledronate.fls.female*(1-input$romo.to.nothing.fls.female)))

# 
rmultinorm$rmultinorm.abaloparatide.to.no.fls.male<-
 get.sample(c(
            input$abaloparatide.to.nothing.no.fls.male,
            input$abaloparatide.to.alendronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.risedronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.strontium.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.ibandronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.denosumab.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.zoledronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male)))
rmultinorm$rmultinorm.abaloparatide.to.no.fls.female<-
 get.sample(c(
            input$abaloparatide.to.nothing.no.fls.female,
            input$abaloparatide.to.alendronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.risedronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.strontium.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.ibandronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.denosumab.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.zoledronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female)))

rmultinorm$rmultinorm.abaloparatide.to.fls.male<-
 get.sample(c(
            input$abaloparatide.to.nothing.fls.male,
            input$abaloparatide.to.alendronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.risedronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.strontium.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.ibandronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.denosumab.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.zoledronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male)))
rmultinorm$rmultinorm.abaloparatide.to.fls.female<-
 get.sample(c(
            input$abaloparatide.to.nothing.fls.female,
            input$abaloparatide.to.alendronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.risedronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.strontium.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.ibandronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.denosumab.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.zoledronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female)))



rmultinorm$rmultinorm.teriparatide.to.no.fls.male<-
 get.sample(c(
            input$teriparatide.to.nothing.no.fls.male,
            input$teriparatide.to.alendronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.risedronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.strontium.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.ibandronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.denosumab.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.zoledronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male)))
rmultinorm$rmultinorm.teriparatide.to.no.fls.female<-
 get.sample(c(
            input$teriparatide.to.nothing.no.fls.female,
            input$teriparatide.to.alendronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.risedronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.strontium.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.ibandronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.denosumab.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.zoledronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female)))

rmultinorm$rmultinorm.teriparatide.to.fls.male<-
 get.sample(c(
            input$teriparatide.to.nothing.fls.male,
            input$teriparatide.to.alendronate.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.risedronate.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.strontium.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.ibandronate.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.denosumab.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.zoledronate.fls.male*(1-input$teriparatide.to.nothing.fls.male)))
rmultinorm$rmultinorm.teriparatide.to.fls.female<-
 get.sample(c(
            input$teriparatide.to.nothing.fls.female,
            input$teriparatide.to.alendronate.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.risedronate.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.strontium.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.ibandronate.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.denosumab.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.zoledronate.fls.female*(1-input$teriparatide.to.nothing.fls.female)))



# 
rmultinorm$rmultinorm.no.fls.monitored.spine.4m.male<-
 get.sample(c(1-input$no.fls.monitored.spine.4m.male,
              input$no.fls.monitored.spine.4m.male))
rmultinorm$rmultinorm.no.fls.monitored.hip.4m.male<-
 get.sample(c(1-input$no.fls.monitored.hip.4m.male,
              input$no.fls.monitored.hip.4m.male))
rmultinorm$rmultinorm.no.fls.monitored.other.4m.male<-
 get.sample(c(1-input$no.fls.monitored.other.4m.male,
              input$no.fls.monitored.other.4m.male))

rmultinorm$rmultinorm.no.fls.monitored.spine.4m.female<-
 get.sample(c(1-input$no.fls.monitored.spine.4m.female,
              input$no.fls.monitored.spine.4m.female))
rmultinorm$rmultinorm.no.fls.monitored.hip.4m.female<-
 get.sample(c(1-input$no.fls.monitored.hip.4m.female,
              input$no.fls.monitored.hip.4m.female))
rmultinorm$rmultinorm.no.fls.monitored.other.4m.female<-
 get.sample(c(1-input$no.fls.monitored.other.4m.female,
              input$no.fls.monitored.other.4m.female))



rmultinorm$rmultinorm.fls.monitored.spine.4m.male<-
 get.sample(c(1-input$fls.monitored.spine.4m.male,
              input$fls.monitored.spine.4m.male))
rmultinorm$rmultinorm.fls.monitored.hip.4m.male<-
 get.sample(c(1-input$fls.monitored.hip.4m.male,
              input$fls.monitored.hip.4m.male))
rmultinorm$rmultinorm.fls.monitored.other.4m.male<-
 get.sample(c(1-input$fls.monitored.other.4m.male,
              input$fls.monitored.other.4m.male))

rmultinorm$rmultinorm.fls.monitored.spine.4m.female<-
 get.sample(c(1-input$fls.monitored.spine.4m.female,
              input$fls.monitored.spine.4m.female))
rmultinorm$rmultinorm.fls.monitored.hip.4m.female<-
 get.sample(c(1-input$fls.monitored.hip.4m.female,
              input$fls.monitored.hip.4m.female))
rmultinorm$rmultinorm.fls.monitored.other.4m.female<-
 get.sample(c(1-input$fls.monitored.other.4m.female,
              input$fls.monitored.other.4m.female))




rmultinorm$rmultinorm.no.fls.monitored.spine.12m.male<-
 get.sample(c(1-input$no.fls.monitored.spine.12m.male,
              input$no.fls.monitored.spine.12m.male))
rmultinorm$rmultinorm.no.fls.monitored.hip.12m.male<-
 get.sample(c(1-input$no.fls.monitored.hip.12m.male,
              input$no.fls.monitored.hip.12m.male))
rmultinorm$rmultinorm.no.fls.monitored.other.12m.male<-
 get.sample(c(1-input$no.fls.monitored.other.12m.male,
              input$no.fls.monitored.other.12m.male))

rmultinorm$rmultinorm.no.fls.monitored.spine.12m.female<-
 get.sample(c(1-input$no.fls.monitored.spine.12m.female,
              input$no.fls.monitored.spine.12m.female))
rmultinorm$rmultinorm.no.fls.monitored.hip.12m.female<-
 get.sample(c(1-input$no.fls.monitored.hip.12m.female,
              input$no.fls.monitored.hip.12m.female))
rmultinorm$rmultinorm.no.fls.monitored.other.12m.female<-
 get.sample(c(1-input$no.fls.monitored.other.12m.female,
              input$no.fls.monitored.other.12m.female))



rmultinorm$rmultinorm.fls.monitored.spine.12m.male<-
 get.sample(c(1-input$fls.monitored.spine.12m.male,
              input$fls.monitored.spine.12m.male))
rmultinorm$rmultinorm.fls.monitored.hip.12m.male<-
 get.sample(c(1-input$fls.monitored.hip.12m.male,
              input$fls.monitored.hip.12m.male))
rmultinorm$rmultinorm.fls.monitored.other.12m.male<-
 get.sample(c(1-input$fls.monitored.other.12m.male,
              input$fls.monitored.other.12m.male))

rmultinorm$rmultinorm.fls.monitored.spine.12m.female<-
 get.sample(c(1-input$fls.monitored.spine.12m.female,
              input$fls.monitored.spine.12m.female))
rmultinorm$rmultinorm.fls.monitored.hip.12m.female<-
 get.sample(c(1-input$fls.monitored.hip.12m.female,
              input$fls.monitored.hip.12m.female))
rmultinorm$rmultinorm.fls.monitored.other.12m.female<-
 get.sample(c(1-input$fls.monitored.other.12m.female,
              input$fls.monitored.other.12m.female))










#
rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_alendronate.male,
              input$not.monitored.4m_adh_alendronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_risedronate.male,
              input$not.monitored.4m_adh_risedronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.male<-
 get.sample(c(1-input$not.monitored.4m_adh_strontium.male,
              input$not.monitored.4m_adh_strontium.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_ibandronate.male,
              input$not.monitored.4m_adh_ibandronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.male<-
 get.sample(c(1-input$not.monitored.4m_adh_denosumab.male,
              input$not.monitored.4m_adh_denosumab.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_zoledronate.male,
              input$not.monitored.4m_adh_zoledronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.male<-
 get.sample(c(1-input$not.monitored.4m_adh_teriparatide.male,
              input$not.monitored.4m_adh_teriparatide.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.male<-
 get.sample(c(1-input$not.monitored.4m_adh_abaloparatide.male,
              input$not.monitored.4m_adh_abaloparatide.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_romo.male<-
 get.sample(c(1-input$not.monitored.4m_adh_romo.male,
              input$not.monitored.4m_adh_romo.male))


rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_alendronate.male,
              input$not.monitored.12m_adh_alendronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_risedronate.male,
              input$not.monitored.12m_adh_risedronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.male<-
 get.sample(c(1-input$not.monitored.12m_adh_strontium.male,
              input$not.monitored.12m_adh_strontium.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_ibandronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_ibandronate.male,
              input$not.monitored.12m_adh_ibandronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.male<-
 get.sample(c(1-input$not.monitored.12m_adh_denosumab.male,
              input$not.monitored.12m_adh_denosumab.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_zoledronate.male,
              input$not.monitored.12m_adh_zoledronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.male<-
 get.sample(c(1-input$not.monitored.12m_adh_teriparatide.male,
              input$not.monitored.12m_adh_teriparatide.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.male<-
 get.sample(c(1-input$not.monitored.12m_adh_abaloparatide.male,
              input$not.monitored.12m_adh_abaloparatide.male))



rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_alendronate.female,
              input$not.monitored.4m_adh_alendronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_risedronate.female,
              input$not.monitored.4m_adh_risedronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.female<-
 get.sample(c(1-input$not.monitored.4m_adh_strontium.female,
              input$not.monitored.4m_adh_strontium.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_ibandronate.female,
              input$not.monitored.4m_adh_ibandronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.female<-
 get.sample(c(1-input$not.monitored.4m_adh_denosumab.female,
              input$not.monitored.4m_adh_denosumab.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_zoledronate.female,
              input$not.monitored.4m_adh_zoledronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.female<-
 get.sample(c(1-input$not.monitored.4m_adh_teriparatide.female,
              input$not.monitored.4m_adh_teriparatide.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.female<-
 get.sample(c(1-input$not.monitored.4m_adh_abaloparatide.female,
              input$not.monitored.4m_adh_abaloparatide.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_romo.female<-
 get.sample(c(1-input$not.monitored.4m_adh_romo.female,
              input$not.monitored.4m_adh_romo.female))


rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_alendronate.female,
              input$not.monitored.12m_adh_alendronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_risedronate.female,
              input$not.monitored.12m_adh_risedronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.female<-
 get.sample(c(1-input$not.monitored.12m_adh_strontium.female,
              input$not.monitored.12m_adh_strontium.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_ibandronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_ibandronate.female,
              input$not.monitored.12m_adh_ibandronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.female<-
 get.sample(c(1-input$not.monitored.12m_adh_denosumab.female,
              input$not.monitored.12m_adh_denosumab.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_zoledronate.female,
              input$not.monitored.12m_adh_zoledronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.female<-
 get.sample(c(1-input$not.monitored.12m_adh_teriparatide.female,
              input$not.monitored.12m_adh_teriparatide.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.female<-
 get.sample(c(1-input$not.monitored.12m_adh_abaloparatide.female,
              input$not.monitored.12m_adh_abaloparatide.female))









rmultinorm$rmultinorm.monitored.4m_adh_alendronate.male<-
 get.sample(c(1-input$monitored.4m_adh_alendronate.male,
              input$monitored.4m_adh_alendronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_risedronate.male<-
 get.sample(c(1-input$monitored.4m_adh_risedronate.male,
              input$monitored.4m_adh_risedronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_strontium.male<-
 get.sample(c(1-input$monitored.4m_adh_strontium.male,
              input$monitored.4m_adh_strontium.male))
rmultinorm$rmultinorm.monitored.4m_adh_ibandronate.male<-
 get.sample(c(1-input$monitored.4m_adh_ibandronate.male,
              input$monitored.4m_adh_ibandronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_denosumab.male<-
 get.sample(c(1-input$monitored.4m_adh_denosumab.male,
              input$monitored.4m_adh_denosumab.male))
rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.male<-
 get.sample(c(1-input$monitored.4m_adh_zoledronate.male,
              input$monitored.4m_adh_zoledronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.male<-
 get.sample(c(1-input$monitored.4m_adh_teriparatide.male,
              input$monitored.4m_adh_teriparatide.male))
rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.male<-
 get.sample(c(1-input$monitored.4m_adh_abaloparatide.male,
              input$monitored.4m_adh_abaloparatide.male))
rmultinorm$rmultinorm.monitored.4m_adh_romo.male<-
 get.sample(c(1-input$monitored.4m_adh_romo.male,
              input$monitored.4m_adh_romo.male))


rmultinorm$rmultinorm.monitored.12m_adh_alendronate.male<-
 get.sample(c(1-input$monitored.12m_adh_alendronate.male,
              input$monitored.12m_adh_alendronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_risedronate.male<-
 get.sample(c(1-input$monitored.12m_adh_risedronate.male,
              input$monitored.12m_adh_risedronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_strontium.male<-
 get.sample(c(1-input$monitored.12m_adh_strontium.male,
              input$monitored.12m_adh_strontium.male))
rmultinorm$rmultinorm.monitored.12m_adh_ibandronate.male<-
 get.sample(c(1-input$monitored.12m_adh_ibandronate.male,
              input$monitored.12m_adh_ibandronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_denosumab.male<-
 get.sample(c(1-input$monitored.12m_adh_denosumab.male,
              input$monitored.12m_adh_denosumab.male))
rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.male<-
 get.sample(c(1-input$monitored.12m_adh_zoledronate.male,
              input$monitored.12m_adh_zoledronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.male<-
 get.sample(c(1-input$monitored.12m_adh_teriparatide.male,
              input$monitored.12m_adh_teriparatide.male))
rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.male<-
 get.sample(c(1-input$monitored.12m_adh_abaloparatide.male,
              input$monitored.12m_adh_abaloparatide.male))



rmultinorm$rmultinorm.monitored.4m_adh_alendronate.female<-
 get.sample(c(1-input$monitored.4m_adh_alendronate.female,
              input$monitored.4m_adh_alendronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_risedronate.female<-
 get.sample(c(1-input$monitored.4m_adh_risedronate.female,
              input$monitored.4m_adh_risedronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_strontium.female<-
 get.sample(c(1-input$monitored.4m_adh_strontium.female,
              input$monitored.4m_adh_strontium.female))
rmultinorm$rmultinorm.monitored.4m_adh_ibandronate.female<-
 get.sample(c(1-input$monitored.4m_adh_ibandronate.female,
              input$monitored.4m_adh_ibandronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_denosumab.female<-
 get.sample(c(1-input$monitored.4m_adh_denosumab.female,
              input$monitored.4m_adh_denosumab.female))
rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.female<-
 get.sample(c(1-input$monitored.4m_adh_zoledronate.female,
              input$monitored.4m_adh_zoledronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.female<-
 get.sample(c(1-input$monitored.4m_adh_teriparatide.female,
              input$monitored.4m_adh_teriparatide.female))
rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.female<-
 get.sample(c(1-input$monitored.4m_adh_abaloparatide.female,
              input$monitored.4m_adh_abaloparatide.female))
rmultinorm$rmultinorm.monitored.4m_adh_romo.female<-
 get.sample(c(1-input$monitored.4m_adh_romo.female,
              input$monitored.4m_adh_romo.female))


rmultinorm$rmultinorm.monitored.12m_adh_alendronate.female<-
 get.sample(c(1-input$monitored.12m_adh_alendronate.female,
              input$monitored.12m_adh_alendronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_risedronate.female<-
 get.sample(c(1-input$monitored.12m_adh_risedronate.female,
              input$monitored.12m_adh_risedronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_strontium.female<-
 get.sample(c(1-input$monitored.12m_adh_strontium.female,
              input$monitored.12m_adh_strontium.female))
rmultinorm$rmultinorm.monitored.12m_adh_ibandronate.female<-
 get.sample(c(1-input$monitored.12m_adh_ibandronate.female,
              input$monitored.12m_adh_ibandronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_denosumab.female<-
 get.sample(c(1-input$monitored.12m_adh_denosumab.female,
              input$monitored.12m_adh_denosumab.female))
rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.female<-
 get.sample(c(1-input$monitored.12m_adh_zoledronate.female,
              input$monitored.12m_adh_zoledronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.female<-
 get.sample(c(1-input$monitored.12m_adh_teriparatide.female,
              input$monitored.12m_adh_teriparatide.female))
rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.female<-
 get.sample(c(1-input$monitored.12m_adh_abaloparatide.female,
              input$monitored.12m_adh_abaloparatide.female))

#


rmultinorm$rmultinorm.primary.adh_alendronate.spine.no.fls.male<-
 get.sample(c(1-input$primary.adh_alendronate.spine.no.fls.male,
              input$primary.adh_alendronate.spine.no.fls.male))
rmultinorm$rmultinorm.primary.adh_risedronate.spine.no.fls.male<-
 get.sample(c(1-input$primary.adh_risedronate.spine.no.fls.male,
              input$primary.adh_risedronate.spine.no.fls.male))
rmultinorm$rmultinorm.primary.adh_strontium.spine.no.fls.male<-
 get.sample(c(1-input$primary.adh_strontium.spine.no.fls.male,
              input$primary.adh_strontium.spine.no.fls.male))
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.no.fls.male<-
 get.sample(c(1-input$primary.adh_ibandronate.spine.no.fls.male,
              input$primary.adh_ibandronate.spine.no.fls.male))
rmultinorm$rmultinorm.primary.adh_denosumab.spine.no.fls.male<-
 get.sample(c(1-input$primary.adh_denosumab.spine.no.fls.male,
              input$primary.adh_denosumab.spine.no.fls.male))
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.no.fls.male<-
 get.sample(c(1-input$primary.adh_zoledronate.spine.no.fls.male,
              input$primary.adh_zoledronate.spine.no.fls.male))
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.no.fls.male<-
 get.sample(c(1-input$primary.adh_teriparatide.spine.no.fls.male,
              input$primary.adh_teriparatide.spine.no.fls.male))
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.no.fls.male<-
 get.sample(c(1-input$primary.adh_abaloparatide.spine.no.fls.male,
              input$primary.adh_abaloparatide.spine.no.fls.male))
rmultinorm$rmultinorm.primary.adh_romo.spine.no.fls.male<-
 get.sample(c(1-input$primary.adh_romo.spine.no.fls.male,
              input$primary.adh_romo.spine.no.fls.male))


rmultinorm$rmultinorm.primary.adh_alendronate.hip.no.fls.male<-
 get.sample(c(1-input$primary.adh_alendronate.hip.no.fls.male,
              input$primary.adh_alendronate.hip.no.fls.male))
rmultinorm$rmultinorm.primary.adh_risedronate.hip.no.fls.male<-
 get.sample(c(1-input$primary.adh_risedronate.hip.no.fls.male,
              input$primary.adh_risedronate.hip.no.fls.male))
rmultinorm$rmultinorm.primary.adh_strontium.hip.no.fls.male<-
 get.sample(c(1-input$primary.adh_strontium.hip.no.fls.male,
              input$primary.adh_strontium.hip.no.fls.male))
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.no.fls.male<-
 get.sample(c(1-input$primary.adh_ibandronate.hip.no.fls.male,
              input$primary.adh_ibandronate.hip.no.fls.male))
rmultinorm$rmultinorm.primary.adh_denosumab.hip.no.fls.male<-
 get.sample(c(1-input$primary.adh_denosumab.hip.no.fls.male,
              input$primary.adh_denosumab.hip.no.fls.male))
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.no.fls.male<-
 get.sample(c(1-input$primary.adh_zoledronate.hip.no.fls.male,
              input$primary.adh_zoledronate.hip.no.fls.male))
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.no.fls.male<-
 get.sample(c(1-input$primary.adh_teriparatide.hip.no.fls.male,
              input$primary.adh_teriparatide.hip.no.fls.male))
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.no.fls.male<-
 get.sample(c(1-input$primary.adh_abaloparatide.hip.no.fls.male,
              input$primary.adh_abaloparatide.hip.no.fls.male))
rmultinorm$rmultinorm.primary.adh_romo.hip.no.fls.male<-
 get.sample(c(1-input$primary.adh_romo.hip.no.fls.male,
              input$primary.adh_romo.hip.no.fls.male))



rmultinorm$rmultinorm.primary.adh_alendronate.other.no.fls.male<-
 get.sample(c(1-input$primary.adh_alendronate.other.no.fls.male,
              input$primary.adh_alendronate.other.no.fls.male))
rmultinorm$rmultinorm.primary.adh_risedronate.other.no.fls.male<-
 get.sample(c(1-input$primary.adh_risedronate.other.no.fls.male,
              input$primary.adh_risedronate.other.no.fls.male))
rmultinorm$rmultinorm.primary.adh_strontium.other.no.fls.male<-
 get.sample(c(1-input$primary.adh_strontium.other.no.fls.male,
              input$primary.adh_strontium.other.no.fls.male))
rmultinorm$rmultinorm.primary.adh_ibandronate.other.no.fls.male<-
 get.sample(c(1-input$primary.adh_ibandronate.other.no.fls.male,
              input$primary.adh_ibandronate.other.no.fls.male))
rmultinorm$rmultinorm.primary.adh_denosumab.other.no.fls.male<-
 get.sample(c(1-input$primary.adh_denosumab.other.no.fls.male,
              input$primary.adh_denosumab.other.no.fls.male))
rmultinorm$rmultinorm.primary.adh_zoledronate.other.no.fls.male<-
 get.sample(c(1-input$primary.adh_zoledronate.other.no.fls.male,
              input$primary.adh_zoledronate.other.no.fls.male))
rmultinorm$rmultinorm.primary.adh_teriparatide.other.no.fls.male<-
 get.sample(c(1-input$primary.adh_teriparatide.other.no.fls.male,
              input$primary.adh_teriparatide.other.no.fls.male))
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.no.fls.male<-
 get.sample(c(1-input$primary.adh_abaloparatide.other.no.fls.male,
              input$primary.adh_abaloparatide.other.no.fls.male))
rmultinorm$rmultinorm.primary.adh_romo.other.no.fls.male<-
 get.sample(c(1-input$primary.adh_romo.other.no.fls.male,
              input$primary.adh_romo.other.no.fls.male))




rmultinorm$rmultinorm.primary.adh_alendronate.spine.no.fls.female<-
 get.sample(c(1-input$primary.adh_alendronate.spine.no.fls.female,
              input$primary.adh_alendronate.spine.no.fls.female))
rmultinorm$rmultinorm.primary.adh_risedronate.spine.no.fls.female<-
 get.sample(c(1-input$primary.adh_risedronate.spine.no.fls.female,
              input$primary.adh_risedronate.spine.no.fls.female))
rmultinorm$rmultinorm.primary.adh_strontium.spine.no.fls.female<-
 get.sample(c(1-input$primary.adh_strontium.spine.no.fls.female,
              input$primary.adh_strontium.spine.no.fls.female))
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.no.fls.female<-
 get.sample(c(1-input$primary.adh_ibandronate.spine.no.fls.female,
              input$primary.adh_ibandronate.spine.no.fls.female))
rmultinorm$rmultinorm.primary.adh_denosumab.spine.no.fls.female<-
 get.sample(c(1-input$primary.adh_denosumab.spine.no.fls.female,
              input$primary.adh_denosumab.spine.no.fls.female))
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.no.fls.female<-
 get.sample(c(1-input$primary.adh_zoledronate.spine.no.fls.female,
              input$primary.adh_zoledronate.spine.no.fls.female))
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.no.fls.female<-
 get.sample(c(1-input$primary.adh_teriparatide.spine.no.fls.female,
              input$primary.adh_teriparatide.spine.no.fls.female))
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.no.fls.female<-
 get.sample(c(1-input$primary.adh_abaloparatide.spine.no.fls.female,
              input$primary.adh_abaloparatide.spine.no.fls.female))
rmultinorm$rmultinorm.primary.adh_romo.spine.no.fls.female<-
 get.sample(c(1-input$primary.adh_romo.spine.no.fls.female,
              input$primary.adh_romo.spine.no.fls.female))


rmultinorm$rmultinorm.primary.adh_alendronate.hip.no.fls.female<-
 get.sample(c(1-input$primary.adh_alendronate.hip.no.fls.female,
              input$primary.adh_alendronate.hip.no.fls.female))
rmultinorm$rmultinorm.primary.adh_risedronate.hip.no.fls.female<-
 get.sample(c(1-input$primary.adh_risedronate.hip.no.fls.female,
              input$primary.adh_risedronate.hip.no.fls.female))
rmultinorm$rmultinorm.primary.adh_strontium.hip.no.fls.female<-
 get.sample(c(1-input$primary.adh_strontium.hip.no.fls.female,
              input$primary.adh_strontium.hip.no.fls.female))
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.no.fls.female<-
 get.sample(c(1-input$primary.adh_ibandronate.hip.no.fls.female,
              input$primary.adh_ibandronate.hip.no.fls.female))
rmultinorm$rmultinorm.primary.adh_denosumab.hip.no.fls.female<-
 get.sample(c(1-input$primary.adh_denosumab.hip.no.fls.female,
              input$primary.adh_denosumab.hip.no.fls.female))
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.no.fls.female<-
 get.sample(c(1-input$primary.adh_zoledronate.hip.no.fls.female,
              input$primary.adh_zoledronate.hip.no.fls.female))
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.no.fls.female<-
 get.sample(c(1-input$primary.adh_teriparatide.hip.no.fls.female,
              input$primary.adh_teriparatide.hip.no.fls.female))
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.no.fls.female<-
 get.sample(c(1-input$primary.adh_abaloparatide.hip.no.fls.female,
              input$primary.adh_abaloparatide.hip.no.fls.female))
rmultinorm$rmultinorm.primary.adh_romo.hip.no.fls.female<-
 get.sample(c(1-input$primary.adh_romo.hip.no.fls.female,
              input$primary.adh_romo.hip.no.fls.female))



rmultinorm$rmultinorm.primary.adh_alendronate.other.no.fls.female<-
 get.sample(c(1-input$primary.adh_alendronate.other.no.fls.female,
              input$primary.adh_alendronate.other.no.fls.female))
rmultinorm$rmultinorm.primary.adh_risedronate.other.no.fls.female<-
 get.sample(c(1-input$primary.adh_risedronate.other.no.fls.female,
              input$primary.adh_risedronate.other.no.fls.female))
rmultinorm$rmultinorm.primary.adh_strontium.other.no.fls.female<-
 get.sample(c(1-input$primary.adh_strontium.other.no.fls.female,
              input$primary.adh_strontium.other.no.fls.female))
rmultinorm$rmultinorm.primary.adh_ibandronate.other.no.fls.female<-
 get.sample(c(1-input$primary.adh_ibandronate.other.no.fls.female,
              input$primary.adh_ibandronate.other.no.fls.female))
rmultinorm$rmultinorm.primary.adh_denosumab.other.no.fls.female<-
 get.sample(c(1-input$primary.adh_denosumab.other.no.fls.female,
              input$primary.adh_denosumab.other.no.fls.female))
rmultinorm$rmultinorm.primary.adh_zoledronate.other.no.fls.female<-
 get.sample(c(1-input$primary.adh_zoledronate.other.no.fls.female,
              input$primary.adh_zoledronate.other.no.fls.female))
rmultinorm$rmultinorm.primary.adh_teriparatide.other.no.fls.female<-
 get.sample(c(1-input$primary.adh_teriparatide.other.no.fls.female,
              input$primary.adh_teriparatide.other.no.fls.female))
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.no.fls.female<-
 get.sample(c(1-input$primary.adh_abaloparatide.other.no.fls.female,
              input$primary.adh_abaloparatide.other.no.fls.female))
rmultinorm$rmultinorm.primary.adh_romo.other.no.fls.female<-
 get.sample(c(1-input$primary.adh_romo.other.no.fls.female,
              input$primary.adh_romo.other.no.fls.female))




rmultinorm$rmultinorm.primary.adh_alendronate.spine.fls.male<-
 get.sample(c(1-input$primary.adh_alendronate.spine.fls.male,
              input$primary.adh_alendronate.spine.fls.male))
rmultinorm$rmultinorm.primary.adh_risedronate.spine.fls.male<-
 get.sample(c(1-input$primary.adh_risedronate.spine.fls.male,
              input$primary.adh_risedronate.spine.fls.male))
rmultinorm$rmultinorm.primary.adh_strontium.spine.fls.male<-
 get.sample(c(1-input$primary.adh_strontium.spine.fls.male,
              input$primary.adh_strontium.spine.fls.male))
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.fls.male<-
 get.sample(c(1-input$primary.adh_ibandronate.spine.fls.male,
              input$primary.adh_ibandronate.spine.fls.male))
rmultinorm$rmultinorm.primary.adh_denosumab.spine.fls.male<-
 get.sample(c(1-input$primary.adh_denosumab.spine.fls.male,
              input$primary.adh_denosumab.spine.fls.male))
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.fls.male<-
 get.sample(c(1-input$primary.adh_zoledronate.spine.fls.male,
              input$primary.adh_zoledronate.spine.fls.male))
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.fls.male<-
 get.sample(c(1-input$primary.adh_teriparatide.spine.fls.male,
              input$primary.adh_teriparatide.spine.fls.male))
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.fls.male<-
 get.sample(c(1-input$primary.adh_abaloparatide.spine.fls.male,
              input$primary.adh_abaloparatide.spine.fls.male))
rmultinorm$rmultinorm.primary.adh_romo.spine.fls.male<-
 get.sample(c(1-input$primary.adh_romo.spine.fls.male,
              input$primary.adh_romo.spine.fls.male))


rmultinorm$rmultinorm.primary.adh_alendronate.hip.fls.male<-
 get.sample(c(1-input$primary.adh_alendronate.hip.fls.male,
              input$primary.adh_alendronate.hip.fls.male))
rmultinorm$rmultinorm.primary.adh_risedronate.hip.fls.male<-
 get.sample(c(1-input$primary.adh_risedronate.hip.fls.male,
              input$primary.adh_risedronate.hip.fls.male))
rmultinorm$rmultinorm.primary.adh_strontium.hip.fls.male<-
 get.sample(c(1-input$primary.adh_strontium.hip.fls.male,
              input$primary.adh_strontium.hip.fls.male))
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.fls.male<-
 get.sample(c(1-input$primary.adh_ibandronate.hip.fls.male,
              input$primary.adh_ibandronate.hip.fls.male))
rmultinorm$rmultinorm.primary.adh_denosumab.hip.fls.male<-
 get.sample(c(1-input$primary.adh_denosumab.hip.fls.male,
              input$primary.adh_denosumab.hip.fls.male))
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.fls.male<-
 get.sample(c(1-input$primary.adh_zoledronate.hip.fls.male,
              input$primary.adh_zoledronate.hip.fls.male))
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.fls.male<-
 get.sample(c(1-input$primary.adh_teriparatide.hip.fls.male,
              input$primary.adh_teriparatide.hip.fls.male))
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.fls.male<-
 get.sample(c(1-input$primary.adh_abaloparatide.hip.fls.male,
              input$primary.adh_abaloparatide.hip.fls.male))
rmultinorm$rmultinorm.primary.adh_romo.hip.fls.male<-
 get.sample(c(1-input$primary.adh_romo.hip.fls.male,
              input$primary.adh_romo.hip.fls.male))



rmultinorm$rmultinorm.primary.adh_alendronate.other.fls.male<-
 get.sample(c(1-input$primary.adh_alendronate.other.fls.male,
              input$primary.adh_alendronate.other.fls.male))
rmultinorm$rmultinorm.primary.adh_risedronate.other.fls.male<-
 get.sample(c(1-input$primary.adh_risedronate.other.fls.male,
              input$primary.adh_risedronate.other.fls.male))
rmultinorm$rmultinorm.primary.adh_strontium.other.fls.male<-
 get.sample(c(1-input$primary.adh_strontium.other.fls.male,
              input$primary.adh_strontium.other.fls.male))
rmultinorm$rmultinorm.primary.adh_ibandronate.other.fls.male<-
 get.sample(c(1-input$primary.adh_ibandronate.other.fls.male,
              input$primary.adh_ibandronate.other.fls.male))
rmultinorm$rmultinorm.primary.adh_denosumab.other.fls.male<-
 get.sample(c(1-input$primary.adh_denosumab.other.fls.male,
              input$primary.adh_denosumab.other.fls.male))
rmultinorm$rmultinorm.primary.adh_zoledronate.other.fls.male<-
 get.sample(c(1-input$primary.adh_zoledronate.other.fls.male,
              input$primary.adh_zoledronate.other.fls.male))
rmultinorm$rmultinorm.primary.adh_teriparatide.other.fls.male<-
 get.sample(c(1-input$primary.adh_teriparatide.other.fls.male,
              input$primary.adh_teriparatide.other.fls.male))
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.fls.male<-
 get.sample(c(1-input$primary.adh_abaloparatide.other.fls.male,
              input$primary.adh_abaloparatide.other.fls.male))
rmultinorm$rmultinorm.primary.adh_romo.other.fls.male<-
 get.sample(c(1-input$primary.adh_romo.other.fls.male,
              input$primary.adh_romo.other.fls.male))




rmultinorm$rmultinorm.primary.adh_alendronate.spine.fls.female<-
 get.sample(c(1-input$primary.adh_alendronate.spine.fls.female,
              input$primary.adh_alendronate.spine.fls.female))
rmultinorm$rmultinorm.primary.adh_risedronate.spine.fls.female<-
 get.sample(c(1-input$primary.adh_risedronate.spine.fls.female,
              input$primary.adh_risedronate.spine.fls.female))
rmultinorm$rmultinorm.primary.adh_strontium.spine.fls.female<-
 get.sample(c(1-input$primary.adh_strontium.spine.fls.female,
              input$primary.adh_strontium.spine.fls.female))
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.fls.female<-
 get.sample(c(1-input$primary.adh_ibandronate.spine.fls.female,
              input$primary.adh_ibandronate.spine.fls.female))
rmultinorm$rmultinorm.primary.adh_denosumab.spine.fls.female<-
 get.sample(c(1-input$primary.adh_denosumab.spine.fls.female,
              input$primary.adh_denosumab.spine.fls.female))
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.fls.female<-
 get.sample(c(1-input$primary.adh_zoledronate.spine.fls.female,
              input$primary.adh_zoledronate.spine.fls.female))
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.fls.female<-
 get.sample(c(1-input$primary.adh_teriparatide.spine.fls.female,
              input$primary.adh_teriparatide.spine.fls.female))
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.fls.female<-
 get.sample(c(1-input$primary.adh_abaloparatide.spine.fls.female,
              input$primary.adh_abaloparatide.spine.fls.female))
rmultinorm$rmultinorm.primary.adh_romo.spine.fls.female<-
 get.sample(c(1-input$primary.adh_romo.spine.fls.female,
              input$primary.adh_romo.spine.fls.female))


rmultinorm$rmultinorm.primary.adh_alendronate.hip.fls.female<-
 get.sample(c(1-input$primary.adh_alendronate.hip.fls.female,
              input$primary.adh_alendronate.hip.fls.female))
rmultinorm$rmultinorm.primary.adh_risedronate.hip.fls.female<-
 get.sample(c(1-input$primary.adh_risedronate.hip.fls.female,
              input$primary.adh_risedronate.hip.fls.female))
rmultinorm$rmultinorm.primary.adh_strontium.hip.fls.female<-
 get.sample(c(1-input$primary.adh_strontium.hip.fls.female,
              input$primary.adh_strontium.hip.fls.female))
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.fls.female<-
 get.sample(c(1-input$primary.adh_ibandronate.hip.fls.female,
              input$primary.adh_ibandronate.hip.fls.female))
rmultinorm$rmultinorm.primary.adh_denosumab.hip.fls.female<-
 get.sample(c(1-input$primary.adh_denosumab.hip.fls.female,
              input$primary.adh_denosumab.hip.fls.female))
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.fls.female<-
 get.sample(c(1-input$primary.adh_zoledronate.hip.fls.female,
              input$primary.adh_zoledronate.hip.fls.female))
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.fls.female<-
 get.sample(c(1-input$primary.adh_teriparatide.hip.fls.female,
              input$primary.adh_teriparatide.hip.fls.female))
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.fls.female<-
 get.sample(c(1-input$primary.adh_abaloparatide.hip.fls.female,
              input$primary.adh_abaloparatide.hip.fls.female))
rmultinorm$rmultinorm.primary.adh_romo.hip.fls.female<-
 get.sample(c(1-input$primary.adh_romo.hip.fls.female,
              input$primary.adh_romo.hip.fls.female))



rmultinorm$rmultinorm.primary.adh_alendronate.other.fls.female<-
 get.sample(c(1-input$primary.adh_alendronate.other.fls.female,
              input$primary.adh_alendronate.other.fls.female))
rmultinorm$rmultinorm.primary.adh_risedronate.other.fls.female<-
 get.sample(c(1-input$primary.adh_risedronate.other.fls.female,
              input$primary.adh_risedronate.other.fls.female))
rmultinorm$rmultinorm.primary.adh_strontium.other.fls.female<-
 get.sample(c(1-input$primary.adh_strontium.other.fls.female,
              input$primary.adh_strontium.other.fls.female))
rmultinorm$rmultinorm.primary.adh_ibandronate.other.fls.female<-
 get.sample(c(1-input$primary.adh_ibandronate.other.fls.female,
              input$primary.adh_ibandronate.other.fls.female))
rmultinorm$rmultinorm.primary.adh_denosumab.other.fls.female<-
 get.sample(c(1-input$primary.adh_denosumab.other.fls.female,
              input$primary.adh_denosumab.other.fls.female))
rmultinorm$rmultinorm.primary.adh_zoledronate.other.fls.female<-
 get.sample(c(1-input$primary.adh_zoledronate.other.fls.female,
              input$primary.adh_zoledronate.other.fls.female))
rmultinorm$rmultinorm.primary.adh_teriparatide.other.fls.female<-
 get.sample(c(1-input$primary.adh_teriparatide.other.fls.female,
              input$primary.adh_teriparatide.other.fls.female))
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.fls.female<-
 get.sample(c(1-input$primary.adh_abaloparatide.other.fls.female,
              input$primary.adh_abaloparatide.other.fls.female))
rmultinorm$rmultinorm.primary.adh_romo.other.fls.female<-
 get.sample(c(1-input$primary.adh_romo.other.fls.female,
              input$primary.adh_romo.other.fls.female))








#
rmultinorm$rmultinorm.monitored.4m_adh_alendronate.male<-
 get.sample(c(1-input$monitored.4m_adh_alendronate.male,
              input$monitored.4m_adh_alendronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_risedronate.male<-
 get.sample(c(1-input$monitored.4m_adh_risedronate.male,
              input$monitored.4m_adh_risedronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_strontium.male<-
 get.sample(c(1-input$monitored.4m_adh_strontium.male,
              input$monitored.4m_adh_strontium.male))
rmultinorm$rmultinorm.monitored.4m_adh_ibandronate.male<-
 get.sample(c(1-input$monitored.4m_adh_ibandronate.male,
              input$monitored.4m_adh_ibandronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_denosumab.male<-
 get.sample(c(1-input$monitored.4m_adh_denosumab.male,
              input$monitored.4m_adh_denosumab.male))
rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.male<-
 get.sample(c(1-input$monitored.4m_adh_zoledronate.male,
              input$monitored.4m_adh_zoledronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.male<-
 get.sample(c(1-input$monitored.4m_adh_teriparatide.male,
              input$monitored.4m_adh_teriparatide.male))
rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.male<-
 get.sample(c(1-input$monitored.4m_adh_abaloparatide.male,
              input$monitored.4m_adh_abaloparatide.male))
rmultinorm$rmultinorm.monitored.4m_adh_romo.male<-
 get.sample(c(1-input$monitored.4m_adh_romo.male,
              input$monitored.4m_adh_romo.male))

rmultinorm$rmultinorm.monitored.12m_adh_alendronate.male<-
 get.sample(c(1-input$monitored.12m_adh_alendronate.male,
              input$monitored.12m_adh_alendronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_risedronate.male<-
 get.sample(c(1-input$monitored.12m_adh_risedronate.male,
              input$monitored.12m_adh_risedronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_strontium.male<-
 get.sample(c(1-input$monitored.12m_adh_strontium.male,
              input$monitored.12m_adh_strontium.male))
rmultinorm$rmultinorm.monitored.12m_adh_ibandronate.male<-
 get.sample(c(1-input$monitored.12m_adh_ibandronate.male,
              input$monitored.12m_adh_ibandronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_denosumab.male<-
 get.sample(c(1-input$monitored.12m_adh_denosumab.male,
              input$monitored.12m_adh_denosumab.male))
rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.male<-
 get.sample(c(1-input$monitored.12m_adh_zoledronate.male,
              input$monitored.12m_adh_zoledronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.male<-
 get.sample(c(1-input$monitored.12m_adh_teriparatide.male,
              input$monitored.12m_adh_teriparatide.male))
rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.male<-
 get.sample(c(1-input$monitored.12m_adh_abaloparatide.male,
              input$monitored.12m_adh_abaloparatide.male))


rmultinorm$rmultinorm.monitored.4m_adh_alendronate.female<-
 get.sample(c(1-input$monitored.4m_adh_alendronate.female,
              input$monitored.4m_adh_alendronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_risedronate.female<-
 get.sample(c(1-input$monitored.4m_adh_risedronate.female,
              input$monitored.4m_adh_risedronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_strontium.female<-
 get.sample(c(1-input$monitored.4m_adh_strontium.female,
              input$monitored.4m_adh_strontium.female))
rmultinorm$rmultinorm.monitored.4m_adh_ibandronate.female<-
 get.sample(c(1-input$monitored.4m_adh_ibandronate.female,
              input$monitored.4m_adh_ibandronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_denosumab.female<-
 get.sample(c(1-input$monitored.4m_adh_denosumab.female,
              input$monitored.4m_adh_denosumab.female))
rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.female<-
 get.sample(c(1-input$monitored.4m_adh_zoledronate.female,
              input$monitored.4m_adh_zoledronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.female<-
 get.sample(c(1-input$monitored.4m_adh_teriparatide.female,
              input$monitored.4m_adh_teriparatide.female))
rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.female<-
 get.sample(c(1-input$monitored.4m_adh_abaloparatide.female,
              input$monitored.4m_adh_abaloparatide.female))
rmultinorm$rmultinorm.monitored.4m_adh_romo.female<-
 get.sample(c(1-input$monitored.4m_adh_romo.female,
              input$monitored.4m_adh_romo.female))

rmultinorm$rmultinorm.monitored.12m_adh_alendronate.female<-
 get.sample(c(1-input$monitored.12m_adh_alendronate.female,
              input$monitored.12m_adh_alendronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_risedronate.female<-
 get.sample(c(1-input$monitored.12m_adh_risedronate.female,
              input$monitored.12m_adh_risedronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_strontium.female<-
 get.sample(c(1-input$monitored.12m_adh_strontium.female,
              input$monitored.12m_adh_strontium.female))
rmultinorm$rmultinorm.monitored.12m_adh_ibandronate.female<-
 get.sample(c(1-input$monitored.12m_adh_ibandronate.female,
              input$monitored.12m_adh_ibandronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_denosumab.female<-
 get.sample(c(1-input$monitored.12m_adh_denosumab.female,
              input$monitored.12m_adh_denosumab.female))
rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.female<-
 get.sample(c(1-input$monitored.12m_adh_zoledronate.female,
              input$monitored.12m_adh_zoledronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.female<-
 get.sample(c(1-input$monitored.12m_adh_teriparatide.female,
              input$monitored.12m_adh_teriparatide.female))
rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.female<-
 get.sample(c(1-input$monitored.12m_adh_abaloparatide.female,
              input$monitored.12m_adh_abaloparatide.female))




rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_alendronate.male,
              input$not.monitored.4m_adh_alendronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_risedronate.male,
              input$not.monitored.4m_adh_risedronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.male<-
 get.sample(c(1-input$not.monitored.4m_adh_strontium.male,
              input$not.monitored.4m_adh_strontium.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_ibandronate.male,
              input$not.monitored.4m_adh_ibandronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.male<-
 get.sample(c(1-input$not.monitored.4m_adh_denosumab.male,
              input$not.monitored.4m_adh_denosumab.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_zoledronate.male,
              input$not.monitored.4m_adh_zoledronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.male<-
 get.sample(c(1-input$not.monitored.4m_adh_teriparatide.male,
              input$not.monitored.4m_adh_teriparatide.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.male<-
 get.sample(c(1-input$not.monitored.4m_adh_abaloparatide.male,
              input$not.monitored.4m_adh_abaloparatide.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_romo.male<-
 get.sample(c(1-input$not.monitored.4m_adh_romo.male,
              input$not.monitored.4m_adh_romo.male))

rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_alendronate.male,
              input$not.monitored.12m_adh_alendronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_risedronate.male,
              input$not.monitored.12m_adh_risedronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.male<-
 get.sample(c(1-input$not.monitored.12m_adh_strontium.male,
              input$not.monitored.12m_adh_strontium.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_ibandronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_ibandronate.male,
              input$not.monitored.12m_adh_ibandronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.male<-
 get.sample(c(1-input$not.monitored.12m_adh_denosumab.male,
              input$not.monitored.12m_adh_denosumab.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_zoledronate.male,
              input$not.monitored.12m_adh_zoledronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.male<-
 get.sample(c(1-input$not.monitored.12m_adh_teriparatide.male,
              input$not.monitored.12m_adh_teriparatide.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.male<-
 get.sample(c(1-input$not.monitored.12m_adh_abaloparatide.male,
              input$not.monitored.12m_adh_abaloparatide.male))


rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_alendronate.female,
              input$not.monitored.4m_adh_alendronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_risedronate.female,
              input$not.monitored.4m_adh_risedronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.female<-
 get.sample(c(1-input$not.monitored.4m_adh_strontium.female,
              input$not.monitored.4m_adh_strontium.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_ibandronate.female,
              input$not.monitored.4m_adh_ibandronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.female<-
 get.sample(c(1-input$not.monitored.4m_adh_denosumab.female,
              input$not.monitored.4m_adh_denosumab.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_zoledronate.female,
              input$not.monitored.4m_adh_zoledronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.female<-
 get.sample(c(1-input$not.monitored.4m_adh_teriparatide.female,
              input$not.monitored.4m_adh_teriparatide.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.female<-
 get.sample(c(1-input$not.monitored.4m_adh_abaloparatide.female,
              input$not.monitored.4m_adh_abaloparatide.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_romo.female<-
 get.sample(c(1-input$not.monitored.4m_adh_romo.female,
              input$not.monitored.4m_adh_romo.female))

rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_alendronate.female,
              input$not.monitored.12m_adh_alendronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_risedronate.female,
              input$not.monitored.12m_adh_risedronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.female<-
 get.sample(c(1-input$not.monitored.12m_adh_strontium.female,
              input$not.monitored.12m_adh_strontium.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_ibandronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_ibandronate.female,
              input$not.monitored.12m_adh_ibandronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.female<-
 get.sample(c(1-input$not.monitored.12m_adh_denosumab.female,
              input$not.monitored.12m_adh_denosumab.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_zoledronate.female,
              input$not.monitored.12m_adh_zoledronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.female<-
 get.sample(c(1-input$not.monitored.12m_adh_teriparatide.female,
              input$not.monitored.12m_adh_teriparatide.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.female<-
 get.sample(c(1-input$not.monitored.12m_adh_abaloparatide.female,
              input$not.monitored.12m_adh_abaloparatide.female))


#
rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.male<-
 get.sample(c(input$adh_annual_decline_alendronate.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_alendronate.no.fls.male))# 2 continuing adhering
rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.male<-
 get.sample(c(input$adh_annual_decline_risedronate.no.fls.male, 
              1-input$adh_annual_decline_risedronate.no.fls.male))
rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.male<-
 get.sample(c(input$adh_annual_decline_strontium.no.fls.male, 
              1-input$adh_annual_decline_strontium.no.fls.male))
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.male<-
 get.sample(c(input$adh_annual_decline_ibandronate.no.fls.male, 
              1-input$adh_annual_decline_ibandronate.no.fls.male))
rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.male<-
 get.sample(c(input$adh_annual_decline_denosumab.no.fls.male, 
              1-input$adh_annual_decline_denosumab.no.fls.male))
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.male<-
 get.sample(c(input$adh_annual_decline_zoledronate.no.fls.male, 
              1-input$adh_annual_decline_zoledronate.no.fls.male))

rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.female<-
 get.sample(c(input$adh_annual_decline_alendronate.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_alendronate.no.fls.female))# 2 continuing adhering
rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.female<-
 get.sample(c(input$adh_annual_decline_risedronate.no.fls.female, 
              1-input$adh_annual_decline_risedronate.no.fls.female))
rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.female<-
 get.sample(c(input$adh_annual_decline_strontium.no.fls.female, 
              1-input$adh_annual_decline_strontium.no.fls.female))
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.female<-
 get.sample(c(input$adh_annual_decline_ibandronate.no.fls.female, 
              1-input$adh_annual_decline_ibandronate.no.fls.female))
rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.female<-
 get.sample(c(input$adh_annual_decline_denosumab.no.fls.female, 
              1-input$adh_annual_decline_denosumab.no.fls.female))
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.female<-
 get.sample(c(input$adh_annual_decline_zoledronate.no.fls.female, 
              1-input$adh_annual_decline_zoledronate.no.fls.female))


rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.male<-
 get.sample(c(input$adh_annual_decline_alendronate.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_alendronate.fls.male))# 2 continuing adhering
rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.male<-
 get.sample(c(input$adh_annual_decline_risedronate.fls.male, 
              1-input$adh_annual_decline_risedronate.fls.male))
rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.male<-
 get.sample(c(input$adh_annual_decline_strontium.fls.male, 
              1-input$adh_annual_decline_strontium.fls.male))
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.male<-
 get.sample(c(input$adh_annual_decline_ibandronate.fls.male, 
              1-input$adh_annual_decline_ibandronate.fls.male))
rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.male<-
 get.sample(c(input$adh_annual_decline_denosumab.fls.male, 
              1-input$adh_annual_decline_denosumab.fls.male))
rmultinorm$rmultinorm.adh_annual_decline_zoledronatefls.male<-
 get.sample(c(input$adh_annual_decline_zoledronate.fls.male, 
              1-input$adh_annual_decline_zoledronate.fls.male))

rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.female<-
 get.sample(c(input$adh_annual_decline_alendronate.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_alendronate.fls.female))# 2 continuing adhering
rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.female<-
 get.sample(c(input$adh_annual_decline_risedronate.fls.female, 
              1-input$adh_annual_decline_risedronate.fls.female))
rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.female<-
 get.sample(c(input$adh_annual_decline_strontium.fls.female, 
              1-input$adh_annual_decline_strontium.fls.female))
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.female<-
 get.sample(c(input$adh_annual_decline_ibandronate.fls.female, 
              1-input$adh_annual_decline_ibandronate.fls.female))
rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.female<-
 get.sample(c(input$adh_annual_decline_denosumab.fls.female, 
              1-input$adh_annual_decline_denosumab.fls.female))
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.female<-
 get.sample(c(input$adh_annual_decline_zoledronate.fls.female, 
              1-input$adh_annual_decline_zoledronate.fls.female))



# for costing 
#browser()
# 1 not hospitalised, 2) hopsitalised (for spine inpatient or outpatient)
rmultinorm$rmultinorm.fx.hosp.hip<-
 get.sample(c(input$prop.not.admitted.clinic.hip,
   input$prop.admitted.surgery.hip+
              input$prop.admitted.no.surgery.hip
              )) 

rmultinorm$rmultinorm.fx.hosp.other<-
 get.sample(c(input$prop.not.admitted.clinic.other,
              input$prop.admitted.surgery.other+
              input$prop.admitted.no.surgery.other))

rmultinorm$rmultinorm.fx.hosp.spine<-
 get.sample(c(input$prop.hospital.community.spine,
              input$prop.hospital.spine))



#surgery- hip/other -if admitted
# 1 no surgery, 2 surgery
rmultinorm$rmultinorm.fx.hosp.surgery.hip<-
 get.sample(c(input$prop.admitted.no.surgery.hip,
              input$prop.admitted.surgery.hip))
rmultinorm$rmultinorm.fx.hosp.surgery.other<-
 get.sample(c(input$prop.admitted.no.surgery.other,
              input$prop.admitted.surgery.other))

#surgery- spine 
# -if admitted
# 1 no surgery, 2 kyphoplasty, 3 vertebroplasty
rmultinorm$rmultinorm.fx.hosp.surgery.spine<-
 get.sample(c(input$prop.hospital.spine.no.intervention,
              input$prop.hospital.spine.kyphoplasty,
              input$prop.hospital.spine.vertebroplasty))
#if not admitted
rmultinorm$rmultinorm.fx.not.admitted.surgery.spine<-
 get.sample(c(input$prop.not.admitted.spine.no.intervention,
              input$prop.not.admitted.spine.kyphoplasty,
              input$prop.not.admitted.spine.vertebroplasty))

#browser()

# for location
# it will depend where they are, can't revert to 



# any possible
# 1 temp rehap
# 2 home no support
# 3 home support
# 4 family home
# 5 long term care

rmultinorm$rmultinorm.discharged.hip.all<-
 get.sample(c(input$prop.discharged.temp.rehab.hip,
              input$prop.discharged.home.no.support.hip,
              input$prop.discharged.home.support.hip,
              input$prop.discharged.family.home.hip,
              input$prop.discharged.long.term.care.hip))


# from temp rehab 
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.hip.temp.rehab<-
 get.sample(c(input$prop.rehab.to.home.no.support.hip,
              input$prop.rehab.to.home.support.hip,
              input$prop.rehab.to.family.home.hip,
              input$prop.rehab.to.long.term.care.hip))


# from home no support 
#scale remaining
sum<-sum(input$prop.discharged.home.no.support.hip,
              input$prop.discharged.home.support.hip,
              input$prop.discharged.family.home.hip,
              input$prop.discharged.long.term.care.hip)
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.hip.from.home.no.support<-
get.sample(c(input$prop.discharged.home.no.support.hip/sum,
input$prop.discharged.home.support.hip/sum,
input$prop.discharged.family.home.hip/sum,
input$prop.discharged.long.term.care.hip/sum))


#from home support
sum<-sum(input$prop.discharged.home.support.hip,
              input$prop.discharged.family.home.hip,
              input$prop.discharged.long.term.care.hip)
# 1 home support
# 2 family home
# 3 long term care
rmultinorm$rmultinorm.discharged.hip.from.home.support<-
get.sample(c(input$prop.discharged.home.support.hip/sum,
input$prop.discharged.family.home.hip/sum,
input$prop.discharged.long.term.care.hip/sum))

#from family home
# 1 family home
# 2 long term care
sum<-sum(input$prop.discharged.family.home.hip,
              input$prop.discharged.long.term.care.hip)
rmultinorm$rmultinorm.discharged.hip.from.family.home<-
get.sample(c(
input$prop.discharged.family.home.hip/sum,
input$prop.discharged.long.term.care.hip/sum))



# any possible
# 1 temp rehap
# 2 home no support
# 3 home support
# 4 family home
# 5 long term care

rmultinorm$rmultinorm.discharged.spine.all<-
 get.sample(c(input$prop.discharged.temp.rehab.spine,
              input$prop.discharged.home.no.support.spine,
              input$prop.discharged.home.support.spine,
              input$prop.discharged.family.home.spine,
              input$prop.discharged.long.term.care.spine))


# from temp rehab 
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.spine.temp.rehab<-
 get.sample(c(input$prop.rehab.to.home.no.support.spine,
              input$prop.rehab.to.home.support.spine,
              input$prop.rehab.to.family.home.spine,
              input$prop.rehab.to.long.term.care.spine))


# from home no support 
#scale remaining
sum<-sum(input$prop.discharged.home.no.support.spine,
              input$prop.discharged.home.support.spine,
              input$prop.discharged.family.home.spine,
              input$prop.discharged.long.term.care.spine)
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.spine.from.home.no.support<-
get.sample(c(input$prop.discharged.home.no.support.spine/sum,
input$prop.discharged.home.support.spine/sum,
input$prop.discharged.family.home.spine/sum,
input$prop.discharged.long.term.care.spine/sum))


#from home support
# 1 home support
# 2 family home
# 3 long term care



if(as.numeric(sum)>0){
rmultinorm$rmultinorm.discharged.spine.from.home.support<-
get.sample(c(input$prop.discharged.home.support.spine/sum,
input$prop.discharged.family.home.spine/sum,
input$prop.discharged.long.term.care.spine/sum))} 


if(as.numeric(sum)==0){
  rmultinorm$rmultinorm.discharged.spine.from.home.support<-1
}

#from family home
# 1 family home
# 2 long term care
sum<-sum(input$prop.discharged.family.home.spine,
              input$prop.discharged.long.term.care.spine)
if(sum>0){
rmultinorm$rmultinorm.discharged.spine.from.family.home<-
get.sample(c(
input$prop.discharged.family.home.spine/sum,
input$prop.discharged.long.term.care.spine/sum))} else {
 rmultinorm$rmultinorm.discharged.spine.from.family.home<-1 
}




# any possible
# 1 temp rehap
# 2 home no support
# 3 home support
# 4 family home
# 5 long term care

rmultinorm$rmultinorm.discharged.other.all<-
 get.sample(c(input$prop.discharged.temp.rehab.other,
              input$prop.discharged.home.no.support.other,
              input$prop.discharged.home.support.other,
              input$prop.discharged.family.home.other,
              input$prop.discharged.long.term.care.other))


# from temp rehab 
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.other.temp.rehab<-
 get.sample(c(input$prop.rehab.to.home.no.support.other,
              input$prop.rehab.to.home.support.other,
              input$prop.rehab.to.family.home.other,
              input$prop.rehab.to.long.term.care.other))


# from home no support 
#scale remaining
sum<-sum(input$prop.discharged.home.no.support.other,
              input$prop.discharged.home.support.other,
              input$prop.discharged.family.home.other,
              input$prop.discharged.long.term.care.other)
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.other.from.home.no.support<-
get.sample(c(input$prop.discharged.home.no.support.other/sum,
input$prop.discharged.home.support.other/sum,
input$prop.discharged.family.home.other/sum,
input$prop.discharged.long.term.care.other/sum))


#from home support
sum<-sum(input$prop.discharged.home.support.other,
              input$prop.discharged.family.home.other,
              input$prop.discharged.long.term.care.other)
# 1 home support
# 2 family home
# 3 long term care
if(sum>0){
rmultinorm$rmultinorm.discharged.other.from.home.support<-
get.sample(c(input$prop.discharged.home.support.other/sum,
input$prop.discharged.family.home.other/sum,
input$prop.discharged.long.term.care.other/sum))} else{
  rmultinorm$rmultinorm.discharged.other.from.home.support<-1
}

#from family home
# 1 family home
# 2 long term care
sum<-sum(input$prop.discharged.family.home.other,
              input$prop.discharged.long.term.care.other)
if(sum>0){
rmultinorm$rmultinorm.discharged.other.from.family.home<-
get.sample(c(
input$prop.discharged.family.home.other/sum,
input$prop.discharged.long.term.care.other/sum))} else{
  rmultinorm$rmultinorm.discharged.other.from.family.home<-1
}



#browser()
# lab test
#1 no
#2 yes
rmultinorm$rmultinorm.lab.test.no.fls.hip<-
 get.sample(c(1-input$prop.lab.test.no.fls.hip,
              input$prop.lab.test.no.fls.hip))
rmultinorm$rmultinorm.lab.test.no.fls.spine<-
 get.sample(c(1-input$prop.lab.test.no.fls.spine,
              input$prop.lab.test.no.fls.spine))
rmultinorm$rmultinorm.lab.test.no.fls.other<-
 get.sample(c(1-input$prop.lab.test.no.fls.other,
              input$prop.lab.test.no.fls.other))

rmultinorm$rmultinorm.lab.test.fls.hip<-
 get.sample(c(1-input$prop.lab.test.fls.hip,
              input$prop.lab.test.fls.hip))
rmultinorm$rmultinorm.lab.test.fls.spine<-
 get.sample(c(1-input$prop.lab.test.fls.spine,
              input$prop.lab.test.fls.spine))
rmultinorm$rmultinorm.lab.test.fls.other<-
 get.sample(c(1-input$prop.lab.test.fls.other,
              input$prop.lab.test.fls.other))

#dxa
rmultinorm$rmultinorm.dxa.no.fls.hip<-
 get.sample(c(1-input$prop.dxa.no.fls.hip,
              input$prop.dxa.no.fls.hip))
rmultinorm$rmultinorm.dxa.no.fls.spine<-
 get.sample(c(1-input$prop.dxa.no.fls.spine,
              input$prop.dxa.no.fls.spine))
rmultinorm$rmultinorm.dxa.no.fls.other<-
 get.sample(c(1-input$prop.dxa.no.fls.other,
              input$prop.dxa.no.fls.other))

rmultinorm$rmultinorm.dxa.fls.hip<-
 get.sample(c(1-input$prop.dxa.fls.hip,
              input$prop.dxa.fls.hip))
rmultinorm$rmultinorm.dxa.fls.spine<-
 get.sample(c(1-input$prop.dxa.fls.spine,
              input$prop.dxa.fls.spine))
rmultinorm$rmultinorm.dxa.fls.other<-
 get.sample(c(1-input$prop.dxa.fls.other,
              input$prop.dxa.fls.other))



rmultinorm
}





## time since fracture -----
# for each type possible
# individuals could have had multiple, so get each independently
m.fun.time.since.fx<-function(df=df, t=t){
  #browser()
if(any(df[,"s_sf"] %in% 1)){
df[t,"t_since_sf"]<-  as.numeric(t- which.max(df[,"s_sf"] == 1) )
}
if(any(df[,"s_hf"] %in% 1)){
df[t,"t_since_hf"]<-  t- which.max(df[,"s_hf"] == 1)
}
if(any(df[,"s_of"] %in% 1)){
df[t,"t_since_of"]<-  t- which.max(df[,"s_of"] == 1)
}
df
}


## transition probabilities -----
# add background transition probabilities 
m.fun.tps<-function(df=df,
                  t=t,
                  background.tps=background.tps){
if(df[1,"sex"]==1 & df[t-1,"h_hf"]<=1){
df[t,"sf.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_sf.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_sf.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_sf.male"])
df[t,"hf.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_hf.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_hf.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_hf.male"])
df[t,"of.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_of.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_of.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_of.male"])
df[t,"tp.death"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf1_d.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_d.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_d.male"])
} else if (df[1,"sex"]=="male" & df[t-1,"h_hf"]>1){
df[t,"sf.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_sf.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_sf.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_sf.male"])
df[t,"hf.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_hf.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_hf.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_hf.male"])
df[t,"of.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_of.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_of.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_of.male"])
df[t,"tp.death"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf2_d.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_d.male"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_d.male"])
} else if(df[1,"sex"]=="female" & df[t-1,"h_hf"]<="1"){
df[t,"sf.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_sf.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_sf.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_sf.female"])
df[t,"hf.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_hf.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_hf.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_hf.female"])
df[t,"of.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_of.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_of.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_of.female"])
df[t,"tp.death"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf1_d.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_d.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_d.female"])
} else {
df[t,"sf.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_sf.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_sf.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_sf.female"])
df[t,"hf.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_hf.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_hf.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_hf.female"])
df[t,"of.tp"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf_of.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_of.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_of.female"])
df[t,"tp.death"]<-
max(background.tps[which(background.tps[,"time"]==
             df[t,"t_since_hf"]),"tp.hf2_d.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_sf"]),"tp.sf_d.female"],
    background.tps[which(background.tps[,"time"]==
             df[t,"t_since_of"]),"tp.of_d.female"])
}

df
}
## limit to max two hip fx-----
# if they've had two hip fx, set tp to zero
m.fun.two.fx<-function(df=df, t=t){
if(df[t-1,"h_hf"]==2){
  df[t,"hf.tp"]<-0
}
df
}

## identified -----
# assign if fx 
# same as previous period if not
m.fun.identified<-function(df=df, t=t, rmultinorm=rmultinorm){

if(df[t-1,"c_af"]==1 & 
   df[t,"intervention"]==1 &   #  no.fls
   df[t,"index_fx"]==1){     # spine
   df[t,"identified"]<-rmultinorm$rmultinorm.no.fls.prob_identification.spine[df[t,"id"]]
  } else if(df[t-1,"c_af"]==1 & 
   df[t,"intervention"]==1 &   #  no.fls
   df[t,"index_fx"]==2 ){     # hip
   df[t,"identified"]<-rmultinorm$rmultinorm.no.fls.prob_identification.hip[df[t,"id"]]
  } else if(df[t-1,"c_af"]==1 & 
   df[t,"intervention"]==1 &   #  no.fls
   df[t,"index_fx"]==3 ){        # other
   df[t,"identified"]<-rmultinorm$rmultinorm.no.fls.prob_identification.other[df[t,"id"]]
  }  else if(df[t-1,"c_af"]==1 & 
   df[t,"intervention"]==2 &   #  fls
   df[t,"index_fx"]==1){   # spine
   df[t,"identified"]<-rmultinorm$rmultinorm.fls.prob_identification.spine[df[t,"id"]]
  }  else if(df[t-1,"c_af"]==1 & 
   df[t,"intervention"]==2 &   #  fls
   df[t,"index_fx"]==2){     # hip
   df[t,"identified"]<-rmultinorm$rmultinorm.fls.prob_identification.hip[df[t,"id"]]
  }  else if(df[t-1,"c_af"]==1 & 
   df[t,"intervention"]==2 &   #  fls
   df[t,"index_fx"]==3 ){    # other
   df[t,"identified"]<-rmultinorm$rmultinorm.fls.prob_identification.other[df[t,"id"]]
  }  else {
   df[t,"identified"]<-df[t-1,"identified"]
  }
df
}

## tr onset ----
# before time to onset or not

# has more than time_to_treat.spine months passed since an identified hip fx, or 
# more than time_to_treat.spine since an identified spine fx, or
# more than time_to_treat.other since an identified other fx,

m.fun.tr.onset<-function(df=df, t=t, input=input){

if(df[t,"identified"]==1){ #not identified 
   df[t,"tr.onset"]<-1
} 

if(df[t,"identified"]==2 &
   df[t,"intervention"]==1 &  # no.fls
   !is.na(df[t,"t_since_sf"])  &
   df[t,"cycle"] > input$no.fls.time_to_treat.spine){
   df[t,"tr.onset"]<-2
   } 
if(df[t,"identified"]==2 &
   df[t,"intervention"]==1 &  # no.fls
   !is.na(df[t,"t_since_hf"])  &
   df[t,"cycle"] > input$no.fls.time_to_treat.hip){
   df[t,"tr.onset"]<-2
   } 
if(df[t,"identified"]==2 &
   df[t,"intervention"]==1 &  # no.fls
   !is.na(df[t,"t_since_of"])  &
   df[t,"cycle"] > input$no.fls.time_to_treat.other){
   df[t,"tr.onset"]<-2
   } 

 if(df[t,"identified"]==2 &
   df[t,"intervention"]==2 &  # fls
   !is.na(df[t,"t_since_sf"])  &
   df[t,"cycle"] > input$fls.time_to_treat.spine){
   df[t,"tr.onset"]<-2
   } 
if(df[t,"identified"]==2 &
   df[t,"intervention"]==2 &  # fls
   !is.na(df[t,"t_since_hf"])  &
   df[t,"cycle"] > input$fls.time_to_treat.hip){
   df[t,"tr.onset"]<-2
   } 
if(df[t,"identified"]==2 &
   df[t,"intervention"]==2 &  # fls
   !is.na(df[t,"t_since_of"])  &
   df[t,"cycle"] > input$fls.time_to_treat.other){
   df[t,"tr.onset"]<-2
   }  
   
   
   
if(is.na(df[t,"tr.onset"])) {
   df[t,"tr.onset"]<-1
}

  
  df
}

## treat -----

# 1) if tr.onset==1, treat==1
# 2) if tr.onset==2 in current period, and c_af==1 in previous period, assign treat
# 3) if tr.onset==2 in current period, and tr.onset==1 in previous, assign treat
# 4) if tr.onset==2 in current and previous period, c_af==0 in previous period,
# keep treat from previous 

# for male/female and no fls/ fls 
 # 1) if tr.onset==1, treat==1
# 2) if tr.onset==2 in current period, and c_af==1 in previous period, assign treat
# 3) if tr.onset==2 in current period, and tr.onset==1 in previous, assign treat
# 4) if tr.onset==2 in current and previous period, c_af==0 in previous period,
# keep treat from previous 

# for male/female and no fls/ fls 
m.fun.treat<-function(df=df, t=t, rmultinorm=rmultinorm){
if(df[t,"intervention"]==1 &
   df[t,"sex"]==1) {   #  no.fls and male

if(df[t,"tr.onset"]==1){ # 1)
df[t,"treat"]<-1
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"c_af"]==1){ # 2)

     # recent.fx<-which.min(c(df[t,"t_since_sf"],
     #           df[t,"t_since_hf"],
     #           df[t,"t_since_of"])) #most recent fx
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.male[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.male[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.male[df[t,"id"]],
                               NA)))
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==1){ # 3)

     # recent.fx<-which.min(c(df[t,"t_since_sf"],
     #           df[t,"t_since_hf"],
     #           df[t,"t_since_of"])) #most recent fx
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.male[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.male[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.male[df[t,"id"]],
                               NA)))
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==2 &
   df[t-1,"c_af"]==0){ # 4)

   df[t,"treat"]<-df[t-1,"treat"]
}

}

if(df[t,"intervention"]==1 &
   df[t,"sex"]==2) {   #  no.fls and female

if(df[t,"tr.onset"]==1){ # 1)
df[t,"treat"]<-1
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"c_af"]==1){ # 2)

     # recent.fx<-which.min(c(df[t,"t_since_sf"],
     #           df[t,"t_since_hf"],
     #           df[t,"t_since_of"])) #most recent fx
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.female[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.female[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.female[df[t,"id"]],
                               NA)))
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==1){ # 3)

     # recent.fx<-which.min(c(df[t,"t_since_sf"],
     #           df[t,"t_since_hf"],
     #           df[t,"t_since_of"])) #most recent fx
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.female[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.female[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.female[df[t,"id"]],
                               NA)))
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==2 &
   df[t-1,"c_af"]==0){ # 4)

   df[t,"treat"]<-df[t-1,"treat"]
}

}


if(df[t,"intervention"]==2 &
   df[t,"sex"]==1) {   #  fls and male

if(df[t,"tr.onset"]==1){ # 1)
df[t,"treat"]<-1
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"c_af"]==1){ # 2)

     # recent.fx<-which.min(c(df[t,"t_since_sf"],
     #           df[t,"t_since_hf"],
     #           df[t,"t_since_of"])) #most recent fx
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.male[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.male[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.male[df[t,"id"]],
                               NA)))
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==1){ # 3)

     # recent.fx<-which.min(c(df[t,"t_since_sf"],
     #           df[t,"t_since_hf"],
     #           df[t,"t_since_of"])) #most recent fx
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.male[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.male[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.male[df[t,"id"]],
                               NA)))
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==2 &
   df[t-1,"c_af"]==0){ # 4)

   df[t,"treat"]<-df[t-1,"treat"]
}

}

if(df[t,"intervention"]==2 &
   df[t,"sex"]==2) {   #  fls and female

if(df[t,"tr.onset"]==1){ # 1)
df[t,"treat"]<-1
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"c_af"]==1){ # 2)

     # recent.fx<-which.min(c(df[t,"t_since_sf"],
     #           df[t,"t_since_hf"],
     #           df[t,"t_since_of"])) #most recent fx
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.female[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.female[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.female[df[t,"id"]],
                               NA)))
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==1){ # 3)

     # recent.fx<-which.min(c(df[t,"t_since_sf"],
     #           df[t,"t_since_hf"],
     #           df[t,"t_since_of"])) #most recent fx
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.female[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.female[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.female[df[t,"id"]],
                               NA)))
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==2 &
   df[t-1,"c_af"]==0){ # 4)

   df[t,"treat"]<-df[t-1,"treat"]
}

}

  
df
}


# if fx - assign
# if not, same as above

# m.fun.treat<-function(df=df, t=t, rmultinorm=rmultinorm){
# 
# # not fx, as above
# if(df[t-1,"c_af"]== 0){
#   df[t,"treat"]<-df[t-1,"treat"]
#   } else if(
#   df[t,"tr.onset"]== 2 &
#   df[t-1,"s_sf"]==1 & #  spine        ### noFLS
#    df[t,"intervention"]==1 &  # no.fls
#    df[t,"sex"]==1){     # male 
# df[t,"treat"]<-rmultinorm$rmultinorm.no.fls.trt.spine.male[df[t,"id"]]
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_hf"]==1 & #  hip 
#    df[t,"intervention"]==1 &  # no.fls
#    df[t,"sex"]==1){     # male 
# df[t,"treat"]<-rmultinorm$rmultinorm.no.fls.trt.hip.male[df[t,"id"]]
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_of"]==1 & #  other 
#    df[t,"intervention"]==1 &  # no.fls
#    df[t,"sex"]==1){     # male 
# df[t,"treat"]<-rmultinorm$rmultinorm.no.fls.trt.other.male[df[t,"id"]]
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_sf"]==1 & #  spine 
#    df[t,"intervention"]==1 &  # no.fls
#    df[t,"sex"]==2){     # female 
# df[t,"treat"]<-rmultinorm$rmultinorm.no.fls.trt.spine.female[df[t,"id"]]
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_hf"]==1 & #  hip 
#    df[t,"intervention"]==1 &  # no.fls
#    df[t,"sex"]==2){     # female 
# df[t,"treat"]<-rmultinorm$rmultinorm.no.fls.trt.hip.female[df[t,"id"]]
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_of"]==1 & #  other 
#    df[t,"intervention"]==1 &  # no.fls
#    df[t,"sex"]==2){     # female 
# df[t,"treat"]<-rmultinorm$rmultinorm.no.fls.trt.other.female[df[t,"id"]]
# 
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_sf"]==1 & #  spine        ### FLS
#    df[t,"intervention"]==2 &  # fls
#    df[t,"sex"]==1){     # male 
# df[t,"treat"]<-rmultinorm$rmultinorm.fls.trt.spine.male[df[t,"id"]]
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_hf"]==1 & #  hip 
#    df[t,"intervention"]==2 &  # fls
#    df[t,"sex"]==1){     # male 
# df[t,"treat"]<-rmultinorm$rmultinorm.fls.trt.hip.male[df[t,"id"]]
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_of"]==1 & #  other 
#    df[t,"intervention"]==2 &  # fls
#    df[t,"sex"]==1){     # male 
# df[t,"treat"]<-rmultinorm$rmultinorm.fls.trt.other.male[df[t,"id"]]
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_sf"]==1 & #  spine 
#    df[t,"intervention"]==2 &  # fls
#    df[t,"sex"]==2){     # female 
# df[t,"treat"]<-rmultinorm$rmultinorm.fls.trt.spine.female[df[t,"id"]]
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_hf"]==1 & #  hip 
#    df[t,"intervention"]==2 &  # fls
#    df[t,"sex"]==2){     # female 
# df[t,"treat"]<-rmultinorm$rmultinorm.fls.trt.hip.female[df[t,"id"]]
# } else if(df[t,"tr.onset"]== 2 &
#   df[t-1,"s_of"]==1 & #  other 
#    df[t,"intervention"]==2 &  # fls
#    df[t,"sex"]==2){     # female 
# df[t,"treat"]<-rmultinorm$rmultinorm.fls.trt.other.female[df[t,"id"]]
# 
# } else {  
#   df[t,"treat"]<-1 # not - tr.onset=1
# }
#   
#   df
# }

# Assignment to medication ------
# 1) Assign to no medication if treat==1

# 2) If treat==2
# and if c_af==1 in previous period
# assign treatment for new fracture

# 3) If treat==2,
# and treat==1 in previous period
# and if c_af==0 in previous period
# assign treatment

# 4) if treat==2,
# and treat==2 in previous period
# and if c_af==0 in previous period
# assign to treatment from previous period

m.fun.assign.treatment<-function(df=df, t=t, rmultinorm=rmultinorm){

# for male/female and no fls/ fls 

if(df[t,"intervention"]==1 &
   df[t,"sex"]==1) {   #  no.fls and male

if(df[t,"treat"]==1){ # 1) no treatment, no medication
  df[t,"medication"]<-0
  } 
  
if(df[t,"treat"]==2){  # treat==2
  
# 2) c_af==1 in previous period, treat==2  
if(df[t-1,"c_af"]==1){ 
  
   # recent.fx<-which.min(c(df[t,"t_since_sf"],
   #             df[t,"t_since_hf"],
   #             df[t,"t_since_of"])) #most recent fx
   df[t,"medication"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.choice.spine.male[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.choice.hip.male[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.choice.other.male[df[t,"id"]],
                               NA)))
 } 
  
# 3) not on treatment in previous, treat==2, no fx in current  
if(df[t-1,"treat"]==1) { 
if(df[t-1,"c_af"]==0)  {  
# recent.fx<-which.min(c(df[t,"t_since_sf"],
#                df[t,"t_since_hf"],
#                df[t,"t_since_of"])) #most recent fx
   df[t,"medication"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.choice.spine.male[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.choice.hip.male[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.choice.other.male[df[t,"id"]],
                               NA)))
 }}
  
# 4) treat==2 in previous period, c_af==0 in previous period
# assign to treatment from previous period
if(df[t-1,"treat"]==2) { 
if(df[t-1,"c_af"]==0)  { 
  df[t,"medication"]<-df[t-1,"medication"]
  
}
  }

}
  
  }
  
  
if(df[t,"intervention"]==1 &
   df[t,"sex"]==2) {   #  no.fls and female

if(df[t,"treat"]==1){ # 1) no treatment, no medication
  df[t,"medication"]<-0
  } 
  
if(df[t,"treat"]==2){  # treat==2
  
# 2) c_af==1 in previous period, treat==2  
if(df[t-1,"c_af"]==1){ 
  
   # recent.fx<-which.min(c(df[t,"t_since_sf"],
   #             df[t,"t_since_hf"],
   #             df[t,"t_since_of"])) #most recent fx
   df[t,"medication"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.choice.spine.female[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.choice.hip.female[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.choice.other.female[df[t,"id"]],
                               NA)))
 } 
  
# 3) not on treatment in previous, treat==2, no fx in current  
if(df[t-1,"treat"]==1) { 
if(df[t-1,"c_af"]==0)  {  
# recent.fx<-which.min(c(df[t,"t_since_sf"],
#                df[t,"t_since_hf"],
#                df[t,"t_since_of"])) #most recent fx
   df[t,"medication"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.choice.spine.female[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.choice.hip.female[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.choice.other.female[df[t,"id"]],
                               NA)))
 }}
  
# 4) treat==2 in previous period, c_af==0 in previous period
# assign to treatment from previous period
if(df[t-1,"treat"]==2) { 
if(df[t-1,"c_af"]==0)  { 
  df[t,"medication"]<-df[t-1,"medication"]
  
}
  }

}
  
  }  


if(df[t,"intervention"]==2 &
   df[t,"sex"]==1) {   #  fls and male

if(df[t,"treat"]==1){ # 1) no treatment, no medication
  df[t,"medication"]<-0
  } 
  
if(df[t,"treat"]==2){  # treat==2
  
# 2) c_af==1 in previous period, treat==2  
if(df[t-1,"c_af"]==1){ 
  
   # recent.fx<-which.min(c(df[t,"t_since_sf"],
   #             df[t,"t_since_hf"],
   #             df[t,"t_since_of"])) #most recent fx
   df[t,"medication"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.choice.spine.male[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.choice.hip.male[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.choice.other.male[df[t,"id"]],
                               NA)))
 } 
  
# 3) not on treatment in previous, treat==2, no fx in current  
if(df[t-1,"treat"]==1) { 
if(df[t-1,"c_af"]==0)  {  
# recent.fx<-which.min(c(df[t,"t_since_sf"],
#                df[t,"t_since_hf"],
#                df[t,"t_since_of"])) #most recent fx
   df[t,"medication"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.choice.spine.male[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.choice.hip.male[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.choice.other.male[df[t,"id"]],
                               NA)))
 }}
  
# 4) treat==2 in previous period, c_af==0 in previous period
# assign to treatment from previous period
if(df[t-1,"treat"]==2) { 
if(df[t-1,"c_af"]==0)  { 
  df[t,"medication"]<-df[t-1,"medication"]
  
}
  }

}
  
  }
  
  
if(df[t,"intervention"]==2 &
   df[t,"sex"]==2) {   #  fls and female

if(df[t,"treat"]==1){ # 1) no treatment, no medication
  df[t,"medication"]<-0
  } 
  
if(df[t,"treat"]==2){  # treat==2
  
# 2) c_af==1 in previous period, treat==2  
if(df[t-1,"c_af"]==1){ 
  
   # recent.fx<-which.min(c(df[t,"t_since_sf"],
   #             df[t,"t_since_hf"],
   #             df[t,"t_since_of"])) #most recent fx
   df[t,"medication"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.choice.spine.female[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.choice.hip.female[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.choice.other.female[df[t,"id"]],
                               NA)))
 } 
  
# 3) not on treatment in previous, treat==2, no fx in current  
if(df[t-1,"treat"]==1) { 
if(df[t-1,"c_af"]==0)  {  
# recent.fx<-which.min(c(df[t,"t_since_sf"],
#                df[t,"t_since_hf"],
#                df[t,"t_since_of"])) #most recent fx
   df[t,"medication"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.choice.spine.female[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.choice.hip.female[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.choice.other.female[df[t,"id"]],
                               NA)))
 }}
  
# 4) treat==2 in previous period, c_af==0 in previous period
# assign to treatment from previous period
if(df[t-1,"treat"]==2) { 
if(df[t-1,"c_af"]==0)  { 
  df[t,"medication"]<-df[t-1,"medication"]
  
}
  }

}
  
}  
  
  df
}

# # If treat=2 and treat!2 in previous period, they are now being assigned to treatment
# # If treat=2 and c_af=1 in previous period, they are reassigned treatment
# m.fun.assign.treatment<-function(df=df, t=t, rmultinorm=rmultinorm){
# 
# 
# if(df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==1   &  # spine
#    df[t,"sex"]==1){
#  
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-0 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # male
#    df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.spine.male[df[t,"id"]]
#   }else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==1   &  # spine
#    df[t,"sex"]==1){    # male
#     df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.spine.male[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# if(df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==2   &  # hip
#    df[t,"sex"]==1){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # male
#    df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.hip.male[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==2   &  # hip
#    df[t,"sex"]==1){    # male
#     df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.hip.male[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# if(df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==3   &  # other
#    df[t,"sex"]==1){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # male
#    df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.other.male[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==3   &  # other
#    df[t,"sex"]==1){    # male
#     df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.other.male[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# if(df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==1   &  # spine
#    df[t,"sex"]==2){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # female
#    df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.spine.female[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==1   &  # spine
#    df[t,"sex"]==2){    # female
#     df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.spine.female[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# if(df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==2   &  # hip
#    df[t,"sex"]==2){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # female
#    df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.hip.female[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==2   &  # hip
#    df[t,"sex"]==2){    # female
#     df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.hip.female[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# if(df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==3   &  # other
#    df[t,"sex"]==2){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # female
#    df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.other.female[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==1 &   #  no.fls
#    df[t,"index_fx"]==3   &  # other
#    df[t,"sex"]==2){    # female
#     df[t,"medication"]<-rmultinorm$rmultinorm.no.fls.trt.choice.other.female[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# 
# 
# if(df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==1   &  # spine
#    df[t,"sex"]==1){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # male
#    df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.spine.male[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==1   &  # spine
#    df[t,"sex"]==1){    # male
#     df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.spine.male[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# if(df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==2   &  # hip
#    df[t,"sex"]==1){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # male
#    df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.hip.male[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==2   &  # hip
#    df[t,"sex"]==1){    # male
#     df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.hip.male[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# if(df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==3   &  # other
#    df[t,"sex"]==1){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # male
#    df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.other.male[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==3   &  # other
#    df[t,"sex"]==1){    # male
#     df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.other.male[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# if(df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==1   &  # spine
#    df[t,"sex"]==2){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # female
#    df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.spine.female[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==1   &  # spine
#    df[t,"sex"]==2){    # female
#     df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.spine.female[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# if(df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==2   &  # hip
#    df[t,"sex"]==2){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # female
#    df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.hip.female[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==2   &  # hip
#    df[t,"sex"]==2){    # female
#     df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.hip.female[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
# 
# if(df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==3   &  # other
#    df[t,"sex"]==2){
#    if(df[t,"treat"]==1){
#   df[t,"medication"]<-1 
# }else if((df[t-1,"treat"]!=2 | is.na(df[t-1,"treat"])) &
#    df[t,"treat"]==2) {
#     # female
#    df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.other.female[df[t,"id"]]
#   }
# else if(df[t-1,"c_af"]==1 &
#    df[t,"treat"]==2 &
#    df[t,"intervention"]==2 &   #  fls
#    df[t,"index_fx"]==3   &  # other
#    df[t,"sex"]==2){    # female
#     df[t,"medication"]<-rmultinorm$rmultinorm.fls.trt.choice.other.female[df[t,"id"]]
#   }  else {
#     df[t,"medication"]<-df[t-1,"medication"]
#   }
# }
#   
#   df
# }

# switching med -----
m.fun.switch.treatment<-function(df=df, t=t, rmultinorm=rmultinorm){
if(t>14 ){
if(df[t,"intervention"]==1 &
   df[t,"sex"]==1&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.romo.to.no.fls.male[df[t,"id"]]
}}

if(t>14 ){
if(df[t,"intervention"]==1 &
   df[t,"sex"]==2&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.romo.to.no.fls.female[df[t,"id"]]
}}

if(t>14 ){
if( df[t,"intervention"]==2 &
   df[t,"sex"]==1&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.romo.to.fls.male[df[t,"id"]]
}}

if(t>14 ){
if(df[t,"intervention"]==2 &
   df[t,"sex"]==2&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.romo.to.fls.female[df[t,"id"]]
}}




if(t>26 ){
if( df[t,"intervention"]==1 &
   df[t,"sex"]==1&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.abaloparatide.to.no.fls.male[df[t,"id"]]
}}

if(t>26){
if( df[t,"intervention"]==1 &
   df[t,"sex"]==2&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.abaloparatide.to.no.fls.female[df[t,"id"]]
}}

if(t>26){
if( df[t,"intervention"]==2 &
   df[t,"sex"]==1&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.abaloparatide.to.fls.male[df[t,"id"]]
}}

if(t>26 ){
if(df[t,"intervention"]==2 &
   df[t,"sex"]==2&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.abaloparatide.to.fls.female[df[t,"id"]]
}}




if(t>26 ){
if(df[t,"intervention"]==1 &
   df[t,"sex"]==1&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.teriparatide.to.no.fls.male[df[t,"id"]]
}}

if(t>26 ){
if(df[t,"intervention"]==1 &
   df[t,"sex"]==2&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.teriparatide.to.no.fls.female[df[t,"id"]]
}}

if(t>26){
if( df[t,"intervention"]==2 &
   df[t,"sex"]==1&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.teriparatide.to.fls.male[df[t,"id"]]
}}

if(t>26){
if(df[t,"intervention"]==2 &
   df[t,"sex"]==2&
   all(df[(t-13):(t-1),"medication"]==9)){
  df[t,"medication"]<-rmultinorm$rmultinorm.teriparatide.to.fls.female[df[t,"id"]]
}}
df
}
###
# adherrence ----
m.fun.adherrence.t1<-function(df=df, t=t, rmultinorm=rmultinorm){
  
  # #  no.fls, male, last fx spine
if(df[t,"intervention"]==1 &
   df[t,"sex"]==1 &
   df[t,"recent.fx"]==1) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.spine.no.fls.male[df[t,"id"]]
}


}

#  no.fls, male, last fx hip
if(df[t,"intervention"]==1 &
   df[t,"sex"]==1 &
   df[t,"recent.fx"]==2) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.hip.no.fls.male[df[t,"id"]]
}


}

#  no.fls, male, last fx other
if(df[t,"intervention"]==1 &
   df[t,"sex"]==2 &
   df[t,"recent.fx"]==3) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.other.no.fls.male[df[t,"id"]]
}


}


#  no.fls, female, last fx spine
if(df[t,"intervention"]==1 &
   df[t,"sex"]==2 &
   df[t,"recent.fx"]==1) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.spine.no.fls.female[df[t,"id"]]
}


}

#  no.fls, female, last fx hip
if(df[t,"intervention"]==1 &
   df[t,"sex"]==2 &
   df[t,"recent.fx"]==2) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.hip.no.fls.female[df[t,"id"]]
}


}

#  no.fls, female, last fx other
if(df[t,"intervention"]==1 &
   df[t,"sex"]==2 &
   df[t,"recent.fx"]==3) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.other.no.fls.female[df[t,"id"]]
}


}


#  fls, male, last fx spine
if(df[t,"intervention"]==2 &
   df[t,"sex"]==1 &
   df[t,"recent.fx"]==1) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.spine.fls.male[df[t,"id"]]
}


}

#  fls, male, last fx hip
if(df[t,"intervention"]==2 &
   df[t,"sex"]==1 &
   df[t,"recent.fx"]==2) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.hip.fls.male[df[t,"id"]]
}


}

#  fls, male, last fx other
if(df[t,"intervention"]==2 &
   df[t,"sex"]==2 &
   df[t,"recent.fx"]==3) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.other.fls.male[df[t,"id"]]
}


}


#  fls, female, last fx spine
if(df[t,"intervention"]==2 &
   df[t,"sex"]==2 &
   df[t,"recent.fx"]==1) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.spine.fls.female[df[t,"id"]]
}


}

#  fls, female, last fx hip
if(df[t,"intervention"]==2 &
   df[t,"sex"]==2 &
   df[t,"recent.fx"]==2) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.hip.fls.female[df[t,"id"]]
}


}

#  fls, female, last fx other
if(df[t,"intervention"]==2 &
   df[t,"sex"]==2 &
   df[t,"recent.fx"]==3) {

  if(df[t,"time_med"]==1 & # stating alendronate
 df[t,"medication"]==1){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_alendronate.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating risedronate
df[t,"medication"]==2){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_risedronate.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating strontium
df[t,"medication"]==3){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_strontium.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating ibandronate
df[t,"medication"]==4){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_ibandronate.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.other.fls.female[df[t,"id"]]
}


}
  
  df
  
}

# 4m after starting drug
# 1) if not adhering in previous period, stay not adhering
# 2) if adhering in previous period, assign adherence
# dependent on if monitored, med, and sex
m.fun.adherrence.t4<-function(df=df, t=t, rmultinorm=rmultinorm){


if(df[t,"time_med"]==4){

if(df[t-1,"adhering"]==1){ #1
  df[t,"adhering"]<-1
}
  
if(df[t-1,"adhering"]==2){ #2
 
if(df[t,"monitored"]==1){   
if(df[t,"sex"]==1){ 
  
if(df[t,"medication"] ==1) {#alendronate
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.male[df[t,"id"]]
  }
if(df[t,"medication"] ==2) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.male[df[t,"id"]]
} 
if(df[t,"medication"] ==3) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.male[df[t,"id"]]
  }
if(df[t,"medication"] ==4) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.male[df[t,"id"]]
} 
  if(df[t,"medication"] ==5) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.male[df[t,"id"]]
  }
if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.male[df[t,"id"]]
} 
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.male[df[t,"id"]]
  }
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.male[df[t,"id"]]
} 
  if(df[t,"medication"] ==9) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_romo.male[df[t,"id"]]
  } 
  
}
if(df[t,"sex"]==2){ 
  
if(df[t,"medication"] ==1) {#alendronate
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.female[df[t,"id"]]
  }
if(df[t,"medication"] ==2) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.female[df[t,"id"]]
} 
if(df[t,"medication"] ==3) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.female[df[t,"id"]]
  }
if(df[t,"medication"] ==4) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.female[df[t,"id"]]
} 
  if(df[t,"medication"] ==5) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.female[df[t,"id"]]
  }
if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.female[df[t,"id"]]
} 
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.female[df[t,"id"]]
  }
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.female[df[t,"id"]]
} 
  if(df[t,"medication"] ==9) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_romo.female[df[t,"id"]]
  } 
  
}  
}
  
if(df[t,"monitored"]==2){   
if(df[t,"sex"]==1){ 
  
if(df[t,"medication"] ==1) {#alendronate
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_alendronate.male[df[t,"id"]]
  }
if(df[t,"medication"] ==2) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_risedronate.male[df[t,"id"]]
} 
if(df[t,"medication"] ==3) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_strontium.male[df[t,"id"]]
  }
if(df[t,"medication"] ==4) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_ibandronate.male[df[t,"id"]]
} 
  if(df[t,"medication"] ==5) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_denosumab.male[df[t,"id"]]
  }
if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.male[df[t,"id"]]
} 
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.male[df[t,"id"]]
  }
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.male[df[t,"id"]]
} 
  if(df[t,"medication"] ==9) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_romo.male[df[t,"id"]]
  } 
  
}
if(df[t,"sex"]==2){ 
  
if(df[t,"medication"] ==1) {#alendronate
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_alendronate.female[df[t,"id"]]
  }
if(df[t,"medication"] ==2) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_risedronate.female[df[t,"id"]]
} 
if(df[t,"medication"] ==3) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_strontium.female[df[t,"id"]]
  }
if(df[t,"medication"] ==4) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_ibandronate.female[df[t,"id"]]
} 
  if(df[t,"medication"] ==5) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_denosumab.female[df[t,"id"]]
  }
if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.female[df[t,"id"]]
} 
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.female[df[t,"id"]]
  }
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.female[df[t,"id"]]
} 
  if(df[t,"medication"] ==9) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_romo.female[df[t,"id"]]
  } 
  
}
}
  
}
  
}  
  

  
  
df
}

# 12m after starting drug
# same logic as for 12m
m.fun.adherrence.t12<-function(df=df, t=t, rmultinorm=rmultinorm){


if(df[t,"time_med"]==12){

if(df[t-1,"adhering"]==1){ #1
  df[t,"adhering"]<-1
}
  
if(df[t-1,"adhering"]==2){ #2
 
if(df[t,"monitored"]==1){   
if(df[t,"sex"]==1){ 
  
if(df[t,"medication"] ==1) {#alendronate
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.male[df[t,"id"]]
  }
if(df[t,"medication"] ==2) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.male[df[t,"id"]]
} 
if(df[t,"medication"] ==3) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.male[df[t,"id"]]
  }
if(df[t,"medication"] ==4) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_ibandronate.male[df[t,"id"]]
} 
  if(df[t,"medication"] ==5) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.male[df[t,"id"]]
  }
if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.male[df[t,"id"]]
} 
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.male[df[t,"id"]]
  }
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.male[df[t,"id"]]
} 
  
}
if(df[t,"sex"]==2){ 
  
if(df[t,"medication"] ==1) {#alendronate
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.female[df[t,"id"]]
  }
if(df[t,"medication"] ==2) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.female[df[t,"id"]]
} 
if(df[t,"medication"] ==3) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.female[df[t,"id"]]
  }
if(df[t,"medication"] ==4) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_ibandronate.female[df[t,"id"]]
} 
  if(df[t,"medication"] ==5) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.female[df[t,"id"]]
  }
if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.female[df[t,"id"]]
} 
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.female[df[t,"id"]]
  }
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.female[df[t,"id"]]
} 
  
}  
}
  
if(df[t,"monitored"]==2){   
if(df[t,"sex"]==1){ 
  
if(df[t,"medication"] ==1) {#alendronate
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_alendronate.male[df[t,"id"]]
  }
if(df[t,"medication"] ==2) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_risedronate.male[df[t,"id"]]
} 
if(df[t,"medication"] ==3) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_strontium.male[df[t,"id"]]
  }
if(df[t,"medication"] ==4) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_ibandronate.male[df[t,"id"]]
} 
  if(df[t,"medication"] ==5) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_denosumab.male[df[t,"id"]]
  }
if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.male[df[t,"id"]]
} 
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.male[df[t,"id"]]
  }
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.male[df[t,"id"]]
} 
  
}
if(df[t,"sex"]==2){ 
  
if(df[t,"medication"] ==1) {#alendronate
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_alendronate.female[df[t,"id"]]
  }
if(df[t,"medication"] ==2) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_risedronate.female[df[t,"id"]]
} 
if(df[t,"medication"] ==3) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_strontium.female[df[t,"id"]]
  }
if(df[t,"medication"] ==4) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_ibandronate.female[df[t,"id"]]
} 
  if(df[t,"medication"] ==5) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_denosumab.female[df[t,"id"]]
  }
if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.female[df[t,"id"]]
} 
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.female[df[t,"id"]]
  }
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.female[df[t,"id"]]
} 
  
}
}
  
}
  
}  
  

  
  
df
}

# 24m/ 36m/ 48m after starting drug
# 1) if not adhering in previous period, stay not adhering
# 2) if adhering in previous period, assign adherence
# dependent on med, and sex

m.fun.adherrence.t24_36_48<-function(df=df, t=t, rmultinorm=rmultinorm){
if(df[t,"time_med"]==24| 
  df[t,"time_med"]==36| 
  df[t,"time_med"]==48){

if(df[t-1,"adhering"]==1){ #1
  df[t,"adhering"]<-1
}
  
if(df[t-1,"adhering"]==2){ #2
 
if(df[t,"sex"]==1){ 
  
if(df[t,"medication"] ==1) {#alendronate
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.male[df[t,"id"]]
  }
if(df[t,"medication"] ==2) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.male[df[t,"id"]]
} 
if(df[t,"medication"] ==3) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.male[df[t,"id"]]
  }
if(df[t,"medication"] ==4) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.male[df[t,"id"]]
} 
  if(df[t,"medication"] ==5) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.male[df[t,"id"]]
  }
if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.male[df[t,"id"]]
} 

  
}

if(df[t,"sex"]==2){ 
  
if(df[t,"medication"] ==1) {#alendronate
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.female[df[t,"id"]]
  }
if(df[t,"medication"] ==2) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.female[df[t,"id"]]
} 
if(df[t,"medication"] ==3) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.female[df[t,"id"]]
  }
if(df[t,"medication"] ==4) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.female[df[t,"id"]]
} 
  if(df[t,"medication"] ==5) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.female[df[t,"id"]]
  }
if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.female[df[t,"id"]]
} 

  
}
  
  
  
}
  
}  

df
}

# m.fun.adherrence<-function(df=df, t=t, rmultinorm=rmultinorm){
#   
#   # 1 - not adhering
# # 2 - adhering
# 
# #first four months adhering
# 
# # at 4 months
# if(t==5){  
# if(df[t,"monitored"]==2){ 
# if(df[t,"sex"]==1){ 
#   
# if(df[t,"medication"]==2){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_alendronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==3){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_risedronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==4){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_strontium.male[df[t,"id"]]
# }else if(df[t,"medication"]==5){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_denosumab.male[df[t,"id"]]
# }else if(df[t,"medication"]==6){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==7){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.male[df[t,"id"]]
# }else if(df[t,"medication"]==8){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.male[df[t,"id"]]
# }else if(df[t,"medication"]==9){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_romo.male[df[t,"id"]]
# } else{
#   df[t,"adhering"]<-2
# }
#   
#   }}}  
#   
# if(t==5){  
# if(df[t,"monitored"]==2){ 
# if(df[t,"sex"]==2){ 
#   
# if(df[t,"medication"]==2){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_alendronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==3){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_risedronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==4){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_strontium.female[df[t,"id"]]
# }else if(df[t,"medication"]==5){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_denosumab.female[df[t,"id"]]
# }else if(df[t,"medication"]==6){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==7){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.female[df[t,"id"]]
# }else if(df[t,"medication"]==8){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.female[df[t,"id"]]
# }else if(df[t,"medication"]==9){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_romo.female[df[t,"id"]]
# } else{
#   df[t,"adhering"]<-2
# }
#   
#   }}}  
# 
# if(t==5){  
# if(df[t,"monitored"]==1){ 
# if(df[t,"sex"]==1){ 
#   
# if(df[t,"medication"]==2){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==3){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==4){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.male[df[t,"id"]]
# }else if(df[t,"medication"]==5){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.male[df[t,"id"]]
# }else if(df[t,"medication"]==6){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==7){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.male[df[t,"id"]]
# }else if(df[t,"medication"]==8){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.male[df[t,"id"]]
# }else if(df[t,"medication"]==9){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_romo.male[df[t,"id"]]
# } else{
#   df[t,"adhering"]<-2
# }
#   
#   }}}  
#   
# if(t==5){  
# if(df[t,"monitored"]==1){ 
# if(df[t,"sex"]==2){ 
#   
# if(df[t,"medication"]==2){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==3){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==4){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.female[df[t,"id"]]
# }else if(df[t,"medication"]==5){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.female[df[t,"id"]]
# }else if(df[t,"medication"]==6){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==7){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.female[df[t,"id"]]
# }else if(df[t,"medication"]==8){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.female[df[t,"id"]]
# }else if(df[t,"medication"]==9){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_romo.female[df[t,"id"]]
# } else{
#   df[t,"adhering"]<-2
# }
#   
#   }}}  
# 
# 
# # 5 to 12 months- same as previous
# if(t>5 & t<13 ){
# df[t,"adhering"]<-df[t-1,"adhering"]
# }
# 
# if(t==13){  
# if(df[t,"monitored"]==2){ 
# if(df[t,"sex"]==1){ 
#   
# if(df[t,"medication"]==2){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_alendronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==3){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_risedronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==4){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_strontium.male[df[t,"id"]]
# }else if(df[t,"medication"]==5){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_denosumab.male[df[t,"id"]]
# }else if(df[t,"medication"]==6){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==7){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.male[df[t,"id"]]
# }else if(df[t,"medication"]==8){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.male[df[t,"id"]]
# } else{
#   df[t,"adhering"]<-2
# }
#   
#   }}}  
#   
# if(t==13){  
# if(df[t,"monitored"]==2){ 
# if(df[t,"sex"]==2){ 
#   
# if(df[t,"medication"]==2){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_alendronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==3){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_risedronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==4){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_strontium.female[df[t,"id"]]
# }else if(df[t,"medication"]==5){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_denosumab.female[df[t,"id"]]
# }else if(df[t,"medication"]==6){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==7){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.female[df[t,"id"]]
# }else if(df[t,"medication"]==8){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.female[df[t,"id"]]
# } else{
#   df[t,"adhering"]<-2
# }
#   
#   }}}  
# 
# if(t==13){  
# if(df[t,"monitored"]==1){ 
# if(df[t,"sex"]==1){ 
#   
# if(df[t,"medication"]==2){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==3){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==4){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.male[df[t,"id"]]
# }else if(df[t,"medication"]==5){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.male[df[t,"id"]]
# }else if(df[t,"medication"]==6){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.male[df[t,"id"]]
# }else if(df[t,"medication"]==7){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.male[df[t,"id"]]
# }else if(df[t,"medication"]==8){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.male[df[t,"id"]]
# } else{
#   df[t,"adhering"]<-2
# }
#   
#   }}}  
#   
# if(t==13){  
# if(df[t,"monitored"]==1){ 
# if(df[t,"sex"]==2){ 
#   
# if(df[t,"medication"]==2){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==3){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==4){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.female[df[t,"id"]]
# }else if(df[t,"medication"]==5){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.female[df[t,"id"]]
# }else if(df[t,"medication"]==6){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.female[df[t,"id"]]
# }else if(df[t,"medication"]==7){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.female[df[t,"id"]]
# }else if(df[t,"medication"]==8){   
# df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.female[df[t,"id"]]
# } else{
#   df[t,"adhering"]<-2
# }
#   
#   }}} 
# 
# # after 12 months- same as previous (unless replaced below because of drop in annual adh)
# if(t>13){
# df[t,"adhering"]<-df[t-1,"adhering"]
# }
# 
# 
# 
# 
# 
# # after 12 months of the model
# # adh- stay as before, but at 24, 36, and 48 months on a treatment 
# # add in annual decline
# 
# # 24 months alendronate
# if(t>26){ # time over 26
#   
# if(all(df[(t-24):t,"medication"]==1)){  # have they been on drug for at least 24 months?
# if(df[(t-25),"medication"]!=1){  # but weren't 25 months (i.e. started 24 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 36 months alendronate
# if(t>38){ 
#   
# if(all(df[(t-36):t,"medication"]==1)){  # have they been on drug for at least 36 months?
# if(df[(t-37),"medication"]!=1){  # but weren't 37 months (i.e. started 36 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 48 months alendronate
# if(t>50){ 
#   
# if(all(df[(t-48):t,"medication"]==1)){  # have they been on drug for at least 48 months?
# if(df[(t-49),"medication"]!=1){  # but weren't 49 months (i.e. started 48 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# 
# 
# # 24 months risedronate
# if(t>26){ # time over 26
#   
# if(all(df[(t-24):t,"medication"]==2)){  # have they been on drug for at least 24 months?
# if(df[(t-25),"medication"]!=2){  # but weren't 25 months (i.e. started 24 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 36 months risedronate
# if(t>38){ 
#   
# if(all(df[(t-36):t,"medication"]==2)){  # have they been on drug for at least 36 months?
# if(df[(t-37),"medication"]!=2){  # but weren't 37 months (i.e. started 36 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 48 months risedronate
# if(t>50){ 
#   
# if(all(df[(t-48):t,"medication"]==2)){  # have they been on drug for at least 48 months?
# if(df[(t-49),"medication"]!=2){  # but weren't 49 months (i.e. started 48 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# 
# # 24 months strontium
# if(t>26){ # time over 26
#   
# if(all(df[(t-24):t,"medication"]==3)){  # have they been on drug for at least 24 months?
# if(df[(t-25),"medication"]!=3){  # but weren't 25 months (i.e. started 24 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 36 months strontium
# if(t>38){ 
#   
# if(all(df[(t-36):t,"medication"]==3)){  # have they been on drug for at least 36 months?
# if(df[(t-37),"medication"]!=3){  # but weren't 37 months (i.e. started 36 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 48 months strontium
# if(t>50){ 
#   
# if(all(df[(t-48):t,"medication"]==3)){  # have they been on drug for at least 48 months?
# if(df[(t-49),"medication"]!=3){  # but weren't 49 months (i.e. started 48 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# 
# 
# # 24 months ibandronate
# if(t>26){ # time over 26
#   
# if(all(df[(t-24):t,"medication"]==4)){  # have they been on drug for at least 24 months?
# if(df[(t-25),"medication"]!=4){  # but weren't 25 months (i.e. started 24 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 36 months ibandronate
# if(t>38){ 
#   
# if(all(df[(t-36):t,"medication"]==4)){  # have they been on drug for at least 36 months?
# if(df[(t-37),"medication"]!=4){  # but weren't 37 months (i.e. started 36 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 48 months ibandronate
# if(t>50){ 
#   
# if(all(df[(t-48):t,"medication"]==4)){  # have they been on drug for at least 48 months?
# if(df[(t-49),"medication"]!=4){  # but weren't 49 months (i.e. started 48 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# 
# 
# 
# # 24 months denosumab
# if(t>26){ # time over 26
#   
# if(all(df[(t-24):t,"medication"]==5)){  # have they been on drug for at least 24 months?
# if(df[(t-25),"medication"]!=5){  # but weren't 25 months (i.e. started 24 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 36 months denosumab
# if(t>38){ 
#   
# if(all(df[(t-36):t,"medication"]==5)){  # have they been on drug for at least 36 months?
# if(df[(t-37),"medication"]!=5){  # but weren't 37 months (i.e. started 36 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 48 months denosumab
# if(t>50){ 
#   
# if(all(df[(t-48):t,"medication"]==5)){  # have they been on drug for at least 48 months?
# if(df[(t-49),"medication"]!=5){  # but weren't 49 months (i.e. started 48 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# 
# 
# # 24 months zoledronate
# if(t>26){ # time over 26
#   
# if(all(df[(t-24):t,"medication"]==6)){  # have they been on drug for at least 24 months?
# if(df[(t-25),"medication"]!=6){  # but weren't 25 months (i.e. started 24 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 36 months zoledronate
# if(t>38){ 
#   
# if(all(df[(t-36):t,"medication"]==6)){  # have they been on drug for at least 36 months?
# if(df[(t-37),"medication"]!=6){  # but weren't 37 months (i.e. started 36 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
# # 48 months zoledronate
# if(t>50){ 
#   
# if(all(df[(t-48):t,"medication"]==6)){  # have they been on drug for at least 48 months?
# if(df[(t-49),"medication"]!=6){  # but weren't 49 months (i.e. started 48 months ago)
#   
# if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.male[df[t,"id"]]
#   }
#     
#  if(df[t,"intervention"]==1 &  # no.fls.
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.female[df[t,"id"]]
#   }
#      
#   if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==1){     # male
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.male[df[t,"id"]]
#   } 
#   
#    if(df[t,"intervention"]==2 &  # fls
# df[t,"sex"]==2){     # female
#  df[t,"adhering"]<- rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.female[df[t,"id"]]
#   }  
#   
#   
# }}}
#   
#   
#   df
# }

# hospitalised ------
m.fun.hospitalised<-function(df=df, t=t, rmultinorm=rmultinorm, input=input){
   #browser()
#save(list=ls(), file="mylocals.Rda")
#load("mylocals.Rda")

  # if fx in current state assign to be hospitalised or not
  # if not fx in  current state assign to be not
  
  # 1 not hospitalised, 2) hopsitalised (for spine inpatient or outpatient)

if(df[t,"c_af"]==1){

if(df[t,"s_hf"]==1){
   df[t,"hospitalised"]<-rmultinorm$rmultinorm.fx.hosp.hip[df[t,"id"]]
}
if(df[t,"s_sf"]==1){
   df[t,"hospitalised"]<-rmultinorm$rmultinorm.fx.hosp.spine[df[t,"id"]]
} 
  if(df[t,"s_of"]==1){ #other
   df[t,"hospitalised"]<-rmultinorm$rmultinorm.fx.hosp.other[df[t,"id"]]
}
  }


if(df[t,"c_af"]!=1){
  df[t,"hospitalised"]<-1
}
  
  df
  }
# procedure ------  
m.fun.procedure<-function(df=df, t=t, rmultinorm=rmultinorm, input=input){ 
  # 1 none
  # 2 hip surgery
  # 3 other surgery
  # 4 kyphoplasty
  # 5 vertebroplasty
  
  # only if hospialised for hip and other
  # 1 set of probs for spine if hospitalised
  # another if not hospitalised
  
  # set to 1, and replace where required
  
df[t,"procedure"]<-1
  

if(df[t,"hospitalised"]==2){
  
if(df[t,"s_hf"]==1){
if(rmultinorm$rmultinorm.fx.hosp.surgery.hip[df[t,"id"]]==2){
df[t,"procedure"]<-2
}}
  
if(df[t,"s_of"]==1){
if(rmultinorm$rmultinorm.fx.hosp.surgery.other[df[t,"id"]]==2){
df[t,"procedure"]<-3
}}  
  
if(df[t,"s_sf"]==1){
if(rmultinorm$rmultinorm.fx.hosp.surgery.spine[df[t,"id"]]==2){ #kyphoplasty
df[t,"procedure"]<-4
}
if(rmultinorm$rmultinorm.fx.hosp.surgery.spine[df[t,"id"]]==3){ #vertebroplasty
df[t,"procedure"]<-5
}  
}
  
}  

# spine- not hospitalised
if(df[t,"hospitalised"]==1){
if(df[t,"s_sf"]==1){
  
if(rmultinorm$rmultinorm.fx.not.admitted.surgery.spine[df[t,"id"]]==2){ #kyphoplasty
df[t,"procedure"]<-4
}
if(rmultinorm$rmultinorm.fx.not.admitted.surgery.spine[df[t,"id"]]==3){ #vertebroplasty
df[t,"procedure"]<-5
}   
  
}}
    
 df  
}
  

# temp rehab -----
m.fun.temp.rehab<-function(df=df, t=t, rmultinorm=rmultinorm, input=input){
 # browser()
 
  #1 no
  #2 yes, visits temp rehab
# 
#   # for first time period with index fx
#   # assign temp.rehab
if(t==1){
     if(df[t,"s_hf"]==1){
     df[t,"temp.rehab"]<- ifelse(rmultinorm$rmultinorm.discharged.hip.all[df[t,"id"]]==1,
                               2,1)
    }
   if(df[t,"s_sf"]==1){
     df[t,"temp.rehab"]<- ifelse(rmultinorm$rmultinorm.discharged.spine.all[df[t,"id"]]==1,
                               2,1)
      }
   if(df[t,"s_of"]==1){
     df[t,"temp.rehab"]<- ifelse(rmultinorm$rmultinorm.discharged.other.all[df[t,"id"]]==1,
                               2,1)
   }
}
 # browser()

# 
#   # for subsequent fx
  if(t>=2){ 
  if(df[t,"c_af"] == 1){ 
     if(df[t,"s_hf"]==1){
     df[t,"temp.rehab"]<- ifelse(rmultinorm$rmultinorm.discharged.hip.all[df[t,"id"]]==1,
                               2,1)
     }
    if(df[t,"s_sf"]==1){
     df[t,"temp.rehab"]<- ifelse(rmultinorm$rmultinorm.discharged.spine.all[df[t,"id"]]==1,
                               2,1)
       }
   if(df[t,"s_of"]==1){
     df[t,"temp.rehab"]<- ifelse(rmultinorm$rmultinorm.discharged.other.all[df[t,"id"]]==1,
                               2,1)
   }
       }}
# 
  # if not, no
  if(is.na(df[t,"temp.rehab"])){
      df[t,"temp.rehab"]<-1
  }

  df
}




# location -----

m.fun.location<-function(df=df, t=t, rmultinorm=rmultinorm, input=input){
  
# assign location at time of a fracture  
  
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
  
  
  #browser()
  df[t,"location"]<-1
  
if(t==1){
   if(df[t,"s_hf"]==1){
    df[t,"location"]<-  rmultinorm$rmultinorm.discharged.hip.from.home.no.support[df[t,"id"]]
   }
    if(df[t,"location"]==1){
         df[t,"location"]<-  rmultinorm$rmultinorm.discharged.spine.from.home.no.support[df[t,"id"]]
    }
    if(df[t,"location"]==1){
        df[t,"location"]<-  rmultinorm$rmultinorm.discharged.other.from.home.no.support[df[t,"id"]]
   }
}



# subsequently, depends on prior location
#  same as previous

  
  
  
# replace if subsequent fx  

  
  if(t>=2){ 
    if(df[t,"c_af"] != 1){ 
      df[t,"location"]<-   df[t-1,"location"]
    }
    
     if(df[t,"c_af"] == 1){ 
       
if(df[t-1,"location"] == 1){
     if(df[t,"s_hf"]==1){
    df[t,"location"]<-  rmultinorm$rmultinorm.discharged.hip.from.home.support[df[t,"id"]]
   }
    if(df[t,"location"]==1){
         df[t,"location"]<-  rmultinorm$rmultinorm.discharged.spine.from.home.support[df[t,"id"]]
    }
    if(df[t,"location"]==1){
        df[t,"location"]<-  rmultinorm$rmultinorm.discharged.other.from.home.support[df[t,"id"]]
    }}
 
       
if(df[t-1,"location"] == 2){
     if(df[t,"s_hf"]==1){
    df[t,"location"]<-  rmultinorm$rmultinorm.discharged.hip.from.home.no.support[df[t,"id"]]
   }
    if(df[t,"location"]==1){
         df[t,"location"]<-  rmultinorm$rmultinorm.discharged.spine.from.home.no.support[df[t,"id"]]
    }
    if(df[t,"location"]==1){
        df[t,"location"]<-  rmultinorm$rmultinorm.discharged.other.from.home.no.support[df[t,"id"]]
    }}             
       
if(df[t-1,"location"] == 3){
     if(df[t,"s_hf"]==1){
    df[t,"location"]<-  rmultinorm$rmultinorm.discharged.hip.from.family.home[df[t,"id"]]
   }
    if(df[t,"location"]==1){
         df[t,"location"]<-  rmultinorm$rmultinorm.discharged.spine.from.family.home[df[t,"id"]]
    }
    if(df[t,"location"]==1){
        df[t,"location"]<-  rmultinorm$rmultinorm.discharged.other.from.family.home[df[t,"id"]]
    }}  
       
if(df[t-1,"location"] == 4){ # stay in long-term care
df[t,"location"]<- 4
    }       
}
}


df
}






# lab.test -----
m.fun.lab.test<-function(df=df, t=t, rmultinorm=rmultinorm, input=input){
  #1 no
  #2 yes
  
  df[t,"lab.test"]<-1 # will replace below if yes
  
  if(df[t,"s_hf"]==1 &
     df[t,"intervention"]==1 ){
     df[t,"lab.test"]<-rmultinorm$rmultinorm.lab.test.no.fls.hip[df[t,"id"]]
  }
   if(df[t,"s_sf"]==1 &
     df[t,"intervention"]==1 ){
     df[t,"lab.test"]<- rmultinorm$rmultinorm.lab.test.no.fls.spine[df[t,"id"]]
   }
  
   if(df[t,"s_of"]==1 &
     df[t,"intervention"]==1 ){
     df[t,"lab.test"]<- rmultinorm$rmultinorm.lab.test.no.fls.other[df[t,"id"]]
   }
  
    if(df[t,"s_hf"]==1 &
     df[t,"intervention"]==2 ){
     df[t,"lab.test"]<-rmultinorm$rmultinorm.lab.test.fls.hip[df[t,"id"]]
  }
   if(df[t,"s_sf"]==1 &
     df[t,"intervention"]==2 ){
     df[t,"lab.test"]<- rmultinorm$rmultinorm.lab.test.fls.spine[df[t,"id"]]
   }
  
   if(df[t,"s_of"]==1 &
     df[t,"intervention"]==2 ){
     df[t,"lab.test"]<- rmultinorm$rmultinorm.lab.test.fls.other[df[t,"id"]]
  }
  
  
  df
}
# dxa
m.fun.dxa<-function(df=df, t=t, rmultinorm=rmultinorm, input=input){
   #1 no
  #2 yes
  
  df[t,"dxa"]<-1 # will replace below if yes
  
  if(df[t,"s_hf"]==1 &
     df[t,"intervention"]==1 ){
     df[t,"dxa"]<-rmultinorm$rmultinorm.dxa.no.fls.hip[df[t,"id"]]
  }
   if(df[t,"s_sf"]==1 &
     df[t,"intervention"]==1 ){
     df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.no.fls.spine[df[t,"id"]]
   }
  
   if(df[t,"s_of"]==1 &
     df[t,"intervention"]==1 ){
     df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.no.fls.other[df[t,"id"]]
   }
  
    if(df[t,"s_hf"]==1 &
     df[t,"intervention"]==2 ){
     df[t,"dxa"]<-rmultinorm$rmultinorm.dxa.fls.hip[df[t,"id"]]
  }
   if(df[t,"s_sf"]==1 &
     df[t,"intervention"]==2 ){
     df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.fls.spine[df[t,"id"]]
   }
  
   if(df[t,"s_of"]==1 &
     df[t,"intervention"]==2 ){
     df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.fls.other[df[t,"id"]]
  }
  
  
  df
}
