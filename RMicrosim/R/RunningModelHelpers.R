# FUNCTIONS FOR HELPING TO RUN MODEL
# USED in run_model
####################

####################



## sample ----
## get random numbers ----
get.sample<-function(probs){
   # set.seed(123)
     sample.int(length(probs),
         replace = TRUE,
         n_microsimulation*6,
          probs)
} # when not specific to fls, no fls

get.sample.no.fls_fls<-function(probs.no.fls, probs.fls){
   # set.seed(123)
  rand<-runif(n_microsimulation*6, min=0, max=10000000)/10000000
  # random draw between 0 and 1
  
  # no.fls
  if(length(probs.no.fls)==2){
   no.fls.sample<-ifelse(rand<probs.no.fls[1], 1,
            ifelse(rand>=(1-probs.no.fls[2]), 2, NA))}
    
  if(length(probs.no.fls)==3){
    no.fls.sample<-ifelse(rand<probs.no.fls[1], 1,
            ifelse(rand>=probs.no.fls[1]&
                   rand<(1-sum(probs.no.fls[3])), 2,  
            ifelse(rand>=(1-probs.no.fls[3]), 3, NA)))}
  
  if(length(probs.no.fls)==4){
    no.fls.sample<-ifelse(rand<probs.no.fls[1], 1,
            ifelse(rand>=probs.no.fls[1]&
                   rand<(1-sum(probs.no.fls[3:4])), 2,  
            ifelse(rand>=sum(probs.no.fls[1:2])&
                   rand<(1-sum(probs.no.fls[4])), 3,    
            ifelse(rand>=(1-probs.no.fls[4]), 4, NA))))}
  
  if(length(probs.no.fls)==5){
    no.fls.sample<-ifelse(rand<probs.no.fls[1], 1,
            ifelse(rand>=probs.no.fls[1]&
                   rand<(1-sum(probs.no.fls[3:5])), 2,  
            ifelse(rand>=sum(probs.no.fls[1:2])&
                   rand<(1-sum(probs.no.fls[4:5])), 3,       
            ifelse(rand>=sum(probs.no.fls[1:3])&
                   rand<(1-sum(probs.no.fls[5])), 4,      
            ifelse(rand>=(1-probs.no.fls[5]), 5, NA)))))}
  
  if(length(probs.no.fls)==6){
    no.fls.sample<-ifelse(rand<probs.no.fls[1], 1,
            ifelse(rand>=probs.no.fls[1]&
                   rand<(1-sum(probs.no.fls[3:6])), 2,  
            ifelse(rand>=sum(probs.no.fls[1:2])&
                   rand<(1-sum(probs.no.fls[4:6])), 3,       
            ifelse(rand>=sum(probs.no.fls[1:3])&
                   rand<(1-sum(probs.no.fls[5:6])), 4,       
            ifelse(rand>=sum(probs.no.fls[1:4])&
                   rand<(1-sum(probs.no.fls[6])), 5,      
            ifelse(rand>=(1-probs.no.fls[6]), 6, NA))))))}
  
  if(length(probs.no.fls)==7){
    no.fls.sample<-ifelse(rand<probs.no.fls[1], 1,
            ifelse(rand>=probs.no.fls[1]&
                   rand<(1-sum(probs.no.fls[3:7])), 2,  
            ifelse(rand>=sum(probs.no.fls[1:2])&
                   rand<(1-sum(probs.no.fls[4:7])), 3,       
            ifelse(rand>=sum(probs.no.fls[1:3])&
                   rand<(1-sum(probs.no.fls[5:7])), 4,       
            ifelse(rand>=sum(probs.no.fls[1:4])&
                   rand<(1-sum(probs.no.fls[6:7])), 5,      
            ifelse(rand>=sum(probs.no.fls[1:5])&
                   rand<(1-sum(probs.no.fls[7])), 6,
            ifelse(rand>=(1-probs.no.fls[7]), 7, NA)))))))}

  if(length(probs.no.fls)==8){
    no.fls.sample<-ifelse(rand<probs.no.fls[1], 1,
            ifelse(rand>=probs.no.fls[1]&
                   rand<(1-sum(probs.no.fls[3:8])), 2,  
            ifelse(rand>=sum(probs.no.fls[1:2])&
                   rand<(1-sum(probs.no.fls[4:8])), 3,       
            ifelse(rand>=sum(probs.no.fls[1:3])&
                   rand<(1-sum(probs.no.fls[5:8])), 4,       
            ifelse(rand>=sum(probs.no.fls[1:4])&
                   rand<(1-sum(probs.no.fls[6:8])), 5,      
            ifelse(rand>=sum(probs.no.fls[1:5])&
                   rand<(1-sum(probs.no.fls[7:8])), 6,
            ifelse(rand>=sum(probs.no.fls[1:6])&
                   rand<(1-sum(probs.no.fls[8])), 7,        
            ifelse(rand>=(1-probs.no.fls[8]), 8, NA))))))))}

  if(length(probs.no.fls)==9){
    no.fls.sample<-ifelse(rand<probs.no.fls[1], 1,
            ifelse(rand>=probs.no.fls[1]&
                   rand<(1-sum(probs.no.fls[3:9])), 2,  
            ifelse(rand>=sum(probs.no.fls[1:2])&
                   rand<(1-sum(probs.no.fls[4:9])), 3,       
            ifelse(rand>=sum(probs.no.fls[1:3])&
                   rand<(1-sum(probs.no.fls[5:9])), 4,       
            ifelse(rand>=sum(probs.no.fls[1:4])&
                   rand<(1-sum(probs.no.fls[6:9])), 5,      
            ifelse(rand>=sum(probs.no.fls[1:5])&
                   rand<(1-sum(probs.no.fls[7:9])), 6,
            ifelse(rand>=sum(probs.no.fls[1:6])&
                   rand<(1-sum(probs.no.fls[8:9])), 7,        
            ifelse(rand>=sum(probs.no.fls[1:7]) &
                   rand<(1-probs.no.fls[9]), 8,
            ifelse(rand>=(1-probs.no.fls[9]), 9, NA)))))))))}
  
    if(length(probs.no.fls)==10){
    no.fls.sample<-ifelse(rand<probs.no.fls[1], 1,
            ifelse(rand>=probs.no.fls[1]&
                   rand<(1-sum(probs.no.fls[3:10])), 2,  
            ifelse(rand>=sum(probs.no.fls[1:2])&
                   rand<(1-sum(probs.no.fls[4:10])), 3,       
            ifelse(rand>=sum(probs.no.fls[1:3])&
                   rand<(1-sum(probs.no.fls[5:10])), 4,       
            ifelse(rand>=sum(probs.no.fls[1:4])&
                   rand<(1-sum(probs.no.fls[6:10])), 5,      
            ifelse(rand>=sum(probs.no.fls[1:5])&
                   rand<(1-sum(probs.no.fls[7:10])), 6,
            ifelse(rand>=sum(probs.no.fls[1:6])&
                   rand<(1-sum(probs.no.fls[8:10])), 7,        
            ifelse(rand>=sum(probs.no.fls[1:7]) &
                   rand<(1-probs.no.fls[9:10]), 8,
            ifelse(rand>=sum(probs.no.fls[1:8]) &
                   rand<(1-probs.no.fls[10]), 9,
            ifelse(rand>=(1-probs.no.fls[10]), 10, NA))))))))))}
  
    # fls
  if(length(probs.fls)==2){
   fls.sample<-ifelse(rand<probs.fls[1], 1,
            ifelse(rand>=(1-probs.fls[2]), 2, NA))}
    
  if(length(probs.fls)==3){
    fls.sample<-ifelse(rand<probs.fls[1], 1,
            ifelse(rand>=probs.fls[1]&
                   rand<(1-sum(probs.fls[3])), 2,  
            ifelse(rand>=(1-probs.fls[3]), 3, NA)))}
  
  if(length(probs.fls)==4){
    fls.sample<-ifelse(rand<probs.fls[1], 1,
            ifelse(rand>=probs.fls[1]&
                   rand<(1-sum(probs.fls[3:4])), 2,  
            ifelse(rand>=sum(probs.fls[1:2])&
                   rand<(1-sum(probs.fls[4])), 3,    
            ifelse(rand>=(1-probs.fls[4]), 4, NA))))}
  
  if(length(probs.fls)==5){
    fls.sample<-ifelse(rand<probs.fls[1], 1,
            ifelse(rand>=probs.fls[1]&
                   rand<(1-sum(probs.fls[3:5])), 2,  
            ifelse(rand>=sum(probs.fls[1:2])&
                   rand<(1-sum(probs.fls[4:5])), 3,       
            ifelse(rand>=sum(probs.fls[1:3])&
                   rand<(1-sum(probs.fls[5])), 4,      
            ifelse(rand>=(1-probs.fls[5]), 5, NA)))))}
  
  if(length(probs.fls)==6){
    fls.sample<-ifelse(rand<probs.fls[1], 1,
            ifelse(rand>=probs.fls[1]&
                   rand<(1-sum(probs.fls[3:6])), 2,  
            ifelse(rand>=sum(probs.fls[1:2])&
                   rand<(1-sum(probs.fls[4:6])), 3,       
            ifelse(rand>=sum(probs.fls[1:3])&
                   rand<(1-sum(probs.fls[5:6])), 4,       
            ifelse(rand>=sum(probs.fls[1:4])&
                   rand<(1-sum(probs.fls[6])), 5,      
            ifelse(rand>=(1-probs.fls[6]), 6, NA))))))}
  
  if(length(probs.fls)==7){
    fls.sample<-ifelse(rand<probs.fls[1], 1,
            ifelse(rand>=probs.fls[1]&
                   rand<(1-sum(probs.fls[3:7])), 2,  
            ifelse(rand>=sum(probs.fls[1:2])&
                   rand<(1-sum(probs.fls[4:7])), 3,       
            ifelse(rand>=sum(probs.fls[1:3])&
                   rand<(1-sum(probs.fls[5:7])), 4,       
            ifelse(rand>=sum(probs.fls[1:4])&
                   rand<(1-sum(probs.fls[6:7])), 5,      
            ifelse(rand>=sum(probs.fls[1:5])&
                   rand<(1-sum(probs.fls[7])), 6,
            ifelse(rand>=(1-probs.fls[7]), 7, NA)))))))}

  if(length(probs.fls)==8){
    fls.sample<-ifelse(rand<probs.fls[1], 1,
            ifelse(rand>=probs.fls[1]&
                   rand<(1-sum(probs.fls[3:8])), 2,  
            ifelse(rand>=sum(probs.fls[1:2])&
                   rand<(1-sum(probs.fls[4:8])), 3,       
            ifelse(rand>=sum(probs.fls[1:3])&
                   rand<(1-sum(probs.fls[5:8])), 4,       
            ifelse(rand>=sum(probs.fls[1:4])&
                   rand<(1-sum(probs.fls[6:8])), 5,      
            ifelse(rand>=sum(probs.fls[1:5])&
                   rand<(1-sum(probs.fls[7:8])), 6,
            ifelse(rand>=sum(probs.fls[1:6])&
                   rand<(1-sum(probs.fls[8])), 7,        
            ifelse(rand>=(1-probs.fls[8]), 8, NA))))))))}

  if(length(probs.fls)==9){
    fls.sample<-ifelse(rand<probs.fls[1], 1,
            ifelse(rand>=probs.fls[1]&
                   rand<(1-sum(probs.fls[3:9])), 2,  
            ifelse(rand>=sum(probs.fls[1:2])&
                   rand<(1-sum(probs.fls[4:9])), 3,       
            ifelse(rand>=sum(probs.fls[1:3])&
                   rand<(1-sum(probs.fls[5:9])), 4,       
            ifelse(rand>=sum(probs.fls[1:4])&
                   rand<(1-sum(probs.fls[6:9])), 5,      
            ifelse(rand>=sum(probs.fls[1:5])&
                   rand<(1-sum(probs.fls[7:9])), 6,
            ifelse(rand>=sum(probs.fls[1:6])&
                   rand<(1-sum(probs.fls[8:9])), 7,        
            ifelse(rand>=sum(probs.fls[1:7]) &
                   rand<(1-probs.fls[9]), 8,
            ifelse(rand>=(1-probs.fls[9]), 9, NA)))))))))}
  
    if(length(probs.fls)==10){
    fls.sample<-ifelse(rand<probs.fls[1], 1,
            ifelse(rand>=probs.fls[1]&
                   rand<(1-sum(probs.fls[3:10])), 2,  
            ifelse(rand>=sum(probs.fls[1:2])&
                   rand<(1-sum(probs.fls[4:10])), 3,       
            ifelse(rand>=sum(probs.fls[1:3])&
                   rand<(1-sum(probs.fls[5:10])), 4,       
            ifelse(rand>=sum(probs.fls[1:4])&
                   rand<(1-sum(probs.fls[6:10])), 5,      
            ifelse(rand>=sum(probs.fls[1:5])&
                   rand<(1-sum(probs.fls[7:10])), 6,
            ifelse(rand>=sum(probs.fls[1:6])&
                   rand<(1-sum(probs.fls[8:10])), 7,
            ifelse(rand>=sum(probs.fls[1:7]) &
                   rand<(1-probs.fls[9:10]), 8,
                        ifelse(rand>=sum(probs.fls[1:8]) &
                   rand<(1-probs.fls[10]), 9,
            ifelse(rand>=(1-probs.fls[10]), 10, NA))))))))))}

  list(no.fls.sample, fls.sample)  
  } # when specific to fls, no fls

get.trans<-function(){
  runif(n_microsimulation*6, min=0, max=10000000)/10000000
}

get.rmultinorm<-function(input, n_microsimulation){
  rmultinorm<-NULL
  # browser()
  
# trt type ------
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.spine.male,
            input$no.fls.trt.risedronate.spine.male,
            input$no.fls.trt.strontium.spine.male,
            input$no.fls.trt.ibandronate.spine.male,
            input$no.fls.trt.raloxifene.spine.male,
            input$no.fls.trt.denosumab.spine.male,
            input$no.fls.trt.zoledronate.spine.male,
            input$no.fls.trt.teriparatide.spine.male,
            input$no.fls.trt.abaloparatide.spine.male,
            input$no.fls.trt.romo.spine.male),
   probs.fls=c(input$fls.trt.alendronate.spine.male,
            input$fls.trt.risedronate.spine.male,
            input$fls.trt.strontium.spine.male,
            input$fls.trt.ibandronate.spine.male,
            input$fls.trt.raloxifene.spine.male,
            input$fls.trt.denosumab.spine.male,
            input$fls.trt.zoledronate.spine.male,
            input$fls.trt.teriparatide.spine.male,
            input$fls.trt.abaloparatide.spine.male,
            input$fls.trt.romo.spine.male)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.spine.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.spine.male<-unlist(sample[2])
 
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.hip.male,
            input$no.fls.trt.risedronate.hip.male,
            input$no.fls.trt.strontium.hip.male,
            input$no.fls.trt.ibandronate.hip.male,
            input$no.fls.trt.raloxifene.hip.male,
            input$no.fls.trt.denosumab.hip.male,
            input$no.fls.trt.zoledronate.hip.male,
            input$no.fls.trt.teriparatide.hip.male,
            input$no.fls.trt.abaloparatide.hip.male,
            input$no.fls.trt.romo.hip.male),
   probs.fls=c(input$fls.trt.alendronate.hip.male,
            input$fls.trt.risedronate.hip.male,
            input$fls.trt.strontium.hip.male,
            input$fls.trt.ibandronate.hip.male,
            input$fls.trt.raloxifene.hip.male,
            input$fls.trt.denosumab.hip.male,
            input$fls.trt.zoledronate.hip.male,
            input$fls.trt.teriparatide.hip.male,
            input$fls.trt.abaloparatide.hip.male,
            input$fls.trt.romo.hip.male)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.hip.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.hip.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.other.male,
            input$no.fls.trt.risedronate.other.male,
            input$no.fls.trt.strontium.other.male,
            input$no.fls.trt.ibandronate.other.male,
            input$no.fls.trt.raloxifene.other.male,
            input$no.fls.trt.denosumab.other.male,
            input$no.fls.trt.zoledronate.other.male,
            input$no.fls.trt.teriparatide.other.male,
            input$no.fls.trt.abaloparatide.other.male,
            input$no.fls.trt.romo.other.male),
   probs.fls=c(input$fls.trt.alendronate.other.male,
            input$fls.trt.risedronate.other.male,
            input$fls.trt.strontium.other.male,
            input$fls.trt.ibandronate.other.male,
            input$fls.trt.raloxifene.other.male,
            input$fls.trt.denosumab.other.male,
            input$fls.trt.zoledronate.other.male,
            input$fls.trt.teriparatide.other.male,
            input$fls.trt.abaloparatide.other.male,
            input$fls.trt.romo.other.male)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.other.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.other.male<-unlist(sample[2])





sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.spine.female,
            input$no.fls.trt.risedronate.spine.female,
            input$no.fls.trt.strontium.spine.female,
            input$no.fls.trt.ibandronate.spine.female,
            input$no.fls.trt.raloxifene.spine.female,
            input$no.fls.trt.denosumab.spine.female,
            input$no.fls.trt.zoledronate.spine.female,
            input$no.fls.trt.teriparatide.spine.female,
            input$no.fls.trt.abaloparatide.spine.female,
            input$no.fls.trt.romo.spine.female),
   probs.fls=c(input$fls.trt.alendronate.spine.female,
            input$fls.trt.risedronate.spine.female,
            input$fls.trt.strontium.spine.female,
            input$fls.trt.ibandronate.spine.female,
            input$fls.trt.raloxifene.spine.female,
            input$fls.trt.denosumab.spine.female,
            input$fls.trt.zoledronate.spine.female,
            input$fls.trt.teriparatide.spine.female,
            input$fls.trt.abaloparatide.spine.female,
            input$fls.trt.romo.spine.female)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.spine.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.spine.female<-unlist(sample[2])
 
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.hip.female,
            input$no.fls.trt.risedronate.hip.female,
            input$no.fls.trt.strontium.hip.female,
            input$no.fls.trt.ibandronate.hip.female,
            input$no.fls.trt.raloxifene.hip.female,
            input$no.fls.trt.denosumab.hip.female,
            input$no.fls.trt.zoledronate.hip.female,
            input$no.fls.trt.teriparatide.hip.female,
            input$no.fls.trt.abaloparatide.hip.female,
            input$no.fls.trt.romo.hip.female),
   probs.fls=c(input$fls.trt.alendronate.hip.female,
            input$fls.trt.risedronate.hip.female,
            input$fls.trt.strontium.hip.female,
            input$fls.trt.ibandronate.hip.female,
            input$fls.trt.raloxifene.hip.female,
            input$fls.trt.denosumab.hip.female,
            input$fls.trt.zoledronate.hip.female,
            input$fls.trt.teriparatide.hip.female,
            input$fls.trt.abaloparatide.hip.female,
            input$fls.trt.romo.hip.female)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.hip.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.hip.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.other.female,
            input$no.fls.trt.risedronate.other.female,
            input$no.fls.trt.strontium.other.female,
            input$no.fls.trt.ibandronate.other.female,
            input$no.fls.trt.raloxifene.other.female,
            input$no.fls.trt.denosumab.other.female,
            input$no.fls.trt.zoledronate.other.female,
            input$no.fls.trt.teriparatide.other.female,
            input$no.fls.trt.abaloparatide.other.female,
            input$no.fls.trt.romo.other.female),
   probs.fls=c(input$fls.trt.alendronate.other.female,
            input$fls.trt.risedronate.other.female,
            input$fls.trt.strontium.other.female,
            input$fls.trt.ibandronate.other.female,
            input$fls.trt.raloxifene.other.female,
            input$fls.trt.denosumab.other.female,
            input$fls.trt.zoledronate.other.female,
            input$fls.trt.teriparatide.other.female,
            input$fls.trt.abaloparatide.other.female,
            input$fls.trt.romo.other.female)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.other.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.other.female<-unlist(sample[2])

# identification  ------

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.prob_identification.spine,
            input$no.fls.prob_identification.spine),
   probs.fls=c(1-input$fls.prob_identification.spine,
            input$fls.prob_identification.spine))  
rmultinorm$rmultinorm.no.fls.prob_identification.spine<-unlist(sample[1])
rmultinorm$rmultinorm.fls.prob_identification.spine<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.prob_identification.hip,
            input$no.fls.prob_identification.hip),
   probs.fls=c(1-input$fls.prob_identification.hip,
            input$fls.prob_identification.hip))  
rmultinorm$rmultinorm.no.fls.prob_identification.hip<-unlist(sample[1])
rmultinorm$rmultinorm.fls.prob_identification.hip<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.prob_identification.other,
            input$no.fls.prob_identification.other),
   probs.fls=c(1-input$fls.prob_identification.other,
            input$fls.prob_identification.other))  
rmultinorm$rmultinorm.no.fls.prob_identification.other<-unlist(sample[1])
rmultinorm$rmultinorm.fls.prob_identification.other<-unlist(sample[2])


# trt  ------


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.spine.male,
            input$no.fls.trt.spine.male),
   probs.fls=c(1-input$fls.trt.spine.male,
            input$fls.trt.spine.male))  
rmultinorm$rmultinorm.no.fls.trt.spine.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.spine.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.hip.male,
            input$no.fls.trt.hip.male),
   probs.fls=c(1-input$fls.trt.hip.male,
            input$fls.trt.hip.male))  
rmultinorm$rmultinorm.no.fls.trt.hip.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.hip.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.other.male,
            input$no.fls.trt.other.male),
   probs.fls=c(1-input$fls.trt.other.male,
            input$fls.trt.other.male))  
rmultinorm$rmultinorm.no.fls.trt.other.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.other.male<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.spine.female,
            input$no.fls.trt.spine.female),
   probs.fls=c(1-input$fls.trt.spine.female,
            input$fls.trt.spine.female))  
rmultinorm$rmultinorm.no.fls.trt.spine.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.spine.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.hip.female,
            input$no.fls.trt.hip.female),
   probs.fls=c(1-input$fls.trt.hip.female,
            input$fls.trt.hip.female))  
rmultinorm$rmultinorm.no.fls.trt.hip.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.hip.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.other.female,
            input$no.fls.trt.other.female),
   probs.fls=c(1-input$fls.trt.other.female,
            input$fls.trt.other.female))  
rmultinorm$rmultinorm.no.fls.trt.other.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.other.female<-unlist(sample[2])


# trt - risk profiles ------

# low.risk
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.spine.male.low.risk,
            input$no.fls.trt.spine.male.low.risk),
   probs.fls=c(1-input$fls.trt.spine.male.low.risk,
            input$fls.trt.spine.male.low.risk))  
rmultinorm$rmultinorm.no.fls.trt.spine.male.low.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.spine.male.low.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.hip.male.low.risk,
            input$no.fls.trt.hip.male.low.risk),
   probs.fls=c(1-input$fls.trt.hip.male.low.risk,
            input$fls.trt.hip.male.low.risk))  
rmultinorm$rmultinorm.no.fls.trt.hip.male.low.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.hip.male.low.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.other.male.low.risk,
            input$no.fls.trt.other.male.low.risk),
   probs.fls=c(1-input$fls.trt.other.male.low.risk,
            input$fls.trt.other.male.low.risk))  
rmultinorm$rmultinorm.no.fls.trt.other.male.low.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.other.male.low.risk<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.spine.female.low.risk,
            input$no.fls.trt.spine.female.low.risk),
   probs.fls=c(1-input$fls.trt.spine.female.low.risk,
            input$fls.trt.spine.female.low.risk))  
rmultinorm$rmultinorm.no.fls.trt.spine.female.low.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.spine.female.low.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.hip.female.low.risk,
            input$no.fls.trt.hip.female.low.risk),
   probs.fls=c(1-input$fls.trt.hip.female.low.risk,
            input$fls.trt.hip.female.low.risk))  
rmultinorm$rmultinorm.no.fls.trt.hip.female.low.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.hip.female.low.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.other.female.low.risk,
            input$no.fls.trt.other.female.low.risk),
   probs.fls=c(1-input$fls.trt.other.female.low.risk,
            input$fls.trt.other.female.low.risk))  
rmultinorm$rmultinorm.no.fls.trt.other.female.low.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.other.female.low.risk<-unlist(sample[2])


# intermediate.risk
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.spine.male.intermediate.risk,
            input$no.fls.trt.spine.male.intermediate.risk),
   probs.fls=c(1-input$fls.trt.spine.male.intermediate.risk,
            input$fls.trt.spine.male.intermediate.risk))  
rmultinorm$rmultinorm.no.fls.trt.spine.male.intermediate.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.spine.male.intermediate.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.hip.male.intermediate.risk,
            input$no.fls.trt.hip.male.intermediate.risk),
   probs.fls=c(1-input$fls.trt.hip.male.intermediate.risk,
            input$fls.trt.hip.male.intermediate.risk))  
rmultinorm$rmultinorm.no.fls.trt.hip.male.intermediate.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.hip.male.intermediate.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.other.male.intermediate.risk,
            input$no.fls.trt.other.male.intermediate.risk),
   probs.fls=c(1-input$fls.trt.other.male.intermediate.risk,
            input$fls.trt.other.male.intermediate.risk))  
rmultinorm$rmultinorm.no.fls.trt.other.male.intermediate.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.other.male.intermediate.risk<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.spine.female.intermediate.risk,
            input$no.fls.trt.spine.female.intermediate.risk),
   probs.fls=c(1-input$fls.trt.spine.female.intermediate.risk,
            input$fls.trt.spine.female.intermediate.risk))  
rmultinorm$rmultinorm.no.fls.trt.spine.female.intermediate.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.spine.female.intermediate.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.hip.female.intermediate.risk,
            input$no.fls.trt.hip.female.intermediate.risk),
   probs.fls=c(1-input$fls.trt.hip.female.intermediate.risk,
            input$fls.trt.hip.female.intermediate.risk))  
rmultinorm$rmultinorm.no.fls.trt.hip.female.intermediate.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.hip.female.intermediate.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.other.female.intermediate.risk,
            input$no.fls.trt.other.female.intermediate.risk),
   probs.fls=c(1-input$fls.trt.other.female.intermediate.risk,
            input$fls.trt.other.female.intermediate.risk))  
rmultinorm$rmultinorm.no.fls.trt.other.female.intermediate.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.other.female.intermediate.risk<-unlist(sample[2])


# high.risk
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.spine.male.high.risk,
            input$no.fls.trt.spine.male.high.risk),
   probs.fls=c(1-input$fls.trt.spine.male.high.risk,
            input$fls.trt.spine.male.high.risk))  
rmultinorm$rmultinorm.no.fls.trt.spine.male.high.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.spine.male.high.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.hip.male.high.risk,
            input$no.fls.trt.hip.male.high.risk),
   probs.fls=c(1-input$fls.trt.hip.male.high.risk,
            input$fls.trt.hip.male.high.risk))  
rmultinorm$rmultinorm.no.fls.trt.hip.male.high.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.hip.male.high.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.other.male.high.risk,
            input$no.fls.trt.other.male.high.risk),
   probs.fls=c(1-input$fls.trt.other.male.high.risk,
            input$fls.trt.other.male.high.risk))  
rmultinorm$rmultinorm.no.fls.trt.other.male.high.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.other.male.high.risk<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.spine.female.high.risk,
            input$no.fls.trt.spine.female.high.risk),
   probs.fls=c(1-input$fls.trt.spine.female.high.risk,
            input$fls.trt.spine.female.high.risk))  
rmultinorm$rmultinorm.no.fls.trt.spine.female.high.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.spine.female.high.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.hip.female.high.risk,
            input$no.fls.trt.hip.female.high.risk),
   probs.fls=c(1-input$fls.trt.hip.female.high.risk,
            input$fls.trt.hip.female.high.risk))  
rmultinorm$rmultinorm.no.fls.trt.hip.female.high.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.hip.female.high.risk<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.other.female.high.risk,
            input$no.fls.trt.other.female.high.risk),
   probs.fls=c(1-input$fls.trt.other.female.high.risk,
            input$fls.trt.other.female.high.risk))  
rmultinorm$rmultinorm.no.fls.trt.other.female.high.risk<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.other.female.high.risk<-unlist(sample[2])

# romo.to  ------


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$romo.to.nothing.no.fls.male,
            input$romo.to.alendronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.risedronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.strontium.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.ibandronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.raloxifene.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.denosumab.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.zoledronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male)),
   probs.fls=c(
            input$romo.to.nothing.fls.male,
            input$romo.to.alendronate.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.risedronate.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.strontium.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.ibandronate.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.raloxifene.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.denosumab.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.zoledronate.fls.male*(1-input$romo.to.nothing.no.fls.male))
   )  

rmultinorm$rmultinorm.romo.to.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.romo.to.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$romo.to.nothing.no.fls.female,
            input$romo.to.alendronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.risedronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.strontium.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.ibandronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.raloxifene.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.denosumab.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.zoledronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female)),
   probs.fls=c(
            input$romo.to.nothing.fls.female,
            input$romo.to.alendronate.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.risedronate.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.strontium.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.ibandronate.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.raloxifene.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.denosumab.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.zoledronate.fls.female*(1-input$romo.to.nothing.no.fls.female))
   )  

rmultinorm$rmultinorm.romo.to.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.romo.to.fls.female<-unlist(sample[2])

# abaloparatide.to  ------

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$abaloparatide.to.nothing.no.fls.male,
            input$abaloparatide.to.alendronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.risedronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.strontium.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.ibandronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.raloxifene.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.denosumab.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.zoledronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male)),
   probs.fls=c(
            input$abaloparatide.to.nothing.fls.male,
            input$abaloparatide.to.alendronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.risedronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.strontium.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.ibandronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.raloxifene.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.denosumab.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.zoledronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male))
   )  

rmultinorm$rmultinorm.abaloparatide.to.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.abaloparatide.to.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$abaloparatide.to.nothing.no.fls.female,
            input$abaloparatide.to.alendronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.risedronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.strontium.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.ibandronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.raloxifene.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.denosumab.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.zoledronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female)),
   probs.fls=c(
            input$abaloparatide.to.nothing.fls.female,
            input$abaloparatide.to.alendronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.risedronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.strontium.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.ibandronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.raloxifene.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.denosumab.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.zoledronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female))
   )  

rmultinorm$rmultinorm.abaloparatide.to.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.abaloparatide.to.fls.female<-unlist(sample[2])



# teriparatide.to  ------
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$teriparatide.to.nothing.no.fls.male,
            input$teriparatide.to.alendronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.risedronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.strontium.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.ibandronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.raloxifene.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.denosumab.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.zoledronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male)),
   probs.fls=c(
            input$teriparatide.to.nothing.fls.male,
            input$teriparatide.to.alendronate.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.risedronate.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.strontium.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.ibandronate.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.raloxifene.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.denosumab.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.zoledronate.fls.male*(1-input$teriparatide.to.nothing.fls.male))
   )  

rmultinorm$rmultinorm.teriparatide.to.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.teriparatide.to.fls.male<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$teriparatide.to.nothing.no.fls.female,
            input$teriparatide.to.alendronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.risedronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.strontium.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.ibandronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.raloxifene.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.denosumab.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.zoledronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female)),
   probs.fls=c(
            input$teriparatide.to.nothing.fls.female,
            input$teriparatide.to.alendronate.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.risedronate.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.strontium.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.ibandronate.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.raloxifene.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.denosumab.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.zoledronate.fls.female*(1-input$teriparatide.to.nothing.fls.female))
   )  

rmultinorm$rmultinorm.teriparatide.to.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.teriparatide.to.fls.female<-unlist(sample[2])



# monitored.4m.to  ------

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.spine.4m.male,
              input$no.fls.monitored.spine.4m.male),
   probs.fls=c(1-input$fls.monitored.spine.4m.male,
              input$fls.monitored.spine.4m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.spine.4m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.spine.4m.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.hip.4m.male,
              input$no.fls.monitored.hip.4m.male),
   probs.fls=c(1-input$fls.monitored.hip.4m.male,
              input$fls.monitored.hip.4m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.hip.4m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.hip.4m.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.other.4m.male,
              input$no.fls.monitored.other.4m.male),
   probs.fls=c(1-input$fls.monitored.other.4m.male,
              input$fls.monitored.other.4m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.other.4m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.other.4m.male<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.spine.4m.female,
              input$no.fls.monitored.spine.4m.female),
   probs.fls=c(1-input$fls.monitored.spine.4m.female,
              input$fls.monitored.spine.4m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.spine.4m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.spine.4m.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.hip.4m.female,
              input$no.fls.monitored.hip.4m.female),
   probs.fls=c(1-input$fls.monitored.hip.4m.female,
              input$fls.monitored.hip.4m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.hip.4m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.hip.4m.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.other.4m.female,
              input$no.fls.monitored.other.4m.female),
   probs.fls=c(1-input$fls.monitored.other.4m.female,
              input$fls.monitored.other.4m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.other.4m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.other.4m.female<-unlist(sample[2])

# monitored.12m.to  ------

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.spine.12m.male,
              input$no.fls.monitored.spine.12m.male),
   probs.fls=c(1-input$fls.monitored.spine.12m.male,
              input$fls.monitored.spine.12m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.spine.12m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.spine.12m.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.hip.12m.male,
              input$no.fls.monitored.hip.12m.male),
   probs.fls=c(1-input$fls.monitored.hip.12m.male,
              input$fls.monitored.hip.12m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.hip.12m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.hip.12m.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.other.12m.male,
              input$no.fls.monitored.other.12m.male),
   probs.fls=c(1-input$fls.monitored.other.12m.male,
              input$fls.monitored.other.12m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.other.12m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.other.12m.male<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.spine.12m.female,
              input$no.fls.monitored.spine.12m.female),
   probs.fls=c(1-input$fls.monitored.spine.12m.female,
              input$fls.monitored.spine.12m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.spine.12m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.spine.12m.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.hip.12m.female,
              input$no.fls.monitored.hip.12m.female),
   probs.fls=c(1-input$fls.monitored.hip.12m.female,
              input$fls.monitored.hip.12m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.hip.12m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.hip.12m.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.other.12m.female,
              input$no.fls.monitored.other.12m.female),
   probs.fls=c(1-input$fls.monitored.other.12m.female,
              input$fls.monitored.other.12m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.other.12m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.other.12m.female<-unlist(sample[2])




# monitored  ------


rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_risedronate.male,
              input$not.monitored.4m_adh_risedronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.male<-
 get.sample(c(1-input$not.monitored.4m_adh_strontium.male,
              input$not.monitored.4m_adh_strontium.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_ibandronate.male,
              input$not.monitored.4m_adh_ibandronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_raloxifene.male<-
 get.sample(c(1-input$not.monitored.4m_adh_raloxifene.male,
              input$not.monitored.4m_adh_raloxifene.male))
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
rmultinorm$rmultinorm.not.monitored.12m_adh_raloxifene.male<-
 get.sample(c(1-input$not.monitored.12m_adh_raloxifene.male,
              input$not.monitored.12m_adh_raloxifene.male))
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
rmultinorm$rmultinorm.not.monitored.4m_adh_raloxifene.female<-
 get.sample(c(1-input$not.monitored.4m_adh_raloxifene.female,
              input$not.monitored.4m_adh_raloxifene.female))
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
rmultinorm$rmultinorm.not.monitored.12m_adh_raloxifene.female<-
 get.sample(c(1-input$not.monitored.12m_adh_raloxifene.female,
              input$not.monitored.12m_adh_raloxifene.female))
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
rmultinorm$rmultinorm.monitored.4m_adh_raloxifene.male<-
 get.sample(c(1-input$monitored.4m_adh_raloxifene.male,
              input$monitored.4m_adh_raloxifene.male))
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
rmultinorm$rmultinorm.monitored.12m_adh_raloxifene.male<-
 get.sample(c(1-input$monitored.12m_adh_raloxifene.male,
              input$monitored.12m_adh_raloxifene.male))
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
rmultinorm$rmultinorm.monitored.4m_adh_raloxifene.female<-
 get.sample(c(1-input$monitored.4m_adh_raloxifene.female,
              input$monitored.4m_adh_raloxifene.female))
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
rmultinorm$rmultinorm.monitored.12m_adh_raloxifene.female<-
 get.sample(c(1-input$monitored.12m_adh_raloxifene.female,
              input$monitored.12m_adh_raloxifene.female))
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

# primary adh  ------


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.spine.no.fls.male,
              input$primary.adh_alendronate.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_alendronate.spine.fls.male,
              input$primary.adh_alendronate.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.spine.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.spine.no.fls.male,
              input$primary.adh_risedronate.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_risedronate.spine.fls.male,
              input$primary.adh_risedronate.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.spine.no.fls.male,
              input$primary.adh_strontium.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_strontium.spine.fls.male,
              input$primary.adh_strontium.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.spine.no.fls.male,
              input$primary.adh_ibandronate.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_ibandronate.spine.fls.male,
              input$primary.adh_ibandronate.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_raloxifene.spine.no.fls.male,
              input$primary.adh_raloxifene.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_raloxifene.spine.fls.male,
              input$primary.adh_raloxifene.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_raloxifene.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_raloxifene.spine.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.spine.no.fls.male,
              input$primary.adh_denosumab.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_denosumab.spine.fls.male,
              input$primary.adh_denosumab.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.spine.no.fls.male,
              input$primary.adh_zoledronate.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_zoledronate.spine.fls.male,
              input$primary.adh_zoledronate.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.spine.no.fls.male,
              input$primary.adh_teriparatide.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_teriparatide.spine.fls.male,
              input$primary.adh_teriparatide.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.spine.no.fls.male,
              input$primary.adh_abaloparatide.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_abaloparatide.spine.fls.male,
              input$primary.adh_abaloparatide.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.spine.no.fls.male,
              input$primary.adh_romo.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_romo.spine.fls.male,
              input$primary.adh_romo.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_romo.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.spine.fls.male<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.hip.no.fls.male,
              input$primary.adh_alendronate.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_alendronate.hip.fls.male,
              input$primary.adh_alendronate.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.hip.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.hip.no.fls.male,
              input$primary.adh_risedronate.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_risedronate.hip.fls.male,
              input$primary.adh_risedronate.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.hip.no.fls.male,
              input$primary.adh_strontium.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_strontium.hip.fls.male,
              input$primary.adh_strontium.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.hip.no.fls.male,
              input$primary.adh_ibandronate.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_ibandronate.hip.fls.male,
              input$primary.adh_ibandronate.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_raloxifene.hip.no.fls.male,
              input$primary.adh_raloxifene.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_raloxifene.hip.fls.male,
              input$primary.adh_raloxifene.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_raloxifene.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_raloxifene.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.hip.no.fls.male,
              input$primary.adh_denosumab.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_denosumab.hip.fls.male,
              input$primary.adh_denosumab.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.hip.no.fls.male,
              input$primary.adh_zoledronate.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_zoledronate.hip.fls.male,
              input$primary.adh_zoledronate.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.hip.no.fls.male,
              input$primary.adh_teriparatide.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_teriparatide.hip.fls.male,
              input$primary.adh_teriparatide.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.hip.no.fls.male,
              input$primary.adh_abaloparatide.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_abaloparatide.hip.fls.male,
              input$primary.adh_abaloparatide.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.hip.no.fls.male,
              input$primary.adh_romo.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_romo.hip.fls.male,
              input$primary.adh_romo.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_romo.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.hip.fls.male<-unlist(sample[2])





sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.other.no.fls.male,
              input$primary.adh_alendronate.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_alendronate.other.fls.male,
              input$primary.adh_alendronate.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.other.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.other.no.fls.male,
              input$primary.adh_risedronate.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_risedronate.other.fls.male,
              input$primary.adh_risedronate.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.other.no.fls.male,
              input$primary.adh_strontium.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_strontium.other.fls.male,
              input$primary.adh_strontium.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.other.no.fls.male,
              input$primary.adh_ibandronate.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_ibandronate.other.fls.male,
              input$primary.adh_ibandronate.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.other.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_raloxifene.other.no.fls.male,
              input$primary.adh_raloxifene.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_raloxifene.other.fls.male,
              input$primary.adh_raloxifene.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_raloxifene.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_raloxifene.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.other.no.fls.male,
              input$primary.adh_denosumab.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_denosumab.other.fls.male,
              input$primary.adh_denosumab.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.other.no.fls.male,
              input$primary.adh_zoledronate.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_zoledronate.other.fls.male,
              input$primary.adh_zoledronate.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.other.no.fls.male,
              input$primary.adh_teriparatide.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_teriparatide.other.fls.male,
              input$primary.adh_teriparatide.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.other.no.fls.male,
              input$primary.adh_abaloparatide.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_abaloparatide.other.fls.male,
              input$primary.adh_abaloparatide.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.other.no.fls.male,
              input$primary.adh_romo.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_romo.other.fls.male,
              input$primary.adh_romo.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_romo.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.other.fls.male<-unlist(sample[2])










sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.spine.no.fls.female,
              input$primary.adh_alendronate.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_alendronate.spine.fls.female,
              input$primary.adh_alendronate.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.spine.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.spine.no.fls.female,
              input$primary.adh_risedronate.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_risedronate.spine.fls.female,
              input$primary.adh_risedronate.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.spine.no.fls.female,
              input$primary.adh_strontium.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_strontium.spine.fls.female,
              input$primary.adh_strontium.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.spine.no.fls.female,
              input$primary.adh_ibandronate.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_ibandronate.spine.fls.female,
              input$primary.adh_ibandronate.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_raloxifene.spine.no.fls.female,
              input$primary.adh_raloxifene.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_raloxifene.spine.fls.female,
              input$primary.adh_raloxifene.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_raloxifene.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_raloxifene.spine.fls.female<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.spine.no.fls.female,
              input$primary.adh_denosumab.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_denosumab.spine.fls.female,
              input$primary.adh_denosumab.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.spine.no.fls.female,
              input$primary.adh_zoledronate.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_zoledronate.spine.fls.female,
              input$primary.adh_zoledronate.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.spine.no.fls.female,
              input$primary.adh_teriparatide.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_teriparatide.spine.fls.female,
              input$primary.adh_teriparatide.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.spine.no.fls.female,
              input$primary.adh_abaloparatide.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_abaloparatide.spine.fls.female,
              input$primary.adh_abaloparatide.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.spine.no.fls.female,
              input$primary.adh_romo.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_romo.spine.fls.female,
              input$primary.adh_romo.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_romo.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.spine.fls.female<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.hip.no.fls.female,
              input$primary.adh_alendronate.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_alendronate.hip.fls.female,
              input$primary.adh_alendronate.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.hip.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.hip.no.fls.female,
              input$primary.adh_risedronate.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_risedronate.hip.fls.female,
              input$primary.adh_risedronate.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.hip.no.fls.female,
              input$primary.adh_strontium.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_strontium.hip.fls.female,
              input$primary.adh_strontium.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.hip.no.fls.female,
              input$primary.adh_ibandronate.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_ibandronate.hip.fls.female,
              input$primary.adh_ibandronate.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_raloxifene.hip.no.fls.female,
              input$primary.adh_raloxifene.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_raloxifene.hip.fls.female,
              input$primary.adh_raloxifene.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_raloxifene.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_raloxifene.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.hip.no.fls.female,
              input$primary.adh_denosumab.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_denosumab.hip.fls.female,
              input$primary.adh_denosumab.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.hip.no.fls.female,
              input$primary.adh_zoledronate.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_zoledronate.hip.fls.female,
              input$primary.adh_zoledronate.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.hip.no.fls.female,
              input$primary.adh_teriparatide.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_teriparatide.hip.fls.female,
              input$primary.adh_teriparatide.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.hip.no.fls.female,
              input$primary.adh_abaloparatide.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_abaloparatide.hip.fls.female,
              input$primary.adh_abaloparatide.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.hip.no.fls.female,
              input$primary.adh_romo.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_romo.hip.fls.female,
              input$primary.adh_romo.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_romo.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.hip.fls.female<-unlist(sample[2])





sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.other.no.fls.female,
              input$primary.adh_alendronate.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_alendronate.other.fls.female,
              input$primary.adh_alendronate.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.other.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.other.no.fls.female,
              input$primary.adh_risedronate.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_risedronate.other.fls.female,
              input$primary.adh_risedronate.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.other.no.fls.female,
              input$primary.adh_strontium.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_strontium.other.fls.female,
              input$primary.adh_strontium.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.other.no.fls.female,
              input$primary.adh_ibandronate.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_ibandronate.other.fls.female,
              input$primary.adh_ibandronate.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.other.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_raloxifene.other.no.fls.female,
              input$primary.adh_raloxifene.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_raloxifene.other.fls.female,
              input$primary.adh_raloxifene.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_raloxifene.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_raloxifene.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.other.no.fls.female,
              input$primary.adh_denosumab.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_denosumab.other.fls.female,
              input$primary.adh_denosumab.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.other.no.fls.female,
              input$primary.adh_zoledronate.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_zoledronate.other.fls.female,
              input$primary.adh_zoledronate.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.other.no.fls.female,
              input$primary.adh_teriparatide.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_teriparatide.other.fls.female,
              input$primary.adh_teriparatide.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.other.no.fls.female,
              input$primary.adh_abaloparatide.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_abaloparatide.other.fls.female,
              input$primary.adh_abaloparatide.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.other.no.fls.female,
              input$primary.adh_romo.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_romo.other.fls.female,
              input$primary.adh_romo.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_romo.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.other.fls.female<-unlist(sample[2])






# monitored ------
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
rmultinorm$rmultinorm.monitored.4m_adh_raloxifene.male<-
 get.sample(c(1-input$monitored.4m_adh_raloxifene.male,
              input$monitored.4m_adh_raloxifene.male))
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
rmultinorm$rmultinorm.monitored.12m_adh_raloxifene.male<-
 get.sample(c(1-input$monitored.12m_adh_raloxifene.male,
              input$monitored.12m_adh_raloxifene.male))
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
rmultinorm$rmultinorm.monitored.4m_adh_raloxifene.female<-
 get.sample(c(1-input$monitored.4m_adh_raloxifene.female,
              input$monitored.4m_adh_raloxifene.female))
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
rmultinorm$rmultinorm.monitored.12m_adh_raloxifene.female<-
 get.sample(c(1-input$monitored.12m_adh_raloxifene.female,
              input$monitored.12m_adh_raloxifene.female))
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
rmultinorm$rmultinorm.not.monitored.4m_adh_raloxifene.male<-
 get.sample(c(1-input$not.monitored.4m_adh_raloxifene.male,
              input$not.monitored.4m_adh_raloxifene.male))
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
rmultinorm$rmultinorm.not.monitored.12m_adh_raloxifene.male<-
 get.sample(c(1-input$not.monitored.12m_adh_raloxifene.male,
              input$not.monitored.12m_adh_raloxifene.male))
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
rmultinorm$rmultinorm.not.monitored.4m_adh_raloxifene.female<-
 get.sample(c(1-input$not.monitored.4m_adh_raloxifene.female,
              input$not.monitored.4m_adh_raloxifene.female))
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
rmultinorm$rmultinorm.not.monitored.12m_adh_raloxifene.female<-
 get.sample(c(1-input$not.monitored.12m_adh_raloxifene.female,
              input$not.monitored.12m_adh_raloxifene.female))
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


# adh_annual ------
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_alendronate.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_alendronate.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_alendronate.fls.male, 
              1-input$adh_annual_decline_alendronate.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_risedronate.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_risedronate.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_risedronate.fls.male, 
              1-input$adh_annual_decline_risedronate.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_strontium.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_strontium.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_strontium.fls.male, 
              1-input$adh_annual_decline_strontium.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_ibandronate.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_ibandronate.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_ibandronate.fls.male, 
              1-input$adh_annual_decline_ibandronate.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_raloxifene.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_raloxifene.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_raloxifene.fls.male, 
              1-input$adh_annual_decline_raloxifene.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_raloxifene.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_raloxifene.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_denosumab.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_denosumab.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_denosumab.fls.male, 
              1-input$adh_annual_decline_denosumab.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_zoledronate.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_zoledronate.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_zoledronate.fls.male, 
              1-input$adh_annual_decline_zoledronate.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.male<-unlist(sample[2])






sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_alendronate.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_alendronate.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_alendronate.fls.female, 
              1-input$adh_annual_decline_alendronate.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_risedronate.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_risedronate.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_risedronate.fls.female, 
              1-input$adh_annual_decline_risedronate.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_strontium.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_strontium.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_strontium.fls.female, 
              1-input$adh_annual_decline_strontium.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_ibandronate.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_ibandronate.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_ibandronate.fls.female, 
              1-input$adh_annual_decline_ibandronate.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_raloxifene.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_raloxifene.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_raloxifene.fls.female, 
              1-input$adh_annual_decline_raloxifene.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_raloxifene.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_raloxifene.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_denosumab.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_denosumab.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_denosumab.fls.female, 
              1-input$adh_annual_decline_denosumab.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_zoledronate.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_zoledronate.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_zoledronate.fls.female, 
              1-input$adh_annual_decline_zoledronate.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.female<-unlist(sample[2])


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

# temp rehab

# any possible
# 1 temp rehap
# 2 no temp rehab

rmultinorm$rmultinorm.discharged.hip.all<-
 get.sample(c(input$prop.discharged.temp.rehab.hip,
              1-input$prop.discharged.temp.rehab.hip))

rmultinorm$rmultinorm.discharged.spine.all<-
 get.sample(c(input$prop.discharged.temp.rehab.spine,
              1-input$prop.discharged.temp.rehab.spine))

rmultinorm$rmultinorm.discharged.other.all<-
 get.sample(c(input$prop.discharged.temp.rehab.other,
              1-input$prop.discharged.temp.rehab.other))





# for location
# it will depend where they are, can't revert to

# 1, "home, no support",
# 2, "family home",
# 3, "home, support",
# 4, "long term care", 

# from home no support
#scale remaining
sum<-sum(input$prop.discharged.home.no.support.hip,
         input$prop.discharged.family.home.hip,
         input$prop.discharged.home.support.hip,
         input$prop.discharged.long.term.care.hip)

rmultinorm$rmultinorm.discharged.hip.from.home.no.support<-
get.sample(c(input$prop.discharged.home.no.support.hip/sum,
input$prop.discharged.family.home.hip/sum,
input$prop.discharged.home.support.hip/sum,
input$prop.discharged.long.term.care.hip/sum))

# from family home
sum<-sum(input$prop.discharged.family.home.hip,
         input$prop.discharged.home.support.hip,
         input$prop.discharged.long.term.care.hip)
rmultinorm$rmultinorm.discharged.hip.from.family.home<-
get.sample(c(0,
input$prop.discharged.family.home.hip/sum,
input$prop.discharged.home.support.hip/sum,
input$prop.discharged.long.term.care.hip/sum))

#from home support
sum<-sum(input$prop.discharged.home.support.hip,
         input$prop.discharged.long.term.care.hip)
rmultinorm$rmultinorm.discharged.hip.from.home.support<-
get.sample(c(0,0,
input$prop.discharged.home.support.hip/sum,
input$prop.discharged.long.term.care.hip/sum))





# 1, "home, no support",
# 2, "family home",
# 3, "home, support",
# 4, "long term care", 

# from home no support
#scale remaining
sum<-sum(input$prop.discharged.home.no.support.spine,
              input$prop.discharged.family.home.spine,
              input$prop.discharged.home.support.spine,
              input$prop.discharged.long.term.care.spine)
rmultinorm$rmultinorm.discharged.spine.from.home.no.support<-
get.sample(c(input$prop.discharged.home.no.support.spine/sum,
input$prop.discharged.family.home.spine/sum,
input$prop.discharged.home.support.spine/sum,
input$prop.discharged.long.term.care.spine/sum))

#from family home
sum<-sum(input$prop.discharged.family.home.spine,
         input$prop.discharged.home.support.spine,
         input$prop.discharged.long.term.care.spine)
if(sum>0){
rmultinorm$rmultinorm.discharged.spine.from.family.home<-
get.sample(c(0,
input$prop.discharged.family.home.spine/sum,
         input$prop.discharged.home.support.spine,
         input$prop.discharged.long.term.care.spine/sum))} else {
 rmultinorm$rmultinorm.discharged.spine.from.family.home<-rep(2, n_microsimulation*6)
}

#from home support
sum<-sum(input$prop.discharged.home.support.spine,
              input$prop.discharged.long.term.care.spine)
# home no support None
if(input$prop.discharged.family.home.spine>0){
rmultinorm$rmultinorm.discharged.spine.from.home.support<-
get.sample(c(0,0,
             input$prop.discharged.home.support.spine/sum,
input$prop.discharged.long.term.care.spine/sum))} else {
  rmultinorm$rmultinorm.discharged.spine.from.home.support<-rep(3, n_microsimulation*6)
}


# from temp rehab
# 1, "home, no support",
# 2, "family home",
# 3, "home, support",
# 4, "long term care", 

# from home no support
#scale remaining
sum<-sum(input$prop.discharged.home.no.support.other,
              input$prop.discharged.family.home.other,
              input$prop.discharged.home.support.other,
              input$prop.discharged.long.term.care.other)
rmultinorm$rmultinorm.discharged.other.from.home.no.support<-
get.sample(c(input$prop.discharged.home.no.support.other/sum,
input$prop.discharged.family.home.other/sum,
input$prop.discharged.home.support.other/sum,
input$prop.discharged.long.term.care.other/sum))

#from family home
sum<-sum(input$prop.discharged.family.home.other,
         input$prop.discharged.home.support.other,
              input$prop.discharged.long.term.care.other)
if(sum>0){
rmultinorm$rmultinorm.discharged.other.from.family.home<-
get.sample(c(0,
input$prop.discharged.family.home.other/sum,
input$prop.discharged.home.support.other/sum,
input$prop.discharged.long.term.care.other/sum))} else{
  rmultinorm$rmultinorm.discharged.other.from.family.home<-rep(2, n_microsimulation*6)
}

#from home support
sum<-sum(input$prop.discharged.home.support.other,
              input$prop.discharged.long.term.care.other)
if(sum>0){
rmultinorm$rmultinorm.discharged.other.from.home.support<-
get.sample(c(0,0,
             input$prop.discharged.home.support.other/sum,
input$prop.discharged.long.term.care.other/sum))} else{
  rmultinorm$rmultinorm.discharged.other.from.home.support<-rep(3, n_microsimulation*6)
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




# trans ----
rmultinorm$rmultinorm.trans<-get.trans()
   
   


rmultinorm
}




## get relative risks for meds -----
get.rr<-function(working.country_data){

  rr<-list()
  rr["rr_alendronate_spine"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_alendronate_spine") %>%
                                           select(Value))
  rr["rr_risedronate_spine"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_risedronate_spine") %>%
                                           select(Value))
  rr["rr_strontium_spine"]<-as.numeric(working.country_data %>%
                                         filter(name=="rr_strontium_spine") %>%
                                         select(Value))
  rr["rr_ibandronate_spine"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_ibandronate_spine") %>%
                                           select(Value))
    rr["rr_raloxifene_spine"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_raloxifene_spine") %>%
                                           select(Value))
  rr["rr_denosumab_spine"]<-as.numeric(working.country_data %>%
                                         filter(name=="rr_denosumab_spine") %>%
                                         select(Value))
  rr["rr_zoledronate_spine"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_zoledronate_spine") %>%
                                           select(Value))
  rr["rr_teriparatide_spine"]<-as.numeric(working.country_data %>%
                                            filter(name=="rr_teriparatide_spine") %>%
                                            select(Value))
  rr["rr_abaloparatide_spine"]<-as.numeric(working.country_data %>%
                                             filter(name=="rr_abaloparatide_spine") %>%
                                             select(Value))
  rr["rr_romo_spine"]<-as.numeric(working.country_data %>%
                                    filter(name=="rr_romo_spine") %>%
                                    select(Value))



  rr["rr_alendronate_hip"]<-as.numeric(working.country_data %>%
                                         filter(name=="rr_alendronate_hip") %>%
                                         select(Value))
  rr["rr_risedronate_hip"]<-as.numeric(working.country_data %>%
                                         filter(name=="rr_risedronate_hip") %>%
                                         select(Value))
  rr["rr_strontium_hip"]<-as.numeric(working.country_data %>%
                                       filter(name=="rr_strontium_hip") %>%
                                       select(Value))
  rr["rr_ibandronate_hip"]<-as.numeric(working.country_data %>%
                                         filter(name=="rr_ibandronate_hip") %>%
                                         select(Value))
    rr["rr_raloxifene_hip"]<-as.numeric(working.country_data %>%
                                         filter(name=="rr_raloxifene_hip") %>%
                                         select(Value))
  rr["rr_denosumab_hip"]<-as.numeric(working.country_data %>%
                                       filter(name=="rr_denosumab_hip") %>%
                                       select(Value))
  rr["rr_zoledronate_hip"]<-as.numeric(working.country_data %>%
                                         filter(name=="rr_zoledronate_hip") %>%
                                         select(Value))
  rr["rr_teriparatide_hip"]<-as.numeric(working.country_data %>%
                                          filter(name=="rr_teriparatide_hip") %>%
                                          select(Value))
  rr["rr_abaloparatide_hip"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_abaloparatide_hip") %>%
                                           select(Value))
  rr["rr_romo_hip"]<-as.numeric(working.country_data %>%
                                  filter(name=="rr_romo_hip") %>%
                                  select(Value))


  rr["rr_alendronate_other"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_alendronate_other") %>%
                                           select(Value))
  rr["rr_risedronate_other"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_risedronate_other") %>%
                                           select(Value))
  rr["rr_strontium_other"]<-as.numeric(working.country_data %>%
                                         filter(name=="rr_strontium_other") %>%
                                         select(Value))
  rr["rr_ibandronate_other"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_ibandronate_other") %>%
                                           select(Value))
  rr["rr_raloxifene_other"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_raloxifene_other") %>%
                                           select(Value))
  rr["rr_denosumab_other"]<-as.numeric(working.country_data %>%
                                         filter(name=="rr_denosumab_other") %>%
                                         select(Value))
  rr["rr_zoledronate_other"]<-as.numeric(working.country_data %>%
                                           filter(name=="rr_zoledronate_other") %>%
                                           select(Value))
  rr["rr_teriparatide_other"]<-as.numeric(working.country_data %>%
                                            filter(name=="rr_teriparatide_other") %>%
                                            select(Value))
  rr["rr_abaloparatide_other"]<-as.numeric(working.country_data %>%
                                             filter(name=="rr_abaloparatide_other") %>%
                                             select(Value))
  rr["rr_romo_other"]<-as.numeric(working.country_data %>%
                                    filter(name=="rr_romo_other") %>%
                                    select(Value))


  rr
}

## get lags -----
get.lags<-function(general.inputs){

  lag<-list()
  lag["lag_alendronate_spine"]<-as.numeric(general.inputs %>% filter(name=="lag_alendronate_spine") %>%
                                             select(Value))
  lag["lag_risedronate_spine"]<-as.numeric(general.inputs %>% filter(name=="lag_risedronate_spine") %>%
                                             select(Value))
  lag["lag_strontium_spine"]<-as.numeric(general.inputs %>% filter(name=="lag_strontium_spine") %>%
                                           select(Value))
  lag["lag_ibandronate_spine"]<-as.numeric(general.inputs %>% filter(name=="lag_ibandronate_spine") %>%
                                             select(Value))
  lag["lag_raloxifene_spine"]<-as.numeric(general.inputs %>% filter(name=="lag_raloxifene_spine") %>%
                                             select(Value))
  lag["lag_denosumab_spine"]<-as.numeric(general.inputs %>% filter(name=="lag_denosumab_spine") %>%
                                           select(Value))
  lag["lag_zoledronate_spine"]<-as.numeric(general.inputs %>% filter(name=="lag_zoledronate_spine") %>%
                                             select(Value))
  lag["lag_teriparatide_spine"]<-as.numeric(general.inputs %>% filter(name=="lag_teriparatide_spine") %>%
                                              select(Value))
  lag["lag_abaloparatide_spine"]<-as.numeric(general.inputs %>% filter(name=="lag_abaloparatide_spine") %>%
                                               select(Value))
  lag["lag_romo_spine"]<-as.numeric(general.inputs %>% filter(name=="lag_romo_spine") %>%
                                               select(Value))

  lag["lag_alendronate_hip"]<-as.numeric(general.inputs %>% filter(name=="lag_alendronate_hip") %>%
                                           select(Value))
  lag["lag_risedronate_hip"]<-as.numeric(general.inputs %>% filter(name=="lag_risedronate_hip") %>%
                                           select(Value))
  lag["lag_strontium_hip"]<-as.numeric(general.inputs %>% filter(name=="lag_strontium_hip") %>%
                                         select(Value))
  lag["lag_ibandronate_hip"]<-as.numeric(general.inputs %>% filter(name=="lag_ibandronate_hip") %>%
                                           select(Value))
  lag["lag_raloxifene_hip"]<-as.numeric(general.inputs %>% filter(name=="lag_raloxifene_hip") %>%
                                           select(Value))
  lag["lag_denosumab_hip"]<-as.numeric(general.inputs %>% filter(name=="lag_denosumab_hip") %>%
                                         select(Value))
  lag["lag_zoledronate_hip"]<-as.numeric(general.inputs %>% filter(name=="lag_zoledronate_hip") %>%
                                           select(Value))
  lag["lag_teriparatide_hip"]<-as.numeric(general.inputs %>% filter(name=="lag_teriparatide_hip") %>%
                                            select(Value))
  lag["lag_abaloparatide_hip"]<-as.numeric(general.inputs %>% filter(name=="lag_abaloparatide_hip") %>%
                                             select(Value))
  lag["lag_romo_hip"]<-as.numeric(general.inputs %>% filter(name=="lag_romo_hip") %>%
                                      select(Value))

  lag["lag_alendronate_other"]<-as.numeric(general.inputs %>% filter(name=="lag_alendronate_other") %>%
                                             select(Value))
  lag["lag_risedronate_other"]<-as.numeric(general.inputs %>% filter(name=="lag_risedronate_other") %>%
                                             select(Value))
  lag["lag_strontium_other"]<-as.numeric(general.inputs %>% filter(name=="lag_strontium_other") %>%
                                           select(Value))
  lag["lag_ibandronate_other"]<-as.numeric(general.inputs %>% filter(name=="lag_ibandronate_other") %>%
                                             select(Value))
    lag["lag_raloxifene_other"]<-as.numeric(general.inputs %>% filter(name=="lag_raloxifene_other") %>%
                                             select(Value))
  lag["lag_denosumab_other"]<-as.numeric(general.inputs %>% filter(name=="lag_denosumab_other") %>%
                                           select(Value))
  lag["lag_zoledronate_other"]<-as.numeric(general.inputs %>% filter(name=="lag_zoledronate_other") %>%
                                             select(Value))
  lag["lag_teriparatide_other"]<-as.numeric(general.inputs %>% filter(name=="lag_teriparatide_other") %>%
                                              select(Value))
  lag["lag_abaloparatide_other"]<-as.numeric(general.inputs %>% filter(name=="lag_abaloparatide_other") %>%
                                               select(Value))
  lag["lag_romo_other"]<-as.numeric(general.inputs %>% filter(name=="lag_romo_other") %>%
                                               select(Value))

  lag }

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
cmp_m.fun.time.since.fx = compiler::cmpfun(m.fun.time.since.fx)


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
cmp_m.fun.tps = compiler::cmpfun(m.fun.tps)

## limit to max two hip fx-----
# if they've had two hip fx, set tp to zero
m.fun.two.fx<-function(df=df, t=t){
if(df[t-1,"h_hf"]==2){
  df[t,"hf.tp"]<-0
}
if(df[t-1,"h_hf"]==1 &
   df[t-1,"s_hf"]==1){
  df[t,"hf.tp"]<-0
}
   
df
}
cmp_m.fun.two.fx = compiler::cmpfun(m.fun.two.fx)

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
cmp_m.fun.identified = compiler::cmpfun(m.fun.identified)



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
cmp_m.fun.tr.onset = compiler::cmpfun(m.fun.tr.onset)

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
cmp_m.fun.treat = compiler::cmpfun(m.fun.treat)
## treat - risk profiles -----

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


# depends on risk profile



# for male/female and no fls/ fls
m.fun.treat.rp<-function(df=df, t=t, rmultinorm=rmultinorm){
   
if(df[t,"intervention"]==1 &
   df[t,"sex"]==1) {   #  no.fls and male

if(df[t,"tr.onset"]==1){ # 1)
df[t,"treat"]<-1
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"c_af"]==1){ # 2)

   
   if(df[t,"risk.type"]==1){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.male.low.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.male.low.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.male.low.risk[df[t,"id"]],
                               NA)))
   } else if (df[t,"risk.type"]==2){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.male.intermediate.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.male.intermediate.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.male.intermediate.risk[df[t,"id"]],
                               NA)))
   } else { 
      df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.male.high.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.male.high.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.male.high.risk[df[t,"id"]],
                               NA)))
   }  
   
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==1){ # 3)

    if(df[t,"risk.type"]==1){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.male.low.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.male.low.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.male.low.risk[df[t,"id"]],
                               NA)))
   } else if (df[t,"risk.type"]==2){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.male.intermediate.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.male.intermediate.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.male.intermediate.risk[df[t,"id"]],
                               NA)))
   } else { 
      df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.male.high.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.male.high.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.male.high.risk[df[t,"id"]],
                               NA)))
   }
   
   
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

     if(df[t,"risk.type"]==1){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.female.low.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.female.low.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.female.low.risk[df[t,"id"]],
                               NA)))
   } else if (df[t,"risk.type"]==2){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.female.intermediate.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.female.intermediate.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.female.intermediate.risk[df[t,"id"]],
                               NA)))
   } else { 
      df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.female.high.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.female.high.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.female.high.risk[df[t,"id"]],
                               NA)))
   }
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==1){ # 3)

     if(df[t,"risk.type"]==1){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.female.low.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.female.low.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.female.low.risk[df[t,"id"]],
                               NA)))
   } else if (df[t,"risk.type"]==2){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.female.intermediate.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.female.intermediate.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.female.intermediate.risk[df[t,"id"]],
                               NA)))
   } else { 
      df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.no.fls.trt.spine.female.high.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.no.fls.trt.hip.female.high.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.no.fls.trt.other.female.high.risk[df[t,"id"]],
                               NA)))
   }
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

      if(df[t,"risk.type"]==1){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.male.low.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.male.low.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.male.low.risk[df[t,"id"]],
                               NA)))
   } else if (df[t,"risk.type"]==2){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.male.intermediate.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.male.intermediate.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.male.intermediate.risk[df[t,"id"]],
                               NA)))
   } else { 
      df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.male.high.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.male.high.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.male.high.risk[df[t,"id"]],
                               NA)))
   }
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==1){ # 3)

      if(df[t,"risk.type"]==1){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.male.low.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.male.low.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.male.low.risk[df[t,"id"]],
                               NA)))
   } else if (df[t,"risk.type"]==2){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.male.intermediate.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.male.intermediate.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.male.intermediate.risk[df[t,"id"]],
                               NA)))
   } else { 
      df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.male.high.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.male.high.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.male.high.risk[df[t,"id"]],
                               NA)))
   }
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

         if(df[t,"risk.type"]==1){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.female.low.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.female.low.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.female.low.risk[df[t,"id"]],
                               NA)))
   } else if (df[t,"risk.type"]==2){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.female.intermediate.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.female.intermediate.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.female.intermediate.risk[df[t,"id"]],
                               NA)))
   } else { 
      df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.female.high.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.female.high.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.female.high.risk[df[t,"id"]],
                               NA)))
   }
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==1){ # 3)

    
       if(df[t,"risk.type"]==1){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.female.low.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.female.low.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.female.low.risk[df[t,"id"]],
                               NA)))
   } else if (df[t,"risk.type"]==2){ 
   df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.female.intermediate.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.female.intermediate.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.female.intermediate.risk[df[t,"id"]],
                               NA)))
   } else { 
      df[t,"treat"]<-ifelse(df[t,"recent.fx"]==1,
                              rmultinorm$rmultinorm.fls.trt.spine.female.high.risk[df[t,"id"]],
                       ifelse(df[t,"recent.fx"]==2,
                               rmultinorm$rmultinorm.fls.trt.hip.female.high.risk[df[t,"id"]],
                        ifelse(df[t,"recent.fx"]==3,
                               rmultinorm$rmultinorm.fls.trt.other.female.high.risk[df[t,"id"]],
                               NA)))
   }
}

if(df[t,"tr.onset"]==2 &
   df[t-1,"tr.onset"]==2 &
   df[t-1,"c_af"]==0){ # 4)

   df[t,"treat"]<-df[t-1,"treat"]
}

}


df
}
cmp_m.fun.treat.rp = compiler::cmpfun(m.fun.treat.rp)


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
cmp_m.fun.assign.treatment = compiler::cmpfun(m.fun.assign.treatment)


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
cmp_m.fun.switch.treatment = compiler::cmpfun(m.fun.switch.treatment)

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

if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.spine.no.fls.male[df[t,"id"]]
}   

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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
   
   if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.hip.no.fls.male[df[t,"id"]]
}


if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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
   
   if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.other.no.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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
   
if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.spine.no.fls.female[df[t,"id"]]
}
   
if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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
   
   if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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

if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.other.no.fls.female[df[t,"id"]]
}   
   
if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.other.no.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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
   
if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.spine.fls.male[df[t,"id"]]
}   

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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

if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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

if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.other.fls.male[df[t,"id"]]
}
   
if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.other.fls.male[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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

if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.spine.fls.female[df[t,"id"]]
}
   
if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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

if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.hip.fls.female[df[t,"id"]]
}
   
if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
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

if(df[t,"time_med"]==1 & # stating raloxifene
df[t,"medication"]==5){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_raloxifene.other.fls.female[df[t,"id"]]
}
   
if(df[t,"time_med"]==1 & # stating denosumab
df[t,"medication"]==6){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_denosumab.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating zoledronate
df[t,"medication"]==7){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_zoledronate.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating teriparatide
df[t,"medication"]==8){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_teriparatide.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating abaloparatide
df[t,"medication"]==9){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_abaloparatide.other.fls.female[df[t,"id"]]
}

if(df[t,"time_med"]==1 & # stating romo
df[t,"medication"]==10){
df[t,"adhering"]<-rmultinorm$rmultinorm.primary.adh_romo.other.fls.female[df[t,"id"]]
}


}

  df

}
cmp_m.fun.adherrence.t1 = compiler::cmpfun(m.fun.adherrence.t1)

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
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_raloxifene.male[df[t,"id"]]
}
  if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.male[df[t,"id"]]
  }
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.male[df[t,"id"]]
}
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.male[df[t,"id"]]
  }
if(df[t,"medication"] ==9) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.male[df[t,"id"]]
}
  if(df[t,"medication"] ==10) {#
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
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_raloxifene.female[df[t,"id"]]
}
  if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.female[df[t,"id"]]
  }
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.female[df[t,"id"]]
}
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.female[df[t,"id"]]
  }
if(df[t,"medication"] ==9) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.female[df[t,"id"]]
}
  if(df[t,"medication"] ==10) {#
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
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_raloxifene.male[df[t,"id"]]
}
  if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_denosumab.male[df[t,"id"]]
  }
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.male[df[t,"id"]]
}
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.male[df[t,"id"]]
  }
if(df[t,"medication"] ==9) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.male[df[t,"id"]]
}
  if(df[t,"medication"] ==10) {#
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
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_raloxifene.female[df[t,"id"]]
}
  if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_denosumab.female[df[t,"id"]]
  }
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.female[df[t,"id"]]
}
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.female[df[t,"id"]]
  }
if(df[t,"medication"] ==9) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.female[df[t,"id"]]
}
  if(df[t,"medication"] ==10) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.4m_adh_romo.female[df[t,"id"]]
  }

}
}

}

}




df
}
cmp_m.fun.adherrence.t4 = compiler::cmpfun(m.fun.adherrence.t4)

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
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_raloxifene.male[df[t,"id"]]
}
  if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.male[df[t,"id"]]
  }
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.male[df[t,"id"]]
}
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.male[df[t,"id"]]
  }
if(df[t,"medication"] ==9) {#
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
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_raloxifene.female[df[t,"id"]]
}
  if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.female[df[t,"id"]]
  }
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.female[df[t,"id"]]
}
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.female[df[t,"id"]]
  }
if(df[t,"medication"] ==9) {#
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
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_raloxifene.male[df[t,"id"]]
}
  if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_denosumab.male[df[t,"id"]]
  }
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.male[df[t,"id"]]
}
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.male[df[t,"id"]]
  }
if(df[t,"medication"] ==9) {#
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
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_raloxifene.female[df[t,"id"]]
}
  if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_denosumab.female[df[t,"id"]]
  }
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.female[df[t,"id"]]
}
if(df[t,"medication"] ==8) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.female[df[t,"id"]]
  }
if(df[t,"medication"] ==9) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.female[df[t,"id"]]
}

}
}

}

}




df
}
cmp_m.fun.adherrence.t12 = compiler::cmpfun(m.fun.adherrence.t12)

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
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_raloxifene.no.fls.male[df[t,"id"]]
}
  if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.male[df[t,"id"]]
  }
if(df[t,"medication"] ==7) {#
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
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_raloxifene.no.fls.female[df[t,"id"]]
}
  if(df[t,"medication"] ==6) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.female[df[t,"id"]]
  }
if(df[t,"medication"] ==7) {#
  df[t,"adhering"]<-rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.female[df[t,"id"]]
}


}



}

}

df
}
cmp_m.fun.adherrence.t24_36_48 = compiler::cmpfun(m.fun.adherrence.t24_36_48)

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
cmp_m.fun.hospitalised = compiler::cmpfun(m.fun.hospitalised)

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
cmp_m.fun.procedure = compiler::cmpfun(m.fun.procedure)


# temp rehab -----
m.fun.temp.rehab<-function(df=df, t=t, rmultinorm=rmultinorm, input=input){
 # browser()

  #1 yes, visits temp rehab
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
cmp_m.fun.temp.rehab = compiler::cmpfun(m.fun.temp.rehab)




# location -----

m.fun.location<-function(df=df, t=t, rmultinorm=rmultinorm, input=input){

# assign location at time of a fracture

# 1, "home, no support",
# 2, "home, support",
# 3, "family home",
# 4, "long term care", 
   
# 5 died   


  #browser()
df[t,"location"]<-1

if(t==1){
   if(df[t,"s_hf"]==1){
    df[t,"location"]<-  rmultinorm$rmultinorm.discharged.hip.from.home.no.support[df[t,"id"]]
   }
    if(df[t,"s_sf"]==1){
         df[t,"location"]<-  rmultinorm$rmultinorm.discharged.spine.from.home.no.support[df[t,"id"]]
    }
    if(df[t,"s_of"]==1){
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
    df[t,"location"]<-  rmultinorm$rmultinorm.discharged.hip.from.home.no.support[df[t,"id"]]
   }
    if(df[t,"s_sf"]==1){
         df[t,"location"]<-  rmultinorm$rmultinorm.discharged.spine.from.home.no.support[df[t,"id"]]
    }
    if(df[t,"s_of"]==1){
        df[t,"location"]<-  rmultinorm$rmultinorm.discharged.other.from.home.no.support[df[t,"id"]]
    }}


if(df[t-1,"location"] == 2){
     if(df[t,"s_hf"]==1){
    df[t,"location"]<-  rmultinorm$rmultinorm.discharged.hip.from.home.no.support[df[t,"id"]]
   }
    if(df[t,"s_sf"]==1){
         df[t,"location"]<-  rmultinorm$rmultinorm.discharged.spine.from.home.no.support[df[t,"id"]]
    }
    if(df[t,"s_of"]==1){
        df[t,"location"]<-  rmultinorm$rmultinorm.discharged.other.from.home.no.support[df[t,"id"]]
    }}

if(df[t-1,"location"] == 3){
     if(df[t,"s_hf"]==1){
    df[t,"location"]<-  rmultinorm$rmultinorm.discharged.hip.from.family.home[df[t,"id"]]
   }
    if(df[t,"s_sf"]==1){
         df[t,"location"]<-  rmultinorm$rmultinorm.discharged.spine.from.family.home[df[t,"id"]]
    }
    if(df[t,"s_sf"]==1){
        df[t,"location"]<-  rmultinorm$rmultinorm.discharged.other.from.family.home[df[t,"id"]]
    }}

if(df[t-1,"location"] == 4){ # stay in long-term care
df[t,"location"]<- 4
    }
}
}

if(df[t,"s_d"]==1){
    df[t,"location"]<-5   
   }

df
}
cmp_m.fun.location = compiler::cmpfun(m.fun.location)






# lab.test -----
m.fun.lab.test<-function(df=df, t=t, rmultinorm, input=input){
  #1 no
  #2 yes

  df[t,"lab.test"]<-1 # will replace below if yes

  if(df[t,"s_hf"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==1 ){
     df[t,"lab.test"]<-rmultinorm$rmultinorm.lab.test.no.fls.hip[df[t,"id"]]
  }
   if(df[t,"s_sf"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==1 ){
     df[t,"lab.test"]<- rmultinorm$rmultinorm.lab.test.no.fls.spine[df[t,"id"]]
   }

   if(df[t,"s_of"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==1 ){
     df[t,"lab.test"]<- rmultinorm$rmultinorm.lab.test.no.fls.other[df[t,"id"]]
   }

    if(df[t,"s_hf"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==2 ){
     df[t,"lab.test"]<-rmultinorm$rmultinorm.lab.test.fls.hip[df[t,"id"]]
  }
   if(df[t,"s_sf"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==2 ){
     df[t,"lab.test"]<- rmultinorm$rmultinorm.lab.test.fls.spine[df[t,"id"]]
   }

   if(df[t,"s_of"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==2 ){
     df[t,"lab.test"]<- rmultinorm$rmultinorm.lab.test.fls.other[df[t,"id"]]
  }


  df
}
# cmp_m.fun.lab.test = compiler::cmpfun(m.fun.lab.test)

# dxa ----
# m.fun.dxa<-function(df=df, t=t, rmultinorm, input=input){
#    #1 no
#   #2 yes
# 
#   df[t,"dxa"]<-1 # will replace below if yes
# 
#   if(df[t,"s_hf"]==1 &
#      df[t,"identified"]==1 &
#      df[t,"intervention"]==1 ){
#      df[t,"dxa"]<-rmultinorm$rmultinorm.dxa.no.fls.hip[df[t,"id"]]
#   }
#    if(df[t,"s_sf"]==1 &
#      df[t,"identified"]==1 &
#      df[t,"intervention"]==1 ){
#      df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.no.fls.spine[df[t,"id"]]
#    }
# 
#    if(df[t,"s_of"]==1 &
#      df[t,"identified"]==1 &
#      df[t,"intervention"]==1 ){
#      df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.no.fls.other[df[t,"id"]]
#    }
# 
#     if(df[t,"s_hf"]==1 &
#      df[t,"identified"]==1 &
#      df[t,"intervention"]==2 ){
#      df[t,"dxa"]<-rmultinorm$rmultinorm.dxa.fls.hip[df[t,"id"]]
#   }
#    if(df[t,"s_sf"]==1 &
#      df[t,"identified"]==1 &
#      df[t,"intervention"]==2 ){
#      df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.fls.spine[df[t,"id"]]
#    }
# 
#    if(df[t,"s_of"]==1 &
#      df[t,"identified"]==1 &
#      df[t,"intervention"]==2 ){
#      df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.fls.other[df[t,"id"]]
#   }
# 
# 
#   df
# }
m.fun.dxa<-function(df=df, t=t, rmultinorm, input=input){
  #1 no
  #2 yes

  df[t,"dxa"]<-1 # will replace below if yes

  if(df[t,"s_hf"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==1 ){
     df[t,"dxa"]<-rmultinorm$rmultinorm.dxa.no.fls.hip[df[t,"id"]]
  }
   if(df[t,"s_sf"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==1 ){
     df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.no.fls.spine[df[t,"id"]]
   }

   if(df[t,"s_of"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==1 ){
     df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.no.fls.other[df[t,"id"]]
   }

    if(df[t,"s_hf"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==2 ){
     df[t,"dxa"]<-rmultinorm$rmultinorm.dxa.fls.hip[df[t,"id"]]
  }
   if(df[t,"s_sf"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==2 ){
     df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.fls.spine[df[t,"id"]]
   }

   if(df[t,"s_of"]==1 &
     df[t,"identified"]==2 &
     df[t,"intervention"]==2 ){
     df[t,"dxa"]<- rmultinorm$rmultinorm.dxa.fls.other[df[t,"id"]]
  }


  df
}

# cmp_m.fun.dxa = compiler::cmpfun(m.fun.dxa)

####################



#fun for running model -----
m.TR.go<-function(df, t, rmultinorm, input=user_inputs,
                  use.risk.profiles){
  df<-as.matrix(df)

  if(t>1){
    # cost only for t==1
    
                      # time since fx
                      df<-cmp_m.fun.time.since.fx(df=df, t=t)

                      # most recent fx
                      df[t,"recent.fx"] <-which.min(c(df[t,"t_since_sf"],
                                                      df[t,"t_since_hf"],
                                                      df[t,"t_since_of"]))

                      # transition probabilities
                      df<-cmp_m.fun.tps(df=df,t=t, background.tps=background.tps)
                      
                      
                      if(use.risk.profiles==TRUE){
                         # apply re-fracture risk multiplier by risk tier
                      
                      if(df[t,"risk.type"]==1){
                      df[t,"sf.tp"]*input$fx.multiplier.low.risk
                      df[t,"hf.tp"]*input$fx.multiplier.low.risk
                      df[t,"of.tp"]*input$fx.multiplier.low.risk
                      } else if (df[t,"risk.type"]==2){
                      df[t,"sf.tp"]*input$fx.multiplier.intermediate.risk
                      df[t,"hf.tp"]*input$fx.multiplier.intermediate.risk
                      df[t,"of.tp"]*input$fx.multiplier.intermediate.risk
                      } else {
                      df[t,"sf.tp"]*input$fx.multiplier.high.risk
                      df[t,"hf.tp"]*input$fx.multiplier.high.risk
                      df[t,"of.tp"]*input$fx.multiplier.high.risk
                      } 
                      }
                      
                      # max two hip fx
                      df<-cmp_m.fun.two.fx(df=df, t=t)

                      # identified
                      df<-cmp_m.fun.identified(df=df, t=t, rmultinorm=rmultinorm)

                      # tr onset
                      df<-cmp_m.fun.tr.onset(df=df, t=t, input=user_inputs)

                      # treat
                      if(use.risk.profiles==TRUE){
                      df<-cmp_m.fun.treat.rp(df=df, t=t, rmultinorm=rmultinorm)
                      } else {
                        df<-cmp_m.fun.treat(df=df, t=t, rmultinorm=rmultinorm) 
                      }
                      
                      
                      #browser()
                      # medication
                      df<-cmp_m.fun.assign.treatment(df=df, t=t, rmultinorm=rmultinorm)

                      # switching medication
                      df<-cmp_m.fun.switch.treatment(df=df, t=t, rmultinorm=rmultinorm)

                      # off medication if died
                      if(df[t-1,"s_d"]==1){
                         df[t,"medication"]<-0   
                         }

                      # time med is same value
                      df[t,"time_med"]<-tail(rle(df[(1:t),"medication"])$lengths,1)



                      # get whether passed lag -----

                      # for lag

                      # check if time on med (time_med) is over lag for that medication
                      # lag is dependent on site so check if lag is over min for any of the fracture types
                      # i.e if spine and hip, if either lag is sufficient then start applying relative risk

                      df[t,"lag.passed"]<-0
                      # replace with 1 if passed

                      if(df[t,"medication"] ==1) {#alendronate

                        if(!is.na(df[t,"t_since_sf"])) {
                          if(df[t,"time_med"]>(lag$lag_alendronate_spine)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_hf"])) {
                          if(df[t,"time_med"]>(lag$lag_alendronate_hip)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_of"])) {
                          if(df[t,"time_med"]>(lag$lag_alendronate_other)){
                            df[t,"lag.passed"]<-1
                          }}

                      }

                      if(df[t,"medication"] ==2) {#risedronate

                        if(!is.na(df[t,"t_since_sf"])) {
                          if(df[t,"time_med"]>(lag$lag_risedronate_spine)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_hf"])) {
                          if(df[t,"time_med"]>(lag$lag_risedronate_hip)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_of"])) {
                          if(df[t,"time_med"]>(lag$lag_risedronate_other)){
                            df[t,"lag.passed"]<-1
                          }}

                      }

                      if(df[t,"medication"] ==3) {#strontium

                        if(!is.na(df[t,"t_since_sf"])) {
                          if(df[t,"time_med"]>(lag$lag_strontium_spine)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_hf"])) {
                          if(df[t,"time_med"]>(lag$lag_strontium_hip)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_of"])) {
                          if(df[t,"time_med"]>(lag$lag_strontium_other)){
                            df[t,"lag.passed"]<-1
                          }}

                      }

                      if(df[t,"medication"] ==4) {#ibandronate

                        if(!is.na(df[t,"t_since_sf"])) {
                          if(df[t,"time_med"]>(lag$lag_ibandronate_spine)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_hf"])) {
                          if(df[t,"time_med"]>(lag$lag_ibandronate_hip)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_of"])) {
                          if(df[t,"time_med"]>(lag$lag_ibandronate_other)){
                            df[t,"lag.passed"]<-1
                          }}

                      }
                      
                    if(df[t,"medication"] ==5) {#raloxifene

                        if(!is.na(df[t,"t_since_sf"])) {
                          if(df[t,"time_med"]>(lag$lag_raloxifene_spine)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_hf"])) {
                          if(df[t,"time_med"]>(lag$lag_raloxifene_hip)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_of"])) {
                          if(df[t,"time_med"]>(lag$lag_raloxifene_other)){
                            df[t,"lag.passed"]<-1
                          }}

                      }

                      if(df[t,"medication"] ==6) {#denosumab

                        if(!is.na(df[t,"t_since_sf"])) {
                          if(df[t,"time_med"]>(lag$lag_denosumab_spine)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_hf"])) {
                          if(df[t,"time_med"]>(lag$lag_denosumab_hip)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_of"])) {
                          if(df[t,"time_med"]>(lag$lag_denosumab_other)){
                            df[t,"lag.passed"]<-1
                          }}

                      }

                      if(df[t,"medication"] ==7) {#zoledronate

                        if(!is.na(df[t,"t_since_sf"])) {
                          if(df[t,"time_med"]>(lag$lag_zoledronate_spine)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_hf"])) {
                          if(df[t,"time_med"]>(lag$lag_zoledronate_hip)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_of"])) {
                          if(df[t,"time_med"]>(lag$lag_zoledronate_other)){
                            df[t,"lag.passed"]<-1
                          }}

                      }

                      if(df[t,"medication"] ==8) {#teriparatide

                        if(!is.na(df[t,"t_since_sf"])) {
                          if(df[t,"time_med"]>(lag$lag_teriparatide_spine)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_hf"])) {
                          if(df[t,"time_med"]>(lag$lag_teriparatide_hip)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_of"])) {
                          if(df[t,"time_med"]>(lag$lag_teriparatide_other)){
                            df[t,"lag.passed"]<-1
                          }}

                      }

                      if(df[t,"medication"] ==9) {#abaloparatide

                        if(!is.na(df[t,"t_since_sf"])) {
                          if(df[t,"time_med"]>(lag$lag_abaloparatide_spine)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_hf"])) {
                          if(df[t,"time_med"]>(lag$lag_abaloparatide_hip)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_of"])) {
                          if(df[t,"time_med"]>(lag$lag_abaloparatide_other)){
                            df[t,"lag.passed"]<-1
                          }}

                      }

                      if(df[t,"medication"] ==10) {#romo

                        if(!is.na(df[t,"t_since_sf"])) {
                          if(df[t,"time_med"]>(lag$lag_romo_spine)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_hf"])) {
                          if(df[t,"time_med"]>(lag$lag_romo_hip)){
                            df[t,"lag.passed"]<-1
                          }}

                        if(!is.na(df[t,"t_since_of"])) {
                          if(df[t,"time_med"]>(lag$lag_romo_other)){
                            df[t,"lag.passed"]<-1
                          }}

                      }








                      # get monitored -----

                      # start off being monitored

                      # if monitored following any of the three fx (i.e. as individuals can have multiple)

                      # monitored

                      working.monitored.sf<-NA
                      working.monitored.hf<-NA
                      working.monitored.of<-NA

                      if(!is.na(df[t,"t_since_sf"])){
                        if(df[t,"t_since_sf"]==4){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==1){     # male
                            working.monitored.sf<- rmultinorm$rmultinorm.no.fls.monitored.spine.4m.male[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_hf"])){
                        if(df[t,"t_since_hf"]==4){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==1){     # male
                            working.monitored.hf<- rmultinorm$rmultinorm.no.fls.monitored.hip.4m.male[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_of"])){
                        if(df[t,"t_since_of"]==4){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==1){     # male
                            working.monitored.sf<- rmultinorm$rmultinorm.no.fls.monitored.spine.4m.male[df[t,"id"]]
                          }}}

                      if(!is.na(df[t,"t_since_sf"])){
                        if(df[t,"t_since_sf"]==4){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==2){     # female
                            working.monitored.sf<- rmultinorm$rmultinorm.no.fls.monitored.spine.4m.female[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_hf"])){
                        if(df[t,"t_since_hf"]==4){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==2){     # female
                            working.monitored.hf<- rmultinorm$rmultinorm.no.fls.monitored.hip.4m.female[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_of"])){
                        if(df[t,"t_since_of"]==4){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==2){     # female
                            working.monitored.sf<- rmultinorm$rmultinorm.no.fls.monitored.spine.4m.female[df[t,"id"]]
                          }}}

                      if(!is.na(df[t,"t_since_sf"])){
                        if(df[t,"t_since_sf"]==12){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==1){     # male
                            working.monitored.sf<- rmultinorm$rmultinorm.no.fls.monitored.spine.12m.male[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_hf"])){
                        if(df[t,"t_since_hf"]==12){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==1){     # male
                            working.monitored.hf<- rmultinorm$rmultinorm.no.fls.monitored.hip.12m.male[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_of"])){
                        if(df[t,"t_since_of"]==12){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==1){     # male
                            working.monitored.sf<- rmultinorm$rmultinorm.no.fls.monitored.spine.12m.male[df[t,"id"]]
                          }}}

                      if(!is.na(df[t,"t_since_sf"])){
                        if(df[t,"t_since_sf"]==12){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==2){     # female
                            working.monitored.sf<- rmultinorm$rmultinorm.no.fls.monitored.spine.12m.female[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_hf"])){
                        if(df[t,"t_since_hf"]==12){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==2){     # female
                            working.monitored.hf<- rmultinorm$rmultinorm.no.fls.monitored.hip.12m.female[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_of"])){
                        if(df[t,"t_since_of"]==12){
                          if(
                            df[t,"intervention"]==1 &  # no.fls
                            df[t,"sex"]==2){     # female
                            working.monitored.sf<- rmultinorm$rmultinorm.no.fls.monitored.spine.12m.female[df[t,"id"]]
                          }}}




                      if(!is.na(df[t,"t_since_sf"])){
                        if(df[t,"t_since_sf"]==4){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==1){     # male
                            working.monitored.sf<- rmultinorm$rmultinorm.fls.monitored.spine.4m.male[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_hf"])){
                        if(df[t,"t_since_hf"]==4){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==1){     # male
                            working.monitored.hf<- rmultinorm$rmultinorm.fls.monitored.hip.4m.male[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_of"])){
                        if(df[t,"t_since_of"]==4){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==1){     # male
                            working.monitored.sf<- rmultinorm$rmultinorm.fls.monitored.spine.4m.male[df[t,"id"]]
                          }}}

                      if(!is.na(df[t,"t_since_sf"])){
                        if(df[t,"t_since_sf"]==4){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==2){     # female
                            working.monitored.sf<- rmultinorm$rmultinorm.fls.monitored.spine.4m.female[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_hf"])){
                        if(df[t,"t_since_hf"]==4){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==2){     # female
                            working.monitored.hf<- rmultinorm$rmultinorm.fls.monitored.hip.4m.female[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_of"])){
                        if(df[t,"t_since_of"]==4){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==2){     # female
                            working.monitored.sf<- rmultinorm$rmultinorm.fls.monitored.spine.4m.female[df[t,"id"]]
                          }}}

                      if(!is.na(df[t,"t_since_sf"])){
                        if(df[t,"t_since_sf"]==12){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==1){     # male
                            working.monitored.sf<- rmultinorm$rmultinorm.fls.monitored.spine.12m.male[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_hf"])){
                        if(df[t,"t_since_hf"]==12){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==1){     # male
                            working.monitored.hf<- rmultinorm$rmultinorm.fls.monitored.hip.12m.male[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_of"])){
                        if(df[t,"t_since_of"]==12){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==1){     # male
                            working.monitored.sf<- rmultinorm$rmultinorm.fls.monitored.spine.12m.male[df[t,"id"]]
                          }}}

                      if(!is.na(df[t,"t_since_sf"])){
                        if(df[t,"t_since_sf"]==12){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==2){     # female
                            working.monitored.sf<- rmultinorm$rmultinorm.fls.monitored.spine.12m.female[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_hf"])){
                        if(df[t,"t_since_hf"]==12){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==2){     # female
                            working.monitored.hf<- rmultinorm$rmultinorm.fls.monitored.hip.12m.female[df[t,"id"]]
                          }}}
                      if(!is.na(df[t,"t_since_of"])){
                        if(df[t,"t_since_of"]==12){
                          if(
                            df[t,"intervention"]==2 &  # fls.
                            df[t,"sex"]==2){     # female
                            working.monitored.sf<- rmultinorm$rmultinorm.fls.monitored.spine.12m.female[df[t,"id"]]
                          }}}


                      # apply if updated, otherwise from previous
                      if(any(!is.na(c(working.monitored.sf,
                                      working.monitored.hf,
                                      working.monitored.of))) &
                         any(c(working.monitored.sf,
                               working.monitored.hf,
                               working.monitored.of)==2, na.rm=T)) {
                        df[t,"monitored"]<-2
                      } else {
                        df[t,"monitored"]<-df[t-1,"monitored"]
                      }



                      # get adherrence ------
                      df[t,"adhering"]<-1
                      # will be replaced below if adhering

                      
                      if(df[t-1,"s_d"]==0){ #alive
                         
                      if(df[t,"time_med"]==1){
                        df<-cmp_m.fun.adherrence.t1(df=df, t=t, rmultinorm=rmultinorm)
                      } else if(df[t,"time_med"]==4){
                        df<-cmp_m.fun.adherrence.t4(df=df, t=t, rmultinorm=rmultinorm)
                      } else if(df[t,"time_med"]==12){
                        df<-cmp_m.fun.adherrence.t12(df=df, t=t, rmultinorm=rmultinorm)
                      } else if(df[t,"time_med"]==24|
                                df[t,"time_med"]==36|
                                df[t,"time_med"]==48){
                        df<-cmp_m.fun.adherrence.t24_36_48(df=df, t=t, rmultinorm=rmultinorm)
                      } else {# if not time 1, 4, 12 same as in previous period
                        df[t,"adhering"]<-df[t-1,"adhering"]
                      }
                         }


                      # apply rr of drug ######
                      # on given drug, identified, tr onset, tr are all 2
                      # adhering


                      # only if on drug above lag

                      if(df[t,"adhering"]==2){
                        if(df[t,"lag.passed"]==1){

                          if(df[t,"medication"]==1 ){ #alendronate
                            df[t,"sf.tp"] <-df[t,"sf.tp"]*rr$rr_alendronate_spine
                            df[t,"hf.tp"] <-df[t,"hf.tp"]*rr$rr_alendronate_hip
                            df[t,"of.tp"] <-df[t,"of.tp"]*rr$rr_alendronate_other

                            df[t,"apply.rr"] <-1

                          }
                          if(df[t,"medication"]==2 ){ #risedronate
                            df[t,"sf.tp"] <-df[t,"sf.tp"]*rr$rr_risedronate_spine
                            df[t,"hf.tp"] <-df[t,"hf.tp"]*rr$rr_risedronate_hip
                            df[t,"of.tp"] <-df[t,"of.tp"]*rr$rr_risedronate_other

                            df[t,"apply.rr"] <-1
                          }
                          if(df[t,"medication"]==3 ){ #strontium
                            df[t,"sf.tp"] <-df[t,"sf.tp"]*rr$rr_strontium_spine
                            df[t,"hf.tp"] <-df[t,"hf.tp"]*rr$rr_strontium_hip
                            df[t,"of.tp"] <-df[t,"of.tp"]*rr$rr_strontium_other

                            df[t,"apply.rr"] <-1
                          }
                          if(df[t,"medication"]==4 ){ #ibandronate
                            df[t,"sf.tp"] <-df[t,"sf.tp"]*rr$rr_ibandronate_spine
                            df[t,"hf.tp"] <-df[t,"hf.tp"]*rr$rr_ibandronate_hip
                            df[t,"of.tp"] <-df[t,"of.tp"]*rr$rr_ibandronate_other

                            df[t,"apply.rr"] <-1
                          }
                          if(df[t,"medication"]==5 ){ #raloxifene
                            df[t,"sf.tp"] <-df[t,"sf.tp"]*rr$rr_raloxifene_spine
                            df[t,"hf.tp"] <-df[t,"hf.tp"]*rr$rr_raloxifene_hip
                            df[t,"of.tp"] <-df[t,"of.tp"]*rr$rr_raloxifene_other

                            df[t,"apply.rr"] <-1
                          }                         
                          if(df[t,"medication"]==6 ){ #denosumab
                            df[t,"sf.tp"] <-df[t,"sf.tp"]*rr$rr_denosumab_spine
                            df[t,"hf.tp"] <-df[t,"hf.tp"]*rr$rr_denosumab_hip
                            df[t,"of.tp"] <-df[t,"of.tp"]*rr$rr_denosumab_other

                            df[t,"apply.rr"] <-1
                          }
                          if(df[t,"medication"]==7 ){ #zoledronate
                            df[t,"sf.tp"] <-df[t,"sf.tp"]*rr$rr_zoledronate_spine
                            df[t,"hf.tp"] <-df[t,"hf.tp"]*rr$rr_zoledronate_hip
                            df[t,"of.tp"] <-df[t,"of.tp"]*rr$rr_zoledronate_other

                            df[t,"apply.rr"] <-1
                          }
                          if(df[t,"medication"]==8 ){ #teriparatide
                            df[t,"sf.tp"] <-df[t,"sf.tp"]*rr$rr_teriparatide_spine
                            df[t,"hf.tp"] <-df[t,"hf.tp"]*rr$rr_teriparatide_hip
                            df[t,"of.tp"] <-df[t,"of.tp"]*rr$rr_teriparatide_other

                            df[t,"apply.rr"] <-1
                          }
                          if(df[t,"medication"]==9 ){ #abaloparatide
                            df[t,"sf.tp"] <-df[t,"sf.tp"]*rr$rr_abaloparatide_spine
                            df[t,"hf.tp"] <-df[t,"hf.tp"]*rr$rr_abaloparatide_hip
                            df[t,"of.tp"] <-df[t,"of.tp"]*rr$rr_abaloparatide_other

                            df[t,"apply.rr"] <-1
                          }
                          if(df[t,"medication"]==10 ){ #romo
                            df[t,"sf.tp"] <-df[t,"sf.tp"]*rr$rr_romo_spine
                            df[t,"hf.tp"] <-df[t,"hf.tp"]*rr$rr_romo_hip
                            df[t,"of.tp"] <-df[t,"of.tp"]*rr$rr_romo_other

                            df[t,"apply.rr"] <-1
                          }

                        }
                      }



                      # get transition #####

                      # make transition
                      # alive in current period
                      if(df[t-1,"s_hf"] == 1|
                         df[t-1,"s_sf"] == 1|
                         df[t-1,"s_of"] == 1|
                         df[t-1,"s_ff"] == 1){

                        # get transition
                        probs<-  c(as.numeric(df[t,"hf.tp"]),
                                         as.numeric(df[t,"of.tp"]),
                                         as.numeric(df[t,"sf.tp"]),
                                         as.numeric( df[t,"tp.death"]),
                                         1-    as.numeric(df[t,"hf.tp"])-
                                           as.numeric(df[t,"of.tp"])-
                                           as.numeric(df[t,"sf.tp"])-
                                          as.numeric(df[t,"tp.death"])  )
                     rand<-  rmultinorm$rmultinorm.trans[df[t,"id"]]
                     
                     working.transition<-ifelse(rand<probs[1], 1,
                             ifelse(rand>=probs[1]&
                                   rand<(1-sum(probs[3:5])), 2,  
                              ifelse(rand>=sum(probs[1:2])&
                                     rand<(1-sum(probs[4:5])), 3,       
                              ifelse(rand>=sum(probs[1:3])&
                                     rand<(1-sum(probs[5])), 4,      
                              ifelse(rand>=(1-probs[5]), 5, NA)))))


                        # if an individual is currently in s_hf,  s_sf,  s_of, or  s_ff,
                        # they can transition to any of the other state

                        # if working.transition=1: to s_hf
                        if(working.transition==1) {
                          df[t,"s_hf"]<-1
                          df[t,"s_sf"]<-0
                          df[t,"s_of"]<-0
                          df[t,"s_ff"]<-0
                          df[t,"s_d"]<-0
                        }

                        # if working.transition=2:  to s_of
                        if(working.transition==2) {
                          df[t,"s_hf"]<-0
                          df[t,"s_sf"]<-0
                          df[t,"s_of"]<-1
                          df[t,"s_ff"]<-0
                          df[t,"s_d"]<-0 }

                        # if working.transition=3:  to s_sf
                        if(working.transition==3) {
                          df[t,"s_hf"]<-0
                          df[t,"s_sf"]<-1
                          df[t,"s_of"]<-0
                          df[t,"s_ff"]<-0
                          df[t,"s_d"]<-0  }

                        # if working.transition=4:   to s_d
                        if(working.transition==4) {
                          df[t,"s_hf"]<-0
                          df[t,"s_sf"]<-0
                          df[t,"s_of"]<-0
                          df[t,"s_ff"]<-0
                          df[t,"s_d"]<-1  }

                        # if working.transition=5:  to s_ff
                        if(working.transition==5) {
                          df[t,"s_hf"]<-0
                          df[t,"s_sf"]<-0
                          df[t,"s_of"]<-0
                          df[t,"s_ff"]<-1
                          df[t,"s_d"]<-0 }
                      } else{
                        # from s_d in current period
                        # remains in s_d
                        df[t,"s_hf"]<-0
                        df[t,"s_sf"]<-0
                        df[t,"s_of"]<-0
                        df[t,"s_ff"]<-0
                        df[t,"s_d"]<-1
                      }


                      # currently in fracture state?
                      df[t,"c_af"]<-sum(df[t,"s_hf"],
                                        df[t,"s_sf"],
                                        df[t,"s_of"])

                      ## update history of fracture
                      df[t,"h_hf"]<-sum(df[1:(t-1),"s_hf"])
                      df[t,"h_sf"]<-sum(df[1:(t-1),"s_sf"])
                      df[t,"h_of"]<-sum(df[1:(t-1),"s_of"])
                      df[t,"h_af"]<-sum(df[1:(t-1),"s_hf"],df[1:(t-1),"s_sf"],df[1:(t-1),"s_of"])


  }  
                      
                      # get hospitalised ------
                      df<-cmp_m.fun.hospitalised(df=df, t=t, rmultinorm=rmultinorm,  input=user_inputs)
                      # get procedure ------
                      df<-cmp_m.fun.procedure(df=df, t=t, rmultinorm=rmultinorm,  input=user_inputs)
                      # get temp rehab ------
                      df<-cmp_m.fun.temp.rehab(df=df, t=t, rmultinorm=rmultinorm,  input=user_inputs)
                      # # get location -----
                      #browser
                      df<-cmp_m.fun.location(df=df, t=t, rmultinorm=rmultinorm,  input=user_inputs)
                      # get lab test -----
                      df<-m.fun.lab.test(df=df, t=t, rmultinorm=rmultinorm,  input=user_inputs)
                      # get dxa -----
                      df<-m.fun.dxa(df=df, t=t, rmultinorm=rmultinorm,  input=user_inputs)


                      df

} # full.rmultinorm list of draws 
cmp_m.TR.go = compiler::cmpfun(m.TR.go)