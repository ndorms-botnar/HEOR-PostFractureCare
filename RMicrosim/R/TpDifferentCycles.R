
# get tps for different cycles
tp.different.cycle.length<-function(tp, increments){
  1-(1-{{tp}})^(1/{{increments}})
}