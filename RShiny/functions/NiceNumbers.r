
# printing numbers with 1 decimal place and commas 
nice.num<-function(x){
  format(round(x,1),
         big.mark=",", nsmall = 1, digits=1, scientific = FALSE)}

# for counts- without decimal place
nice.num.count<-function(x){
  format(x,
         big.mark=",", nsmall = 0, digits=0, scientific = FALSE)}