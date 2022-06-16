
# printing numbers with 1 decimal place and commas
nice.num<-function(x){
  for(i in 1:length(x)){
    x[i]<-  ifelse(x[i]=="", "",
                   format(as.numeric(x[i]),
                          big.mark=",", nsmall = 1, digits=1, scientific = FALSE))
  }
  x
  }

# for counts- without decimal place
nice.num.count<-function(x){
  for(i in 1:length(x)){
    x[i]<-  ifelse(x[i]=="", "",
                   format(as.numeric(x[i]),
                          big.mark=",", nsmall = 0, digits=0, scientific = FALSE))
  }
  x
}
