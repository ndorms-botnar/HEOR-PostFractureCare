m.TR<-collect_model_output()
table(m.TR$intervention, m.TR$identified)  # no fls 26447   
table(m.TR$intervention, m.TR$tr.onset)    # no fls 24674   
table(m.TR$intervention, m.TR$treat)       #no fls  4499  
table(m.TR$intervention, m.TR$monitored)   # no fls 22226   
table(m.TR$intervention, m.TR$adhering)    #no fls  2295  
table(m.TR$intervention, m.TR$apply.rr)    #no fls 1374    
table(m.TR$intervention, m.TR$s_hf)        #no fls  786     
mean(m.TR$total.cost)    #82665.28


table(m.TR$intervention, m.TR$medication) 


# japan
  #        alendronate denosumab ibandronate   none risedronate   romo strontium teriparatide zoledronate
  # FLS           1137      8417       12603  54331       28793    991      8476         4575        8777
  # no FLS        2409         0         393 123601        1636      0         0           61           0


# inject only
  #        alendronate denosumab ibandronate   none risedronate   romo strontium teriparatide zoledronate
  # FLS              0     35321          47  53869          47   1006      1219         4518       32073
  # no FLS        2409         0         393 123601        1636      0         0           61           0
  # 
  # 
  # 
  
  