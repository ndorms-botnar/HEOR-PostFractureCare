

country<-"UnitedKingdom"
on.ARC<-FALSE
ARC.path<-"PfcCalculatorARC/PfcCalculatorArc"
run.in.parallel<-TRUE

# packages -----

library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(Hmisc)
library(kableExtra)
library(stringr)
library(scales)
library(tidyr)
library(withr)
library(parallel)
library(RSQLite)
library(DBI)
library(rmarkdown)
library(cmprsk)
library(survminer)

if(on.ARC!=TRUE){
library(here)}

# run model ----
if(on.ARC!=TRUE){  
source("RMicrosim/extras/CodeToRun.R")} else {
source(paste0(ARC.path,"/RMicrosim/extras/CodeToRun.R"))  
}
  

# write report output (locally only)  -----
if(on.ARC!=TRUE){
# if on arc download and write report locally

results.folder<-here("results",country)
load(paste0(results.folder, "/results.output.RData"))  
  
render(
  here("report","report.Rmd"),
       params = list(
    country = country,
    on.ARC = on.ARC),
     output_dir =  here("report",country),
       output_file=paste0("report ",country," ", format(Sys.Date(),"%Y_%m_%d"), " ", n_microsimulation, "_sims" ))
}

Sys.time()-start
