
start<-Sys.time()
country<-"NetherlandsPerfectPfc"
run.in.parallel<-TRUE
use.risk.profiles<-FALSE

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
library(here)

# run model ----
source("RMicrosim/extras/CodeToRun.R")


# write report output -----
gc()
# memory.limit(170000) 


if (!file.exists( here("report",country))){
  dir.create( here("report",country), recursive = TRUE)}
# write csv with all the inputs
write.csv(t(data.frame(user_inputs)), 
          here("report",country,
               paste0("report ",country," ", format(Sys.Date(),"%Y_%m_%d"), " ", n_microsimulation, "_sims.csv" )
          ))
# write report
render(here("report","report.Rmd"),
       params = list(
         country = country,
         on.ARC = FALSE),
       output_dir =  here("report",country),
       output_file=paste0("report ",country," ", format(Sys.Date(),"%Y_%m_%d"), " ", n_microsimulation, "_sims" ))


Sys.time()-start
