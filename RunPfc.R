


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

# Set up -----
country<-"Spain"
# one of 
## Spain
## Netherlands
## NetherlandsPerfectPfc
## Japan
## JapanInjectOnly
## JapanReducedPfc
## Brazil
## Colombia
## France
## Mexico
## UnitedKingdom
## GenericCountry



# shiny::runApp(here("RShiny"))

#switch to true if using risk profiles
use.risk.profiles<-FALSE

# run model ---
start<-Sys.time()
run.in.parallel<-TRUE
source("RMicrosim/extras/CodeToRun.R")

# generate report -----
# create folder to save report if it doesn´t already exist
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
