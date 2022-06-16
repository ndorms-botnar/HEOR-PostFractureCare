
library(dplyr)
library(here)
#### LIFETABLE DATA ------
# #lifetables

# # #install.packages("demography")
# library(demography)
# library(readr)
# # takes mortality data from https://www.mortality.org/ The Human Mortality Database
# 
# japan.demography<-hmd.mx("JPN", "edward.burn@ndorms.ox.ac.uk", "1576952100", label = "JPN")
# #max(japan.demography$year) 2016
# japan.demography<-extract.years(japan.demography, 2016)
# # mortality rates
# japan.mortality<-data.frame(age= as.numeric(row.names(japan.demography$rate$total)),
#            male=as.numeric(japan.demography$rate$male),
#            female=as.numeric(japan.demography$rate$female))%>%
#   filter(age>=50 & age<100)
# rm(japan.demography)
# 
# spain.demography<-hmd.mx("ESP", "edward.burn@ndorms.ox.ac.uk", "1576952100", label = "ESP")
# #max(spain.demography$year) 2016
# spain.demography<-extract.years(spain.demography, 2016)
# # mortality rates
# spain.mortality<-data.frame(age= as.numeric(row.names(spain.demography$rate$total)),
#            male=as.numeric(spain.demography$rate$male),
#            female=as.numeric(spain.demography$rate$female))%>%
#   filter(age>=50 & age<100)
# rm(spain.demography)
# #
# 
# uk.demography<-hmd.mx("GBR_NP", "edward.burn@ndorms.ox.ac.uk", "1576952100", label = "GBR_NP")
# #max(uk.demography$year) 2016
# uk.demography<-extract.years(uk.demography, 2016)
# # mortality rates
# uk.mortality<-data.frame(age= as.numeric(row.names(uk.demography$rate$total)),
#            male=as.numeric(uk.demography$rate$male),
#            female=as.numeric(uk.demography$rate$female))%>%
#   filter(age>=50 & age<100)
# rm(uk.demography)
# 
# 
# netherlands.demography<-hmd.mx("NLD", "edward.burn@ndorms.ox.ac.uk", "1576952100", label = "NLD")
# #max(netherlands.demography$year) 2016
# netherlands.demography<-extract.years(netherlands.demography, 2016)
# # mortality rates
# netherlands.mortality<-data.frame(age= as.numeric(row.names(netherlands.demography$rate$total)),
#            male=as.numeric(netherlands.demography$rate$male),
#            female=as.numeric(netherlands.demography$rate$female))%>%
#   filter(age>=50 & age<100)
# rm(netherlands.demography)
# 
# france.demography<-hmd.mx("FRATNP", "edward.burn@ndorms.ox.ac.uk", "1576952100", label = "ESP")
# #max(spain.demography$year) 2016
# france.demography<-extract.years(france.demography, 2016)
# # mortality rates
# france.mortality<-data.frame(age= as.numeric(row.names(france.demography$rate$total)),
#            male=as.numeric(france.demography$rate$male),
#            female=as.numeric(france.demography$rate$female))%>%
#   filter(age>=50 & age<100)
# rm(france.demography)
# 
# mexico.mortality<-read_csv(here("RShiny", "www", "mexico_mortality.csv")) %>%
#   filter(age>=50 & age<100)
# #https://mort.soa.org/
# #Table Reference:	Jorge Rendón Elizando reprinted Marisol Villanueva Castillo, “Seguro para Cuidados Prolongados y Bienestar de los Adultos Mayores”, Comisión Nacional de Suguros  y Fianzas (2005). Accessed: February, 2014 from http://www.cnsf.gob.mx/Eventos/Premios/2005%20Seguros/Qualitas%20Vita%20incorporado.pdf
# #Comments:	Study Data: Insured data from the joint Asociación Mexicana de Instituciones de Seguros (AMIS) and Mexican Asociatión of Actuaries (AMA) 1995-1998 study of standard ordinary insurance for durations 5 years and over. Contracts issued under rated, extended term and reduced paid up not included.  Methodology: Quinquennial age crude rates with Jenkins interpolation. No loading. Multiple decrements theory was used for high and low ages. Data Transcription Errors Detected: 02/2014. Data Corrected and Certified: 02/2014
# 
# colombia.mortality<-read_csv(here("RShiny", "www", "colombia_mortality.csv")) %>%
#   filter(age>=50 & age<100)
# #https://mort.soa.org/
# #Table Reference:	Superintendencia Financiera de Colombia, “Tabla de Mortalidad de Rentistas 1980-1989”, Resolución Numero 1555 d 2010, El Superintendente Financiero (Bogata, 2010). Accessed: September, 2014 from https://www.superfinanciera.gov.co/SFCant/ConsumidorFinanciero/r155510.doc
# #Comments:	Study Data: Active lives and annuitants from pension institutes observed from 2005 to 2008. Methodology: Not available at this time. Data Transcription Errors: None. Data Certified: 09/2014
# 
# brazil.mortality<-read_csv(here("RShiny", "www", "brazil_mortality.csv")) %>%
#   filter(age>=50 & age<100)
# #https://mort.soa.org/
# #Table Reference:	DE OLIVEIRA, M. M. C.; RAMIREZ, M. R.; FRISCHTAK, R. M.; BORGES, R. B. de R.; COSTA, B.; PEDROSO, R. C. Mortality tables for the Brazilian insured population. Revista Brasileira de Estudos de População, v. 33, n. 3, p. 653–677, 2016. http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0102-30982016000300653
# #Comments:	Study Data: Data are based on the experience of the Brazilian life insurance market for the years 2004 to 2012. The data set was provided by 13 insurance companies, representing about 80 percent of the Brazilian insurance market. Methodology: Tukey method was used to exclude outlier data. Mortality rates were graduated by applying the Heligman–Pollard model on the young ages and moving averages on the intermediate age range (approximately 18–90 years). The mortality trend was extrapolated to older ages. Further details can be found in the Table Reference. Data Certified: 07/2018.
# 
# # 
# # library(ggplot2)
# # rbind(japan.mortality %>%  mutate(country="Japan"),
# #       netherlands.mortality %>%  mutate(country="Netherlands"),
# #       spain.mortality %>%  mutate(country="Spain"),
# #       france.mortality %>%  mutate(country="France"),
# #       uk.mortality %>%  mutate(country="UK"),
# #       mexico.mortality %>%  mutate(country="Mexico"),
# #       colombia.mortality %>%  mutate(country="Colombia"),
# #       brazil.mortality %>%  mutate(country="Brazil")) %>% 
# #   ggplot()+
# #   geom_line(aes(age, male, colour=country))



# save.image(file = here("RShiny", "www","lifetables.Rdata"))

load( here("RShiny", "www","lifetables.Rdata"))

#### GOOGLE SHEETS INPUTS ------


update.google.inputs<-function(){
 library(googlesheets4)
general.inputs<-read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="General",
           col_types= "c")
general.inputs<-general.inputs %>%
  select(name, Description, Value) %>%
 # filter(!is.na(Value)) %>%
  filter(Value!="N/A")%>%
  filter(Value!="timing of risk")%>%
  filter(Value!="only_hip_fx_n * 0.75")%>%
  filter(Value!="(5*only_hip_fx_n) - only_hip_fx_n - der_spine_fx_n")
save(general.inputs, file = here("RShiny", "www","general.inputs.Rdata"))

spain.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Spain",
           range="A1:D1000",
           col_types= "c")
spain.inputs<-spain.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(spain.inputs, file = here("RShiny", "www","spain.inputs.Rdata"))

spain.cost.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Spain costs",
           range="B1:D500",
           col_types= "c")
spain.cost.inputs<-spain.cost.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(spain.cost.inputs, file = here("RShiny", "www","spain.cost.inputs.Rdata"))



colombia.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Colombia",
           range="A1:D1000",
           col_types= "c")
colombia.inputs<-colombia.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(colombia.inputs, file = here("RShiny", "www","colombia.inputs.Rdata"))

colombia.cost.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Colombia costs",
           range="B1:D500",
           col_types= "c")
colombia.cost.inputs<-colombia.cost.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(colombia.cost.inputs, file = here("RShiny", "www","colombia.cost.inputs.Rdata"))


france.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="France",
           range="A1:D1000",
           col_types= "c")
france.inputs<-france.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(france.inputs, file = here("RShiny", "www","france.inputs.Rdata"))

france.cost.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="France costs",
           range="B1:D500",
           col_types= "c")
france.cost.inputs<-france.cost.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(france.cost.inputs, file = here("RShiny", "www","france.cost.inputs.Rdata"))

mexico.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Mexico",
           range="A1:D1000",
           col_types= "c")
mexico.inputs<-mexico.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(mexico.inputs, file = here("RShiny", "www","mexico.inputs.Rdata"))

mexico.cost.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Mexico costs",
           range="B1:D500",
           col_types= "c")
mexico.cost.inputs<-mexico.cost.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(mexico.cost.inputs, file = here("RShiny", "www","mexico.cost.inputs.Rdata"))

brazil.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Brazil",
           range="A1:D1000",
           col_types= "c")
brazil.inputs<-brazil.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(brazil.inputs, file = here("RShiny", "www","brazil.inputs.Rdata"))

brazil.cost.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Brazil costs",
           range="B1:D500",
           col_types= "c")
brazil.cost.inputs<-brazil.cost.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(brazil.cost.inputs, file = here("RShiny", "www","brazil.cost.inputs.Rdata"))

japan.inputs<-  read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Japan",
           range="A1:D1000",
           col_types= "c")
japan.inputs<-japan.inputs %>%
  select(name, Value) %>%
  filter(!is.na(Value))
save(japan.inputs, file = here("RShiny", "www","japan.inputs.Rdata"))

japan.cost.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Japan costs",
           range="B1:D500",
           col_types= "c")
japan.cost.inputs<-japan.cost.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(japan.cost.inputs, file = here("RShiny", "www","japan.cost.inputs.Rdata"))


japan_inject_only.inputs<-  read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Japan_inject_only",
           range="A1:D1000",
           col_types= "c")
japan_inject_only.inputs<-japan_inject_only.inputs %>%
  select(name, Value) %>%
  filter(!is.na(Value))
save(japan_inject_only.inputs, file = here("RShiny", "www","japan_inject_only.inputs.Rdata"))

Japan.costs_inject_only<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Japan costs_inject_only",
           range="B1:D500",
           col_types= "c")
Japan.costs_inject_only<-Japan.costs_inject_only %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(Japan.costs_inject_only, file = here("RShiny", "www","Japan.costs_inject_only.Rdata"))


Japan_reducedPFC<-  read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Japan_reducedPFC",
           range="A1:D1000",
           col_types= "c")
Japan_reducedPFC<-Japan_reducedPFC %>%
  select(name, Value) %>%
  filter(!is.na(Value))
save(Japan_reducedPFC, file = here("RShiny", "www","Japan_reducedPFC.Rdata"))

Japan.costs_reducedPFC<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Japan costs_reducedPFC",
           range="B1:D500",
           col_types= "c")
Japan.costs_reducedPFC<-Japan.costs_reducedPFC %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(Japan.costs_reducedPFC, file = here("RShiny", "www","Japan.costs_reducedPFC.Rdata"))



#save current as backup
# load("www/netherlands.inputs.Rdata")
# save(netherlands.inputs, file = "www/netherlands.inputs_previous.Rdata")
# updated
netherlands.inputs<-  read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Netherlands",
           range="A1:D1000",
           col_types= "c")
netherlands.inputs<-netherlands.inputs %>%
  select(name, Value) %>%
  filter(!is.na(Value))
save(netherlands.inputs, file = here("RShiny", "www","netherlands.inputs.Rdata"))

#save current as backup
# load("www/netherlands.cost.inputs.Rdata")
# save(netherlands.cost.inputs, file = "www/netherlands.cost.inputs_previous.Rdata")
netherlands.cost.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Netherlands costs",
           range="B1:D500",
           col_types= "c")
netherlands.cost.inputs<-netherlands.cost.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(netherlands.cost.inputs, file = here("RShiny", "www","netherlands.cost.inputs.Rdata"))

# reduced netherlands pfc sensitivity
NetherlandsPerfectPfc.inputs<-  read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="NetherlandsPerfectPfc",
           range="A1:D1000",
           col_types= "c")
NetherlandsPerfectPfc.inputs<-NetherlandsPerfectPfc.inputs %>%
  select(name, Value) %>%
  filter(!is.na(Value))
save(NetherlandsPerfectPfc.inputs, file = here("RShiny", "www","NetherlandsPerfectPfc.inputs.Rdata"))

NetherlandsPerfectPfcCosts<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="NetherlandsPerfectPfcCosts",
           range="B1:D500",
           col_types= "c")
NetherlandsPerfectPfcCosts<-NetherlandsPerfectPfcCosts %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(NetherlandsPerfectPfcCosts, file = here("RShiny", "www","NetherlandsPerfectPfcCosts.Rdata"))




united_kingdom.inputs<-  read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="United Kingdom",
           range="A1:D1000",
           col_types= "c")
united_kingdom.inputs<-united_kingdom.inputs %>%
  select(name, Value) %>%
  filter(!is.na(Value))
save(united_kingdom.inputs, file = here("RShiny", "www","united_kingdom.inputs.Rdata"))

united_kingdom.cost.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="United Kingdom costs",
           range="B1:D500",
           col_types= "c")
united_kingdom.cost.inputs<-united_kingdom.cost.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(united_kingdom.cost.inputs, file = here("RShiny", "www","united_kingdom.cost.inputs.Rdata"))



generic_country.inputs<-  read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Generic country",
           range="A1:D1000",
           col_types= "c")
generic_country.inputs<-generic_country.inputs %>%
  select(name, Value) %>%
  filter(!is.na(Value))
save(generic_country.inputs, file = here("RShiny", "www","generic_country.inputs.Rdata"))

generic_country.cost.inputs<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Generic country costs",
           range="B1:D500",
           col_types= "c")
generic_country.cost.inputs<-generic_country.cost.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(generic_country.cost.inputs, file = here("RShiny", "www","generic_country.cost.inputs.Rdata"))

generic_country.qol.inputs<-  read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Generic country qol",
           range="A1:D20",
           col_types= "c")
generic_country.qol.inputs<-generic_country.qol.inputs %>%
  select(name, Description, Value) %>%
  filter(!is.na(Value))
save(generic_country.qol.inputs, file = here("RShiny", "www","generic_country.qol.inputs.Rdata"))




UI.text<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="UI",
           range="A1:B316",
           col_types= "c")
UI.text<-UI.text %>%
  select(name,	text) %>%
  filter(!is.na(name))
save(UI.text, file = here("RShiny", "www","UI.text.Rdata"))

# fx timing
 timing_fx_risk.5y<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
            sheet="Timing of risk",
            range="F1:I316",
            col_types= "n")
timing_fx_risk.5y<-timing_fx_risk.5y %>%
   filter(!is.na(Month))
save(timing_fx_risk.5y, file = here("RShiny", "www","timing_fx_risk.5y.Rdata"))

timing_fx_risk.10y<- read_sheet("https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
           sheet="Timing of risk",
           range="A1:D316",
           col_types= "n")
timing_fx_risk.10y<-timing_fx_risk.10y %>%
   filter(!is.na(Month))
save(timing_fx_risk.10y, file = here("RShiny", "www","timing_fx_risk.10y.Rdata"))


}
  
 #update.google.inputs()  
load(here("RShiny", "www","general.inputs.Rdata"))

load(here("RShiny", "www","spain.inputs.Rdata"))
load(here("RShiny", "www","spain.cost.inputs.Rdata"))

load(here("RShiny", "www","colombia.inputs.Rdata"))
load(here("RShiny", "www","colombia.cost.inputs.Rdata"))

load(here("RShiny", "www","france.inputs.Rdata"))
load(here("RShiny", "www","france.cost.inputs.Rdata"))

load(here("RShiny", "www","mexico.inputs.Rdata"))
load(here("RShiny", "www","mexico.cost.inputs.Rdata"))

load(here("RShiny", "www","brazil.inputs.Rdata"))
load(here("RShiny", "www","brazil.cost.inputs.Rdata"))

load(here("RShiny", "www","japan.inputs.Rdata"))
load(here("RShiny", "www","japan.cost.inputs.Rdata"))
load(here("RShiny", "www","japan_inject_only.inputs.Rdata"))
load(here("RShiny", "www","Japan.costs_inject_only.Rdata"))
load(here("RShiny", "www","Japan_reducedPFC.Rdata"))
load(here("RShiny", "www","Japan.costs_reducedPFC.Rdata"))

# japan_inject_only.inputs<-japan.inputs
# Japan.costs_inject_only<-japan.cost.inputs

load(here("RShiny", "www","netherlands.inputs.Rdata"))
load(here("RShiny", "www","netherlands.cost.inputs.Rdata"))

load(here("RShiny", "www","NetherlandsPerfectPfc.inputs.Rdata"))
load(here("RShiny", "www","NetherlandsPerfectPfcCosts.Rdata"))

load(here("RShiny", "www","united_kingdom.inputs.Rdata"))
load(here("RShiny", "www","united_kingdom.cost.inputs.Rdata"))

load(here("RShiny", "www","generic_country.inputs.Rdata"))
load(here("RShiny", "www","generic_country.cost.inputs.Rdata"))
load(here("RShiny", "www","generic_country.qol.inputs.Rdata"))

load(here("RShiny", "www","timing_fx_risk.5y.Rdata"))
load(here("RShiny", "www","timing_fx_risk.10y.Rdata"))
load(here("RShiny", "www","UI.text.Rdata"))

# variable names
vars<-rbind(general.inputs,
      spain.inputs,
      spain.cost.inputs,
      generic_country.qol.inputs) %>% 
  select(name, Description)
  

# lag
lag_alendronate_hip<-as.numeric(general.inputs %>% filter(name=="lag_alendronate_hip") %>% 
  select(Value))
lag_risedronate_hip<-as.numeric(general.inputs %>% filter(name=="lag_risedronate_hip") %>% 
  select(Value))
lag_strontium_hip<-as.numeric(general.inputs %>% filter(name=="lag_strontium_hip") %>% 
  select(Value))
lag_ibandronate_hip<-as.numeric(general.inputs %>% filter(name=="lag_ibandronate_hip") %>% 
  select(Value))
lag_raloxifene_hip<-as.numeric(general.inputs %>% filter(name=="lag_raloxifene_hip") %>% 
  select(Value))
lag_denosumab_hip<-as.numeric(general.inputs %>% filter(name=="lag_denosumab_hip") %>% 
  select(Value))
lag_zoledronate_hip<-as.numeric(general.inputs %>% filter(name=="lag_zoledronate_hip") %>% 
  select(Value))
lag_teriparatide_hip<-as.numeric(general.inputs %>% filter(name=="lag_teriparatide_hip") %>% 
  select(Value))
lag_abaloparatide_hip<-as.numeric(general.inputs %>% filter(name=="lag_abaloparatide_hip") %>% 
  select(Value))
lag_romo_hip<-as.numeric(general.inputs %>% filter(name=="lag_romo_hip") %>% 
  select(Value))

lag_alendronate_spine<-as.numeric(general.inputs %>% filter(name=="lag_alendronate_spine") %>% 
  select(Value))
lag_risedronate_spine<-as.numeric(general.inputs %>% filter(name=="lag_risedronate_spine") %>% 
  select(Value))
lag_strontium_spine<-as.numeric(general.inputs %>% filter(name=="lag_strontium_spine") %>% 
  select(Value))
lag_ibandronate_spine<-as.numeric(general.inputs %>% filter(name=="lag_ibandronate_spine") %>% 
  select(Value))
lag_raloxifene_spine<-as.numeric(general.inputs %>% filter(name=="lag_raloxifene_spine") %>% 
  select(Value))
lag_denosumab_spine<-as.numeric(general.inputs %>% filter(name=="lag_denosumab_spine") %>% 
  select(Value))
lag_zoledronate_spine<-as.numeric(general.inputs %>% filter(name=="lag_zoledronate_spine") %>% 
  select(Value))
lag_teriparatide_spine<-as.numeric(general.inputs %>% filter(name=="lag_teriparatide_spine") %>% 
  select(Value))
lag_abaloparatide_spine<-as.numeric(general.inputs %>% filter(name=="lag_abaloparatide_spine") %>% 
  select(Value))
lag_romo_spine<-as.numeric(general.inputs %>% filter(name=="lag_romo_spine") %>% 
  select(Value))


lag_alendronate_other<-as.numeric(general.inputs %>% filter(name=="lag_alendronate_other") %>% 
  select(Value))
lag_risedronate_other<-as.numeric(general.inputs %>% filter(name=="lag_risedronate_other") %>% 
  select(Value))
lag_strontium_other<-as.numeric(general.inputs %>% filter(name=="lag_strontium_other") %>% 
  select(Value))
lag_ibandronate_other<-as.numeric(general.inputs %>% filter(name=="lag_ibandronate_other") %>% 
  select(Value))
lag_raloxifene_other<-as.numeric(general.inputs %>% filter(name=="lag_raloxifene_other") %>% 
  select(Value))
lag_denosumab_other<-as.numeric(general.inputs %>% filter(name=="lag_denosumab_other") %>% 
  select(Value))
lag_zoledronate_other<-as.numeric(general.inputs %>% filter(name=="lag_zoledronate_other") %>% 
  select(Value))
lag_teriparatide_other<-as.numeric(general.inputs %>% filter(name=="lag_teriparatide_other") %>% 
  select(Value))
lag_abaloparatide_other<-as.numeric(general.inputs %>% filter(name=="lag_abaloparatide_other") %>% 
  select(Value))
lag_romo_other<-as.numeric(general.inputs %>% filter(name=="lag_romo_other") %>% 
  select(Value))



# a<-japan.inputs %>% 
#   filter(japan_inject_only.inputs$Value!=japan.inputs$Value)
# b<-japan_inject_only.inputs %>% 
#   filter(japan_inject_only.inputs$Value!=japan.inputs$Value)


